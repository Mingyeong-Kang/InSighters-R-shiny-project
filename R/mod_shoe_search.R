# =============================================================================
# mod_shoe_search.R — Tab 1. Shoe Search
# 반환값: list(selected = reactive(쿼리 정보), K = reactive(K))  -> 다른 탭과 공유
# =============================================================================

# selfcheck 사진 목록 + 정답 라벨 (과제 제공 selfcheck/labels.csv: 컬럼 photo, category)
# - BOM / CRLF 줄바꿈 / 앞뒤 공백 / 대소문자 차이가 있어도 파일명으로 정확히 매칭
# - labels.csv 가 없거나 컬럼이 다르면 status 에 이유를 담아 화면에 안내 (라벨을 추측하지 않음)
list_selfcheck <- function(dir = PATH_SELFCHECK) {
  files <- sort(list.files(dir, pattern = "\\.(png|jpe?g)$", ignore.case = TRUE))
  out <- data.frame(photo = files, category = NA_character_, stringsAsFactors = FALSE)
  lab_path <- file.path(dir, "labels.csv")
  if (!file.exists(lab_path)) {
    attr(out, "status") <- sprintf("'%s' 파일이 없어 실제 카테고리를 표시할 수 없습니다.", lab_path)
    return(out)
  }
  lab <- tryCatch(utils::read.csv(lab_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM",
                                  strip.white = TRUE),
                  error = function(e) NULL)
  if (is.null(lab)) { attr(out, "status") <- "labels.csv 를 읽을 수 없습니다."; return(out) }
  names(lab) <- tolower(trimws(names(lab)))
  if (!all(c("photo", "category") %in% names(lab))) {
    attr(out, "status") <- sprintf("labels.csv 컬럼이 photo, category 가 아닙니다 (현재: %s).",
                                   paste(names(lab), collapse = ", "))
    return(out)
  }
  key <- function(x) tolower(basename(trimws(x)))
  out$category <- trimws(lab$category)[match(key(out$photo), key(lab$photo))]
  missing <- out$photo[is.na(out$category)]
  attr(out, "status") <- if (length(missing))
    sprintf("labels.csv 에 라벨이 없는 사진: %s", paste(missing, collapse = ", ")) else "ok"
  out
}

shoeSearchUI <- function(id) {
  ns <- NS(id)
  sc <- list_selfcheck()
  sc_choices_names <- lapply(seq_len(nrow(sc)), function(i) {
    span(tags$img(src = gray_uri(mat_to_vec(preprocess_photo(file.path(PATH_SELFCHECK, sc$photo[i]), crop = FALSE)),
                                 scale = 2),
                  width = 64, height = 64, alt = sc$photo[i], class = "border rounded"),
         sub("\\.[a-z]+$", "", sc$photo[i]))
  })

  tagList(
    page_header("Tab 1 · PCA image search", "Shoe Search",
                "신발 사진 한 장을 고르면, 2,000장 카탈로그를 PCA 공간에 투영해 가장 가까운 신발 5개를 찾고 카테고리를 예측합니다."),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 360, title = "📷 Choose a photo",
        # 입력 방법 3가지: 탭(nav)으로 전환 — 선택한 탭 이름이 input$src 가 된다
        navset_pill(
          id = ns("src"),
          nav_panel("Catalogue",
            div(class = "pt-2",
                numericInput(ns("cat_id"), "Catalogue id (1–2000)", 1, 1, 2000, step = 1),
                actionButton(ns("random"), "🎲 Random id", class = "btn-outline-primary btn-sm"),
                hint("검색할 때 이 사진 자체는 결과에서 제외됩니다."))),
          nav_panel("Self-check",
            div(class = "pt-2",
                if (nrow(sc) == 0) notice("selfcheck/ 폴더에 사진과 labels.csv 를 넣어 주세요.")
                else tagList(
                  radioButtons(ns("sc_file"), "Self-check photo", choiceNames = sc_choices_names,
                               choiceValues = sc$photo),
                  if (!identical(attr(sc, "status"), "ok")) notice(attr(sc, "status"))
                  else hint("정답 라벨: selfcheck/labels.csv")))),
          nav_panel("Upload",
            div(class = "pt-2",
                fileInput(ns("photo"), "JPG / PNG (끌어다 놓기 가능)", accept = c(".jpg", ".jpeg", ".png"),
                          buttonLabel = "Browse…", placeholder = "사진을 선택하세요"),
                checkboxInput(ns("autocrop"), "배경 제거 · 신발 영역 자동 자르기 (권장)", TRUE),
                checkboxInput(ns("autoflip"), "좌우 방향 자동 맞춤", TRUE),
                selectInput(ns("upload_truth"), "실제 카테고리 (알고 있다면)", c("Unknown" = "", CATEGORIES)),
                uiOutput(ns("upload_preview"))))
        ),
        hr(),
        sliderInput(ns("K"), "Number of PCs (K)", 1, K_MAX, K_DEFAULT, step = 1, ticks = FALSE),
        uiOutput(ns("k_info")),
        actionButton(ns("reset"), "↺ Reset", class = "btn-outline-secondary")
      ),

      # ---------------- 결과 ----------------
      ui_card("Search result", icon = "🔎",
              header_right = downloadButton(ns("dl_top5"), "Top 5 CSV", class = "btn-outline-primary btn-sm"),
              uiOutput(ns("result"))),
      ui_card("Top 5 table", icon = "📋", tableOutput(ns("top5_table")))
    )
  )
}

shoeSearchServer <- function(id, pca) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    sc <- list_selfcheck()

    observeEvent(input$random, {
      updateNumericInput(session, "cat_id", value = sample(pca$meta$id, 1))
    })
    observeEvent(input$reset, {
      nav_select("src", "Catalogue", session = session)
      updateNumericInput(session, "cat_id", value = 1)
      updateSliderInput(session, "K", value = K_DEFAULT)
      updateCheckboxInput(session, "autocrop", value = TRUE)
      updateCheckboxInput(session, "autoflip", value = TRUE)
      updateSelectInput(session, "upload_truth", selected = "")
      if (nrow(sc)) updateRadioButtons(session, "sc_file", selected = sc$photo[1])
    })

    K <- reactive({
      k <- input$K
      if (is.null(k) || is.na(k)) K_DEFAULT else as.integer(k)
    })

    # 선택된 쿼리 이미지 (모든 결과의 출발점)
    selected <- reactive({
      src <- input$src
      if (identical(src, "Catalogue")) {
        id <- suppressWarnings(as.integer(input$cat_id))
        validate(need(!is.na(id) && id %in% pca$meta$id,
                      "카탈로그 id 는 1 부터 2000 사이의 정수여야 합니다."))
        i <- match(id, pca$meta$id)
        list(x = pca$X[i, ], label = sprintf("Catalogue #%04d", id),
             truth = pca$meta$category[i], exclude = id, source = "Catalogue")
      } else if (identical(src, "Self-check")) {
        validate(need(nrow(sc) > 0, "selfcheck 사진이 없습니다."),
                 need(!is.null(input$sc_file), "Self-check 사진을 선택하세요."))
        f <- input$sc_file
        x <- tryCatch(mat_to_vec(preprocess_photo(file.path(PATH_SELFCHECK, f), crop = FALSE)),
                      error = function(e) NULL)
        validate(need(!is.null(x), "Self-check 사진을 읽지 못했습니다."))
        list(x = x, label = f, truth = sc$category[match(f, sc$photo)],
             exclude = NULL, source = "Self-check")
      } else {
        validate(need(!is.null(input$photo), "JPG 또는 PNG 사진을 업로드하세요."))
        x <- tryCatch(mat_to_vec(preprocess_photo(input$photo$datapath, crop = isTRUE(input$autocrop))),
                      error = function(e) NULL)
        validate(need(!is.null(x), "이미지를 읽을 수 없습니다. JPG/PNG 파일인지 확인해 주세요."))
        flipped <- FALSE
        if (isTRUE(input$autoflip)) {                 # 원본/좌우반전 중 Best match 가 더 가까운 쪽
          f <- search_auto_flip(pca, x, K())
          x <- f$x; flipped <- f$flipped
        }
        tr <- input$upload_truth
        list(x = x, label = paste0(input$photo$name, if (flipped) " (mirrored)" else ""),
             truth = if (nzchar(tr %||% "")) tr else NA,
             exclude = NULL, source = "Upload", flipped = flipped)
      }
    })

    result <- reactive({
      s <- selected()
      search_similar(pca, s$x, K(), TOP_N, exclude_id = s$exclude)
    })

    output$upload_preview <- renderUI({
      req(input$photo, identical(input$src, "Upload"))
      s <- tryCatch(selected(), error = function(e) NULL)
      if (is.null(s)) return(error_state("이미지를 읽을 수 없습니다."))
      x <- s$x
      orig <- tryCatch(base64enc::dataURI(file = input$photo$datapath,
                                          mime = if (grepl("png$", tolower(input$photo$name))) "image/png" else "image/jpeg"),
                       error = function(e) NULL)
      layout_columns(col_widths = c(6, 6),
          div(class = "text-center small text-muted",
              tags$img(src = orig, alt = "original", height = 96, class = "img-fluid border rounded"),
              div("Original")),
          div(class = "text-center small text-muted",
              shoe_img(gray_uri(x, scale = 2), "processed", 96),
              div(if (isTRUE(s$flipped)) "64×64 gray · mirrored" else "64×64 gray (PCA input)")))
    })

    output$k_info <- renderUI({
      k <- K()
      hint(
        HTML(sprintf("PC 1–%d 이 전체 분산의 <b>%.1f%%</b> 를 설명합니다. 4,096개 픽셀 대신 %d개의 숫자로 비교합니다.",
                     k, 100 * pca$cum_ratio[k], k)))
    })

    output$result <- renderUI({
      s <- selected(); r <- result()
      render_match_panel(pca, s$x, s$label, r, K(), s$truth)
    })

    output$top5_table <- renderTable({
      t <- result()$top
      t$id <- sprintf("%04d", t$id)
      names(t) <- c("Rank", "ID", "Category", "Distance (PC space)")
      t
    }, digits = 3, striped = TRUE, hover = TRUE, width = "100%")

    output$dl_top5 <- downloadHandler(
      filename = function() paste0("top5_", gsub("[^A-Za-z0-9]+", "_", selected()$label), "_K", K(), ".csv"),
      content = function(file) {
        t <- result()$top
        t$query <- selected()$label; t$K <- K(); t$predicted <- result()$prediction
        utils::write.csv(t, file, row.names = FALSE)
      })

    list(selected = selected, K = K, result = result)
  })
}
