# =============================================================================
# mod_shoe_search.R — Tab 1. Shoe Search
# 반환값: list(selected = reactive(쿼리 정보), K = reactive(K))  -> 다른 탭과 공유
# =============================================================================

list_selfcheck <- function() {
  lab_path <- file.path(PATH_SELFCHECK, "labels.csv")
  files <- sort(list.files(PATH_SELFCHECK, pattern = "\\.(png|jpe?g)$", ignore.case = TRUE))
  labels <- if (file.exists(lab_path)) utils::read.csv(lab_path, stringsAsFactors = FALSE) else
    data.frame(photo = character(), category = character())
  data.frame(photo = files,
             category = labels$category[match(files, labels$photo)],
             stringsAsFactors = FALSE)
}

shoeSearchUI <- function(id) {
  ns <- NS(id)
  sc <- list_selfcheck()
  sc_choices_names <- lapply(seq_len(nrow(sc)), function(i) {
    div(class = "sc-option",
        tags$img(src = gray_uri(mat_to_vec(preprocess_photo(file.path(PATH_SELFCHECK, sc$photo[i]), crop = FALSE))),
                 width = 64, height = 64, alt = sc$photo[i]),
        span(sub("\\.[a-z]+$", "", sc$photo[i])))
  })

  tagList(
    page_header("Tab 1 · PCA image search", "Shoe Search",
                "신발 사진 한 장을 고르면, 2,000장 카탈로그를 PCA 공간에 투영해 가장 가까운 신발 5개를 찾고 카테고리를 예측합니다."),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(4, 8)),

      # ---------------- 입력 패널 ----------------
      div(class = "sticky-col",
        ui_card("1 · Choose a photo", icon = "📷",
          div(class = "segmented",
              radioButtons(ns("src"), NULL, c("Catalogue", "Self-check", "Upload"), inline = TRUE)),

          conditionalPanel("input.src == 'Catalogue'", ns = ns,
            div(class = "id-row",
                numericInput(ns("cat_id"), "Catalogue id (1–2000)", 1, 1, 2000, step = 1),
                actionButton(ns("random"), "🎲 Random", class = "btn-ghost")),
            p(class = "hint", "검색할 때 이 사진 자체는 결과에서 제외됩니다.")),

          conditionalPanel("input.src == 'Self-check'", ns = ns,
            if (nrow(sc) == 0) empty_state("selfcheck 폴더가 비어 있어요",
                                           p("selfcheck/ 폴더에 사진과 labels.csv 를 넣어 주세요."))
            else div(class = "sc-picker",
                     radioButtons(ns("sc_file"), "Self-check photo", choiceNames = sc_choices_names,
                                  choiceValues = sc$photo, inline = TRUE))),

          conditionalPanel("input.src == 'Upload'", ns = ns,
            div(class = "dropzone",
                fileInput(ns("photo"), NULL, accept = c(".jpg", ".jpeg", ".png"),
                          buttonLabel = "Browse…", placeholder = "또는 여기로 사진을 끌어다 놓으세요"),
                p(class = "hint", "JPG / PNG · 자동으로 흑백 변환 → 신발 영역 자르기 → 64×64")),
            checkboxInput(ns("autocrop"), "배경 제거 · 신발 영역 자동 자르기 (권장)", TRUE),
            selectInput(ns("upload_truth"), "실제 카테고리 (알고 있다면)",
                        c("Unknown" = "", CATEGORIES)),
            uiOutput(ns("upload_preview")))
        ),
        ui_card("2 · Number of PCs", icon = "🎚️",
          sliderInput(ns("K"), "Number of PCs (K)", 1, K_MAX, K_DEFAULT, step = 1, ticks = FALSE),
          uiOutput(ns("k_info")),
          div(class = "btn-row",
              actionButton(ns("reset"), "↺ Reset", class = "btn-ghost")))
      ),

      # ---------------- 결과 패널 ----------------
      div(
        ui_card("Search result", icon = "🔎",
                header_right = downloadButton(ns("dl_top5"), "Top 5 CSV", class = "btn-ghost btn-sm"),
                uiOutput(ns("result"))),
        ui_card("Top 5 table", icon = "📋", class = "compact",
                tableOutput(ns("top5_table")))
      )
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
      updateRadioButtons(session, "src", selected = "Catalogue")
      updateNumericInput(session, "cat_id", value = 1)
      updateSliderInput(session, "K", value = K_DEFAULT)
      updateCheckboxInput(session, "autocrop", value = TRUE)
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
        tr <- input$upload_truth
        list(x = x, label = input$photo$name, truth = if (nzchar(tr %||% "")) tr else NA,
             exclude = NULL, source = "Upload")
      }
    })

    result <- reactive({
      s <- selected()
      search_similar(pca, s$x, K(), TOP_N, exclude_id = s$exclude)
    })

    output$upload_preview <- renderUI({
      req(input$photo)
      x <- tryCatch(mat_to_vec(preprocess_photo(input$photo$datapath, crop = isTRUE(input$autocrop))),
                    error = function(e) NULL)
      if (is.null(x)) return(error_state("이미지를 읽을 수 없습니다."))
      orig <- tryCatch(base64enc::dataURI(file = input$photo$datapath,
                                          mime = if (grepl("png$", tolower(input$photo$name))) "image/png" else "image/jpeg"),
                       error = function(e) NULL)
      div(class = "upload-preview fade-in",
          div(tags$img(src = orig, class = "orig zoomable", alt = "original"), span("Original")),
          span(class = "arrow", "→"),
          div(shoe_img(gray_uri(x), "processed", 96), span("64×64 gray (PCA input)")))
    })

    output$k_info <- renderUI({
      k <- K()
      p(class = "hint",
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
