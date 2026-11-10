# =============================================================================
# mod_our_shoes.R — Tab 3. Our Shoes
# our_shoes/ 폴더에 "이름_카테고리_번호.jpg" 형식으로 넣으면 자동 인식됩니다.
#   예) Whae_Sandals_1.jpg, Minji_Boots_2.png
# =============================================================================

normalize_category <- function(x) {
  key <- tolower(sub("s$", "", x))
  hit <- CATEGORIES[tolower(sub("s$", "", CATEGORIES)) == key]
  if (length(hit)) hit else NA_character_
}

list_our_shoes <- function(dir = PATH_OUR_SHOES) {
  files <- list.files(dir, pattern = "\\.(jpe?g|png)$", ignore.case = TRUE)
  if (!length(files)) return(data.frame(file = character(), member = character(),
                                        category = character(), n = character()))
  parts <- regmatches(files, regexec("^([^_]+)_([A-Za-z]+)_([0-9]+)\\.[A-Za-z]+$", files))
  ok <- lengths(parts) == 4
  df <- data.frame(
    file     = files[ok],
    member   = vapply(parts[ok], `[`, "", 2),
    category = vapply(parts[ok], function(p) normalize_category(p[3]), ""),
    n        = vapply(parts[ok], `[`, "", 4),
    stringsAsFactors = FALSE)
  attr(df, "skipped") <- files[!ok]
  df[order(df$member, df$category, df$n), ]
}

ourShoesUI <- function(id) {
  ns <- NS(id)
  tagList(
    page_header("Tab 3 · Never-seen photos", "Our Shoes",
                "Can your app recognize real shoes it has never seen? 팀원이 직접 찍은 신발 사진을 같은 전처리·같은 PCA 로 검색합니다."),
    uiOutput(ns("body"))
  )
}

ourShoesServer <- function(id, pca, search) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shoes <- list_our_shoes()

    # 앱 시작 시 1회 전처리 (사진 수 × 수 ms)
    vecs <- lapply(shoes$file, function(f)
      tryCatch(mat_to_vec(preprocess_photo(file.path(PATH_OUR_SHOES, f), crop = TRUE)),
               error = function(e) NULL))
    names(vecs) <- shoes$file

    output$body <- renderUI({
      if (!nrow(shoes)) {
        return(empty_state("아직 등록된 팀원 신발 사진이 없어요",
          p("1. 각 팀원이 자기 신발 3켤레를 촬영합니다 (신발만, 단색 배경)."),
          p(HTML("2. 파일 이름을 <code>이름_카테고리_번호.jpg</code> 로 저장합니다. 예: <code>Whae_Sandals_1.jpg</code>")),
          p(HTML("3. 카테고리는 <code>Boots / Sandals / Shoes / Slippers</code> 중 하나.")),
          p(HTML("4. 프로젝트의 <code>our_shoes/</code> 폴더에 넣고 앱을 다시 실행하면 자동으로 표시됩니다.")),
          icon = "👟"))
      }
      skipped <- attr(shoes, "skipped")
      tagList(
        if (length(skipped)) div(class = "alert-soft",
          sprintf("파일 이름 규칙에 맞지 않아 제외된 파일: %s", paste(skipped, collapse = ", "))),
        layout_columns(col_widths = breakpoints(sm = 12, lg = c(5, 7)),
          ui_card("Gallery", icon = "🖼️",
            subtitle = sprintf("%d명 · %d장 — 사진을 클릭하거나 아래 목록에서 선택",
                               length(unique(shoes$member)), nrow(shoes)),
            div(class = "filter-row",
                selectInput(ns("member"), "Member", c("All", unique(shoes$member))),
                selectInput(ns("cat"), "Category", c("All", CATEGORIES)),
                actionButton(ns("reset"), "↺", class = "btn-ghost btn-icon", title = "Reset filters")),
            selectInput(ns("pick"), "Choose a shoe", shoes$file, width = "100%"),
            uiOutput(ns("gallery"))),
          div(
            ui_card("Result", icon = "🔎", subtitle = "Shoe Search 탭의 K 값을 공유합니다",
                    uiOutput(ns("result"))),
            ui_card("Team summary", icon = "📊", class = "compact",
                    uiOutput(ns("summary")))))
      )
    })

    observeEvent(input$reset, {
      updateSelectInput(session, "member", selected = "All")
      updateSelectInput(session, "cat", selected = "All")
    })
    observeEvent(input$card_click, {
      updateSelectInput(session, "pick", selected = input$card_click)
    })

    filtered <- reactive({
      d <- shoes
      if (!is.null(input$member) && input$member != "All") d <- d[d$member == input$member, ]
      if (!is.null(input$cat) && input$cat != "All") d <- d[d$category %in% input$cat, ]
      d
    })

    output$gallery <- renderUI({
      d <- filtered()
      if (!nrow(d)) return(empty_state("조건에 맞는 사진이 없습니다", p("필터를 초기화해 보세요."), icon = "🔍"))
      groups <- split(d, d$member)
      tagList(lapply(names(groups), function(m) {
        g <- groups[[m]]
        div(class = "member-group",
            h5(class = "member-name", m),
            div(class = "our-grid",
                lapply(seq_len(nrow(g)), function(i) {
                  f <- g$file[i]; v <- vecs[[f]]
                  tags$button(type = "button",
                    class = paste("our-card", if (identical(f, input$pick)) "selected"),
                    onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("card_click"), f),
                    `aria-label` = paste("Select", f),
                    if (!is.null(v)) tags$img(src = gray_uri(v), width = 88, height = 88, alt = f)
                    else div(class = "img-missing", "?"),
                    span(class = "fname", f),
                    cat_chip(g$category[i] %||% "Unknown"))
                })))
      }))
    })

    output$result <- renderUI({
      req(input$pick)
      f <- input$pick; v <- vecs[[f]]
      if (is.null(v)) return(error_state(paste(f, "을(를) 읽을 수 없습니다.")))
      truth <- shoes$category[shoes$file == f]
      res <- search_similar(pca, v, search$K(), TOP_N)
      render_match_panel(pca, v, f, res, search$K(), truth)
    })

    output$summary <- renderUI({
      K <- search$K()
      preds <- vapply(shoes$file, function(f) {
        v <- vecs[[f]]; if (is.null(v)) NA_character_ else search_similar(pca, v, K)$prediction
      }, "")
      ok <- !is.na(preds) & !is.na(shoes$category)
      acc <- if (any(ok)) mean(preds[ok] == shoes$category[ok]) else NA
      tagList(
        div(class = "kpi-row",
            div(class = "kpi", span(class = "kpi-label", "Photos"), span(class = "kpi-value", nrow(shoes))),
            div(class = "kpi", span(class = "kpi-label", "Correct"),
                span(class = "kpi-value", sprintf("%d / %d", sum(preds[ok] == shoes$category[ok]), sum(ok)))),
            div(class = "kpi", span(class = "kpi-label", sprintf("Accuracy (K=%d)", K)),
                span(class = "kpi-value", if (is.na(acc)) "—" else sprintf("%.0f%%", 100 * acc)))),
        tags$table(class = "table table-sm mini-table",
          tags$thead(tags$tr(tags$th("Photo"), tags$th("Actual"), tags$th("Predicted"), tags$th(""))),
          tags$tbody(lapply(seq_along(preds), function(i) tags$tr(
            tags$td(shoes$file[i]), tags$td(shoes$category[i]), tags$td(preds[i]),
            tags$td(if (isTRUE(preds[i] == shoes$category[i])) "✓" else "✗"))))))
    })
  })
}
