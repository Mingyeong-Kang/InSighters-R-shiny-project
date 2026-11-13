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
                "Can your app recognize real shoes it has never seen? Each team member's own shoe photos go through the same preprocessing and the same PCA as the catalogue."),
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
        return(empty_state("No team shoe photos yet",
          p("1. Each member photographs 3 of their own shoes (shoe only, plain background)."),
          p(HTML("2. Name each file <code>Name_Category_n.jpg</code>, e.g. <code>Whae_Sandals_1.jpg</code>")),
          p(HTML("3. Category is one of <code>Boots / Sandals / Shoes / Slippers</code>.")),
          p(HTML("4. Put the files in the <code>our_shoes/</code> folder and restart the app; they appear automatically.")),
          icon = "👟"))
      }
      skipped <- attr(shoes, "skipped")
      tagList(
        if (length(skipped)) notice(
          sprintf("Skipped (file name is not Name_Category_n.jpg): %s", paste(skipped, collapse = ", "))),
        layout_columns(col_widths = breakpoints(sm = 12, lg = c(5, 7)),
          ui_card("Gallery", icon = "🖼️",
            subtitle = sprintf("%d members · %d photos — pick one",
                               length(unique(shoes$member)), nrow(shoes)),
            layout_columns(col_widths = c(5, 5, 2),
                selectInput(ns("member"), "Member", c("All", unique(shoes$member))),
                selectInput(ns("cat"), "Category", c("All", CATEGORIES)),
                div(class = "pt-4", actionButton(ns("reset"), "↺", class = "btn-outline-secondary",
                                                 title = "Reset filters"))),
            uiOutput(ns("gallery"))),
          div(
            ui_card("Result", icon = "🔎", subtitle = "Uses the same K as the Shoe Search tab",
                    uiOutput(ns("result"))),
            ui_card("Team summary", icon = "📊", uiOutput(ns("summary")))))
      )
    })

    observeEvent(input$reset, {
      updateSelectInput(session, "member", selected = "All")
      updateSelectInput(session, "cat", selected = "All")
    })

    filtered <- reactive({
      d <- shoes
      if (!is.null(input$member) && input$member != "All") d <- d[d$member == input$member, ]
      if (!is.null(input$cat) && input$cat != "All") d <- d[d$category %in% input$cat, ]
      d
    })

    # 갤러리 = 선택 위젯: 사진·이름·카테고리를 라벨로 가진 radioButtons
    output$gallery <- renderUI({
      d <- filtered()
      if (!nrow(d)) return(empty_state("No photos match these filters", p("Try resetting the filters."), icon = "🔍"))
      current <- isolate(input$pick)
      labels <- lapply(seq_len(nrow(d)), function(i) {
        f <- d$file[i]; v <- vecs[[f]]
        span(if (!is.null(v)) tags$img(src = gray_uri(v, scale = 2), width = 88, height = 88, alt = f,
                                       class = "border rounded")
             else span(class = "badge text-bg-secondary", "Read error"),
             strong(d$member[i]), cat_chip(d$category[i] %||% "Unknown"),
             span(class = "small text-muted", f))
      })
      radioButtons(ns("pick"), "Choose a shoe", choiceNames = labels, choiceValues = d$file,
                   selected = if (!is.null(current) && current %in% d$file) current else d$file[1],
                   width = "100%")
    })

    output$result <- renderUI({
      req(input$pick)
      f <- input$pick; v <- vecs[[f]]
      if (is.null(v)) return(error_state(paste("Could not read", f)))
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
        layout_column_wrap(width = "160px", fill = FALSE,
            value_box("Photos", nrow(shoes), theme = "light"),
            value_box("Correct", sprintf("%d / %d", sum(preds[ok] == shoes$category[ok]), sum(ok)), theme = "light"),
            value_box(sprintf("Accuracy (K=%d)", K), if (is.na(acc)) "—" else sprintf("%.0f%%", 100 * acc),
                      theme = "primary")),
        tags$table(class = "table table-sm",
          tags$thead(tags$tr(tags$th("Photo"), tags$th("Actual"), tags$th("Predicted"), tags$th(""))),
          tags$tbody(lapply(seq_along(preds), function(i) tags$tr(
            tags$td(shoes$file[i]), tags$td(shoes$category[i]), tags$td(preds[i]),
            tags$td(if (isTRUE(preds[i] == shoes$category[i])) "✓" else "✗"))))))
    })
  })
}
