# =============================================================================
# mod_data1.R — Tab 4. Data 1 (여대 상권 비교, 기술통계)
# 데이터 연결 전에는 "Dataset Not Connected" 상태, DEMO 모드는 명확히 라벨링
# =============================================================================

data1UI <- function(id) {
  ns <- NS(id)
  tagList(
    page_header("Tab 4 · Project 1 dataset", "Data 1 — University Commercial Districts",
                "이화여대 · 숙명여대 · 성신여대 주변 상권을 기술통계(filter, sort, count, mean, median, rate, group summary)로 비교합니다."),
    radioButtons(ns("mode"), NULL, c("Real data" = "real", "Demo mode" = "demo"), inline = TRUE),
    hint("Demo mode 는 UI 시연용 가상 데이터이며 분석 결과가 아닙니다."),
    uiOutput(ns("body"))
  )
}

data1Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    real <- load_data1_real()
    demo <- make_demo_data1()

    state <- reactive(if (identical(input$mode, "demo")) c(demo, demo = TRUE) else c(real, demo = FALSE))

    output$body <- renderUI({
      st <- state()
      if (!isTRUE(st$ok)) {
        return(tagList(
          card(class = "border-warning mt-3",
            card_header(span(class = "badge text-bg-warning", "● Dataset Not Connected")),
            card_body(
              h4("Project 1 최종 데이터셋이 아직 연결되지 않았습니다"),
              p(st$reason),
              tags$ol(
                tags$li(HTML("최종 CSV 를 <code>data1/data1.csv</code> 로 저장합니다.")),
                tags$li(HTML("<code>data1/data1_config.R</code> 에서 university / period / industry / metrics 컬럼 이름을 실제 이름으로 바꿉니다.")),
                tags$li("앱을 다시 실행하면 필터·그래프·요약 통계가 실제 데이터로 갱신됩니다.")),
              hint("실제 데이터 없이 숫자를 표시하지 않습니다. 화면 구성을 보려면 위의 Demo mode 를 선택하세요.")))))
      }
      periods <- if (st$has$period) sort(unique(st$df$.period)) else NULL
      inds <- if (st$has$industry) sort(unique(st$df$.industry)) else NULL
      tagList(
        if (st$demo) div(class = "alert alert-warning text-center fw-bold fs-5 mt-2", role = "alert",
                         "⚠️ DEMO DATA — NOT ACTUAL RESULTS"),
        if (!st$demo && isTRUE(st$unmatched > 0))
          notice(sprintf("university 값이 매핑되지 않은 %d행은 제외되었습니다 (data1_config.R 의 DATA1_UNIV_VALUES 확인).", st$unmatched)),
        layout_columns(col_widths = breakpoints(sm = 12, lg = c(3, 9)),
          div(
            ui_card("Filters", icon = "⚙️",
              checkboxGroupInput(ns("univ"), "University",
                                 choiceNames = unname(UNIV_LABELS[names(UNIV_COLORS)]),
                                 choiceValues = names(UNIV_COLORS), selected = names(UNIV_COLORS)),
              if (!is.null(periods)) {
                if (is.numeric(periods)) sliderInput(ns("period"), "Period", min(periods), max(periods),
                                                     range(periods), step = 1, sep = "")
                else selectInput(ns("period_sel"), "Period", periods, periods, multiple = TRUE)
              } else hint("🚫 Period — 미지원 (컬럼 미매핑)"),
              if (!is.null(inds)) selectInput(ns("industry"), "Industry", inds, inds, multiple = TRUE)
              else hint("🚫 Industry — 미지원 (컬럼 미매핑)"),
              selectInput(ns("metric"), "Metric", stats::setNames(st$metrics, st$labels)),
              radioButtons(ns("stat"), "Statistic", c("Mean" = "mean", "Median" = "median", "Sum" = "sum", "Count" = "count"),
                           inline = TRUE),
              actionButton(ns("reset"), "↺ Reset filters", class = "btn-outline-secondary w-100"))),
          div(
            uiOutput(ns("kpis")),
            layout_columns(col_widths = breakpoints(sm = 12, lg = c(6, 6)),
              ui_card("By university", icon = "🏫", plotly::plotlyOutput(ns("p_univ"), height = "300px")),
              ui_card("By industry", icon = "🏪",
                      if (st$has$industry) plotly::plotlyOutput(ns("p_ind"), height = "300px")
                      else hint("🚫 업종 컬럼이 매핑되지 않아 미지원"))),
            ui_card("Trend over period", icon = "📈",
                    if (st$has$period) plotly::plotlyOutput(ns("p_trend"), height = "300px")
                    else hint("🚫 기간 컬럼이 매핑되지 않아 미지원")),
            ui_card("Group summary table", icon = "📋",
                    header_right = downloadButton(ns("dl"), "CSV", class = "btn-outline-primary btn-sm"),
                    DT::DTOutput(ns("table"))),
            h4(class = "fw-bold mt-3", "Three descriptive findings"),
            uiOutput(ns("analyses"))))
      )
    })

    observeEvent(input$reset, {
      st <- state(); req(st$ok)
      updateCheckboxGroupInput(session, "univ", selected = names(UNIV_COLORS))
      if (st$has$period) {
        p <- sort(unique(st$df$.period))
        if (is.numeric(p)) updateSliderInput(session, "period", value = range(p))
        else updateSelectInput(session, "period_sel", selected = p)
      }
      if (st$has$industry) updateSelectInput(session, "industry", selected = sort(unique(st$df$.industry)))
      updateRadioButtons(session, "stat", selected = "mean")
    })

    filtered <- reactive({
      st <- state(); req(st$ok)
      d <- st$df
      d <- d[d$.univ %in% (input$univ %||% character()), , drop = FALSE]
      if (st$has$period) {
        if (!is.null(input$period) && is.numeric(d$.period))
          d <- d[d$.period >= input$period[1] & d$.period <= input$period[2], , drop = FALSE]
        if (!is.null(input$period_sel)) d <- d[d$.period %in% input$period_sel, , drop = FALSE]
      }
      if (st$has$industry && !is.null(input$industry)) d <- d[d$.industry %in% input$industry, , drop = FALSE]
      d
    })

    metric <- reactive({ req(input$metric); input$metric })
    stat_label <- reactive(c(mean = "Mean", median = "Median", sum = "Sum", count = "Count")[[input$stat %||% "mean"]])

    output$kpis <- renderUI({
      d <- filtered(); m <- metric(); st <- state()
      if (!nrow(d)) return(empty_state("필터 조건에 맞는 데이터가 없습니다", p("필터를 넓히거나 초기화하세요."), icon = "🔍"))
      gs <- group_summary(d, m)
      # 대학별 요약 카드: 대학 고정 컬러를 value_box 테마(R 인자)로 지정
      do.call(layout_column_wrap, c(list(width = "220px", fill = FALSE),
          lapply(seq_len(nrow(gs)), function(i) {
            k <- gs$group[i]
            val <- if ((input$stat %||% "mean") == "count") gs$n[i] else gs[[input$stat %||% "mean"]][i]
            value_box(
              title = UNIV_LABELS[[k]],
              value = format(round(val, 2), big.mark = ","),
              p(sprintf("%s of %s · n = %d · share %.1f%%", stat_label(), st$labels[[m]], gs$n[i], gs$share_pct[i])),
              theme = value_box_theme(bg = UNIV_COLORS[[k]], fg = "#FFFFFF"))
          })))
    })

    base_layout <- function(p, ytitle) {
      p |> plotly::layout(yaxis = list(title = ytitle), legend = list(orientation = "h", y = -0.25),
                          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") |>
        plotly::config(displaylogo = FALSE)
    }
    stat_by <- function(d, by) {
      s <- input$stat %||% "mean"; m <- metric()
      agg <- stats::aggregate(d[[m]], by = d[by], FUN = function(x) summarise_stat(x, s))
      names(agg)[ncol(agg)] <- "value"; agg
    }

    output$p_univ <- plotly::renderPlotly({
      d <- filtered(); validate(need(nrow(d) > 0, "데이터 없음"))
      a <- stat_by(d, ".univ")
      plotly::plot_ly(a, x = ~factor(UNIV_LABELS[.univ], levels = UNIV_LABELS), y = ~value, type = "bar",
                      marker = list(color = unname(UNIV_COLORS[a$.univ])),
                      text = ~round(value, 2), textposition = "outside",
                      hovertemplate = "%{x}<br>%{y:,.2f}<extra></extra>") |>
        plotly::layout(xaxis = list(title = "")) |>
        base_layout(paste(stat_label(), "of", state()$labels[[metric()]]))
    })

    output$p_ind <- plotly::renderPlotly({
      d <- filtered(); validate(need(nrow(d) > 0, "데이터 없음"))
      a <- stat_by(d, c(".industry", ".univ"))
      p <- plotly::plot_ly()
      for (k in intersect(names(UNIV_COLORS), unique(a$.univ))) {
        s <- a[a$.univ == k, ]
        p <- plotly::add_bars(p, data = s, x = ~.industry, y = ~value, name = UNIV_LABELS[[k]],
                              marker = list(color = UNIV_COLORS[[k]]))
      }
      p |> plotly::layout(barmode = "group", xaxis = list(title = "Industry")) |>
        base_layout(stat_label())
    })

    output$p_trend <- plotly::renderPlotly({
      d <- filtered(); validate(need(nrow(d) > 0, "데이터 없음"))
      a <- stat_by(d, c(".period", ".univ")); a <- a[order(a$.period), ]
      p <- plotly::plot_ly()
      for (k in intersect(names(UNIV_COLORS), unique(a$.univ))) {
        s <- a[a$.univ == k, ]
        p <- plotly::add_trace(p, data = s, x = ~.period, y = ~value, type = "scatter", mode = "lines+markers",
                               name = UNIV_LABELS[[k]], line = list(color = UNIV_COLORS[[k]], width = 2.5),
                               marker = list(color = UNIV_COLORS[[k]], size = 7))
      }
      p |> plotly::layout(xaxis = list(title = "Period")) |> base_layout(stat_label())
    })

    summary_table <- reactive({
      d <- filtered(); req(nrow(d) > 0)
      gs <- group_summary(d, metric())
      gs$group <- UNIV_LABELS[gs$group]
      names(gs) <- c("University", "Count (n)", "Share (%)", "Mean", "Median", "Sum")
      gs
    })

    output$table <- DT::renderDT({
      DT::datatable(summary_table(), rownames = FALSE,
                    options = list(dom = "t", ordering = TRUE, pageLength = 10)) |>
        DT::formatRound(c("Share (%)", "Mean", "Median", "Sum"), 2)
    })

    output$dl <- downloadHandler(
      filename = function() sprintf("data1_summary_%s%s.csv", metric(), if (isTRUE(state()$demo)) "_DEMO" else ""),
      content = function(file) {
        t <- summary_table()
        if (isTRUE(state()$demo)) t$note <- "DEMO DATA - NOT ACTUAL RESULTS"
        utils::write.csv(t, file, row.names = FALSE, fileEncoding = "UTF-8")
      })

    output$analyses <- renderUI({
      st <- state(); d <- filtered()
      an <- st$analyses %||% list()
      filt_txt <- sprintf("Universities: %s%s%s",
                          paste(input$univ, collapse = ", "),
                          if (!is.null(input$period)) sprintf(" · Period %s–%s", input$period[1], input$period[2]) else "",
                          if (!is.null(input$industry)) sprintf(" · Industries: %d selected", length(input$industry)) else "")
      do.call(layout_column_wrap, c(list(width = "300px", fill = FALSE), lapply(an, function(a) {
        has_metric <- !is.na(a$metric %||% NA) && a$metric %in% st$metrics
        nums <- if (has_metric && nrow(d)) {
          gs <- group_summary(d, a$metric)
          val <- if (a$stat == "count") gs$n else gs[[a$stat]]
          tags$ul(lapply(seq_len(nrow(gs)), function(i)
            tags$li(sprintf("%s: %s", UNIV_LABELS[[gs$group[i]]], format(round(val[i], 2), big.mark = ",")))))
        } else hint("🚫 지표가 설정되지 않음 (data1_config.R 의 metric)")
        ui_card(a$title,
                tags$dl(
                  tags$dt("Question"), tags$dd(a$question),
                  tags$dt("Inputs (current filters)"), tags$dd(filt_txt),
                  tags$dt(sprintf("Result — %s of %s", a$stat, if (has_metric) st$labels[[a$metric]] else "?")),
                  tags$dd(nums),
                  tags$dt("Why it matters"), tags$dd(a$interpretation),
                  tags$dt("Next analysis"), tags$dd(a$next_step)))
      })))
    })
  })
}
