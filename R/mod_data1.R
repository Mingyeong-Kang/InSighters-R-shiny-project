# =============================================================================
# mod_data1.R — Tab 4. Data 1 (여대 상권 비교: 이화여대·숙명여대·성신여대 음식점 개·폐업)
# 원본 인허가 CSV 를 읽어 연도별 개업·폐업 통계를 앱 안에서 직접 계산한다.
# 데이터 파일이 없으면 "Dataset Not Connected" 안내를 보여 준다.
# =============================================================================

data1UI <- function(id) {
  ns <- NS(id)
  tagList(
    page_header("Tab 4 · Project 1 dataset", "Data 1 — Women’s University Commercial District Comparison",
                "Compares yearly restaurant openings and closures around Ewha Womans University (Daehyeon-dong), Sookmyung Women's University (Cheongpa-dong), and Sungshin Women's University (Dongseon-dong) using descriptive statistics (filter, count, mean, median, rate, group summary)."),
    uiOutput(ns("body"))
  )
}

data1Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    st <- load_data1_real()           # 앱 시작 시 1회 로드
    
    lab <- function(k) unname(st$dong_labels[k])    # 동네 키 → "Ewha (Daehyeon-dong)"
    
    stat_en <- c(mean = "Mean", median = "Median", sum = "Sum")
    
    # 통계값 표시 (비율 지표는 %, 나머지는 숫자)
    fmt_stat <- function(x, metric, stat) {
      digits <- if (metric %in% DATA1_RATE_METRICS || stat %in% c("mean", "median")) 2 else 0
      fmt_metric(x, metric, digits)
    }
    
    # ---- 화면 ------------------------------------------------------------------
    output$body <- renderUI({
      if (!isTRUE(st$ok)) {
        return(card(class = "border-warning mt-3",
                    card_header(span(class = "badge text-bg-warning", "● Dataset Not Connected")),
                    card_body(
                      h4("The Project 1 dataset is not connected yet"),
                      p(st$reason),
                      tags$ol(
                        tags$li(HTML("Save the source CSV (the merged restaurant file for Cheongpa-dong, Dongseon-dong, and Daehyeon-dong) as <code>data1/data1.csv</code>.")),
                        tags$li(HTML("In <code>data1/data1_config.R</code>, check the column names for administrative district, license date, closure date, and business type.")),
                        tags$li("Restart the app and the filters, charts, and summary statistics will be computed from the real data.")),
                      hint("No numbers are shown without real data."))))
      }
      
      last_year <- max(st$years)
      partial <- st$cutoff < as.Date(sprintf("%d-12-31", last_year))
      tagList(
        if (partial)
          notice(sprintf("%d is a partial year that only includes data up to the last date in the dataset (%s). For full-year comparisons, narrow the year range to end at %d.",
                         last_year, format(st$cutoff, "%Y-%m-%d"), last_year - 1)),
        layout_columns(col_widths = breakpoints(sm = 12, lg = c(3, 9)),
                       div(
                         ui_card("Filters", icon = "⚙️",
                                 checkboxGroupInput(ns("dong"), "University (district)", choiceNames = unname(lab(st$dongs)),
                                                    choiceValues = st$dongs, selected = st$dongs),
                                 sliderInput(ns("years"), "Year range", min(st$years), max(st$years),
                                             range(st$years), step = 1, sep = ""),
                                 selectInput(ns("industry"), "Business type (multiple)", st$industries, st$industries, multiple = TRUE),
                                 actionButton(ns("reset"), "↺ Reset filters", class = "btn-outline-secondary w-100")),
                         hint("Openings = number of establishments whose license date falls in that year. Closures = number of establishments whose closure date falls in that year. Net change = Openings − Closures. Opening rate / Closure rate = each count ÷ (establishments operating at the start of the year + that year's openings).")),
                       div(
                         uiOutput(ns("kpis")),
                         layout_columns(col_widths = breakpoints(sm = 12, lg = c(6, 6)),
                                        ui_card("1. Opening Rate Comparison", icon = "🟢", plotly::plotlyOutput(ns("p_open"), height = "320px")),
                                        ui_card("2. Closure Rate Comparison", icon = "🔴", plotly::plotlyOutput(ns("p_close"), height = "320px"))),
                         layout_columns(col_widths = breakpoints(sm = 12, lg = c(6, 6)),
                                        ui_card("3. Average Opening vs. Closure Rate by University", icon = "⚖️", plotly::plotlyOutput(ns("p_avg"), height = "320px")),
                                        ui_card("5. Cumulative Net Change Comparison", icon = "📊", plotly::plotlyOutput(ns("p_cum"), height = "320px"))),
                         ui_card("4. Net Change (Openings − Closures)", icon = "➕", plotly::plotlyOutput(ns("p_net"), height = "320px")),
                         ui_card("Group summary table", icon = "📋",
                                 header_right = downloadButton(ns("dl"), "CSV", class = "btn-outline-primary btn-sm"),
                                 DT::DTOutput(ns("table"))),
                                                  h4(class = "fw-bold mt-3", "Descriptive findings"),
                         uiOutput(ns("analyses"))))
      )
    })
    
    observeEvent(input$reset, {
      updateCheckboxGroupInput(session, "dong", selected = st$dongs)
      updateSliderInput(session, "years", value = range(st$years))
      updateSelectInput(session, "industry", selected = st$industries)
    })
    
    # ---- 데이터 흐름: 업태 필터 → 동네×연도 집계 → 동네·연도 필터 ---------------
    yearly_all <- reactive({
      ev <- st$events
      ev <- ev[ev$industry %in% (input$industry %||% character()), , drop = FALSE]
      validate(need(nrow(ev) > 0, "Select at least one business type."))
      build_yearly(ev, st$dongs, st$years)
    })
    
    sel_dongs <- reactive(intersect(st$dongs, input$dong %||% character()))
    
    filtered <- reactive({
      req(input$years)
      d <- yearly_all()
      d[d$.dong %in% sel_dongs() & d$.year >= input$years[1] & d$.year <= input$years[2], , drop = FALSE]
    })
    
    # ---- KPI: 동네별 누적 순증감 + 평균 개업률·폐업률 ----------------------------
    output$kpis <- renderUI({
      d <- filtered()
      if (!nrow(d)) return(empty_state("No data matches the current filters", p("Select a university or widen the filters."), icon = "🔍"))
      net <- group_summary(d, "net_n", order = st$dongs)
      orr <- group_summary(d, "open_rate", order = st$dongs)
      crr <- group_summary(d, "close_rate", order = st$dongs)
      do.call(layout_column_wrap, c(list(width = "220px", fill = FALSE),
                                    lapply(seq_len(nrow(net)), function(i) {
                                      k <- net$group[i]
                                      value_box(
                                        title = paste0(lab(k), " · Cumulative net change"),
                                        value = sprintf("%+d", as.integer(net$sum[i])),
                                        p(sprintf("Avg. opening rate %s · Avg. closure rate %s", fmt_metric(orr$mean[i], "open_rate"), fmt_metric(crr$mean[i], "close_rate"))),
                                        theme = value_box_theme(bg = st$colors[[k]], fg = "#FFFFFF"))
                                    })))
    })
    
    # ---- 그래프 ----------------------------------------------------------------
    base_layout <- function(p, ytitle, yfmt = NULL) {
      p |> plotly::layout(yaxis = list(title = ytitle, tickformat = yfmt),
                          legend = list(orientation = "h", y = -0.25),
                          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") |>
        plotly::config(displaylogo = FALSE)
    }
    
    rate_trend <- function(metric, ytitle) {
      d <- filtered(); validate(need(nrow(d) > 0, "No data"))
      p <- plotly::plot_ly()
      for (k in intersect(st$dongs, unique(as.character(d$.dong)))) {
        s <- d[d$.dong == k, ]; s <- s[order(s$.year), ]
        p <- plotly::add_trace(p, x = s$.year, y = s[[metric]], type = "scatter", mode = "lines+markers",
                               name = lab(k), line = list(color = st$colors[[k]], width = 2.5),
                               marker = list(color = st$colors[[k]], size = 7),
                               hovertemplate = paste0("%{x} · %{y:.2%}<extra>", lab(k), "</extra>"))
      }
      p |> plotly::layout(xaxis = list(title = "Year", dtick = 1)) |> base_layout(ytitle, ".0%")
    }
    
    output$p_open  <- plotly::renderPlotly(rate_trend("open_rate", "Opening rate"))
    output$p_close <- plotly::renderPlotly(rate_trend("close_rate", "Closure rate"))
    
    output$p_avg <- plotly::renderPlotly({
      d <- filtered(); validate(need(nrow(d) > 0, "No data"))
      o <- group_summary(d, "open_rate", order = st$dongs)
      c <- group_summary(d, "close_rate", order = st$dongs)
      plotly::plot_ly() |>
        plotly::add_bars(x = lab(o$group), y = o$mean, name = "Opening rate",
                         marker = list(color = st$rate_colors[["open"]]),
                         text = sprintf("%.2f%%", 100 * o$mean), textposition = "outside",
                         hovertemplate = "%{x}<br>Avg. opening rate %{y:.2%}<extra></extra>") |>
        plotly::add_bars(x = lab(c$group), y = c$mean, name = "Closure rate",
                         marker = list(color = st$rate_colors[["close"]]),
                         text = sprintf("%.2f%%", 100 * c$mean), textposition = "outside",
                         hovertemplate = "%{x}<br>Avg. closure rate %{y:.2%}<extra></extra>") |>
        plotly::layout(barmode = "group",
                       xaxis = list(title = "", categoryorder = "array", categoryarray = lab(o$group))) |>
        base_layout("Average", ".0%")
    })
    
    output$p_net <- plotly::renderPlotly({
      d <- filtered(); validate(need(nrow(d) > 0, "No data"))
      p <- plotly::plot_ly()
      for (k in intersect(st$dongs, unique(as.character(d$.dong)))) {
        s <- d[d$.dong == k, ]; s <- s[order(s$.year), ]
        p <- plotly::add_bars(p, x = s$.year, y = s$net_n, name = lab(k), marker = list(color = st$colors[[k]]),
                              hovertemplate = paste0("%{x} · %{y:+d}<extra>", lab(k), "</extra>"))
      }
      p |> plotly::layout(barmode = "group", xaxis = list(title = "Year", dtick = 1),
                          shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                                             line = list(color = "#6B7280", width = 1)))) |>
        base_layout("Net change (count)")
    })
    
    output$p_cum <- plotly::renderPlotly({
      d <- filtered(); validate(need(nrow(d) > 0, "No data"))
      g <- group_summary(d, "net_n", order = st$dongs)
      plotly::plot_ly(x = lab(g$group), y = g$sum, type = "bar",
                      marker = list(color = unname(st$colors[g$group])),
                      text = sprintf("%+d", as.integer(g$sum)), textposition = "outside",
                      hovertemplate = "%{x}<br>Cumulative net change %{y:+d}<extra></extra>") |>
        plotly::layout(xaxis = list(title = "", categoryorder = "array", categoryarray = lab(g$group)),
                       shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                                          line = list(color = "#6B7280", width = 1)))) |>
        base_layout("Cumulative net change (count)")
    })
    
    # ---- 요약표 (평균 / 중앙값 / 합계) -------------------------------------------
    summary_table <- reactive({
      d <- filtered(); req(nrow(d) > 0)
      o <- group_summary(d, "open_rate", order = st$dongs)
      c <- group_summary(d, "close_rate", order = st$dongs)
      on <- group_summary(d, "open_n", order = st$dongs)
      cn <- group_summary(d, "close_n", order = st$dongs)
      nt <- group_summary(d, "net_n", order = st$dongs)
      data.frame(
        `University (district)` = lab(o$group), `Years` = o$n,
        `Avg. opening rate` = o$mean, `Median opening rate` = o$median,
        `Avg. closure rate` = c$mean, `Median closure rate` = c$median,
        `Total openings` = on$sum, `Total closures` = cn$sum, `Cumulative net change` = nt$sum,
        check.names = FALSE, stringsAsFactors = FALSE)
    })
    
    output$table <- DT::renderDT({
      DT::datatable(summary_table(), rownames = FALSE,
                    options = list(dom = "t", ordering = TRUE, pageLength = 10, scrollX = TRUE)) |>
        DT::formatPercentage(c("Avg. opening rate", "Median opening rate", "Avg. closure rate", "Median closure rate"), 2) |>
        DT::formatRound(c("Total openings", "Total closures", "Cumulative net change"), 0)
    })
    
    output$dl <- downloadHandler(
      filename = function() "data1_summary.csv",
      content = function(file) utils::write.csv(summary_table(), file, row.names = FALSE, fileEncoding = "UTF-8"))
    
    # ---- 세 가지 기술통계 결과 (질문·입력·수치·해석·시사점) ------------------------
    output$analyses <- renderUI({
      d <- filtered()
      an <- st$analyses %||% list()
      n_ind <- length(input$industry)
      filt_txt <- sprintf("Universities: %s · Years %s–%s%s",
                          paste(lab(sel_dongs()), collapse = ", "), input$years[1], input$years[2],
                          if (n_ind < length(st$industries))
                            sprintf(" · %d/%d business types selected", n_ind, length(st$industries)) else " · All business types")
      do.call(layout_column_wrap, c(list(width = "300px", fill = FALSE), lapply(an, function(a) {
        metrics <- a$metric %||% NA_character_
        stats   <- rep_len(a$stat %||% "mean", length(metrics))
        ok      <- !is.na(metrics) & metrics %in% st$metrics
        result <- if (any(ok) && nrow(d)) {
          lapply(which(ok), function(j) {
            m <- metrics[j]; s <- stats[j]
            gs  <- group_summary(d, m, order = st$dongs)
            val <- gs[[s]]
            tagList(
              tags$strong(sprintf("%s of %s", stat_en[[s]], st$labels[[m]])),
              tags$ul(lapply(seq_len(nrow(gs)), function(i)
                tags$li(sprintf("%s: %s", lab(gs$group[i]), fmt_stat(val[i], m, s))))))
          })
        } else list(hint("🚫 No metric is configured, or the filtered result is empty (check metric in data1_config.R)"))
        ui_card(a$title,
                tags$dl(
                  tags$dt("Question"), tags$dd(a$question),
                  tags$dt("Inputs (current filters)"), tags$dd(filt_txt),
                  tags$dt("Result"), tags$dd(result),
                  tags$dt("Why it matters"), tags$dd(a$interpretation),
                  tags$dt("Next analysis"), tags$dd(a$next_step)))
      })))
    })
    })
}
