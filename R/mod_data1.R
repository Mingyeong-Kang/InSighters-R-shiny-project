# =============================================================================
# mod_data1.R — Tab 4. Data 1 (여대 상권 비교: 이화여대·숙명여대·성신여대 음식점 개·폐업)
# 원본 인허가 CSV 를 읽어 연도별 개업·폐업 통계를 앱 안에서 직접 계산한다.
# 데이터 파일이 없으면 "Dataset Not Connected" 안내를 보여 준다.
# =============================================================================

data1UI <- function(id) {
  ns <- NS(id)
  tagList(
    page_header("Tab 4 · Project 1 dataset", "Data 1 — 여대 상권 비교",
                "이화여대(대현동) · 숙명여대(청파동) · 성신여대(동선동) 주변 음식점의 연도별 개업·폐업을 기술통계(filter, count, mean, median, rate, group summary)로 비교합니다."),
    uiOutput(ns("body"))
  )
}

data1Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    st <- load_data1_real()           # 앱 시작 시 1회 로드
    
    lab <- function(k) unname(st$dong_labels[k])    # 동네 키 → "이화여대 (대현동)"
    
    stat_ko <- c(mean = "평균", median = "중앙값", sum = "합계", count = "개수")
    
    # 통계값 표시 (count 는 개수, 비율 지표는 %, 나머지는 숫자)
    fmt_stat <- function(x, metric, stat) {
      if (stat == "count") return(format(x, big.mark = ",", trim = TRUE))
      digits <- if (metric %in% DATA1_RATE_METRICS || stat %in% c("mean", "median")) 2 else 0
      fmt_metric(x, metric, digits)
    }
    
    # ---- 화면 ------------------------------------------------------------------
    output$body <- renderUI({
      if (!isTRUE(st$ok)) {
        return(card(class = "border-warning mt-3",
                    card_header(span(class = "badge text-bg-warning", "● Dataset Not Connected")),
                    card_body(
                      h4("Project 1 데이터셋이 아직 연결되지 않았습니다"),
                      p(st$reason),
                      tags$ol(
                        tags$li(HTML("원본 CSV(청파동_동선동_대현동_음식점_합본.csv)를 <code>data1/data1_raw.csv</code> 로 저장합니다.")),
                        tags$li(HTML("<code>data1/data1_config.R</code> 에서 행정동 / 인허가일자 / 폐업일자 / 업태구분명 컬럼 이름을 확인합니다.")),
                        tags$li("앱을 다시 실행하면 필터·그래프·요약 통계가 실제 데이터로 계산됩니다.")),
                      hint("실제 데이터 없이 숫자를 표시하지 않습니다."))))
      }
      
      last_year <- max(st$years)
      partial <- st$cutoff < as.Date(sprintf("%d-12-31", last_year))
      tagList(
        if (partial)
          notice(sprintf("%d년은 데이터의 마지막 날짜(%s)까지만 포함된 부분 연도입니다. 연간 비교가 필요하면 연도 범위를 %d년까지로 줄이세요.",
                         last_year, format(st$cutoff, "%Y-%m-%d"), last_year - 1)),
        if (isTRUE(st$unmatched > 0))
          notice(sprintf("행정동이 분석 대상 동네가 아닌 %d행은 제외되었습니다 (data1_config.R 의 DATA1_DONGS 확인).", st$unmatched)),
        if (isTRUE(st$bad_dates > 0))
          notice(sprintf("인허가일자를 읽을 수 없는 %d행은 제외되었습니다.", st$bad_dates)),
        layout_columns(col_widths = breakpoints(sm = 12, lg = c(3, 9)),
                       div(
                         ui_card("Filters", icon = "⚙️",
                                 checkboxGroupInput(ns("dong"), "대학 (동네)", choiceNames = unname(lab(st$dongs)),
                                                    choiceValues = st$dongs, selected = st$dongs),
                                 sliderInput(ns("years"), "연도 범위", min(st$years), max(st$years),
                                             range(st$years), step = 1, sep = ""),
                                 if (st$has$industry)
                                   selectInput(ns("industry"), "업태 (복수 선택)", st$industries, st$industries, multiple = TRUE)
                                 else hint("🚫 업태 — 미지원 (컬럼 미매핑)"),
                                 actionButton(ns("reset"), "↺ Reset filters", class = "btn-outline-secondary w-100")),
                         hint("개업수 = 인허가일자가 그 해인 업소 수, 폐업수 = 폐업일자가 그 해인 업소 수, 순증감 = 개업수 − 폐업수. 개업률·폐업률 = 각 수 ÷ (연초 영업 중 업소 수 + 그해 개업수).")),
                       div(
                         uiOutput(ns("kpis")),
                         layout_columns(col_widths = breakpoints(sm = 12, lg = c(6, 6)),
                                        ui_card("1. 개업률 비교", icon = "🟢", plotly::plotlyOutput(ns("p_open"), height = "320px")),
                                        ui_card("2. 폐업률 비교", icon = "🔴", plotly::plotlyOutput(ns("p_close"), height = "320px"))),
                         layout_columns(col_widths = breakpoints(sm = 12, lg = c(6, 6)),
                                        ui_card("3. 대학별 평균 개업률 vs 폐업률", icon = "⚖️", plotly::plotlyOutput(ns("p_avg"), height = "320px")),
                                        ui_card("5. 누적 순증감 비교", icon = "📊", plotly::plotlyOutput(ns("p_cum"), height = "320px"))),
                         ui_card("4. 순증감 (개업 수 − 폐업 수)", icon = "➕", plotly::plotlyOutput(ns("p_net"), height = "320px")),
                         ui_card("Group summary table", icon = "📋",
                                 header_right = downloadButton(ns("dl"), "CSV", class = "btn-outline-primary btn-sm"),
                                 DT::DTOutput(ns("table"))),
                         h4(class = "fw-bold mt-3", "Three descriptive findings"),
                         uiOutput(ns("analyses"))))
      )
    })
    
    observeEvent(input$reset, {
      updateCheckboxGroupInput(session, "dong", selected = st$dongs)
      updateSliderInput(session, "years", value = range(st$years))
      if (st$has$industry) updateSelectInput(session, "industry", selected = st$industries)
    })
    
    # ---- 데이터 흐름: 업태 필터 → 동네×연도 집계 → 동네·연도 필터 ---------------
    yearly_all <- reactive({
      ev <- st$events
      if (st$has$industry) ev <- ev[ev$industry %in% (input$industry %||% character()), , drop = FALSE]
      validate(need(nrow(ev) > 0, "업태를 하나 이상 선택하세요."))
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
      if (!nrow(d)) return(empty_state("필터 조건에 맞는 데이터가 없습니다", p("대학을 선택하거나 필터를 넓히세요."), icon = "🔍"))
      net <- group_summary(d, "net_n", order = st$dongs)
      orr <- group_summary(d, "open_rate", order = st$dongs)
      crr <- group_summary(d, "close_rate", order = st$dongs)
      do.call(layout_column_wrap, c(list(width = "220px", fill = FALSE),
                                    lapply(seq_len(nrow(net)), function(i) {
                                      k <- net$group[i]
                                      value_box(
                                        title = paste0(lab(k), " · 누적 순증감"),
                                        value = sprintf("%+d", as.integer(net$sum[i])),
                                        p(sprintf("평균 개업률 %s · 평균 폐업률 %s", fmt_metric(orr$mean[i], "open_rate"), fmt_metric(crr$mean[i], "close_rate"))),
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
      d <- filtered(); validate(need(nrow(d) > 0, "데이터 없음"))
      p <- plotly::plot_ly()
      for (k in intersect(st$dongs, unique(as.character(d$.dong)))) {
        s <- d[d$.dong == k, ]; s <- s[order(s$.year), ]
        p <- plotly::add_trace(p, x = s$.year, y = s[[metric]], type = "scatter", mode = "lines+markers",
                               name = lab(k), line = list(color = st$colors[[k]], width = 2.5),
                               marker = list(color = st$colors[[k]], size = 7),
                               hovertemplate = paste0("%{x}년 · %{y:.2%}<extra>", lab(k), "</extra>"))
      }
      p |> plotly::layout(xaxis = list(title = "연도", dtick = 1)) |> base_layout(ytitle, ".0%")
    }
    
    output$p_open  <- plotly::renderPlotly(rate_trend("open_rate", "개업률"))
    output$p_close <- plotly::renderPlotly(rate_trend("close_rate", "폐업률"))
    
    output$p_avg <- plotly::renderPlotly({
      d <- filtered(); validate(need(nrow(d) > 0, "데이터 없음"))
      o <- group_summary(d, "open_rate", order = st$dongs)
      c <- group_summary(d, "close_rate", order = st$dongs)
      plotly::plot_ly() |>
        plotly::add_bars(x = lab(o$group), y = o$mean, name = "개업률",
                         marker = list(color = st$rate_colors[["open"]]),
                         text = sprintf("%.2f%%", 100 * o$mean), textposition = "outside",
                         hovertemplate = "%{x}<br>평균 개업률 %{y:.2%}<extra></extra>") |>
        plotly::add_bars(x = lab(c$group), y = c$mean, name = "폐업률",
                         marker = list(color = st$rate_colors[["close"]]),
                         text = sprintf("%.2f%%", 100 * c$mean), textposition = "outside",
                         hovertemplate = "%{x}<br>평균 폐업률 %{y:.2%}<extra></extra>") |>
        plotly::layout(barmode = "group",
                       xaxis = list(title = "", categoryorder = "array", categoryarray = lab(o$group))) |>
        base_layout("평균", ".0%")
    })
    
    output$p_net <- plotly::renderPlotly({
      d <- filtered(); validate(need(nrow(d) > 0, "데이터 없음"))
      p <- plotly::plot_ly()
      for (k in intersect(st$dongs, unique(as.character(d$.dong)))) {
        s <- d[d$.dong == k, ]; s <- s[order(s$.year), ]
        p <- plotly::add_bars(p, x = s$.year, y = s$net_n, name = lab(k), marker = list(color = st$colors[[k]]),
                              hovertemplate = paste0("%{x}년 · %{y:+d}개<extra>", lab(k), "</extra>"))
      }
      p |> plotly::layout(barmode = "group", xaxis = list(title = "연도", dtick = 1),
                          shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                                             line = list(color = "#6B7280", width = 1)))) |>
        base_layout("순증감 (개)")
    })
    
    output$p_cum <- plotly::renderPlotly({
      d <- filtered(); validate(need(nrow(d) > 0, "데이터 없음"))
      g <- group_summary(d, "net_n", order = st$dongs)
      plotly::plot_ly(x = lab(g$group), y = g$sum, type = "bar",
                      marker = list(color = unname(st$colors[g$group])),
                      text = sprintf("%+d", as.integer(g$sum)), textposition = "outside",
                      hovertemplate = "%{x}<br>누적 순증감 %{y:+d}개<extra></extra>") |>
        plotly::layout(xaxis = list(title = "", categoryorder = "array", categoryarray = lab(g$group)),
                       shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                                          line = list(color = "#6B7280", width = 1)))) |>
        base_layout("누적 순증감 (개)")
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
        `대학 (동네)` = lab(o$group), `연도 수` = o$n,
        `평균 개업률` = o$mean, `중앙값 개업률` = o$median,
        `평균 폐업률` = c$mean, `중앙값 폐업률` = c$median,
        `개업 합계` = on$sum, `폐업 합계` = cn$sum, `누적 순증감` = nt$sum,
        check.names = FALSE, stringsAsFactors = FALSE)
    })
    
    output$table <- DT::renderDT({
      DT::datatable(summary_table(), rownames = FALSE,
                    options = list(dom = "t", ordering = TRUE, pageLength = 10, scrollX = TRUE)) |>
        DT::formatPercentage(c("평균 개업률", "중앙값 개업률", "평균 폐업률", "중앙값 폐업률"), 2) |>
        DT::formatRound(c("개업 합계", "폐업 합계", "누적 순증감"), 0)
    })
    
    output$dl <- downloadHandler(
      filename = function() "data1_summary.csv",
      content = function(file) utils::write.csv(summary_table(), file, row.names = FALSE, fileEncoding = "UTF-8"))
    
    # ---- 세 가지 기술통계 결과 (질문·입력·수치·해석·시사점) ------------------------
    output$analyses <- renderUI({
      d <- filtered()
      an <- st$analyses %||% list()
      n_ind <- if (st$has$industry) length(input$industry) else NA
      filt_txt <- sprintf("대학: %s · 연도 %s–%s%s",
                          paste(lab(sel_dongs()), collapse = ", "), input$years[1], input$years[2],
                          if (st$has$industry && n_ind < length(st$industries))
                            sprintf(" · 업태 %d/%d 선택", n_ind, length(st$industries)) else " · 업태 전체")
      do.call(layout_column_wrap, c(list(width = "300px", fill = FALSE), lapply(an, function(a) {
        has_metric <- !is.null(a$metric) && !is.na(a$metric) && a$metric %in% st$metrics
        nums <- if (has_metric && nrow(d)) {
          gs <- group_summary(d, a$metric, order = st$dongs)
          val <- if (a$stat == "count") gs$n else gs[[a$stat]]
          tags$ul(lapply(seq_len(nrow(gs)), function(i)
            tags$li(sprintf("%s: %s", lab(gs$group[i]), fmt_stat(val[i], a$metric, a$stat)))))
        } else hint("🚫 지표가 설정되지 않았거나 필터 결과가 비어 있음 (data1_config.R 의 metric)")
        ui_card(a$title,
                tags$dl(
                  tags$dt("Question"), tags$dd(a$question),
                  tags$dt("Inputs (current filters)"), tags$dd(filt_txt),
                  tags$dt(sprintf("Result — %s of %s", stat_ko[[a$stat]], if (has_metric) st$labels[[a$metric]] else "?")),
                  tags$dd(nums),
                  tags$dt("Why it matters"), tags$dd(a$interpretation),
                  tags$dt("Next analysis"), tags$dd(a$next_step)))
      })))
    })
  })
}