# 공통 Step 탭 UI
step_panel <- function(n, title, formula, desc, ...) {
  nav_panel(
    title = paste0(n, ". ", title),
    value = paste0("step", n),
    div(class = "pt-2",
        h4(class = "fw-bold mb-1", paste0("Step ", n, ". ", title)),
        if (!is.null(formula)) tags$code(formula),
        p(class = "text-muted mt-2", desc),
        ...)
  )
}

# Explain PCA 화면 구성
explainPcaUI <- function(id) {
  ns <- NS(id)
  tagList(
    page_header("Tab 2 · how PCA finds the match ", "Explain PCA",
                "Shoe Search에서 선택한 신발 사진을 PCA로 분석하고, 주요 특징을 추출해 비슷한 신발을 찾는 과정을 단계별로 보여줍니다."),
    uiOutput(ns("current_photo")),

    navset_card_pill(
      id = ns("step"),

      step_panel(1, "Average shoe", "x̄ = (1/2000) Σ xᵢ",
        "2,000장의 신발 사진에서 같은 위치에 있는 픽셀들의 밝기를 평균하여 만든 이미지입니다. PCA는 각 사진이 이 평균 이미지와 얼마나 다른지를 바탕으로 신발의 특징을 분석합니다.",
        uiOutput(ns("step1"))),

      step_panel(2, "Photo − average", "x − x̄",
        "선택한 신발 사진에서 평균 이미지를 빼면 픽셀별 밝기 차이를 확인할 수 있습니다. 평균보다 밝은 부분은 주황색, 어두운 부분은 파란색으로 표시되며 차이가 없는 부분은 흰색으로 나타납니다.",
        uiOutput(ns("step2"))),

      step_panel(3, "Score map", "zᵢ₁ = (xᵢ − x̄)·v₁,  zᵢ₂ = (xᵢ − x̄)·v₂",
        "2,000장의 신발 사진을 첫 번째 주성분(PC1)과 두 번째 주성분(PC2)을 기준으로 나타낸 그래프입니다. ★ 은 선택한 사진을 나타냅니다.",
        checkboxGroupInput(ns("cat_filter"), "Show categories", CATEGORIES, CATEGORIES, inline = TRUE),
        plotly::plotlyOutput(ns("score_map"), height = "460px")),

      step_panel(4, "Eigenshoes", "v₁, v₂, …  (loadings, 4,096 → 64 × 64)",
        "각 PC가 나타내는 밝기 변화 패턴을 64×64 이미지로 표현한 Eigenshoe입니다. 아래 그래프에서는 각 PC의 설명분산비율과 누적 설명분산비율을 확인할 수 있습니다.",
        sliderInput(ns("n_eigen"), "Number of PCs to display", 4, 32, 12, step = 4, ticks = FALSE),
        uiOutput(ns("eigen_gallery")),
        plotly::plotlyOutput(ns("scree"), height = "320px")),

      step_panel(5, "Scores", "z = Vₖᵀ (x − x̄)",
        "선택한 신발의 PC score를 확인하고 가장 비슷한 신발의 PC score와 비교합니다.",
        plotly::plotlyOutput(ns("scores_plot"), height = "340px"),
        div(class = "table-responsive", tableOutput(ns("scores_table")))),

      step_panel(6, "Rebuild", "x̂ₖ = x̄ + Σⱼ₌₁ᴷ zⱼ vⱼ",
        "평균 신발에 PC score와 Eigenshoe를 이용해 선택한 신발 사진을 복원합니다. K가 커질수록 원본 사진과 비슷해지며 복원 오차(RMSE)는 줄어듭니다.",
        uiOutput(ns("rebuild_gallery")),
        plotly::plotlyOutput(ns("rebuild_curve"), height = "300px")),

      step_panel(7, "More: how good is K?", "Leave-one-out Top-5 vote accuracy",
        "각 신발 사진을 검색할 때 자기 자신을 제외한 1,999장에서 가장 비슷한 신발 Top 5를 찾습니다. Top 5의 카테고리를 다수결로 결정하고 실제 카테고리와 일치한 비율을 LOO 정확도로 나타냅니다. 이를 통해 K에 따른 검색 성능을 비교할 수 있습니다.",
        plotly::plotlyOutput(ns("loo_plot"), height = "300px"))
    )
  )
}

# Shoe Search의 선택 사진과 K를 받아 결과를 갱신
explainPcaServer <- function(id, pca, search) {
  moduleServer(id, function(input, output, session) {
    sel <- search$selected
    K   <- search$K
    res <- search$result

    # 현재 선택한 신발
    output$current_photo <- renderUI({
      s <- sel()
      card(card_body(class = "py-2",
        layout_columns(col_widths = c(2, 10),
          shoe_img(gray_uri(s$x, scale = 2), s$label, 64),
          div(strong("Explaining: "), span(s$label), span(class = "text-muted", sprintf(" · K = %d", K())),
              hint("다른 사진/K 를 보려면 Shoe Search 탭에서 바꾸세요. 이 탭은 자동으로 갱신됩니다.")))))
    })

    # Step 1: 평균 신발
    output$step1 <- renderUI({
      layout_columns(col_widths = breakpoints(sm = 12, md = c(4, 8)),
          shoe_figure(pca$mean, "x̄ (average shoe)", size = 180),
          div(
              p(sprintf("카탈로그 %s장 × %s 픽셀 행렬의 열 평균입니다.", format(nrow(pca$X), big.mark = ","),
                        format(ncol(pca$X), big.mark = ","))),
              p(sprintf("평균 밝기: %.3f (0 = 검정, 1 = 흰색)", mean(pca$mean))),
              p(class = "text-muted", "흐릿한 이유: 부츠·샌들·운동화가 겹쳐 평균되었기 때문입니다.")))
    })

    # Step 2: 선택 사진과 평균의 픽셀별 차이
    output$step2 <- renderUI({
      s <- sel(); d <- s$x - pca$mean
      lim <- max(abs(d))
      tagList(
        layout_column_wrap(width = "150px", fill = FALSE,
          shoe_figure(s$x, "x (your photo)", size = 140),
          shoe_figure(pca$mean, "x̄ (average)", size = 140),
          shoe_figure(d, "x − x̄", size = 140, diverging = TRUE, lim = lim)),
        div(class = "mt-2 small",
            span(sprintf("darker (−%.2f) ", lim)),
            tags$img(src = diverging_legend_uri(), width = 240, height = 14, alt = "legend", class = "border"),
            span(sprintf(" brighter (+%.2f)", lim)))
      )
    })

    # Step 3: PC1·PC2 점수 분포와 선택 사진의 위치
    output$score_map <- plotly::renderPlotly({
      cats <- input$cat_filter
      df <- data.frame(id = pca$meta$id, category = pca$meta$category,
                       PC1 = pca$scores[, 1], PC2 = pca$scores[, 2])
      df <- df[df$category %in% cats, ]
      z <- project_photo(pca, sel()$x, 2)
      p <- plotly::plot_ly()

      if (nrow(df)) {
        p <- plotly::add_markers(p, data = df, x = ~PC1, y = ~PC2, color = ~category,
                                 colors = CAT_COLORS, marker = list(size = 6, opacity = 0.55),
                                 text = ~sprintf("ID %04d<br>%s", id, category), hoverinfo = "text")
      }
      p |>

        plotly::add_markers(x = z[1], y = z[2], name = "Your photo",
                            marker = list(symbol = "star", size = 20, color = COL$danger,
                                          line = list(color = "#FFFFFF", width = 1.5)),
                            text = sel()$label, hoverinfo = "text", inherit = FALSE) |>

        plotly::layout(
          xaxis = list(title = sprintf("PC1 score (%.1f%% of variance)", 100 * pca$var_ratio[1]),
                       zeroline = TRUE, zerolinecolor = COL$border),
          yaxis = list(title = sprintf("PC2 score (%.1f%% of variance)", 100 * pca$var_ratio[2]),
                       zeroline = TRUE, zerolinecolor = COL$border),
          legend = list(orientation = "h", y = -0.18),
          font = list(family = "Inter, Pretendard, sans-serif", color = COL$text),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") |>
        plotly::config(displaylogo = FALSE,
                       toImageButtonOptions = list(filename = "pca_score_map", format = "png"))
    })

    # Step 4: 고유벡터를 이미지로 표현한 Eigenshoe
    output$eigen_gallery <- renderUI({
      n <- input$n_eigen %||% 12
      tagList(
        do.call(layout_column_wrap, c(list(width = "110px", fill = FALSE),
          lapply(seq_len(n), function(j) {
            card(class = if (j <= K()) "border-primary border-2" else NULL,
                 card_body(class = "text-center p-2",
                   shoe_img(diverging_uri(pca$loadings[, j], scale = 2), paste("PC", j), 96),
                   div(class = "small fw-semibold", paste0("PC", j), if (j <= K()) span(class = "badge text-bg-primary", "in K")),
                   div(class = "small text-muted", sprintf("%.2f%% of variance", 100 * pca$var_ratio[j]))
                 ))
          }))),
        hint(sprintf("파란 테두리는 현재 검색에 사용하는 PC를 나타냅니다. (K = %d)", K())))
    })

    # 각 PC의 설명분산비율과 누적 설명분산비율
    output$scree <- plotly::renderPlotly({
      n <- min(50, length(pca$var_ratio))
      df <- data.frame(PC = seq_len(n), var = 100 * pca$var_ratio[1:n], cum = 100 * pca$cum_ratio[1:n])
      plotly::plot_ly(df, x = ~PC) |>

        plotly::add_bars(y = ~var, name = "Explained variance (%)",
                         marker = list(color = ifelse(df$PC <= K(), COL$primary, "#B8CCC4")),
                         hovertemplate = "PC%{x}: %{y:.2f}%<extra></extra>") |>

        plotly::add_lines(y = ~cum, name = "Cumulative (%)", yaxis = "y2",
                          line = list(color = COL$text, width = 2),
                          hovertemplate = "PC1–%{x}: %{y:.1f}%<extra></extra>") |>
        plotly::layout(xaxis = list(title = "Principal component"),
                       yaxis = list(title = "Explained variance (%)"),
                       yaxis2 = list(title = "Cumulative (%)", overlaying = "y", side = "right",
                                     range = c(0, 100)),
                       legend = list(orientation = "h", y = -0.25), margin = list(r = 60),
                       paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") |>
        plotly::config(displaylogo = FALSE)
    })

    # Step 5: 선택 사진과 가장 유사한 신발의 첫 K개 PC score
    scores_df <- reactive({
      k <- K()
      best <- res()$top$id[1]
      data.frame(PC = seq_len(k), photo = res()$z[seq_len(k)],
                 best = pca$scores[match(best, pca$meta$id), seq_len(k)])
    })

    output$scores_plot <- plotly::renderPlotly({
      df <- scores_df()
      plotly::plot_ly(df, x = ~PC) |>
        plotly::add_bars(y = ~photo, name = "Your photo", marker = list(color = COL$primary),
                         hovertemplate = "PC%{x}: %{y:.3f}<extra>Your photo</extra>") |>
        plotly::add_bars(y = ~best, name = sprintf("Best match #%04d", res()$top$id[1]),
                         marker = list(color = "#B8C4CE"),
                         hovertemplate = "PC%{x}: %{y:.3f}<extra>Best match</extra>") |>
        plotly::layout(barmode = "group", xaxis = list(title = "Principal component", dtick = if (nrow(df) <= 30) 1 else NULL),
                       yaxis = list(title = "Score z (pixel-intensity units)"),
                       legend = list(orientation = "h", y = -0.25),
                       paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") |>
        plotly::config(displaylogo = FALSE)
    })

    output$scores_table <- renderTable({
      df <- scores_df()
      show <- head(df, 20)
      out <- rbind(sprintf("%.3f", show$photo), sprintf("%.3f", show$best))
      colnames(out) <- paste0("PC", show$PC)
      cbind(` ` = c("Your photo", "Best match"), out)
    }, striped = TRUE, width = "100%", caption = "처음 최대 20개 점수 (전체는 위 그래프 hover)")

    # Step 6: 여러 K에서 이미지 복원 결과 비교
    rebuild_Ks <- reactive(sort(unique(c(1, 5, 10, 20, 50, 100, K()))))

    output$rebuild_gallery <- renderUI({
      s <- sel(); z <- project_photo(pca, s$x, max(rebuild_Ks()))
      do.call(layout_column_wrap, c(list(width = "120px", fill = FALSE),
        lapply(rebuild_Ks(), function(k) {
          r <- reconstruct(pca, z, k)

          shoe_figure(r, paste("K =", k),
                      sprintf("RMSE %.3f · %.0f%% var", sqrt(mean((r - s$x)^2)), 100 * pca$cum_ratio[k]),
                      size = 104, highlight = if (k == K()) "best" else NULL)
        }),
        list(shoe_figure(s$x, "Original", "4,096 pixels", size = 104, highlight = "query"))))
    })

    # K에 따른 복원 오차(RMSE)
    output$rebuild_curve <- plotly::renderPlotly({
      s <- sel(); ks <- 1:K_MAX
      z <- project_photo(pca, s$x, K_MAX)
      rmse <- vapply(ks, function(k) sqrt(mean((reconstruct(pca, z, k) - s$x)^2)), numeric(1))
      plotly::plot_ly(x = ks, y = rmse, type = "scatter", mode = "lines",
                      line = list(color = COL$primary, width = 2.5),
                      hovertemplate = "K = %{x}<br>RMSE = %{y:.4f}<extra></extra>") |>
        plotly::add_markers(x = K(), y = rmse[K()], marker = list(size = 11, color = COL$danger),
                            name = "current K", hovertemplate = "current K<extra></extra>", inherit = FALSE) |>
        plotly::layout(showlegend = FALSE, xaxis = list(title = "K (number of PCs)"),
                       yaxis = list(title = "Reconstruction RMSE (0–1 scale)"),
                       paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") |>
        plotly::config(displaylogo = FALSE)
    })

    # Step 7: 사전 계산된 LOO 정확도로 K별 검색 성능 비교
    output$loo_plot <- plotly::renderPlotly({
      la <- pca$loo_accuracy
      validate(need(!is.null(la), "캐시에 LOO 정확도가 없습니다. scripts/precompute_pca.R 를 다시 실행하세요."))
      plotly::plot_ly(la, x = ~K, y = ~ 100 * accuracy, type = "scatter", mode = "lines+markers",
                      line = list(color = COL$primary, width = 2.5), marker = list(size = 8),
                      hovertemplate = "K = %{x}<br>accuracy = %{y:.1f}%<extra></extra>") |>
        plotly::layout(xaxis = list(title = "K (number of PCs)", type = "log"),
                       yaxis = list(title = "LOO accuracy (%)", range = c(0, 100)),
                       paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") |>
        plotly::config(displaylogo = FALSE)
    })
  })
}
