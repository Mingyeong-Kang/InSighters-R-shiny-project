# =============================================================================
# mod_explain_pca.R — Tab 2. Explain PCA (Shoe Search 에서 고른 사진을 그대로 사용)
# =============================================================================

step_section <- function(n, title, formula, desc, ...) {
  div(class = "step-section", id = paste0("pca-step-", n),
      div(class = "step-head",
          span(class = "step-num", n),
          div(h3(title), if (!is.null(formula)) span(class = "formula", formula))),
      p(class = "step-desc", desc),
      ...)
}

explainPcaUI <- function(id) {
  ns <- NS(id)
  steps <- c("Average shoe", "Photo − average", "Score map", "Eigenshoes", "Scores", "Rebuild", "More")
  tagList(
    page_header("Tab 2 · How the match is found", "Explain PCA",
                "Shoe Search 에서 고른 사진이 어떻게 숫자 K 개로 요약되고, 그 숫자로 가장 비슷한 신발을 찾는지 6단계로 보여줍니다."),
    div(class = "step-nav", role = "navigation", `aria-label` = "PCA steps",
        lapply(seq_along(steps), function(i)
          tags$a(href = paste0("#pca-step-", i), class = "step-link",
                 span(class = "n", i), span(steps[i])))),
    uiOutput(ns("current_photo")),

    step_section(1, "Average shoe", "x̄ = (1/2000) Σ xᵢ",
      "2,000장의 같은 위치 픽셀 밝기를 평균한 '전형적인 신발'입니다. PCA 는 모든 사진을 이 평균에서 얼마나 벗어나는지로 설명합니다.",
      uiOutput(ns("step1"))),

    step_section(2, "Photo − average", "x − x̄",
      "선택한 사진에서 평균 신발을 뺀 결과입니다. 주황색은 평균보다 밝은 픽셀, 파란색은 평균보다 어두운 픽셀, 흰색은 차이가 없는 픽셀입니다.",
      uiOutput(ns("step2"))),

    step_section(3, "Score map (PC1 vs PC2)", "zᵢ₁ = (xᵢ − x̄)·v₁,  zᵢ₂ = (xᵢ − x̄)·v₂",
      "카탈로그 2,000장을 처음 두 주성분 좌표에 찍었습니다. 확대/축소, hover 로 id 를 확인할 수 있고 ★ 이 선택한 사진, ◆ 가 Top 5 입니다.",
      div(class = "inline-controls",
          checkboxGroupInput(ns("cat_filter"), "Show categories", CATEGORIES, CATEGORIES, inline = TRUE)),
      plotly::plotlyOutput(ns("score_map"), height = "460px")),

    step_section(4, "Eigenshoes", "v₁, v₂, …  (loadings, 4,096 → 64 × 64)",
      "각 주성분의 loading 벡터를 64×64 그림으로 그린 것입니다. 주황/파랑은 그 PC 점수가 커질 때 밝아지는/어두워지는 영역입니다. 아래 그래프는 PC별 설명분산비율과 누적비율입니다.",
      div(class = "inline-controls",
          sliderInput(ns("n_eigen"), "How many eigenshoes", 4, 32, 12, step = 4, ticks = FALSE)),
      uiOutput(ns("eigen_gallery")),
      plotly::plotlyOutput(ns("scree"), height = "320px")),

    step_section(5, "PCA scores", "z = Vₖᵀ (x − x̄)",
      "선택한 사진의 처음 K 개 점수입니다. 검색은 이 K 개 숫자 벡터와 카탈로그 각 신발의 점수 벡터 사이의 유클리드 거리로 이루어집니다. 막대 색은 Best match 와의 비교를 위해 나란히 표시합니다.",
      plotly::plotlyOutput(ns("scores_plot"), height = "340px"),
      div(class = "score-table", tableOutput(ns("scores_table")))),

    step_section(6, "Rebuild", "x̂ₖ = x̄ + Σⱼ₌₁ᴷ zⱼ vⱼ",
      "평균 신발에 eigenshoe 를 점수만큼 더해 사진을 다시 만듭니다. K 가 작으면 대략적인 형태(부츠인지, 굽이 있는지)만 남고, K 가 커질수록 끈·밑창 같은 세부가 살아나며 원본과의 오차(RMSE)가 줄어듭니다.",
      uiOutput(ns("rebuild_gallery")),
      plotly::plotlyOutput(ns("rebuild_curve"), height = "300px")),

    step_section(7, "More: how good is K?", "Leave-one-out Top-5 vote accuracy",
      "카탈로그 각 사진을 자기 자신을 뺀 1,999장에서 검색했을 때 Top 5 투표가 실제 카테고리와 맞은 비율입니다(실제 계산값). K 를 무작정 늘리는 것보다 적당한 K 에서 정확도가 포화되는지 확인할 수 있습니다.",
      plotly::plotlyOutput(ns("loo_plot"), height = "300px"))
  )
}

explainPcaServer <- function(id, pca, search) {
  moduleServer(id, function(input, output, session) {
    sel <- search$selected
    K   <- search$K
    res <- search$result

    output$current_photo <- renderUI({
      s <- sel()
      div(class = "current-photo-bar",
          shoe_img(gray_uri(s$x), s$label, 56),
          div(strong("Explaining: "), span(s$label), span(class = "muted", sprintf(" · K = %d", K())),
              p(class = "hint", "다른 사진/K 를 보려면 Shoe Search 탭에서 바꾸세요. 이 탭은 자동으로 갱신됩니다.")))
    })

    output$step1 <- renderUI({
      div(class = "img-row",
          div(class = "img-fig", shoe_img(gray_uri(pca$mean), "average shoe", 180), span("x̄ (average shoe)")),
          div(class = "explain-box",
              p(sprintf("카탈로그 %s장 × %s 픽셀 행렬의 열 평균입니다.", format(nrow(pca$X), big.mark = ","),
                        format(ncol(pca$X), big.mark = ","))),
              p(sprintf("평균 밝기: %.3f (0 = 검정, 1 = 흰색)", mean(pca$mean))),
              p(class = "muted", "흐릿한 이유: 부츠·샌들·운동화가 겹쳐 평균되었기 때문입니다.")))
    })

    output$step2 <- renderUI({
      s <- sel(); d <- s$x - pca$mean
      lim <- max(abs(d))
      div(class = "img-row",
          div(class = "img-fig", shoe_img(gray_uri(s$x), "photo", 140), span("x (your photo)")),
          span(class = "op", "−"),
          div(class = "img-fig", shoe_img(gray_uri(pca$mean), "mean", 140), span("x̄")),
          span(class = "op", "="),
          div(class = "img-fig", shoe_img(diverging_uri(d, lim), "difference", 140), span("x − x̄")),
          div(class = "legend-diverging",
              span(class = "sw neg"), span(sprintf("darker (−%.2f)", lim)),
              span(class = "sw zero"), span("0"),
              span(class = "sw pos"), span(sprintf("brighter (+%.2f)", lim)),
              p(class = "hint", sprintf("평균보다 밝은 픽셀 %.0f%% · 어두운 픽셀 %.0f%%",
                                        100 * mean(d > 0.05), 100 * mean(d < -0.05)))))
    })

    output$score_map <- plotly::renderPlotly({
      cats <- input$cat_filter
      df <- data.frame(id = pca$meta$id, category = pca$meta$category,
                       PC1 = pca$scores[, 1], PC2 = pca$scores[, 2])
      df <- df[df$category %in% cats, ]
      z <- project_photo(pca, sel()$x, 2)
      top <- res()$top
      tp <- data.frame(PC1 = pca$scores[match(top$id, pca$meta$id), 1],
                       PC2 = pca$scores[match(top$id, pca$meta$id), 2], id = top$id)
      p <- plotly::plot_ly()
      if (nrow(df)) {
        p <- plotly::add_markers(p, data = df, x = ~PC1, y = ~PC2, color = ~category,
                                 colors = CAT_COLORS, marker = list(size = 6, opacity = 0.55),
                                 text = ~sprintf("ID %04d<br>%s", id, category), hoverinfo = "text")
      }
      p |>
        plotly::add_markers(data = tp, x = ~PC1, y = ~PC2, name = "Top 5",
                            marker = list(symbol = "diamond", size = 11, color = "#FFFFFF",
                                          line = list(color = COL$text, width = 2)),
                            text = ~sprintf("Top 5 · ID %04d", id), hoverinfo = "text", inherit = FALSE) |>
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

    output$eigen_gallery <- renderUI({
      n <- input$n_eigen %||% 12
      div(class = "eigen-grid",
          lapply(seq_len(n), function(j) {
            div(class = paste("eigen-card", if (j <= K()) "in-k"),
                shoe_img(diverging_uri(pca$loadings[, j]), paste("PC", j), 96),
                div(class = "cap", strong(paste0("PC", j)),
                    span(sprintf("%.2f%%", 100 * pca$var_ratio[j]))),
                div(class = "var-bar", div(style = sprintf("width:%.0f%%", 100 * pca$var_ratio[j] / pca$var_ratio[1]))))
          }),
          p(class = "hint full", sprintf("초록 테두리 = 현재 검색에 쓰이는 PC (K = %d)", K())))
    })

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

    rebuild_Ks <- reactive(sort(unique(c(1, 2, 5, 10, 20, 50, 100, K()))))

    output$rebuild_gallery <- renderUI({
      s <- sel(); z <- project_photo(pca, s$x, max(rebuild_Ks()))
      div(class = "rebuild-strip",
          lapply(rebuild_Ks(), function(k) {
            r <- reconstruct(pca, z, k)
            div(class = paste("rebuild-card", if (k == K()) "is-current"),
                shoe_img(gray_uri(r), paste("K =", k), 104),
                div(class = "cap", strong(paste("K =", k)),
                    span(sprintf("RMSE %.3f", sqrt(mean((r - s$x)^2)))),
                    span(class = "muted", sprintf("%.0f%% var", 100 * pca$cum_ratio[k]))))
          }),
          div(class = "rebuild-card original",
              shoe_img(gray_uri(s$x), "original", 104),
              div(class = "cap", strong("Original"), span("4,096 pixels"))))
    })

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
