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
                "Explore how PCA represents the selected shoe and uses its PC scores to find similar shoes."),
    uiOutput(ns("current_photo")),

    navset_card_pill(
      id = ns("step"),

      step_panel(1, "Average shoe", "x̄ = (1/2000) Σ xᵢ",
        "The average image is made by averaging each pixel across 2,000 shoe photos. PCA analyzes how each photo differs from this average.",
        uiOutput(ns("step1"))),

      step_panel(2, "Photo − average", "x − x̄",
        "Subtract the average shoe from the selected photo to see pixel-level differences. Orange means brighter, blue means darker, and white means little or no difference.",
        uiOutput(ns("step2"))),

      step_panel(3, "Score map", "zᵢ₁ = (xᵢ − x̄)·v₁,  zᵢ₂ = (xᵢ − x̄)·v₂",
                 "This plot shows 2,000 shoes by their PC1 and PC2 scores. The star marks the selected photo.",
        checkboxGroupInput(ns("cat_filter"), "Show categories", CATEGORIES, CATEGORIES, inline = TRUE),
        plotly::plotlyOutput(ns("score_map"), height = "460px")),

      step_panel(4, "Eigenshoes", "v₁, v₂, …  (loadings, 4,096 → 64 × 64)",
        "Each eigenshoe visualizes the pixel-brightness pattern represented by a PC as a 64×64 image. The chart below shows explained and cumulative variance.",
        sliderInput(ns("n_eigen"), "Number of PCs to display", 4, 32, 12, step = 4, ticks = FALSE),
        uiOutput(ns("eigen_gallery")),
        plotly::plotlyOutput(ns("scree"), height = "320px")),

      step_panel(5, "Scores", "z = Vₖᵀ (x − x̄)",
        "Compare the selected shoe’s PC scores with those of its closest match.",
        plotly::plotlyOutput(ns("scores_plot"), height = "340px"),
        div(class = "table-responsive", tableOutput(ns("scores_table")))),

      step_panel(6, "Rebuild", "x̂ₖ = x̄ + Σⱼ₌₁ᴷ zⱼ vⱼ",
        "Reconstruct the selected shoe using the average image, PC scores, and eigenshoes. As the number of PCs (K) increases, the reconstruction gets closer to the original and the reconstruction error (RMSE) decreases.",
        uiOutput(ns("rebuild_gallery")),
        plotly::plotlyOutput(ns("rebuild_curve"), height = "300px")),

      step_panel(7, "More: how good is K?", "Leave-one-out Top-5 vote accuracy",
        "For each shoe, find the top five matches among the other 1,999 shoes. Predict its category by majority vote and compare it with the true category. LOO accuracy shows how search performance changes with K.",
        plotly::plotlyOutput(ns("loo_plot"), height = "300px"))
    )
  )
}

# Shoe Search에서 선택 사진과 K를 받아 결과를 갱신
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
          div(strong("Explaining: "), span(s$label),
          span(class = "text-muted", sprintf(" · K = %d", K()))))))
    })

    # Step 1: 평균 신발
    output$step1 <- renderUI({
      div(
        class = "d-flex justify-content-center",
        shoe_figure(pca$mean, "x̄ (average shoe)", size = 180)
      )
    })

    # Step 2: 선택 사진과 평균의 픽셀별 차이
    output$step2 <- renderUI({
      s <- sel(); d <- s$x - pca$mean
      lim <- max(abs(d))
      tagList(
        layout_column_wrap(width = "150px", fill = FALSE,
                           div(class = "d-flex justify-content-center",
                               shoe_figure(s$x, "x (your photo)", size = 140)),
                           div(class = "d-flex justify-content-center",
                               shoe_figure(pca$mean, "x̄ (average)", size = 140)),
                           div(class = "d-flex justify-content-center",
                               shoe_figure(d, "x − x̄", size = 140, diverging = TRUE, lim = lim))
        ),
        
        div(class = "mt-2 small",
            span(sprintf("darker (−%.2f) ", lim)),
            tags$img(src = diverging_legend_uri(), width = 240, height = 14,
                     alt = "legend", class = "border"),
            span(sprintf(" brighter (+%.2f)", lim))
        )
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
        hint(sprintf("PCs with bold green borders are used in the current search. (K = %d)", K())))
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
        plotly::layout(barmode = "group", xaxis = list(title = "PC", dtick = if (nrow(df) <= 30) 1 else NULL),
                       yaxis = list(title = "PC scores"),
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
    }, striped = TRUE, width = "100%", caption = "Scores up to PC20")

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
      validate(need(!is.null(la), "LOO accuracy data is missing."))
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
