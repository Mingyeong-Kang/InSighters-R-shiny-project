# =============================================================================
# ui_helpers.R — 여러 탭에서 재사용하는 UI 조각
# =============================================================================

ui_card <- function(title = NULL, ..., subtitle = NULL, class = "", icon = NULL,
                    header_right = NULL) {
  div(class = paste("p-card", class),
      if (!is.null(title)) div(class = "p-card-head",
        div(h3(class = "p-card-title", if (!is.null(icon)) span(class = "p-ico", icon), title),
            if (!is.null(subtitle)) p(class = "p-card-sub", subtitle)),
        header_right),
      div(class = "p-card-body", ...))
}

page_header <- function(kicker, title, desc) {
  div(class = "page-header",
      span(class = "kicker", kicker),
      h2(title),
      p(class = "lead-text", desc))
}

empty_state <- function(title, ..., icon = "📭", tone = "neutral") {
  div(class = paste("empty-state", tone),
      div(class = "empty-icon", icon),
      h4(title),
      div(class = "empty-body", ...))
}

error_state <- function(msg) empty_state("문제가 발생했어요", p(msg), icon = "⚠️", tone = "error")

# 정답 여부 배지
verdict_badge <- function(pred, truth) {
  if (is.null(truth) || is.na(truth) || !nzchar(truth)) {
    return(span(class = "badge-verdict unknown", "정답 정보 없음 (Unknown)"))
  }
  if (identical(pred, truth)) span(class = "badge-verdict right", "✓ Right — 정답")
  else span(class = "badge-verdict wrong", "✗ Wrong — 오답")
}

cat_chip <- function(cat) {
  span(class = "cat-chip", style = sprintf("--chip:%s", CAT_COLORS[[cat]] %||% "#667085"), cat)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# 쿼리·베스트매치·평균·복원 4장 비교 + Top 5 갤러리
render_match_panel <- function(pca, query_vec, query_label, res, K, truth = NULL) {
  top  <- res$top
  best <- top[1, ]
  best_vec <- pca$X[match(best$id, pca$meta$id), ]
  rebuilt  <- reconstruct(pca, res$z, K)
  dmax <- max(top$distance)

  tagList(
    div(class = "prediction-banner fade-in",
        div(class = "pred-left",
            span(class = "pred-label", "Predicted category (Top 5 최빈값)"),
            div(class = "pred-value", cat_chip(res$prediction)),
            span(class = "pred-votes",
                 paste(sprintf("%s × %d", names(table(top$category)), as.integer(table(top$category))),
                       collapse = " · "))),
        div(class = "pred-right",
            span(class = "pred-label", "Actual"),
            div(class = "pred-value", if (!is.null(truth) && !is.na(truth)) cat_chip(truth) else span("—")),
            verdict_badge(res$prediction, truth))),

    div(class = "compare-grid fade-in",
        div(class = "compare-item query",
            shoe_img(gray_uri(query_vec), query_label, 150),
            div(class = "cap", strong("Your photo"), span(query_label))),
        div(class = "compare-item best",
            shoe_img(gray_uri(best_vec), paste("Best match", best$id), 150),
            div(class = "cap", strong("Best match"),
                span(sprintf("#%04d · %s · d = %.3f", best$id, best$category, best$distance)))),
        div(class = "compare-item",
            shoe_img(gray_uri(pca$mean), "Average shoe", 150),
            div(class = "cap", strong("Average shoe"), span("x̄ of 2,000 photos"))),
        div(class = "compare-item",
            shoe_img(gray_uri(rebuilt), paste("Rebuilt K =", K), 150),
            div(class = "cap", strong(sprintf("Rebuilt with K = %d", K)),
                span(sprintf("x̄ + Σ z_k v_k  (RMSE %.3f)", sqrt(mean((rebuilt - query_vec)^2))))))),

    h4(class = "sub-head", "Top 5 most similar shoes"),
    div(class = "top5-grid fade-in",
        lapply(seq_len(nrow(top)), function(i) {
          r <- top[i, ]
          v <- pca$X[match(r$id, pca$meta$id), ]
          div(class = paste("top5-card", if (i == 1) "is-best"),
              span(class = "rank", paste0("#", i)),
              shoe_img(gray_uri(v), paste("Catalogue", r$id), 110),
              div(class = "meta", strong(sprintf("ID %04d", r$id)), cat_chip(r$category)),
              div(class = "dist", span(sprintf("distance %.3f", r$distance)),
                  div(class = "dist-bar", div(style = sprintf("width:%.0f%%", 100 * r$distance / dmax)))))
        }))
  )
}
