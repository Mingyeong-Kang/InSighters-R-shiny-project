# =============================================================================
# ui_helpers.R — 여러 탭에서 재사용하는 UI 조각
# CSS/JS 파일 없이 R 함수(shiny + bslib)만으로 화면을 만든다.
# 색·모서리·폰트는 app.R 의 bs_theme() 한 곳에서 정하고,
# 여기서는 bslib 컴포넌트(card, layout_column_wrap 등)와 Bootstrap 기본 클래스만 사용한다.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# 카드: 제목 + 본문 (+ 오른쪽 버튼 등)
ui_card <- function(title = NULL, ..., subtitle = NULL, icon = NULL, header_right = NULL,
                    full_screen = FALSE) {
  card(
    full_screen = full_screen,
    if (!is.null(title)) card_header(
      div(class = "d-flex justify-content-between align-items-center gap-2",
          div(strong(paste(c(icon, title), collapse = " ")),
              if (!is.null(subtitle)) div(class = "small text-muted", subtitle)),
          header_right)),
    card_body(fillable = FALSE, ...)   # 내용 높이만큼만 (빈 공간으로 늘어나지 않게)
  )
}

page_header <- function(kicker, title, desc) {
  div(class = "mb-3",
      div(class = "small text-primary fw-semibold text-uppercase", kicker),
      h2(class = "fw-bold mb-1", title),
      p(class = "text-muted mb-0", desc))
}

hint <- function(...) p(class = "small text-muted mb-0", ...)

empty_state <- function(title, ..., icon = "📭", tone = "neutral") {
  card(class = if (tone == "error") "border-danger" else NULL,
       card_body(class = "text-center py-4",
                 div(class = "fs-2", icon),
                 h5(class = "fw-bold", title),
                 div(class = "text-muted", ...)))
}

error_state <- function(msg) empty_state("Something went wrong", p(msg), icon = "⚠️", tone = "error")

notice <- function(..., type = "warning") div(class = paste0("alert alert-", type, " py-2 small"), ...)

# 정답 여부 배지 (Bootstrap badge 클래스)
verdict_badge <- function(pred, truth) {
  if (is.null(truth) || is.na(truth) || !nzchar(truth)) {
    return(span(class = "badge text-bg-secondary fs-6", "Actual category unknown"))
  }
  if (identical(pred, truth)) span(class = "badge text-bg-success fs-6", "✓ Right")
  else span(class = "badge text-bg-danger fs-6", "✗ Wrong")
}

# 카테고리 배지: 색 대신 Bootstrap 테마 색 이름으로 구분 + 글자 라벨 병기
CAT_BADGE <- c(Boots = "text-bg-primary", Sandals = "text-bg-warning",
               Shoes = "text-bg-success", Slippers = "text-bg-info")
cat_chip <- function(cat, size = "") {
  span(class = paste("badge rounded-pill", CAT_BADGE[[cat]] %||% "text-bg-secondary", size), cat)
}

# 신발 그림 한 장 + 설명 (작은 카드)
shoe_figure <- function(v, title, subtitle = NULL, size = 128, diverging = FALSE, lim = NULL,
                        highlight = NULL) {
  uri <- if (diverging) diverging_uri(v, lim, scale = 3) else gray_uri(v, scale = 3)
  card(class = switch(highlight %||% "none", best = "border-primary border-2",
                      query = "border-dark border-2", NULL),
       card_body(class = "text-center p-2",
                 tags$img(src = uri, alt = title, width = size, height = size, class = "img-fluid border rounded"),
                 div(class = "small fw-semibold mt-1", title),
                 if (!is.null(subtitle)) div(class = "small text-muted", subtitle)))
}

# 쿼리·베스트매치·평균·복원 4장 비교 + Top 5 갤러리
render_match_panel <- function(pca, query_vec, query_label, res, K, truth = NULL) {
  top  <- res$top
  best <- top[1, ]
  best_vec <- pca$X[match(best$id, pca$meta$id), ]
  rebuilt  <- reconstruct(pca, res$z, K)
  votes <- table(top$category)

  tagList(
    card(class = "border-primary",
         card_body(
           layout_columns(col_widths = breakpoints(sm = 12, md = c(6, 6)),
             div(div(class = "small text-muted text-uppercase fw-semibold", "Predicted category (Top 5 majority vote)"),
                 div(class = "my-1", cat_chip(res$prediction, "fs-5")),
                 div(class = "small text-muted",
                     paste(sprintf("%s × %d", names(votes), as.integer(votes)), collapse = " · "))),
             div(class = "text-md-end",
                 div(class = "small text-muted text-uppercase fw-semibold", "Actual"),
                 div(class = "my-1", if (!is.null(truth) && !is.na(truth)) cat_chip(truth, "fs-5") else "—"),
                 verdict_badge(res$prediction, truth))))),

    layout_column_wrap(width = "150px", fill = FALSE,
      shoe_figure(query_vec, "Your photo", query_label, 150, highlight = "query"),
      shoe_figure(best_vec, "Best match", sprintf("#%04d · %s · d = %.3f", best$id, best$category, best$distance),
                  150, highlight = "best"),
      shoe_figure(pca$mean, "Average shoe", "x̄ of 2,000 photos", 150),
      shoe_figure(rebuilt, sprintf("Rebuilt with K = %d", K),
                  sprintf("x̄ + Σ z_k v_k (RMSE %.3f)", sqrt(mean((rebuilt - query_vec)^2))), 150)),

    h5(class = "fw-bold mt-3", "Top 5 most similar shoes"),
    do.call(layout_column_wrap, c(list(width = "120px", fill = FALSE),
      lapply(seq_len(nrow(top)), function(i) {
        r <- top[i, ]
        v <- pca$X[match(r$id, pca$meta$id), ]
        card(class = if (i == 1) "border-primary border-2" else NULL,
             card_body(class = "text-center p-2",
                       span(class = "badge text-bg-dark", paste0("#", i)),
                       tags$img(src = gray_uri(v, scale = 3), alt = paste("Catalogue", r$id),
                                width = 110, height = 110, class = "img-fluid border rounded my-1"),
                       div(class = "small fw-semibold", sprintf("ID %04d", r$id)),
                       cat_chip(r$category),
                       div(class = "small text-muted mt-1", sprintf("distance %.3f", r$distance)),
                       tags$progress(value = round(r$distance, 3), max = round(max(top$distance), 3),
                                     class = "w-100")))
      })))
  )
}
