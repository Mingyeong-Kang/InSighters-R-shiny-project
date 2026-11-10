# =============================================================================
# data1_loader.R — Data 1 데이터 로딩 / 검증 / 표준화 (UI 와 분리)
# 결과는 항상 같은 표준 형식으로 반환:
#   list(ok, reason, df(.univ, .period, .industry, + metric cols), metrics, labels, has)
# =============================================================================

load_data1_config <- function() {
  env <- new.env()
  cfg_path <- file.path(PATH_DATA1_DIR, "data1_config.R")
  if (file.exists(cfg_path)) sys.source(cfg_path, envir = env)
  as.list(env)
}

standardize_data1 <- function(raw, cols, univ_values, metric_labels = c()) {
  has_col <- function(x) !is.null(x) && length(x) == 1 && !is.na(x) && x %in% names(raw)
  if (!has_col(cols$university))
    return(list(ok = FALSE, reason = sprintf("university 컬럼(%s)을 CSV 에서 찾을 수 없습니다.",
                                             cols$university %||% "NA")))
  metrics <- intersect(cols$metrics, names(raw))
  metrics <- metrics[vapply(metrics, function(m) is.numeric(raw[[m]]), logical(1))]
  if (!length(metrics))
    return(list(ok = FALSE, reason = "숫자형 지표(metrics) 컬럼이 하나도 매핑되지 않았습니다."))

  key <- names(univ_values)[match(as.character(raw[[cols$university]]), univ_values)]
  df <- data.frame(.univ = key, stringsAsFactors = FALSE)
  df$.period   <- if (has_col(cols$period))   raw[[cols$period]]   else NA
  df$.industry <- if (has_col(cols$industry)) as.character(raw[[cols$industry]]) else NA
  for (m in metrics) df[[m]] <- raw[[m]]
  unmatched <- sum(is.na(df$.univ))
  df <- df[!is.na(df$.univ), , drop = FALSE]
  labels <- stats::setNames(metrics, metrics)
  hit <- intersect(names(metric_labels), metrics)
  labels[hit] <- metric_labels[hit]
  list(ok = TRUE, df = df, metrics = metrics, labels = labels, unmatched = unmatched,
       has = list(period = has_col(cols$period), industry = has_col(cols$industry)))
}

load_data1_real <- function() {
  cfg <- load_data1_config()
  f <- cfg$DATA1_FILE %||% file.path(PATH_DATA1_DIR, "data1.csv")
  if (!file.exists(f))
    return(list(ok = FALSE, reason = sprintf("데이터 파일 '%s' 이(가) 없습니다.", f)))
  raw <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = FALSE),
                  error = function(e) NULL)
  if (is.null(raw) || !nrow(raw)) return(list(ok = FALSE, reason = "CSV 를 읽을 수 없거나 비어 있습니다."))
  out <- standardize_data1(raw, cfg$DATA1_COLUMNS, cfg$DATA1_UNIV_VALUES, cfg$DATA1_METRIC_LABELS %||% c())
  out$analyses <- cfg$DATA1_ANALYSES
  out
}

# ---- DEMO 전용 가상 데이터: 실제 결과가 아님 --------------------------------
make_demo_data1 <- function(seed = 1) {
  set.seed(seed)
  g <- expand.grid(univ = c("이화여대", "숙명여대", "성신여대"), year = 2020:2025,
                   industry = c("카페", "음식점", "편의점", "미용", "의류"), stringsAsFactors = FALSE)
  g$demo_metric_a <- round(stats::rpois(nrow(g), 40))
  g$demo_metric_b <- round(stats::rlnorm(nrow(g), 3, 0.5), 1)
  out <- standardize_data1(g, list(university = "univ", period = "year", industry = "industry",
                                   metrics = c("demo_metric_a", "demo_metric_b")),
                           c(Ewha = "이화여대", Sookmyung = "숙명여대", Sungshin = "성신여대"),
                           c(demo_metric_a = "Demo metric A", demo_metric_b = "Demo metric B"))
  out$analyses <- list(
    list(title = "Demo analysis 1", question = "(DEMO) 대학별 Demo metric A 평균은?",
         metric = "demo_metric_a", stat = "mean",
         interpretation = "DEMO DATA — 해석하지 마세요.", next_step = "실제 데이터 연결 후 작성"),
    list(title = "Demo analysis 2", question = "(DEMO) 대학별 Demo metric B 중앙값은?",
         metric = "demo_metric_b", stat = "median",
         interpretation = "DEMO DATA — 해석하지 마세요.", next_step = "실제 데이터 연결 후 작성"),
    list(title = "Demo analysis 3", question = "(DEMO) 대학별 레코드 수는?",
         metric = "demo_metric_a", stat = "count",
         interpretation = "DEMO DATA — 해석하지 마세요.", next_step = "실제 데이터 연결 후 작성"))
  out
}

# ---- 집계 함수 ----------------------------------------------------------------
summarise_stat <- function(x, stat) {
  x <- x[!is.na(x)]
  switch(stat,
         mean = if (length(x)) mean(x) else NA,
         median = if (length(x)) stats::median(x) else NA,
         sum = sum(x),
         count = length(x))
}

group_summary <- function(df, metric, by = ".univ") {
  sp <- split(df[[metric]], df[[by]])
  n_total <- nrow(df)
  data.frame(group = names(sp),
             n = vapply(sp, length, integer(1)),
             share_pct = 100 * vapply(sp, length, integer(1)) / max(1, n_total),
             mean = vapply(sp, function(x) summarise_stat(x, "mean"), numeric(1)),
             median = vapply(sp, function(x) summarise_stat(x, "median"), numeric(1)),
             sum = vapply(sp, function(x) summarise_stat(x, "sum"), numeric(1)),
             stringsAsFactors = FALSE, row.names = NULL)
}
