# =============================================================================
# data1_loader.R — Data 1 데이터 로딩 / 검증 / 집계 (UI 와 분리)
# 원본(업소 1행 = 1건) → 업소 이벤트 표(events) → 동네×연도 표(build_yearly)
# load_data1_real() 의 반환값:
#   list(ok, reason, events, dongs, dong_labels, years, industries, cutoff, metrics,
#        labels, colors, rate_colors, analyses, has, unmatched, bad_dates)
# =============================================================================

if (!exists("%||%", mode = "function"))
  `%||%` <- function(a, b) if (is.null(a) || !length(a) || (length(a) == 1 && is.na(a))) b else a

DATA1_RATE_METRICS <- c("open_rate", "close_rate")

load_data1_config <- function() {
  env <- new.env()
  cfg_path <- file.path(PATH_DATA1_DIR, "data1_config.R")
  if (file.exists(cfg_path)) source(cfg_path, local = env, encoding = "UTF-8")
  as.list(env)
}

# 인코딩을 자동으로 찾아 읽기 (UTF-8 BOM / UTF-8 / CP949 순서). 모든 컬럼을 문자로 읽는다.
read_data1_raw <- function(f, cols) {
  need <- unlist(cols[c("dong", "open_date", "close_date")])
  for (enc in c("UTF-8-BOM", "UTF-8", "CP949")) {
    raw <- tryCatch(
      suppressWarnings(utils::read.csv(f, fileEncoding = enc, check.names = FALSE,
                                       colClasses = "character", stringsAsFactors = FALSE)),
      error = function(e) NULL)
    if (!is.null(raw) && nrow(raw) && all(need %in% names(raw))) return(raw)
  }
  NULL
}

to_date_safe <- function(x) {
  x <- trimws(x)                       # 폐업일자가 공백 문자열인 행(영업 중)이 있음
  x[!nzchar(x)] <- NA_character_
  as.Date(substr(x, 1, 10), format = "%Y-%m-%d")
}

# raw → 표준 events 표 (dong, industry, open, close)
standardize_data1 <- function(raw, cols, dongs, year_from = 2015) {
  has_ind <- !is.null(cols$industry) && length(cols$industry) == 1 &&
    !is.na(cols$industry) && cols$industry %in% names(raw)
  
  ev <- data.frame(
    dong     = trimws(raw[[cols$dong]]),
    industry = if (has_ind) trimws(raw[[cols$industry]]) else NA_character_,
    open     = to_date_safe(raw[[cols$open_date]]),
    close    = to_date_safe(raw[[cols$close_date]]),
    stringsAsFactors = FALSE)
  if (has_ind) ev$industry[is.na(ev$industry) | !nzchar(ev$industry)] <- "(미상)"
  
  unmatched <- sum(!(ev$dong %in% dongs))
  ev <- ev[ev$dong %in% dongs, , drop = FALSE]
  bad_dates <- sum(is.na(ev$open))                 # 개업일을 알 수 없는 행은 제외
  ev <- ev[!is.na(ev$open), , drop = FALSE]
  if (!nrow(ev)) return(list(ok = FALSE, reason = "분석 대상 동네(행정동)에 해당하는 행이 없습니다."))
  
  last_date <- max(c(ev$open, ev$close), na.rm = TRUE)
  year_to <- as.integer(format(last_date, "%Y"))
  list(ok = TRUE, events = ev, dongs = dongs,
       years = seq.int(as.integer(year_from), year_to),
       industries = if (has_ind) sort(unique(ev$industry)) else NULL,
       cutoff = last_date, unmatched = unmatched, bad_dates = bad_dates,
       has = list(industry = has_ind))
}

load_data1_real <- function() {
  cfg <- load_data1_config()
  f <- cfg$DATA1_FILE %||% file.path(PATH_DATA1_DIR, "data1_raw.csv")
  if (!file.exists(f))
    return(list(ok = FALSE, reason = sprintf("데이터 파일 '%s' 이(가) 없습니다.", f)))
  cols <- cfg$DATA1_COLUMNS
  raw <- read_data1_raw(f, cols)
  if (is.null(raw))
    return(list(ok = FALSE, reason = "CSV 를 읽을 수 없거나, 설정한 컬럼(행정동·인허가일자·폐업일자)이 없습니다."))
  
  dongs <- cfg$DATA1_DONGS %||% c("대현동", "청파동", "동선동")
  out <- standardize_data1(raw, cols, dongs, cfg$DATA1_YEAR_FROM %||% 2015)
  if (!isTRUE(out$ok)) return(out)
  
  labels <- cfg$DATA1_METRIC_LABELS
  out$metrics <- names(labels)
  out$labels <- labels
  out$dong_labels <- cfg$DATA1_DONG_LABELS %||% stats::setNames(dongs, dongs)
  out$colors <- cfg$DATA1_DONG_COLORS
  out$rate_colors <- cfg$DATA1_RATE_COLORS
  out$analyses <- cfg$DATA1_ANALYSES %||% list()
  out
}

# ---- 동네 × 연도 집계 ---------------------------------------------------------
# 개업수 = 인허가 연도 건수, 폐업수 = 폐업 연도 건수, 순증감 = 개업수 − 폐업수
# 개업률·폐업률의 분모 = 연초(1/1) 영업 중이던 업소 수 + 그해 개업수
build_yearly <- function(ev, dongs, years) {
  ev$oy <- as.integer(format(ev$open, "%Y"))
  ev$cy <- as.integer(format(ev$close, "%Y"))
  rows <- lapply(dongs, function(d) {
    s <- ev[ev$dong == d, , drop = FALSE]
    do.call(rbind, lapply(years, function(y) {
      jan1 <- as.Date(sprintf("%d-01-01", y))
      start_n <- sum(s$open < jan1 & (is.na(s$close) | s$close >= jan1))
      open_n <- sum(s$oy == y, na.rm = TRUE)
      close_n <- sum(s$cy == y, na.rm = TRUE)
      base <- start_n + open_n
      data.frame(.dong = d, .year = y, start_n = start_n, open_n = open_n, close_n = close_n,
                 net_n = open_n - close_n,
                 open_rate = if (base > 0) open_n / base else NA_real_,
                 close_rate = if (base > 0) close_n / base else NA_real_,
                 stringsAsFactors = FALSE)
    }))
  })
  do.call(rbind, rows)
}

# ---- 집계 함수 ----------------------------------------------------------------
summarise_stat <- function(x, stat) {
  x <- x[!is.na(x)]
  switch(stat,
         mean = if (length(x)) mean(x) else NA_real_,
         median = if (length(x)) stats::median(x) else NA_real_,
         sum = sum(x),
         count = length(x))
}

# 동네별 요약: n = 연도 수, mean / median / sum
group_summary <- function(df, metric, by = ".dong", order = NULL) {
  keys <- unique(as.character(df[[by]]))
  if (!is.null(order)) keys <- intersect(order, keys)
  sp <- lapply(keys, function(k) df[[metric]][as.character(df[[by]]) == k])
  data.frame(group = keys,
             n = vapply(sp, function(x) sum(!is.na(x)), integer(1)),
             mean = vapply(sp, summarise_stat, numeric(1), stat = "mean"),
             median = vapply(sp, summarise_stat, numeric(1), stat = "median"),
             sum = vapply(sp, summarise_stat, numeric(1), stat = "sum"),
             stringsAsFactors = FALSE)
}

# 지표 값 표시: 비율은 %, 개수는 천 단위 구분
fmt_metric <- function(x, metric, digits = 2) {
  if (metric %in% DATA1_RATE_METRICS) sprintf("%.*f%%", digits, 100 * x)
  else format(round(x, digits), big.mark = ",", trim = TRUE)
}