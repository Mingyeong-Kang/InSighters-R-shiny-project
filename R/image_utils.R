# =============================================================================
# image_utils.R — 이미지 읽기 / 전처리 / 화면 표시용 변환
#
# 모든 이미지는 같은 규칙으로 4,096 차원 벡터가 됩니다.
#   - 64 × 64 흑백, 행(row) 단위로 펼침 (템플릿 app.R 의 방식과 동일한 순서)
#   - 픽셀값 0 = 검정 ~ 1 = 흰색 (0~255 를 255 로 나눈 값)
# 카탈로그·selfcheck 는 이미 64×64 흑백이라 그대로 읽고,
# 업로드/팀원 사진은 아래 preprocess_photo() 로 같은 형식으로 맞춥니다.
# =============================================================================

# 값의 범위를 [lo, hi] 로 자르되 행렬 형태(dim)는 유지
clamp <- function(m, lo = 0, hi = 1) { m[] <- pmin(pmax(m, lo), hi); m }

# 행렬(64x64, [row, col]) -> 행 단위 벡터
mat_to_vec <- function(m) as.numeric(t(m))
# 벡터 -> 행렬(64x64)
vec_to_mat <- function(v, n = IMG_SIZE) matrix(v, n, n, byrow = TRUE)

# 어떤 이미지 파일이든 [h, w] 흑백 행렬(0~1)로 읽기
read_gray <- function(path) {
  ext <- tolower(tools::file_ext(path))
  arr <- if (ext %in% c("jpg", "jpeg")) jpeg::readJPEG(path) else png::readPNG(path)
  if (length(dim(arr)) == 2) return(arr)
  ch <- dim(arr)[3]
  if (ch == 2) {                         # gray + alpha -> 흰 배경 위에 합성
    return(arr[, , 1] * arr[, , 2] + (1 - arr[, , 2]))
  }
  g <- 0.299 * arr[, , 1] + 0.587 * arr[, , 2] + 0.114 * arr[, , 3]
  if (ch == 4) g <- g * arr[, , 4] + (1 - arr[, , 4])
  g
}

# 면적 평균(box filter) 리사이즈: 큰 사진을 줄일 때 계단 현상이 적음
resize_area <- function(m, out_h, out_w) {
  h <- nrow(m); w <- ncol(m)
  ri <- pmin(h, floor((seq_len(h) - 1) * out_h / h) + 1)
  ci <- pmin(w, floor((seq_len(w) - 1) * out_w / w) + 1)
  if (h >= out_h && w >= out_w) {
    rs <- rowsum(m, ri) / as.vector(table(ri))                 # 행 방향 평균
    out <- t(rowsum(t(rs), ci) / as.vector(table(ci)))         # 열 방향 평균
    return(out)
  }
  # 확대가 필요한 경우: 최근접 이웃
  m[pmin(h, ceiling(seq_len(out_h) * h / out_h)),
    pmin(w, ceiling(seq_len(out_w) * w / out_w)), drop = FALSE]
}

# 새 사진 -> 카탈로그와 같은 64x64 형식
#  1) 흑백 변환  2) 테두리 픽셀의 중앙값을 배경으로 추정, 어두운 배경이면 반전
#  3) 배경이 1(흰색)이 되도록 밝기 보정  4) 신발 영역(bounding box) 자르기
#  5) 흰 여백으로 정사각형 패딩  6) 64x64 면적 평균 축소
preprocess_photo <- function(path, crop = TRUE, margin = 0.04) {
  g <- read_gray(path)
  h <- nrow(g); w <- ncol(g)
  if (h == IMG_SIZE && w == IMG_SIZE && !crop) return(g)
  border <- c(g[1:max(1, h %/% 50), ], g[h - 0:max(0, h %/% 50 - 1), ],
              g[, 1:max(1, w %/% 50)], g[, w - 0:max(0, w %/% 50 - 1)])
  bg <- stats::median(border)
  if (bg < 0.5) { g <- 1 - g; bg <- 1 - bg }
  g <- clamp(g / max(bg, 0.05))                        # 배경 -> 1
  if (crop) {
    mask <- g < 0.85
    rows <- which(rowMeans(mask) > 0.01)
    cols <- which(colMeans(mask) > 0.01)
    if (length(rows) > 2 && length(cols) > 2) {
      g <- g[min(rows):max(rows), min(cols):max(cols), drop = FALSE]
    }
  }
  # 정사각형 + 여백
  s <- max(dim(g)); s <- ceiling(s * (1 + 2 * margin))
  canvas <- matrix(1, s, s)
  r0 <- (s - nrow(g)) %/% 2; c0 <- (s - ncol(g)) %/% 2
  canvas[r0 + seq_len(nrow(g)), c0 + seq_len(ncol(g))] <- g
  out <- resize_area(canvas, IMG_SIZE, IMG_SIZE)
  clamp(out)
}

# ---- 화면 표시: 행렬 -> PNG data URI ---------------------------------------
png_uri <- function(arr) {
  raw <- png::writePNG(arr)
  paste0("data:image/png;base64,", base64enc::base64encode(raw))
}

# 흑백 이미지 (0~1)
gray_uri <- function(v) png_uri(clamp(vec_to_mat(v)))

# 부호가 있는 값(사진-평균, eigenshoe): 0 = 흰색, 음수 = 파랑, 양수 = 주황
diverging_uri <- function(v, lim = NULL) {
  m <- vec_to_mat(v)
  if (is.null(lim)) lim <- max(abs(m), 1e-9)
  t <- clamp(m / lim, -1, 1)
  neg <- c(0.13, 0.40, 0.75); pos <- c(0.85, 0.37, 0.05)
  arr <- array(1, c(nrow(m), ncol(m), 3))
  for (k in 1:3) {
    arr[, , k] <- ifelse(t < 0, 1 + (-t) * (neg[k] - 1), 1 + t * (pos[k] - 1))
  }
  png_uri(arr)
}

# 카드 안에 들어가는 이미지 태그 (클릭하면 확대 모달)
shoe_img <- function(uri, alt = "shoe", size = 128, cls = "") {
  tags$img(src = uri, alt = alt, width = size, height = size,
           class = paste("shoe-img zoomable", cls), tabindex = "0")
}
