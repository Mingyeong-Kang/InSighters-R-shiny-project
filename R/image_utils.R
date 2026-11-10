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

# 휴대폰 사진 읽기: magick 이 있으면 EXIF 회전 정보대로 바로 세우고(세로 사진이 눕는 문제 방지),
# 투명 배경은 흰색으로 채우고, 계산이 빠르도록 긴 변을 800px 이하로 줄인다.
# (팀원 구가현·조가희 브랜치의 magick 전처리 아이디어를 반영)
read_photo_gray <- function(path) {
  if (!requireNamespace("magick", quietly = TRUE)) return(read_gray(path))
  img <- magick::image_read(path)[1]
  img <- magick::image_orient(img)
  img <- magick::image_flatten(magick::image_background(img, "white"))
  img <- magick::image_scale(img, "800x800>")
  a <- as.integer(magick::image_data(img, channels = "gray"))[, , 1]   # [height, width]
  a / 255
}

# 새 사진 -> 카탈로그와 같은 64x64 형식
#  1) 흑백 변환 (EXIF 회전 보정)
#  2) 테두리 픽셀의 중앙값을 배경으로 추정, 어두운 배경이면 반전
#  3) 배경이 1(흰색)이 되도록 밝기 보정
#  4) 신발 영역(bounding box) 자르기
#  5) 카탈로그와 같은 자리에 배치: 비율을 유지한 채 62 x 47 상자에 맞추고,
#     가로 가운데 + 바닥을 55번째 줄에 맞춤 (실제 카탈로그 2,000장 측정값, config.R)
preprocess_photo <- function(path, crop = TRUE) {
  g <- if (crop) read_photo_gray(path) else read_gray(path)
  h <- nrow(g); w <- ncol(g)
  if (h == IMG_SIZE && w == IMG_SIZE && !crop) return(g)     # selfcheck: 이미 카탈로그 형식
  b <- max(1, min(h, w) %/% 50)
  border <- c(g[seq_len(b), ], g[h - seq_len(b) + 1, ], g[, seq_len(b)], g[, w - seq_len(b) + 1])
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
  place_like_catalogue(g)
}

# 잘라낸 신발 이미지를 64x64 흰 도화지의 카탈로그 위치에 놓기
place_like_catalogue <- function(g, box_w = CAT_BOX_W, box_h = CAT_BOX_H, bottom = CAT_BOTTOM_ROW) {
  s  <- min(box_w / ncol(g), box_h / nrow(g))
  nh <- max(1, min(box_h, round(nrow(g) * s)))
  nw <- max(1, min(box_w, round(ncol(g) * s)))
  small  <- resize_area(g, nh, nw)
  canvas <- matrix(1, IMG_SIZE, IMG_SIZE)
  r0 <- bottom - nh                      # 바닥 줄 = r0 + nh
  c0 <- (IMG_SIZE - nw) %/% 2
  canvas[r0 + seq_len(nh), c0 + seq_len(nw)] <- small
  clamp(canvas)
}

# 좌우 반전 (카탈로그 신발은 대부분 앞코가 오른쪽). 4,096 벡터 -> 4,096 벡터
mirror_vec <- function(v) mat_to_vec(vec_to_mat(v)[, IMG_SIZE:1])

# ---- 화면 표시: 행렬 -> PNG data URI -------------------------------------
# 64x64 를 브라우저가 확대하면 흐려지므로, R 에서 미리 scale 배로 키운다(최근접 이웃, kronecker).
upscale <- function(m, scale = 1) if (scale > 1) kronecker(m, matrix(1, scale, scale)) else m

png_uri <- function(arr) {
  raw <- png::writePNG(arr)
  paste0("data:image/png;base64,", base64enc::base64encode(raw))
}

# 흑백 이미지 (0~1)
gray_uri <- function(v, scale = 1) png_uri(upscale(clamp(vec_to_mat(v)), scale))

# 부호가 있는 값(사진-평균, eigenshoe): 0 = 흰색, 음수 = 파랑, 양수 = 주황
diverging_rgb <- function(m, lim) {
  t <- clamp(m / lim, -1, 1)
  neg <- c(0.13, 0.40, 0.75); pos <- c(0.85, 0.37, 0.05)
  arr <- array(1, c(nrow(m), ncol(m), 3))
  for (k in 1:3) arr[, , k] <- ifelse(t < 0, 1 + (-t) * (neg[k] - 1), 1 + t * (pos[k] - 1))
  arr
}
diverging_uri <- function(v, lim = NULL, scale = 1) {
  m <- vec_to_mat(v)
  if (is.null(lim)) lim <- max(abs(m), 1e-9)
  png_uri(diverging_rgb(upscale(m, scale), lim))
}

# 범례용 색 막대 (−lim … 0 … +lim) 를 R 로 그린 PNG
diverging_legend_uri <- function(width = 240, height = 14) {
  ramp <- matrix(rep(seq(-1, 1, length.out = width), each = height), height, width)
  png_uri(diverging_rgb(ramp, 1))
}

# 이미지 태그
shoe_img <- function(uri, alt = "shoe", size = 128) {
  tags$img(src = uri, alt = alt, width = size, height = size, class = "img-fluid border rounded")
}
