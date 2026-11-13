# =============================================================================
# pca_engine.R — PCA 학습(사전계산), 투영, 검색, 복원
#
# 표기:  X  (n=2000 × p=4096)  각 행이 한 장의 신발
#        x̄  = colMeans(X)                  평균 신발
#        Xc = X − x̄                        중심화 (표준화는 하지 않음: 모든 픽셀이 같은 단위)
#        Xc = U D Vᵀ  (SVD)                V 의 열 = 주성분 loadings = eigenshoes
#        scores = Xc V = U D               각 신발의 PC 점수
#        설명분산비율_j = d_j² / Σ d²
#  새 사진 x:  z = (x − x̄) V[,1:K]        학습 때의 평균과 loadings 를 그대로 사용
#  검색:      거리 = ‖z − scores_i[1:K]‖₂  (PC 공간의 유클리드 거리)
#  복원:      x̂_K = x̄ + z V[,1:K]ᵀ
# =============================================================================

# ---- 원본 이미지 행렬 읽기 ---------------------------------------------------
load_catalogue_matrix <- function(shoe_dir = PATH_SHOES, meta_path = PATH_META) {
  meta <- utils::read.csv(meta_path, stringsAsFactors = FALSE)
  files <- file.path(shoe_dir, sprintf("%04d.png", meta$id))
  missing <- files[!file.exists(files)]
  if (length(missing)) stop(sprintf("신발 이미지 %d개를 찾을 수 없습니다. 예: %s",
                                    length(missing), missing[1]))
  X <- t(vapply(files, function(f) mat_to_vec(read_gray(f)), numeric(IMG_SIZE^2)))
  rownames(X) <- meta$id
  list(X = X, meta = meta)
}

# ---- PCA 계산 (scripts/precompute_pca.R 또는 캐시가 없을 때 1회) -------------
build_pca <- function(X, meta, n_keep = N_PC_KEEP) {
  mu <- colMeans(X)
  Xc <- sweep(X, 2, mu)
  # 2000 × 4096 이므로 XcXcᵀ (2000×2000) 고유분해가 SVD 보다 빠르고 결과는 동일
  G  <- tcrossprod(Xc)
  eg <- eigen(G, symmetric = TRUE)
  lam <- pmax(eg$values, 0)                    # = d²
  keep <- seq_len(min(n_keep, sum(lam > 1e-10)))
  d <- sqrt(lam[keep])
  U <- eg$vectors[, keep, drop = FALSE]
  V <- crossprod(Xc, U) %*% diag(1 / d)        # V = Xcᵀ U D⁻¹   (p × k)
  # 부호 규칙: 각 PC 의 loadings 합이 양수가 되도록 (그림 해석 일관성)
  sgn <- sign(colSums(V)); sgn[sgn == 0] <- 1
  V <- sweep(V, 2, sgn, `*`); U <- sweep(U, 2, sgn, `*`)
  scores <- sweep(U, 2, d, `*`)                # = Xc V
  ve <- lam / sum(lam)
  list(
    mean      = mu,
    loadings  = V,
    scores    = scores,
    sdev      = d / sqrt(nrow(X) - 1),
    var_ratio = ve[keep],
    cum_ratio = cumsum(ve)[keep],
    meta      = meta,
    X         = X,                             # 원본(카탈로그 이미지 표시용)
    created   = Sys.time()
  )
}

# ---- 정확성 검증: 직교성 & 복원 오차 ------------------------------------------
verify_pca <- function(pca) {
  V <- pca$loadings
  k <- ncol(V)
  orth_err <- max(abs(crossprod(V) - diag(k)))
  Xc <- sweep(pca$X, 2, pca$mean)
  score_err <- max(abs(Xc %*% V - pca$scores))
  # prcomp 와 비교 (분산비율 PC1~5)
  list(orthonormal_max_error = orth_err,
       score_max_error       = score_err)
}

# ---- 캐시 로드 ---------------------------------------------------------------
load_or_build_pca <- function() {
  if (file.exists(PATH_PCA_CACHE)) {
    pca <- readRDS(PATH_PCA_CACHE)
    attr(pca, "source") <- "cache"
    return(pca)
  }
  cat_data <- load_catalogue_matrix()
  pca <- build_pca(cat_data$X, cat_data$meta)
  pca$loo_accuracy <- loo_accuracy(pca)
  dir.create(dirname(PATH_PCA_CACHE), showWarnings = FALSE, recursive = TRUE)
  try(saveRDS(pca, PATH_PCA_CACHE), silent = TRUE)
  attr(pca, "source") <- "computed"
  pca
}

# ---- 투영 / 복원 -------------------------------------------------------------
project_photo <- function(pca, x, K = ncol(pca$loadings)) {
  as.numeric(crossprod(pca$loadings[, seq_len(K), drop = FALSE], x - pca$mean))
}

reconstruct <- function(pca, z, K) {
  K <- min(K, length(z))
  pca$mean + as.numeric(pca$loadings[, seq_len(K), drop = FALSE] %*% z[seq_len(K)])
}

# ---- 검색 + 분류 -------------------------------------------------------------
# exclude_id: 카탈로그 사진을 검색할 때 자기 자신을 제외
search_similar <- function(pca, x, K, top_n = TOP_N, exclude_id = NULL) {
  z  <- project_photo(pca, x, K)
  S  <- pca$scores[, seq_len(K), drop = FALSE]
  d2 <- rowSums(sweep(S, 2, z)^2)
  ids <- pca$meta$id
  if (!is.null(exclude_id)) d2[ids %in% exclude_id] <- Inf
  ord <- order(d2)[seq_len(top_n)]
  top <- data.frame(rank = seq_len(top_n), id = ids[ord],
                    category = pca$meta$category[ord],
                    distance = sqrt(d2[ord]), stringsAsFactors = FALSE)
  list(z = z, top = top, prediction = vote_category(top))
}

# 좌우 방향 자동 맞춤: 원본과 좌우반전 사진을 둘 다 검색해 Best match 거리가 더 짧은 쪽을 사용
# (카탈로그 신발은 대부분 앞코가 오른쪽 — 팀원 구가현 브랜치 아이디어). 업로드·팀원 사진용.
search_auto_flip <- function(pca, x, K, top_n = TOP_N, exclude_id = NULL) {
  a <- search_similar(pca, x, K, top_n, exclude_id)
  xf <- mirror_vec(x)
  b <- search_similar(pca, xf, K, top_n, exclude_id)
  if (b$top$distance[1] < a$top$distance[1]) { b$x <- xf; b$flipped <- TRUE; b }
  else { a$x <- x; a$flipped <- FALSE; a }
}

# 최빈 카테고리. 동률이면 동률 카테고리 중 가장 가까운 신발의 카테고리를 선택
vote_category <- function(top) vote_vec(top$category)

# cc: 가까운 순서대로 정렬된 카테고리 벡터
vote_vec <- function(cc) {
  u <- unique(cc)
  cnt <- tabulate(match(cc, u))
  winners <- u[cnt == max(cnt)]
  cc[cc %in% winners][1]
}

# ---- 추가 분석: Leave-one-out Top-5 투표 정확도 (K = 1 ~ 100) -------------
# 카탈로그의 각 사진을 자기 자신을 뺀 나머지 1,999장에서 검색했을 때 정답률.
# 전체 정렬 대신 행별 최솟값을 5번 찾는 방식으로 빠르게 계산 (팀원 구가현 prep.R 아이디어).
loo_accuracy <- function(pca, Ks = 1:min(100, ncol(pca$scores))) {
  cats <- pca$meta$category
  n <- length(cats)
  acc <- vapply(Ks, function(K) {
    S <- pca$scores[, seq_len(K), drop = FALSE]
    sq <- rowSums(S^2)
    D <- outer(sq, sq, "+") - 2 * tcrossprod(S)
    diag(D) <- Inf
    nn <- matrix(0L, n, TOP_N)
    for (j in seq_len(TOP_N)) {
      nn[, j] <- max.col(-D, ties.method = "first")
      D[cbind(seq_len(n), nn[, j])] <- Inf
    }
    pred <- apply(matrix(cats[nn], n, TOP_N), 1, vote_vec)
    mean(pred == cats)
  }, numeric(1))
  data.frame(K = Ks, accuracy = acc)
}
