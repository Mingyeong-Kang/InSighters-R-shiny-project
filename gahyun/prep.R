# =============================================================================
# prep.R — PCA 사전 계산 (한 번만 실행, 약 2.5분)
# 카탈로그 신발 2,000장을 읽어 PCA를 하고, K = 1~100 각각의 정확도를 미리 계산해
# pca_cache.rds 한 파일로 저장한다. app.R은 이 파일만 읽으므로 앱이 바로 뜬다.
# app.R이 캐시 파일이 없을 때 이 스크립트를 자동으로 실행한다.
# 다시 계산하고 싶으면 pca_cache.rds를 지우고 앱을 다시 실행하면 된다.
# =============================================================================
library(magick)

# 사진 한 장 -> 숫자 4,096개. 템플릿 app.R에 주석으로 있던 코드와 같다.
# 64 x 64 흑백, 윗줄부터 한 줄씩 (0 = 검정, 255 = 흰색)
read_gray64 <- function(path) {
  img <- image_read(path)
  as.numeric(t(matrix(as.integer(image_data(img, channels = "gray")), 64, 64)))
}

# ---- 1. 사진 읽기 ----
meta <- read.csv("shoes_meta.csv")     # id, category
meta$category <- factor(meta$category, levels = c("Boots", "Sandals", "Shoes", "Slippers"))
files <- sprintf("shoes/%04d.png", meta$id)            # 1 -> "shoes/0001.png"

X <- t(vapply(files, read_gray64, numeric(4096)))      # 2000 x 4096 (사진 한 장 = 한 행)
rownames(X) <- meta$id

# ---- 2. 평균 빼기 (중심화) ----
KMAX <- 100                            # 저장할 PC 개수
mu  <- colMeans(X)                     # 평균 신발 x̄: 픽셀별 평균
Xc  <- sweep(X, 2, mu)                 # 모든 사진에서 평균을 뺀 행렬
n   <- nrow(Xc)

# ---- 3. PCA ----
# prcomp(Xc)는 2000 x 4096 행렬을 통째로 SVD해서 이 노트북에서 80초 넘게 걸렸다.
# 사진 수(n = 2000) < 픽셀 수(p = 4096) 이므로, 더 작은 2000 x 2000 그람 행렬을 고유분해해도 같은 PC가 나온다.
#   Xc Xc' / (n-1) = U L U'
#   적재벡터(eigenshoes) V = Xc' U / sqrt((n-1) L)   (각 열의 길이 = 1)
#   점수 S = Xc V
# PC 부호(+/-)는 임의라 prcomp와 색이 반대일 수 있지만 거리·검색 결과는 같다.
G   <- tcrossprod(Xc) / (n - 1)                       # 2000 x 2000
e   <- eigen(G, symmetric = TRUE)                     # 고유값 큰 순서로 정렬되어 나옴
eig <- pmax(e$values, 0)                              # 모든 고유값 = 각 PC의 분산 (아주 작은 음수 오차는 0으로)
V   <- crossprod(Xc, e$vectors[, 1:KMAX]) %*% diag(1 / sqrt((n - 1) * eig[1:KMAX]))   # 4096 x 100
S   <- Xc %*% V                                       # 2000 x 100 점수
colnames(V) <- colnames(S) <- paste0("PC", 1:KMAX)
var_exp <- eig / sum(eig)                             # PC별 설명분산 비율 (합 = 1)

# ---- 4. 예측 규칙 (app.R의 vote5와 같은 규칙을 여러 사진에 한꺼번에 적용) ----
# Top 5 중 가장 많은 카테고리. 동점이면 동점 카테고리 중 더 가까운 순위에 먼저 나온 것.
vote5 <- function(catnn) {                           # catnn: (사진 수 x 5) 카테고리 번호, 가까운 순
  cnt <- sapply(1:4, function(c) rowSums(catnn == c))                       # 사진별 카테고리 개수 (n x 4)
  top <- apply(cnt, 1, max)                                                 # 사진별 최다 득표 수
  pos_cnt <- matrix(cnt[cbind(rep(seq_len(nrow(catnn)), 5), as.vector(catnn))], ncol = 5)  # 각 순위 카테고리의 득표 수
  first <- max.col(pos_cnt == top, ties.method = "first")                   # 최다 득표인 첫 순위
  catnn[cbind(seq_len(nrow(catnn)), first)]
}

# ---- 5. Leave-one-out 정확도: K = 1~100 각각 ----
# 카탈로그 사진 하나를 빼고 나머지 1,999장에서 Top 5를 찾아 투표로 예측. 2,000장 모두 반복.
cat_code <- as.integer(meta$category)                # 카테고리를 1~4 숫자로
loo_pred <- matrix(NA_integer_, n, KMAX)             # 사진 x K 예측 결과 (혼동행렬용으로 저장)
for (k in 1:KMAX) {
  Sk <- S[, 1:k, drop = FALSE]
  sq <- rowSums(Sk^2)
  D2 <- outer(sq, sq, "+") - 2 * tcrossprod(Sk)      # 모든 사진 쌍의 거리 제곱: |a|^2 + |b|^2 - 2 a·b
  diag(D2) <- Inf                                    # 자기 자신은 제외
  nn <- matrix(0L, n, 5)
  for (j in 1:5) {                                   # 가장 가까운 것부터 하나씩 5개 (전체 정렬보다 빠름)
    nn[, j] <- max.col(-D2, ties.method = "first")   # 행마다 최솟값 위치
    D2[cbind(1:n, nn[, j])] <- Inf                   # 찾은 것은 지우고 다음으로
  }
  loo_pred[, k] <- vote5(matrix(cat_code[nn], n, 5))
}
loo_acc <- colMeans(loo_pred == cat_code)            # K별 정확도

# ---- 6. 저장 ----
saveRDS(list(meta = meta, X = X, mu = mu, V = V, S = S, eig = eig, var_exp = var_exp,
             loo_pred = loo_pred, loo_acc = loo_acc),
        "pca_cache.rds", compress = "xz")
cat("Saved pca_cache.rds. Best LOO accuracy:", round(max(loo_acc), 3), "at K =", which.max(loo_acc), "\n")
