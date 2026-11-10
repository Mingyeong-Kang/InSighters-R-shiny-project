# =============================================================================
# precompute_pca.R — PCA 를 한 번 계산해 data/pca_cache.rds 로 저장 + 정확성 검증
# 실행 (프로젝트 루트에서):  Rscript scripts/precompute_pca.R
# 앱은 이 캐시를 읽기만 하므로 시작이 빠릅니다. shoes/ 가 바뀌면 다시 실행하세요.
# =============================================================================
for (f in c("config.R", "image_utils.R", "pca_engine.R")) source(file.path("R", f), encoding = "UTF-8")

t0 <- Sys.time()
cat_data <- load_catalogue_matrix()
cat(sprintf("Loaded %d photos × %d pixels\n", nrow(cat_data$X), ncol(cat_data$X)))
pca <- build_pca(cat_data$X, cat_data$meta)
cat(sprintf("PCA done in %.1fs\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

# ---- 검증 1: loadings 직교정규성, 점수 = Xc V -------------------------------
v <- verify_pca(pca)
cat(sprintf("max |VᵀV − I|      = %.2e\n", v$orthonormal_max_error))
cat(sprintf("max |XcV − scores| = %.2e\n", v$score_max_error))

# ---- 검증 2: prcomp 와 분산비율 비교 (처음 5개) -----------------------------
pr <- prcomp(cat_data$X, center = TRUE, scale. = FALSE, rank. = 5)
ve_pr <- pr$sdev^2 / sum(pr$sdev^2)
cat("variance ratio (ours)  :", round(pca$var_ratio[1:5], 5), "\n")
cat("variance ratio (prcomp):", round(ve_pr[1:5], 5), "\n")
cat(sprintf("|cor| of PC1 scores with prcomp = %.6f\n", abs(cor(pca$scores[, 1], pr$x[, 1]))))

# ---- 검증 3: 복원 — K 가 커질수록 오차 감소, 모든 PC 사용 시 원본과 거의 일치 -------
x <- cat_data$X[1, ]; z <- project_photo(pca, x)
for (K in c(1, 10, 50, ncol(pca$loadings))) {
  cat(sprintf("rebuild RMSE (photo 1, K=%3d) = %.5f\n", K, sqrt(mean((reconstruct(pca, z, K) - x)^2))))
}

# ---- 추가: LOO Top-5 정확도 ---------------------------------------------------
pca$loo_accuracy <- loo_accuracy(pca)
print(pca$loo_accuracy)

dir.create("data", showWarnings = FALSE)
saveRDS(pca, PATH_PCA_CACHE, compress = TRUE)
cat(sprintf("Saved %s (%.1f MB) in %.1fs total\n", PATH_PCA_CACHE,
            file.size(PATH_PCA_CACHE) / 1e6, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
