# Project 2 — Shoe PCA Search (R Shiny)

Statistical Graphics with Big Data (39209), Fall 2026 · 5 tabs: **Shoe Search · Explain PCA · Our Shoes · Data 1 · AI**

## 1. 폴더 구조

```
P2_students 2/                 ← 교수님 zip 을 푼 폴더 = 프로젝트 루트
├── app.R                      # 진입점: R/ 파일 source → UI/서버 조립
├── R/
│   ├── _disable_autoload.R    # Shiny 자동 로드 끄기 (app.R 이 순서대로 source)
│   ├── config.R               # ★ 팀명, 팀원, 서명, 경로, 색상 토큰
│   ├── image_utils.R          # 이미지 읽기·전처리(64×64 흑백)·화면표시
│   ├── pca_engine.R           # PCA 계산·투영·검색·복원·LOO 정확도
│   ├── ui_helpers.R           # 카드, 빈 상태, 결과 패널 등 공용 UI
│   ├── data1_loader.R         # Data 1 로딩/검증/표준화 (UI 와 분리)
│   ├── ai_content.R           # ★ AI 탭 표 내용 (팀이 직접 작성)
│   ├── mod_shoe_search.R      # Tab 1
│   ├── mod_explain_pca.R      # Tab 2
│   ├── mod_our_shoes.R        # Tab 3
│   ├── mod_data1.R            # Tab 4
│   └── mod_ai.R               # Tab 5
├── www/styles.css, app.js     # 디자인 시스템, 로딩바, 이미지 확대, 스텝 내비
├── scripts/precompute_pca.R   # PCA 사전계산 + 정확성 검증 → data/pca_cache.rds
├── data/pca_cache.rds         # (생성됨) 캐시
├── data1/data1_config.R       # ★ Data 1 컬럼 매핑 / 분석 3개
├── data1/data1.csv            # (팀이 추가) Project 1 최종 데이터
├── our_shoes/                 # (팀이 추가) 이름_카테고리_번호.jpg
├── shoes/ shoes_meta.csv selfcheck/   # 수업 제공 데이터 (git 에 올리지 않음)
└── .gitignore
```

## 2. 설치 · 실행

```r
install.packages(c("shiny", "bslib", "png", "jpeg", "base64enc", "plotly", "DT"))
setwd("~/Downloads/P2_students 2")          # 이 폴더를 작업 디렉터리로
source("scripts/precompute_pca.R")          # 최초 1회 (~1–2분). 검증 결과가 콘솔에 출력됨
shiny::runApp()
```
캐시가 없어도 앱은 시작 시 1회 PCA 를 계산하고 캐시를 저장합니다(첫 실행만 느림).

## 3. PCA 핵심 (팀 회의 설명용)

| 단계 | 수식 | 코드 |
|---|---|---|
| 벡터화 | 64×64 흑백 → 행 단위 4,096 벡터, 값 0–1 | `read_gray`, `mat_to_vec` |
| 평균 | x̄ = colMeans(X) | `build_pca` |
| 중심화 | Xc = X − x̄ (스케일링 없음: 모든 픽셀이 같은 단위) | `build_pca` |
| 분해 | XcXcᵀ = U Λ Uᵀ (2000×2000, 4096×4096 보다 빠름), d = √λ, V = XcᵀU D⁻¹ | `build_pca` |
| 점수 | scores = U D = Xc V | |
| 설명분산 | d²ⱼ / Σd² | `var_ratio` |
| 새 사진 | z = Vₖᵀ(x − x̄) — **학습 평균·loadings 재사용** | `project_photo` |
| 검색 | PC 공간 유클리드 거리, 카탈로그 쿼리는 자기 id 제외 | `search_similar` |
| 분류 | Top 5 최빈 카테고리 (동률이면 더 가까운 쪽) | `vote_category` |
| 복원 | x̂ₖ = x̄ + Vₖ z | `reconstruct` |

검증(`scripts/precompute_pca.R` 출력): VᵀV = I 오차, Xc V = scores 오차, `prcomp` 와 분산비율·PC1 비교, K 증가 시 복원 RMSE 감소.
**효율성:** PCA 는 한 번만 계산해 `.rds` 로 캐시. 검색 1회 = 2000×K 행렬 거리 계산(수 ms).

새 사진 전처리(`preprocess_photo`): 흑백 변환 → 테두리 중앙값으로 배경 추정(어두우면 반전) → 배경을 흰색(1)으로 보정 → 신발 영역 자르기 → 흰 여백으로 정사각형 → 64×64 면적평균 축소. 카탈로그·selfcheck 는 이미 64×64 이므로 그대로 사용.

## 4. Our Shoes 사진 추가
`our_shoes/` 에 `이름_카테고리_번호.jpg` (예: `Whae_Sandals_1.jpg`). 카테고리는 Boots/Sandals/Shoes/Slippers (단수형도 인식). 규칙에 안 맞는 파일은 화면에 경고로 표시.

## 5. Data 1 연결 (최종 데이터가 오면)
1. CSV 를 `data1/data1.csv` 로 저장 (UTF-8).
2. `data1/data1_config.R` 수정:
   - `DATA1_COLUMNS$university` ← 대학 컬럼명 (필수)
   - `period`, `industry` ← 있으면 컬럼명, 없으면 `NA` (해당 필터/그래프는 "미지원" 표시)
   - `metrics` ← 숫자 지표 컬럼명 벡터 (필수)
   - `DATA1_UNIV_VALUES` ← CSV 안의 실제 대학 값 (예: "이화여대")
   - `DATA1_ANALYSES` ← 분석 3개의 질문·지표·통계·해석·시사점 (수치는 앱이 자동 계산)
3. 앱 재실행. 파일이 없거나 매핑이 틀리면 **Dataset Not Connected** 와 원인이 표시됩니다.
Demo mode 는 가상 데이터이며 화면·CSV 모두 `DEMO DATA — NOT ACTUAL RESULTS` 로 표시됩니다.

## 6. 팀이 반드시 수정할 것
- `R/config.R`: `TEAM_NAME`, `TEAM_MEMBERS$name`, 각자 서약 후 `signed = TRUE`
- `R/ai_content.R`: "What we did", Reflection (실제 사실만)
- `our_shoes/` 사진, `data1/` 데이터

## 7. 배포 (shinyapps.io)
```r
rsconnect::deployApp(appFiles = c("app.R", "R", "www", "data", "data1", "our_shoes",
                                  "shoes", "shoes_meta.csv", "selfcheck"))
```
- 앱이 데이터를 읽어야 하므로 배포 번들에는 포함되지만, **GitHub 저장소와 제출 zip 에는 신발 데이터를 넣지 않습니다** (`.gitignore`). 신발 사진은 수업 외 사용/공유 금지.
- `data/pca_cache.rds` 를 함께 올리면 서버에서 PCA 를 다시 계산하지 않아 시작이 빠릅니다.
- 마감(11/16 23:59) 이후 재배포 금지 — 배포 시각을 기록해 두세요.

## 8. 구현 / 미구현 · 검증 필요
**구현:** 5개 탭 전체, 3가지 입력(id·selfcheck·업로드 drag&drop+미리보기), K 슬라이더 공유, Best match·평균·K 복원·Top 5 갤러리·거리·예측/정답, Top5 CSV, 6단계+추가(LOO 정확도) 설명, 카테고리 필터 산점도, eigenshoe 갤러리, 이미지 확대, 로딩바, 빈/오류 상태, Data 1 미연결/데모 구조, AI 탭 템플릿 형식.
**검증 필요:** 실제 2,000장으로 `precompute_pca.R` 실행 결과 확인, selfcheck 4장 정답 여부, 실제 팀원 사진(조명·배경에 따라 자동 자르기 품질 확인), Data 1 실데이터 연결, 모바일 화면 확인, shinyapps.io 메모리(캐시 ~15MB, 충분).
