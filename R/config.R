# =============================================================================
# config.R — 팀 설정, 경로, 디자인 토큰 (팀원이 가장 먼저 수정할 파일)
# =============================================================================

# ---- 팀 정보 ---------------------------------------------------------------
TEAM_NAME <- "Team Name"            # TODO: 팀명으로 변경 (앱 제목에 표시)

# 팀원 목록. signed 는 각 팀원이 윤리 서약문을 직접 읽고 동의한 뒤 TRUE 로 바꿉니다.
TEAM_MEMBERS <- data.frame(
  name   = c("Member 1", "Member 2", "Member 3", "Member 4", "Member 5"),  # TODO: 실명
  signed = c(FALSE, FALSE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

# ---- 경로 (모두 app.R 기준 상대경로) ---------------------------------------
PATH_SHOES      <- "shoes"                 # 0001.png ~ 2000.png
PATH_META       <- "shoes_meta.csv"        # id, category
PATH_SELFCHECK  <- "selfcheck"             # selfcheck_01.png ~ + labels.csv
PATH_OUR_SHOES  <- "our_shoes"             # Name_Category_n.jpg
PATH_PCA_CACHE  <- file.path("data", "pca_cache.rds")
PATH_DATA1_DIR  <- "data1"

# ---- PCA 설정 ---------------------------------------------------------------
IMG_SIZE    <- 64
N_PC_KEEP   <- 200      # 캐시에 저장할 주성분 수 (슬라이더 최대값 100 < 200)
K_DEFAULT   <- 20
K_MAX       <- 100
TOP_N       <- 5
CATEGORIES  <- c("Boots", "Sandals", "Shoes", "Slippers")

# ---- 디자인 토큰 -------------------------------------------------------------
COL <- list(
  primary   = "#48866F",
  bg        = "#F7F9F8",
  surface   = "#FFFFFF",
  text      = "#1F2933",
  text2     = "#667085",
  border    = "#E3E8E5",
  success   = "#2E7D57",
  danger    = "#C2410C"
)

# 대학 비교 전용 고정 컬러 (Data 1 에서만 사용)
UNIV_COLORS <- c(
  "Ewha"       = "#48866F",
  "Sookmyung"  = "#5987F5",
  "Sungshin"   = "#C58AB8"
)
UNIV_LABELS <- c(
  "Ewha"      = "Ewha Womans Univ. (이화여대)",
  "Sookmyung" = "Sookmyung Women's Univ. (숙명여대)",
  "Sungshin"  = "Sungshin Women's Univ. (성신여대)"
)

# 신발 카테고리 색 (색각 이상자도 구분 가능한 Okabe–Ito 계열 + 라벨 병기)
CAT_COLORS <- c(
  "Boots"    = "#1F4E79",
  "Sandals"  = "#E69F00",
  "Shoes"    = "#009E73",
  "Slippers" = "#CC79A7"
)
