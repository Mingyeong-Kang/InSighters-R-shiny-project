# =============================================================================
# data1_config.R — Data 1 (Project 1 대학 상권 데이터) 연결 설정
# 최종 데이터가 오면 이 파일만 수정하면 됩니다. 자세한 방법은 README 의 "Data 1 연결" 참고.
# =============================================================================

# 1) 데이터 파일 경로 (app.R 기준 상대경로). CSV (UTF-8) 권장.
DATA1_FILE <- file.path("data1", "data1.csv")

# 2) 컬럼 매핑: 오른쪽 문자열을 실제 CSV 의 컬럼 이름으로 바꾸세요.
#    해당 컬럼이 없다면 NA 로 두면 그 기능(필터/그래프)은 "미지원"으로 표시됩니다.
DATA1_COLUMNS <- list(
  university = NA,   # 필수. 예: "university"  (대학/상권 구분)
  period     = NA,   # 선택. 예: "year" 또는 "quarter" (숫자 또는 정렬 가능한 문자)
  industry   = NA,   # 선택. 예: "industry" (업종)
  metrics    = c()   # 필수(1개 이상). 숫자형 지표 컬럼. 예: c("store_count", "sales")
)

# 3) 지표 표시 이름(선택): c(store_count = "점포 수 (개)")
DATA1_METRIC_LABELS <- c()

# 4) university 컬럼의 실제 값 -> 앱 내부 키 (색상 고정에 사용)
DATA1_UNIV_VALUES <- c(
  Ewha      = "이화여대",
  Sookmyung = "숙명여대",
  Sungshin  = "성신여대"
)

# 5) 분석 결과 3개. 질문/해석/시사점은 실제 데이터를 본 뒤 팀이 작성합니다.
#    metric 은 DATA1_COLUMNS$metrics 중 하나, stat 은 "mean","median","sum","count" 중 하나.
#    수치 결과는 앱이 현재 필터 기준으로 자동 계산합니다.
DATA1_ANALYSES <- list(
  list(title = "Analysis 1", question = "TODO: 연구 질문 1",
       metric = NA, stat = "mean",
       interpretation = "TODO: 결과 해석 (실제 수치를 확인한 뒤 작성)",
       next_step = "TODO: 다음 분석에 대한 시사점"),
  list(title = "Analysis 2", question = "TODO: 연구 질문 2",
       metric = NA, stat = "median",
       interpretation = "TODO", next_step = "TODO"),
  list(title = "Analysis 3", question = "TODO: 연구 질문 3",
       metric = NA, stat = "count",
       interpretation = "TODO", next_step = "TODO")
)
