# =============================================================================
# data1_config.R — Data 1 (여대 상권 비교: 음식점 개·폐업) 연결 설정
# 이화여대(대현동) · 숙명여대(청파동) · 성신여대(동선동)
# 원본 인허가 CSV 를 앱이 직접 읽어 연도별 개업·폐업 통계를 계산합니다.
# 파일 위치: data1/data1_config.R   (이 파일은 UTF-8 로 저장하세요)
# =============================================================================

# 1) 원본 CSV 경로 (app.R 기준 상대경로).
#    '청파동_동선동_대현동_음식점_합본.csv' 를 data1/data1.csv 로 이름을 바꿔 두세요.
#    (한글 파일명은 OS 설정에 따라 문제가 될 수 있어 영문 이름을 권장합니다.)
DATA1_FILE <- file.path("data1", "data1.csv")

# 2) 원본 CSV 의 컬럼 이름. industry 는 선택(없으면 NA → 업태 필터 미지원).
DATA1_COLUMNS <- list(
  dong       = "행정동",
  open_date  = "인허가일자",   # 개업일로 사용 (YYYY-MM-DD, YYYYMMDD 등)
  close_date = "폐업일자",     # 영업 중이면 비어 있음 (YYYY-MM-DD, YYYYMMDD 등)
  industry   = "업태구분명"
)

# 3) 분석 대상 동네(표시 순서 = 이 순서), 화면 표시 이름(대학), 고정 색상
#    화면에는 "이화여대 (대현동)" 처럼 대학 + 동네로 표시됩니다.
DATA1_DONGS <- c("대현동", "청파동", "동선동")
DATA1_DONG_LABELS <- c("대현동" = "이화여대 (대현동)",
                       "청파동" = "숙명여대 (청파동)",
                       "동선동" = "성신여대 (동선동)")
DATA1_DONG_COLORS <- c("대현동" = "#48866F", "청파동" = "#5987F5", "동선동" = "#C58AB8")
# 4) 개업률 / 폐업률 막대 색
DATA1_RATE_COLORS <- c(open = "#2A9D8F", close = "#E76F51")

# 5) 분석 시작 연도 (끝 연도는 데이터의 마지막 연도로 자동 결정)
DATA1_YEAR_FROM <- 2015

# 6) 지표 표시 이름
#    개업수/폐업수 = 해당 연도 인허가일자/폐업일자 건수
#    순증감 = 개업수 − 폐업수
#    개업률 = 개업수 ÷ (연초 영업 중인 업소 수 + 그해 개업수)
#    폐업률 = 폐업수 ÷ (연초 영업 중인 업소 수 + 그해 개업수)
DATA1_METRIC_LABELS <- c(
  open_n     = "개업 수 (개)",
  close_n    = "폐업 수 (개)",
  net_n      = "순증감 (개)",
  open_rate  = "개업률",
  close_rate = "폐업률"
)

# 7) 분석 결과 3개. 수치는 앱이 현재 필터 기준으로 자동 계산합니다.
#    metric : DATA1_METRIC_LABELS 의 이름 중 하나
#    stat   : "mean", "median", "sum", "count" 중 하나 (동네별로 연도 값들을 요약)
#    질문/해석/시사점은 실제 수치를 본 뒤 팀이 직접 작성하세요.
DATA1_ANALYSES <- list(
  list(title = "Analysis 1", question = "대학별 평균 개업률은 어떻게 다른가?",
       metric = "open_rate", stat = "mean",
       interpretation = "TODO",
       next_step = "TODO"),
  list(title = "Analysis 2", question = "대학별 평균 폐업률은 어떻게 다른가?",
       metric = "close_rate", stat = "mean",
       interpretation = "TODO", next_step = "TODO"),
  list(title = "Analysis 3", question = "선택한 기간에 어느 대학 상권의 업소 수가 가장 많이 늘고 줄었나?",
       metric = "net_n", stat = "sum",
       interpretation = "TODO", next_step = "TODO")
)