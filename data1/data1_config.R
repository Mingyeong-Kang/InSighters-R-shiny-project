# =============================================================================
# data1_config.R — Data 1 (여대 상권 비교: 음식점 개·폐업) 연결 설정
# 이화여대(대현동) · 숙명여대(청파동) · 성신여대(동선동)
# 원본 인허가 CSV 를 앱이 직접 읽어 연도별 개업·폐업 통계를 계산합니다.
# 파일 위치: data1/data1_config.R   (이 파일은 UTF-8 로 저장하세요)
# =============================================================================

# 1) 원본 CSV 경로 (app.R 기준 상대경로). UTF-8 (BOM 포함) 로 저장된 파일이어야 합니다.
DATA1_FILE <- file.path("data1", "data1.csv")

# 2) 원본 CSV 의 컬럼 이름 (네 컬럼 모두 필수).
DATA1_COLUMNS <- list(
  dong       = "행정동",
  open_date  = "인허가일자",   # 개업일로 사용 (YYYY-MM-DD)
  close_date = "폐업일자",     # 영업 중이면 비어 있음 (YYYY-MM-DD)
  industry   = "업태구분명"
)

# 3) 분석 대상 동네(표시 순서 = 이 순서), 화면 표시 이름(대학), 고정 색상
#    DATA1_DONGS 의 값은 CSV 의 행정동 값과 같아야 하므로 한글 그대로 둡니다.
#    화면에는 "Ewha Womans Univ. (Daehyeon-dong)" 처럼 대학 + 동네로 표시됩니다.
DATA1_DONGS <- c("대현동", "청파동", "동선동")
DATA1_DONG_LABELS <- c("대현동" = "Ewha (Daehyeon-dong)",
                       "청파동" = "Sookmyung (Cheongpa-dong)",
                       "동선동" = "Sungshin (Dongseon-dong)")
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
  open_n     = "Openings (count)",
  close_n    = "Closures (count)",
  net_n      = "Net change (count)",
  open_rate  = "Opening rate",
  close_rate = "Closure rate"
)

# 7) 분석 결과 3개. 수치는 앱이 현재 필터 기준으로 자동 계산합니다.
#    metric : DATA1_METRIC_LABELS 의 이름 중 하나
#    stat   : "mean", "median", "sum" 중 하나 (동네별로 연도 값들을 요약)
#    질문/해석/시사점은 실제 수치를 본 뒤 팀이 직접 작성하세요. (화면에 영어로 표시됩니다)
DATA1_ANALYSES <- list(
  list(title = "Analysis 1",
       question = "How does the average opening rate differ across universities?",
       metric = "open_rate", stat = "mean",
       interpretation = "TODO",
       next_step = "TODO"),
  list(title = "Analysis 2",
       question = "How does the average closure rate differ across universities?",
       metric = "close_rate", stat = "mean",
       interpretation = "TODO", next_step = "TODO"),
  list(title = "Analysis 3",
       question = "Which university's commercial district gained or lost the most establishments over the selected period?",
       metric = "net_n", stat = "sum",
       interpretation = "TODO", next_step = "TODO")
)