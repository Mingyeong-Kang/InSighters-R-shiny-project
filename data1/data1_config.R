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

# 7) 분석 결과 1개. 수치는 앱이 현재 필터 기준으로 자동 계산합니다.
#    metric : DATA1_METRIC_LABELS 의 이름 중 하나
#    stat   : "mean", "median", "sum" 중 하나 (동네별로 연도 값들을 요약)
#
#    질문 : 2008–2026년 일반음식점·휴게음식점 기준, 이대(대현동) 상권이
#           숙대(청파동)·성신여대(동선동) 상권보다 더 빠르게 줄었는가?
#    지표 : 개업률·폐업률 (당해 1월 1일 점포 수 대비) → 세 상권 비교용
#           개업 수 − 폐업 수 (개수) → 증가·감소 방향(+/−)과 실제 규모 확인용
#           (개수는 상권 크기의 영향을 받아 상권 간 비교에는 쓰지 않음)
#    중요성 : 상권 쇠퇴는 개업률 하락 또는 폐업률 상승으로 일어남.
#             소비자 구성(여대생 중심)이 비슷한데 재학생 수가 가장 많은 이대만 더 빨리 줄었다면
#             학생 수가 아닌 임대료·유동인구 변화·정부 정책 등의 요인일 가능성
#    다음 분석 : (1) 개업 − 폐업이 마이너스로 돌아선 연도 / 개폐업률 차이가 가장 큰 구간
#                  → 로드뷰 이미지 분석 기준 연도 (미신고 공실 교차 검증)
#                (2) 주요 연도·상권 간 차이 → 인터랙티브 앱 필터 기본값·강조 구간
#    (화면에 표시되는 문구는 아래 question / interpretation / next_step 에 영어로 작성)
DATA1_ANALYSES <- list(
  list(
    title = "Did the Ewha district decline faster?",
    question = "Among the commercial districts around three women's universities whose main consumers are female students — Ewha (대현동), Sookmyung (청파동), and Sungshin (동선동) — did the Ewha district decline faster than the other two, based on general and rest restaurants (일반음식점·휴게음식점) from 2008 to 2026? (Metrics: opening rate and closure rate relative to the number of stores as of January 1 of each year, and openings − closures. Since counts are affected by district size, the three districts are compared using opening and closure rates, while openings − closures is used to check the direction and actual scale of change.)",
    metric = NA, stat = "mean",
    interpretation = "A commercial district declines either through a falling opening rate or a rising closure rate. Looking at both rates together allows us to compare the path and speed of decline across three districts of different sizes on the same basis, while openings − closures shows whether the number of stores is actually shrinking. Since the three districts have a similar consumer base centered on female university students, if only Ewha — which has the largest student population — declined faster, the difference is likely driven not by student numbers but by factors such as rent, changes in foot traffic, or government policy.",
    next_step = "(1) The year when Ewha's openings − closures turned negative, and the period with the largest gap in opening and closure rates compared with the other two districts, can be used as reference years for the road-view image analysis, cross-validating vacancies not captured in the license data. (2) The key years and district differences revealed by this result will be used as default year/district filter values and highlighted periods in the interactive app."
  )
)
