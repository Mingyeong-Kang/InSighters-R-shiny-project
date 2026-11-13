# =============================================================================
# app.R — Project 2 (Statistical Graphics with Big Data, Fall 2026)
# 실행: RStudio 에서 이 폴더를 작업 디렉터리로 두고  shiny::runApp()
# 구조: R/ 폴더의 파일을 순서대로 불러오고, 각 탭은 모듈(R/mod_*.R)로 분리
# UI 는 R 코드(shiny + bslib)로만 구성한다. 별도 CSS/JS 파일은 사용하지 않는다.
# =============================================================================
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
})

for (f in c("config.R", "image_utils.R", "pca_engine.R", "ui_helpers.R", "data1_loader.R",
            "ai_content.R", "mod_shoe_search.R", "mod_explain_pca.R", "mod_our_shoes.R",
            "mod_data1.R", "mod_ai.R")) {
  source(file.path("R", f), local = TRUE, encoding = "UTF-8")
}

# ---- PCA: 캐시(data/pca_cache.rds)가 있으면 즉시 로드, 없으면 1회 계산 후 저장 ----
PCA <- tryCatch(load_or_build_pca(), error = function(e) e)
PCA_OK <- !inherits(PCA, "error")

# ---- 디자인: 색·폰트·모서리는 전부 bs_theme() 의 R 인자로 지정 ----------------
theme <- bs_theme(
  version = 5,
  primary = COL$primary, success = COL$success, danger = COL$danger,
  bg = COL$bg, fg = COL$text,
  base_font = font_collection("Pretendard", "Apple SD Gothic Neo", "Malgun Gothic",
                              "-apple-system", "Segoe UI", "sans-serif"),
  "border-color" = COL$border,
  "border-radius" = "0.75rem",
  "card-bg" = COL$surface,
  "navbar-bg" = COL$surface
)

data_missing_ui <- function() {
  empty_state("신발 데이터를 불러오지 못했습니다",
    p(if (!PCA_OK) conditionMessage(PCA)),
    p("P2_for_students.zip 의 shoes/, shoes_meta.csv, selfcheck/ 를 app.R 과 같은 폴더에 두고 다시 실행하세요."),
    icon = "⚠️", tone = "error")
}

# 계산 중 표시: Shiny 1.8.1 이상이면 내장 busy indicator(R 함수) 사용
busy_ui <- if (exists("useBusyIndicators", asNamespace("shiny"))) shiny::useBusyIndicators() else NULL

ui <- page_navbar(
  title = paste("👟", TEAM_NAME),
  window_title = paste(TEAM_NAME, "· Shoe PCA"),
  id = "main_nav",
  theme = theme,
  fillable = FALSE,
  header = busy_ui,
  nav_panel("Shoe Search", if (PCA_OK) shoeSearchUI("search") else data_missing_ui()),
  nav_panel("Explain PCA", if (PCA_OK) explainPcaUI("explain") else data_missing_ui()),
  nav_panel("Our Shoes",   if (PCA_OK) ourShoesUI("ours") else data_missing_ui()),
  nav_panel("Data 1",      data1UI("data1")),
  nav_panel("AI",          aiUI("ai")),
  nav_spacer(),
  nav_item(span(class = "badge text-bg-light border",
                if (PCA_OK) sprintf("PCA · %d photos · %s", nrow(PCA$X),
                                    if (identical(attr(PCA, "source"), "cache")) "cached" else "computed")
                else "data missing"))
)

server <- function(input, output, session) {
  if (PCA_OK) {
    search <- shoeSearchServer("search", PCA)
    explainPcaServer("explain", PCA, search)
    ourShoesServer("ours", PCA, search)
  }
  data1Server("data1")
  aiServer("ai")
}

shinyApp(ui, server)
