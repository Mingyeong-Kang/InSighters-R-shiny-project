# =============================================================================
# app.R — Project 2 (Statistical Graphics with Big Data, Fall 2026)
# 실행: RStudio 에서 이 폴더를 작업 디렉터리로 두고  shiny::runApp()
# 구조: R/ 폴더의 파일을 순서대로 불러오고, 각 탭은 모듈(R/mod_*.R)로 분리
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

theme <- bs_theme(
  version = 5, primary = COL$primary, bg = COL$surface, fg = COL$text,
  base_font = font_collection("Pretendard", "Inter", "-apple-system", "Segoe UI", "sans-serif"),
  "border-color" = COL$border, "border-radius" = "0.75rem"
)

data_missing_ui <- function() {
  div(class = "container py-5",
      empty_state("신발 데이터를 불러오지 못했습니다",
        p(if (!PCA_OK) conditionMessage(PCA)),
        p(HTML("P2_for_students.zip 의 <code>shoes/</code>, <code>shoes_meta.csv</code>, <code>selfcheck/</code> 를 app.R 과 같은 폴더에 두고 다시 실행하세요.")),
        icon = "⚠️", tone = "error"))
}

ui <- page_navbar(
  title = span(class = "brand", span(class = "brand-dot"), TEAM_NAME),
  window_title = paste(TEAM_NAME, "· Shoe PCA"),
  id = "main_nav",
  theme = theme,
  fillable = FALSE,
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$script(src = "app.js"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")),
    div(id = "busy-bar", `aria-hidden` = "true"),
    div(id = "zoom-modal", role = "dialog", `aria-modal` = "true", `aria-label` = "Enlarged image",
        tags$img(id = "zoom-img", alt = "enlarged"), span(class = "zoom-close", "×"))),
  nav_panel("Shoe Search", div(class = "page", if (PCA_OK) shoeSearchUI("search") else data_missing_ui())),
  nav_panel("Explain PCA", div(class = "page", if (PCA_OK) explainPcaUI("explain") else data_missing_ui())),
  nav_panel("Our Shoes",   div(class = "page", if (PCA_OK) ourShoesUI("ours") else data_missing_ui())),
  nav_panel("Data 1",      div(class = "page", data1UI("data1"))),
  nav_panel("AI",          div(class = "page", aiUI("ai"))),
  nav_spacer(),
  nav_item(span(class = "nav-meta", if (PCA_OK) sprintf("PCA · %d photos · %s", nrow(PCA$X),
                                                         if (identical(attr(PCA, "source"), "cache")) "cached" else "computed") else "data missing"))
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
