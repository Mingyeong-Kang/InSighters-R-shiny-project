# =============================================================================
# mod_ai.R — Tab 5. AI (템플릿 형식 유지: 윤리 서약 → 서명 → ai_table → ai_reflection)
# =============================================================================

aiUI <- function(id) {
  ns <- NS(id)
  tagList(
    page_header("Tab 5 · Responsible AI use", "AI",
                "AI 사용 원칙에 대한 팀의 서약, 각 탭에서 AI 와 팀이 한 일, 팀원별 회고입니다."),
    ui_card("Ethics statement", icon = "📜",
      tags$blockquote(class = "ethics", AI_ETHICS_STATEMENT),
      h5("Signed"),
      div(class = "sign-row", lapply(seq_len(nrow(TEAM_MEMBERS)), function(i) {
        m <- TEAM_MEMBERS[i, ]
        div(class = paste("sign-chip", if (m$signed) "signed" else "pending"),
            span(class = "sign-mark", if (m$signed) "✓" else "…"),
            span(m$name), span(class = "sign-status", if (m$signed) "signed" else "pending"))
      })),
      p(class = "hint", "각 팀원이 서약문을 읽고 동의한 뒤 R/config.R 의 TEAM_MEMBERS$signed 를 TRUE 로 바꿉니다.")),
    ui_card("What AI did and what we did", icon = "🤝",
            div(class = "table-wrap", tableOutput(ns("ai_table")))),
    ui_card("Reflection", icon = "💭",
            div(class = "table-wrap", tableOutput(ns("ai_reflection"))))
  )
}

aiServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$ai_table <- renderTable(AI_TABLE, striped = TRUE, width = "100%")
    output$ai_reflection <- renderTable(AI_REFLECTION, striped = TRUE, width = "100%")
  })
}
