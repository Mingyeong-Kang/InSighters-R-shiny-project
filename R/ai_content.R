# =============================================================================
# ai_content.R — AI 탭 내용 (팀원이 직접 수정)
# 실제로 사용한 도구와 실제로 한 일만 적으세요. 아래 "What AI did" 는 이 코드 초안이
# Claude(Anthropic)로 생성되었다는 사실만 적어 두었습니다. 팀이 다른 AI 를 썼다면 추가하세요.
# =============================================================================

# 공식 과제 안내문의 문구 그대로 (수정 금지)
AI_ETHICS_STATEMENT <- paste(
  "We understand the rules on data and AI use for this project, including following the license and privacy rules",
  "of every dataset we used, understanding all of our code, and knowing that presenting AI output as our own work may be",
  "academic misconduct. We certify that we have followed all of them. Any violation is our responsibility,",
  "and we agree to receive an F in this course if we have not.")

# 템플릿 형식 유지: Tab / What AI did / What we did
AI_TABLE <- data.frame(
  Tab           = c("Shoe Search", "Explain PCA", "Our Shoes", "Data 1"),
  `What AI did` = c(
    "Claude (Anthropic) drafted the R code: image preprocessing, PCA via eigen-decomposition, projection, top-5 search and the result UI.",
    "Claude drafted the six-step visualisation code (mean image, difference map, score map, eigenshoes, scores, rebuild) and the LOO-accuracy plot.",
    "Claude drafted the gallery / filter / result code and the file-name parser.",
    "Claude drafted the data-loading structure, column mapping, empty state, demo mode and dashboard layout (no real data was provided to it)."),
  `What we did` = c("TODO", "TODO", "TODO", "TODO: our dataset, questions, interpretation"),
  check.names = FALSE, stringsAsFactors = FALSE)

# 팀원별 Reflection (한두 문단). Member 이름은 config.R 의 TEAM_MEMBERS 와 맞추세요.
AI_REFLECTION <- data.frame(
  Member     = TEAM_MEMBERS$name,
  Reflection = c("TODO: one or two paragraphs: what AI helped with, where it failed, what you learned.",
                 rep("TODO", nrow(TEAM_MEMBERS) - 1)),
  stringsAsFactors = FALSE)
