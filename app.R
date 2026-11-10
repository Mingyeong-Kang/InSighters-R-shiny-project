# Project 2 template: the tabs and the inputs only. Everything else, and the look of the app, is yours.
library(shiny)
library(ggplot2)

# Example: read one photo as 4,096 numbers (64 x 64 grayscale, row by row, 0 = black to 255 = white)
# library(magick)
# img <- image_read("shoes/0001.png")
# x <- as.numeric(t(matrix(as.integer(image_data(img, channels = "gray")), 64, 64)))

ui <- navbarPage("InSighters",
  tabPanel("Shoe Search",
    radioButtons("src", "Photo", c("Catalogue", "Self-check", "Upload"), inline = TRUE),
    numericInput("cat_id", "Catalogue id", 1, 1, 2000),
    selectInput("sc_file", "Self-check photo", sprintf("selfcheck_%02d.png", 1:4)),
    fileInput("photo", "Upload", accept = c(".jpg", ".jpeg", ".png")),
    sliderInput("K", "Number of PCs (K)", 1, 100, 20),
    textOutput("result")   # TODO: best match, top 5, prediction, right or wrong
  ),
  tabPanel("Explain PCA",
    p("For the photo chosen in Shoe Search, show how PCA finds the match."),
    h4("Step 1. Average shoe"), p("x̅, the mean of the 2,000 photos."),
    h4("Step 2. Photo − average"), p("x − x̅, where the photo is brighter or darker than the average shoe."),
    h4("Step 3. Score map"), p("All catalogue photos on PC1 and PC2, colored by category, with your photo marked."),
    h4("Step 4. Eigenshoes"), p("Show PC1, PC2, … as pictures (their loadings drawn as 64 × 64 images), so we can see what shape each PC captures, and how much variance each explains."),
    h4("Step 5. Scores"), p("The photo’s first K PC scores, the numbers used to find the match."),
    h4("Step 6. Rebuild"), p("The photo rebuilt from the average shoe and the first K eigenshoes, for several K."),
    h4("More"), p("Anything else you want to show or explain related to it.")
  ),
  tabPanel("Our Shoes",
    p("Can your app recognize real shoes it has never seen?"),
    p("Each member photographs 3 different shoes of their own (shoe only, plain background), e.g., YourName_Sandals_1.jpg."),
    p("Show all of them with name and category (3 × n photos for n members)."),
    p("Choose a shoe with a widget; each choice shows its best match, its top 5 and whether it is right.")
  ),
  tabPanel("Data 1",
           p("One Project 1 dataset, descriptive results only (filter, sort, count, mean, median, rate, group summary)."),
           p("It should include:"),
           tags$ul(
             tags$li("the question you are trying to answer;"),
             tags$li("the inputs that show it (e.g., the filter, grouping or time range you choose);"),
             tags$li("the result in numbers;"),
             tags$li("why it matters;"),
             tags$li("how this finding helps your next analysis of this dataset.")),
           
           hr(),
           h4("Question you are trying to answer"),
           
           hr(),
           h4("Inputs"),
           sliderInput("d1_years", "Year range", min = 2015, max = 2026,
                       value = c(2015, 2026), step = 1, sep = ""),
           
           hr(),
               h4("1. 개업률 비교"),
    selectInput("d1_open_dong", "동네 선택",
                choices = c("전체", "청파동", "동선동", "대현동"),
                selected = "전체", selectize = FALSE),
    plotOutput("d1_open_plot", height = "320px"),

    hr(),
    h4("2. 폐업률 비교"),
    selectInput("d1_close_dong", "동네 선택",
                choices = c("전체", "청파동", "동선동", "대현동"),
                selected = "전체", selectize = FALSE),
    plotOutput("d1_close_plot", height = "320px"),

    hr(),
    h4("3. 동네별 평균 비교"),
    plotOutput("d1_avg_plot", height = "320px"),
           hr(),
           h4("Why it matters"),
           hr(),
           h4("How this finding helps your next analysis"),
  ),
  tabPanel("AI",
    h4("Ethics statement"),
    p("We understand the rules on data and AI use for this project, including following the license and privacy rules",
      "of every dataset we used, understanding all of our code, and knowing that presenting AI output as our own work may be",
      "academic misconduct. We certify that we have followed all of them. Any violation is our responsibility,",
      "and we agree to receive an F in this course if we have not."),
    p("Signed: TODO (every member)"),
    h4("What AI did and what we did"),
    tableOutput("ai_table"),
    h4("Reflection"),
    tableOutput("ai_reflection")
  )
)

server <- function(input, output, session) {
  output$result <- renderText("TODO")

  # AI tab: fill in the two tables. Keep the columns.
  output$ai_table <- renderTable(data.frame(
    Tab           = c("Shoe Search", "Explain PCA", "Our Shoes", "Data 1"),
    `What AI did` = c("TODO", "TODO", "TODO", "TODO"),
    `What we did` = c("TODO", "TODO", "TODO", "TODO"),
    check.names = FALSE))
  output$ai_reflection <- renderTable(data.frame(
    Member     = c("TODO", "TODO", "TODO", "TODO", "TODO"),
    Reflection = c("TODO: one or two paragraphs: what AI helped with, where it failed, what you learned.",
                   "TODO", "TODO", "TODO", "TODO")))
  # ---- Data 1 ----
  dongs <- c("청파동", "동선동", "대현동")
  
  # "전체"면 세 동네 모두, 아니면 선택한 동네만
  pick_dong <- function(sel) if (sel == "전체") dongs else sel
  
  d1_raw <- read.csv("data1.csv", fileEncoding = "UTF-8-BOM", check.names = FALSE)
  
  # wide -> long (연도, 동네, 개업률, 폐업률)
  d1_long <- do.call(rbind, lapply(dongs, function(d) {
    data.frame(
      연도   = d1_raw[["연도"]],
      동네   = d,
      개업률 = d1_raw[[paste0("개업률_", d)]],
      폐업률 = d1_raw[[paste0("폐업률_", d)]]
    )
  }))
  d1_long$동네 <- factor(d1_long$동네, levels = dongs)
  
  # 연도 범위 필터
  d1_filtered <- reactive({
    subset(d1_long, 연도 >= input$d1_years[1] & 연도 <= input$d1_years[2])
  })
  
  # 동네별 평균 (개업률 vs 폐업률)  <- 이게 빠져 있던 부분
  d1_avg <- reactive({
    df <- d1_filtered()
    do.call(rbind, lapply(dongs, function(d) {
      s <- subset(df, 동네 == d)
      data.frame(동네 = d,
                 구분 = c("개업률", "폐업률"),
                 평균 = c(mean(s$개업률), mean(s$폐업률)))
    }))
  })
  
  rate_plot <- function(df, y, title) {
    ggplot(df, aes(x = 연도, y = .data[[y]], color = 동네)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = unique(df$연도)) +
      scale_y_continuous(labels = function(v) paste0(round(v * 100, 1), "%")) +
      labs(title = title, x = "연도", y = y) +
      theme_minimal(base_size = 14)
  }
  
  output$d1_open_plot <- renderPlot({
    df <- subset(d1_filtered(), 동네 %in% pick_dong(input$d1_open_dong))
    rate_plot(df, "개업률", "동네별 개업률")
  })
  
  output$d1_close_plot <- renderPlot({
    df <- subset(d1_filtered(), 동네 %in% pick_dong(input$d1_close_dong))
    rate_plot(df, "폐업률", "동네별 폐업률")
  })
  
  output$d1_avg_plot <- renderPlot({
    df <- d1_avg()
    df$동네 <- factor(df$동네, levels = dongs)
    ggplot(df, aes(x = 동네, y = 평균, fill = 구분)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = sprintf("%.2f%%", 평균 * 100)),
                position = position_dodge(width = 0.9), vjust = -0.4) +
      scale_y_continuous(labels = function(v) paste0(round(v * 100, 1), "%"),
                         expand = expansion(mult = c(0, 0.12))) +
      labs(title = "동네별 평균 개업률 vs 폐업률", x = NULL, y = "평균") +
      theme_minimal(base_size = 14)
  })
  
  # 수치 요약표 (평균 / 중앙값)
  output$d1_table <- renderTable({
    df <- d1_filtered()
    do.call(rbind, lapply(dongs, function(d) {
      s <- subset(df, 동네 == d)
      data.frame(
        동네            = d,
        `평균 개업률`   = sprintf("%.2f%%", mean(s$개업률) * 100),
        `중앙값 개업률` = sprintf("%.2f%%", median(s$개업률) * 100),
        `평균 폐업률`   = sprintf("%.2f%%", mean(s$폐업률) * 100),
        `중앙값 폐업률` = sprintf("%.2f%%", median(s$폐업률) * 100),
        check.names = FALSE
      )
    }))
  })
}

shinyApp(ui, server)
