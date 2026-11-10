# Project 2 template: the tabs and the inputs only. Everything else, and the look of the app, is yours.

library(shiny)
#install.packages("magick")
library(magick)

# Example: read one photo as 4,096 numbers (64 x 64 grayscale, row by row, 0 = black to 255 = white)
# img <- image_read("shoes/0001.png")
# print(img)
# x <- as.numeric(t(matrix(as.integer(image_data(img, channels = "gray")), 64, 64)))

#####이미지 전체 불러오기####
X <- matrix(0, nrow = 2000, ncol = 4096)
for (i in 1:2000) {
  filename <- paste0("shoes/", sprintf("%04d", i), ".png") #paste0 = 공백없이 붙임, sprintf = 형식 정해서 문자열 만들기 
  img <- image_read(filename)
  x <- as.numeric(t(matrix(as.integer(image_data(img, channels = "gray")),64, 64)))
  
  X[i, ] <- x
}
dim(X)

selfcheck_X <- matrix(0, nrow = 4, ncol = 4096)

for (i in 1:4) {
  filename <- paste0("selfcheck/selfcheck_", sprintf("%02d", i), ".png")
  img <- image_read(filename)
  x <- as.numeric(t(matrix(as.integer(image_data(img, channels = "gray")),64, 64)))
  
  selfcheck_X[i, ] <- x
}

dim(selfcheck_X)

####신발 메타데이터 불러오기####
shoes_meta <- read.csv("shoes_meta.csv")
head(shoes_meta)

selfcheck_meta <- read.csv("selfcheck/labels.csv")
head(selfcheck_meta)

####PCA####
shoes_PCA <-prcomp(X)
addResourcePath("shoes", normalizePath("shoes"))

####UI####
ui <- navbarPage("InSighters",
  tabPanel("Shoe Search",
    radioButtons("src", "Photo", c("Catalogue", "Self-check", "Upload"), inline = TRUE),
    numericInput("cat_id", "Catalogue id", 1, 1, 2000),
    selectInput("sc_file", "Self-check photo", sprintf("selfcheck_%02d.png", 1:4)),
    fileInput("photo", "Upload", accept = c(".jpg", ".jpeg", ".png")),
    sliderInput("K", "Number of PCs (K)", 1, 100, 20),
  
    h4("Best match"),
    plotOutput("best_match", height = "220px"),
    
    h4("Average shoe"),
    plotOutput("average_shoe", height = "220px"),
    
    h4("Rebuilt photo with K PCs"),
    plotOutput("rebuilt_shoe", height = "220px"),
    
    h4("Top 5"),
    uiOutput("top5_photos"), #신발 사진 보여주기
    tableOutput("top5"), #id, 카테고리, 거리 table
    
    h4("Prediction"),
    textOutput("result")
  ),
  
  tabPanel("Explain PCA",
           p("For the photo chosen in Shoe Search, show how PCA finds the match."),
           
           h4("Step 1. Average shoe"),
           p("The mean of the 2,000 photos."),
           plotOutput("ex_average", height = "220px"),
           
           h4("Step 2. Photo - average"),
           p("The difference between the selected photo and the average shoe."),
           plotOutput("ex_difference", height = "220px"),
           
           h4("Step 3. Score map"),
           p("All catalogue photos on PC1 and PC2, colored by category, with your photo marked."),
           plotOutput("ex_scoremap", height = "400px"),
           
           h4("Step 4. Eigenshoes"),
           p("PC directions as images and the variance each explains."),
           plotOutput("ex_eigenshoes", height = "300px"),
           
           h4("Step 5. Scores"),
           p("The selected photo's first K PC scores."),
           plotOutput("ex_scores", height = "300px"),
           
           h4("Step 6. Rebuild"),
           p("The photo rebuilt using several values of K."),
           plotOutput("ex_rebuild", height = "300px")
  ),
#############################################################################  
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
      tags$li("how this finding helps your next analysis of this dataset."))
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
####server####
server <- function(input, output, session) {
  # Shoe search tab
  search <- reactive({ # 카탈로그, 셀프체크, 업로드 중에 하나 고르기 
    if (input$src == "Catalogue") {
      id <- input$cat_id
      x <- X[id, ]
      actual <- shoes_meta$category[id]
      
    } else if (input$src == "Self-check") {
      id <- match(input$sc_file, selfcheck_meta$photo)
      x <- selfcheck_X[id, ]
      actual <- selfcheck_meta$category[id]
      
    } else if (input$src == "Upload") {
      req(input$photo) #사진 업로드 안됐으면 계산 X
      img <- image_read(input$photo$datapath)
      img <- image_resize(img, "64x64!") #이미지 크기 64 * 64 픽셀로 바꾸기
      img <- image_convert(img, colorspace = "gray") #선택한 사진 흑백으로 바꾸기 
      x <- as.numeric(t(matrix(
        as.integer(image_data(img, channels = "gray")),
        64, 64
      )))
      actual <- NA
    }
    list(x = x, actual = actual) # reactive() 안에서 만든 x 와 정답 묶은 list 
  })
  
  pca_search <- reactive({
    x <- search()$x
    K <- input$K
    target <- as.numeric(
      (x - shoes_PCA$center) %*% # (위에서 선택한 사진 - 평균 사진)
        shoes_PCA$rotation[, 1:K, drop = FALSE])  # *주성분(PC 방향으로 내적)
    
    distance <- numeric(2000)
    
    for (i in 1:2000) {
      distance[i] <- sqrt(sum(
        (shoes_PCA$x[i, 1:K] - target)^2
      ))
    }
    list(target = target, distance = distance) 
    #pca_search에서 선택한 사진의 PC score(K개)와 distance(2000개) 값 저장
    
    if (input$src == "Catalogue") {
      distance[input$cat_id] <- Inf #자기 자신과의 거리를 무한대로 만들기
    }
    
    top5 <- order(distance)[1:5] #top 5 찾기 (2000개 정렬 후 1~5) 뽑기  
    list(target = target, distance = distance, top5 = top5)
  })
  # Top 5 신발 사진
  output$top5_photos <- renderUI({
    top5 <- pca_search()$top5
    fluidRow(
      lapply(top5, function(id) {  #lapply: 반복, fluidRow: 열 한줄에 배치(화면 너비에 맞게) 
        column(2,
               tags$img(         #top 5 이미지 넣기
                 src = paste0("shoes/", sprintf("%04d", id), ".png"),
                 width = "100%"
               )
        )
      })
    )
  })
  # Top 5 요약표
  output$top5 <- renderTable({
    top5 <- pca_search()$top5
    distance <- pca_search()$distance
    
    data.frame(
      Rank = 1:5, #가까운 순위
      ID = top5, #ID
      Category = shoes_meta$category[top5], #카테고리
      Distance = round(distance[top5], 2) #거리
    )
  })
   # Best Match 
  output$best_match <- renderPlot({
    best_id <- pca_search()$top5[1]
    filename <- paste0("shoes/", sprintf("%04d", best_id), ".png")
    img <- image_read(filename)
    plot(img) #선택한 신발 사진과 가장 가까운 신발 사진 띄우기
  })
  
  output$average_shoe <- renderPlot({
    avg <- matrix(shoes_PCA$center, 64, 64, byrow = TRUE)
    image(t(avg)[, 64:1], col = gray.colors(256),
          axes = FALSE, asp = 1) #Avg shoe(카탈로그 2000장의 평균 신발)
  })
  
  output$rebuilt_shoe <- renderPlot({
    K <- input$K
    target <- pca_search()$target
    
    rebuilt <- shoes_PCA$center +
      as.numeric(target %*% t(shoes_PCA$rotation[, 1:K, drop = FALSE]))
    
    rebuilt_img <- matrix(rebuilt, 64, 64, byrow = TRUE)
    
    image(t(rebuilt_img)[, 64:1], col = gray.colors(256),
          zlim = c(0, 255), axes = FALSE, asp = 1)
  }) #rebuilt photo(선택한 사진을 K개 PC로 복원한 이미지)
 
  # Prediction
  output$result <- renderText({
    top5 <- pca_search()$top5
    actual <- search()$actual
    
    categories <- shoes_meta$category[top5]
    
    predicted <- names(which.max(table(categories))) # 가장 많이 나온 카테고리를 예측값으로 사용
    
    if (is.na(actual)) { # 업로드 사진일 경우 : 실제 카테고리 모름
      paste("Predicted category:", predicted,
            "(No actual category)")
    
    } else if (predicted == actual) {
      paste("Predicted category:", predicted,
            "| Actual category:", actual,
            "| Correct!")
      
    } else {
      paste("Predicted category:", predicted,
            "| Actual category:", actual,
            "| Incorrect!")
    }
  })
  # Explain PCA tab
  # step 1
  output$ex_average <- renderPlot({
    avg <- matrix(shoes_PCA$center, 64, 64, byrow = TRUE)
    image(t(avg)[, 64:1], col = gray.colors(256),
          axes = FALSE, asp = 1)
  })
  # step 2 
  output$ex_difference <- renderPlot({
    x <- search()$x
    difference <- x - shoes_PCA$center
    diff_img <- matrix(difference, 64, 64, byrow = TRUE)
    limit <- max(abs(difference))
    image(t(diff_img)[, 64:1],
          col = colorRampPalette(c("blue", "white", "red"))(256), 
          zlim = c(-limit, limit),
          axes = FALSE, asp = 1)
  })
  # step 3
  output$ex_scoremap <- renderPlot({
    x <- search()$x
    target <- as.numeric(
      (x - shoes_PCA$center) %*%
        shoes_PCA$rotation[, 1:2]
    )
    categories <- as.factor(shoes_meta$category)
    plot(shoes_PCA$x[, 1], shoes_PCA$x[, 2],
         col = as.numeric(categories),
         pch = 16, cex = 0.5,
         xlab = "PC1", ylab = "PC2",
         main = "PC1 vs PC2")

    points(target[1], target[2],
           col = "red", pch = 8, cex = 2, lwd = 2)
    
    legend("topright",
           legend = c(levels(categories), "Selected"),
           col = c(seq_along(levels(categories)), "red"),
           pch = c(rep(16, nlevels(categories)), 8),
           cex = 0.8)
  })
  
  
  # step 4
  output$ex_eigenshoes <- renderPlot({
    par(mfrow = c(1, 5))
    
    for (i in 1:5) {
      pc <- shoes_PCA$rotation[, i]
      pc_img <- matrix(pc, 64, 64, byrow = TRUE)
      
      variance <- 100 * shoes_PCA$sdev[i]^2 /
        sum(shoes_PCA$sdev^2)
      
      image(t(pc_img)[, 64:1],
            col = gray.colors(256),
            zlim = range(pc),
            axes = FALSE, asp = 1,
            main = paste0("PC", i, "\n",
                          round(variance, 2), "%"))
    }
    
    par(mfrow = c(1, 1))
  })
  
  
  # step 5
  output$ex_scores <- renderPlot({
    target <- pca_search()$target
    K <- input$K
    
    barplot(target,
            names.arg = 1:K,
            col = "steelblue",
            xlab = "Principal Component",
            ylab = "PC Score",
            main = paste("First", K, "PC Scores"))
  })
  
  
  # step 6
  output$ex_rebuild <- renderPlot({
    x <- search()$x
    
    K_values <- unique(c(1, 5, 20, 50, input$K))
    K_values <- sort(K_values)
    
    par(mfrow = c(1, length(K_values)))
    
    for (K in K_values) {
      target <- as.numeric(
        (x - shoes_PCA$center) %*%
          shoes_PCA$rotation[, 1:K, drop = FALSE]
      )
      
      rebuilt <- shoes_PCA$center +
        as.numeric(target %*%
                     t(shoes_PCA$rotation[, 1:K, drop = FALSE]))
      
      rebuilt_img <- matrix(rebuilt, 64, 64, byrow = TRUE)
      
      image(t(rebuilt_img)[, 64:1],
            col = gray.colors(256),
            zlim = c(0, 255),
            axes = FALSE, asp = 1,
            main = paste("K =", K))
    }
    
    par(mfrow = c(1, 1))
  })
  
  # Our Shoes tab
  
  
########################################################  
  # Data 1 tab
  

  # AI tab: fill in the two tables. Keep the columns.
  output$ai_table <- renderTable(data.frame(
    Tab           = c("Shoe Search", "Explain PCA", "Our Shoes", "Data 1"),
    `What AI did` = c("TODO", "TODO", "TODO", "TODO"),
    `What we did` = c("TODO", "TODO", "TODO", "TODO"),
    check.names = FALSE))
  output$ai_reflection <- renderTable(data.frame(
    Member     = c("강민경", "구가현", "김지유", "서은서", "조가희"),
    Reflection = c("TODO: one or two paragraphs: what AI helped with, where it failed, what you learned.",
                   "TODO", "TODO", "TODO", "TODO")))
}

shinyApp(ui, server)
