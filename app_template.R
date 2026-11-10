# Project 2 template: the tabs and the inputs only. Everything else, and the look of the app, is yours.
library(shiny)

# Example: read one photo as 4,096 numbers (64 x 64 grayscale, row by row, 0 = black to 255 = white)
# library(magick)
# img <- image_read("shoes/0001.png")
# x <- as.numeric(t(matrix(as.integer(image_data(img, channels = "gray")), 64, 64)))

ui <- navbarPage("TODO: team name",
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
}

shinyApp(ui, server)
