# ============================================================
# P2 Part 1. Shoes — PCA shoe search (one app.R, ready to deploy)
# ------------------------------------------------------------
# Folder layout (unzip P2_for_students.zip into this folder)
#   shoes_app/
#   ├── app.R              <- this file
#   ├── shoes_meta.csv     <- id, category
#   ├── shoes/             <- 0001.png ... 2000.png (64 x 64 grayscale)
#   ├── selfcheck/         <- 4 practice photos + labels.csv
#   └── our_shoes/         <- our own photos, named  Name_Category_n.jpg
#                             e.g. Whae_Sandals_1.jpg  (3 per member)
# ============================================================


# ------------------------------------------------------------
# Load packages
# (Do not put install.packages() in app.R)
# ------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(png)      # read the catalogue PNGs
library(magick)   # read and resize uploaded / our own photos (JPG or PNG)

IMG   <- 64    # every photo is IMG x IMG
K_MAX <- 200   # keep the first K_MAX principal components


# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

# Read a catalogue PNG as a 64 x 64 matrix of gray values in [0, 1]
read_gray <- function(path) {
  m <- png::readPNG(path)
  if (length(dim(m)) == 3) {                      # RGB(A) -> gray
    m <- if (dim(m)[3] >= 3) (m[, , 1] + m[, , 2] + m[, , 3]) / 3 else m[, , 1]
  }
  m
}

# Read any photo (JPG / PNG, any size) and turn it into a catalogue-style
# 64 x 64 gray vector: white background, shoe centred, square.
load_photo <- function(path) {
  img <- image_read(path) |>
    image_orient() |>                             # fix phone rotation (EXIF)
    image_background("white") |>
    image_flatten() |>
    image_convert(colorspace = "gray")
  info <- image_info(img)
  
  if (!(info$width == IMG && info$height == IMG)) {
    img  <- image_trim(img, fuzz = 15)            # cut away the plain background
    info <- image_info(img)
    side <- round(max(info$width, info$height) * 1.1)   # square + small margin
    img  <- img |>
      image_extent(paste0(side, "x", side), gravity = "center", color = "white") |>
      image_resize(paste0(IMG, "x", IMG, "!"))
  }
  arr <- as.integer(image_data(img, channels = "gray"))  # height x width x 1
  as.vector(arr[, , 1]) / 255
}

# Draw a vector of length 64*64 as a picture.
# diverging = TRUE : blue (negative) / white (0) / red (positive),
#                    for "photo - average" and eigenshoes.
draw_img <- function(v, main = "", diverging = FALSE) {
  m <- matrix(v, IMG, IMG)
  if (diverging) {
    pal <- colorRampPalette(c("#2166AC", "white", "#B2182B"))(101)
    s   <- max(abs(m)); if (s == 0) s <- 1
    r   <- matrix(pal[round((m / s + 1) * 50) + 1], IMG, IMG)
  } else {
    r <- pmin(pmax(m, 0), 1)
  }
  plot.new()
  plot.window(c(0, 1), c(0, 1), asp = 1)
  rasterImage(as.raster(r), 0, 0, 1, 1, interpolate = FALSE)
  title(main, cex.main = 0.95, font.main = 1)
}


# ------------------------------------------------------------
# Load the data: always with 'relative paths'
# ------------------------------------------------------------
meta <- read.csv("shoes_meta.csv") |>
  mutate(category = as.character(category),
         file     = file.path("shoes", sprintf("%04d.png", id)))
cats <- sort(unique(meta$category))

cat_vec <- function(id) as.vector(read_gray(meta$file[meta$id == id]))


# ------------------------------------------------------------
# PCA on the 2,000 catalogue photos (rows = photos, columns = 4,096 pixels)
# Computed once when the app starts. The result is cached in pca_cache.rds
# so later starts are fast. Delete pca_cache.rds if the photos change.
# ------------------------------------------------------------
if (file.exists("pca_cache.rds")) {
  pc <- readRDS("pca_cache.rds")
} else {
  X  <- t(vapply(meta$file, function(f) as.vector(read_gray(f)), numeric(IMG * IMG)))
  p  <- prcomp(X, center = TRUE, scale. = FALSE, rank. = K_MAX)
  pc <- list(center = p$center, rotation = p$rotation, scores = p$x, sdev = p$sdev)
  try(saveRDS(pc, "pca_cache.rds"), silent = TRUE)
  rm(X, p)
}

x_bar   <- pc$center                          # Step 1: average shoe (length 4096)
V       <- pc$rotation                        # eigenshoes (4096 x K_MAX)
S       <- unname(pc$scores)                  # catalogue scores (2000 x K_MAX)
var_exp <- pc$sdev^2 / sum(pc$sdev^2)         # variance explained by each PC
K_MAX   <- ncol(V)

# Find the closest catalogue photos using the first K PC scores
search_shoe <- function(z_all, K, exclude_id = NA) {
  z <- z_all[1:K]
  d <- sqrt(colSums((t(S[, 1:K, drop = FALSE]) - z)^2))   # Euclidean distance
  res <- meta |>
    select(id, category) |>
    mutate(distance = d) |>
    filter(is.na(exclude_id) | id != exclude_id) |>        # leave the photo itself out
    arrange(distance)
  top5 <- head(res, 5)
  
  # Prediction: most common category in the top 5 (tie -> the closer one wins)
  tab     <- table(top5$category)
  winners <- names(tab)[tab == max(tab)]
  pred    <- top5$category[top5$category %in% winners][1]
  
  list(z       = z,
       rebuild = x_bar + drop(V[, 1:K, drop = FALSE] %*% z),
       top5    = top5,
       pred    = pred)
}

project <- function(v) drop((v - x_bar) %*% V)   # all K_MAX scores of a photo


# ------------------------------------------------------------
# Self-check photos (+ their true categories from labels.csv)
# ------------------------------------------------------------
self_files <- list.files("selfcheck", pattern = "\\.(png|jpe?g)$", ignore.case = TRUE)

lab_path  <- c("selfcheck/labels.csv", "labels.csv")
lab_path  <- lab_path[file.exists(lab_path)][1]
sc_labels <- if (!is.na(lab_path)) read.csv(lab_path) else data.frame(file = character(), category = character())
names(sc_labels)[1] <- "file"                    # first column = file name

self_truth <- function(f) {
  key <- tools::file_path_sans_ext(basename(as.character(sc_labels$file)))
  hit <- sc_labels$category[key == tools::file_path_sans_ext(f)]
  if (length(hit) == 0) NA else as.character(hit[1])
}


# ------------------------------------------------------------
# Our own shoes: our_shoes/Name_Category_n.jpg
# ------------------------------------------------------------
our_files <- list.files("our_shoes", pattern = "\\.(png|jpe?g)$",
                        ignore.case = TRUE, full.names = TRUE)
parts <- strsplit(tools::file_path_sans_ext(basename(our_files)), "_")
our <- data.frame(
  file     = our_files,
  name     = vapply(parts, `[`, "", 1),
  category = vapply(parts, function(x) if (length(x) >= 2) x[2] else NA_character_, ""),
  stringsAsFactors = FALSE
)
our$label <- basename(our$file)
our_vecs  <- lapply(our$file, load_photo)       # processed once at start
our_z     <- lapply(our_vecs, project)


# ------------------------------------------------------------
# UI : what to show, and where
# ------------------------------------------------------------
ui <- page_navbar(
  title = "InSighters",
  fillable = FALSE, 
  
  # ---- Shared sidebar: choose the photo + K ----------------------
  sidebar = sidebar(
    width = 300,
    
    radioButtons(
      inputId  = "source",
      label    = "Choose a photo",
      choices  = c("Catalogue id"      = "cat",
                   "Self-check photo"  = "self",
                   "Upload (JPG/PNG)"  = "up"),
      selected = "cat"
    ),
    
    conditionalPanel(
      "input.source == 'cat'",
      numericInput("cat_id", "Catalogue id", value = min(meta$id),
                   min = min(meta$id), max = max(meta$id), step = 1)
    ),
    conditionalPanel(
      "input.source == 'self'",
      selectInput("self_file", "Self-check photo", choices = self_files)
    ),
    conditionalPanel(
      "input.source == 'up'",
      fileInput("upload", "Upload a shoe photo", accept = c(".jpg", ".jpeg", ".png")),
      selectInput("up_true", "True category (optional)", choices = c("Unknown", cats))
    ),
    
    hr(),
    sliderInput("K", "Number of PCs (K)", min = 1, max = K_MAX, value = min(50, K_MAX)),
    helpText("K is used in Shoe Search, Explain PCA and Our Shoes.")
  ),
  
  # ==== Tab 1. Shoe Search =========================================
  nav_panel(
    "Shoe Search",
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(title = "Prediction (top 5 vote)", value = textOutput("pred")),
      value_box(title = "True category",           value = textOutput("truth")),
      value_box(title = "Correct?",                value = textOutput("correct"))
    ),
    card(
      card_header("Your photo · average shoe · rebuilt with K PCs · best match"),
      plotOutput("match_plot", height = "260px")
    ),
    layout_columns(
      col_widths = c(5, 7),
      card(card_header("Top 5 closest photos"), tableOutput("top5_table")),
      card(card_header("Top 5 as pictures"),    plotOutput("top5_plot", height = "200px"))
    )
  ),
  
  # ==== Tab 2. Explain PCA =========================================
  nav_panel(
    "Explain PCA",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Step 1–2. Average shoe x̄ and photo − average (x − x̄)"),
        plotOutput("step12_plot", height = "260px"),
        p("Red = brighter than the average shoe, blue = darker.")
      ),
      card(
        card_header("Step 3. Score map (PC1 vs PC2)"),
        plotOutput("score_map", height = "320px"),
        p("Black star = your photo, circled points = its top 5 matches.")
      )
    ),
    card(
      card_header("Step 4. Eigenshoes (PC loadings drawn as 64 × 64 images)"),
      sliderInput("n_eigen", "How many eigenshoes", min = 2, max = 16, value = 8, width = "300px"),
      plotOutput("eigen_plot", height = "300px"),
      plotOutput("var_plot", height = "260px")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Step 5. Your photo's first K PC scores"),
        plotOutput("score_bar", height = "280px"),
        verbatimTextOutput("score_text")
      ),
      card(
        card_header("Step 6. Rebuild: x̄ + z₁·PC1 + … + z_K·PC_K"),
        plotOutput("rebuild_plot", height = "280px")
      )
    ),
    card(
      card_header("Extra. Does the prediction depend on K?"),
      plotOutput("k_plot", height = "260px"),
      p("For your photo: the predicted category and the distance to the best match for each K.")
    )
  ),
  
  # ==== Tab 3. Our Shoes ===========================================
  nav_panel(
    "Our Shoes",
    card(
      card_header("All our shoes (name / category)"),
      plotOutput("our_gallery", height = "auto")
    ),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Choose one of our shoes"),
        selectInput("our_pick", NULL,
                    choices = setNames(seq_len(nrow(our)), our$label)),
        textOutput("our_result"),
        hr(),
        tableOutput("our_top5")
      ),
      card(
        card_header("Our photo · rebuilt with K PCs · best match"),
        plotOutput("our_match_plot", height = "260px")
      )
    ),
    card(
      card_header("How many of our shoes are recognised? (current K)"),
      textOutput("our_acc"),
      tableOutput("our_all_table")
    )
  ),
  
  # ==== Tab 4. Data 1 ==============================================
  # One Project 1 dataset, descriptive results only
  # (filter, sort, count, mean, median, rate, group summary).
  nav_panel(
    "Data 1",
    p("One Project 1 dataset, descriptive results only (filter, sort, count, mean, median, rate, group summary)."),
    p("It should include:"),
    tags$ul(
      tags$li("the question you are trying to answer;"),
      tags$li("the inputs that show it (e.g., the filter, grouping or time range you choose);"),
      tags$li("the result in numbers;"),
      tags$li("why it matters;"),
      tags$li("how this finding helps your next analysis of this dataset.")
    )
  ),
  
  # ==== Tab 5. AI (same format as the template) =====================
  nav_panel(
    "AI",
    h4("Ethics statement"),
    p("We understand the rules on data and AI use for this project, including following the license and privacy rules of every dataset we used, understanding all of our code, and knowing that presenting AI output as our own work may be academic misconduct. We certify that we have followed all of them. Any violation is our responsibility, and we agree to receive an F in this course if we have not."),
    p("Signed: TODO (every member)"),
    h4("What AI did and what we did"),
    tableOutput("ai_table"),
    h4("Reflection"),
    tableOutput("reflection_table")
  )
)


# ------------------------------------------------------------
# Server : takes the inputs, computes, and makes the outputs
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  # [Key] the chosen photo -> one reactive, reused by every tab
  query <- reactive({
    if (input$source == "cat") {
      id <- suppressWarnings(as.integer(input$cat_id))
      validate(need(!is.na(id) && id %in% meta$id,
                    paste0("Enter an id between ", min(meta$id), " and ", max(meta$id), ".")))
      list(vec     = cat_vec(id),
           label   = paste("Catalogue id", id),
           truth   = meta$category[meta$id == id],
           exclude = id)
      
    } else if (input$source == "self") {
      validate(need(isTruthy(input$self_file), "No self-check photos found in selfcheck/."))
      list(vec     = load_photo(file.path("selfcheck", input$self_file)),
           label   = input$self_file,
           truth   = self_truth(input$self_file),
           exclude = NA)
      
    } else {
      validate(need(input$upload, "Upload a JPG or PNG shoe photo."))
      list(vec     = load_photo(input$upload$datapath),
           label   = input$upload$name,
           truth   = if (input$up_true == "Unknown") NA else input$up_true,
           exclude = NA)
    }
  })
  
  z_all  <- reactive(project(query()$vec))
  result <- reactive(search_shoe(z_all(), input$K, query()$exclude))
  
  # ================= Tab 1. Shoe Search ===================================
  output$pred  <- renderText(result()$pred)
  output$truth <- renderText(ifelse(is.na(query()$truth), "Unknown", query()$truth))
  output$correct <- renderText({
    t <- query()$truth
    if (is.na(t)) "-" else if (t == result()$pred) "Yes ✓" else "No ✗"
  })
  
  output$match_plot <- renderPlot({
    r    <- result()
    best <- r$top5[1, ]
    par(mfrow = c(1, 4), mar = c(0, 0, 2, 0))
    draw_img(query()$vec, query()$label)
    draw_img(x_bar,       "Average shoe")
    draw_img(r$rebuild,   paste0("Rebuilt (K = ", input$K, ")"))
    draw_img(cat_vec(best$id),
             paste0("Best: id ", best$id, " (", best$category, ")"))
  })
  
  output$top5_table <- renderTable(digits = 3, {
    result()$top5 |> mutate(rank = row_number(), .before = id)
  })
  
  output$top5_plot <- renderPlot({
    t5 <- result()$top5
    par(mfrow = c(1, 5), mar = c(0, 0, 2, 0))
    for (i in seq_len(nrow(t5))) {
      draw_img(cat_vec(t5$id[i]), paste0(t5$id[i], "\n", t5$category[i]))
    }
  })
  
  # ================= Tab 2. Explain PCA ===================================
  
  # Step 1–2
  output$step12_plot <- renderPlot({
    par(mfrow = c(1, 3), mar = c(0, 0, 2, 0))
    draw_img(query()$vec,          "Your photo  x")
    draw_img(x_bar,                "Average shoe  x̄")
    draw_img(query()$vec - x_bar,  "x − x̄", diverging = TRUE)
  })
  
  # Step 3
  output$score_map <- renderPlot({
    df  <- data.frame(PC1 = S[, 1], PC2 = S[, 2], category = meta$category)
    top <- df[match(result()$top5$id, meta$id), ]
    me  <- data.frame(PC1 = z_all()[1], PC2 = z_all()[2])
    
    ggplot(df, aes(PC1, PC2, color = category)) +
      geom_point(alpha = 0.45, size = 1.6) +
      geom_point(data = top, shape = 21, size = 4, stroke = 1.2,
                 color = "black", fill = NA) +
      geom_point(data = me, aes(PC1, PC2), inherit.aes = FALSE,
                 shape = 8, size = 6, stroke = 1.6, color = "black") +
      labs(x = sprintf("PC1 (%.1f%%)", 100 * var_exp[1]),
           y = sprintf("PC2 (%.1f%%)", 100 * var_exp[2]),
           color = "Category") +
      theme_minimal(base_size = 13)
  })
  
  # Step 4
  output$eigen_plot <- renderPlot({
    n <- min(input$n_eigen, K_MAX)
    par(mfrow = c(ceiling(n / 8), min(n, 8)), mar = c(0, 0, 2, 0))
    for (j in seq_len(n)) {
      draw_img(V[, j], sprintf("PC%d (%.1f%%)", j, 100 * var_exp[j]), diverging = TRUE)
    }
  })
  
  output$var_plot <- renderPlot({
    n  <- min(50, K_MAX)
    df <- data.frame(PC = 1:n, each = var_exp[1:n], cumulative = cumsum(var_exp)[1:n])
    ggplot(df, aes(PC)) +
      geom_col(aes(y = each), fill = "#B57A23") +
      geom_line(aes(y = cumulative)) +
      geom_point(aes(y = cumulative), size = 1) +
      geom_vline(xintercept = min(input$K, n), linetype = "dashed") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Principal component",
           y = "Variance explained",
           title = sprintf("Bars = each PC, line = cumulative. First K = %d PCs explain %.1f%%",
                           input$K, 100 * sum(var_exp[1:input$K]))) +
      theme_minimal(base_size = 13)
  })
  
  # Step 5
  output$score_bar <- renderPlot({
    n  <- min(input$K, 30)
    df <- data.frame(PC = factor(paste0("PC", 1:n), levels = paste0("PC", 1:n)),
                     score = z_all()[1:n])
    ggplot(df, aes(PC, score, fill = score > 0)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("#2166AC", "#B2182B")) +
      labs(x = NULL, y = "Score  z = (x − x̄) · PC",
           title = if (input$K > 30) "Showing the first 30 of K scores" else NULL) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  })
  
  output$score_text <- renderPrint({
    K    <- input$K
    best <- result()$top5$id[1]
    z    <- setNames(round(result()$z, 2), paste0("PC", 1:K))
    s    <- setNames(round(S[meta$id == best, 1:K], 2), paste0("PC", 1:K))
    cat("Your photo's first", K, "PC scores (z):\n")
    print(z)
    cat("\nBest match (id", best, ") scores:\n")
    print(s)
    cat("\nDistance = sqrt( sum_j (z_j - s_j)^2 ), j = 1..K  =",
        round(result()$top5$distance[1], 3), "\n")
  })
  
  # Step 6
  output$rebuild_plot <- renderPlot({
    Ks <- sort(unique(c(1, 5, 20, 50, 100, K_MAX, input$K)))
    Ks <- Ks[Ks <= K_MAX]
    z  <- z_all()
    par(mfrow = c(2, ceiling((length(Ks) + 1) / 2)), mar = c(0, 0, 2, 0))
    draw_img(query()$vec, "Original")
    for (k in Ks) {
      rec <- x_bar + drop(V[, 1:k, drop = FALSE] %*% z[1:k])
      draw_img(rec, paste0("K = ", k, if (k == input$K) " (chosen)" else ""))
    }
  })
  
  # Extra: prediction and best distance across K
  output$k_plot <- renderPlot({
    Ks <- unique(round(c(1:10, seq(15, K_MAX, by = 5))))
    Ks <- Ks[Ks <= K_MAX]
    df <- do.call(rbind, lapply(Ks, function(k) {
      r <- search_shoe(z_all(), k, query()$exclude)
      data.frame(K = k, distance = r$top5$distance[1], prediction = r$pred)
    }))
    ggplot(df, aes(K, distance)) +
      geom_line(color = "gray60") +
      geom_point(aes(color = prediction), size = 2.5) +
      geom_vline(xintercept = input$K, linetype = "dashed") +
      labs(y = "Distance to best match", color = "Prediction") +
      theme_minimal(base_size = 13)
  })
  
  # ================= Tab 3. Our Shoes =====================================
  output$our_gallery <- renderPlot({
    validate(need(nrow(our) > 0, "Put our photos in the our_shoes/ folder (Name_Category_n.jpg)."))
    n <- nrow(our)
    par(mfrow = c(ceiling(n / 6), min(n, 6)), mar = c(0, 0, 2, 0))
    for (i in seq_len(n)) draw_img(our_vecs[[i]], paste0(our$name[i], "\n", our$category[i]))
  }, height = function() 170 * max(1, ceiling(nrow(our) / 6)))
  
  our_res <- reactive({
    validate(need(nrow(our) > 0, "No photos in our_shoes/."))
    i <- as.integer(input$our_pick)
    list(i = i, r = search_shoe(our_z[[i]], input$K))
  })
  
  output$our_result <- renderText({
    o <- our_res(); t <- our$category[o$i]
    paste0("True: ", t, "  |  Predicted: ", o$r$pred, "  |  ",
           if (identical(t, o$r$pred)) "Correct ✓" else "Wrong ✗")
  })
  
  output$our_top5 <- renderTable(digits = 3, our_res()$r$top5)
  
  output$our_match_plot <- renderPlot({
    o <- our_res(); best <- o$r$top5[1, ]
    par(mfrow = c(1, 3), mar = c(0, 0, 2, 0))
    draw_img(our_vecs[[o$i]], our$label[o$i])
    draw_img(o$r$rebuild, paste0("Rebuilt (K = ", input$K, ")"))
    draw_img(cat_vec(best$id), paste0("Best: id ", best$id, " (", best$category, ")"))
  })
  
  our_all <- reactive({
    validate(need(nrow(our) > 0, "No photos in our_shoes/."))
    preds <- vapply(our_z, function(z) search_shoe(z, input$K)$pred, "")
    data.frame(photo = our$label, member = our$name, true = our$category,
               predicted = preds, correct = ifelse(our$category == preds, "✓", "✗"))
  })
  
  output$our_acc <- renderText({
    d <- our_all()
    sprintf("%d of %d correct (%.0f%%) with K = %d",
            sum(d$correct == "✓"), nrow(d), 100 * mean(d$correct == "✓"), input$K)
  })
  
  output$our_all_table <- renderTable(our_all(), striped = TRUE, spacing = "xs")
  
  # ================= Tab 5. AI ============================================
  output$ai_table <- renderTable(data.frame(
    Tab            = c("Shoe Search", "Explain PCA", "Our Shoes", "Data 1"),
    `What AI did`  = "TODO",
    `What we did`  = "TODO",
    check.names = FALSE
  ))
  
  output$reflection_table <- renderTable(data.frame(
    Member     = rep("TODO", 5),
    Reflection = c("TODO: one or two paragraphs: what AI helped with, where it failed, what you learned.",
                   rep("TODO", 4))
  ))
}


# ------------------------------------------------------------
# Run the app
# ------------------------------------------------------------
shinyApp(ui, server)

