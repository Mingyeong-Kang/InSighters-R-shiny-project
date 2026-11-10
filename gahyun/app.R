# =============================================================================
# Project 2 — PCA로 비슷한 신발 찾기 (Statistical Graphics with Big Data, Fall 2026)
# 팀 InSighters · 구가현 개인 시안 (최종 제출안 아님)
#
# 파일 구성 (위에서 아래 순서)
#   0. 데이터와 PCA 불러오기   : prep.R이 만든 pca_cache.rds를 읽는다
#   1. 도우미 함수            : 사진 전처리, 검색, 투표, 그림 그리기
#   2. Our Shoes 사진 준비     : our_shoes/ 폴더의 팀원 사진을 미리 읽어 둔다
#   3. UI (화면)             : 탭 5개의 배치. "무엇을 어디에 보여줄지"만 정한다
#   4. Server (계산)          : 입력이 바뀌면 다시 계산해서 화면의 각 자리를 채운다
#   마지막 줄 shinyApp(ui, server) 가 둘을 합쳐 앱을 실행한다.
#
# 실행: RStudio에서 이 파일을 열고 오른쪽 위 "Run App" 클릭.
# =============================================================================

library(shiny)     # 웹 앱 프레임워크 (ui, server, reactive, render 함수)
library(bslib)     # 화면 꾸미기 (page_navbar, card, value_box, 테마 색)
library(magick)    # 사진 읽기·자르기·크기 조절 (업로드 사진, 팀원 사진 전처리)
library(ggplot2)   # 점수 지도, 설명분산, 정확도 그래프

# -----------------------------------------------------------------------------
# 0. 데이터와 PCA 불러오기
#    PCA 계산은 2.5분 정도 걸려서 prep.R에서 한 번만 하고 결과를 파일로 저장해 둔다.
#    캐시 파일이 없을 때만 prep.R을 자동 실행한다.
# -----------------------------------------------------------------------------
if (!file.exists("pca_cache.rds")) source("prep.R", local = new.env())
P    <- readRDS("pca_cache.rds")
meta <- P$meta   # 2000행: id, category (Boots / Sandals / Shoes / Slippers)
X    <- P$X      # 2000 x 4096 행렬: 사진 한 장 = 한 행 = 픽셀 4,096개 (0 = 검정, 255 = 흰색)
mu   <- P$mu     # 길이 4096: 평균 신발 x̄ (픽셀별 평균)
V    <- P$V      # 4096 x 100: eigenshoes (PC1~PC100의 적재벡터, 각 열의 길이는 1)
S    <- P$S      # 2000 x 100: 카탈로그 사진마다 PC1~PC100 점수
CATS <- levels(meta$category)   # 카테고리 4개 이름
KMAX <- ncol(V)                 # 저장해 둔 PC 개수 (100)
# 카테고리 색: 색맹도 구분 가능한 Okabe-Ito 팔레트. 모든 그림에서 같은 색을 쓴다.
CAT_COL <- c(Boots = "#0072B2", Sandals = "#E69F00", Shoes = "#009E73", Slippers = "#CC79A7")
K_DEFAULT <- 20                 # K 슬라이더 기본값 (교수님 템플릿과 같은 값)

# -----------------------------------------------------------------------------
# 1. 도우미 함수
# -----------------------------------------------------------------------------

# [사진 전처리] 아무 사진(JPG/PNG) -> 숫자 4,096개.
# 직접 찍은 사진은 카탈로그와 모양이 다르므로(배경색, 크기, 위치) 카탈로그처럼 맞춰 준다.
#   crop  = TRUE : 배경을 잘라내고 신발을 카탈로그와 같은 위치에 놓는다 (업로드, 팀원 사진)
#   clean = TRUE : 회색 종이 배경을 흰색으로 밝힌다
# self-check 사진은 이미 카탈로그 형식이라 crop = FALSE, clean = FALSE로 그대로 읽는다.
photo_to_vec <- function(path, crop = TRUE, clean = TRUE) {
  img <- image_read(path)[1]                             # 첫 프레임만 (GIF 등 대비)
  img <- image_orient(img)                               # 폰 사진 회전 정보(EXIF)대로 바로 세우기
  img <- image_flatten(image_background(img, "white"))   # 투명 배경 PNG -> 흰 배경
  img <- image_convert(image_scale(img, "600x600>"), colorspace = "gray")  # 계산이 빠르게 600px 이하로 줄이고 흑백 변환
  if (clean) {
    # 사진 테두리 픽셀의 중앙값 = 배경 밝기로 보고, 그 밝기가 흰색(255)이 되도록 전체를 밝힌다.
    a  <- as.integer(image_data(img, channels = "gray"))[, , 1]
    bg <- median(c(a[1, ], a[nrow(a), ], a[, 1], a[, ncol(a)]))
    if (bg > 40 && bg < 250) img <- image_level(img, black_point = 0, white_point = 100 * bg / 255)
  }
  if (crop) {
    # 카탈로그 2,000장을 재 보니 신발이 가로를 거의 꽉 채우고(1~62열),
    # 바닥이 54번째 줄에 닿고, 높이는 46픽셀 정도까지였다. 같은 자리에 놓는다.
    img <- image_trim(img, fuzz = 20)                    # 배경과 비슷한 가장자리(20% 이내)를 잘라 신발만 남김
    img <- image_resize(img, "62x46")                    # 비율을 유지하며 62 x 46 상자 안에 맞춤
    inf <- image_info(img)
    img <- image_composite(image_blank(64, 64, "white"), img,      # 64 x 64 흰 도화지 위에
                           offset = sprintf("+%d+%d", (64 - inf$width) %/% 2, 55 - inf$height))  # 가로 가운데, 바닥 54줄
    img <- image_convert(img, colorspace = "gray")
  }
  img <- image_resize(img, "64x64!")                     # 최종 64 x 64 ("!" = 비율 무시하고 정확히)
  # 템플릿과 같은 규칙: 윗줄부터 한 줄씩 읽어 4,096개 숫자로 편다.
  as.numeric(t(matrix(as.integer(image_data(img, channels = "gray")), 64, 64)))
}

# [좌우 반전] 카탈로그 신발은 모두 앞코가 오른쪽. 왼쪽을 향한 사진은 뒤집어서 비교한다.
mirror_vec <- function(x) as.numeric(t(matrix(x, 64, 64, byrow = TRUE)[, 64:1]))

# [예측 규칙] Top 5 중 가장 많이 나온 카테고리.
# 동점이면(예: 2-2-1) 동점 카테고리 중 더 가까운 순위에 먼저 나온 것을 고른다.
vote5 <- function(cats) {
  cats <- as.character(cats)                       # cats는 가까운 순서대로 5개
  tb   <- table(factor(cats, levels = CATS))       # 카테고리별 개수
  best <- names(tb)[tb == max(tb)]                 # 최다 득표 카테고리(동점이면 여러 개)
  cats[cats %in% best][1]                          # 그중 가장 가까운 순위의 것
}

# [PCA 검색] 핵심 함수. 사진 x(숫자 4,096개)와 K를 받아 가장 가까운 카탈로그 사진을 찾는다.
#   1) 점수: score_k = (x - x̄) · v_k  (k = 1..K)
#   2) 거리: 카탈로그 사진 2,000장의 점수와 유클리드 거리
#   3) 가장 가까운 5장 = Top 5, 투표로 카테고리 예측
#   exclude: 카탈로그 사진을 검색할 때 자기 자신을 빼기 위한 id
pca_search <- function(x, K, exclude = NULL) {
  q  <- drop(crossprod(V[, 1:K, drop = FALSE], x - mu))          # 이 사진의 K개 점수
  Sk <- S[, 1:K, drop = FALSE]                                    # 카탈로그 2,000장의 K개 점수
  d  <- sqrt(rowSums((Sk - rep(q, each = nrow(Sk)))^2))           # 2,000개 거리 (PC 공간의 유클리드 거리)
  if (!is.null(exclude)) d[exclude] <- Inf                        # 자기 자신은 무한대 거리로 -> 검색에서 제외
  o  <- order(d)[1:5]                                             # 거리 작은 순 5개의 행 번호
  top <- data.frame(Rank = 1:5, id = meta$id[o], Category = as.character(meta$category[o]),
                    Distance = round(d[o], 1))
  list(q = q,                     # 점수 K개 (Explain PCA Step 5에서 사용)
       d = d,                     # 모든 거리
       top = top,                 # Top 5 표
       best = o[1],               # best match의 행 번호
       pred = vote5(top$Category),# 예측 카테고리
       rebuilt = drop(mu + V[, 1:K, drop = FALSE] %*% q))   # K개 PC로 복원한 사진: x̂ = x̄ + Σ score_k · v_k
}

# [방향까지 고려한 검색] mirror = "No"(그대로), "Yes"(뒤집어서), "Auto"(둘 다 해 보고 더 가까운 쪽)
search_oriented <- function(x, K, mirror = "Auto", exclude = NULL) {
  a <- pca_search(x, K, exclude); a$x <- x; a$mirrored <- FALSE
  if (mirror == "No") return(a)
  xm <- mirror_vec(x)
  b <- pca_search(xm, K, exclude); b$x <- xm; b$mirrored <- TRUE
  if (mirror == "Yes" || min(b$d) < min(a$d)) b else a
}

# ---- 그림 도우미: 64 x 64 사진을 base R 그래픽으로 빠르게 그린다 ----

# 숫자 4,096개 -> 흑백 이미지 (0~255를 0~1로 바꿔 raster로)
gray_raster <- function(v) as.raster(matrix(pmin(pmax(v, 0), 255) / 255, 64, 64, byrow = TRUE))

# 양수/음수가 있는 값(x − x̄, eigenshoe) -> 파랑(음수)·흰색(0)·빨강(양수) 색 이미지
DIV_PAL <- colorRampPalette(c("#2166AC", "#92C5DE", "#F7F7F7", "#F4A582", "#B2182B"))(101)
div_raster <- function(v, lim = max(abs(v))) {
  idx <- round((pmin(pmax(v, -lim), lim) / lim + 1) * 50) + 1    # -lim..lim 을 1..101번 색으로
  as.raster(matrix(DIV_PAL[idx], 64, 64, byrow = TRUE))
}

# 사진 한 칸 그리기: 이미지 + 테두리 + 위 제목(main) + 아래 설명(sub)
tile <- function(r, main = "", sub = "", border = "grey80", cex = 1) {
  plot.new(); plot.window(c(0, 1), c(0, 1), asp = 1, xaxs = "i", yaxs = "i")   # 정사각형 그림 영역
  rasterImage(r, 0, 0, 1, 1, interpolate = FALSE)                            # 픽셀을 흐리게 보간하지 않음
  rect(0, 0, 1, 1, border = border, lwd = 3)
  text(0.5, 1.04, main, adj = c(0.5, 0), font = 2, cex = cex, xpd = NA)      # 사진 바로 위 제목
  if (nzchar(sub)) text(0.5, -0.04, sub, adj = c(0.5, 1), cex = 0.85 * cex, col = "grey30", xpd = NA)  # 사진 바로 아래
}
# 여러 칸을 한 줄(또는 여러 줄)로 놓기. cex = 1 은 칸이 많아도 글씨가 작아지지 않게 고정.
tile_par <- function(n_col, n_row = 1) par(mfrow = c(n_row, n_col), mar = c(2.2, 0.4, 2.8, 0.4), bg = "white", cex = 1)

# 예측 결과 카드: 맞으면 초록(✓), 틀리면 빨강(✗), 정답을 모르면 회색(?)
result_box <- function(pred, truth) {
  if (is.na(truth) || truth == "Unknown") {
    value_box(title = "Prediction (top-5 vote)", value = pred, showcase = icon_txt("?"),
              p("True category unknown"), theme = "secondary")
  } else if (pred == truth) {
    value_box(title = "Prediction (top-5 vote)", value = pred, showcase = icon_txt("✓"),
              p(paste("Right: the true category is", truth)), theme = "success")
  } else {
    value_box(title = "Prediction (top-5 vote)", value = pred, showcase = icon_txt("✗"),
              p(paste("Wrong: the true category is", truth)), theme = "danger")
  }
}
icon_txt <- function(s) tags$span(s, style = "font-size:3rem;font-weight:700;line-height:1")  # 카드 왼쪽 큰 기호

# Top 5 사진 5칸 (테두리 색 = 카테고리 색). Shoe Search와 Our Shoes에서 같이 씀.
top5_plot <- function(res) {
  tile_par(5)
  for (i in 1:5) {
    j <- res$top$id[i]; ct <- res$top$Category[i]
    tile(gray_raster(X[j, ]), main = sprintf("#%d  id %d", i, j),
         sub = sprintf("%s · d = %.0f", ct, res$top$Distance[i]), border = CAT_COL[ct])
  }
}

# -----------------------------------------------------------------------------
# 2. Our Shoes 사진 준비 (앱이 켜질 때 한 번)
#    our_shoes/ 폴더에 "이름_카테고리_번호.jpg" 형식으로 넣으면 자동 인식한다.
#    예: Gahyun_Sandals_1.jpg  -> 이름 Gahyun, 카테고리 Sandals, 번호 1
# -----------------------------------------------------------------------------
OUR_DIR <- "our_shoes"
our_files <- if (dir.exists(OUR_DIR)) list.files(OUR_DIR, "\\.(jpe?g|png)$", ignore.case = TRUE) else character(0)
# 파일 이름을 정규식으로 쪼갬: (이름)_(카테고리)_(번호).확장자
our_parts <- regmatches(our_files, regexec("^([^_]+)_([A-Za-z]+)_(\\d+)\\.[A-Za-z]+$", our_files))
ok <- lengths(our_parts) == 4                    # 형식이 맞는 파일만
OUR <- data.frame(
  file     = our_files[ok],
  name     = vapply(our_parts[ok], `[`, "", 2),
  category = tools::toTitleCase(tolower(vapply(our_parts[ok], `[`, "", 3))),  # sandals -> Sandals
  n        = vapply(our_parts[ok], `[`, "", 4),
  stringsAsFactors = FALSE)
OUR <- OUR[OUR$category %in% CATS, , drop = FALSE]                 # 카테고리 철자가 틀린 파일은 제외
OUR$label <- sprintf("%s · %s %s", OUR$name, OUR$category, OUR$n)  # 화면에 보일 이름
# 폰 사진은 커서(수 MB) 갤러리가 느려진다 -> 260px 썸네일을 임시 폴더에 만들어 웹에 공개
THUMB_DIR <- file.path(tempdir(), "our_thumbs"); dir.create(THUMB_DIR, showWarnings = FALSE)
addResourcePath("ourthumbs", THUMB_DIR)          # 브라우저에서 "ourthumbs/파일명"으로 접근 가능
# 각 사진을 미리 숫자 4,096개로 바꿔 둔다 (위젯을 바꿀 때마다 다시 읽지 않도록)
OUR_X <- lapply(seq_len(nrow(OUR)), function(i) {
  f <- file.path(OUR_DIR, OUR$file[i])
  image_write(image_scale(image_orient(image_read(f)[1]), "260x260"),
              file.path(THUMB_DIR, paste0(OUR$file[i], ".png")), format = "png")
  photo_to_vec(f)
})

# -----------------------------------------------------------------------------
# 3. UI (화면 배치)
#    여기서는 위젯과 출력 "자리"만 만든다. 자리를 채우는 건 4. Server의 output$<id>.
#    예) plotOutput("best_plot")  <->  output$best_plot <- renderPlot(...)
# -----------------------------------------------------------------------------

# 테마: 색과 글꼴을 한 곳에서 정한다 (bslib / Bootstrap 5)
theme <- bs_theme(version = 5, primary = "#2F5D7C", secondary = "#6C757D", success = "#2E7D32",
                  danger = "#C62828", base_font = font_collection("Segoe UI", "Helvetica", "Arial", "sans-serif"),
                  "navbar-bg" = "#1F3B4D")
# 추가 CSS: 카드 제목 굵게, Step 번호 동그라미, 회색 설명 글씨, 팀원 사진 카드, 스크롤 표
theme <- bs_add_rules(theme, "
  .card-header { font-weight: 600; }
  .step-num { display:inline-block; width:1.8rem; height:1.8rem; border-radius:50%; background:#2F5D7C;
              color:#fff; text-align:center; line-height:1.8rem; margin-right:.5rem; font-size:.9rem; }
  .note { color:#5b6770; font-size:.92rem; }
  .our-card img { width:100%; aspect-ratio:1/1; object-fit:contain; background:#fff; border-radius:6px; }
  .our-card { border:2px solid transparent; border-radius:10px; padding:6px; text-align:center; }
  .scroll-table { max-height: 320px; overflow-y: auto; }
")
step_header <- function(n, txt) card_header(tags$span(class = "step-num", n), txt)   # "① 제목" 모양 카드 머리

ui <- page_navbar(                 # 상단 탭 막대가 있는 페이지 (템플릿의 navbarPage와 같은 역할)
  title = "InSighters",            # 앱 제목 = 팀 이름 (과제 요구사항)
  id = "nav",
  # 주소 뒤에 #explain, #our, #data, #ai 를 붙이면 그 탭으로 바로 열리게 하는 작은 JavaScript
  header = tags$script(HTML("
    // open a tab from the address, e.g. .../#explain
    $(document).on('shiny:connected', function() {
      var tabs = {search:'Shoe Search', explain:'Explain PCA', our:'Our Shoes', data:'Data 1', ai:'AI'};
      var t = tabs[location.hash.slice(1)];
      if (t) $('a[data-value=\"' + t + '\"]').tab('show');
    });")),
  theme = theme,
  fillable = FALSE,                # 화면 높이에 억지로 맞추지 않고 아래로 스크롤

  # ---------------- 탭 1. Shoe Search ----------------
  nav_panel("Shoe Search",
    layout_sidebar(                # 왼쪽 = 입력(sidebar), 오른쪽 = 결과
      sidebar = sidebar(width = 300,
        # 사진 고르는 방법 3가지
        radioButtons("src", "Photo", c("Catalogue", "Self-check", "Upload"), inline = TRUE),
        # conditionalPanel: 선택한 방법에 맞는 입력창만 보여 준다 (조건은 JavaScript 문법)
        conditionalPanel("input.src == 'Catalogue'",
          numericInput("cat_id", "Catalogue id (1–2000)", 1, 1, 2000, 1),
          actionButton("rand_id", "Random id", class = "btn-sm btn-outline-primary"),
          p(class = "note", "The photo itself is left out of the search.")),
        conditionalPanel("input.src == 'Self-check'",
          selectInput("sc_file", "Self-check photo", sprintf("selfcheck_%02d.png", 1:4))),
        conditionalPanel("input.src == 'Upload'",
          fileInput("photo", "Upload a shoe photo", accept = c(".jpg", ".jpeg", ".png")),
          selectInput("up_truth", "True category (to check the prediction)", c("Unknown", CATS)),  # 맞았는지 보려면 정답 선택
          radioButtons("up_mirror", "Mirror the photo?", c("Auto", "No", "Yes"), inline = TRUE),
          p(class = "note", "Catalogue shoes point right. Auto tries both directions and keeps the closer match.")),
        sliderInput("K", "Number of PCs (K)", 1, KMAX, K_DEFAULT, 1),   # 검색에 쓸 PC 개수
        uiOutput("k_info")                                              # K에 따른 설명분산·정확도 안내 문장
      ),
      uiOutput("search_boxes"),    # 맨 위 카드 3개: 예측(맞음/틀림), best match, 선택한 사진
      card(card_header("Best match"),
           plotOutput("best_plot", height = "260px"),   # 내 사진 | 평균 신발 | K개 PC로 복원 | best match
           p(class = "note", "Rebuilt with K PCs = average shoe + the photo's first K scores × eigenshoes. ",
             "The search compares these K scores, not the raw 4,096 pixels.")),
      card(card_header("Top 5 closest catalogue photos"),
           plotOutput("top5_plot", height = "200px"),   # Top 5 사진
           tableOutput("top5_table"))                   # Top 5 표: 순위, id, 카테고리, 거리
    )
  ),

  # ---------------- 탭 2. Explain PCA ----------------
  # Shoe Search에서 고른 사진을 그대로 설명한다 (같은 res()를 사용)
  nav_panel("Explain PCA",
    uiOutput("explain_photo"),     # "지금 설명 중인 사진: ..." 안내
    layout_columns(col_widths = c(4, 8),   # 한 줄을 12칸으로 보고 4 : 8 로 나눔
      card(step_header(1, "Average shoe x̄"),
           plotOutput("s1_plot", height = "260px"),
           p(class = "note", "The pixel-by-pixel mean of all 2,000 photos. Every photo is described as a difference from this shoe.")),
      card(step_header(2, "Photo − average (x − x̄)"),
           plotOutput("s2_plot", height = "260px"),
           p(class = "note", HTML("<b style='color:#B2182B'>Red</b>: the photo is brighter than the average shoe. ",
                                  "<b style='color:#2166AC'>Blue</b>: darker. White: about the same.")))
    ),
    layout_columns(col_widths = c(7, 5),
      card(step_header(3, "Score map"),
           layout_columns(col_widths = c(6, 6),     # 축으로 쓸 PC 고르기 (기본 PC1, PC2)
             selectInput("pcx", "x axis", paste0("PC", 1:10), "PC1"),
             selectInput("pcy", "y axis", paste0("PC", 1:10), "PC2")),
           plotOutput("s3_plot", height = "430px"),
           p(class = "note", "Each dot is a catalogue photo. Black star: your photo. Black rings: its top 5 (found with all K PCs, so they can look far apart in only two PCs).")),
      card(step_header(4, "Eigenshoes and variance explained"),
           sliderInput("n_eig", "How many eigenshoes to draw", 4, 24, 12, 4, width = "100%"),
           plotOutput("s4_plot", height = "330px"),      # eigenshoe 그림들
           plotOutput("s4_scree", height = "200px"),     # PC별 설명분산 + 누적
           p(class = "note", "Loadings drawn as 64 × 64 pictures. Red pixels push the photo brighter when the score is positive, blue pixels darker."))
    ),
    layout_columns(col_widths = c(6, 6),
      card(step_header(5, "Scores: the numbers used to find the match"),
           plotOutput("s5_plot", height = "300px"),      # 내 사진 점수(막대) vs best match 점수(점)
           uiOutput("s5_formula"),                       # 점수·거리 공식 (MathJax 수식)
           div(class = "scroll-table", tableOutput("s5_table"))),
      card(step_header(6, "Rebuild with the first K eigenshoes"),
           plotOutput("s6_plot", height = "220px"),      # K = 1, 5, 10, 20, 50, 100 (+현재 K) 복원
           plotOutput("s6_err", height = "200px"),       # K에 따른 복원 오차 곡선
           p(class = "note", "x̂ = x̄ + score₁·PC₁ + … + score_K·PC_K. More PCs, more detail; the error shrinks quickly at first, then slowly."))
    ),
    # 과제의 "And things that you want to add" 부분: 검색이 얼마나 잘 되는지
    card(card_header("More: how good is the search, and where does it fail?"),
      layout_columns(col_widths = c(6, 6),
        div(plotOutput("m_acc", height = "280px"),       # K별 정확도 곡선
            p(class = "note", "Leave-one-out: every catalogue photo is searched against the other 1,999 and predicted by the top-5 vote.")),
        div(h6(textOutput("m_conf_title")), tableOutput("m_conf"),   # 혼동행렬
            p(class = "note", "Rows: true category. Columns: predicted. The diagonal is correct."))),
      layout_columns(col_widths = c(12),
        div(h6("Average shoe of each category"), plotOutput("m_catmean", height = "200px")))   # 카테고리별 평균 신발
    )
  ),

  # ---------------- 탭 3. Our Shoes ----------------
  nav_panel("Our Shoes",
    card(card_header("Can the app recognize real shoes it has never seen?"),
      p("Each member photographed 3 of their own shoes on a plain background. Files are named Name_Category_n.jpg."),
      uiOutput("our_gallery")),    # 팀원별 사진 갤러리 (이름 + 카테고리)
    layout_sidebar(
      sidebar = sidebar(width = 300,
        selectInput("our_pick", "Choose a shoe", choices = setNames(seq_len(nrow(OUR)), OUR$label)),  # 신발 고르는 위젯
        sliderInput("K_our", "Number of PCs (K)", 1, KMAX, K_DEFAULT, 1),
        radioButtons("our_mirror", "Mirror the photo?", c("Auto", "No", "Yes"), inline = TRUE)),
      uiOutput("our_box"),         # 예측 맞음/틀림 + best match 카드
      card(card_header("Best match"), plotOutput("our_best", height = "260px")),
      card(card_header("Top 5"), plotOutput("our_top5", height = "200px"), tableOutput("our_top5_table")),
      card(card_header("All our shoes at this K"), tableOutput("our_all"))   # 전체 팀원 사진 정답 개수
    )
  ),

  # ---------------- 탭 4. Data 1 (데이터셋 미정 -> 비워 둠) ----------------
  nav_panel("Data 1",
    card(card_header("TODO: Project 1 dataset"),
         p("Descriptive results only. Question, inputs, result in numbers, why it matters, how it helps the next analysis."))
  ),

  # ---------------- 탭 5. AI (템플릿 형식 그대로 유지해야 함) ----------------
  nav_panel("AI",
    card(
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
)

# -----------------------------------------------------------------------------
# 4. Server (계산)
#    input$<id>  : 사용자가 위젯에서 고른 값
#    output$<id> : UI의 같은 id 자리에 들어갈 그림/표/글
#    reactive()  : 여러 출력이 같이 쓰는 중간 계산. 입력이 바뀔 때만 다시 계산된다.
#    bindCache() : 같은 사진·같은 K로 다시 오면 그림을 새로 그리지 않고 저장본을 재사용 (속도)
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  # "Random id" 버튼을 누르면 카탈로그 id 칸에 무작위 숫자를 넣는다
  observeEvent(input$rand_id, updateNumericInput(session, "cat_id", value = sample(meta$id, 1)))
  K <- reactive(min(max(as.integer(input$K), 1L), KMAX))   # K를 1~100 사이 정수로 보정

  # [query] 지금 고른 사진을 숫자 4,096개로 + 정답, 이름, 캐시 키
  query <- reactive({
    if (input$src == "Catalogue") {
      id <- suppressWarnings(as.integer(input$cat_id))
      validate(need(!is.na(id) && id >= 1 && id <= 2000, "Enter a catalogue id between 1 and 2000."))  # 잘못된 id면 안내문
      list(x = X[id, ], label = paste("Catalogue id", id), truth = as.character(meta$category[id]),
           exclude = id,                        # 자기 자신은 검색에서 제외 (과제 요구사항)
           mirror = "No", key = paste0("cat", id))
    } else if (input$src == "Self-check") {
      lab <- read.csv("selfcheck/labels.csv")   # self-check 정답표
      f <- input$sc_file
      list(x = photo_to_vec(file.path("selfcheck", f), crop = FALSE, clean = FALSE), label = f,
           truth = lab$category[lab$photo == f], exclude = NULL, mirror = "No", key = f)
    } else {
      validate(need(!is.null(input$photo), "Upload a JPG or PNG photo of one shoe."))
      list(x = photo_to_vec(input$photo$datapath), label = input$photo$name, truth = input$up_truth,
           exclude = NULL, mirror = input$up_mirror,
           key = paste0("up", unname(tools::md5sum(input$photo$datapath))))   # 파일 내용으로 만든 고유 키
    }
  })
  # [res] 검색 결과. Shoe Search와 Explain PCA의 모든 출력이 이것 하나를 같이 쓴다.
  res <- reactive({
    q <- query()
    search_oriented(q$x, K(), q$mirror, q$exclude)
  })
  ckey <- reactive(paste(query()$key, query()$mirror, K()))   # 그림 캐시용 키 = 사진 + 방향 + K

  # ---------------- Shoe Search 출력 ----------------
  output$k_info <- renderUI({
    k <- K()
    p(class = "note", sprintf("First %d PCs explain %.1f%% of the variance. Catalogue accuracy at this K: %.1f%%.",
                              k, 100 * sum(P$var_exp[1:k]), 100 * P$loo_acc[k]))
  })

  output$search_boxes <- renderUI({
    r <- res(); q <- query()
    layout_columns(col_widths = c(4, 4, 4),
      result_box(r$pred, q$truth),
      value_box(title = "Best match", value = sprintf("id %d", r$top$id[1]),
                p(sprintf("%s · distance %.1f", r$top$Category[1], r$top$Distance[1])), theme = "primary"),
      value_box(title = "Your photo", value = q$label, theme = "light",
                p(sprintf("K = %d%s", K(), if (isTRUE(r$mirrored)) " · mirrored" else ""))))
  })

  output$best_plot <- renderPlot({
    r <- res(); b <- r$top$id[1]
    tile_par(4)
    tile(gray_raster(r$x), "Your photo", query()$truth)
    tile(gray_raster(mu), "Average shoe", "x̄")
    tile(gray_raster(r$rebuilt), sprintf("Rebuilt with K = %d", K()), "what the search sees")
    tile(gray_raster(X[b, ]), sprintf("Best match: id %d", b), r$top$Category[1], border = CAT_COL[r$top$Category[1]])
  }) |> bindCache(ckey())

  output$top5_plot  <- renderPlot(top5_plot(res())) |> bindCache(ckey())
  output$top5_table <- renderTable({
    t <- res()$top
    # 카테고리 글자를 카테고리 색으로 (HTML 그대로 출력하려고 sanitize.text.function = identity)
    t$Category <- sprintf("<span style='color:%s;font-weight:600'>%s</span>", CAT_COL[t$Category], t$Category)
    t
  }, sanitize.text.function = identity, digits = 1, width = "100%")

  # ---------------- Explain PCA 출력 ----------------
  output$explain_photo <- renderUI({
    q <- query()
    div(class = "alert alert-light", sprintf("Explaining the photo chosen in Shoe Search: %s (K = %d). Change it there.",
                                              q$label, K()))
  })

  # Step 1. 평균 신발
  output$s1_plot <- renderPlot({ tile_par(1); tile(gray_raster(mu), "x̄ (mean of 2,000 photos)") })

  # Step 2. 사진 − 평균: 빨강 = 평균보다 밝음, 파랑 = 어두움
  output$s2_plot <- renderPlot({
    r <- res(); dif <- r$x - mu
    tile_par(3)
    tile(gray_raster(r$x), "Photo x")
    tile(gray_raster(mu), "Average x̄")
    tile(div_raster(dif, 255), "x − x̄")          # 색 범위를 ±255로 고정해 사진끼리 비교 가능
  }) |> bindCache(ckey())

  # Step 3. 점수 지도: 카탈로그 2,000장을 고른 두 PC 점수로 찍고, 내 사진(별)과 Top 5(동그라미) 표시
  output$s3_plot <- renderPlot({
    r <- res()
    i <- as.integer(sub("PC", "", input$pcx)); j <- as.integer(sub("PC", "", input$pcy))
    q_full <- drop(crossprod(V[, c(i, j)], r$x - mu))          # 내 사진의 PCi, PCj 점수
    df  <- data.frame(a = S[, i], b = S[, j], Category = meta$category)
    top <- df[match(r$top$id, meta$id), ]
    ggplot(df, aes(a, b, color = Category)) +
      geom_point(alpha = 0.45, size = 1.3) +
      geom_point(data = top, shape = 21, size = 4.5, stroke = 1.1, color = "black", fill = NA) +
      annotate("point", x = q_full[1], y = q_full[2], shape = 8, size = 6, stroke = 1.8) +
      annotate("label", x = q_full[1], y = q_full[2], label = "your photo", vjust = -1.1, size = 3.5) +
      scale_color_manual(values = CAT_COL) +
      labs(x = sprintf("%s score (%.1f%% of variance)", input$pcx, 100 * P$var_exp[i]),
           y = sprintf("%s score (%.1f%% of variance)", input$pcy, 100 * P$var_exp[j]), color = NULL) +
      theme_minimal(base_size = 13) + theme(legend.position = "top")
  }) |> bindCache(ckey(), input$pcx, input$pcy)

  # Step 4-1. eigenshoes: 각 PC의 적재벡터(4,096개)를 64 x 64 그림으로. 현재 K 안의 PC는 파란 테두리.
  output$s4_plot <- renderPlot({
    n <- input$n_eig; nc <- min(n, 6)
    tile_par(nc, ceiling(n / nc))
    for (k in 1:n) tile(div_raster(V[, k]), sprintf("PC%d · %.1f%%", k, 100 * P$var_exp[k]),
                        border = if (k <= K()) "#2F5D7C" else "grey85", cex = 0.95)
  }) |> bindCache(input$n_eig, K())

  # Step 4-2. 설명분산: 막대 = PC 하나의 %, 선 = 누적 %, 점선 = 현재 K
  output$s4_scree <- renderPlot({
    df <- data.frame(k = 1:KMAX, v = 100 * P$var_exp[1:KMAX], cum = 100 * cumsum(P$var_exp)[1:KMAX])
    ggplot(df, aes(k)) +
      geom_col(aes(y = v * 3), fill = "#9FB6C6", width = 0.8) +        # 막대는 3배로 키워 오른쪽 축(÷3)에 맞춤
      geom_line(aes(y = cum), color = "#2F5D7C", linewidth = 1) +
      geom_vline(xintercept = K(), linetype = 2) +
      annotate("label", x = K(), y = 95, label = sprintf("K = %d: %.1f%%", K(), df$cum[K()]), hjust = -0.05, size = 3.3) +
      scale_y_continuous("Cumulative %", limits = c(0, 100), sec.axis = sec_axis(~ . / 3, name = "Each PC %")) +
      labs(x = "Principal component") + theme_minimal(base_size = 12)
  }) |> bindCache(K())

  # Step 5. 점수: 내 사진(막대)과 best match(빨간 점)의 PC1~PCK 점수. 차이가 작을수록 가깝다.
  output$s5_plot <- renderPlot({
    r <- res(); k <- K(); b <- r$best
    df <- data.frame(PC = factor(1:k), photo = r$q, match = S[b, 1:k])
    ggplot(df, aes(PC)) +
      geom_col(aes(y = photo, fill = "Your photo"), width = 0.75) +
      geom_point(aes(y = match, color = "Best match"), size = 2.6) +
      scale_fill_manual(NULL, values = "#9FB6C6") + scale_color_manual(NULL, values = "#C62828") +
      scale_x_discrete(breaks = if (k > 30) seq(5, k, 5) else waiver()) +   # K가 크면 눈금을 5개마다
      labs(x = "PC", y = "Score") + theme_minimal(base_size = 12) + theme(legend.position = "top")
  }) |> bindCache(ckey())

  output$s5_formula <- renderUI({
    r <- res()
    withMathJax(p(class = "note",
      sprintf("Score\\(_k = (x - \\bar x)^\\top v_k\\). Distance to best match = \\(\\sqrt{\\sum_{k=1}^{%d}(\\text{score}_k - \\text{match}_k)^2}\\) = %.1f. The 5 smallest distances among all catalogue photos are the top 5.",
              K(), r$top$Distance[1])))
  })

  output$s5_table <- renderTable({
    r <- res(); k <- K()
    data.frame(PC = paste0("PC", 1:k), `Your photo` = r$q, `Best match` = S[r$best, 1:k],
               Difference = r$q - S[r$best, 1:k], check.names = FALSE)
  }, digits = 1, width = "100%")

  # Step 6. 복원: 여러 K로 x̂ = x̄ + Σ score_k · v_k 를 그리고 RMSE(원본과의 평균 오차) 표시
  rebuild_ks <- reactive(sort(unique(c(1, 5, 10, 20, 50, 100, K()))))
  output$s6_plot <- renderPlot({
    r <- res(); ks <- rebuild_ks()
    tile_par(length(ks) + 1)
    tile(gray_raster(r$x), "Original")
    for (k in ks) {
      xh <- drop(mu + V[, 1:k, drop = FALSE] %*% crossprod(V[, 1:k, drop = FALSE], r$x - mu))
      tile(gray_raster(xh), sprintf("K = %d", k), sprintf("RMSE %.1f", sqrt(mean((r$x - xh)^2))),
           border = if (k == K()) "#2F5D7C" else "grey80")
    }
  }) |> bindCache(ckey())

  output$s6_err <- renderPlot({
    r <- res(); z <- crossprod(V, r$x - mu)                       # PC 100개 점수 한 번에
    rmse <- vapply(1:KMAX, function(k) sqrt(mean((r$x - mu - V[, 1:k, drop = FALSE] %*% z[1:k])^2)), 0)
    df <- data.frame(k = 1:KMAX, rmse = rmse)
    ggplot(df, aes(k, rmse)) + geom_line(color = "#2F5D7C", linewidth = 1) +
      geom_point(data = df[K(), ], color = "#C62828", size = 3) +
      labs(x = "K", y = "Rebuild error (RMSE, gray levels)") + theme_minimal(base_size = 12)
  }) |> bindCache(ckey())

  # More 1. K별 정확도 (prep.R에서 미리 계산한 leave-one-out 결과)
  output$m_acc <- renderPlot({
    df <- data.frame(k = 1:KMAX, acc = 100 * P$loo_acc)
    kb <- which.max(P$loo_acc)
    ggplot(df, aes(k, acc)) + geom_line(color = "#2F5D7C", linewidth = 1) +
      geom_point(data = df[K(), ], color = "#C62828", size = 3) +
      annotate("label", x = K(), y = df$acc[K()], label = sprintf("K = %d: %.1f%%", K(), df$acc[K()]),
               vjust = 1.5, size = 3.4) +
      geom_hline(yintercept = 25, linetype = 3) +                  # 무작위로 찍으면 25%
      annotate("text", x = KMAX, y = 27, label = "guessing = 25%", hjust = 1, size = 3.2) +
      labs(x = "K", y = "Leave-one-out accuracy (%)",
           subtitle = sprintf("Best: %.1f%% at K = %d", 100 * P$loo_acc[kb], kb)) +
      theme_minimal(base_size = 12)
  }) |> bindCache(K())

  # More 2. 혼동행렬: 행 = 실제, 열 = 예측, 마지막 열 = 카테고리별 재현율(맞힌 비율)
  output$m_conf_title <- renderText(sprintf("Confusion matrix at K = %d (accuracy %.1f%%)", K(), 100 * P$loo_acc[K()]))
  output$m_conf <- renderTable({
    tb <- table(True = meta$category, Predicted = factor(CATS[P$loo_pred[, K()]], levels = CATS))
    m <- as.data.frame.matrix(tb)
    data.frame(True = rownames(m), m, `Recall %` = round(100 * diag(tb) / rowSums(tb), 1), check.names = FALSE)
  }, digits = 1, width = "100%")

  # More 3. 카테고리별 평균 신발 4장
  output$m_catmean <- renderPlot({
    tile_par(4)
    for (ct in CATS) tile(gray_raster(colMeans(X[meta$category == ct, ])), ct, "average of 500", border = CAT_COL[ct])
  })

  # ---------------- Our Shoes 출력 ----------------
  # 갤러리: 팀원마다 한 줄, 사진 카드 테두리 = 카테고리 색
  output$our_gallery <- renderUI({
    if (nrow(OUR) == 0)
      return(div(class = "alert alert-warning",
                 "No photos yet. Put them in the folder our_shoes/ named Name_Category_n.jpg (Category: Boots, Sandals, Shoes or Slippers)."))
    people <- unique(OUR$name)
    tagList(lapply(people, function(pn) {
      rows <- which(OUR$name == pn)
      cards <- lapply(rows, function(i) div(class = "our-card", style = sprintf("border-color:%s", CAT_COL[OUR$category[i]]),
        tags$img(src = paste0("ourthumbs/", OUR$file[i], ".png")),
        div(tags$b(OUR$category[i]), tags$small(paste0(" #", OUR$n[i])))))
      div(h6(pn, class = "mt-2"),
          do.call(layout_columns, c(list(col_widths = rep(2, length(rows))), cards)))
    }))
  })

  # 고른 팀원 신발의 검색 결과 (Shoe Search와 같은 함수, 다른 입력)
  our_res <- reactive({
    validate(need(nrow(OUR) > 0, "Add photos to our_shoes/ first."))
    i <- as.integer(input$our_pick)
    search_oriented(OUR_X[[i]], input$K_our, input$our_mirror)
  })
  our_key <- reactive(paste(input$our_pick, input$K_our, input$our_mirror))

  output$our_box <- renderUI({
    r <- our_res(); i <- as.integer(input$our_pick)
    layout_columns(col_widths = c(6, 6),
      result_box(r$pred, OUR$category[i]),
      value_box(title = "Best match", value = sprintf("id %d", r$top$id[1]),
                p(sprintf("%s · distance %.1f%s", r$top$Category[1], r$top$Distance[1],
                          if (r$mirrored) " · mirrored" else "")), theme = "primary"))
  })
  # 원본 사진 | 앱이 보는 64x64 | K개 PC로 복원 | best match
  output$our_best <- renderPlot({
    r <- our_res(); i <- as.integer(input$our_pick); b <- r$top$id[1]
    tile_par(4)
    plot.new(); plot.window(c(0, 1), c(0, 1), asp = 1)
    th <- image_read(file.path(THUMB_DIR, paste0(OUR$file[i], ".png")))
    inf <- image_info(th); w <- inf$width / max(inf$width, inf$height); h <- inf$height / max(inf$width, inf$height)
    rasterImage(as.raster(th), (1 - w) / 2, (1 - h) / 2, (1 + w) / 2, (1 + h) / 2)   # 원본 비율 유지
    text(0.5, 1.04, OUR$label[i], adj = c(0.5, 0), font = 2, xpd = NA)
    tile(gray_raster(r$x), "What the app sees", "64 × 64 gray")
    tile(gray_raster(r$rebuilt), sprintf("Rebuilt with K = %d", input$K_our))
    tile(gray_raster(X[b, ]), sprintf("Best match: id %d", b), r$top$Category[1], border = CAT_COL[r$top$Category[1]])
  }) |> bindCache(our_key())
  output$our_top5 <- renderPlot(top5_plot(our_res())) |> bindCache(our_key())
  output$our_top5_table <- renderTable(our_res()$top, digits = 1, width = "100%")

  # 팀원 사진 전체를 현재 K로 검색한 결과 표 + 맞힌 개수
  output$our_all <- renderTable({
    validate(need(nrow(OUR) > 0, ""))
    out <- lapply(seq_len(nrow(OUR)), function(i) {
      r <- search_oriented(OUR_X[[i]], input$K_our, input$our_mirror)
      data.frame(Member = OUR$name[i], Photo = OUR$file[i], True = OUR$category[i],
                 `Best match` = sprintf("id %d (%s)", r$top$id[1], r$top$Category[1]),
                 Predicted = r$pred, Right = ifelse(r$pred == OUR$category[i], "✓", "✗"),
                 check.names = FALSE)
    })
    out <- do.call(rbind, out)
    rbind(out, data.frame(Member = "Total", Photo = "", True = "", `Best match` = "", Predicted = "",
                          Right = sprintf("%d / %d", sum(out$Right == "✓"), nrow(out)), check.names = FALSE))
  }, width = "100%")

  # ---------------- AI 탭: 두 표를 채운다. 열 이름은 바꾸지 말 것 (템플릿 형식) ----------------
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

# ui와 server를 합쳐 앱 실행
shinyApp(ui, server)
