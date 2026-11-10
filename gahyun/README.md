# 구가현 시안 (Project 2 Shiny)

팀 최종안이 아닌 개인 시안입니다. Part 1(Shoe Search, Explain PCA, Our Shoes)만 동작하고, Data 1과 AI 탭은 비어 있습니다.

## 실행 방법
1. 패키지 설치: `install.packages(c("shiny", "bslib", "magick", "ggplot2"))`
2. `P2_for_students.zip`을 풀어서 `shoes/`, `selfcheck/`, `shoes_meta.csv`를 이 폴더(`gahyun/`)에 넣습니다.
   과제 데이터는 공유 금지라서 레포에는 올리지 않았습니다(`.gitignore`).
3. RStudio에서 `gahyun/app.R`을 열고 **Run App**을 누릅니다.
   처음 실행할 때 `prep.R`이 PCA를 계산해 `pca_cache.rds`를 만듭니다(약 2.5분, 한 번만).

## 파일
- `app.R`: 앱 본체 (UI + server). 모든 줄에 한국어 주석이 있습니다.
- `prep.R`: PCA 사전 계산 → `pca_cache.rds`
- `our_shoes/`(직접 만들기): 팀원 신발 사진을 `이름_카테고리_번호.jpg`로 넣으면 Our Shoes 탭에 나옵니다.

자세한 설명서: https://claude.ai/artifact/GJKnuUDhuWJUY6sTmHafQs
