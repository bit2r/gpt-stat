suppressWarnings(suppressMessages({

  extrafont::loadfonts()

  # 1. ggplot 그래프 ------------------------
  extrafont::loadfonts("win")

  ## 테마 (글꼴) -----------------------------
  theme_korean <- function() {

    # ggthemes::theme_tufte() +
    ggplot2::theme_minimal() +

      ggplot2::theme(
        plot.title     = ggplot2::element_text(family = "NanumSquare", size = 18, face = "bold"),
        plot.subtitle  = ggplot2::element_text(family = "MaruBuri", size = 13),
        axis.title.x   = ggplot2::element_text(family = "MaruBuri"),
        axis.title.y   = ggplot2::element_text(family = "MaruBuri"),
        axis.text.x    = ggplot2::element_text(family = "MaruBuri", size = 11),
        axis.text.y    = ggplot2::element_text(family = "MaruBuri", size = 11),
        legend.title   = ggplot2::element_text(family = "MaruBuri", size=13),
        plot.caption   = ggplot2::element_text(family = "NanumSquare", color = "gray20")
      )
  }

  ## 색상
  ### 정당
  민주당_2색상 <- c("#00A0E2", "#004EA1")
  민주당_4색상 <- c("#8AC452", "#00AA7D", "#008CCD", "#004EA1")

  국힘_3색상 <- c("#FFFFFF", "#E61E2B", "#00B5E2")
  국힘_6색상 <- c("#EDB19D", "#F18070", "#BDE4F8", "#004C7E", "#112C56")

  정의당_3색상 <- c("#ffed00", "#e8306d", "#00a366", "#623e91")

  무소속_색상 <- "#999999"

  # 정당, 색상코드, 시각화
  party_name_palette <- c("민주당" = 민주당_2색상[2],
                          "국민의힘" = 국힘_3색상[2],
                          "정의당" = 정의당_3색상[1],
                          "무소속" = 무소속_색상)
  party_palette <- c(민주당_2색상[2], 국힘_3색상[2], 정의당_3색상[1], 무소속_색상)

  ### 웨스 앤더슨
  color_palette <- wesanderson::wes_palette("Darjeeling1", n = 5)

  ggplot2::theme_set(theme_korean())

  # 2. gt 표 ------------------------
  library(gt)
  library(gtExtras)

  gt_theme_hangul <- function(gt_tbl) {

    # Grab number of rows of data from gt object
    n_rows <- nrow(gt_tbl$`_data`)

    gt_tbl |>
      gt_theme_538() |>
      tab_options(
        # column_labels.background.color = '#1E61B0', # R logo 파란색
        table.font.names ="NanumSquare",
        heading.title.font.size = px(26),
        heading.subtitle.font.size = px(16),
        heading.background.color = "transparent",
        column_labels.font.weight = 'bold',
        table_body.hlines.width = px(0),
        data_row.padding = px(6),
        heading.align = 'center',
        stub.background.color = "#ffffff",
        stub.font.weight = "bold",
        source_notes.font.size = px(10),
        row.striping.include_table_body = FALSE
      ) |>
      cols_align( align = "center", columns = where(is.numeric)) |>
      cols_align( align = "auto", columns = where(is.character)) |>
      ## 글꼴 달리 적용
      tab_style(
        style = cell_text(
          font = "MaruBuri",
          weight = 'bold'
        ),
        locations = cells_title(groups = 'subtitle')
      ) |>
      tab_style(
        style = cell_text(
          font = "MaruBuri",
          weight = 'bold'
        ),
        locations = cells_body()
      ) |>
      tab_style(
        style = cell_text(
          font = "MaruBuri",
          weight = 'bold'
        ),
        locations = cells_column_labels()
      ) |>
      tab_style(
        style = cell_text(
          font = "MaruBuri",
        ),
        locations = cells_source_notes()
      ) |>
      tab_style(
        style = cell_fill(color = 'grey90'),
        locations = cells_body(rows = seq(1, n_rows, 2))
      )
  }

  # 3. 그래프 표 저장 ------------------------




}))
