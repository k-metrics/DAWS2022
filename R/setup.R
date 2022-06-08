# quartoでは、セットアップ・チャンクが認識されないようなので、外に出してみる
require(tidyverse)

# knitr::opts_chunk$set(echo = TRUE,                 # Rコードを出力する
#                       warning = FALSE,             # Rの警告を抑止する
#                       fig_caption = TRUE,
#                       fig.align = 'center',        # 図を中央揃えにする
#                       # attr.source='.numberLines',  # Rコードに行番号表示
#                       out.width = "90%"            # 画像幅のデフォルト
# )

# 出力がPDF(LaTeX)の場合の追加設定
if (knitr::is_latex_output()) {
  knitr::opts_chunk$set(dev = "cairo_pdf",       # 図の出力にPDFデバイスを使う
                        # dev.args = list(family = "Noto Serif"),
                        dev.args = list(family = "Source Han Code JP"),
                        # 図のフォントを指定する
                        fig.pos = "H",           # 図をその場で表示させる
                        out.extra = "",
                        tinytex.verbose = TRUE   # デバッグ用メッセージを出力
  )
  
  # Update packages.bib file
  tidyverse::tidyverse_packages() %>% 
    c(., tidymodels::tidymodels_packages(), "knitr", "rmarkdown","bookdown",
      "DBI", "zoo", "htmlwidgets", "shiny", "modeest", "patchwork", "Rcmdr",
      "e1071", "statip") %>% 
    sort() %>% unique() %>% 
    knitr::write_bib("./bib/packages.bib")
  
  # ePUB形式の場合の追加設定
} else if (!knitr::is_html_output(excludes = "epub")) {
  knitr::opts_chunk$set(dev = "jpeg")            # 図の出力にJPEGデバイスを使う
}

# 図の描画位置調整については以下を参照
# https://gedevan-aleksizde.github.io/rmarkdown-cookbook/figure-placement.html#force-floats-forward
# opts_chunkで"fig.pos"を指定することで全てのチャンク出力に対して調整可能
# h: フロートをここ (here) に配置します. つまりソーステキスト上に現れるところとほぼ同じ位置です.
# t: そのページの先頭 (top) に配置します.
# b: そのページの末尾 (bottom) に配置します.
# p: フロート専用の特別なページに配置します.
# !: LaTex が「良い」フロートの位置を決定するための内部パラメータ上書きします.
# H: フロートを正確に LaTex コード上と同じ位置に配置します. float パッケージが必要です (\usepackage{float}).

htmltools::tagList(rmarkdown::html_dependency_font_awesome())


## 出力によりテーブル表記を変更する
# HTML format use rmarkdown::paged_table
# PDF/DOCX format use kableExtra package
#  https://haozhu233.github.io/kableExtra/
#  https://haozhu233.github.io/kableExtra/awesome_table_in_pdf.pdf
# WARNING:
# Should NOT exec df_print befor skimr::skim (has bug) 
# Do NOT require() or library() kableExtra package with using skimr package
df_print <- function(df = NULL,             # 表示させたいデータをフレームを指定
                     n = 3L,                # 省略表示時の行数
                     caption = NULL,        # 表見出しを指定
                     booktabs = TRUE,       # FALSEで縦罫線を描画
                     font_size = NULL,      # 表内文字のフォントサイズ
                     position = "center",   # 表の横位置
                     full_width = FALSE,    # TRUEで幅いっぱいに拡大表示
                     scale_down = FALSE,    # TRUEで横幅に収まるよう縮小表示
                     all = FALSE,           # TRUEですべてのデータを表示
                     head_tail = FALSE,     # TRUEで先頭・末尾をn行ずつ表示
                     ...) {                 # kableExtra::kblに渡す引数
  if (!is.null(df)) {
    if (knitr::is_latex_output()) {
      if (scale_down == FALSE) {
        # latex_option = c("striped", "HOLD_position")
        latex_option = c("striped", "HOLD_position")
      } else {
        # latex_option = c("striped", "HOLD_position", "scale_down")
        latex_option = c("striped", "HOLD_position", "scale_down")
      }
      if (all == FALSE) {
        df <- df %>% 
          psych::headTail(top = n,
                          bottom = dplyr::if_else(head_tail == FALSE, 0L, n))
      }
      df %>% 
        kableExtra::kbl(caption  = caption, booktabs = booktabs, ...) %>%
        kableExtra::kable_styling(font_size = font_size, position = position,
                                  full_width = full_width,
                                  latex_options = latex_option)
    } else {   # 出力タイプ不明の場合
      df %>% 
        rmarkdown::paged_table()
    }
  }
}



## 母分散を求める
VAR <- function(x) {
  sum((x - mean(x)) ^ 2) / length(x)
}

var.p <- function(x) {
  (length(x) - 1) / length(x) * var(x)
}


## 母標準偏差を求める
SD <- function(x) {
  # sqrt((length(x) - 1) / length(x)) * sd(x)
  sqrt(sum((x - mean(x)) ^ 2) / length(x))
}

sd.p <- function(x) {
  sqrt(var.p(x))
}



