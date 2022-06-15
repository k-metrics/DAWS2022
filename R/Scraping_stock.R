# -----------------------------------------------------------------------------
# 株探のサイトから月次株価一覧を取得する
# -----------------------------------------------------------------------------
library(tidyverse)
library(rvest)

path <- "./data/"
date <- as.character(lubridate::today())

site_url <- "https://kabutan.jp/stock/kabuka?code=7203&ashi=mon&page="
pages <- c(1:5)
xpath <- '//*[@id="stock_kabuka_table"]/table[2]'

stock <- tidyr::crossing(site_url, pages) %>% 
  dplyr::mutate(url = paste0(site_url, pages)) %>% 
  dplyr::pull(url) %>% 
  purrr::map_df(.f = function(.x) {
    xml2::read_html(.x) %>% 
      rvest::html_element(xpath = xpath) %>%
      rvest::html_table(header = FALSE) %>%
      dplyr::slice(-1)
  }) %>% 
  dplyr::mutate(dplyr::across(.col = dplyr::everything(),
                              .f = readr::parse_guess)) %>% 
  dplyr::mutate(X1 = lubridate::as_date(X1)) %>% 
  dplyr::rename(`日付` = X1, `始値` = X2, `高値` = X3, `安値` = X4,
                `終値` = X5, `前月比` = X6, `月次収益率` = X7, `売買高` = X8)

stock %>% 
  df_print() %>% 
  readr::write_excel_csv(paste0(path, "株価の月次推移_", date, ".csv"))


# #| code-summary: "以前のやり方"
# #| include: false
# #| eval: false
# stock <- purrr::map2(site_url, pages, ~ paste0(.x, .y)) %>%
#   purrr::map2_df(pages, .f = function(.x, .y) {
#     xml2::read_html(.x) %>% 
#       rvest::html_element(xpath = xpath) %>%
#       rvest::html_table(header = FALSE) %>%
#       dplyr::slice(-1)
#   }
#   ) %>% 
#   dplyr::mutate_all(readr::parse_guess) %>% 
#   dplyr::mutate(X1 = lubridate::as_date(X1)) %>% 
#   dplyr::rename(`日付` = X1, `始値` = X2, `高値` = X3, `安値` = X4,
#                 `終値` = X5, `前月比` = X6, `月次収益率` = X7, `売買高` = X8)


url1 <- "https://kabutan.jp/stock/kabuka?code="
url2 <- "&ashi=mon&page="
pages <- c(1:5)
xpath <- '//*[@id="stock_kabuka_table"]/table[2]'
code = c(7201, 7203, 7211, 7261, 7267, 7270)

# 動作確認用
# url <-  paste0(url1, 7201, url2, 1)
# xml2::read_html(url) %>%
#   rvest::html_element(xpath = xpath) %>%
#   rvest::html_table()

stock <- tidyr::crossing(url1, code, url2, pages, xpath) %>% 
  dplyr::mutate(url = paste0(url1, code, url2, pages)) %>% 
  dplyr::select(url, xpath, code) %>% 
  purrr::pmap_df(.f = function(url, xpath, code) {
    xml2::read_html(url) %>% 
      rvest::html_element(xpath = xpath) %>% 
      rvest::html_table() %>% 
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>% 
      dplyr::mutate(code = code)
  })

stock %>% 
  df_print() %>% 
  readr::write_excel_csv(paste0(path, "メーカー６社の株価の月次推移_",
                                date, ".csv"))


