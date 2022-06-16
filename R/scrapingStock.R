# -----------------------------------------------------------------------------
# 株探のサイトから任意の一銘柄の月次株価一覧を取得する
# -----------------------------------------------------------------------------
library(tidyverse)
library(rvest)

path <- "./data/"
date <- as.character(lubridate::today())

url <- "https://kabutan.jp/stock/kabuka?code=7203&ashi=mon&page="
pages <- c(1:5)
xpath <- '//*[@id="stock_kabuka_table"]/table[2]'

stock <- tidyr::crossing(url, pages, xpath) %>% 
  dplyr::mutate(url = paste0(url, pages)) %>% 
  dplyr::select(url, xpath) %>% 
  purrr::pmap_df(.f = function(url, xpath) {
    xml2::read_html(url) %>% 
      rvest::html_element(xpath = xpath) %>% 
      rvest::html_table() %>% 
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  })

stock %>% 
  df_print() %>% 
  readr::write_excel_csv(paste0(path, "株価の月次推移_", date, ".csv"))


# -----------------------------------------------------------------------------
# 株探のサイトから複数銘柄の月次株価一覧を取得する
# -----------------------------------------------------------------------------
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

