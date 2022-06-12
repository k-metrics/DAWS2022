# その他試行錯誤中

require(tidyverse)
require(rvest)

# -----------------------------------------------------------------------------
# フィデリティ証券は日興証券と同じコードが取得できる
# -----------------------------------------------------------------------------
# 
# url <- "https://www.fidelity.jp/fund-guide/fund-search/?ifShowCompare=FALSE&ifShowAllFunds=FALSE&removeCondition=&orders=&currentPage=1&tabName=basicInfo&requestApp=fullFundList&fundAssociationIdList=&sbsFlag=&scrollHeight=&keyword=&openedFilter=fundCategoryIdImg%3AareaIdImg%3Aundefined%3Aundefined%3AjyCntSliderImg%3AcompanyIdImg%3AaccumulationNisaFlagImg%3A&filtersStr=accumulationNisaFlagId%3A1%24"
# 
# xpath <- '//*[@id="basicInfo"]/div[1]/div[3]/table'
# 
# url %>% 
#   xml2::read_html() %>% 
#   rvest::html_element(xpath = xpath) %>% 
#   # rvest::html_table()
#   rvest::html_nodes("a") %>% 
#   rvest::html_attr("href") 


  
# -----------------------------------------------------------------------------
# セッションによる動的方法を試してみる
# -----------------------------------------------------------------------------
# url <- "https://toushin-lib.fwg.ne.jp/FdsWeb/FDST999903"
# 
# top_page <- "https://toushin-lib.fwg.ne.jp/FdsWeb/FDST000000"
# xpath <- '//*[@id="tab_content1_table"]'
# xpath <- '//*[@id="tab_content1_table"]/tbody'
# 
# rvest::read_html(url) %>% 
#   rvest::html_nodes("table")
# 
# 
# session <- rvest::session(top_page) %>% 
#   rvest::session_follow_link(xpath = '/html/body/header[1]/div[2]/nav/ul/li[4]/a')
# 
# session %>% 
#   xml2::read_html() %>% 
#   rvest::html_element(xpath = xpath) %>% 
#   rvest::html_table()
# 
# httr::POST(url)
# 
# httr::GET(url) %>% 
#   xml2::read_html() %>% 
#   rvest::html_element(xpath = xpath) %>% 
#   rvest::html_table()


# -----------------------------------------------------------------------------
# 日興証券の場合
# -----------------------------------------------------------------------------
# SMBC日興証券は取扱ファンド一覧からカテゴリごとに商品を選択できますが、
# リターン、シャープレシオの期間が異なっています。
# 
# url <- "https://fund2.smbcnikko.co.jp/smbc_nikko_fund/qsearch.exe?F=detail_kokunai1&KEY1="
# code <- c("9C311125", "39312149", "9K31419A", "32315984", "4931112B",
#           "3231203C", "0431218A", "39312149", "42311081", "79314144")
# 
# 
# #| code-summary: "累積リターン（利回り）"
# xpath <- '//*[@id="main"]/section/section[3]/section[2]/div/div[1]/div/table'
# 
# return <- tidyr::crossing(url, code) %>% 
#   dplyr::mutate(site = paste0(url, code)) %>% 
#   dplyr::select(code, site) %>% 
#   purrr::pmap_df(.f = function (code, site) {
#     xml2::read_html(site) %>% 
#       rvest::html_element(xpath = xpath) %>% 
#       rvest::html_table(header = FALSE) %>% 
#       dplyr::slice(-1) %>% 
#       dplyr::mutate(code = code)
#   })
# 
# return %>% 
#   dplyr::select(-(X5:X8)) %>% 
#   tidyr::unite("Y1", X1, X2) %>% 
#   tidyr::unite("Y2", X3, X4) %>% 
#   tidyr::pivot_longer(cols = dplyr::starts_with("Y"),
#                       names_to = "name", values_to = "value") %>% 
#   tidyr::separate(value, into = c("期間", "value"), sep = "_") %>% 
#   dplyr::mutate(value = stringr::str_remove_all(value, pattern = "[+%]"),
#                 value = dplyr::na_if(value, "--"),
#                 value = as.numeric(value)) %>% 
#   dplyr::select(-name) %>% 
#   tidyr::pivot_wider(names_from = 期間, values_from = value)
# 
# 
# 
# #| code-summary: "シャープレシオ"
# xpath <- '//*[@id="main"]/section/section[3]/section[2]/div/div[2]/div/table'
# 
# sr <- tidyr::crossing(url, code) %>% 
#   dplyr::mutate(site = paste0(url, code)) %>% 
#   dplyr::select(code, site) %>% 
#   purrr::pmap_df(.f = function (code, site) {
#     xml2::read_html(site) %>% 
#       rvest::html_element(xpath = xpath) %>% 
#       rvest::html_table(header = FALSE) %>% 
#       dplyr::slice(-1) %>% 
#       dplyr::mutate(code = code)
#   })
# 
# sr %>% 
#   dplyr::rename(期間 = X1, SR = X2, SD = X3, 基準日 = X4) %>% 
#   dplyr::mutate(SR = dplyr::na_if(SR, "--"), SD = dplyr::na_if(SD, "--")) %>% 
#   dplyr::mutate(SR = as.numeric(SR), SD = as.numeric(SD)) %>% 
#   dplyr::select(code, 期間, SR, SD, 基準日)
# 
# 
# 
# #| code-summary: "決算実績"
# xpath = '//*[@id="main"]/section/section[3]/section[3]/div/div[2]/div/table'
# 
# result <- tidyr::crossing(url, code) %>% 
#   dplyr::mutate(site = paste0(url, code)) %>% 
#   dplyr::select(code, site) %>% 
#   purrr::pmap_df(.f = function (code, site) {
#     xml2::read_html(site) %>% 
#       rvest::html_element(xpath = xpath) %>% 
#       rvest::html_table(header = FALSE) %>% 
#       dplyr::mutate(code = code) %>% 
#       dplyr::slice(-(1:2))
#   })
# 
# result %>% 
#   dplyr::mutate(dplyr::across(.cols = X1:X4, 
#                               ~ dplyr::na_if(.x, "--")))




