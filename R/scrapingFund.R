# 金融商品情報のスクレイピング

require(tidyverse)
require(rvest)

interval <- 1.0            # スクレイピング実行の間隔 [sec]
path <- "./data/sample/"   # ファイルの保存先
date <- as.character(lubridate::today())



# -----------------------------------------------------------------------------
# 協会コード
#   投信協会ライブラリーならびに楽天証券で利用してる"JP"から始まるコード体系と
#   SMBC日興証券やフィデリティ証券で利用している数字から始まるコード体系が
#   存在する。
#   例）ひふみプラス：　投信協会（JP90C0008CH5）、日興証券（9C311125）
# 
# データ作成手順
# 1) 楽天証券のつみたてNISA取扱商品のページにある一覧表のリンクから
#    投信協会JPコード（仮称）を取得する
#     https://www.rakuten-sec.co.jp/nisa/tsumitate/products.html
# 2）取得したJPコードを元に投信総合検索ライブラリーからファンドに
#    関するファンドの基本情報をスクレイピングする
#     https://toushin-lib.fwg.ne.jp/FdsWeb/FDST030000?isinCd=JPxxxxxxxxxx
# 3) 取得したJPコードを元に価格コムから投信協会ファンドコードを
#    取得する
#     https://kakaku.com/fund/detail.asp?si_isin=JPxxxxxxxxxx
# 4) 取得した情報を結合して基本情報を作成する
# 5) 取得したJPコードを元に投信総合検索ライブラリーからファンドの
#    運用情報を取得しファイルを作成する

# -----------------------------------------------------------------------------
# 楽天証券から協会コード（JPコード）を取得する
# -----------------------------------------------------------------------------
# つみたてNISA取扱商品のページ
url <- "https://www.rakuten-sec.co.jp/nisa/tsumitate/products.html"
# ファンド名（商品名）一覧のテーブル
xpath <- '//*[@id="str-main"]/div[4]/table'


# ファンド名（商品名）一覧テーブルを読み込み、ファンド名欄にあるリンク情報から
# 協会コードを取得する
nisa_code <- xml2::read_html(url) %>% 
  rvest::html_element(xpath = xpath) %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  tibble::as_tibble() %>% 
  dplyr::rename(code = value) %>% 
  dplyr::mutate(code = stringr::str_sub(code, start = 22))

# ファンド名（商品名）一覧テーブルを読み込む
nisa_name <- xml2::read_html(url) %>% 
  rvest::html_element(xpath = xpath) %>% 
  rvest::html_table() %>% 
  dplyr::select(-1)

# 協会コードとファンド名を単純結合する
nisa <- nisa_code %>% 
  dplyr::bind_cols(nisa_name)

# 取得結果を保存する
# nisa %>% 
#   readr::write_excel_csv(paste0(path, "つみたてNISA_取扱商品一覧_",
#                                 as.character(lubridate::today()),
#                                 ".csv"))



# -----------------------------------------------------------------------------
# 投信協会のライブラリから情報を取得する
# -----------------------------------------------------------------------------
# 投信協会の商品情報ページ
url <- "https://toushin-lib.fwg.ne.jp/FdsWeb/FDST030000?isinCd="
# 協会JPコード一覧
code <- nisa_code$code

# -----------------------------------------------------------------------------
# ファンド名（商品名）の取得
# ファンド名テーブル
xpath <- '/html/body/div[4]/div/div/div[1]/div[1]/h3'

# 動作確認用
# xml2::read_html(paste0(url, "JP90C00009X9")) %>%
#   rvest::html_element(xpath = xpath) %>%
#   rvest::html_text() %>%
#   stringr::str_remove_all("[\n\t]")

# ファンド名（商品名）の取得
nisa_fund <- tidyr::crossing(url, code, xpath) %>% 
  dplyr::mutate(url = paste0(url, code)) %>% 
  purrr::pmap_df(.f = function (url, xpath, code) {
    Sys.sleep(interval)
    xml2::read_html(url) %>% 
      rvest::html_element(xpath = xpath) %>% 
      rvest::html_text() %>% 
      stringr::str_remove_all("[\n\t]") %>% 
      tibble::as_tibble() %>% 
      dplyr::mutate(code = code)
  }) %>% 
  dplyr::rename(fund = value)

nisa_fund



# -----------------------------------------------------------------------------
# 運用会社名の取得
# 運用会社名
xpath <- '/html/body/div[4]/div/div/div[1]/div[1]/div[2]'

# 運用会社名の取得動作確認用
# xml2::read_html(paste0(url, "JP90C0002H06")) %>%
#   rvest::html_element(xpath = xpath) %>%
#   rvest::html_text() %>% 
#   stringr::str_remove_all("[\n\t[:space:]]") %>% 
#   stringr::str_remove("運用会社名：")

nisa_inves <- tidyr::crossing(url, code, xpath) %>% 
  dplyr::mutate(url = paste0(url, code)) %>% 
  purrr::pmap_df(.f = function (url, xpath, code) {
    Sys.sleep(interval)
    xml2::read_html(url) %>% 
      rvest::html_element(xpath = xpath) %>% 
      rvest::html_text() %>% 
      stringr::str_remove_all("[\n\t[:space:]]") %>% 
      stringr::str_remove("運用会社名：") %>% 
      tibble::as_tibble() %>% 
      dplyr::mutate(code = code)
  }) %>% 
  dplyr::rename(inves = value)

nisa_inves



# -----------------------------------------------------------------------------
# 商品分類の取得
# 商品名欄
xpath <- '/html/body/div[4]/div/div/div[2]/div[4]/div[2]'

# 商品名の取得動作確認用
# xml2::read_html(paste0(url, "JP90C0002H06")) %>%
#   rvest::html_element(xpath = xpath) %>%
#   rvest::html_text()

nisa_type <- tidyr::crossing(url, code, xpath) %>% 
  dplyr::mutate(url = paste0(url, code)) %>% 
  purrr::pmap_df(.f = function (url, xpath, code) {
    Sys.sleep(interval)
    xml2::read_html(url) %>% 
      rvest::html_element(xpath = xpath) %>% 
      rvest::html_text() %>% 
      stringr::str_remove_all("[\n\t[:space:]]") %>% 
      tibble::as_tibble() %>% 
      dplyr::mutate(code = code)
  }) %>% 
  dplyr::rename(type = value)

nisa_type



# -----------------------------------------------------------------------------
# 手数料・運用管理費情報
xpath <- '//*[@id="tab_content1"]/div/div[1]/div/div[1]/table'

# 手数料・運用管理費取得動作確認用
# xml2::read_html(paste0(url, "JP90C000EMH6")) %>%
#   rvest::html_element(xpath = xpath) %>% 
#   rvest::html_table()

nisa_fee <- tidyr::crossing(url, code, xpath) %>% 
  dplyr::mutate(url = paste0(url, code)) %>% 
  purrr::pmap_df(.f = function (url, xpath, code) {
    Sys.sleep(interval)
    xml2::read_html(url) %>% 
      rvest::html_element(xpath = xpath) %>% 
      rvest::html_table(header = TRUE) %>%
      dplyr::mutate(code = code)
  })


nisa_fee



# -----------------------------------------------------------------------------
# 基本情報
xpath <- '//*[@id="tab_content1"]/div/table'

# 動作確認用
# xml2::read_html(paste0(url, "JP90C0001QZ2")) %>%
#   rvest::html_element(xpath = xpath) %>%
#   rvest::html_table()

nisa_base <- tidyr::crossing(url, code, xpath) %>% 
  dplyr::mutate(url = paste0(url, code)) %>% 
  purrr::pmap_df(.f = function (url, xpath, code) {
    Sys.sleep(interval)
    xml2::read_html(url) %>% 
      rvest::html_element(xpath = xpath) %>% 
      rvest::html_table() %>%
      dplyr::mutate(code = code)
  }) %>% 
  dplyr::mutate(X1 = stringr::str_remove_all(X1, "[\n\t[:space:]]"),
                X2 = stringr::str_remove_all(X2, "[\n\t]")) %>% 
  tidyr::pivot_wider(names_from = X1, values_from = X2)


nisa_base



# -----------------------------------------------------------------------------
# 価格コムからファンドコードを取得する
# -----------------------------------------------------------------------------
url <- "https://kakaku.com/fund/detail.asp?si_isin="
xpath <- '//*[@id="mainLeft"]/div[5]/p'

# 動作確認用
# url %>% 
#   xml2::read_html() %>% 
#   rvest::html_element(xpath = xpath) %>% 
#   rvest::html_text()


kakakucom_code <- tidyr::crossing(url, code, xpath) %>% 
  dplyr::mutate(url = paste0(url, code)) %>% 
  purrr::pmap_df(.f = function (url, xpath, code) {
    Sys.sleep(interval)
    xml2::read_html(url) %>% 
      rvest::html_element(xpath = xpath) %>% 
      rvest::html_text() %>% 
      stringr::str_remove("投信協会ファンドコード：") %>%
      tibble::as_tibble() %>% 
      dplyr::mutate(code = code)
  }) %>% 
  dplyr::rename(fundcode = value)

kakakucom_code



# -----------------------------------------------------------------------------
# 基本情報の作成
# -----------------------------------------------------------------------------
# 基本情報の組み立て
nisa_info <- nisa_fund %>% 
  dplyr::left_join(kakakucom_code, by = "code") %>% 
  dplyr::left_join(nisa_base, by = "code") %>% 
  dplyr::left_join(nisa_type, by = "code") %>% 
  dplyr::left_join(nisa_inves, by = "code") %>% 
  dplyr::left_join(nisa_fee, by = "code") %>% 
  dplyr::select(code, dplyr::everything())

# 結果を保存する
nisa_info %>% 
  readr::write_excel_csv(paste0(path, "つみたてNISA_基本情報_",
                                date, ".csv"))



# -----------------------------------------------------------------------------
# 投信協会のライブラリから騰落率・リターン・シャープレシオを取得する
# -----------------------------------------------------------------------------
# 商品情報ページ（ファンド詳細）
url <- "https://toushin-lib.fwg.ne.jp/FdsWeb/FDST030000?isinCd="
# 運用情報
xpath <- c('//*[@id="tab_content2"]/div/div[2]/div[1]/div[1]/div/table',
           '//*[@id="tab_content2"]/div/div[2]/div[1]/div[2]/div/table',
           '//*[@id="tab_content2"]/div/div[2]/div[1]/div[3]/div/table')

nisa_rrs <- tidyr::crossing(url, code, xpath) %>% 
  dplyr::mutate(url = paste0(url, code)) %>% 
  purrr::pmap_df(.f = function (url, xpath, code) {
    Sys.sleep(interval)
    xml2::read_html(url) %>% 
      rvest::html_element(xpath = xpath) %>% 
      rvest::html_table(header = FALSE) %>% 
      dplyr::slice(-(1:2)) %>% 
      dplyr::mutate(code = code)
  })

nisa_rrs %>% 
  dplyr::select(-4) %>% 
  dplyr::mutate(X3 = stringr::str_remove(X3, "%"),
                X3 = dplyr::na_if(X3, "-"),
                X3 = as.numeric(X3),
                X2 = forcats::fct_inorder(X2)) %>% 
  tidyr::pivot_wider(names_from = X1, values_from = X3) %>% 
  dplyr::select(code, 期間 = X2, dplyr::everything()) %>%
  dplyr::left_join(kakakucom_code, by = "code") %>% 
  dplyr::select(code, fundcode, dplyr::everything()) %>% 
  print() %>% 
  readr::write_excel_csv(paste0(path, "つみたてNISA_運用情報_",
                                date, ".csv"))



# -----------------------------------------------------------------------------
# 商品分類別平均値
#------------------------------------------------------------------------------
nisa_info %>% 
  dplyr::select(code, type) %>% 
  dplyr::left_join(nisa_rrs, by = "code") %>% 
  dplyr::select(-code, -X3) %>% 
  dplyr::mutate(X4 = stringr::str_remove(X4, "%")) %>% 
  dplyr::mutate(type = dplyr::if_else(stringr::str_detect(type, "資産複合"),
                                      "資産複合（バランス型）", type)) %>% 
  # DT::datatable()
  dplyr::distinct(type, X1, X2, .keep_all = TRUE) %>% 
  tidyr::pivot_wider(names_from = X1, values_from = X4) %>% 
  print() %>% 
  readr::write_excel_csv(paste0(path, "つみたてNISA_商品分類別運用情報_",
                                date, ".csv"))



# -----------------------------------------------------------------------------
# 演習用データの作成
#------------------------------------------------------------------------------
"./data/sample/つみたてNISA_運用情報_2022-06-12.csv" %>% 
  read_csv() %>% 
  dplyr::filter(期間 == "3年") %>% 
  tidyr::drop_na(騰落率) %>% 
  dplyr::sample_n(size = 10) %>% 
  readr::write_excel_csv("./data/Exercies_6_1.csv")

"./data/sample/つみたてNISA_運用情報_2022-06-12.csv" %>%
  readr::read_csv() %>%
  dplyr::mutate(期間 = forcats::fct_inorder(期間)) %>%
  dplyr::left_join(code_list, by = "code") %>%
  dplyr::filter(code %in% code_list$code) %>%
  # dplyr::filter(期間 %in% c("6ヶ月", "1年", "3年", "5年")) %>%
  dplyr::mutate(リターンの平均値 = シャープレシオ * `リスク（標準偏差）`) %>% 
  readr::write_excel_csv("./data/Exercies_6_2.csv")


