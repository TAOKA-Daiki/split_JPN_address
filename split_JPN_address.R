## Title #####################################################
#
#    Function for Splitting JPN address
#
##############################################################

### 0. Load Packages ----
library(tidyverse)
library(zipangu)
library(rvest)

### 1. Define Function ----
split_address <- function(address){
  require(magrittr)
  require(stringr)
  pref <- c()
  city <- c()
  street <- c()
  
  gyosei <- c("大阪市", "横浜市", "名古屋市", "京都市", "札幌市", "さいたま市", "神戸市",
              "広島市", "新潟市", "川崎市", "浜松市", "堺市", "北九州市", "福岡市", "千葉市", 
              "仙台市", "熊本市", "岡山市", "相模原市", "静岡市")
  
  special_cities1 <- c("大和郡山市", "蒲郡市", "小郡市", "武蔵村山市", "東村山市", "羽村市") # ~郡~市/~村~市
  special_cities2 <- c("野々市市", "廿日市市", "四日市市") # ~市市
  special_guns <- c("高市郡", "余市郡") # ~市郡
  special_towns <- c("大町町", "玉村町") # ~町町/~村町

  if(str_detect(address, "東京都")){ # 東京都の場合
    # prefecture
    pref <- "東京都"
    # city
    if(str_detect(address, "(?<=東京都)[^市]+区")){ # 特別区の場合
      city <- str_extract(address, "(?<=東京都)[:graph:]+?区")
    }else if(str_detect(address, str_c("(?<=東京都)(", str_flatten(special_cities1, collapse = "|"),")"))){
      city <- str_extract(address, str_c("(?<=東京都)(", str_flatten(special_cities1, collapse = "|"), ")"))
    }else if(str_detect(address, str_c("(?<=東京都)[^市]+?郡[:graph:]+?(町|村)"))){ # 郡+町/村
      city <- str_extract(address, str_c("(?<=東京都)[^市]+?郡[:graph:]+?(町|村)"))
    }else if(str_detect(address, str_c("(?<=東京都)[^市]+?(町|村)"))){ # 町/村
      city <- str_extract(address, str_c("(?<=東京都)[^市]+?(町|村)"))
    }else{ # 市の場合
      city <- str_extract(address, "(?<=東京都)[:graph:]+?市")
    }
    # street
    street <- str_extract(address, str_c("(?<=東京都", city, ")[:graph:]+$"))
  }else{ # 東京都以外の場合
    # prefecture
    pref <- str_extract(address, "[:graph:]+?(道|府|県)")
    # city
    if(str_detect(address, str_c("(?<=", pref, ")(", str_flatten(special_cities1, collapse = "|"),")"))){
      city <- str_extract(address, str_c("(?<=", pref, ")(", str_flatten(special_cities1, collapse = "|"), ")"))
    }else if(str_detect(address, str_c("(?<=", pref, ")(", str_flatten(special_guns, collapse = "|"),"|[^市]+?郡)[:graph:]+?(町|村)"))){ # 郡+町/村
      city <- str_extract(address, str_c("(?<=", pref, ")(", str_flatten(special_guns, collapse = "|"), "|[^市]+?郡)(", str_flatten(special_towns, collapse = "|"), "|[:graph:]+?(町|村))"))
    }else if(str_detect(address, str_c("(?<=", pref, ")(", str_flatten(gyosei, collapse = "|"), ")"))){ # 行政区のある市の場合
      city <- str_extract(address, str_c("(?<=", pref, ")[:graph:]+?区"))
    }else{ # 市または行政区のない市の場合
      city <- str_extract(address, str_c("(?<=", pref, ")(", str_flatten(special_cities2, collapse = "|"), "|[:graph:]+?市)"))
    }
    # street
    street <- str_extract(address, str_c("(?<=", pref, city, ")[:graph:]+$"))
  }
  
  return(list(pref = pref, city = city, street = street))
}

split_address("東京都市川市中央")
split_address("福井県福井市市ノ瀬町")
split_address("島根県邑智郡美郷町村之郷")
split_address("長崎県大村市松原本町郷")
split_address("山形県北村山郡大石田町豊田")
split_address("富山県中新川郡上市町浅生")
split_address("福島県郡山市久留米")
split_address("東京都西多摩郡檜原村数馬")
split_address("佐賀県杵島郡大町町福母")
split_address("奈良県大和郡山市小林町西")
split_address("群馬県佐波郡玉村町中央")
split_address("愛知県名古屋市中村区中央")
split_address("山梨県西矢代郡市川三郷町吉村")
split_address("三重県鈴鹿市郡山町")
split_address("東京都大島町以下に掲載がない場合")
split_address("京都府京都市右京区西京極郡附洲町")
split_address("東京都武蔵村山市本町")
split_address("東京都小平市小川東町")
split_address("奈良県高市郡高取町谷田")
split_address("北海道余市郡仁木町以下に掲載がない場合")


### 2. Validation ----
# download data from Japan Post Office
temp <- tempfile(fileext = ".zip")
download.file(url = "http://www.post.japanpost.jp/zipcode/dl/roman/ken_all_rome.zip",
              destfile = temp)
unzip(zipfile = temp, exdir = tempdir(), overwrite = TRUE)

file.path <- list.files(tempdir()) %>% 
   str_subset(pattern = "^KEN_ALL_ROME.csv$") %>% 
    str_c(tempdir(), ., sep = "\\")

# create dataframe of answer (splitted addresses without whitespace)
answer_df <- read_csv(file = file.path, col_names = FALSE,
                      locale = locale(encoding = "Shift-JIS"),
                      col_select = 2:4) %>% 
  dplyr::transmute(pref = X2 %>% str_replace_all(pattern = "[:blank:]", replacement = ""),
                   city = X3 %>% str_replace_all(pattern = "[:blank:]", replacement = ""),
                   street = X4 %>% str_replace_all(pattern = "[:blank:]", replacement = ""))
# create dataframe to test
test_df <- answer_df %>% 
  dplyr::transmute(address = str_c(pref, city, street))

# test: split JPN address into prefecture, city, street
system.time(splitted_df <- test_df %>%
  dplyr::transmute(
    pref = map_chr(address, ~split_address(.)$pref),
    city = map_chr(address, ~split_address(.)$city),
    street = map_chr(address, ~split_address(.)$street)
  )
)
# Note: It probably takes few minutes.
# user:314.07, system:3.54, duration:317.74

# validation
all_equal(answer_df, splitted_df) # TRUE

### 3. Performance Comparison with zipangu::separate_address ----
# n = 10
set.seed(1234)
target_df_10 <- dplyr::sample_n(test_df, size = 10)
# zipangu::separate_address
time_separate_10 <- system.time(
  target_df_10 %>%
    dplyr::transmute(
      pref = map_chr(address, ~separate_address(.)$pref),
      city = map_chr(address, ~separate_address(.)$city),
      street = map_chr(address, ~separate_address(.)$street)
    )
) %>% .["elapsed"]
# split_address
time_split_10 <- system.time(
  target_df_10 %>%
    dplyr::transmute(
      pref = map_chr(address, ~split_address(.)$pref),
      city = map_chr(address, ~split_address(.)$city),
      street = map_chr(address, ~split_address(.)$street)
    )
) %>% .["elapsed"]

str_c(round(time_split_10/time_separate_10*100, 2), "%") # saved about 5-9%

# n = 100
set.seed(1234)
target_df_100 <- dplyr::sample_n(test_df, size = 100)
# zipangu::separate_address
time_separate_100 <- system.time(
  target_df_100 %>%
    dplyr::transmute(
      pref = map_chr(address, ~separate_address(.)$pref),
      city = map_chr(address, ~separate_address(.)$city),
      street = map_chr(address, ~separate_address(.)$street)
    )
) %>% .["elapsed"]
# split_address
time_split_100 <- system.time(
  target_df_100 %>%
    dplyr::transmute(
      pref = map_chr(address, ~split_address(.)$pref),
      city = map_chr(address, ~split_address(.)$city),
      street = map_chr(address, ~split_address(.)$street)
    )
) %>% .["elapsed"]

str_c(round(time_split_100/time_separate_100*100, 2), "%") # saved about 4-5%

# n = 1000
set.seed(1234)
target_df_1000 <- dplyr::sample_n(test_df, size = 1000)
# zipangu::separate_address
time_separate_1000 <- system.time(
  target_df_1000 %>%
    dplyr::transmute(
      pref = map_chr(address, ~separate_address(.)$pref),
      city = map_chr(address, ~separate_address(.)$city),
      street = map_chr(address, ~separate_address(.)$street)
    )
) %>% .["elapsed"]
# split_address
time_split_1000 <- system.time(
  target_df_1000 %>%
    dplyr::transmute(
      pref = map_chr(address, ~split_address(.)$pref),
      city = map_chr(address, ~split_address(.)$city),
      street = map_chr(address, ~split_address(.)$street)
    )
) %>% .["elapsed"]

str_c(round(time_split_1000/time_separate_1000*100, 2), "%") # saved about 4-5%

# n = 10000
set.seed(1234)
target_df_10000 <- dplyr::sample_n(test_df, size = 10000)
# zipangu::separate_address
time_separate_10000 <- system.time(
  target_df_10000 %>%
    dplyr::transmute(
      pref = map_chr(address, ~separate_address(.)$pref),
      city = map_chr(address, ~separate_address(.)$city),
      street = map_chr(address, ~separate_address(.)$street)
    )
) %>% .["elapsed"]
# split_address
time_split_10000 <- system.time(
  target_df_10000 %>%
    dplyr::transmute(
      pref = map_chr(address, ~split_address(.)$pref),
      city = map_chr(address, ~split_address(.)$city),
      street = map_chr(address, ~split_address(.)$street)
    )
) %>% .["elapsed"]

str_c(round(time_split_10000/time_separate_10000*100, 2), "%") # saved about 4-5%


### 4. Computational Environment ----
sessionInfo()$R.version$version.string # "R version 4.2.2 (2022-10-31 ucrt)"
sessionInfo()$platform # "x86_64-w64-mingw32/x64 (64-bit)"
sessionInfo()$running # "Windows 10 x64 (build 19044) with CPU Intel Core i7-3770 and DDR3 24GB RAM"