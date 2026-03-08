####授業用のデータセット####
###======下準備======###
#パッケージをダウンロード
install.packages("tidyverse","bloom","janitor")
library(tidyverse)
library(bloom)
library(janitor)

##作業用##
pacman::p_load(
  #Rを学ぶ
  learnr,
  swirl,
  nhanesA,
  NHANES,
  #データクリーニング
  here,
  rio,
  tidyverse,
  skimr,
  lubridate,
  #記述統計・図表
  janitor,
  epikit,
  forcats,
  ggplot2,
  rstatix,
  gtsummary,
  flextable,
  RColorBrewer,
  tableone,
  broom,
  forestploter,
  #統計解析
  rqlm,
  splines,
  rms,
  mgcv,
  survival,
  missRanger
)

####データを読み込む
#テーブル探し
nhanesSearchTableNames('DEMO', includerdc=TRUE, nchar=42, details=TRUE)

# 2017-2018年サイクルの「人口統計(DEMO)」テーブル一覧を表示
nhanesTables('DEMO', 2017)
# 変数の意味を確認（コードブックの表示）
nhanesCodebook('DEMO_L', 'RIAGENDR')

# 2021-2023のデータをダウンロード
demo_data <- nhanes('DEMO_L')
demo_dm <- nhanes("DIQ_L")

# 突合
df <- demo_data %>% left_join(demo_dm, by = "SEQN")


