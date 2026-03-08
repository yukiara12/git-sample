####授業用のデータセット####
###======下準備======###
#パッケージをロード
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
##を読み込む
df_sample <- NHANES
df_nhanes_death <- import(here("mortality_linked_file_1999_2000.csv"))

#dfの結合
df <- df %>% rename(SEQN = seqn)
nhanes_all <- left_join(df_base, df, by = "SEQN")

#解析対象者を作成
analysis_df <- nhanes_all %>% filter(eligstat == 1)

#deathを作成
analysis_df <- analysis_df %>% mutate(death = case_when(
  mortstat == 1 ~ 1,
  mortstat == 0 ~ 0,
  T ~ 0
))

# 「人口統計(DEMO)」テーブル一覧を表示
nhanesSearchTableNames('DEMO', includerdc=TRUE, nchar=42, details=TRUE)
# 変数の意味を確認（コードブックの表示）
nhanesCodebook('DEMO', 'RIDAGEYR')
nhanesCodebook('BMX', 'BMXWAIST')

##棒グラフ
ggplot(analysis_df, aes(x = RIDAGEYR, fill = RIDAGEYR)) +
  geom_histogram()
##箱ひげ図
ggplot(analysis_df, aes(y = BMXWAIST)) +
  geom_boxplot()
##散布図+回帰直線
ggplot(analysis_df, aes(x = RIDAGEYR, y = BMXWAIST)) +
  geom_point() +
  geom_smooth(method = "lm")

##線形回帰
model <- lm(BMXWAIST ~ RIDAGEYR, analysis_df)
summary(model)

# 描画エリアを2x2に分割して、4つのプロットを一度に表示
par(mfrow = c(2, 2))
# 診断プロットを生成
plot(model)
# 描画エリアの設定を元に戻す
par(mfrow = c(1, 1))

##ロジスティック回帰
model2 <- glm(death ~ BMXWAIST, family = binomial(link="logit"), analysis_df)
summary(model2)