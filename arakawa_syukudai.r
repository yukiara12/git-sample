######大学院疫学講義宿題######
####2025.11.27実施

###======下準備======###
#パッケージをロード
pacman::p_load(
  #Rを学ぶ
  learnr,
  swirl,
  #データクリーニング
  here,
  rio,
  tidyverse,
  skimr,
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
  performance,
  #統計解析
  rqlm,
  splines,
  rms,
  mgcv
)

####データを読み込む
##savを読み込む
df <- import(here("shhs.csv"))

###変数を因子型に変換する
#smoking
df <- df %>% mutate(smoking = factor(case_when(
  smoking == 1 ~ "Never",
  smoking == 2 ~ "Ex",
  smoking == 3 ~ "Current"
)))
#activity
df <- df %>% mutate(activity =factor(case_when(
  activity == 1 ~ "active",
  activity == 2 ~ "average",
  activity == 3 ~ "inactive"
)))

#参照カテゴリかえる
df$activity <- relevel(df$activity, ref = "inactive")
df$smoking <- relevel(df$smoking, ref = "Never")


###summaryを用いて要約
summary(df)
hist(df$age)
hist(df$totchol)
hist(df$bmi)
hist(df$systol)
df %>% ggplot(aes(smoking)) + geom_bar()
df %>% ggplot(aes(activity)) + geom_bar()
df %>% ggplot(aes(chd)) + geom_bar()
hist(df$days)


###問2.単回帰分析（目的:systol, 説明bmi）
ggplot(df, aes(x=bmi, y=systol))+
         geom_point() +
         labs(
           x="BMI",
           y="Systolic Blood Pressure",
           title = "Scatter plot: BMI vs Systolic BP"
         )
fit2 <- lm(systol ~ bmi, data = df)
summary(fit2)
plot(fit2)

###問3.ageを説明変数として追加
fit3 <- lm(systol ~ bmi + age, data = df)
summary(fit3)

###問4.交互作用項を追加
fit4 <- lm(systol ~ bmi*smoking, data = df)
summary(fit4)

###問5.chdをアウトカム、曝説明変数をsmokingにしてリスク比を計算
##全体
#Crudeの表を作る
tab <- df %>% tabyl(chd,smoking)
#Currentのリスクを計算
risk_current <- tab$Current[2]/sum(tab$Current)
risk_current
#Neverのリスクを計算
risk_never <- tab$Never[2]/sum(tab$Never)
risk_never
#Crudeのリスク比を出す
RR = risk_current/risk_never
RR

##50歳未満で実行
df_u50 <- df %>% filter(age <50)
#Crudeの表を作る
tab_u50 <- df_u50 %>% tabyl(chd,smoking)
#Currentのリスクを計算
risk_current_u50 <- tab_u50$Current[2]/sum(tab_u50$Current)
risk_current_u50
#Neverのリスクを計算
risk_never_u50 <- tab_u50$Never[2]/sum(tab_u50$Never)
risk_never_u50
#Crudeのリスク比を出す
RR_u50 = risk_current_u50/risk_never_u50
RR_u50

##50歳以上で実行
df_over50 <- df %>% filter(age >=50)
#Crudeの表を作る
tab_over50 <- df_over50 %>% tabyl(chd,smoking)
#Currentのリスクを計算
risk_current_over50 <- tab_over50$Current[2]/sum(tab_over50$Current)
risk_current_over50
#Neverのリスクを計算
risk_never_over50 <- tab_over50$Never[2]/sum(tab_over50$Never)
risk_never_over50
#Crudeのリスク比を出す
RR_over50 = risk_current_over50/risk_never_over50
RR_over50

RR_results <- data.frame(
  RR = RR,
  RR_u50 = RR_u50,
  RR_over50 = RR_over50,
  row.names = "RR_summary"
  )

###問6 chdが目的変数、説明変数avtivity,age,smoking,bmi
fit6 <- glm(chd ~ activity + age + smoking + bmi, data = df, family = binomial)
summary(fit6)
check_model(fit6)
results6 <- tidy(fit6, conf.int = TRUE, exponentiate = TRUE)
results6

###問7 修正ポワソン回帰
fit7 <- glm(chd ~ activity + age + smoking + bmi, data = df, family = poisson(link = "log"))
cov.robust <- sandwich::vcovHC(fit7, type = "HC0")
results7 <- broom::tidy(fit7, 
                        conf.int = TRUE, 
                        exponentiate = TRUE, 
                        conf.level = 0.95,
                        vcov = cov.robust)
results7