####JPHC_NEXTの読み込みとクリーニング####
##開始2024-05-31
##修正2025-04-18
##git-hub使用開始2025-11-12
##作成者：荒川

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
  missRanger,
  mice,
  miceadds,
  mitml,
  lmtp
)

####JPHC_NEXTのデータを読み込む
##csvを読み込む
df_q00 <- import(here("data","JPHC_NEXT_q00.csv"))
df_dm_receipt <- import(here("data","kanyusya_rece_all.csv"))

####IDを突合する
df_jphc <- left_join(df_q00, df_dm_receipt, by = c("id" = "NEXTID"))
####レセプトありなしの変数を作成する
df_jphc <- df_jphc %>% mutate(receipt = case_when(
  is.na(V1) ~ 0,
  T ~ 1
))

####====JPHC_loneliness_DMのクリーニング====####
####【Population】レセプト加入歴があるDMのない人を区別する変数を作成
##レセプトがあり、q00でDMがない人とDM処方がないに限るデータセットdf_dmを作る
df_dm1 <- df_jphc %>% filter(phdm != 1) %>% filter(med_go != 1)
df_dm <- df_dm1 %>% drop_na(V1) 

####【Exposure】曝露のlonelinessを作成
#孤独感の質問票はCES-D11の問6を使用
#「一人ぼっちでさびしい」
#0:missing, 1:全く/ほとんどなかった,2:たまにあった,3:しばしばあった,4:いつもあった
#集計表と棒グラフを作成
df_dm %>% tabyl(mind06)
ggplot(df_dm, aes(x = mind06)) +
  geom_bar()

##曝露の孤独感について欠損mind06=0を落とす（※要検討）
##定義：孤独感が3:しばしばあった、4:いつもあったを孤独ありと定義
df_dm <- df_dm %>% mutate(loneliness = case_when(
  mind06 == 1 ~ 0,
  mind06 == 2 ~ 0,
  mind06 == 3 ~ 1,
  mind06 == 4 ~ 1,
  T ~ NA_real_))
df_dm %>% tabyl(loneliness)

##定義2：孤独感が2:あまりなかった、3:しばしばあった、4:いつもあったを孤独少し以上あると定義
df_dm <- df_dm %>% mutate(some_loneliness = case_when(
  mind06 == 1 ~ 0,
  mind06 == 2 ~ 1,
  mind06 == 3 ~ 1,
  mind06 == 4 ~ 1,
  T ~ NA_real_))
df_dm %>% tabyl(some_loneliness)

##定義感度分析：孤独感を段階で取り扱う（1:ほとんどなかった、2:あまりなかった、3:しばしばあった、4:いつもあった）
df_dm <- df_dm %>% mutate(lonely_level = case_when(
  mind06 == 1 ~ "no loneliness",
  mind06 == 2 ~ "low loneliness",
  mind06 == 3 ~ "mid loneliness",
  mind06 == 4 ~ "high loneliness",
  T ~ NA_character_))
df_dm <- df_dm %>% mutate(lonely_level = fct_relevel(
  lonely_level,"no loneliness","low loneliness","mid loneliness","high loneliness"),
  lonely_revel = as.ordered(lonely_level))
levels(df_dm$lonely_level)
df_dm %>% tabyl(lonely_level)


####【Time to event】組入からアウトカムのDMもしくはfollow終了までの時間を作成
##※要検討※回答年月のどちらかが欠損の場合、どうする？？
##※要検討※欠損の場合は読み込み年月日を回答日とする
##※要検討※年月が分かる場合は、X月1日を回答日とする
##回答年を作成
df_dm %>% tabyl(nxt_y)
df_dm <- df_dm %>% mutate(answer_y = case_when(
  nxt_y == 1 ~ "2009",
  nxt_y == 2 ~ "2010",
  nxt_y == 3 ~ "2011",
  nxt_y == 4 ~ "2012",
  nxt_y == 5 ~ "2013",
  nxt_y == 6 ~ "2014",
  nxt_y == 7 ~ "2015",
  nxt_y == 8 ~ "2016",
  nxt_y == 9 ~ "2017",
  nxt_y == 0 ~ NA_character_,
))

##回答月を作成
df_dm %>% tabyl(nxt_m)
df_dm <- df_dm %>% mutate(answer_m = case_when(
  nxt_m == 1 ~ "01",
  nxt_m == 2 ~ "02",
  nxt_m == 3 ~ "03",
  nxt_m == 4 ~ "04",
  nxt_m == 5 ~ "05",
  nxt_m == 6 ~ "06",
  nxt_m == 7 ~ "07",
  nxt_m == 8 ~ "08",
  nxt_m == 9 ~ "09",
  nxt_m == 10 ~ "10",
  nxt_m == 11 ~ "11",
  nxt_m == 12 ~ "12",
  TRUE ~ NA_character_,
))

##回答年月日を作成（文字型で作成し日付型で格納）
#文字型で作成
df_dm <- df_dm %>% mutate(sankabi = paste0(substr(answer_y,1,4),substr(answer_m,1,2),"01"))
#日付型に変換
df_dm <- df_dm %>% mutate(date_sanka = ymd(sankabi))
##取り込み日を日付型に変換
df_dm <- df_dm %>% mutate(date_ankdt = ymd(ankdt))
#回答年か回答月が欠損のものは全て取り込み日を回答日にする
df_dm <- df_dm %>% mutate(date_sanka = case_when(
  is.na(answer_y) ~ date_ankdt,
  is.na(answer_m) ~ date_ankdt,
  T ~ date_sanka))
#回答年月日が取り込み日より前の場合は、取り込み日を回答日にする
df_dm <- df_dm %>% mutate(date_sanka = case_when(
  date_ankdt < date_sanka ~ date_ankdt,
  T ~ date_sanka))

##処方日、加算日、病名付与日を日付型に変換
df_dm <- df_dm %>% mutate(date_syohou_kaishi1 = ymd(date_syohou_kaishi1))
df_dm <- df_dm %>% mutate(date_syohou_kaishi2 = ymd(date_syohou_kaishi2))
df_dm <- df_dm %>% mutate(date_kasan_kaishi = ymd(date_kasan_kaishi))
df_dm <- df_dm %>% mutate(date_icd_kaishi = ymd(date_icd_kaishi))
df_dm <- df_dm %>% mutate(date_icd_E10_kaishi = ymd(date_icd_E10_kaishi))
df_dm <- df_dm %>% mutate(df_dmicd_E11_kaishi = ymd(df_dmicd_E11_kaishi))
df_dm <- df_dm %>% mutate(df_dmicd_E13_kaishi = ymd(df_dmicd_E13_kaishi))
df_dm <- df_dm %>% mutate(df_dmicd_E14_kaishi = ymd(df_dmicd_E14_kaishi))
df_dm <- df_dm %>% mutate(date_rece_final = ymd(date_rece_final))

##syohou1とsyohou2の早い方をsyohouとする
df_dm <- df_dm %>% mutate(date_syohou_kaishi = case_when(
  is.na(date_syohou_kaishi1) & is.na(date_syohou_kaishi2) ~ NA_Date_,
  is.na(date_syohou_kaishi1) & date_syohou_kaishi2 > 0 ~ date_syohou_kaishi2,
  date_syohou_kaishi1 > 0 & is.na(date_syohou_kaishi2) ~ date_syohou_kaishi1,
  date_syohou_kaishi1 > date_syohou_kaishi2 ~ date_syohou_kaishi2,
  date_syohou_kaishi1 < date_syohou_kaishi2 ~ date_syohou_kaishi1,
  T ~ NA_Date_)
)

##処方年を取り出す
df_dm <- df_dm %>% mutate(syohou_kaishi_y = format(date_syohou_kaishi, "%Y"))


##レセプトが発生した最も若い年数から、レセ開始年を作成
df_dm <- df_dm %>% mutate(rece_start_y = case_when(
  rece_2011 == 1 ~ 2011,
  rece_2012 == 1 ~ 2012,
  rece_2013 == 1 ~ 2013,
  rece_2014 == 1 ~ 2014,
  rece_2015 == 1 ~ 2015,
  rece_2016 == 1 ~ 2016,
  rece_2017 == 1 ~ 2017,
  rece_2018 == 1 ~ 2018,
  rece_2019 == 1 ~ 2019,
  rece_2020 == 1 ~ 2020,
  rece_2021 == 1 ~ 2021,
  T ~ NA_real_
))

#最も早いレセ開始日を文字型で作成
df_dm <- df_dm %>% mutate(recekaishibi = paste0(substr(rece_start_y,1,4),"0401"))
#日付型に変換
df_dm <- df_dm %>% mutate(date_rece_start = ymd(recekaishibi))



##参加日から処方日、加算日、病名付与日、最終レセ発生日までの時間を作成
#参加日ー処方日
df_dm <- df_dm %>% mutate(day_ans_syohou = date_syohou_kaishi - date_sanka)
#分布を確認（マイナスがいる）
ggplot(df_dm, aes(x = day_ans_syohou)) +
  geom_histogram()
#参加日ー加算日
df_dm <- df_dm %>% mutate(day_ans_kasan = date_kasan_kaishi - date_sanka)
#分布を確認（マイナスがいる）
ggplot(df_dm, aes(x = day_ans_kasan)) +
  geom_histogram()
#参加日ー病名付与日
df_dm <- df_dm %>% mutate(day_ans_icd = date_icd_kaishi - date_sanka)
#分布を確認（マイナスがいる）
ggplot(df_dm, aes(x = day_ans_icd)) +
  geom_histogram()
#参加日ー最終レセ発生日
df_dm <- df_dm %>% mutate(day_ans_recefinal = date_rece_final - date_sanka)
#分布を確認（マイナスがいる）
ggplot(df_dm, aes(x = day_ans_recefinal)) +
  geom_histogram()

##レセの最終月がベースライン回答日より前の人（627人）
df0 <- df_dm %>% filter(day_ans_recefinal < 0)
##処方レセがベースライン回答日より前の人（20人）
df1 <- df_dm %>% filter(day_ans_syohou < 0)
##加算レセがベースライン回答日より前の人（1人）
df2 <- df_dm %>% filter(day_ans_kasan < 0)
##DM病名レセがベースライン回答日より前の人（422人）
df3 <- df_dm %>% filter(day_ans_icd < 0)
##レセ最終日が処方や加算より前の人（417人）※重要！！！！※
df4 <- df_dm %>% filter(day_ans_recefinal < day_ans_syohou | day_ans_recefinal < day_ans_kasan)
##DMの診断日がE10（Type1DM）だけの人（7人）
df5 <- df_dm %>% filter(date_icd_E10_kaishi>0 & is.na(df_dmicd_E11_kaishi) & is.na(df_dmicd_E13_kaishi) & is.na(df_dmicd_E14_kaishi))
##DMの診断日がE10（Type1DM）の人（人）
df6 <- df_dm %>% filter(date_icd_E10_kaishi == date_icd_kaishi)

##ドロップ対象を作る（最終レセ<0d, 処方<30d)※どうする！！？？
df_dm <- df_dm %>% mutate(drop1 = case_when(
  day_ans_recefinal < 0 ~ 1,
  T ~ 0))
df_dm <- df_dm %>% mutate(drop2 = case_when(
  day_ans_syohou < 30 ~ 1,
  T ~ 0))
df_dm <- df_dm %>% mutate(drop = case_when(
  day_ans_recefinal < 0 ~ 1,
  day_ans_syohou < 30 ~ 1,
  T ~ 0))


###レセプトデータを用いてdmの診断あり/なしの変数を作成
###primaryの定義は、「DM薬処方コード発生」&「ICD10コードE11-14があり」とする
##Type1DMの変数を作成
df_dm <- df_dm %>% mutate(t1dm = case_when(
  date_icd_E10_kaishi>0 & is.na(df_dmicd_E11_kaishi) & is.na(df_dmicd_E13_kaishi) & is.na(df_dmicd_E14_kaishi) ~ 1,
  T ~ 0))
##処方イベントあり/なしの変数を作成
df_dm <- df_dm %>% mutate(dm_syohou = case_when(
  day_ans_syohou > 0 ~ 1,
  T ~ 0))
##加算イベントあり/なしの変数を作成
df_dm <- df_dm %>% mutate(dm_kasan = case_when(
  day_ans_kasan > 0 ~ 1,
  T ~ 0))
##処方あるのに病名ないという不一致変数を作成(9人いる)
df_dm <- df_dm %>% mutate(syohou1_icd0 = case_when(
  day_ans_syohou > 0 & is.na(date_icd_kaishi)~ 1,
T ~ 0))
##DM発症ありの変数を作成【Primary イベントあり】
df_dm <- df_dm %>% mutate(dm = case_when(
  t1dm == 1 ~ 0,
  date_icd_kaishi > 0 & dm_syohou == 1 ~ 1,
  T ~ 0))
##Type2DM発症ありの変数を作成
df_dm <- df_dm %>% mutate(t2dm = case_when(
  t1dm == 1 ~ 0,
  df_dmicd_E11_kaishi > 0 & dm_syohou == 1 ~ 1,
  T ~ 0))


##【Primay analysis】参加-イベント発生の時間time_eventを作成
##糖尿病の発生は、day_syohou_kaishiまでの期間
##最終レセがベースラインより後、処方がベースライン30日経過前にない場合に限る
##処方日があるがicd10コードがない場合は、イベント発生なしとする【定義】
##イベント発生は処方開始日
df_dm <- df_dm %>% mutate(time_event = case_when(
  (day_ans_syohou < day_ans_recefinal) & dm == 1 ~ day_ans_syohou,
  T ~ day_ans_recefinal
  ))
##イベント発生を処方開始日+T2DMに絞る
df_dm <- df_dm %>% mutate(time_event2 = case_when(
  (day_ans_syohou < day_ans_recefinal) & t2dm == 1 ~ day_ans_syohou,
  T ~ day_ans_recefinal
))

##研究参加時に国保加入しているかどうかの変数を作成
df_dm <- df_dm %>% mutate(yeskokuho = case_when(
  date_rece_start < date_sanka ~ 1,
  T ~ 0
))
##イベント発生年より前の年に国保加入しているかどうかの変数を作成
df_dm <- df_dm %>% mutate(yeskokuho_beforeDM = case_when(
  (date_rece_start < date_syohou_kaishi) & (rece_start_y != syohou_kaishi_y) & (dm == 1) ~ 1,
  T ~ 0
))
##dm発症ありとされた人で国保加入状況を確認
df_dm %>% filter(dm == 1) %>% tabyl(yeskokuho_beforeDM)




####【共変量】Variables：共変量やその他必要な変数を作成
###調整する共変量を作成する
##年齢カテゴリー化age_cat
df_dm <- df_dm %>% mutate(age_cat = age_categories(
  age0,
  breakers = c(40,50,60,70)
  ))
df_dm %>% tabyl(age_cat, show_na = T)
levels(df_dm$age_cat)

##性別が男性male==1
df_dm <- df_dm %>% mutate(male = case_when(
  gender0 == 1 ~ 1,
  gender0 == 2 ~ 0))

##BMIを算出したい
##身長をクリーニング
df_dm <- df_dm %>% mutate(ht_bmi = case_when(
  ht > 0 & ht < 100 ~ NA_real_,
  ht >= 100 & ht <= 200 ~ ht,
  ht > 200 & ht < 999 ~ NA_real_,
  T ~ NA_real_))
df_dm %>% tabyl(ht_bmi)

##体重をクリーニング
df_dm <- df_dm %>% mutate(wt_bmi = case_when(
  wt > 0 & wt < 30 ~ NA_real_,
  wt >= 30 & wt <= 150 ~ wt,
  wt > 150 & wt < 999 ~ NA_real_,
  T ~ NA_real_,))
df_dm %>% tabyl(wt_bmi)

##20年前の体重をクリーニング
df_dm <- df_dm %>% mutate(wt_bmi20yrs = case_when(
  wt20yrs > 0 & wt20yrs < 30 ~ NA_real_,
  wt20yrs >= 30 & wt20yrs <= 150 ~ wt20yrs,
  wt20yrs > 150 & wt20yrs < 999 ~ NA_real_,
  T ~ NA_real_,))
df_dm %>% tabyl(wt_bmi20yrs)

##BMIを計算
df_dm <- df_dm %>% mutate(bmi = wt_bmi/(ht_bmi/100)^2) %>% mutate(bmi = round(bmi,1))
df_dm <- df_dm %>% mutate(bmi = case_when(
  bmi > 0 & bmi < 14 ~ NA_real_,
  bmi >= 14 & bmi <= 45 ~ bmi,
  bmi > 45 & bmi < 100 ~ NA_real_,
  T ~ NA_real_,))
df_dm %>% tabyl(bmi)
df_dm <- df_dm %>% mutate(bmi_c = case_when(
  bmi < 18.5 ~ "<18.5",
  bmi >= 18.5 & bmi < 25 ~ "18.5-25",
  bmi >= 25 & bmi < 30 ~ "25-30",
  bmi >=30 ~ "≥30"))
df_dm <- df_dm %>% mutate(bmi_c = fct_relevel(
  bmi_c,"<18.5","18.5-25","25-30","≥30"),
  bmi_c = as.ordered(bmi_c))
levels(df_dm$bmi_c)
df_dm %>% tabyl(bmi_c)

##20年前のBMIを計算
df_dm <- df_dm %>% mutate(bmi20yrs = wt_bmi20yrs/(ht_bmi/100)^2) %>% mutate(bmi20yrs = round(bmi20yrs,1))
df_dm %>% tabyl(bmi20yrs)

##家族の変数
#婚姻状況marital（0:既婚,1:未婚,2:離別,3:その他）
df_dm <- df_dm %>% mutate(marital = case_when(
  mari == 1 ~ "partner",
  mari == 2 ~ "separated",
  mari == 3 ~ "separated",
  mari == 4 ~ "separated",
  mari == 5 ~ "single",
  mari == 6 ~ "other",
  mari == 9 ~ NA_character_,
  mari == 0 ~ NA_character_,
  T ~ NA_character_))
df_dm <- df_dm %>% mutate(marital = fct_relevel(
  marital,"partner","single","separated","other"))
levels(df_dm$marital)
df_dm %>% tabyl(marital)

#世帯構成変数のクリーニング
#独居livealoneを作成
df_dm <- df_dm %>% mutate(livealone = case_when(
  fam_sgl == 1 & fam_ptn==0 & fam_cld==0 & fam_prnt==0 & fam_ot==0 ~ 1,
  fam_sgl == 0 & fam_ptn==0 & fam_cld==0 & fam_prnt==0 & fam_ot==0 & doukyo_n >0 ~ 0,
  fam_sgl == 0 & fam_ptn==0 & fam_cld==0 & fam_prnt==0 & fam_ot==0 & (doukyo_n ==0|is.na(doukyo_n)) ~ 1,
  fam_sgl == 1 & (fam_ptn!=0|fam_cld!=0|fam_prnt!=0|fam_ot!=0) ~ 0,
  fam_sgl == 0 & (fam_ptn!=0|fam_cld!=0|fam_prnt!=0|fam_ot!=0) ~ 0,  
  T ~ NA_real_))
df_dm %>% tabyl(doukyo_n,livealone)

#同居人数という変数を作る
df_dm <- df_dm %>% mutate(fam_n = case_when(
  fam_sgl == 1 & fam_ptn==0 & fam_cld==0 & fam_prnt==0 & fam_ot==0 ~ 1, #独居
  fam_sgl == 0 & fam_ptn==1 & fam_cld==0 & fam_prnt==0 & fam_ot==0 & doukyo_n == 1 ~ 2, #配偶者と二人暮らし
  fam_sgl == 0 & fam_ptn==1 & fam_cld==0 & fam_prnt==0 & fam_ot==0 & doukyo_n == 2 ~ 2, #配偶者と二人暮らしだが記載ミス
  livealone != 1 ~ doukyo_n +1, #独居じゃなくて誰かと住んでる  
  T ~ NA_real_))
df_dm <- df_dm %>% mutate(fam_n = case_when(
  doukyo_n > 15 ~ NA_real_, #同居人数が15人を超えているものは欠損
  fam_sgl == 0 & fam_ptn==1 & fam_cld==0 & fam_prnt==0 & fam_ot==0 & (is.na(doukyo_n)|doukyo_n==0) ~ 2, #配偶者と二人暮らしと判断
  fam_sgl == 0 & fam_ptn==0 & fam_cld==0 & fam_prnt==0 & fam_ot==0 & is.na(doukyo_n) ~ 1, #独居と判断 
  livealone == 0 & fam_n == 1 ~ NA_real_,
  T ~ fam_n))
df_dm %>% tabyl(livealone,fam_n)
df_dm %>% tabyl(doukyo_n,fam_n)

##異常値を確認
df_notgood <- df_dm %>% filter(livealone == 0 & fam_n == 1)

###世帯所得のカテゴリーを作成
df_dm <- df_dm %>% mutate(income_cat = case_when(
  income == 1 ~ "<3 million yen",
  income == 2 ~ "≥3&<6 million yen",
  income == 3 ~ "≥6&<9 million yen",
  income == 4 ~ "≥9&<12 million yen",
  income == 5 ~ "≥12&<15 million yen",
  income == 6 ~ "≥15 million yen",
  income == 0 ~ NA_character_,
  T ~ NA_character_))
df_dm <- df_dm %>% mutate(income_cat = fct_relevel(
  income_cat,"<3 million yen","≥3&<6 million yen","≥6&<9 million yen","≥9&<12 million yen","≥12&<15 million yen","≥15 million yen"),
  income_cat = as.ordered(income_cat))
levels(df_dm$income_cat)
df_dm %>% tabyl(income_cat)

##もう少し分類減らしたincomeカテゴリー
df_dm <- df_dm %>% mutate(income_c = case_when(
  income_cat == "<3 million yen" ~ "income_low",
  income_cat == "≥3&<6 million yen" ~ "income_mid",
  income_cat == "≥6&<9 million yen" ~ "income_mid",
  income_cat == "≥9&<12 million yen" ~ "income_hi",
  income_cat == "≥12&<15 million yen" ~ "income_hi",
  income_cat == "≥15 million yen" ~ "income_hi",
  income_cat == "other" ~ "other",
  T ~ "other"))
df_dm <- df_dm %>% mutate(income_c = fct_relevel(
  income_c,"income_low","income_mid","income_hi","other"))
levels(df_dm$income_c)
df_dm %>% tabyl(income_c)

##等価世帯所得の値を作成
#世帯所得に数字を割り当てる
df_dm <- df_dm %>% mutate(income_real = case_when(
  income == 1 ~ 150,
  income == 2 ~ 450,
  income == 3 ~ 750,
  income == 4 ~ 1050,
  income == 5 ~ 1350,
  income == 6 ~ 1650,
  income == 0 ~ NA_real_,
  T ~ NA_real_))
#世帯人数の値を用いて等価世帯所得を計算
df_dm <- df_dm %>% mutate(income_touka = income_real/sqrt(fam_n))
hist(df_dm$income_touka)
#等価世帯所得のカテゴリーを作成
df_dm <- df_dm %>% mutate(toukasyotoku_cat = age_categories(
  income_touka,
  breakers = c(0,150,300)))
df_dm$toukasyotoku_cat <- as.ordered(df_dm$toukasyotoku_cat)
is.ordered(df_dm$toukasyotoku_cat)
levels(df_dm$toukasyotoku_cat)
df_dm %>% tabyl(toukasyotoku_cat, show_na = T)
df_dm %>% tabyl(income_touka,toukasyotoku_cat)

###教育歴カテゴリーedu(1:中卒,2:高卒,3:短大専門,4:大卒,5:その他未回答)
df_dm <- df_dm %>% mutate(edu = case_when(
  SCHOOL == 1 ~ "Junior high school",
  SCHOOL == 2 ~ "High school",
  SCHOOL == 3 ~ "College or higher",
  SCHOOL == 4 ~ "College or higher",
  SCHOOL == 5 ~ "other",
  SCHOOL == 0 ~ NA_character_,
  T ~ NA_character_))
df_dm <- df_dm %>% mutate(edu = fct_relevel(
  edu,"Junior high school","High school","College or higher","other"),
  edu = as.ordered(edu))
levels(df_dm$edu)
df_dm %>% tabyl(edu)

###職業カテゴリーwork（1:仕事あり,2:主婦3:無職4:その他）
df_dm <- df_dm %>% mutate(work = case_when(
  JOB == 3|JOB == 4|JOB == 5|JOB == 6|JOB == 7|JOB == 8|JOB == 9|JOB == 10|JOB == 11|JOB == 12 ~ "working",
  JOB == 2 ~ "housekeeping",
  JOB == 1 ~ "no job",
  T ~ NA_character_))
df_dm <- df_dm %>% mutate(work = fct_relevel(
  work,"working","housekeeping","no job","other"))
df_dm %>% tabyl(work)

###疾患の有無を作成
##既往に「がん」あり
df_dm <- df_dm %>% mutate(ph_cancer = case_when(
  phgc == 1|phcc == 1|phlc == 1|phhcc == 1|phbc == 1|phpc == 1|photc == 1 ~ 1,
  T ~ 0))
##既往に身体疾患あり(mi,ap,str,ca,ch,gt,atm,copd,ld,sas)
df_dm <- df_dm %>% mutate(ph_physical = case_when(
  phmi == 1|phap == 1|phstr == 1|phca == 1|phch == 1|phgt == 1|phatm == 1|phcopd == 1|phld == 1|phsas == 1 ~ 1,
  T ~ 0))
##既往にうつphdepあり
df_dm <- df_dm %>% mutate(ph_mental = case_when(
  phdep == 1 ~ 1,
  T ~ 0))

##DM家族歴fam_dmあり
df_dm <- df_dm %>% mutate(fam_dm = case_when(
  FHDM1 == 1|FHDM2 ==1|FHDM3 ==1|FHDM4 ==1|FHDM6 ==1 ~ 1,
  T ~ 0))

##うつ症状のスコアをCESD11の孤独以外で計算
#mind05をリバーススコアリング
df_dm <- df_dm %>% mutate(mind05r = case_when(
  mind05 == 0 ~ 0,
  mind05 == 1 ~ 3,
  mind05 == 2 ~ 2,
  mind05 == 3 ~ 1,
  mind05 == 4 ~ 0,  
  T ~ NA_real_))
#mind08をリバーススコアリング
df_dm <- df_dm %>% mutate(mind08r = case_when(
  mind08 == 0 ~ 0,
  mind08 == 1 ~ 3,
  mind08 == 2 ~ 2,
  mind08 == 3 ~ 1,
  mind08 == 4 ~ 0,  
  T ~ NA_real_))
#合計を計算
df_dm <- df_dm %>% mutate(ces_d = (mind01-1)+(mind02-1)+(mind03-1)+(mind04-1)+mind05r+(mind07-1)+mind08r+(mind09-1)+(mind10-1)+(mind11-1))
#どれか1つでも欠損なら欠損にする
df_dm <- df_dm %>% mutate(ces_d = case_when(
  mind01==0|mind02==0|mind03==0|mind04==0|mind05==0|mind07==0|mind08==0|mind09==0|mind10==0|mind11==0 ~ NA_real_,
  T ~ ces_d))
df_dm %>% tabyl(ces_d)



###社会関係変数
##社会参加ありsocail_participation
df_dm <- df_dm %>% mutate(social_participation = case_when(
  voluntFR == 0|voluntFR == 1 ~ 0,
  voluntFR == 2|voluntFR == 3|voluntFR == 4 ~ 1,  
  T ~ 0))

##サポートの有無
df_dm <- df_dm %>% mutate(social_support = case_when(
  SPT1 == 2 |SPT1 == 3|SPT1 == 4|SPT1 == 5 ~ 1,
  SPT2 == 2 |SPT2 == 3|SPT2 == 4|SPT2 == 5 ~ 1,
  SPT3 == 2 |SPT3 == 3|SPT3 == 4|SPT3 == 5 ~ 1,
  SPT4 == 2 |SPT4 == 3|SPT4 == 4|SPT4 == 5 ~ 1,
  SPT5 == 2 |SPT5 == 3|SPT5 == 4|SPT5 == 5 ~ 1,
  SPT6 == 2 |SPT6 == 3|SPT6 == 4|SPT6 == 5 ~ 1,
  T ~ 0))

#サポートの量を数値で定義:ESSI
df_dm <- df_dm %>% mutate(ESSI = SPT1 + SPT2 + SPT3 + SPT4 + SPT5 + SPT6)
df_dm <- df_dm %>% mutate(ESSI = case_when(
  SPT1==0|SPT2==0|SPT3==0|SPT4==0|SPT5==0|SPT6==0 ~ NA_real_,
  T ~ ESSI))
tabyl(df_dm,ESSI)
hist(df_dm$ESSI)
quantile(df_dm$ESSI, na.rm = T)
tabyl(df_dm$ESSI)
df_dm <- df_dm %>% mutate(ESSI_cat = case_when(
  ESSI <20 ~ 1,
  ESSI >=20 & ESSI < 26 ~ 2,
  ESSI >=26 & ESSI < 30 ~ 3,
  ESSI >= 30 ~ 4,
  T ~ NA_real_))
hist(df_dm$ESSI_cat)

##カテゴリ化する
df_dm <- df_dm %>% mutate(ESSI_level = case_when(
  ESSI_cat == 1 ~ "low support",
  ESSI_cat == 2 ~ "low-mid support",
  ESSI_cat == 3 ~ "mid-high support",
  ESSI_cat == 4 ~ "high support",
  T ~ NA_character_))
df_dm <- df_dm %>% mutate(ESSI_level = fct_relevel(
  ESSI_level,"low support","low-mid support","mid-high support","high support"))
df_dm %>% tabyl(ESSI_level)

##相談できる親しい友人の数
df_dm <- df_dm %>% mutate(soudan_friend = case_when(
  friendn == 2 |friendn == 3|friendn == 4 ~ 1,
  T ~ 0))

##相談できる親類の数
df_dm <- df_dm %>% mutate(soudan_family = case_when(
  familyn == 2 |familyn == 3|familyn == 4 ~ 1,
  T ~ 0))


####生活様式変数
###<<<<<Total metsを算出するためのクリーニング>>>>>####
##座位時間sedantarを時間に変換
df_dm <- df_dm %>% mutate(sit_h = case_when(
  sedantar == 0 ~ 0,
  sedantar == 1 ~ 0,
  sedantar == 2 ~ 0.5,
  sedantar == 3 ~ 2,
  sedantar == 4 ~ 4,
  sedantar == 5 ~ 6,
  sedantar == 6 ~ 8,
  sedantar == 7 ~ 10,
  sedantar == 8 ~ 11,
  is.na(sedantar) ~ 0))
df_dm %>% tabyl(sedantar,sit_h)

##立位時間standを時間に変換
df_dm <- df_dm %>% mutate(stand_h = case_when(
  stand == 0 ~ 0,
  stand == 1 ~ 0,
  stand == 2 ~ 0.5,
  stand == 3 ~ 2,
  stand == 4 ~ 4,
  stand == 5 ~ 6,
  stand == 6 ~ 8,
  stand == 7 ~ 10,
  stand == 8 ~ 11,
  is.na(stand) ~ 0))
df_dm %>% tabyl(stand,stand_h)

##歩行時間walktimeを時間に変換
df_dm <- df_dm %>% mutate(walk_h = case_when(
  walktime == 0 ~ 0,
  walktime == 1 ~ 0,
  walktime == 2 ~ 0.5,
  walktime == 3 ~ 2,
  walktime == 4 ~ 4,
  walktime == 5 ~ 6,
  walktime == 6 ~ 8,
  walktime == 7 ~ 10,
  walktime == 8 ~ 11,
  is.na(walktime) ~ 0))
df_dm %>% tabyl(walktime,walk_h)

##力仕事時間hardworkを時間に変換
df_dm <- df_dm %>% mutate(power_h = case_when(
  hardwork == 0 ~ 0,
  hardwork == 1 ~ 0,
  hardwork == 2 ~ 0.5,
  hardwork == 3 ~ 2,
  hardwork == 4 ~ 4,
  hardwork == 5 ~ 6,
  hardwork == 6 ~ 8,
  hardwork == 7 ~ 10,
  hardwork == 8 ~ 11,
  is.na(hardwork) ~ 0))
df_dm %>% tabyl(hardwork,power_h)


####余暇の身体の動かし方の変数を頻度からカテゴリー化
###余暇歩行の頻度と時間
##ゆっくり歩く頻度を回数に変換,1が30/30と考えられる
df_dm <- df_dm %>% mutate(slowwalk_kaisu = case_when(
  LTwalkFR == 0 ~ 0,
  LTwalkFR == 1 ~ 0,
  LTwalkFR == 2 ~ 2/30,
  LTwalkFR == 3 ~ 1.5/7,
  LTwalkFR == 4 ~ 3.5/7,
  LTwalkFR == 5 ~ 1,
  is.na(LTwalkFR)  ~ 0))
table(df_dm$LTwalkFR, df_dm$slowwalk_kaisu, useNA = c("ifany"))

##ゆっくり歩く1回あたりの時間を割り当て
df_dm <- df_dm %>% mutate(slowwalk_h = case_when(
  LTwalkTM == 0 ~ 0,
  LTwalkTM == 1 ~ 0.25,
  LTwalkTM == 2 ~ 0.75,
  LTwalkTM == 3 ~ 1.5,
  LTwalkTM == 4 ~ 2.5,
  LTwalkTM == 5 ~ 3.5,
  LTwalkTM == 6 ~ 4,
  is.na(LTwalkTM)  ~ 0))
table(df_dm$LTwalkTM)

##速歩きする頻度を回数に変換,1が30/30と考えられる）
df_dm <- df_dm %>% mutate(fastwalk_kaisu = case_when(
  LTfwalkFR == 0 ~ 0,
  LTfwalkFR == 1 ~ 0,
  LTfwalkFR == 2 ~ 2/30,
  LTfwalkFR == 3 ~ 1.5/7,
  LTfwalkFR == 4 ~ 3.5/7,
  LTfwalkFR == 5 ~ 1,
  is.na(LTfwalkFR)  ~ 0))
table(df_dm$LTfwalkFR)

##速歩き1回あたりの時間を割り当て
df_dm <- df_dm %>% mutate(fastwalk_h = case_when(
  LTfwalkTM == 0 ~ 0,
  LTfwalkTM == 1 ~ 0.25,
  LTfwalkTM == 2 ~ 0.75,
  LTfwalkTM == 3 ~ 1.5,
  LTfwalkTM == 4 ~ 2.5,
  LTfwalkTM == 5 ~ 3.5,
  LTfwalkTM == 6 ~ 4,
  is.na(LTfwalkTM)  ~ 0))
df_dm %>% tabyl(LTfwalkTM,fastwalk_h)

##軽い運動の頻度を回数に変換,1が30/30と考えられる
df_dm <- df_dm %>% mutate(ex_light_kaisu = case_when(
  LTexeFR == 0 ~ 0,
  LTexeFR == 1 ~ 0,
  LTexeFR == 2 ~ 2/30,
  LTexeFR == 3 ~ 1.5/7,
  LTexeFR == 4 ~ 3.5/7,
  LTexeFR == 5 ~ 1,
  is.na(LTexeFR)  ~ 0))

##軽い運動1回あたりの時間を割り当て
df_dm <- df_dm %>% mutate(ex_light_h = case_when(
  LTexeTM == 0 ~ 0,  
  LTexeTM == 1 ~ 0.25,
  LTexeTM == 2 ~ 0.75,
  LTexeTM == 3 ~ 1.5,
  LTexeTM == 4 ~ 2.5,
  LTexeTM == 5 ~ 3.5,
  LTexeTM == 6 ~ 4,
  is.na(LTexeTM)  ~ 0))
df_dm %>% tabyl(LTexeTM,ex_light_h)

##激しい運動の頻度を回数に変換,1が30/30と考えられる
df_dm <- df_dm %>% mutate(ex_strenuous_kaisu = case_when(
  LThexeFR == 0 ~ 0,  
  LThexeFR == 1 ~ 0,
  LThexeFR == 2 ~ 2/30,
  LThexeFR == 3 ~ 1.5/7,
  LThexeFR == 4 ~ 3.5/7,
  LThexeFR == 5 ~ 1,
  is.na(LThexeFR)  ~ 0))

##激しい運動1回あたりの時間を割り当て
df_dm <- df_dm %>% mutate(ex_strenuous_h = case_when(
  LThexeTM == 1 ~ 0.25,
  LThexeTM == 2 ~ 0.75,
  LThexeTM == 3 ~ 1.5,
  LThexeTM == 4 ~ 2.5,
  LThexeTM == 5 ~ 3.5,
  LThexeTM == 6 ~ 4,
  is.na(LThexeTM)  ~ 0))
df_dm %>% tabyl(LTexeTM,ex_light_h)

##睡眠時間の時間を割り当て
df_dm <- df_dm %>% mutate(sleep_h = case_when(
  sleep == 0 ~ 0,
  sleep == 1 ~ 2.5,
  sleep == 2 ~ 6,
  sleep == 3 ~ 7,
  sleep == 4 ~ 8,
  sleep == 5 ~ 9,
  sleep == 6 ~ 11,
  is.na(sleep)  ~ 0))
df_dm %>% tabyl(sleep,sleep_h)

##合計時間を作成する
df_dm <- df_dm %>% mutate(sigoto_h = round(sit_h + stand_h + walk_h + power_h, .1))
df_dm <- df_dm %>% mutate(yoka_h = round(slowwalk_kaisu*slowwalk_h + fastwalk_kaisu*fastwalk_h +
                                     ex_light_kaisu*ex_light_h + ex_strenuous_kaisu*ex_strenuous_h, .1))
#その他の時間を割り当てるためのグループ分け
df_dm <- df_dm %>% mutate(metsgroup = case_when(
  sleep_h == 0 & sigoto_h == 0   ~ 4,
  sleep_h == 0 & sigoto_h > 0    ~ 3,
  sleep_h > 0 & sigoto_h == 0    ~ 2,
  sleep_h > 0 & sigoto_h > 0     ~ 1,
  TRUE ~ 0))
#その他の時間を作成し、24時間超えは時間圧縮
df_dm <- df_dm %>% mutate(add = sleep_h + sigoto_h + yoka_h)
df_dm <- df_dm %>% mutate(other = case_when(
  metsgroup == 1 & add > 0 & add <= 24 ~ 24 - add,
  metsgroup == 2 & add > 0 & add <= 24 ~ 24 - add,
  TRUE ~ 0))
df_dm <- df_dm %>% mutate(sleep1 = case_when(
  metsgroup == 3 & add > 0 & add <= 24 ~ 24 - add,
  TRUE ~ 0))
df_dm <- df_dm %>% mutate(hi = case_when(
  add > 24 ~ 24/add,
  TRUE ~ 1))
#すべての活動時間を作成
df_dm <- df_dm %>% mutate(alltime = round(
  hi*(sit_h + stand_h + walk_h + power_h +
        slowwalk_kaisu*slowwalk_h + fastwalk_kaisu*fastwalk_h +
        ex_light_kaisu*ex_light_h + ex_strenuous_kaisu*ex_strenuous_h +
        sleep_h + other + sleep1),.1))
#Metsを計算
df_dm <- df_dm %>% mutate(mets = round(
  hi*(sit_h*1.3 + stand_h*2 + walk_h*3 + power_h*6 +
        slowwalk_kaisu*slowwalk_h*2.8 + fastwalk_kaisu*fastwalk_h*4 +
        ex_light_kaisu*ex_light_h*3 + ex_strenuous_kaisu*ex_strenuous_h*6 +
        sleep_h*0.9 + other*1.3 + sleep1*0.9),.1))
df_dm <- df_dm %>% mutate(mets = case_when(
  metsgroup == 4 ~ NA_real_,
  metsgroup == 4 & sit_h == 0 & stand_h == 0 & walk_h == 0 & power_h == 0 ~ 7*0.9 + 17*1.3,
  TRUE ~ mets))

#Metsの分布を確認
hist(df_dm$mets)
summary(df_dm$mets)


####地域の変数を作成
df_dm <- df_dm %>% mutate(area = case_when(
  str_sub(id,1,2) == "A1" ~ "Yokote",
  str_sub(id,1,2) == "A2" ~ "Saku",
  str_sub(id,1,2) == "A3" ~ "Ninohe",
  str_sub(id,1,2) == "G0" ~ "Chikusei",
  str_sub(id,1,2) == "E0" ~ "Ozu",
  str_sub(id,1,2) == "F2" ~ "Kochi",
  str_sub(id,1,2) == "B1" ~ "Nagasaki",
  T ~ "NA"),
  area = as.factor(area))


