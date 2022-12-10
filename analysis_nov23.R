##LOAD ANALYSIS PACKAGES
library(lmtest)
library(sandwich)
library(clubSandwich)
library(multiwaycov)
library(MASS)
library(texreg)
library(alpaca)

library(pglm)
library(vtable)
library(ggsci)
library(rlang)
library(tidyverse)

require(pscl)
require(boot)
library(stargazer)


#### ALL ANALYSIS ¤¤¤¤

###LOAD BOTH DATASETS
##main session
{
  
  data_full <- read.csv("replication_data_nov23.csv")
  
  p4 <- data_full %>% group_by(year_speech, period, name, pref_id) %>% summarize()
  see <- p4 %>% group_by(period, name, pref_id) %>% summarize(N = n())                                                                                                                             
  see <- see %>% group_by(period) %>% mutate(year_n = N, max = max(N), below = N<max)
  data_full <- merge(data_full, see, by = c("period", "name", "pref_id"))
  
  
  p2 <- data_full %>% group_by(year = period, name, reelect, mutohyo, party, age, totseat, 選挙区名, pref_id,year_n
                               ) %>% summarize(obs = length(unique(V1, type)),
                                                                                                                               obs_2 = length(V1),
                                                                                                                               obs_3 = length(unique(date)))
  p2$reelect <- ifelse(is.na(p2$reelect), 0, p2$reelect)
  
  ##load data on ALL elected politicians
  {
    cand_ <- read.csv("cand_data_may13.csv")
    cand_ <- cand_[cand_$pref_id %in% unique(p2$pref_id),]
    cand_ <- cand_[cand_$win == TRUE,]
    
    cand_2 <- read.csv("elec_varsfeb26.csv")
    
    cand_2 <- cand_2[cand_2$pref_id %in% unique(p2$pref_id),]
    
    cand_$X <- NULL
    cand_2$X <- NULL
    
    cand_3 <- merge(cand_, cand_2, by = c("pref_id","選挙区名", "name", "year_merge") )
    cand_3$year <- cand_3$period
    cand_3$incum <- substr(cand_3$party, 2,2)
    cand_3$party <- substr(cand_3$party, 1,1)
    cand_3$mutohyo <- ifelse(is.na(cand_3$mutohyo), 0, cand_3$mutohyo)
    cand_3$reelect <- ifelse(is.na(cand_3$reelect), 0, cand_3$reelect)
    {
      cand_3$name[cand_3$name %in% c("花城ダイスケ") & cand_3$pref_id == 47] <- "花城大輔" #u1 clean
      cand_3$name[cand_3$name %in% c("宮城イチロー") & cand_3$pref_id == 47] <- "宮城一郎" #u1 clean
      cand_3$name[cand_3$name %in% c("玉城ノブコ") & cand_3$pref_id == 47] <- "玉城ノブ子" #u1 clean
      cand_3$name[cand_3$name %in% c("玉城みつる") & cand_3$pref_id == 47] <- "玉城満" #u1 clean
      cand_3$name[cand_3$name %in% c("金城ツトム") & cand_3$pref_id == 47] <- "金城勉" #u1 clean
      cand_3$name[cand_3$name %in% c("金城ヤスクニ") & cand_3$pref_id == 47] <- "金城泰邦" #u1 clean
      cand_3$name[cand_3$name %in% c("具志堅トオル") & cand_3$pref_id == 47] <- "玉城ノブ子" #u1 clean
      cand_3$name[cand_3$name %in% c("山川のりじ") & cand_3$pref_id == 47] <- "山川典二" #u1 clean
      cand_3$name[cand_3$name %in% c("照屋タイガ") & cand_3$pref_id == 47] <- "照屋大河" #u1 clean
      cand_3$name[cand_3$name %in% c("照屋モリユキ") & cand_3$pref_id == 47] <- "照屋守之" #u1 clean
      
      cand_3$name[cand_3$name %in% c("上原あきら") & cand_3$pref_id == 47] <- "上原章" #u1 clean
      cand_3$name[cand_3$name %in% c("上原まさじ") & cand_3$pref_id == 47] <- "上原正次" #u1 clean
      cand_3$name[cand_3$name %in% c("新垣アラタ") & cand_3$pref_id == 47] <- "新垣新" #u1 clean
      cand_3$name[cand_3$name %in% c("金城ヤスクニ") & cand_3$pref_id == 47] <- "金城泰邦"
      cand_3$name[cand_3$name %in% c("花城ダイスケ") & cand_3$pref_id == 47] <-"花城大輔"
      cand_3$name[cand_3$name %in% c("翁長まさとし") & cand_3$pref_id == 47] <-"翁長政俊"
      cand_3$name[cand_3$name %in% c("玉城みつる") & cand_3$pref_id == 47] <-"玉城満"
      cand_3$name[cand_3$name %in% c("照屋タイガ") & cand_3$pref_id == 47] <-"照屋大河"
      cand_3$name[cand_3$name %in% c("比嘉みずき") & cand_3$pref_id == 47] <-"比嘉瑞己"
      cand_3$name[cand_3$name %in% c("新垣アラタ") & cand_3$pref_id == 47] <-"新垣新"
      cand_3$name[cand_3$name %in% c("島袋ダイ") & cand_3$pref_id == 47] <-"島袋大"
      cand_3$name[cand_3$name %in% c("山川のりじ") & cand_3$pref_id == 47] <-"山川典二"
      cand_3$name[cand_3$name %in% c("宮城イチロー") & cand_3$pref_id == 47] <-"宮城一郎"
      cand_3$name[cand_3$name %in% c("大浜イチロー") & cand_3$pref_id == 47] <-"大浜一郎"
      cand_3$name[cand_3$name %in% c("大城ノリユキ") & cand_3$pref_id == 47] <-"大城憲幸"
      cand_3$name[cand_3$name %in% c("又吉セイギ") & cand_3$pref_id == 47] <-"又吉清義"
      cand_3$name[cand_3$name %in% c("仲宗根サトル") & cand_3$pref_id == 47] <-"仲宗根悟"
      cand_3$name[cand_3$name %in% c("中川キョウキ") & cand_3$pref_id == 47] <-"中川京貴"
      cand_3$name[cand_3$name %in% c("上原まさじ") & cand_3$pref_id == 47] <-"上原正次"
      cand_3$name[cand_3$name %in% c("じろく成崇") & cand_3$pref_id == 47] <-"次呂久成崇"
      cand_3$name[cand_3$name %in% c("たまき武光") & cand_3$pref_id == 47] <-"玉城武光"
      cand_3$name[cand_3$name %in% c("とう山勝利") & cand_3$pref_id == 47] <-"当山勝利"
      cand_3$name[cand_3$name %in% c("とぐち修") & cand_3$pref_id == 47] <-"渡久地修"
      cand_3$name[cand_3$name %in% c("にった宜明") & cand_3$pref_id == 47] <-"新田宜明"
      cand_3$name[cand_3$name %in% c("オド良太郎") & cand_3$pref_id == 47] <-"小渡良太郎"
      cand_3$name[cand_3$name %in% c("オヤ川敬") & cand_3$pref_id == 47] <-"親川敬"
      cand_3$name[cand_3$name %in% c("ギマ光秀") & cand_3$pref_id == 47] <-"儀間光秀"
      cand_3$name[cand_3$name %in% c("サキ山嗣幸") & cand_3$pref_id == 47] <-"崎山嗣幸"
      cand_3$name[cand_3$name %in% c("ザキミ一幸") & cand_3$pref_id == 47] <-"座喜味一幸"
      cand_3$name[cand_3$name %in% c("ザハはじめ") & cand_3$pref_id == 47] <-"座波一"
      cand_3$name[cand_3$name %in% c("スエマツ文信") & cand_3$pref_id == 47] <-"末松文信"
      cand_3$name[cand_3$name %in% c("ズケラン功") & cand_3$pref_id == 47] <-"瑞慶覧功"
      cand_3$name[cand_3$name %in% c("セナガ美佐雄") & cand_3$pref_id == 47] <-"瀬長美佐雄"
      cand_3$name[cand_3$name %in% c("タイラ昭一") & cand_3$pref_id == 47] <-"平良昭一"
      cand_3$name[cand_3$name %in% c("ナカザト全孝") & cand_3$pref_id == 47] <-"仲里全孝"
      cand_3$name[cand_3$name %in% c("ナカムラ家治") & cand_3$pref_id == 47] <-"仲村家治"
      cand_3$name[cand_3$name %in% c("ニシメ啓史郎") & cand_3$pref_id == 47] <-"西銘啓史郎"
      cand_3$name[cand_3$name %in% c("ニシメ純恵") & cand_3$pref_id == 47] <-"西銘純恵"
      cand_3$name[cand_3$name %in% c("なかむらみお") & cand_3$pref_id == 47] <-"仲村未央"
      cand_3$name[cand_3$name %in% c("具志堅トオル") & cand_3$pref_id == 47] <-"具志堅透"
      #(iii)HORIUCHI has double entries (eg) & cand_3$pref_id == 47] <-both kanji & hiragana/katakana)
      #GIJI has one each entry
      cand_3$name[cand_3$name %in% c("いとす朝則") & cand_3$pref_id == 47] <-"糸洲朝則"
      cand_3$name[cand_3$name %in% c("かりまたのぶこ") & cand_3$pref_id == 47] <-"狩俣信子"
      cand_3$name[cand_3$name %in% c("ひが京子") & cand_3$pref_id == 47] <-"比嘉京子"
      cand_3$name[cand_3$name %in% c("アラカキ清涼") & cand_3$pref_id == 47] <-"新垣清涼"
      cand_3$name[cand_3$name %in% c("カヨウ宗儀") & cand_3$pref_id == 47] <-"嘉陽宗儀"
      cand_3$name[cand_3$name %in% c("ゴヤ宏") & cand_3$pref_id == 47] <-"呉屋宏"
      cand_3$name[cand_3$name %in% c("シンザト米吉") & cand_3$pref_id == 47] <-"新里米吉"
      cand_3$name[cand_3$name %in% c("ナカダ弘毅") & cand_3$pref_id == 47] <-"仲田弘毅"
      cand_3$name[cand_3$name %in% c("大城一マ") & cand_3$pref_id == 47] <-"大城一馬"
      cand_3$name[cand_3$name %in% c("當山真市") & cand_3$pref_id == 47] <-"当山真市"
      cand_3$name[cand_3$name %in% c("玉城ノブコ") & cand_3$pref_id == 47] <-"玉城ノブ子"
      cand_3$name[cand_3$name %in% c("金城ツトム") & cand_3$pref_id == 47] <-"金城勉"
      cand_3$name[cand_3$name %in% c("照屋モリユキ") & cand_3$pref_id == 47] <-"照屋守之"
      cand_3$name[cand_3$name %in% c("上原あきら") & cand_3$pref_id == 47] <-"上原章"
      #(iv)Names in both GIJI and HORIUCHI are incorrect
      #ie) & cand_3$pref_id == 47] <-GIJI writes traditional and HORIUCHI writes hiragana/katakana
      #Both GIJI and HORIUCHI have one each entry
      #GIJI writes "吉田勝廣" and HORIUCHI writes "ヨシダ勝広"
      #m_data["name"] = m_data["name"].replace("吉田勝廣") & cand_3$pref_id == 47] <-"吉田勝広"    
      #cand_3$name[cand_3$name %in% c("ヨシダ勝広","吉田勝広"    
      #GIJI writes "當間盛夫" and HORIUCHI writes "当間モリオ"
      #(HORIUCHI also has entry "當間盛夫")
      #m_data["name"] = m_data["name"].replace("當間盛夫") & cand_3$pref_id == 47] <-"当間盛夫"
      cand_3$name[cand_3$name %in% c("當間盛夫") & cand_3$pref_id == 47] <-"当間盛夫"
      cand_3$name[cand_3$name %in% c("当間モリオ") & cand_3$pref_id == 47] <-"当間盛夫"
      #GIJI writes "亀濱玲子" and HORIUCHI writes "亀浜レイコ"
      #m_data["name"] = m_data["name"].replace("亀濱玲子") & cand_3$pref_id == 47] <-"亀浜玲子"
      cand_3$name[cand_3$name %in% c("亀浜レイコ") & cand_3$pref_id == 47] <-"亀浜玲子"
      #(PATTERN B) ONLY HORIUCHI has entries
      #(v)Names found only in horiuchi once
      #replace hiragana/katakana -> kanji
      cand_3$name[cand_3$name %in% c("赤嶺ノボル") & cand_3$pref_id == 47] <-"赤嶺昇"
      cand_3$name[cand_3$name %in% c("ひがせいじん") & cand_3$pref_id == 47] <-"比嘉清仁"
      cand_3$name[cand_3$name %in% c("コクバ栄正") & cand_3$pref_id == 47] <-"国場栄正"
      cand_3$name[cand_3$name %in% c("なかまつ勤") & cand_3$pref_id == 47] <-"仲松勤"
      cand_3$name[cand_3$name %in% c("マエツ究") & cand_3$pref_id == 47] <-"前津究"
      cand_3$name[cand_3$name %in% c("モリネ伸夫") & cand_3$pref_id == 47] <-"森根伸夫"
      cand_3$name[cand_3$name %in% c("やまざと昌輝") & cand_3$pref_id == 47] <-"山里昌輝"
      cand_3$name[cand_3$name %in% c("伊佐ミツオ") & cand_3$pref_id == 47] <-"伊佐光雄"
      cand_3$name[cand_3$name %in% c("嘉手納ナマブ") & cand_3$pref_id == 47] <-"嘉手納学"
      cand_3$name[cand_3$name %in% c("山城せいじ") & cand_3$pref_id == 47] <-"山城誠司"
      cand_3$name[cand_3$name %in% c("大城たみお") & cand_3$pref_id == 47] <-"大城民夫"
      cand_3$name[cand_3$name %in% c("ザマミ邦昭") & cand_3$pref_id == 47] <-"座間味邦昭"
    }
                               }
  
  ###create zeroes by merging all available election data
  p2 <- merge(p2, cand_3, by = c("pref_id","選挙区名", "name", "year", "mutohyo", "party",
                                 "reelect", "age", "totseat"), all.y = T)
  
  
  ##create variables regarding previous job experience
  {
    p2$secretary <- grepl("議員秘書",p2$prev_job)
    p2$city_assembly <- grepl("市議",p2$prev_job)
    p2$lawyer <- grepl("弁護",p2$prev_job)
    
    p2$party_boss <- grepl("党県議団長|党県幹事長|党県副会長|県議秘書|党県会長|党県総務会長|党県副代表|党県代表|党県総務会長|党県政調会長|党県代表|
      党県総務|県議会派幹事長", p2$prev_job)
    
    p2$current <- sapply(strsplit(p2$prev_job, "〈元〉"), "[", 1)
    
    p2$party_boss_2 <- grepl("党県議団長|党県幹事長|党県副会長|県議秘書|党県会長|党県総務会長|党県副代表|党県代表|党県総務会長|党県政調会長|党県代表|
      党県総務|県議会派幹事長", p2$current)
    p2$c_speaker <- grepl("県議長|県議会議長|県議会副議長|県副議長|府議長|府議会議長|府副議長|府議会副議長", p2$current)
    p2$c_speaker_2 <- grepl("県議長|県議会議長|府議長|府議会議長", p2$current)
    
  }
  
  
  ### create new obs variable giving zeroes to unknown cases
  {
    p2 <- p2[variable.names(p2)]
    
    p2$obs_4 <- ifelse(is.na(p2$obs_2), 0, p2$obs_2)
    p2$obs_5 <- ifelse(is.na(p2$obs_3), 0, p2$obs_3)
    
    p2$missing <- ifelse(is.na(p2$obs), 1, 0)
    p2$un_full <- ifelse(p2$un_full >= 3, 3, p2$un_full) #mmakes it so 3 is 3 or more times uncontested
    
    p2$un_full_2 <- ifelse(is.na(p2$un_full), 1, p2$un_full)
    p2$party <- ifelse(!p2$party %in% c("自","共","無","公","民","社","維"), "他", p2$party)
    
    p2 <- within(p2, party <- relevel(as.factor(party), ref = "自")) ##set the new reference for party to LDP
    p2$smd <- ifelse(p2$totseat == 1, 1, 0)
    p2$ldp <- ifelse(p2$party == "自", 1, 0)
    
    
    p2$totseat_2 <- ifelse(p2$totseat >= 4, 4, p2$totseat)
    
    p2 <- p2 %>% mutate(reelect_2 = case_when(reelect %in% c(1,2) ~ 1,
                                              reelect %in% c(3,4,5) ~ 2,
                                              reelect %in% c(6,7,8) ~ 3,
                                              reelect %in% c(9,10) ~ 4,
                                              reelect > 10 ~ 5))
    
  }
  
  
  p2 <- p2 %>% group_by(pref_id, year) %>% mutate(maj = n()/2, 
                                                  ldp_s = sum(party == "自"), ldp_maj = ldp_s > maj,
                                                  dpj_s = sum(party == "民"), dpj_maj = dpj_s > maj,
                                                  un_s = sum(party == "無"), un_maj = un_s > maj,
                                                  ishin_s = sum(party == "維"), ishin_maj = ishin_s > maj,
                                                  rul_p = (ldp == 1 & ldp_maj == TRUE)
  )
  
  ## combine with district level data
  {
    dist_data <- read.csv("estat_population_oct28.csv")
    
    p2$distname_p <- p2$選挙区名
    p2 <- merge(p2, dist_data, by = c("pref_id", "distname_p"), all.x = T)
  }
  
  ##remove speakers and those in leadership positions and speeches outside analysis period
  p2_filter <- p2 %>% filter(year != 5, year != 0)
  p2_filter <- p2_filter %>% filter(c_speaker_2 != 1)
  data_main <- p2_filter
  rm(cand_, cand_2, cand_3, data_full, dist_data, dist_data2, p2, p2_filter)
}

#### dont use until Aso updates the file - Included now, can be used
##load governors party
{
gov_2 <- read.csv("gov_period_oct25.csv")
gov_2 <- gov_2[!is.na(gov_2$period),]

gov_2 <- rename(gov_2, year = period)
data_main <- merge(data_main, gov_2, by = c("pref_id", "year"))

data_main$party_2 <- ifelse(!data_main$party %in% c("自","共","無","公","民","社","維"), "他", data_main$party)
###create a governor ruling party dummy
data_main$gov_suisen <- ifelse(is.na(data_main$gov_suisen), "None",data_main$gov_suisen)
data_main$rul_1 <- data_main$party %in% data_main$gov_suisen
data_main$ainori <- ifelse(is.na(data_main$ainori), 0,data_main$ainori)
data_main$rul_2 <- data_main$ainori == 1 & (data_main$party %in% c("自","無","公","民","維"))
data_main$anti_ldp <- ifelse(is.na(data_main$anti_ldp), 0,data_main$anti_ldp)
data_main$rul_3 <- data_main$anti_ldp == 1 & (data_main$party %in% c("無","民","社"))
data_main$rul_4 <- data_main$anti_ldp == 0 & (data_main$party %in% c("自","公"))


data_main$gov_ruling <- ifelse(data_main$rul_2 == T| data_main$rul_3 == T| data_main$rul_4 == T, T, F)
}

### LOAD gender variable (October 2022)
{
gender_df <- read_csv("aso_gender_oct.csv")

##fix gender party column
gender_df$party <- ifelse(!gender_df$party_2 %in% c("自","共","無","公","民","社","維"), "他", gender_df$party_2)
gender_df <- within(gender_df, party <- relevel(as.factor(party), ref = "自"))

gender_df <- gender_df[c("pref_id", "name", "sex", "party")]
data_main <- merge(data_main, gender_df, by = c("pref_id", "name", "party"))
data_main <- data_main[!duplicated(data_main),]
}


### data problem nov 21. 
## pref 47 should be excluded because only year 2003 is correct
## pref 6 is fixed now
## pref 28 and 10 can be reuploaded after they have been updated, not neccessarily a problem

data_main <- data_main %>% filter(!pref_id %in% c(47))

#see <- data_main %>% group_by(pref_id) %>% summarise(mean(obs_5))
see_1 <- data_main %>% filter(pref_id == 47)


### FIND UNIQUE NUMBER OF POLITICIANS IN ANALYSIS (abstract and p.12)
nrow(data_main %>% group_by(pref_id, name) %>% summarise(mutohyo = max(mutohyo)))
test <- data_main %>% group_by(pref_id, name) %>% summarise(mutohyo = max(mutohyo))
data_main %>% group_by(mutohyo) %>% summarise(n())
nrow(unique(test[test$mutohyo == 1,]))/nrow(test)
data_main %>% group_by(mutohyo) %>% summarise(n()/nrow(data_main))


###table 1 - descriptive statistics
out <- data_main[ , names(data_main) %in% c("year_election","obs_4", "obs_5","mutohyo", "age", "city_assebly", "reelect", "totseat", 
                                            "meansize_10k", "pop_65over_perc", "year", "pref_id", "smd", "sex",
                                            "secretary","city_assembly","gov_ruling"),]
out$gov_ruling <-as.numeric(out$gov_ruling)
out$secretary <-as.numeric(out$secretary)
out$city_assembly <-as.numeric(out$city_assembly)

out$un_full_2 <- as.factor(out$un_full_2)
sumtable(out, factor.percent = T,out='html',file='C:/Users/robba/Dropbox/Politician_effectiveness_paper/regression_models_july22/tabS1_main_nov23.html')




### Table 2 - MAIN SESSION 
##may 24 note: obs_5 gives nicer results, consider changing
###oct 21 note, remember to include governors party when ready
{
  m1 <- glm.nb(obs_5 ~ factor(mutohyo) + factor(year) + factor(pref_id), data = data_main)
  cov1 <- vcov(m1, cluster = data_main$pref_id)
  cov_m1    <- sqrt(diag(cov1))

  m2  <- glm.nb(obs_5 ~ factor(mutohyo) + reelect + factor(year)+ factor(pref_id), data = data_main)
  cov2 <- vcov(m2, cluster = data_main$pref_id)
  cov_m2    <- sqrt(diag(cov2))

  m3  <- glm.nb(obs_5 ~ factor(mutohyo) + reelect + sex + totseat+ secretary + city_assembly + party_boss
                + party+ gov_ruling + age + meansize_10k + pop_65over_perc+ factor(year)+ factor(pref_id), data = data_main)
  cov3 <- vcov(m3, cluster = data_main$pref_id)
  cov_m3    <- sqrt(diag(cov3))

  m4  <- glm.nb(obs_5 ~ factor(mutohyo) + totseat + sex  + totseat + secretary + city_assembly + party_boss
                + party+ gov_ruling+ meansize_10k + pop_65over_perc + factor(year)+ factor(pref_id), data = data_main)
  cov4 <- vcov(m4, cluster = data_main$pref_id)
  cov_m4    <- sqrt(diag(cov4))
  m5  <- glm.nb(obs_5 ~ factor(mutohyo) + reelect + sex  + secretary + city_assembly + party_boss
                + party+ gov_ruling + age+ meansize_10k + pop_65over_perc+ factor(year)+ factor(pref_id), data = data_main)
  cov5 <- vcov(m5, cluster = data_main$pref_id)
  cov_m5    <- sqrt(diag(cov5))

  m6  <- glm.nb(obs_5 ~ factor(mutohyo) + sex  + secretary + city_assembly + party_boss
                + party + gov_ruling+ meansize_10k + pop_65over_perc + factor(year)+ factor(pref_id), data = data_main)
  cov6 <- vcov(m6, cluster = data_main$pref_id)
  cov_m6    <- sqrt(diag(cov6))

  m7  <- glm.nb(obs_5 ~ factor(mutohyo) + sex  +secretary + city_assembly  + party_boss
                + gov_ruling+ age+ meansize_10k + pop_65over_perc+ factor(year)+ factor(pref_id), data = data_main)
  cov7 <- vcov(m7, cluster = data_main$pref_id)
  cov_m7    <- sqrt(diag(cov7))

  m10 <- glm.nb(obs_5 ~ factor(un_full_2) + factor(year)+ factor(pref_id), data = data_main)
  cov10 <- vcov(m10, cluster = data_main$pref_id)
  cov_m10    <- sqrt(diag(cov10))

  
  m11 <- glm.nb(obs_5 ~ factor(un_full_2) + totseat+reelect + sex  + secretary + city_assembly + party_boss 
                + party + gov_ruling + age+ meansize_10k + pop_65over_perc + factor(year)+ factor(pref_id), data = data_main)
  cov11 <- vcov(m11, cluster = data_main$pref_id)
  cov_m11    <- sqrt(diag(cov11))
  
  m12 <- glm.nb(obs_5 ~ factor(un_full_2) + totseat + secretary + sex  + city_assembly + party_boss 
                + party + gov_ruling + meansize_10k + pop_65over_perc + factor(year)+ factor(pref_id), data = data_main)
  cov12 <- vcov(m12, cluster = data_main$pref_id)
  cov_m12    <- sqrt(diag(cov12))
  
  m13 <- glm.nb(obs_5 ~ factor(un_full_2) + reelect + secretary + sex  + city_assembly + party_boss 
                + party + gov_ruling + age+ meansize_10k + pop_65over_perc+ factor(year)+ factor(pref_id), data = data_main)
  cov13 <- vcov(m13, cluster = data_main$pref_id)
  cov_m13    <- sqrt(diag(cov13))
  
  m14 <- glm.nb(obs_5 ~ factor(un_full_2) + sex  + secretary + city_assembly + party_boss 
                + party + gov_ruling + meansize_10k + pop_65over_perc+ factor(year)+ factor(pref_id), data = data_main)
  cov14 <- vcov(m14, cluster = data_main$pref_id)
  cov_m14    <- sqrt(diag(cov14))

  stargazer(m1,m3,m4,m5,m6, type = "text",
            se = list(cov_m1, cov_m3,cov_m4,cov_m5,cov_m6))
  order=c(1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,2)
  
  #stargazer(m1,m3,m4,m5,m6,m10,m11,m12,m13,m14, type = "text",add.lines=list(c("Time effects", "YES", "YES", "YES", "YES", "YES", "YES"),
   #                                                                          c("Prefecture effects", "YES", "YES", "YES", "YES", "YES", "YES")),
    #        se = list(cov_m1, cov_m3,cov_m4,cov_m5,cov_m6,cov_m10,cov_m11,cov_m12,cov_m13,cov_m14), omit = c("year", "pref_id"))
  
  stargazer(m1,m3,m4,m5,m6,m10,m11,m12,m13,m14, type = "html",covariate.labels = c("Uncontested","Tenure", "Sex (female)", "DistrictMagnitude",
                                                                                   "Political Secretary", "Former city assembly", "Party leader",
                                                                                   "Party Ishin", "Party JCP", "Party Komei",
                                                                                   "Party SDP", "Party Other", "Party DPJ", 
                                                                                   "Party Unaffiliated", "Alligned with governor", "Age", 
                                                                                   "Avg. muncipality pop (10,000)", "elderly % (65+)", 
                                                                                   "Uncontested (1 term)" ,"Uncontested (2 terms)", "Uncontested (3+ terms)"
                                                                                    ),   
     add.lines=list(c("Time effects", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES"),
                           c("Prefecture effects", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES")),
            se = list(cov_m1, cov_m3,cov_m4,cov_m5,cov_m6,cov_m10,cov_m11,cov_m12,cov_m13,cov_m14), omit = c("year", "pref_id"),
            out = "C:/Users/robba/Dropbox/Politician_effectiveness_paper/regression_models_july22/tab1_nov29.html")
  
  stargazer(m1,m3,m4,m5,m6, type = "html",
            
            add.lines=list(c("Time effects", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES"),
                           c("Prefecture effects", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES"),
                           c("Control variables", "NO", "YES", "YES", "YES", "YES", "NO", "YES", "YES", "YES", "YES"),
                           c("Party dummies", "NO", "YES", "YES", "YES", "YES", "NO", "YES", "YES", "YES", "YES"),
                           c("Demographic variables", "NO", "YES", "YES", "YES", "YES", "NO", "YES", "YES", "YES", "YES")),
            se = list(cov_m1, cov_m3,cov_m4,cov_m5,cov_m6,cov_m10,cov_m11,cov_m12,cov_m13,cov_m14), omit = c("year", "pref_id", "secretary", 
                                                                                                             "city_assembly", "party", "gov_ruling",
                                                                                                             "meansize_10k", "pop_65over_perc"),
            covariate.labels = c("Uncontested","Tenure", "Sex (female)", "DistrictMagnitude", "Age"),
  out = "C:/Users/robba/Dropbox/Politician_effectiveness_paper/regression_models_july22/tab1_dec1_shortened.html")

  }




### table 3 - characteristics of contested and uncontested districts ###
data_main$mean_size
out <- data_main %>% group_by(mutohyo, un_full_2) %>% summarise(mean(totseat),
                                              mean(age),
                                              mean(reelect),
                                              mean(mean_size, na.rm = T)) %>% mutate(across(where(is.numeric), ~ round(., 3))) %>%
  rename(Contestation = mutohyo, " " = un_full_2, "Avg. magnitude" = "mean(totseat)", "Avg. age" = "mean(age)", "Avg. tenure" = "mean(reelect)", 
         "Avg. municipality pop" = "mean(mean_size, na.rm = T)")

out_1 <- data_main %>% group_by(mutohyo) %>% summarise(mean(totseat),
                                                                mean(age),
                                                                mean(reelect),
                                                                mean(mean_size, na.rm = T)) %>% mutate(across(where(is.numeric), ~ round(., 3))) %>%
  rename(Contestation = mutohyo, "Avg. magnitude" = "mean(totseat)", "Avg. age" = "mean(age)", "Avg. tenure" = "mean(reelect)",
         "Avg. municipality pop" = "mean(mean_size, na.rm = T)")
out <- out[-2,] 
out <- rbind(out[1,], out_1[2,], out[2:4,])

out[1] <- c("Contested", "Uncontested", "","","")
out[2] <- c("", "", "Uncontested 1 term","Uncontested 2 terms","Uncontested 3 terms +")



stargazer(out, type = "text", summary=FALSE, rownames=FALSE, digits=1, digits.extra=1)
stargazer(out, type = "html", summary=FALSE, rownames=FALSE, digits=1, digits.extra=1,
          out = "C:/Users/robba/Dropbox/Politician_effectiveness_paper/regression_models_july22/tab3_oct25.html")



### calculate for p.19
mutohyo_n <- data_main %>% group_by(mutohyo) %>% summarize(n())
n_seats <- data_main %>% group_by(mutohyo, totseat) %>% summarise(n())
n_seats$perc_mut <- n_seats$`n()`/mutohyo_n$`n()`[2]

mutohyo_n <- data_main_2 %>% group_by(mutohyo) %>% summarize(n())
n_seats <- data_main %>% group_by(mutohyo, meansize_2) %>% summarise(n())
n_seats$perc_mut <- n_seats$`n()`/mutohyo_n$`n()`[2]


### party boss analysis
m14 <- glm(party_boss ~ factor(mutohyo) + sex  + secretary + city_assembly
              + party + gov_ruling + meansize_10k + pop_65over_perc+ factor(year)+ factor(pref_id), data = data_main,
           family = binomial(link = "logit"))
cov14 <- vcov(m14, cluster = data_main$pref_id)
cov_m14    <- sqrt(diag(cov14))
summary(m14)





##comittee
{
  ##for comittees
  data_full <- read.csv( "comittee_replication_data_oct21.csv")
  p2 <- data_full %>% group_by(year = period, name, mutohyo, party, reelect, age, totseat, 選挙区名, pref_id) %>% summarize(obs = length(unique(V1, type)),
                  obs_2 = length(V1),
                  obs_3 = length(unique(date)))
  ##load data on ALL elected politicians
  {
    cand_ <- read.csv("cand_data_may13.csv")
    cand_ <- cand_[cand_$pref_id %in% unique(p2$pref_id),]
    cand_ <- cand_[cand_$win == TRUE,]
    
    cand_2 <- read.csv("elec_varsfeb26.csv")
    
    cand_2 <- cand_2[cand_2$pref_id %in% unique(p2$pref_id),]
    
    cand_$X <- NULL
    cand_2$X <- NULL
    
    cand_3 <- merge(cand_, cand_2, by = c("pref_id","選挙区名", "name", "year_merge") )
    cand_3$year <- cand_3$period
    cand_3$incum <- substr(cand_3$party, 2,2)
    cand_3$party <- substr(cand_3$party, 1,1)
    cand_3$mutohyo <- ifelse(is.na(cand_3$mutohyo), 0, cand_3$mutohyo)
    cand_3$reelect <- ifelse(is.na(cand_3$reelect), 0, cand_3$reelect)
  }
  ##set cand_3 to only imcldue prefectures included
  cand_3 <- cand_3[cand_3$pref_id %in% unique(p2$pref_id),]
  
  ###create zeroes by merging all available election data
  p2 <- merge(p2, cand_3, by = c("pref_id","選挙区名", "name", "year", "mutohyo", "party",
                                 "reelect", "age", "totseat"), all.y = T)
  
  
  ##create variables regarding previous job experience
  {
    p2$secretary <- grepl("議員秘書",p2$prev_job)
    p2$city_assembly <- grepl("市議",p2$prev_job)
    p2$lawyer <- grepl("弁護",p2$prev_job)
    
    p2$party_boss <- grepl("党県議団長|党県幹事長|党県副会長|県議秘書|党県会長|党県総務会長|党県副代表|党県代表|党県総務会長|党県政調会長|党県代表|
      党県総務|県議会派幹事長", p2$prev_job)
    
    p2$current <- sapply(strsplit(p2$prev_job, "〈元〉"), "[", 1)
    
    p2$party_boss_2 <- grepl("党県議団長|党県幹事長|党県副会長|県議秘書|党県会長|党県総務会長|党県副代表|党県代表|党県総務会長|党県政調会長|党県代表|
      党県総務|県議会派幹事長", p2$current)
    p2$c_speaker <- grepl("県議長|県議会議長|県議会副議長|県副議長|府議長|府議会議長|府副議長|府議会副議長", p2$current)
    p2$c_speaker_2 <- grepl("県議長|県議会議長|府議長|府議会議長", p2$current)
    
  }
  
  
  ### create new obs variable giving zeroes to unknown cases
  {
    p2 <- p2[variable.names(p2)]
    
    p2$obs_4 <- ifelse(is.na(p2$obs_2), 0, p2$obs_2)
    p2$obs_5 <- ifelse(is.na(p2$obs_3), 0, p2$obs_3)
    
    p2$missing <- ifelse(is.na(p2$obs), 1, 0)
    p2$un_full <- ifelse(p2$un_full >= 3, 3, p2$un_full) #mmakes it so 3 is 3 or more times uncontested
    
    p2$un_full_2 <- ifelse(is.na(p2$un_full), 1, p2$un_full)
    p2$party <- ifelse(!p2$party %in% c("自","共","無","公","民","社","維"), "他", p2$party)
    
    p2 <- within(p2, party <- relevel(as.factor(party), ref = "自")) ##set the new reference for party to LDP
    p2$smd <- ifelse(p2$totseat == 1, 1, 0)
    p2$ldp <- ifelse(p2$party == "自", 1, 0)
    
    p2$totseat_2 <- ifelse(p2$totseat >= 4, 4, p2$totseat)
  }
  
  p2 <- p2 %>% group_by(pref_id, year) %>% mutate(maj = n()/2, 
                                                  ldp_s = sum(party == "自"), ldp_maj = ldp_s > maj,
                                                  dpj_s = sum(party == "民"), dpj_maj = dpj_s > maj,
                                                  un_s = sum(party == "無"), un_maj = un_s > maj,
                                                  ishin_s = sum(party == "維"), ishin_maj = ishin_s > maj,
                                                  rul_p = (ldp == 1 & ldp_maj == TRUE))
  
  ## combine with district level data
  {
    dist_data <- read.csv("estat_population_oct28.csv")
    
    p2$distname_p <- p2$選挙区名
    p2 <- merge(p2, dist_data, by = c("pref_id", "distname_p"), all.x = T)
  }
  
  ##remove speakers and those in leadership positions and speaches outside analysis period
  p2_filter <- p2 %>% filter(year != 5, year != 0)
  p2_filter <- p2_filter %>% filter(c_speaker_2 != 1, party_boss_2 != 1)
  data_comm <- p2_filter
  rm(cand_, cand_2, cand_3, data_full, dist_data, dist_data2, p2, p2_filter)
}
##load governors party
{
  gov_2 <- read.csv("gov_period_oct25.csv")
  gov_2 <- gov_2[!is.na(gov_2$period),]
  
  gov_2 <- rename(gov_2, year = period)
  data_comm <- merge(data_comm, gov_2, by = c("pref_id", "year"))
  
  data_comm$party_2 <- ifelse(!data_comm$party %in% c("自","共","無","公","民","社","維"), "他", data_comm$party)
  ###create a governor ruling party dummy
  data_comm$gov_suisen <- ifelse(is.na(data_comm$gov_suisen), "None",data_comm$gov_suisen)
  data_comm$rul_1 <- data_comm$party %in% data_comm$gov_suisen
  data_comm$ainori <- ifelse(is.na(data_comm$ainori), 0,data_comm$ainori)
  data_comm$rul_2 <- data_comm$ainori == 1 & (data_comm$party %in% c("自","無","公","民","維"))
  data_comm$anti_ldp <- ifelse(is.na(data_comm$anti_ldp), 0,data_comm$anti_ldp)
  data_comm$rul_3 <- data_comm$anti_ldp == 1 & (data_comm$party %in% c("無","民","社"))
  data_comm$rul_4 <- data_comm$anti_ldp == 0 & (data_comm$party %in% c("自","公"))
  
  
  data_comm$gov_ruling <- ifelse(data_comm$rul_2 == T| data_comm$rul_3 == T| data_comm$rul_4 == T, T, F)
}
### LOAD gender variable (October 2022)
{
  gender_df <- read_csv("aso_gender_oct.csv")
  
  ##fix gender party column
  gender_df$party <- ifelse(!gender_df$party_2 %in% c("自","共","無","公","民","社","維"), "他", gender_df$party_2)
  gender_df <- within(gender_df, party <- relevel(as.factor(party), ref = "自"))
  
  gender_df <- gender_df[c("pref_id", "name", "sex", "party")]
  data_comm <- merge(data_comm, gender_df, by = c("pref_id", "name", "party"))
  data_comm <- data_comm[!duplicated(data_comm),]
}


### Table S2 - COMITTEE
##may 24 note: obs_5 gives nicer results, consider changing

{
  m1 <- glm.nb(obs_5 ~ factor(mutohyo) + factor(year) + factor(pref_id), data = data_comm)
  cov1 <- vcov(m1, cluster = data_comm$pref_id)
  cov_m1    <- sqrt(diag(cov1))
  
  m2  <- glm.nb(obs_5 ~ factor(mutohyo) + reelect + factor(year)+ factor(pref_id), data = data_comm)
  cov2 <- vcov(m2, cluster = data_comm$pref_id)
  cov_m2    <- sqrt(diag(cov2))
  
  m3  <- glm.nb(obs_5 ~ factor(mutohyo) + reelect + sex + totseat+ secretary + city_assembly + party_boss
                + party+ gov_ruling + age+ meansize_10k + pop_65over_perc +factor(year)+ factor(pref_id), data = data_comm)
  cov3 <- vcov(m3, cluster = data_comm$pref_id)
  cov_m3    <- sqrt(diag(cov3))
  
  m4  <- glm.nb(obs_5 ~ factor(mutohyo) + sex + totseat + secretary + city_assembly + party_boss
                + party+ gov_ruling+ meansize_10k + pop_65over_perc  +factor(year)+ factor(pref_id), data = data_comm)
  cov4 <- vcov(m4, cluster = data_comm$pref_id)
  cov_m4    <- sqrt(diag(cov4))
  m5  <- glm.nb(obs_5 ~ factor(mutohyo) + reelect + sex + secretary + city_assembly + party_boss
                + party+ gov_ruling+ meansize_10k + pop_65over_perc + age  +factor(year)+ factor(pref_id), data = data_comm)
  cov5 <- vcov(m5, cluster = data_comm$pref_id)
  cov_m5    <- sqrt(diag(cov5))

  m6  <- glm.nb(obs_5 ~ factor(mutohyo) + sex + secretary + city_assembly + party_boss
                + party+ gov_ruling + meansize_10k + pop_65over_perc+factor(year)+ factor(pref_id), data = data_comm)
  cov6 <- vcov(m6, cluster = data_comm$pref_id)
  cov_m6    <- sqrt(diag(cov6))
  
  m7  <- glm.nb(obs_5 ~ factor(mutohyo) + sex +secretary + city_assembly  + party_boss
                + gov_ruling + age + factor(year)+ factor(pref_id), data = data_comm)
  cov7 <- vcov(m7, cluster = data_comm$pref_id)
  cov_m7    <- sqrt(diag(cov7))
  
  m10 <- glm.nb(obs_5 ~ factor(un_full_2) + factor(year)+ factor(pref_id), data = data_comm)
  cov10 <- vcov(m10, cluster = data_comm$pref_id)
  cov_m10    <- sqrt(diag(cov10))
  
  
  m11 <- glm.nb(obs_5 ~ factor(un_full_2) + totseat+reelect + sex + secretary + city_assembly + party_boss 
                + party + gov_ruling + age+ meansize_10k + pop_65over_perc + factor(year)+ factor(pref_id), data = data_comm)
  cov11 <- vcov(m11, cluster = data_comm$pref_id)
  cov_m11    <- sqrt(diag(cov11))
  
  m12 <- glm.nb(obs_5 ~ factor(un_full_2) + sex + totseat + secretary + city_assembly + party_boss 
                + party + gov_ruling+ meansize_10k + pop_65over_perc + factor(year)+ factor(pref_id), data = data_comm)
  cov12 <- vcov(m12, cluster = data_comm$pref_id)
  cov_m12    <- sqrt(diag(cov12))
  
  m13 <- glm.nb(obs_5 ~ factor(un_full_2) + reelect + sex + secretary + city_assembly + party_boss 
                + party + gov_ruling + age+ meansize_10k + pop_65over_perc+ factor(year)+ factor(pref_id), data = data_comm)
  cov13 <- vcov(m13, cluster = data_comm$pref_id)
  cov_m13    <- sqrt(diag(cov13))
  
  m14 <- glm.nb(obs_5 ~ factor(un_full_2) + sex + secretary + city_assembly + party_boss 
                + party + gov_ruling+ meansize_10k + pop_65over_perc + factor(year)+ factor(pref_id), data = data_comm)
  cov14 <- vcov(m14, cluster = data_comm$pref_id)
  cov_m14    <- sqrt(diag(cov14))
  
  stargazer(m1,m3,m4,m5,m6, type = "text",
            se = list(cov_m1, cov_m3,cov_m4,cov_m5,cov_m6), omit = c("year", "pref_id", "secretary", 
                                                                     "city_assembly", "party", "gov_ruling"
                                                                     ))
  order=c(1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,2)
  stargazer(m1,m3,m4,m5,m6, type = "html",
            
            add.lines=list(c("Time effects", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES"),
                           c("Prefecture effects", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES"),
                           c("Control variables", "NO", "YES", "YES", "YES", "YES", "NO", "YES", "YES", "YES", "YES"),
                           c("Party dummies", "NO", "YES", "YES", "YES", "YES", "NO", "YES", "YES", "YES", "YES"),
                           c("Demographic variables", "NO", "YES", "YES", "YES", "YES", "NO", "YES", "YES", "YES", "YES")),
            se = list(cov_m1, cov_m3,cov_m4,cov_m5,cov_m6,cov_m10,cov_m11,cov_m12,cov_m13,cov_m14), omit = c("year", "pref_id", "secretary", 
                                                                                                             "city_assembly", "party", "gov_ruling",
                                                                                                             "meansize_10k", "pop_65over_perc"),
            covariate.labels = c("Uncontested","Tenure", "DistrictMagnitude", "Age", "Sex (female)"),
            out = "C:/Users/robba/Dropbox/Politician_effectiveness_paper/regression_models_july22/tabS2_nov24.html")
  
}


#### table S3 - instumental variable analysis


##prep code
library(ivreg)
#### MERGE WITH HIREI DATA (national election results from Asahi shimbun)
hirei <- read.csv("hirei/hirei_merged_nov10.csv")
hirei <- hirei %>% rename(year_hirei= year,
                          ldp_hirei = ldp,
                          tot_vote_hirei = tot_vote
) %>% mutate(
  ldp_hirei_perc = ldp_hirei/tot_vote_hirei
)
data_main <- merge(data_main, hirei, by = c("pref_id", "distname_p", "year_merge")) 

{
iv_m_1 <- ivreg(obs_5 ~ factor(mutohyo) + factor(year)+ factor(pref_id)| ldp_hirei_perc + factor(year)+ factor(pref_id),
                data = data_main) 
cov1 <- vcov(iv_m_1, cluster = data_main$pref_id)
cov_m1    <- sqrt(diag(cov1))


iv_m_2 <- ivreg(obs_5 ~ factor(mutohyo)  + reelect + totseat + age + sex  + secretary + city_assembly + party_boss
                + party+ gov_ruling+ meansize_10k + pop_65over_perc+ factor(year)+ factor(pref_id)| ldp_hirei_perc +  reelect+ age+ totseat
                + sex  + secretary + city_assembly + party_boss+ meansize_10k + pop_65over_perc
                + party+ gov_ruling + factor(year)+ factor(pref_id),
                data = data_main) 
cov2 <- vcov(iv_m_2, cluster = data_main$pref_id)
cov_m2    <- sqrt(diag(cov2))

iv_m_3 <- ivreg(obs_5 ~ factor(mutohyo) + totseat + sex  + secretary + city_assembly + party_boss
                + party+ gov_ruling+ meansize_10k + pop_65over_perc+ factor(year)+ factor(pref_id)| ldp_hirei_perc +  totseat
                + sex  + secretary + city_assembly + party_boss+ meansize_10k + pop_65over_perc
                + party+ gov_ruling + factor(year)+ factor(pref_id),
                data = data_main) 
cov3 <- vcov(iv_m_3, cluster = data_main$pref_id)
cov_m3    <- sqrt(diag(cov3))


iv_m_4 <- ivreg(obs_5 ~ factor(mutohyo)  + reelect + age + sex  + secretary + city_assembly + party_boss
                + party+ gov_ruling+ meansize_10k + pop_65over_perc+ factor(year)+ factor(pref_id)| ldp_hirei_perc +  reelect+ age
                + sex  + secretary + city_assembly + party_boss+ meansize_10k + pop_65over_perc
                + party+ gov_ruling + factor(year)+ factor(pref_id),
                data = data_main) 
cov4 <- vcov(iv_m_4, cluster = data_main$pref_id)
cov_m4    <- sqrt(diag(cov4))

iv_m_5 <- ivreg(obs_5 ~ factor(mutohyo)  + sex  + secretary + city_assembly + party_boss
                + party+ gov_ruling+ meansize_10k + pop_65over_perc+ factor(year)+ factor(pref_id)| ldp_hirei_perc
                + sex  + secretary + city_assembly + party_boss+ meansize_10k + pop_65over_perc
                + party+ gov_ruling + factor(year)+ factor(pref_id),
                data = data_main) 
cov5 <- vcov(iv_m_5, cluster = data_main$pref_id)
cov_m5    <- sqrt(diag(cov5))
summary(iv_m_5)

stargazer(iv_m_1,iv_m_2,iv_m_3,iv_m_4,iv_m_5, type = "html",
          se = list(cov_m1, cov_m2, cov_m3,cov_m4,cov_m5),
          add.lines=list(c("Time effects", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES"),
                         c("Prefecture effects", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES"),
                         c("Control variables", "NO", "YES", "YES", "YES", "YES", "NO", "YES", "YES", "YES", "YES"),
                         c("Party dummies", "NO", "YES", "YES", "YES", "YES", "NO", "YES", "YES", "YES", "YES"),
                         c("Demographic variables", "NO", "YES", "YES", "YES", "YES", "NO", "YES", "YES", "YES", "YES")),
           omit = c("year", "pref_id", "secretary", 
                                                                                                           "city_assembly", "party", "gov_ruling",
                                                                                                           "meansize_10k", "pop_65over_perc"),
          covariate.labels = c("Uncontested","Tenure", "DistrictMagnitude", "Age", "Sex (female)"),
          out = "C:/Users/robba/Dropbox/Politician_effectiveness_paper/regression_models_july22/tabS3_nov24.html")
}

#### Table S4 - effect with different thresholds

{
  data_main_2 <- data_main %>% filter(!is.na(meansize_10k))
  
  m1 <- glm.nb(obs_5 ~ factor(mutohyo_uncomp) + reelect+ secretary + city_assembly + sex
               + party  + gov_ruling + age + totseat + meansize_10k + pop_65over_perc +factor(year)+ factor(pref_id), data = data_main_2)
  cov1 <- vcov(m1, cluster = data_main_2$pref_id)
  cov_m1    <- sqrt(diag(cov1))
  
  
  m2 <- glm.nb(obs_5 ~ factor(mutohyo_uncomp_2) + reelect+ secretary + city_assembly+ sex
               + party  + gov_ruling + age + totseat + meansize_10k + pop_65over_perc +factor(year)+ factor(pref_id), data = data_main_2)
  cov2 <- vcov(m2, cluster = data_main_2$pref_id)
  cov_m2    <- sqrt(diag(cov2))
  
  m3  <- glm.nb(obs_5 ~ factor(mutohyo_uncomp_3) + reelect+ secretary + city_assembly+ sex
                + party  + gov_ruling + age + totseat + meansize_10k + pop_65over_perc +factor(year)+ factor(pref_id), data = data_main_2)
  cov3 <- vcov(m3, cluster = data_main_2$pref_id)
  cov_m3    <- sqrt(diag(cov3))
  
  m4  <- glm.nb(obs_5 ~ competitiveness + reelect+ secretary + city_assembly+ sex
                + party  + gov_ruling + age + totseat + meansize_10k + pop_65over_perc +factor(year)+ factor(pref_id), data = data_main_2)
  cov4 <- vcov(m4, cluster = data_main_2$pref_id)
  cov_m4    <- sqrt(diag(cov4))
  
 
  stargazer(m1,m2,m3,m4, type = "html",
            se = list(cov_m1, cov_m2, cov_m3,cov_m4),add.lines=list(c("Time effects", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES"),
                                                                                               c("Prefecture effects", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES"),
                                                                                               c("Control variables", "YES", "YES", "YES", "YES", "YES", "NO", "YES", "YES", "YES", "YES"),
                                                                                               c("Party dummies", "YES", "YES", "YES", "YES", "YES", "NO", "YES", "YES", "YES", "YES"),
                                                                                               c("Demographic variables", "YES", "YES", "YES", "YES", "YES", "NO", "YES", "YES", "YES", "YES")),
            omit = c("year", "pref_id", "secretary", 
                                                                                                             "city_assembly", "party", "gov_ruling",
                                                                                                             "meansize_10k", "pop_65over_perc"),
            covariate.labels = c("Uncontested","Tenure", "DistrictMagnitude", "Age", "Sex (female)"),
            out = "C:/Users/robba/Dropbox/Politician_effectiveness_paper/regression_models_july22/tabS4_nov24.html")
  
}

### table S5 - different measurements of dependent variable
library(multiwayvcov)
install.packages("multiwayvcov")
data_main <- data_main %>% mutate(below_2 =  year_n < 3)
{
m1 <- glm.nb(obs_4 ~ factor(mutohyo) + reelect + secretary + city_assembly + sex
             + party + gov_ruling + age + totseat + meansize_10k + pop_65over_perc + factor(year)+ factor(pref_id), data = data_main)
cov1 <- vcov(m1, cluster = data_main$pref_id)
cov_m1 <- sqrt(diag(cov1))
m2 <- glm(below_2 ~ factor(mutohyo) + reelect+ secretary + city_assembly+ sex
          + party  + gov_ruling + age + totseat + meansize_10k + pop_65over_perc +factor(year)+ factor(pref_id), data = data_main, family = "binomial")
cov2 <- vcov(m2, cluster = data_main$pref_id)
cov_m2    <- sqrt(diag(cov2))

stargazer(m1,m2, type = "text",
          se = list(cov_m1,cov_m2), omit = c("year", "pref_id", "secretary", 
                                             "city_assembly", "party", "gov_ruling",
                                             "meansize_10k", "pop_65over_perc"),
          covariate.labels = c("Uncontested","Tenure", "DistrictMagnitude", "Age", "Sex (female)"))

stargazer(m1,m2, type = "html",
          se = list(cov_m1,cov_m2), omit = c("year", "pref_id", "secretary", 
                                              "city_assembly", "party", "gov_ruling",
                                              "meansize_10k", "pop_65over_perc"),add.lines=list(c("Time effects", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES"),
                                                                                                c("Prefecture effects", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES"),
                                                                                                c("Control variables", "YES", "YES", "YES", "YES", "YES", "NO", "YES", "YES", "YES", "YES"),
                                                                                                c("Party dummies", "YES", "YES", "YES", "YES", "YES", "NO", "YES", "YES", "YES", "YES"),
                                                                                                c("Demographic variables", "YES", "YES", "YES", "YES", "YES", "NO", "YES", "YES", "YES", "YES")),
          covariate.labels = c("Uncontested","Tenure", "DistrictMagnitude", "Age", "Sex (female)"),
          out = "C:/Users/robba/Dropbox/Politician_effectiveness_paper/regression_models_july22/tabS5_nov24.html")

}
