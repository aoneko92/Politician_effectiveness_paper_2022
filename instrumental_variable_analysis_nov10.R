#install.packages("ivreg", dependencies = TRUE)
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


#### INSTRUMENTAL VARIABLE ANALYSIS ####
library(ivreg)


###LOAD BOTH DATASETS
##main session
{
  
  data_full <- read.csv("replication_data_oct21.csv")
  
  
  
  p2 <- data_full %>% group_by(year = period, name, mutohyo, party, reelect, age, totseat, 選挙区名, pref_id
  ) %>% summarize(obs = length(unique(V1, type)),
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
  
  ##remove speakers and those in leadership positions and speaches outside analysis period
  p2_filter <- p2 %>% filter(year != 5, year != 0)
  p2_filter <- p2_filter %>% filter(c_speaker_2 != 1, party_boss_2 != 1)
  data_main <- p2_filter
  rm(cand_, cand_2, cand_3, data_full, dist_data, dist_data2, p2, p2_filter)
}
#### dont use until Aso updates the file
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


### test endogeneity
data_main$meansize_10k
summary(lm(mutohyo ~ ldp_hirei_perc + reelect + totseat+ sex  + secretary + city_assembly + party_boss
           + party+ gov_ruling + age+ meansize_10k + pop_65over_perc + pop_did_perc + factor(year)+ factor(pref_id), data= data_main))

#### MERGE WITH HIREI DATA (national election results from Asahi shimbun)
hirei <- read.csv("hirei/hirei_merged_nov10.csv")
hirei <- hirei %>% rename(year_hirei= year,
                          ldp_hirei = ldp,
                          tot_vote_hirei = tot_vote
                          ) %>% mutate(
                            ldp_hirei_perc = ldp_hirei/tot_vote_hirei
                          )
data_main <- merge(data_main, hirei, by = c("pref_id", "distname_p", "year_merge")) 


#### REGULAR REGERSSION MODEL
ols_m  <- lm(obs_5 ~ factor(mutohyo)  + reelect + totseat+ sex  + secretary + city_assembly + party_boss
             + party+ gov_ruling + age+ meansize_10k + pop_65over_perc+ factor(year)+ factor(pref_id), data = data_main)
summary(ols_m)


hirei$ldp
iv_m_1 <- ivreg(obs_5 ~ factor(mutohyo)  + reelect + totseat + age + sex  + secretary + city_assembly + party_boss
              + party+ gov_ruling+ meansize_10k + pop_65over_perc+ factor(year)+ factor(pref_id)| ldp_hirei_perc +  reelect+ age+ totseat
              + sex  + secretary + city_assembly + party_boss+ meansize_10k + pop_65over_perc
              + party+ gov_ruling + factor(year)+ factor(pref_id),
              data = data_main) 
cov1 <- clubSandwich::vcovCR(iv_m_1, type = "CR2", cluster = data_main$pref_id)
cov_m1    <- sqrt(diag(cov1))

summary(iv_m_1)


stargazer(iv_m_1, type = "text",
          se = list(cov_m1))






d2 <- data_main[data_main$period ==1,]
iv_m_2 <- ivreg(obs_5 ~ factor(mutohyo)  + reelect + totseat+ sex  + secretary + city_assembly + party_boss
              + party+ gov_ruling + age+ meansize_10k + pop_65over_perc+ factor(year)+ factor(pref_id)| pop_did_perc +  reelect+ age+ totseat+
                sex  + secretary + city_assembly + party_boss+ meansize_10k + pop_65over_perc
              + party+ gov_ruling + factor(year)+ factor(pref_id),
              data = data_main) 
summary(iv_m_2)

library("modelsummary")


modelplot(list(ols_m, iv_m_1), coef_omit = "pref_id|year") + geom_vline(xintercept = 0) +
  ggtitle("Comparison between regular OLS (model 1) and 2SLS model (model 2), IV = % LDP in lower house election")
















#### create generalized models with instrumental variable #### 
install.packages("instruments")
### isntruments package is old so modify and work on by yourself
source("helpers.R")
source("iv_glm.R")
source("iv_glm_nb.R")

data_main$f_mutohyo <- factor(data_main$mutohyo)
data_main$f_year <- factor(data_main$year)
data_main$f_pref_id <- factor(data_main$pref_id)

iv_nb_mod <- iv.glm.nb(obs_5 ~ f_mutohyo  + reelect + totseat+ sex  + secretary + city_assembly + party_boss
      + party+ gov_ruling + age+ meansize_10k + pop_65over_perc+ f_year+ f_pref_id, mutohyo ~ ldp_hirei_perc,
      data = data_main)

summary(iv_nb_mod[6])

model_formula <- obs_5 ~ mutohyo  + reelect + totseat+ sex  + secretary + city_assembly + party_boss + party+ gov_ruling + age+ meansize_10k + pop_65over_perc+ factor(year)+ factor(pref_id)
 
instrument_formula <-  mutohyo ~ ldp_hirei_perc




ols_m  <- lm(obs_5 ~ factor(mutohyo)+ldp_hirei_perc  + reelect + totseat+ sex  + secretary + city_assembly + party_boss
             + party+ gov_ruling + age+ meansize_10k + pop_65over_perc+ factor(year)+ factor(pref_id), data = data_main)
summary(ols_m)


