

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


library(dplyr)


##read speech dataset
data_full <- read.csv("replication_data_apr14.csv")


## combine with district level data
{
dist_data <- read.csv("pref_all_vars.csv")

dist_data2 <- dist_data %>% filter(year_p >= 2015) %>% group_by(year_p, pref_id, distname_p, mean_size, meansize_10k, pop, pop_65over_perc) %>% summarize()
dist_data2 <- dist_data2 %>% filter(year_p != 2019)  %>% group_by(pref_id, distname_p, mean_size, meansize_10k, pop, pop_65over_perc) %>% summarize()
data_full$distname_p <- data_full$選挙区名
look_m_3 <- merge(data_full, dist_data2, by = c("pref_id", "distname_p"), all.x = T)
}

p2 <- look_m_3 %>% group_by(year_election, year = period, name, mutohyo, party, reelect, age, totseat, 選挙区名, pref_id
                            ,incum, un_full, un_sum, un_full_na_op1, un_full_na_op2, period, secretary,
                            city_assembly, lawyer, party_boss, party_boss_2, c_speaker, c_speaker_2, un_8799,
                            mean_size, meansize_10k, pop, pop_65over_perc) %>% summarize(obs = length(unique(V1, type)),
                                                                                                              obs_2 = length(V1),
                                                                                                              obs_3 = length(unique(date)))



##Checking numbers for general stats chapter 
{
  
  ##calculate number of politicians and mutohyo experience
  length(unique(p2$name, p2$pref_id))
  length(unique(p2$pref_id))
  
  see <- p2 %>% group_by(name) %>% summarise(mt = mean(mutohyo))
  length(unique(see[see$mt > 0,]$name))
  length(unique(see[see$mt > 0,]$name))/length(unique(p2$name))
  

  #filter by those that have very high numbers
                            }


##data preparation 
{
##summarize small parties into groups
p2$party <- ifelse(!p2$party %in% c("自","共","無","公","民","社","維"), "他", p2$party)
p2$un_full <- ifelse(p2$un_full >= 3, 3, p2$un_full) #mmakes it so 3 is 3 or more times uncontested

## fix the NA issue (all the NA cases are uncontested - therefore assume that un_full == 1)
p2$un_full_2 <- ifelse(is.na(p2$un_full), 1, p2$un_full)
unique(p2[p2$reelect == 1,]$un_full_2)
p2$un_full_2 <- ifelse(p2$reelect == 1 & p2$un_full_2 > 0, 1, p2$un_full_2)
p2$un_full_2 <- factor(p2$un_full_2)

p2$smd <- ifelse(p2$totseat == 1, 1, 0)

##remove cases outside of analysis period
p2_filter <- p2 %>% filter(year != 5, year != 0)
##remove speakers and those in leadership positions
p2_filter <- p2_filter %>% filter(c_speaker_2 != 1, party_boss_2 != 1)
p2_filter <- within(p2_filter, party <- relevel(as.factor(party), ref = "自")) ##set the new reference for party to LDP

}


### table 1, Dexcriptive statistics of data for analysis
##remove cases outside of analysis period
sumtable(p2_filter, factor.percent = T,out='html',file='desc_honkaigi_filter_apr14.html')


##LOAD ANALYSIS PACKAGES
library(lmtest)
library(sandwich)
library(clubSandwich)
library(multiwaycov)
library(MASS)
library(texreg)
library(alpaca)
library(pglm)
library(modEvA)

library(stargazer)

### Table 2 - Main regression models
m1 <- glm.nb(obs_2 ~ factor(mutohyo) + factor(year) + factor(pref_id), data = p2_filter)
cov1 <- clubSandwich::vcovCR(m1, type = "CR2", cluster = p2_filter$pref_id)
cov_m1    <- sqrt(diag(cov1))

m2 <- glm.nb(obs_2 ~ factor(un_full_2) + factor(year) + factor(pref_id), data = p2_filter)
cov2 <- clubSandwich::vcovCR(m2, type = "CR2", cluster = p2_filter$pref_id)
cov_m2    <- sqrt(diag(cov2))

m3  <- glm.nb(obs_2 ~ factor(mutohyo) + secretary + city_assembly + lawyer 
              + party + reelect + age + totseat + factor(year)+ factor(pref_id), data = p2_filter)
cov3 <- clubSandwich::vcovCR(m3, type = "CR2", cluster = p2_filter$pref_id)
cov_m3    <- sqrt(diag(cov3))

m4 <- glm.nb(obs_2 ~ factor(un_full_2) + secretary + city_assembly + lawyer 
             + party + reelect + age + totseat + factor(year)+ factor(pref_id), data = p2_filter)
cov4 <- clubSandwich::vcovCR(m4, type = "CR2", cluster = p2_filter$pref_id)
cov_m4    <- sqrt(diag(cov4))

m5  <- glm.nb(obs_2 ~ factor(mutohyo)+ secretary + city_assembly + lawyer 
              + party + age + totseat +factor(year)+ factor(pref_id), data = p2_filter)
cov5 <- clubSandwich::vcovCR(m5, type = "CR2", cluster = p2_filter$pref_id)
cov_m5    <- sqrt(diag(cov5))

m6 <- glm.nb(obs_2 ~ factor(un_full_2) + secretary + city_assembly + lawyer
             + party + age + totseat + factor(year)+ factor(pref_id), data = p2_filter)
cov6 <- clubSandwich::vcovCR(m6, type = "CR2", cluster = p2_filter$pref_id)
cov_m6    <- sqrt(diag(cov6))

m7  <- glm.nb(obs_2 ~ factor(mutohyo) + secretary + city_assembly + lawyer 
              + party + reelect + age + totseat + meansize_10k + pop_65over_perc + factor(year)+ factor(pref_id), data = p2_filter)
cov7 <- clubSandwich::vcovCR(m7, type = "CR2", cluster = p2_filter$pref_id)
cov_m7    <- sqrt(diag(cov7))

m8 <- glm.nb(obs_2 ~ factor(un_full_2) + secretary + city_assembly + lawyer 
             + party + reelect + age + totseat + meansize_10k + pop_65over_perc + factor(year)+ factor(pref_id), data = p2_filter)
cov8 <- clubSandwich::vcovCR(m8, type = "CR2", cluster = p2_filter$pref_id)
cov_m8    <- sqrt(diag(cov8))

m9  <- glm.nb(obs_2 ~ factor(mutohyo)+ secretary + city_assembly + lawyer 
              + party + age + totseat + meansize_10k + pop_65over_perc +factor(year)+ factor(pref_id), data = p2_filter)
cov9 <- clubSandwich::vcovCR(m9, type = "CR2", cluster = p2_filter$pref_id)
cov_m9    <- sqrt(diag(cov9))

m10 <- glm.nb(obs_2 ~ factor(un_full_2) + secretary + city_assembly + lawyer
             + party + age + totseat + meansize_10k + pop_65over_perc + factor(year)+ factor(pref_id), data = p2_filter)
cov10 <- clubSandwich::vcovCR(m10, type = "CR2", cluster = p2_filter$pref_id)
cov_m10    <- sqrt(diag(cov10))


stargazer(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, type = "text",
          se = list(cov_m1, cov_m2, cov_m3, cov_m4,cov_m5,cov_m6,cov_m7,cov_m8,cov_m9,cov_m10), omit = c("year", "pref_id"))
stargazer(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, type = "text",
          se = list(cov_m1, cov_m2, cov_m3, cov_m4,cov_m5,cov_m6,cov_m7,cov_m8,cov_m9,cov_m10), out = "tab2_apr15.txt")
stargazer(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, type = "text",
          se = list(cov_m1, cov_m2, cov_m3, cov_m4,cov_m5,cov_m6,cov_m7,cov_m8,cov_m9,cov_m10), omit = c("year", "pref_id", "party"), out = "tab2_apr15.html")

##calculate pseudo r2
modEvA::RsqGLM(model=m1)
modEvA::RsqGLM(model=m2)
modEvA::RsqGLM(model=m3)
modEvA::RsqGLM(model=m4)
modEvA::RsqGLM(model=m5)
modEvA::RsqGLM(model=m6)
modEvA::RsqGLM(model=m7)
modEvA::RsqGLM(model=m8)
modEvA::RsqGLM(model=m9)
modEvA::RsqGLM(model=m10)


### Table 3 - Panel data models 
p2 <- look_m_3 %>% group_by(year = period, name, mutohyo, party, reelect, age, totseat, 選挙区名, pref_id,
                            , incum, un_full, un_sum, un_8799, 
                            city_assembly, lawyer, party_boss, party_boss_2, secretary, c_speaker, c_speaker_2) %>% summarize(obs = length(unique(V1, type)), 
                                                                                                                obs_2 = length(V1),
                                                                                                                obs_3 = length(unique(date)))

##data preparation 
{
##balance the panel
p2 <- transform(p2, id2=apply(p2[c("name", "選挙区名",  "pref_id")], 1, paste, collapse="."))
p2 <- p2 %>% group_by(id2, year) %>% filter(!n()>1) 

stopifnot(!any(duplicated(p2[c("id2", "year")])))
p2$party <- ifelse(!p2$party %in% c("自","共","無","公","民","社","維"), "他", p2$party)
p2 <- within(p2, party <- relevel(as.factor(party), ref = "自")) ##set the new reference for party to LDP

p2$un_full <- ifelse(p2$un_full >= 3, 3, p2$un_full) #mmakes it so 3 is 3 or more times uncontested
## fix the NA issue (all the NA cases are uncontested - therefore assume that un_full == 1)
p2$un_full_2 <- ifelse(is.na(p2$un_full), 1, p2$un_full)
p2$un_full_2 <- ifelse(p2$reelect == 1 & p2$un_full_2 > 0, 1, p2$un_full_2)
p2$un_full_2 <- ifelse(p2$mutohyo == 0, 0, p2$un_full_2)

p2_filt <- p2[p2$year != 0 & p2$year != 5,] %>% filter(c_speaker_2 != 1, party_boss_2 != 1)

                            }


m1 <- feglm.nb(obs_2 ~ factor(mutohyo) + reelect+
                 age + totseat| id2 + year, data = p2_filt)

m2 <- feglm.nb(obs_2 ~ factor(un_full_2) + reelect+
                 age + totseat| id2 + year, data = p2_filt)

screenreg(list(m1,m2), type = "clustered", cluster = ~id2 + year,stars = c(0.01, 0.05, 0.1, 0.15), 
          include.loglik = T)
htmlreg(list(m1,m2), type = "clustered", cluster = ~id2 + year, stars = c(0.01, 0.05, 0.1, 0.15), 
        file = "tab3_apr15.doc", include.loglik = T)


### Table 4 - Panel data models by district characteristics

{
  p2 <- look_m_3 %>% group_by(year = period, name, mutohyo, party, reelect, age, totseat, 選挙区名, pref_id,
                              , incum, un_full, un_sum, un_8799,
                              city_assembly, lawyer, party_boss, party_boss_2, secretary, c_speaker, c_speaker_2,
                              mean_size, meansize_10k, pop, pop_65over_perc) %>% summarize(obs = length(unique(V1)), 
                                                                                           obs_2 = length(V1),
                                                                                           obs_3 = length(unique(date, type)))
  
  
  ##balance the panel
  p2$distname_p <- p2$選挙区名
  
  p2 <- transform(p2, id2=apply(p2[c("name", "distname_p",  "pref_id")], 1, paste, collapse="."))
  p2 <- p2 %>% group_by(id2, year) %>% filter(!n()>1) 
  
  stopifnot(!any(duplicated(p2[c("id2", "year")])))
  p2$party <- ifelse(!p2$party %in% c("自","共","無","公","民","社","維"), "他", p2$party)
  p2 <- within(p2, party <- relevel(as.factor(party), ref = "自")) ##set the new reference for party to LDP
  
  p2$un_full <- ifelse(p2$un_full >= 3, 3, p2$un_full) #mmakes it so 3 is 3 or more times uncontested
  ## fix the NA issue (all the NA cases are uncontested - therefore assume that un_full == 1)
  p2$un_full_2 <- ifelse(is.na(p2$un_full), 1, p2$un_full)
  unique(p2[p2$reelect == 1,]$un_full_2)
  p2$un_full_2 <- ifelse(p2$reelect == 1 & p2$un_full_2 > 0, 1, p2$un_full_2)
  
  
  p2 <- p2 %>% group_by(pref_id) %>% mutate(norm_obs =  obs/mean(obs),
                                            norm_obs_3 = obs_3/mean(obs_3))
  p2_filter <- p2[p2$year != 0 & p2$year != 5,] %>% filter(c_speaker_2 != 1, party_boss_2 != 1)
  p2_filter <- within(p2_filter, party <- relevel(as.factor(party), ref = "自")) ##set the new reference for party to LDP

}



p2_filter_smd <- p2_filter %>% filter(totseat %in% c(1))
p2_filter_2 <- p2_filter %>% filter(totseat %in% c(2))
p2_filter_3 <- p2_filter %>% filter(totseat %in% c(3))
p2_filter_4 <- p2_filter %>% filter(totseat %in% c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))

p2_filter_rural <- p2_filter %>% filter(meansize_10k < 3)
p2_filter_mid <- p2_filter %>% filter(meansize_10k >= 3, meansize_10k < 20)
p2_filter_urban <- p2_filter %>% filter(meansize_10k > 20)


### Final estimation with converged \theta ###

m1 <- feglm.nb(obs_2 ~ factor(un_full_2) + reelect +
                 age   | id2 + year, data = p2_filter_smd)
m2 <- feglm.nb(obs_2 ~ factor(un_full_2) + reelect +
                 age  | id2 + year, data = p2_filter_2)
m3 <- feglm.nb(obs_2 ~ factor(un_full_2) + reelect +
                 age  | id2 + year, data = p2_filter_3)
m4 <- feglm.nb(obs_2 ~ factor(un_full_2) + reelect +
                 age  | id2 + year, data = p2_filter_4)
m5 <- feglm.nb(obs_2 ~ factor(un_full_2) + reelect +
                 age  | id2 + year, data = p2_filter_rural)
m6 <- feglm.nb(obs_2 ~ factor(un_full_2) + reelect +
                 age  | id2 + year, data = p2_filter_mid)
m7 <- feglm.nb(obs_2 ~ factor(un_full_2) + reelect +
                 age  | id2 + year, data = p2_filter_urban)

screenreg(list(m1,m2,m3,m4,m5,m6,m7),  type = "clustered", cluster = ~id2 + year, stars = c(0.01, 0.05, 0.1, 0.15), 
          include.loglik = T)

htmlreg(list(m1,m2,m3,m4,m5,m6,m7), type = "clustered", cluster = ~id2 + year, stars = c(0.01, 0.05, 0.1, 0.15), 
        file = "tab4_apr15.doc", include.loglik = T)

