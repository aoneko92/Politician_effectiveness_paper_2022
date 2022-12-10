#### Plots for manuscript - Spring 2022 ####
library(dplyr)
library(ggplot2)
library(scales)
library(cowplot)
library(ggrepel)
library(tidyr)

library(ggcorrplot)
library(corrplot)
library(marginaleffects)

length(unique(p2_filter$pref_id))

## plot 1 - Number of uncontested elections across years
{

data <- read.csv("pref_candidats8518_Natori,Horiuchi.csv")
variable.names(data)

data <- data %>% group_by(year, pref_id = kencode, distname1, totseat, mutohyo) %>% summarise()
data <- data %>% mutate(mutohyo_n = mutohyo*totseat)

data <- data %>% mutate(year_p = case_when(year < 1991 ~ 1,
                                           year < 1995 & year >= 1991 ~ 2,
                                           year < 1999 & year >= 1995 ~ 3,
                                           year < 2003 & year >= 1999 ~ 4,
                                           year < 2007 & year >= 2003 ~ 5,
                                           year < 2011 & year >= 2007 ~ 6,
                                           year < 2015 & year >= 2011 ~ 7,
                                           year >= 2015 ~ 8))
data <- data %>% group_by(year_p) %>% summarise(totseat = sum(totseat, na.rm = T), mutohyo_n = sum(mutohyo_n, na.rm = T), perc = mutohyo_n/totseat)

data <- data[!is.na(data$year_p),]
ggplot(data, aes(x = factor(year_p), y = perc*100, group = 1, label = round((perc*100), digits = 2))) + geom_line() + geom_label_repel()+ geom_point() + ylim(0, 30) +
  scale_x_discrete(labels=c("1" = "1987 - 1990", "2" = "1991 - 1994", "3" = "1995 - 1998",
                            "4" = "1999 - 2002", "5" = "2003 - 2006", "6" = "2007 - 2010",
                            "7" = "2011 - 2014", "8" = "2015 - 2018")) +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) + 
  ylab("% seats uncontested")+
  xlab("Period") + labs(caption = "")

}




### Plot 2 - Uncontested by electoral rules
cand_ <- read.csv("cand_data_may13.csv")
cand_ <- cand_[cand_$win == TRUE,]

cand_$un_full <- ifelse(cand_$un_full >= 3, 3, cand_$un_full) #mmakes it so 3 is 3 or more times uncontested
cand_$un_full_2 <- ifelse(is.na(cand_$un_full), 1, cand_$un_full)
cand_$mutohyo <- ifelse(cand_$un_full_2 == 0, 0, cand_$mutohyo)
cand_$totseat_2 <- ifelse(cand_$totseat >= 4, 4, cand_$totseat)

cand_ %>% group_by(totseat_2) %>% filter(!is.na(totseat_2)) %>% mutate(N = n()) %>% 
  ggplot(aes(x = factor(totseat_2), (mutohyo/N)*100, fill = factor(un_full_2))) + geom_bar(position="stack",stat="identity") +
  scale_fill_manual(values = c("white", "red", "orange", "blue"), name= "# uncontested", labels = c("","Once", "Twice", "Three times +")) +
  scale_x_discrete(labels = c("SMD", "2-seat MMD", "3-seat MMD", "4-seat + MMD")) + xlab("") + 
  ylab("percentage %") + ggtitle("Plot 2: Percentage of uncontested seats by district magnitude")
  


###load main session and comittee data separately

##main session
{
  
  data_full <- read.csv("replication_data_nov23.csv")
  
  
  
  p2 <- data_full %>% group_by(year = period, name, mutohyo, party, reelect, age, totseat, 選挙区名, pref_id
                               ,incum, un_full, un_sum, un_full_na_op1, un_full_na_op2, period, secretary,
                               city_assembly, lawyer, party_boss, party_boss_2, c_speaker, c_speaker_2, un_8799, gov_ruling) %>% summarize(obs = length(unique(V1, type)),
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

  p2$period <- NULL
  ###create zeroes by merging all available election data
  p2 <- merge(p2, cand_3, by = c("pref_id","選挙区名", "name", "year", "mutohyo", "party",
                                 "reelect", "age", "totseat", "un_full", "un_sum",
                                 "incum"), all.y = T)
  
  
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

##comittee
{
  ##for comittees
  data_full <- read.csv( "comittee_replication_data_oct21.csv")
  
  p2 <- data_full %>% group_by(year = period, name, mutohyo, party, reelect, age, totseat, 選挙区名, pref_id
                               ,incum, un_full, un_sum, un_full_na_op1, un_full_na_op2, period, secretary,
                               city_assembly, lawyer, party_boss, party_boss_2, c_speaker, c_speaker_2, un_8799, gov_ruling) %>% summarize(obs = length(unique(V1, type)),
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
                                 "reelect", "age", "totseat", "un_full", "un_sum",
                                 "incum"), all.y = T)
  
  
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

###load analysis data (see "analysis_replications.R")
###create p2_filter for tab1 and tab2

### Plot 3 - Obs density plot
{
### Main session
stats <- data_main %>% summarise(mean = mean(obs_5), median = median(obs_5)) %>%   gather(key = key, value = value, mean:median)

p3_1 <- ggplot(data_main, aes(obs_5)) + geom_histogram( binwidth = 1)  + 
  xlab("N remarks") + 
  ylab("N") + xlim(-1,70) +
  ggtitle("")  + labs(caption = "* Highest value is 64") +
  geom_vline(data = stats, aes(xintercept = value, color = key))
  

p_ <- plot_grid(p3_1, ncol = 1)

}

plot_grid(title, p_, ncol = 1,
          rel_heights = c(0.1,1)) 

### Plot 4 - Effect of uncontested on n remarks
##generate analysis df for tab 2 before running scripts ("")

{
plot <- data_main %>% group_by(mutohyo) %>% summarise(n = n(),mean = mean(obs_5), sd = sd(obs_5)) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

p1 <- ggplot(plot) +
  geom_bar( aes(x=factor(mutohyo), y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=factor(mutohyo), ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("Uncontested")+ 
  scale_x_discrete(labels= c("Contested", "Uncontested")) + xlab("")






plot <- data_main %>% group_by(un_full_2) %>% summarise(n = n(),mean = mean(obs_5), sd = sd(obs_5)) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

p5 <- ggplot(plot) +
  geom_bar( aes(x=factor(un_full_2), y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=factor(un_full_2), ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("Uncontested (consecutive terms)")+
  scale_x_discrete(labels= c("0 terms", "1 term", "2 terms", "3+ terms"))+ xlab("")





plot <- data_main %>% group_by(party) %>% summarise(n = n(),mean = mean(obs_5), sd = sd(obs_5)) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

p9 <- ggplot(plot) +
  geom_bar( aes(x=party, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=party, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("Party of legislator")+
  scale_x_discrete(labels = c("LDP", "Ishin", "JCP", "KMT", "SDP", "Other", "DPJ", "Unaf"))+ xlab("")





plot <- data_main %>% group_by(reelect) %>% summarise(n = n(),mean = mean(obs_5), sd = sd(obs_5)) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

p13 <- ggplot(plot) +
  geom_bar( aes(x=reelect, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=reelect, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("Tenure (# times reelected)") + xlab("")
  scale_x_discrete(name = "")

  plot <- data_main %>% group_by(totseat) %>% summarise(n = n(),mean = mean(obs_5), sd = sd(obs_5)) %>%
    mutate( se=sd/sqrt(n))  %>%
    mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
  
  p15 <- ggplot(plot) +
    geom_bar( aes(x=totseat, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
    geom_errorbar( aes(x=totseat, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
    ggtitle("District magnitude")+ xlab("") 
  scale_x_discrete(name = "")



p_ <- plot_grid(p1,p5,p9,p13,p15, ncol = 2, 
          hjust = -0.5, vjust = -0.5)

title <- ggdraw() + 
  draw_label(
    "Plot 4: Effect of main independent variables on number of days spoken in assemblies",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

}
plot_grid(p_, ncol = 1,
          rel_heights = c(0.1,1)) 




### Plot 5 ? - Marginal effects of uncontested elections depending on tenure
##main session
{
  
  data_full <- read.csv("replication_data_nov23.csv")
  
  
  
  p2 <- data_full %>% group_by(year = period, name, reelect, mutohyo, party, age, totseat, 選挙区名, pref_id
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
  
  ##remove speakers and those in leadership positions and speaches outside analysis period
  p2_filter <- p2 %>% filter(year != 5, year != 0)
  p2_filter <- p2_filter %>% filter(c_speaker_2 != 1)
  data_main <- p2_filter
  rm(cand_, cand_2, cand_3, data_full, dist_data, dist_data2, p2, p2_filter)
  
  }
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

data_main <- data_main %>% mutate(mag = case_when(totseat %in% c(1) ~ 1,
                                                  totseat %in% c(2,3) ~ 2,
                                                  totseat %in% c(4,5,6,7) ~ 3,
                                                  totseat %in% c(8,9,10,11,12,13,14,15,16,17,18,19) ~ 4),
                                  reelect_3 = case_when(reelect < 2 ~ 1,
                                                        reelect %in% c(2,3,4) ~ 2,
                                                        reelect %in% c(5,6,7) ~ 3,
                                                        reelect > 7 ~ 4),
                                  meansize_2 = case_when(meansize_10k <= 5 ~ 1,
                                                         meansize_10k <= 10 & meansize_10k > 5 ~ 2,
                                                         meansize_10k <= 20 & meansize_10k > 10 ~ 3,
                                                         meansize_10k > 20 ~ 4))
{
mod  <- glm.nb(obs_5 ~factor(mutohyo)+ totseat + reelect + secretary + city_assembly
               + party + gov_ruling + age +  factor(year)+ factor(pref_id), data = data_main)
m1 <-mod
p1 <- plot_cme(mod, effect = "mutohyo", condition = c(""))+
  geom_hline(yintercept = 0, linetype = 2) + xlab("")+ ylab("")+ ggtitle("Effect of uncontested") +scale_x_discrete(labels = "Uncontested")

mod  <- glm.nb(obs_5 ~ factor(mutohyo)*totseat + reelect + secretary + city_assembly
               + party + gov_ruling + age +  factor(year)+ factor(pref_id), data = data_main)
p2 <- plot_cme(mod, effect = "mutohyo", condition = c("totseat"))+
  geom_hline(yintercept = 0, linetype = 2) + xlab("")+ ylab("")+ ggtitle("District magnitude") +scale_x_discrete(limits = c("1","2","3","4"),
                                                                                                                 labels = c("SMD", "2 - 3 seats", "4 - 7 seats",
                                                                                                                            "8 + seats ") )
m2 <-mod


mod  <- glm.nb(obs_5 ~ factor(mutohyo)*reelect + totseat + secretary + city_assembly
               + party + gov_ruling + age +  factor(year)+ factor(pref_id), data = data_main)
p3 <- plot_cme(mod, effect = "mutohyo", condition = c("reelect"))+
  geom_hline(yintercept = 0, linetype = 2) + xlab("")+ ylab("")+ ggtitle("Tenure (# reelected)") +scale_x_discrete(limits = c("1","2","3","4"),labels = c("Less than 2", 
                                                                                                                                                                          "2-4 times", 
                                                                                                                                                                        "5-7 times",
                                                                                                                                                                          "more than 7") )
m3 <-mod
mod  <- glm.nb(obs_5 ~ factor(mutohyo)*factor(meansize_2) + reelect + totseat + secretary + city_assembly
               + gov_ruling + age +  factor(year)+ factor(pref_id), data = data_main)
p4 <- plot_cme(mod, effect = "mutohyo", condition = c("meansize_2"))+
  geom_hline(yintercept = 0, linetype = 2) + xlab("")+ ylab("")+ ggtitle("Municipality population")+scale_x_discrete(limits = c(1,2,3,4,5),
                                                                                                               labels = c("Less than 50,000", "50,000 - 100,000", 
                                                                                                                          "100,000 - 200,000",
                                                                                                                          "More than 200,000") )
m4 <-mod
#mod  <- glm.nb(obs_5 ~ factor(mutohyo)*factor(party) + reelect + totseat + secretary + city_assembly
 #              + gov_ruling + age +  factor(year)+ factor(pref_id), data = data_main)
#p4 <- plot_cme(mod, effect = "mutohyo", condition = c("party"))+
 # geom_hline(yintercept = 0, linetype = 2) + xlab("")+ ylab("")+ ggtitle("Party affiliation")+scale_x_discrete(limits = c( "自","無","共","民","社","公","他","維"),
  #                                                                                                             labels = c("LDP", "Unaffiliated", 
   #                                                                                                                       "JCP", "DPJ",
    #                                                                                                                    "SDP", "Komei", "Other", "Ishin") )

p_ <- plot_grid(p1, p2, p3, p4, nrow  = 2)
print(p_)

title <- ggdraw() + 
  draw_label(
    "Plot 5: Marginal effects of uncontested elections on participation by main confounding variables",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )


plot_grid( p_, ncol = 1,
          rel_heights = c(0.1,1)) 
}


mod  <- glm.nb(obs_5 ~ totseat*reelect + secretary + city_assembly
               + party + gov_ruling + age +  factor(year)+ factor(pref_id), data = data_main)

summary(mod)

plot_cme(mod, effect = "totseat", condition = c("reelect"))+
  geom_hline(yintercept = 0, linetype = 2) + xlab("")+ ylab("")+ ggtitle("District magnitude")

### Plot 6: Plot on un_fulll_2
{
mod  <- glm.nb(obs_5 ~factor(un_full_2)+ totseat + reelect + secretary + city_assembly
               + party + gov_ruling + age+ meansize_10k + pop_65over_perc +  factor(year)+ factor(pref_id), data = data_main)

var_set <- summary(marginaleffects::marginalmeans(mod))
var_set <- var_set[var_set$term == "un_full_2",]
var_set$zero <- var_set$estimate[var_set$value == 0]
#var_set <- var_set[var_set$value != 0]

var_set$diff <- var_set$estimate - var_set$zero
var_set$ci <- var_set$conf.high-var_set$estimate

p1 <- ggplot(var_set, aes(x = factor(value), y = diff)) +geom_pointrange(aes(ymin = diff-ci, ymax = diff+ci)) +
  geom_hline(yintercept = 0, linetype = 2) + xlab("")+ ylab("N days")+ ggtitle("") +
  scale_x_discrete(limits = c("0","1","2","3"),labels = c("Contested", 
                                                          "Uncontested (1 term)", 
                                                          "Uncontested (2 terms)",
                                                          "Uncontested (3 terms)") )


p_2 <- plot_grid(p1, nrow = 1)
title <- ggdraw() + 
  draw_label(
    "",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(title, p_2, ncol = 1,
          rel_heights = c(0.1,1)) 
}




              