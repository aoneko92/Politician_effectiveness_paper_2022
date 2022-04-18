#### Plots for manuscript - Spring 2022 ####
library(dplyr)
library(ggplot2)
library(scales)



## plot 1 - Number of uncontested elections across years
{
data <- read.csv("pref_candidats8518_Natori,Horiuchi.csv")
variable.names(data)

data <- data %>% group_by(year, pref_id = kencode, distname1, totseat, mutohyo) %>% summarise()
data <- data %>% mutate(mutohyo_n = mutohyo*totseat)
data_19 <- read.csv("dist_data_2019.csv")
data <- data[data$pref_id %in% unique(data_19$pref_id),]

data <- data %>% group_by(year) %>% summarize(totseat = sum(totseat, na.rm = T), mutohyo = sum(mutohyo), mutohyo_n = sum(mutohyo_n, na.rm = T))

data <- data %>% mutate(year_p = case_when(year < 1991 ~ 1,
                                   year < 1995 & year >= 1991 ~ 2,
                                   year < 1999 & year >= 1995 ~ 3,
                                   year < 2003 & year >= 1999 ~ 4,
                                   year < 2007 & year >= 2003 ~ 5,
                                   year < 2011 & year >= 2007 ~ 6,
                                   year < 2015 & year >= 2011 ~ 7,
                                   year >= 2015 ~ 8))



data_19 <- read.csv("dist_data_2019.csv")
data_19 <- data_19 %>% summarize(totseat = sum(totseat, na.rm = T), mutohyo_n = sum(mutohyo_seat, na.rm = T), perc =  mutohyo_n/totseat)
data_19$year_p <- 9

data <- data %>% group_by(year_p) %>% summarise(totseat = sum(totseat, na.rm = T), mutohyo_n = sum(mutohyo_n, na.rm = T), perc = mutohyo_n/totseat)
data <- data[!is.na(data$year_p),] 
}
data <- rbind(data, data_19)
ggplot(data, aes(x = factor(year_p), y = perc*100, group = 1, label = round((perc*100), digits = 2))) + geom_line() + geom_label_repel()+ geom_point() + ylim(0, 30) +
  scale_x_discrete(labels=c("1" = "1987 - 1990", "2" = "1991 - 1994", "3" = "1995 - 1998",
                            "4" = "1999 - 2002", "5" = "2003 - 2006", "6" = "2007 - 2010",
                            "7" = "2011 - 2014", "8" = "2015 - 2018", "9" = "2019 - ")) +
  ggtitle("Plot 1: Uncontested prefectural assembly elections 1987 - 2019*") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) + 
  ylab("% members uncontested")+
  xlab("Period") + labs(caption = "*Includes only values from 44 prefectures that hold elections during unified local elections. 1987 - 2018 results were generated 
                                  from the Japanese Local Elections Dataset (Horiuchi 2019) while 2019 values were generated from the NHK elections database
                                  (https://www.nhk.or.jp/senkyo/database/touitsu/2019/)")



### Plot 2 - Uncontested by prefecture population
{
  
  ##laod population figures
  library(readr)
  library(ggrepel)
  
  pref_pop <- read_csv("estat_pref_population_apr18.csv")
  pref_pop$pref_id <- pref_pop$`全国・都道府県 コード`/1000
  pref_pop$totpop <- as.numeric(unlist(pref_pop[,4]))
  pref_pop <- pref_pop[,c("pref_id", "totpop", "en_pref")]
  
  ##prepare main data
  data <- read.csv("pref_candidats8518_Natori,Horiuchi.csv")
  data <- data %>% group_by(year, kencode, distname1) %>% mutate(rank = dense_rank(desc(votet)),
                                                         win_ldp = case_when(rank <= totseat & ldp == 1 ~ 1))
  
  data$win_ldp <- ifelse(!is.na(data$win_ldp), data$win_ldp, ifelse(data$mutohyo == 1 & data$ldp == 1, 1, 0))
  #data$win_ldp <- ifelse(is.na(data$win_ldp), 0, data$win_ldp)
  data <- data %>% group_by(year, pref_id = kencode, distname1, totseat, mutohyo, vote) %>% summarise(win_ldp = sum(win_ldp), N = n())
  data <- data %>% mutate(mutohyo_n = mutohyo*totseat, win_ldp = win_ldp/N)
  data$win_ldp <- ifelse(data$win_ldp > 0, 1, 0)
  
  data <- data %>% group_by(year, pref_id, distname1, totseat, mutohyo) %>% summarise(win_ldp = sum(win_ldp))
  
  data <- data %>% mutate(mutohyo_n = mutohyo*totseat)
  data <- data %>% group_by(year, pref_id) %>% summarize(totseat = sum(totseat, na.rm = T), mutohyo = sum(mutohyo), mutohyo_n = sum(mutohyo_n, na.rm = T), win_ldp = sum(win_ldp))
  
  data <- data %>% mutate(year_p = case_when(year < 1991 ~ 1,
                                             year < 1995 & year >= 1991 ~ 2,
                                             year < 1999 & year >= 1995 ~ 3,
                                             year < 2003 & year >= 1999 ~ 4,
                                             year < 2007 & year >= 2003 ~ 5,
                                             year < 2011 & year >= 2007 ~ 6,
                                             year < 2015 & year >= 2011 ~ 7,
                                             year >= 2015 ~ 8))
  data <- data %>% filter(year_p == 8)
  
  data <- merge(data, pref_pop, by = "pref_id")
  data$en_pref <- gsub("-ken", "",data$en_pref)
  }
ggplot(data, aes(x = (win_ldp/totseat)*100, y = (mutohyo_n/totseat)*100, label = en_pref)) + geom_point() +
  geom_label_repel() + 
  xlab("% seats LDP") + ylab("% seats uncontested") +
  ggtitle("Plot 2: Uncontested seats in prefectural assemblies compared to LDP seat share (2015)") + 
  geom_smooth(method = "lm")


###load analysis data (see "analysis_replications.R")
###create p2_filter for tab1 and tab2

### Plot 3 - Obs density plot
plot_3 <- p2_filter %>% group_by(obs_2) %>% summarise(sum = (n()/nrow(p2)*100))
median(p2_filter$obs_2) ##get median

plot_3$median <- plot_3$obs_2 == 11
ggplot(plot_3, aes(obs_2, sum)) + geom_col(aes(fill = median)) + scale_fill_manual(name = 'Median ( N = 11)', guide = 'legend',labels = c("NO", "YES"), values = c("darkgrey", "red")) + 
  xlab("N remarks") + 
  ylab("percentage %") + xlim(0,200) +
  ggtitle("Plot 3: Density plot of number of remarks per member")  + labs(caption = "* X-axis cut off at 200 questions. ")


### Plot 4 - Effect of uncontested on n remarks
ggplot(p2_filter, aes(factor(un_full_2), obs_2)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + 
  scale_x_discrete(name = "# times reelected") +
  scale_y_continuous(name = "N remarks / year *logarithmic axis", trans = log_trans(),breaks = c(1, 5, 25, 125, 625)) + 
  ggtitle("Plot 4: Number of remarks by consecutive times uncontested") + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red") + xlab("") +
  scale_x_discrete(labels= c("Contested", "Uncontested (1 term)", "Uncontested (2 term)", "Uncontested (3 term+)")) + labs(caption = "
       
       Each dot represents the number of times one legislator had made remarks during a four-year period. 
                                                                                                                           Red dot signifies the mean in each group")

### Plot 5 - Effect of uncontested on n remarks
ggplot(p2_filter_smd, aes(factor(un_full_2), obs)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + 
  scale_x_discrete(name = "# times reelected") +
  scale_y_continuous(name = "N remarks / year *logarithmic axis", trans = log_trans(),breaks = c(1, 5, 25, 125, 625)) + 
  ggtitle("Plot 4: Number of remarks by consecutive times uncontested") + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red") + xlab("") +
  scale_x_discrete(labels= c("Contested", "Uncontested (1 term)", "Uncontested (2 term)", "Uncontested (3 term+)")) + labs(caption = "
       
       Each dot represents the number of times one legislator had made remarks during a four-year period. 
                                                                                                                           Red dot signifies the mean in each group")

              