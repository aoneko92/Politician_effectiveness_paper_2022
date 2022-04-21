#### Plots for manuscript - Spring 2022 ####
library(dplyr)
library(ggplot2)
library(scales)
library(cowplot)

length(unique(p2_filter$pref_id))

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
##generate analysis df for tab 2 before running scripts ("")

{
plot <- p2_filter %>% group_by(mutohyo) %>% summarise(n = n(),mean = mean(obs_2), sd = sd(obs_2)) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

p1 <- ggplot(plot) +
  geom_bar( aes(x=factor(mutohyo), y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=factor(mutohyo), ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("")+ 
  scale_x_discrete(labels= c("Contested", "Uncontested")) + xlab("")

p2 <- ggplot(p2_filter, aes(factor(mutohyo), obs_2)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + 
  scale_x_discrete(name = "# times reelected") +
  scale_y_continuous(name = "N remarks", trans = log_trans(),breaks = c(1, 5, 25, 125, 625)) + 
  ggtitle("") + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red") + xlab("") +
  scale_x_discrete(labels= c("Contested", "Uncontested")) + labs(caption = "")

plot <- p2_filter %>% group_by(un_full_2) %>% summarise(n = n(),mean = mean(obs_2), sd = sd(obs_2)) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

p3 <- ggplot(plot) +
  geom_bar( aes(x=un_full_2, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=un_full_2, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("")+
  scale_x_discrete(labels= c("Cont.", "Uncont. (1)", "Uncont. (2)", "Uncont. (3+)"))+ xlab("")

p4 <- ggplot(p2_filter, aes(factor(un_full_2), obs_2)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + 
  scale_x_discrete(name = "# times reelected") +
  scale_y_continuous(name = "N remarks", trans = log_trans(),breaks = c(1, 5, 25, 125, 625)) + 
  ggtitle("") + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red") + xlab("") +
  scale_x_discrete(labels= c("Cont.", "Uncont. (1)", "Uncont. (2)", "Uncont. (3+)")) + labs(caption = "")

plot <- p2_filter %>% group_by(party) %>% summarise(n = n(),mean = mean(obs_2), sd = sd(obs_2)) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

p5 <- ggplot(plot) +
  geom_bar( aes(x=party, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=party, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("")+
  scale_x_discrete(labels = c("LDP", "Ishin", "JCP", "KMT", "SDP", "Other", "DPJ", "Unaf"))+ xlab("")

p6 <- ggplot(p2_filter, aes(factor(party), obs_2)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + 
  scale_x_discrete(name = "# times reelected") +
  scale_y_continuous(name = "N remarks", trans = log_trans(),breaks = c(1, 5, 25, 125, 625)) + 
  ggtitle("") + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red") + xlab("") +
  scale_x_discrete(labels = c("LDP", "Ishin", "JCP", "KMT", "SDP", "Other", "DPJ", "Unaf")) + labs(caption = "")

lab <- c("A - Uncontested","","B - Uncontested (# terms)","","C - Party","")

p_ <- plot_grid(p1, p2, p3, p4, p5, p6, ncol = 2, 
          labels =lab)
lab <- c("A - Uncontested","B - Uncontested","C - Uncontested","D - Uncontested","E - Party","F - Party")
title <- ggdraw() + 
  draw_label(
    "Plot 4: Effect of main independent variables on legislator performance",
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
plot_grid(title, p_, ncol = 1,
          rel_heights = c(0.1,1)) 

### Plot 5 - Effect depending on district characteristics
p2_filter <- p2_filter %>% mutate(mag = case_when(totseat %in% c(1) ~ 1,
                                     totseat %in% c(2) ~ 2,
                                     totseat %in% c(3) ~ 3,
                                     totseat %in% c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18) ~ 4))


p2_filter <- p2_filter[!is.na(p2_filter$mag),]

{
  plot <- p2_filter %>% group_by(mag) %>% summarise(n = n(),mean = mean(obs_2), sd = sd(obs_2)) %>%
    mutate( se=sd/sqrt(n))  %>%
    mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
  
  p1 <- ggplot(plot) +
    geom_bar( aes(x=factor(mag), y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
    geom_errorbar( aes(x=factor(mag), ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
    ggtitle("")+ 
    scale_x_discrete(labels= c("SMD", "2 seat MMD", "3 seat MMD", "4+ seat MMD")) + xlab("")
  
  p2 <- ggplot(p2_filter, aes(factor(mag), obs_2)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + 
    scale_x_discrete(name = "# times reelected") +
    scale_y_continuous(name = "N remarks", trans = log_trans(),breaks = c(1, 5, 25, 125, 625)) + 
    ggtitle("") + 
    stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red") + xlab("") +
    scale_x_discrete(labels= c("SMD", "2 seat MMD", "3 seat MMD", "4+ seat MMD")) + labs(caption = "")
  
  p2_filter <- p2_filter %>% mutate(mag_2 = case_when(meansize_10k < 3 ~ 1,
                                                    meansize_10k >= 3 & meansize_10k < 20 ~ 2,
                                                    meansize_10k > 20 ~ 3))
  
  p2_filter <- p2_filter[!is.na(p2_filter$mag_2),]
  
  
  plot <- p2_filter %>% group_by(mag_2) %>% summarise(n = n(),mean = mean(obs_2), sd = sd(obs_2)) %>%
    mutate( se=sd/sqrt(n))  %>%
    mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
  
  p3 <- ggplot(plot) +
    geom_bar( aes(x=factor(mag_2), y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
    geom_errorbar( aes(x=factor(mag_2), ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
    ggtitle("")+ scale_x_discrete(labels= c("Rural", "Suburban", "Urban")) + xlab("")
  
  p4 <- ggplot(p2_filter, aes(factor(mag_2), obs_2)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + 
    scale_x_discrete(name = "# times reelected") +
    scale_y_continuous(name = "N remarks", trans = log_trans(),breaks = c(1, 5, 25, 125, 625)) + 
    ggtitle("") + 
    stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red") + xlab("") +
    scale_x_discrete(labels= c("Rural", "Suburban", "Urban")) + labs(caption = "")
  

  
  
  p_2 <- plot_grid(p1, p2, p3, p4, ncol = 2, 
                  labels =c("District magnitude", "", "Degree of urbanity", ""))
  
  lab <- c("A - Uncontested","","B - Uncontested (# terms)","","C - Party","")
  title <- ggdraw() + 
    draw_label(
      "Supplementary plot 1: Legislator performance by district characteristics",
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
plot_grid(title, p_2, ncol = 1,
          rel_heights = c(0.1,1)) 


              