
library(tidyverse)
library(plm)
library(stargazer)

#### Robustness checks ####


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




### Using different measurement of uncontested
### FOR VARIABLE CREATION, SEE "election_result_vars:feb23.R"

cand_mg <- read.csv("elec_varsfeb26.csv")

look_m_3[c("X.x", "X.y")] <- NULL
look_m_4 <- merge(look_m_3, cand_mg, by = c("pref_id", "選挙区名", "name", "period"), all.x = T)


{
  p2 <- look_m_4 %>% group_by(year = period, name, mutohyo, party, reelect, age, totseat, 選挙区名, pref_id,
                              incum, un_full, un_sum, sekihai, 
                              city_assembly, lawyer, party_boss, party_boss_2, secretary, c_speaker, c_speaker_2,
                              mutohyo_uncomp, mutohyo_uncomp_2, mutohyo_uncomp_3,
                              competitiveness, un_full_spec_.1, un_full_spec_.2, un_full_spec_.3) %>% summarize(obs_2 = length(V1), 
                                                                                           obs_3 = length(unique(date)))
  p2 <- p2 %>% group_by(name, year) %>% filter(!n()>1) 
  p2$distname_p <- p2$選挙区名
  
  p2 <- transform(p2, id2=apply(p2[c("name", "distname_p",  "pref_id")], 1, paste, collapse="."))
  stopifnot(!any(duplicated(p2[c("name", "pref_id", "year")])))
  p2$party <- ifelse(!p2$party %in% c("自","共","無","公","民","社","維"), "他", p2$party)
  p2 <- within(p2, party <- relevel(as.factor(party), ref = "自")) ##set the new reference for party to LDP
  
  
  
  
  p2$un_full_spec_.1 <- ifelse(p2$un_full_spec_.1 >= 3, 3, p2$un_full_spec_.1) #mmakes it so 3 is 3 or more times uncontested
  p2$un_full_spec_.2 <- ifelse(p2$un_full_spec_.2 >= 3, 3, p2$un_full_spec_.2) #mmakes it so 3 is 3 or more times uncontested
  p2$un_full_spec_.3 <- ifelse(p2$un_full_spec_.3 >= 3, 3, p2$un_full_spec_.3) #mmakes it so 3 is 3 or more times uncontested
  
  ## fix the NA issue (all the NA cases are uncontested - therefore assume that un_full == 1)
  #.1
  p2$un_full_spec_1 <- ifelse(is.na(p2$un_full_spec_.1), 1, p2$un_full_spec_.1)
  unique(p2[p2$reelect == 1,]$un_full_2)
  p2$un_full_spec_1 <- ifelse(p2$reelect == 1 & p2$un_full_spec_1 > 0, 1, p2$un_full_spec_1)
  p2$un_full_spec_1 <- ifelse(p2$mutohyo == 0, 0, p2$un_full_spec_1)
  #.2
  p2$un_full_spec_2 <- ifelse(is.na(p2$un_full_spec_.2), 1, p2$un_full_spec_.2)
  unique(p2[p2$reelect == 1,]$un_full_2)
  p2$un_full_spec_2 <- ifelse(p2$reelect == 1 & p2$un_full_spec_2 > 0, 1, p2$un_full_spec_2)
  p2$un_full_spec_2 <- ifelse(p2$mutohyo == 0, 0, p2$un_full_spec_2)
  #.2
  p2$un_full_spec_3 <- ifelse(is.na(p2$un_full_spec_.3), 1, p2$un_full_spec_.3)
  unique(p2[p2$reelect == 1,]$un_full_2)
  p2$un_full_spec_3 <- ifelse(p2$reelect == 1 & p2$un_full_spec_3 > 0, 1, p2$un_full_spec_3)
  p2$un_full_spec_3 <- ifelse(p2$mutohyo == 0, 0, p2$un_full_spec_3)
  
  p2$un_full <- ifelse(p2$un_full >= 3, 3, p2$un_full) #mmakes it so 3 is 3 or more times uncontested
  ## fix the NA issue (all the NA cases are uncontested - therefore assume that un_full == 1)
  p2$un_full_2 <- ifelse(is.na(p2$un_full), 1, p2$un_full)
  unique(p2[p2$reelect == 1,]$un_full_2)
  p2$un_full_2 <- ifelse(p2$reelect == 1 & p2$un_full_2 > 0, 1, p2$un_full_2)
  p2$un_full_2 <- ifelse(p2$mutohyo == 0, 0, p2$un_full_2)
  
  

}

p2_filter <- p2[p2$year != 0 & p2$year != 5,] %>% filter(c_speaker_2 != 1, party_boss_2 != 1)
p2_filter <- within(p2_filter, party <- relevel(as.factor(party), ref = "自")) ##set the new reference for party to LDP



## threshold .1
m1 <- feglm.nb(obs_2 ~ factor(mutohyo_uncomp) + reelect + 
                 age + totseat| id2 + year, data = p2_filter)

summary(m1)
## threshold .1
m2 <- feglm.nb(obs_2 ~ factor(mutohyo_uncomp_2) + reelect + 
                 age + totseat| id2 + year, data = p2_filter)

summary(m2)

## threshold .2 
m3 <- feglm.nb(obs_2 ~ factor(mutohyo_uncomp_3) + reelect + 
                 age + totseat| id2 + year, data = p2_filter)

summary(m3)
## competitiveness (first over last)
m4 <- feglm.nb(obs_2 ~ competitiveness + reelect + 
                 age + totseat| id2 + year, data = p2_filter)
summary(m4)

screenreg(list(m1,m2,m3,m4),  type = "clustered", cluster = ~id2 + year, stars = c(0.01, 0.05, 0.1, 0.15), 
          include.loglik = T)
htmlreg(list(m1,m2,m3,m4,m5,m6), type = "clustered", cluster = ~id2 + year, stars = c(0.01, 0.05, 0.1, 0.15), 
        file = "si_tab1_apr20.doc", include.loglik = T)

### inclusion of lagged dependent variable
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


m1 <- plm(obs_2 ~  factor(mutohyo) + c_speaker_2 + reelect + 
            age + totseat, data = p2_filt,
          index = c("id2", "year"),
          model = "within", effect = "twoways")
cov1 <- clubSandwich::vcovCR(m1, type = "CR2", cluster = p2_filt$id2)
cov_m1  <- sqrt(diag(cov1))

m2 <- plm(obs_2 ~ factor(un_full_2) + c_speaker_2 + reelect + 
            age + totseat, data = p2_filt,
          index = c("id2", "year"),
          model = "within", effect = "twoways")
cov2 <- clubSandwich::vcovCR(m2, type = "CR2", cluster = p2_filt$id2)
cov_m2  <- sqrt(diag(cov2))

m3 <- plm(obs_2 ~ lag(obs_2) + factor(mutohyo) + c_speaker_2 + reelect + 
            age + totseat, data = p2_filt,
          index = c("id2", "year"),
          model = "within", effect = "twoways")
cov3 <- clubSandwich::vcovCR(m3, type = "CR2", cluster = p2_filt$id2)
cov_m3  <- sqrt(diag(cov3))

m4 <- plm(obs_2 ~ lag(obs_2) +  factor(un_full_2) + c_speaker_2 + reelect + 
            age + totseat, data = p2_filt,
          index = c("id2", "year"),
          model = "within", effect = "twoways")
cov4 <- clubSandwich::vcovCR(m4, type = "CR2", cluster = p2_filt$id2)
cov_m4  <- sqrt(diag(cov4))


stargazer(m1,m2,m3,m4, type = "text",
          se = list(cov_m1,cov_m2,cov_m3,cov_m4 ))
stargazer(m1,m2,m3,m4, type = "html",
          se = list(cov_m1,cov_m2,cov_m3,cov_m4 ), out = "si_tab2_apr20.html")

### tests on different dependent variable
##create dummy for those who abstain whole year
p4 <- look_m_3 %>% group_by(year_speech, period, name, pref_id) %>% summarize()
see <- p4 %>% group_by(period, name, pref_id) %>% summarize(N = n())                                                                                                                             
see <- see %>% group_by(period) %>% mutate(year_n = N, max = max(N), below = N<max,
                                           y_1 = (N == 1),
                                           y_2 = (N == 2),
                                           y_3 = (N == 3),
                                           y_4 = (N == 4),
                                           y_5 = (N == 5)
                                           
                                           )
look_m_5 <- merge(look_m_3, see, by = c("period", "name", "pref_id"))

{
p2 <- look_m_5 %>% group_by(year = period, name, mutohyo, party, reelect, age, totseat, 選挙区名, pref_id,
                            , incum, un_full, un_sum, sekihai, vote_perc,
                            city_assembly, lawyer, party_boss, party_boss_2, secretary, c_speaker, c_speaker_2, max, below, year_n) %>% summarize(obs = length(V1), 
                                                                                                                obs_3 = length(unique(date)))
p2 <- p2 %>% group_by(name, year) %>% filter(!n()>1) 

p2 <- transform(p2, id2=apply(p2[c("name", "選挙区名",  "pref_id")], 1, paste, collapse="."))
stopifnot(!any(duplicated(p2[c("name", "pref_id", "year")])))
p2$party <- ifelse(!p2$party %in% c("自","共","無","公","民","社","維"), "他", p2$party)
p2 <- within(p2, party <- relevel(as.factor(party), ref = "自")) ##set the new reference for party to LDP

p2$un_full <- ifelse(p2$un_full >= 3, 3, p2$un_full) #mmakes it so 3 is 3 or more times uncontested
## fix the NA issue (all the NA cases are uncontested - therefore assume that un_full == 1)
p2$un_full_2 <- ifelse(is.na(p2$un_full), 1, p2$un_full)
unique(p2[p2$reelect == 1,]$un_full_2)
p2$un_full_2 <- ifelse(p2$reelect == 1 & p2$un_full_2 > 0, 1, p2$un_full_2)
p2$un_full_2 <- ifelse(p2$mutohyo == 0, 0, p2$un_full_2)


p2 <- p2 %>% group_by(pref_id) %>% mutate(norm_obs =  obs/mean(obs),
                                          norm_obs_3 = obs_3/mean(obs_3))
}

p2_filt <- p2[p2$year != 0 & p2$year != 5,] %>% filter(c_speaker_2 != 1, party_boss_2 != 1)

##obs_3 (N of days speaking in assembly)
m1 <- feglm.nb(obs_3 ~ factor(mutohyo) + reelect + 
                 age + totseat| id2 + year, data = p2_filt)
summary(m1)
m2 <- feglm.nb(obs_3 ~ factor(un_full_2) + reelect + 
                 age + totseat| id2 + year, data = p2_filt)
summary(m2)

##below (years of activity)
m3 <- feglm.nb(year_n ~ factor(mutohyo) + reelect + 
                 age + totseat| id2 + year, data = p2_filt)
summary(m3)

m4 <- feglm.nb(year_n ~ factor(un_full_2) + reelect + 
                 age + totseat| id2 + year, data = p2_filt)
summary(m4)

screenreg(list(m1,m2,m3,m4),  type = "clustered", cluster = ~id2 + year, stars = c(0.01, 0.05, 0.1, 0.15), 
          include.loglik = T)
htmlreg(list(m1,m2,m3,m4), type = "clustered", cluster = ~id2 + year, stars = c(0.01, 0.05, 0.1, 0.15), 
        file = "si_tab3_apr20.doc", include.loglik = T)



debug_contr_error <- function (dat, subset_vec = NULL) {
  if (!is.null(subset_vec)) {
    ## step 0
    if (mode(subset_vec) == "logical") {
      if (length(subset_vec) != nrow(dat)) {
        stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
      }
      subset_log_vec <- subset_vec
    } else if (mode(subset_vec) == "numeric") {
      ## check range
      ran <- range(subset_vec)
      if (ran[1] < 1 || ran[2] > nrow(dat)) {
        stop("'numeric' `subset_vec` provided but values are out of bound")
      } else {
        subset_log_vec <- logical(nrow(dat))
        subset_log_vec[as.integer(subset_vec)] <- TRUE
      } 
    } else {
      stop("`subset_vec` must be either 'logical' or 'numeric'")
    }
    dat <- base::subset(dat, subset = subset_log_vec)
  } else {
    ## step 1
    dat <- stats::na.omit(dat)
  }
  if (nrow(dat) == 0L) warning("no complete cases")
  ## step 2
  var_mode <- sapply(dat, mode)
  if (any(var_mode %in% c("complex", "raw"))) stop("complex or raw not allowed!")
  var_class <- sapply(dat, class)
  if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
    stop("matrix variables with 'AsIs' class must be 'numeric'")
  }
  ind1 <- which(var_mode %in% c("logical", "character"))
  dat[ind1] <- lapply(dat[ind1], as.factor)
  ## step 3
  fctr <- which(sapply(dat, is.factor))
  if (length(fctr) == 0L) warning("no factor variables to summary")
  ind2 <- if (length(ind1) > 0L) fctr[-ind1] else fctr
  dat[ind2] <- lapply(dat[ind2], base::droplevels.factor)
  ## step 4
  lev <- lapply(dat[fctr], base::levels.default)
  nl <- lengths(lev)
  ## return
  list(nlevels = nl, levels = lev)
}
