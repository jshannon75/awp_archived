#********************************************************************
# read in AWP project data                                          #
# original project created: April 5, 2017                           #
# this version created: September 6, 2017                           #
# last modifed: 9.19.17                                             #
# Author: Taylor Hafley                                             #
# saved: ("desktop/AWP/)                                            #
# title : AWP_hafleyscript.R                                        #
# copied (selected) from AWP5.r                                     #
# additional notes and outputs in AWP5.r                            #
# about logic for decisions made                                    #
# additional outputs in AWP_dataclean                               #
# this reflects a 'stripped-down' logic for each decision           #
#********************************************************************

#load necessary packages
library(tidyverse)

#these can be read in from Dropbox working group folder. I downloaded to my own machine.
awp <- read_csv("desktop/AWP/Master Dataset_Merged with Weights.csv")
awp2 <- read_csv("desktop/AWP/AWP Master Data_With Special Pop Weights.csv")

# reduce awp2 down to necessary variables that do not exist in awp.
# prep for join to awp
# idvar is join variable

# don't need famsize2:safety. These are dichotomous variables created by Grace/another user;
# it is possible retroactively determine the logic/decisions, but in order to create clean, 
# step-X-step documentation, I am excluding them (for now)
# awp2a <- select(awp2, idvar, famsize2:safety,middleschool,specialweightcedarhs:specialweightclarkems)

awp2a <- select(awp2, idvar,middleschool,specialweightcedarhs:specialweightclarkems)

#join
awp <- left_join(awp, awp2a, by = 'idvar')

# export this as proper 'cleaned' dataset for indiviudal survey data
# following join

#write_csv(awp, "desktop/AWP/awp_clean.csv")

#clean/remove text/character columns
text <- select(awp,contains("TEXT"))
awp <- awp[!(awp %in% text)]

text.more <- select(awp, race, emp_ben, welfareben, bankaccounts, healthcond, reason_mental, reason_sud,
                    homeapp, amenities, rentalpay, `_911_rec`, accpd_rec, Q64, Q66, Q75, FORM_NOTES,
                    sudaccess, homeprob, insure_source)
awp <- awp[!(awp %in% text.more)]

rm(text)
rm(text.more)

# export dataset without text columns
#write_csv(awp, "desktop/AWP/awp_clean2.csv") # old copy, includes Grace-created dichot. vars
#write_csv(awp, "desktop/AWP/awp_clean3.csv") # 282 vars

#clean data further to ease data analysis
awp <- select(awp,-(c(building,newlyrecieved,file,framecount,numsampled,numrespond,noneligible,eligible,papersurvey,flagnewstratum,consent,`_merge1`,esol,`_merge2`)))
awp <- select(awp, -(c(monthlyinc, age, neigh_safe2,`_0to5num`,`_6to12num`,`_13to17num`,`_18to25num`,`_26to64num`,`_65plusnum`,famsize,no_needsudtreat)))

#write_csv(awp, "desktop/AWP/awp_codebook.csv") # 257 vars

###################################################################
#    recode variables
#    see: Hafley_codebook.xls
###################################################################

#template
#awp[,10]<-sapply(awp.sml[,10],function(x)ifelse(x>3,3,x))
#awp[,10:12]<-sapply(awp.sml[,10:12],function(x)ifelse(x<1,1,x))

# lines 70 - 165 group variables together based on how they need to be recoded

# 0,1,2,3,4 to 1,2,3 where 4 | 3 = 3; 2 = 2, 0 | 1 = 1

all <- c("influenceathens", "effortathens","schoolclean","schoolpride","schoolconnect","schoolinvolve","schools_dogood",
         "safe_elem","safe_middle","safe_high","schoolwelcome","learn_expect","learn_confident","educ_success",
         "neigh_trust","neigh_getalong","neigh_play","neigh_safe","accpd_satisfy","accpd_speed","accpd_profess",
         "accpd_knowledge","accpd_service","accpd_respect","accpd_fairness","accpd_helpful","accpd_courteous",
         "accpd_friendly","accpd_dependable","accpd_youth","accpd_issues","accpd_quantity","accpd_info","accpd_confidence",
         "accpd_officer_respect","accpd_interest","accpd_goodjob","childcare_afford","childcare_flexible",
         "childcare_safe","childcare_quality","call_satisfy","call_profess","call_knowledge","call_service","call_speed")

awp[,all] <- sapply(awp[,all],function(x)ifelse(x<1,1,x))
awp[,all] <- sapply(awp[,all],function(x)ifelse(x>3,3,x))

#test
awp %>%
  count(childcare_quality)

#0,1,2,3,4 to 0,1 where 0 | 1 = 0 & 2 | 3 | 4 = 1

all2 <- c("atten_govt","healthstat")

awp[,all2] <- sapply(awp[,all2],function(x)ifelse(x==1,0,x))
awp[,all2] <- sapply(awp[,all2],function(x)ifelse(x>1,1,x))


#0,1,2,3,4 to 0,1 where 0 = 0 & 1 | 2 | 3 | 4 = 1

all3 <- c("partnerhurt","partnertalk","partnerthreat","partnerscream","school_culture","school_parent","school_ged",
          "school_literacy","school_health","school_nut","school_esl")

awp[,all3] <- sapply(awp[,all3],function(x)ifelse(x>0,1,x))

# freq relig: 0,1,2,3,4 where 0|1|2 = 0 & 3 | 4 = 1
awp[,"freq_relig"] <- sapply(awp[,"freq_relig"],function(x)ifelse(x<=2,0,x))
awp[,"freq_relig"] <- sapply(awp[,"freq_relig"],function(x)ifelse(x>=3,1,x))

# freq relig: 0,1,2,3,4 where 1 = 1 & 0|2|3|4 = 0
awp[,"maritalstat"] <- sapply(awp[,"maritalstat"],function(x)ifelse(x>=2,0,x))

#intenertsource,acc-child,freq_relig,
# 0,1,2 to 0,1 where 2 | 1 = 1 & 0 = 0

awp[,c("medbill_worry","hh_finance","eviction","neigh_crime")] <- sapply(awp[,c("medbill_worry","hh_finance","eviction","neigh_crime")],function(x)ifelse(x==2,1,x))

# 0,1,2 to 0,1 where 0 | 2 = 0 & 1 = 1

awp[,c("acc_child")] <- sapply(awp[,c("acc_child")],function(x)ifelse(x==2,0,x))

# 1,2,3,4,5 to 0,1 where 1 = 1 & 2|3|4|5 = 0

awp[,c("internetsource")] <- sapply(awp[,c("internetsource")],function(x)ifelse(x>=2,0,x))

# 1,2,3,4,5 to 0,1 where 1|2|3 = 0 & 4|5 = 1

awp[,c("educ_achieve","educ","hoursworked","workstat")] <- sapply(awp[,c("educ_achieve","educ","hoursworked","workstat")],function(x)ifelse(x<=3,0,x))
awp[,c("educ_achieve","educ","hoursworked","workstat")] <- sapply(awp[,c("educ_achieve","educ","hoursworked","workstat")],function(x)ifelse(x>=4,1,x))


# 0,1,2,3 to 0,1 where 0 = 0 & 1|2|3 = 1

all4 <- c("relig_guide","guns","gangviolence","drugs","drugsell","robbery","theft","phys_assault","sex_assault","homeless","eviction_neighborhood","litter","abandoned","vandalism","graffiti","childabuse","domviolence")

#awp[,all4] <- sapply(awp[,all4],function(x)ifelse(x==1,0,x))
awp[,all4] <- sapply(awp[,all4],function(x)ifelse(x>=1,1,x))


# 0,1,2,3 to 0,1 where 0|1 = 0 & 2|3 = 1

all5 <- c("satisfy_life","satisfy_home","satisfy_job","moved","home_maint")

awp[,all5] <- sapply(awp[,all5],function(x)ifelse(x<=1,0,x))
awp[,all5] <- sapply(awp[,all5],function(x)ifelse(x>=2,1,x))

# 1,2,3 to 0,1 where 1 = 1 & 2 | 3 = 0

awp[,"homeownstat"] <- sapply(awp[,"homeownstat"],function(x)ifelse(x>=2,0,x))

# 0,1,9 to 0,1 where 1 = 1 & 0 | 9 = 0

all6 <- c("home_safe","street_safe","park_safe","dt_safe","gang_fight","gang_present","gang_drugs")

awp[,all6] <- sapply(awp[,all6],function(x)ifelse(x==9,0,x))

# 0,1,2,3,4,5,9 to 0,1 where 1 = 1 & 0,2:5,9 = 0

awp[,"transport"] <- sapply(awp[,"transport"],function(x)ifelse(x>=2,0,x))

# 0,1,2,3,4,5,6,7 to 0,1 where 0,1,2 = 0 & >3 = 1

awp[,"fastfoodfreq"] <- sapply(awp[,"fastfoodfreq"],function(x)ifelse(x<=2,0,x))
awp[,"fastfoodfreq"] <- sapply(awp[,"fastfoodfreq"],function(x)ifelse(x>=3,1,x))

# 0,1,2,3,4,5 to 0,1 where 0,1,2 = 0 & 3,4,5 = 1

awp[,"socialgather"] <- sapply(awp[,"socialgather"],function(x)ifelse(x<=2,0,x))
awp[,"socialgather"] <- sapply(awp[,"socialgather"],function(x)ifelse(x>=3,1,x))

awp[,"gender"] <- sapply(awp[,"gender"],function(x)ifelse(x>1,NA,x))

awp[,"wic"] <- sapply(awp[,"wic"],function(x)ifelse(x==10,1,x))

#####################################################################
#    end recode variables
#   colnames(awp)
####################################################################

# organize data

awp[6:249] <- lapply(awp[6:249],as.integer)
awp.int <- awp

#awp[6:249] <- lapply(awp[6:249],as.factor)
#str(awp)
#awp.fact <- awp

# using 'awp' is using awp data as int.
# factors available using awp.fact

#####################################
whoY <- awp %>%
  gather(key = variable, value = value, gender:call_service)
whoY

############################################

whoY7 <- select(whoY, idvar, finalweight, specialweightcoilems:specialweightclarkems, stratumnew, middleschool, variable, value)

#clm <- as.data.frame(xtabs( ~ vava, disit1))


# coile = c(7,9,10,16,17,18)
whoY7 %>%
  group_by(variable) %>%
  filter(stratumnew %in% c(7,9,10,16,17,18)) %>%
  filter(!is.na(value)) %>%
  count(value,wt = specialweightcoilems) %>%
  mutate(freq = n/sum(n),
         pct = freq*100) -> cms

# clarkems = c(1,3,5,13,17,19)
whoY7 %>%
  group_by(variable) %>%
  filter(stratumnew %in% c(1,3,5,13,17,19)) %>%
  filter(!is.na(value)) %>%
  count(value,wt = specialweightclarkems) %>%
  mutate(freq = n/sum(n),
         pct = freq*100) -> clms

# bhlyons = c(4,6,12,15,19)
whoY7 %>%
  group_by(variable) %>%
  filter(stratumnew %in% c(4,6,12,15,19)) %>%
  filter(!is.na(value)) %>%
  count(value,wt = specialweightbhlyonsms) %>%
  mutate(freq = n/sum(n),
         pct = freq*100) -> bhlms

# hilsman = c(2,8,11,14,17,19)
whoY7 %>%
  group_by(variable) %>%
  filter(stratumnew %in% c(2,8,11,14,17,19)) %>%
  filter(!is.na(value)) %>%
  count(value,wt = specialweighthilsmanms) %>%
  mutate(freq = n/sum(n),
         pct = freq*100) -> hms


cms$middleschool <- "Coile"
clms$middleschool <- "Clarke"

clarkeCoile <- full_join(cms, clms)

bhlms$middleschool <- "BHLyons"
hms$middleschool <- "Hilsman"

hilsmanbh <- full_join(bhlms,hms)

ACCms <- full_join(clarkeCoile,hilsmanbh)

ACCms

# county
whoY2 %>%
  group_by(variable) %>%
  filter(!is.na(value)) %>%
  count(value,wt = finalweight) %>%
  mutate(freq = n/sum(n),
         pct = freq*100) ->wfna2
wfna2

#write_csv(wfna2, "desktop/AWP/awp_codebookFinal.csv")
####################################
