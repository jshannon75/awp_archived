#********************************************************************
# read in AWP project data                                          #
# original project created: April 5, 2017                           #
# this version created: October 28, 2017                            #
# last modifed: 11.9.17                                            #
# Author: Taylor Hafley                                             #
# saved: ("desktop/AWP/)                                            #
# title : AWP_orgdata.R                                             #
# copied (selected) from awp_svy.r                                  #
# additional notes and outputs in AWP5.r & awp_svy.R                #
# about logic for decisions made                                    #
# additional outputs in AWP_dataclean                               #
# this reflects a 'stripped-down' logic for each decision           #
#********************************************************************

#load necessary packages
library(tidyverse)
library(survey)
#library(stargazer)
#library(magrittr)

#these can be read in from Dropbox working group folder. I downloaded to my own machine.

awp <- read_csv("desktop/AWP/Master Dataset_Merged with Weights.csv")
awp2 <- read_csv("desktop/AWP/AWP Master Data_With Special Pop Weights.csv")

# reduce awp2 down to necessary variables that do not exist in awp.
# prep for join to awp
# idvar is join variable

# don't need famsize2:safety. These are dichotomous variables created by Grace/another user;
# it is possible retroactively determine the logic/decisions, but in order to create clean, 
# step-X-step documentation, I am excluding them

awp2a <- select(awp2, idvar,middleschool,specialweightcedarhs:specialweightclarkems)

awp <- left_join(awp, awp2a, by = 'idvar')

#clean/remove text/character columns
text <- select(awp,contains("TEXT"))
awp <- awp[!(awp %in% text)]

text.more <- select(awp, race, emp_ben, welfareben, bankaccounts, healthcond, reason_mental, reason_sud,
                    homeapp, amenities, rentalpay, `_911_rec`, accpd_rec, Q64, Q66, Q75, FORM_NOTES,
                    sudaccess, homeprob, insure_source)
awp <- awp[!(awp %in% text.more)]

rm(text)
rm(text.more)

# additional deletions
awp <- select(awp,-(c(building,newlyrecieved,file,framecount,numsampled,numrespond,noneligible,
                      eligible,papersurvey,flagnewstratum,consent,`_merge1`,esol,`_merge2`,
                      neigh_safe2,`_0to5num`,`_6to12num`,`_13to17num`,`_18to25num`,`_26to64num`,
                      `_65plusnum`,famsize,no_needsudtreat)))
# 259 variables
# export this as 'cleaned' dataset for indiviudal survey data following join
# write_csv(awp, "desktop/AWP/end/awp_clean.csv")


# set stratrum 18 to Coile Middleschool. Special population 18 only feeds into Coile
awp[awp$stratumnew==18,]$middleschool <- 'Coile'

# lines 65 - 133 set up data so that special population (stratrum 17 and 19) observations
# have a single quadrant weight rather than multiple
sp3 <- awp %>%
  filter(stratumnew == 17)

sp3a <- awp %>%
  filter(stratumnew == 17)

sp3b <- awp %>%
  filter(stratumnew == 17)

sp3$middleschool <- 'Coile'
sp3$quadwt <- sp3$specialweightcoilems

sp3a$middleschool <- 'Clarke'
sp3a$quadwt <- sp3a$specialweightclarkems

sp3b$middleschool <- 'Hilsman'
sp3b$quadwt <- sp3b$specialweighthilsmanms

sp3c <- full_join(sp3,sp3a)
sp3d <- full_join(sp3c,sp3b)

sp4 <- awp %>%
  filter(stratumnew == 19)

sp4a <- awp %>%
  filter(stratumnew == 19)

sp4b <- awp %>%
  filter(stratumnew == 19)

sp4$middleschool <- 'BHLyons'
sp4$quadwt <- sp4$specialweightbhlyonsms

sp4a$middleschool <- 'Clarke'
sp4a$quadwt <- sp4a$specialweightclarkems

sp4b$middleschool <- 'Hilsman'
sp4b$quadwt <- sp4b$specialweighthilsmanms

sp4c <- full_join(sp4,sp4a)
sp4d <- full_join(sp4c,sp4b)

sp4d %>%
  group_by(middleschool) %>%
  count(stratumnew)

#join stratum 17 and stratum 19
spX <- full_join(sp4d,sp3d)

rm(sp3,sp3a,sp3b,sp3c,sp3d,sp4,sp4a,sp4b,sp4c,sp4d)

# extract stratum 17 and 19 from awp to assign quadwt
# for all non special population variables + stratum 18
# stratums 17 and 19 have three weights

awp <- awp %>% 
  filter(!((stratumnew %in% c(17,19))))

vn <- awp %>%
  filter(middleschool == 'Clarke') %>%
  mutate(
    quadwt = specialweightclarkems
  ) 

vn2 <- awp %>%
  filter(middleschool == 'Coile') %>%
  mutate(
    quadwt = specialweightcoilems
  ) 

vn3 <- awp %>%
  filter(middleschool == 'Hilsman') %>%
  mutate(
    quadwt = specialweighthilsmanms
  ) 

vn4 <- awp %>%
  filter(middleschool == 'BHLyons') %>%
  mutate(
    quadwt = specialweightbhlyonsms
  )

vn5a <- full_join(vn,vn2)
vn5b <- full_join(vn5a,vn3)
vnX <- full_join(vn5b,vn4)

rm(vn, vn2, vn3, vn4, vn5a, vn5b)

awpF <- full_join(vnX,spX)

awpF %>%
  group_by(middleschool) %>%
  count(stratumnew)

# ch <- select(awpF,middleschool,stratumnew,quadwt,specialweightcoilems:specialweightclarkems)

# write_csv(awpF, "desktop/AWP/end/awp_organized.csv")



###################################################################
#    recode variables
#    see: Hafley_codebook.xls
###################################################################

# 0,1,2,3,4 to 1,2,3 where 4 | 3 = 3; 2 = 2, 0 | 1 = 1

all <- c("influenceathens", "effortathens","schoolclean","schoolpride","schoolconnect","schoolinvolve","schools_dogood",
         "safe_elem","safe_middle","safe_high","schoolwelcome","learn_expect","learn_confident","educ_success",
         "neigh_trust","neigh_getalong","neigh_play","neigh_safe","accpd_satisfy","accpd_speed","accpd_profess",
         "accpd_knowledge","accpd_service","accpd_respect","accpd_fairness","accpd_helpful","accpd_courteous",
         "accpd_friendly","accpd_dependable","accpd_youth","accpd_issues","accpd_quantity","accpd_info","accpd_confidence",
         "accpd_officer_respect","accpd_interest","accpd_goodjob","childcare_afford","childcare_flexible",
         "childcare_safe","childcare_quality","call_satisfy","call_profess","call_knowledge","call_service","call_speed")

awpF[,all] <- sapply(awpF[,all],function(x)ifelse(x<1,1,x))
awpF[,all] <- sapply(awpF[,all],function(x)ifelse(x>3,3,x))

#test

#0,1,2,3,4 to 0,1 where 0 | 1 = 0 & 2 | 3 | 4 = 1
all2 <- c("atten_govt","healthstat")
awpF[,all2] <- sapply(awpF[,all2],function(x)ifelse(x==1,0,x))
awpF[,all2] <- sapply(awpF[,all2],function(x)ifelse(x>1,1,x))

#0,1,2,3,4 to 0,1 where 0 = 0 & 1 | 2 | 3 | 4 = 1
all3 <- c("partnerhurt","partnertalk","partnerthreat","partnerscream","school_culture","school_parent","school_ged",
          "school_literacy","school_health","school_nut","school_esl")
awpF[,all3] <- sapply(awpF[,all3],function(x)ifelse(x>0,1,x))

# freq relig: 0,1,2,3,4 where 0|1|2 = 0 & 3 | 4 = 1
awpF[,"freq_relig"] <- sapply(awpF[,"freq_relig"],function(x)ifelse(x<=2,0,x))
awpF[,"freq_relig"] <- sapply(awpF[,"freq_relig"],function(x)ifelse(x>=3,1,x))

# freq relig: 0,1,2,3,4 where 1 = 1 & 0|2|3|4 = 0
awpF[,"maritalstat"] <- sapply(awpF[,"maritalstat"],function(x)ifelse(x>=2,0,x))
#intenertsource,acc-child,freq_relig,

# 0,1,2 to 0,1 where 2 | 1 = 1 & 0 = 0
awpF[,c("medbill_worry","hh_finance","eviction","neigh_crime")] <- sapply(awpF[,c("medbill_worry","hh_finance","eviction","neigh_crime")],function(x)ifelse(x==2,1,x))

# 0,1,2 to 0,1 where 0 | 2 = 0 & 1 = 1
awpF[,c("acc_child")] <- sapply(awpF[,c("acc_child")],function(x)ifelse(x==2,0,x))

# 1,2,3,4,5 to 0,1 where 1 = 1 & 2|3|4|5 = 0
awpF[,c("internetsource")] <- sapply(awpF[,c("internetsource")],function(x)ifelse(x>=2,0,x))

# 1,2,3,4,5 to 0,1 where 1|2|3 = 0 & 4|5 = 1
awpF[,c("educ_achieve","educ","hoursworked","workstat")] <- sapply(awpF[,c("educ_achieve","educ","hoursworked","workstat")],function(x)ifelse(x<=3,0,x))
awpF[,c("educ_achieve","educ","hoursworked","workstat")] <- sapply(awpF[,c("educ_achieve","educ","hoursworked","workstat")],function(x)ifelse(x>=4,1,x))

# 0,1,2,3 to 0,1 where 0 = 0 & 1|2|3 = 1
all4 <- c("relig_guide","guns","gangviolence","drugs","drugsell","robbery","theft","phys_assault","sex_assault","homeless","eviction_neighborhood","litter","abandoned","vandalism","graffiti","childabuse","domviolence")

awpF[,all4] <- sapply(awpF[,all4],function(x)ifelse(x>=1,1,x))

# 0,1,2,3 to 0,1 where 0|1 = 0 & 2|3 = 1
all5 <- c("satisfy_life","satisfy_home","satisfy_job","moved","home_maint")
awpF[,all5] <- sapply(awpF[,all5],function(x)ifelse(x<=1,0,x))
awpF[,all5] <- sapply(awpF[,all5],function(x)ifelse(x>=2,1,x))

# 1,2,3 to 0,1 where 1 = 1 & 2 | 3 = 0
awpF[,"homeownstat"] <- sapply(awpF[,"homeownstat"],function(x)ifelse(x>=2,0,x))

# 0,1,9 to 0,1 where 1 = 1 & 0 | 9 = 0
all6 <- c("home_safe","street_safe","park_safe","dt_safe","gang_fight","gang_present","gang_drugs")
awpF[,all6] <- sapply(awpF[,all6],function(x)ifelse(x==9,0,x))

# 0,1,2,3,4,5,9 to 0,1 where 1 = 1 & 0,2:5,9 = 0
awpF[,"transport"] <- sapply(awpF[,"transport"],function(x)ifelse(x>=2,0,x))

# 0,1,2,3,4,5,6,7 to 0,1 where 0,1,2 = 0 & >3 = 1
awpF[,"fastfoodfreq"] <- sapply(awpF[,"fastfoodfreq"],function(x)ifelse(x<=2,0,x))
awpF[,"fastfoodfreq"] <- sapply(awpF[,"fastfoodfreq"],function(x)ifelse(x>=3,1,x))

# 0,1,2,3,4,5 to 0,1 where 0,1,2 = 0 & 3,4,5 = 1
awpF[,"socialgather"] <- sapply(awpF[,"socialgather"],function(x)ifelse(x<=2,0,x))
awpF[,"socialgather"] <- sapply(awpF[,"socialgather"],function(x)ifelse(x>=3,1,x))
awpF[,"gender"] <- sapply(awpF[,"gender"],function(x)ifelse(x>1,NA,x))
awpF[,"wic"] <- sapply(awpF[,"wic"],function(x)ifelse(x==10,1,x))

# write_csv(awpF, "desktop/AWP/end/awp_organized_recode.csv")

#####################################################################
#    end recode variables
# read that in.     **** option at the top

