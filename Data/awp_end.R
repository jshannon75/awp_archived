#*******************************************************************#
# READ IN DATA ORGANIZED/DATA ORGANIZING ******* OPTION             #
# RECODE/NOT RECODE ****** OPTION                                   #
#   SURVEY SET UP/GGPLOT SET UP                                     #
#     VARIABLE/LAPPLY                                               #
#     SURVEY PACKAGE EXAMPLES                                       #
#     COUNTY/MIDDLESCHOOL ******** OPTION                           #
#                                                                   #
# original project created: April 5, 2017                           #
# this version created: November 6, 2017                            #
# last modifed: 12.12.17                                            #
# Author: Taylor Hafley                                             #
# saved: ("desktop/community/AWP/end)                               #
# title : awp_end.R                                                 #
# copied (selected) from: awp_orgadata.R                            #
# additional notes and outputs in AWP5.r & awp_svy.R                #
# Note date created; script written after properly producing        #
# standard erros, ci, etc.                                          #
#                                                                   #
# steps, actions, options commented throughout                      #
#********************************************************************

#load necessary packages
library(tidyverse)
library(survey)
library (plyr)
#library(stargazer)
#library(magrittr)

# optional read ins; produced in awp_orgdata.R
# can read in data and run from here
# read in one of two options below, recode option
awp <- read_csv("desktop/community/AWP/end/awp_organized.csv")
#awp <- read_csv("desktop/community/AWP/end/awp_organized_recode.csv")

# data_clean

###############################
########### survey ############
###############################

# survey package works best if all variables are stored as factors.
# the following code skips the first five vaariables. then skips 
# variable 21 (age), which is stored as integer

#awp[c(6:20,22:251)] <- lapply(awp[c(6:20,22:251)],as.integer)
awp[c(6:20,22:251)] <- lapply(awp[c(6:20,22:251)],as.factor)

# The below are examples that generage various statistics (such as estimate,
# rates, error terms, etc. at the quadrant level 

# set up survey desing [note: using 'quadwt']
# so, any estimates or  statistics must be produced for the middleschool level. 
awpsvy <- svydesign(id = ~idvar, strata = ~stratumnew, weights = ~quadwt, data = awp)
summary(awpsvy)



# produce sample estimate and standard error for single variable by middleschool
svyby(~black, ~middleschool, design=awpsvy, svytotal, na.rm = TRUE)
svyby(~influenceathens, ~middleschool, design=awpsvy, svytotal, na.rm = TRUE)

# produce percentage estimate and standard error for single variable by middleschool
svyby(~black, ~middleschool, design=awpsvy, svymean, na.rm = TRUE)
svyby(~black, ~middleschool, design=awpsvy, svytotal, na.rm = TRUE)

# extract only standard error from single variable by middleschool
SE(svyby(~black, ~middleschool, design=awpsvy, svytotal, na.rm = TRUE))
SE(svyby(~black, ~middleschool, design=awpsvy, svymean, na.rm = TRUE))

# sample estimate and standard error for combination of variable by middle school )
svyby(~black, ~middleschool, design=awpsvy, svytotal, na.rm = TRUE)
svyby(~black+influenceathens, ~middleschool, design=awpsvy, svytotal, na.rm = TRUE, keep.var = FALSE)

# confidence intervals for estimate (total and percentage)
confint(svyby(~black, ~middleschool, design=awpsvy, svytotal, na.rm = TRUE))
confint(svyby(~black, ~middleschool, design=awpsvy, svymean, na.rm = TRUE))

## extractor functions
(a<-svyby(~black, ~middleschool, awpsvy, svytotal, na.rm = TRUE, deff=TRUE, verbose=TRUE,
          vartype=c("se","cv","cvpct","var")))
deff(a)
SE(a)
cv(a)
coef(a)
confint(a, df=degf(awpsvy))

# produce estimate and standard error for entire sample

variables <- names(awp)[c(6:20,22:251)]
variables

# estimates and standard errors
f <- lapply( variables , function( z ) svyby( as.formula( paste0( "~" , z ) ) , by = ~middleschool , awpsvy, svytotal , na.rm = TRUE, vartype = 'se' ) )

f2a <- ldply (f, data.frame)

f2a %>%
  group_by(middleschool) %>%
  summarize_all(.,sum,na.rm = TRUE)-> f2a

# pull out variables that start with se. gather estimates and errors,
# then create join them as a new column
f2a %>%
  select(starts_with("se.")) -> f3

# last entry should be 'se.call_service5' if not recode; 'se.call_service3' if recoded.
f4 <- f2a %>%
  gather(variable,est,gender0:se.call_service5)

f3a <- f3 %>%
  gather(variable,se,se.gender0:se.call_service5)

f4 <- anti_join(f4,f3a)

#library(magrittr)
f4 %<>%
  mutate(
    id = row_number(variable)
    )

f3a %<>%
  mutate(
    id = row_number(variable)
  )

f5 <- left_join(f4,f3a,by = 'id')

f5a <- f5 %>%
  select(-(c(id,variable.y)))

# write_csv(f5a,"desktop/community/AWP/end/awp_estAll.csv")

# estimates and confidence intervals
g <- lapply( variables , function( z ) svyby( as.formula( paste0( "~" , z ) ) , by = ~middleschool , awpsvy, svytotal , na.rm = TRUE, vartype = 'ci' ) )
g2 <- as.data.frame(g)

# percentages and SE
# k (percentages) is different from f (estimates)
k <- lapply( variables , function( z ) svyby( as.formula( paste0( "~" , z ) ) , by = ~middleschool , awpsvy, svymean , na.rm = TRUE, vartype = 'se' ) )

k2a <- ldply (k, data.frame)

k2a %>%
  group_by(middleschool) %>%
  summarize_all(.,sum,na.rm = TRUE)-> k2a

# pull out variables that start with se. gather estimates and errors,
# then create join them as a new column
k2a %>%
  select(starts_with("se.")) -> k3


# last entry should be 'se.call_service5' if not recode; 'se.call_service3' if recoded.
k4 <- k2a %>%
  gather(variable,pct,gender0:se.call_service5)

k3a <- k3 %>%
  gather(variable,pctse,se.gender0:se.call_service5)

k4 <- anti_join(k4,k3a)

k4 %<>%
  mutate(
    id = row_number(variable)
  )

k3a %<>%
  mutate(
    id = row_number(variable)
  )

k5 <- left_join(k4,k3a,by = 'id')


k5a <- k5 %>%
  select(-(c(id,variable.y)))

# write_csv(k5a,"desktop/community/AWP/end/awp_pctAll.csv")

k5b <- k5 %>%
  select(-(c(middleschool,variable.x, variable.y)))

z5 <- left_join(f5,k5b,by = 'id')

z5 <- z5 %>%
  select(-(c(id,variable.y)))

# write_csv(z5,"desktop/community/AWP/end/awp_estPct.csv")



######################################################################################
# various options listed below for County estimates;
# the first 2 lines resets the survey
# here, awp (1354 observations) reset to factors and use finalwights in survey
# different from awpF because of 1549 objects with quadwight (stratums 17 an 19)
#######################################################################################

awp <- read_csv("desktop/AWP/end/awp_clean.csv")
awp[c(6:20,22:251)] <- lapply(awp[c(6:20,22:251)],as.factor)
awpcntysvy <- svydesign(id = ~idvar, strata = ~stratumnew, weights = ~finalweight, data = awp)

svymean(~black, design=awpcntysvy, na.rm = TRUE)
svyby(~black, ~middleschool, design=awpcntysvy, svytotal, na.rm = TRUE)
svytotal(~black,awpcntysvy, na.rm = TRUE)

# extract only standard error from single variable by middleschool
SE(svyby(~black, ~middleschool, design=awpcntysvy, svytotal, na.rm = TRUE))
SE(svyby(~black, ~middleschool, design=awpcntysvy, svymean, na.rm = TRUE))

# sample estimate and standard error for combination of variable by middle school )
svyby(~black, ~middleschool, design=awpcntysvy, svytotal, na.rm = TRUE)
svyby(~black+influenceathens, ~middleschool, design=awpcntysvy, svytotal, na.rm = TRUE)

svytotal(~black,awpcntysvy, na.rm=TRUE)

# confidence intervals for estimate (total and percentage)
confint(svyby(~black, ~middleschool, design=awpcntysvy, svytotal, na.rm = TRUE))
confint(svyby(~black, ~middleschool, design=awpcntysvy, svymean, na.rm = TRUE))


#################################
# examples below produce models via survey package 
# must read awp values as integers, and then reset svydesign in line 54 above
# or you will receive some errors
# awp[c(6:20,22:251)] <- lapply(awp[c(6:20,22:251)],as.integer)

m1 <- svyglm(checking~monthlyinc, awpsvy)
m2 <- svyglm(checking~black, awpsvy)
m3 <- svyglm(checking~monthlyinc+black,awpsvy)
m4 <- svyglm(checking~monthlyinc+black+gender,awpsvy)
m5 <- svyglm(checking~monthlyinc+black+gender+eviction,awpsvy)
m6 <- svyglm(checking~monthlyinc+black+gender+middleschool,awpsvy)

stargazer(m1, m2, m3,m4,m5, type="text", style="demography", 
          column.labels = c("M1", "M2", "M3", 'M4'), 
          keep.stat="n", model.names=F, align=T)

e1 <- svyglm(eviction ~ monthlyinc, awpsvy)
e2 <- svyglm(eviction ~ black, awpsvy)
e3 <- svyglm(eviction ~ monthlyinc + black, awpsvy)
e4 <- svyglm(eviction ~ monthlyinc + black + gender, awpsvy)
e5 <- svyglm(eviction ~ monthlyinc + black + gender + middleschool, awpsvy)
summary(e5)

stargazer(e1,e2,e3,e4,e5, type="text", style="demography", 
          keep.stat="n", model.names=F, align=T)

t1 <- svyglm(moved~monthlyinc, awpsvy)
t2 <- svyglm(moved~black, awpsvy)
t3 <- svyglm(moved~monthlyinc+black, awpsvy)
t4 <- svyglm(moved ~ monthlyinc + black + gender, awpsvy)
t5 <- svyglm(moved ~ monthlyinc + black + gender + middleschool, awpsvy)

stargazer(t1,t2,t3,t4,t5, type="text", style="demography", 
          keep.stat="n", model.names=F, align=T)

awp %>%
  count(homeownstat)


###
# end of script
###
#####################################################################