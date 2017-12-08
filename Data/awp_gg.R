# code is originally copied from
# lines 596- 1055 in awp_svy.R
# start on line 15
# created: October 28, 2017
# Author: Taylor
# read in data: awp_organized
###########################################


#can read in data and run from here ** #
library(tidyverse)
awp <- read_csv("desktop/AWP/end/awp_organized.csv")

awp[6:249] <- lapply(awp[6:249],as.integer)
str(awp)

# which(colnames(awp) == 'age')

# using 'awp' is using awp data as int.
#####################################

whoY <- awp %>%
  gather(key = variable, value = value, gender:call_service, na.rm = TRUE)
whoY

# remove some of the unneeded columns in whoY
whoY7 <- select(whoY, idvar, finalweight, quadwt, specialweightcoilems:specialweightclarkems, stratumnew, middleschool, variable, value)
whoY7

############################################

# ACCms is the sample estimate and percent estimate for each variable
# by middleschool

whoY7 %>%
  group_by(variable, middleschool) %>%
  filter(!is.na(value)) %>%
  count(value, wt = quadwt) %>%
  mutate(freq = n/sum(n),
         pct = freq*100) -> ACCms
ACCms


# write_csv(ACCms, "desktop/AWP/ACCms.csv")

# accms2 and accms3 are  just the spread and gathered versions of pct or estimate. 

accms2 <- ACCms %>% 
  unite(vava,variable,value) %>%
  select(middleschool, vava, pct) 
head(accms2)

acc3b <- accms2 %>%
  spread(vava,pct)

acc3$call_satisfy_3
acc3b$call_satisfy_3
#write_csv(acc3b, "desktop/AWP/end/acc3_recode.csv")

# creates acc2 or acc3 from above for sample estimats rather than percentages
# the below output is the same output as svyby in svymean or svytotaol

#ACCms %>% 
#  unite(vava,variable,value) %>%
#  select(middleschool, vava, n) %>%
#  spread(vava,n)

acc2b <- select(ACCms,middleschool,variable,value,n)
acc2b

awp88 <- awp %>%
  gather(code, value, gender:call_service, na.rm = TRUE) %>%
  select(idvar, quadwt, finalweight, specialweightcoilems:specialweightclarkems, stratumnew, middleschool, code, value)

# keeps NA's
#awp89 <- awp %>%
#  gather(key = variable, value = value, gender:call_service)

# county
whoY7 %>%
  group_by(variable) %>%
  filter(!is.na(value)) %>%
  count(value,wt = finalweight) %>%
  mutate(freq = n/sum(n),
         pct = freq*100) ->wfna2

wfna2b <- wfna2 %>% 
  unite(vava,variable,value) %>%
  select(vava, freq) 
head(wfna2b)

wfna2c <- wfna2b %>%
  spread(vava,freq)

wfna2c %>%
  count(other_race_0)

#####################################################
########    gg PLOT    ##############################
#####################################################

# ggplot where it is a subset of specific questions (i.e. police questions) to show how they
# do overall, or examine subsets of those questions to highlight high and low responses

awp[c(6:20,22:251)] <- lapply(awp[c(6:20,22:251)],as.factor)

# produce various visualizations
awp %>%
  group_by(middleschool) %>%
  count(n = black, wt = quadwt) %>%
  ggplot(mapping = aes(x = middleschool, y = nn, fill = n)) +
  geom_bar(stat = "identity", position = 'fill') +
  scale_fill_brewer(type = 'div')

awp %>%
  group_by(middleschool) %>%
  filter(!is.na(black)) %>%
  count(n = black, wt = quadwt) %>%
  ggplot(mapping = aes(x = middleschool, y = nn, fill = n)) +
  geom_bar(stat = "identity", position = 'fill', width = 0.6) +
  scale_fill_brewer(type = 'div') +
  coord_flip()

awp %>%
  group_by(middleschool) %>%
  count(n = black, wt = quadwt) %>%
  ggplot(mapping = aes(x = middleschool, y = nn)) +
  geom_boxplot()

awp %>%
  group_by(middleschool) %>%
  count(n = black, wt = finalweight) %>%
  ggplot(mapping = aes(x = middleschool, y = nn, fill = n)) +
  geom_bar(stat = "identity", position = 'fill')

ggplot(data = awp) + 
  geom_bar(mapping = aes(x = middleschool, fill = as.factor(schoolwelcome)), position = "dodge")

ggplot(data = awp, mapping = aes(x = middleschool, fill = as.factor(schoolwelcome))) + 
  geom_bar(alpha = 2/5, position = "identity")


####################################
### base/template for bar graphs ###
####################################

awp %>%
  group_by(middleschool) %>%
  filter(!is.na(influenceathens)) %>%
  count(n = influenceathens, wt = quadwt) %>%
  ggplot(mapping = aes(x = middleschool, y = nn, fill = n)) +
  geom_bar(stat = "identity", position = 'fill',width = .6) +
  xlab('') +
  ylab("% of responses") +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = 'Pastel1', label = c('Strongly disagree', 'Disagree', 'Neither', 'Agree', 'Strongly agree')) +
  ggtitle("Influence Athens") +
  coord_flip() -> p

#######################################

awp %>%
  group_by(middleschool) %>%
  filter(!(is.na(black))) %>%
  count(n = black, wt = quadwt) %>%
  ggplot(mapping = aes(x = middleschool, y = nn, fill = n)) +
  geom_bar(stat = "identity", width = .6) +
  xlab('') +
  ylab("Total Population") +
  theme(legend.position = "bottom" ) + 
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette = 'Pastel1', label = c('Other', 'Black')) +
  ggtitle("Black Population by Middle School") +
  coord_flip()-> p2
p2

awp %>%
  group_by(middleschool) %>%
  filter(!(is.na(monthlyinc))) %>%
  count(n = monthlyinc, wt = quadwt) %>%
  ggplot(mapping = aes(x = middleschool, y = nn, fill = n)) +
  geom_bar(stat = "identity", position = 'fill', width = .6) +
  xlab('') +
  ylab("Total Population") +
  theme(legend.position = "bottom" ) + 
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette = 'Pastel1', label = c('$0- $1000',
                                                 '$1001- $2000',
                                                 '$2001- $3000',
                                                 '$3001- $4000',
                                                 '$4001- $5000',
                                                 '$5001- $6000',
                                                 '$6001-$7000',
                                                 '$7001+' )) +
  ggtitle('Annual Household Income') +
  coord_flip()-> p3
p3


awp %>%
  group_by(middleschool) %>%
  filter(!(is.na(accpd_satisfy))) %>%
  count(n = accpd_satisfy, wt = quadwt) %>%
  ggplot(mapping = aes(x = middleschool, y = nn, fill = n)) +
  geom_bar(stat = "identity", position = 'fill', width = .6) +
  xlab("Middleschool") +
  ylab("n") +
  theme(legend.position = "bottom" ) +
  scale_fill_brewer(palette = 'Pastel2', label = c('agree', 'neutral', 'disagree')) +
  guides(color=guide_legend("Satsify Police")) +
  coord_flip() -> p4
p4

awp %>%
  group_by(middleschool) %>%
  filter(!(is.na(accpd_goodjob))) %>%
  count(n = accpd_goodjob, wt = quadwt) %>%
  ggplot(mapping = aes(x = middleschool, y = nn, fill = n)) +
  geom_bar(stat = "identity", position = 'fill', width = .6) +
  xlab("Middleschool") +
  ylab("n") +
  theme(legend.position = "bottom" ) +
  scale_fill_brewer(palette = 'Pastel2', label = c('agree', 'neutral', 'disagree')) +
  guides(color=guide_legend("Satsify Police")) +
  coord_flip() -> p5
p5


awp %>%
  group_by(middleschool) %>%
  filter(!(is.na(accpd_satisfy))) %>%
  count(n = accpd_satisfy, wt = quadwt) %>%
  ggplot(mapping = aes(x = middleschool, y = nn, fill = n)) +
  geom_bar(stat = "identity", position = 'fill', width = .6) +
  xlab("Middleschool") +
  ylab("Number of Responses") +
  theme(legend.position = "bottom" ) +
  scale_fill_brewer(palette = 'BrBG', labels = c('Very dissatisfied','Dissatisfied',
                                                 'Neither','Satisfied','Very satisfied')) +
  ggtitle("Overall, how satisfied were you with the \nassistance provided by the ACC Police Officer?") +
  coord_flip() -> p6
p6

awp %>%
  group_by(middleschool) %>%
  filter(!(is.na(savings))) %>%
  count(n = savings, wt = quadwt) %>%
  ggplot(mapping = aes(x = middleschool, y = nn, fill = n)) +
  geom_bar(stat = "identity", position = 'fill', width = .6) +
  xlab("Middleschool") +
  ylab("Total Responses") +
  theme(legend.position = "bottom" ) +
  scale_fill_brewer(palette = 'Pastel2') +
  ggtitle("Do you have a savings account?") -> p7
p7

awp %>%
  ggplot(mapping = aes(x = black, y = savings)) +
  geom_bar(stat = "identity", width = .6) +
  xlab("Middleschool") +
  ylab("n") +
  theme(legend.position = "bottom" ) +
  scale_fill_brewer(palette = 'Pastel2') +
  guides(color=guide_legend("Satsify Police")) +
  coord_flip() +
  facet_wrap(~middleschool)-> p8
p8

awp %>%
  group_by(middleschool) %>%
  filter(!is.na(black)) %>%
  count(black, n = checking,wt = quadwt) %>%
  ggplot(mapping = aes(x = black, y = nn, fill = n)) +
  geom_bar(stat = "identity", width = .6) +
  scale_x_discrete("Black", drop = FALSE) +
  ylab("Total") +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette = 'Accent', labels = c('No','Yes')) + 
  ggtitle("Checking Account by Black Pop by Middle School")  +
  facet_wrap(~middleschool) -> p9
p9

awp %>%
  group_by(middleschool) %>%
  count(black, n = checking, wt = quadwt)

awp %>%
  group_by(middleschool) %>%
  count(checking, wt = quadwt)

awp %>%
  group_by(middleschool) %>%
  filter(!is.na(black)) %>%
  count(black, n = savings,wt = quadwt) %>%
  ggplot(mapping = aes(x = black, y = nn, fill = n)) +
  geom_bar(stat = "identity", width = .6) +
  scale_x_discrete("Black", drop = FALSE) +
  ylab("Total") +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette = 'Accent', labels = c('No','Yes')) + 
  ggtitle("Savings Account by Black Pop by Middle School")  +
  facet_wrap(~middleschool) -> p10
p10





###
# end of script
###