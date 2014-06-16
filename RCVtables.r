###################################
# Kellen Gracey                   #
# Department of Political Science #
# University of Iowa              #
# Ranked Choice Voting Results    #
###################################

library(foreign)
mydata <- read.dta("Updated.dta")

library(aod)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
library(rms)
library(apsrtable)
library(robustbase)
library(sandwich)

# housekeeping recodes
mydata$newedu[mydata$education=="incomplete"] <- 1
mydata$newedu[mydata$education=="highschool"] <- 2
mydata$newedu[mydata$education=="somecollege"] <- 3
mydata$newedu[mydata$education=="collegegraduate"] <- 4
mydata$newedu[mydata$education=="post-graduate"] <- 5

mydata$newwhite[mydata$white=="non-white"] <- 0
mydata$newwhite[mydata$white=="white"] <- 1

mydata$polint[mydata$q26=="Hardly at all?"] <- 1
mydata$polint[mydata$q26=="Only now and then, or"] <- 2
mydata$polint[mydata$q26=="Some of the time"] <- 3
mydata$polint[mydata$q26=="Most of the time"] <- 4

mydata$mobilization[mydata$q22=="No"] <- 0
mydata$mobilization[mydata$q22=="Yes"] <- 1
mydata$mobilization[mydata$q22=="Dont know [VOL]"] <- NA
mydata$mobilization[mydata$q22=="Refused [VOL]"] <- NA

mydata$city_satisfaction[mydata$q27=="Very satisfied"] <- 3
mydata$city_satisfaction[mydata$q27=="Fairly satisfied"] <- 2
mydata$city_satisfaction[mydata$q27=="Not very satisfied"] <- 1
mydata$city_satisfaction[mydata$q27=="Not at all satisfied"] <- 0
mydata$city_satisfaction[mydata$q27=="No opinion_DK"] <- NA
mydata$city_satisfaction[mydata$q27=="Refused"] <- NA

########################
# Positive Campaigning #
########################


# create factor variable for ologit

mydata$positive_campaigning[mydata$q33=="Dont know"] <- NA
mydata$positive_campaigning[mydata$q33=="Refused"] <- NA
mydata$positive_campaigning[mydata$q33=="More"] <- NA
mydata$positive_campaigning[mydata$q33=="Less"] <- NA
mydata$positive_campaigning <- as.numeric(mydata$positive_campaigning)
mydata$positive_campaigning[mydata$q33=="About the same"] <- 3

mydata$positive_campaigning[mydata$q34a=="a lot"] <- 1
mydata$positive_campaigning[mydata$q34a=="a little"] <- 2
mydata$positive_campaigning[mydata$q34a=="about the same [vol]"] <- 3

mydata$positive_campaigning[mydata$q34b=="a lot"] <- 5
mydata$positive_campaigning[mydata$q34b=="a little"] <- 4
mydata$positive_campaigning[mydata$q34b=="about the same [vol]"] <- 3



mydata$positive_campaigning.f <- factor(mydata$positive_campaigning, labels=c("A lot more negative", "A little more negative", "About the same", "A little less negative", "A lot less negative"))
mydata$logit_positive.n[mydata$positive_campaigning==1] <- 0
mydata$logit_positive.n[mydata$positive_campaigning==2] <- 0
mydata$logit_positive.n[mydata$positive_campaigning==3] <- 0
mydata$logit_positive.n[mydata$positive_campaigning==4] <- 1
mydata$logit_positive.n[mydata$positive_campaigning==5] <- 1

mydata$fairness1[mydata$q35=="No"] <- 0
  mydata$fairness1[mydata$q35=="Yes"] <- 1
  mydata$fairness1[mydata$q35=="Dont know"] <- NA
  mydata$fairness1[mydata$q35=="Refused"] <- NA

mydata$newint[mydata$q28=="Interesting"] <- 1
mydata$newint[mydata$q28=="Dull"] <- 0
mydata$newint[mydata$q28=="[DO NOT READ] Neither"] <- NA
mydata$newint[mydata$q28=="DK"] <- NA
mydata$newint[mydata$q28=="Refused"] <- NA

mydata$useful_information.n[mydata$q30=="A great deal of useful information"] <- 4
  mydata$useful_information.n[mydata$q30=="Some information"] <- 3
  mydata$useful_information.n[mydata$q30=="Not too much information"] <- 2
  mydata$useful_information.n[mydata$q30=="No useful information"] <- 1
  mydata$useful_information.n[mydata$q30=="Dont know"] <- NA
  mydata$useful_information.n[mydata$q30=="Refused"] <- NA

mydata$praise[mydata$q31=="Dont know"] <- NA
mydata$praise[mydata$q31=="Refused"] <- NA

mydata$ocrit.n[mydata$q32=="A great deal of time"] <- 4
mydata$ocrit.n[mydata$q32=="Some of the time"] <- 3
mydata$ocrit.n[mydata$q32=="Not too much, or"] <- 2
mydata$ocrit.n[mydata$q32=="They werent doing this at all"] <- 1
mydata$ocrit.n[mydata$q32=="Dont know"] <- NA
mydata$ocrit.n[mydata$q32=="Refused"] <- NA

mydata$no_criticize.n[mydata$ocrit.n==1] <- 0
mydata$no_criticize.n[mydata$ocrit.n==2] <- 0
mydata$no_criticize.n[mydata$ocrit.n==3] <- 1
mydata$no_criticize.n[mydata$ocrit.n==4] <- 1


# logit
log.pos.base <- glm(logit_positive.n ~ rcv_voting + newwhite + age + male + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
log.pos.step1 <- glm(logit_positive.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, family="binomial")
log.pos.step2 <- glm(logit_positive.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
log.pos.final <- glm(logit_positive.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, family="binomial")

log.pos.base$se<-vcovHC(log.pos.base, type="HC1")
log.pos.step1$se<-vcovHC(log.pos.step1, type="HC1")
log.pos.step2$se<-vcovHC(log.pos.step2, type="HC1")
log.pos.final$se<-vcovHC(log.pos.final, type="HC1")


# robust se check
log.pos.base.r <- glmrob(logit_positive.n ~ rcv_voting + newwhite + age + male + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
log.pos.step1.r <- glmrob(logit_positive.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, family="binomial")
log.pos.step2.r <- glmrob(logit_positive.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
log.pos.final.r <- glmrob(logit_positive.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, family="binomial")

lpos.base.r.sum <- summary(log.pos.base.r)
lpos.step1.r.sum <- summary(log.pos.step1.r)
lpos.step2.r.sum <- summary(log.pos.step2.r)
lpos.final.r.sum <- summary(log.pos.final.r)

# ordered logit
olog.pos.base <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, Hess = TRUE)
olog.pos.step1 <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, Hess = TRUE)
olog.pos.step2 <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, Hess = TRUE)
olog.pos.final <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, Hess = TRUE)

# robust se check
olog.pos.base.r <- glmrob(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
olog.pos.step1.r <- glmrob(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, family="binomial")
olog.pos.step2.r <- glmrob(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
olog.pos.final.r <- glmrob(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, family="binomial")

pos.base.r.sum <- summary(olog.pos.base.r)
pos.step1.r.sum <- summary(olog.pos.step1.r)
pos.step2.r.sum <- summary(olog.pos.step2.r)
pos.final.r.sum <- summary(olog.pos.final.r)

olog.pos.base <- glm(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
olog.pos.step1 <- glm(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, family="binomial")
olog.pos.step2 <- glm(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
olog.pos.final <- glm(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, family="binomial")

olog.pos.base$se<-vcovHC(olog.pos.base, type="HC1")
olog.pos.step1$se<-vcovHC(olog.pos.step1, type="HC1")
olog.pos.step2$se<-vcovHC(olog.pos.step2, type="HC1")
olog.pos.final$se<-vcovHC(olog.pos.final, type="HC1")

##################
# RCV Preference #
##################

# logit
log.pref.base <- glm(rcv_pref ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
log.pref.step1 <- glm(rcv_pref ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, family="binomial")
log.pref.step2 <- glm(rcv_pref ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
log.pref.final <- glm(rcv_pref ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, family="binomial")

##################
# Less Criticism #
##################
mydata$ocrit.f <- factor(mydata$ocrit.n, labels=c("A great deal of time", "Some of the time", "Not too much", "Not at all"))

# logit
log.crit.base <- glm(no_criticize.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
log.crit.step1 <- glm(no_criticize.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, family="binomial")
log.crit.step2 <- glm(no_criticize.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
log.crit.final <- glm(no_criticize.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, family="binomial")

log.crit.base$se<-vcovHC(log.crit.base, type="HC1")
log.crit.step1$se<-vcovHC(log.crit.step1, type="HC1")
log.crit.step2$se<-vcovHC(log.crit.step2, type="HC1")
log.crit.final$se<-vcovHC(log.crit.final, type="HC1")


# robust se check
log.crit.base.r <- glmrob(no_criticize.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
log.crit.step1.r <- glmrob(no_criticize.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, family="binomial")
log.crit.step2.r <- glmrob(no_criticize.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
log.crit.final.r <- glmrob(no_criticize.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, family="binomial")

log.crit.base.r.sum <- summary(log.crit.base.r)
log.crit.step1.r.sum <- summary(log.crit.step1.r)
log.crit.step2.r.sum <- summary(log.crit.step2.r)
log.crit.final.r.sum <- summary(log.crit.final.r)

# ordered logit
olog.crit.base <- polr(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, Hess = TRUE)
olog.crit.step1 <- polr(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, Hess = TRUE)
olog.crit.step2 <- polr(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, Hess = TRUE)
olog.crit.final <- polr(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, Hess = TRUE)

# robust se check
olog.crit.base.r <- glmrob(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
olog.crit.step1.r <- glmrob(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, family="binomial")
olog.crit.step2.r <- glmrob(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
olog.crit.final.r <- glmrob(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, family="binomial")

crit.base.r.sum <- summary(olog.crit.base.r)
crit.step1.r.sum <- summary(olog.crit.step1.r)
crit.step2.r.sum <- summary(olog.crit.step2.r)
crit.final.r.sum <- summary(olog.crit.final.r)

olog.crit.base <- glm(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
olog.crit.step1 <- glm(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, family="binomial")
olog.crit.step2 <- glm(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
olog.crit.final <- glm(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, family="binomial")

olog.crit.base$se<-vcovHC(olog.crit.base, type="HC1")
olog.crit.step1$se<-vcovHC(olog.crit.step1, type="HC1")
olog.crit.step2$se<-vcovHC(olog.crit.step2, type="HC1")
olog.crit.final$se<-vcovHC(olog.crit.final, type="HC1")


#########################
# Campaign Satisfaction #
#########################

mydata$campaign_satisfaction.n[mydata$q29=="Very satisfied"] <- 4
mydata$campaign_satisfaction.n[mydata$q29=="Fairly satisfied"] <- 3
mydata$campaign_satisfaction.n[mydata$q29=="Not very satisfied"] <- 2
mydata$campaign_satisfaction.n[mydata$q29=="Not at all satisfied"] <- 1
mydata$campaign_satisfaction.n[mydata$q29=="No opinion_DK"] <- NA
mydata$campaign_satisfaction.n[mydata$q29=="Refused"] <- NA


mydata$campaign_satisfaction.f <- factor(mydata$campaign_satisfaction.n, labels=c("Not at all satisfied", "Not very satisfied", "Fairly satisfied", "Very satisfied"))
mydata$logit_satisfied.n[mydata$campaign_satisfaction.n==1] <- 0
mydata$logit_satisfied.n[mydata$campaign_satisfaction.n==2] <- 0
mydata$logit_satisfied.n[mydata$campaign_satisfaction.n==3] <- 0
mydata$logit_satisfied.n[mydata$campaign_satisfaction.n==4] <- 1
mydata$logit_satisfied.n[mydata$campaign_satisfaction.n==5] <- 1

# logit
log.sat.base <- glm(logit_satisfied.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
log.sat.step1 <- glm(logit_satisfied.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, family="binomial")
log.sat.step2 <- glm(logit_satisfied.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
log.sat.final <- glm(logit_satisfied.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, family="binomial")

log.sat.base$se<-vcovHC(log.sat.base, type="HC1")
log.sat.step1$se<-vcovHC(log.sat.step1, type="HC1")
log.sat.step2$se<-vcovHC(log.sat.step2, type="HC1")
log.sat.final$se<-vcovHC(log.sat.final, type="HC1")


# robust se check
log.sat.base.r <- glmrob(logit_satisfied.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
log.sat.step1.r <- glmrob(logit_satisfied.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, family="binomial")
log.sat.step2.r <- glmrob(logit_satisfied.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
log.sat.final.r <- glmrob(logit_satisfied.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, family="binomial")

log.sat.base.r.sum <- summary(log.sat.base.r)
log.sat.step1.r.sum <- summary(log.sat.step1.r)
log.sat.step2.r.sum <- summary(log.sat.step2.r)
log.sat.final.r.sum <- summary(log.sat.final.r)

# ordered logit
olog.sat.base <- polr(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, Hess = TRUE)
olog.sat.step1 <- polr(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, Hess = TRUE)
olog.sat.step2 <- polr(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, Hess = TRUE)
olog.sat.final <- polr(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, Hess = TRUE)

olog.sat.base <- glm(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
olog.sat.step1 <- glm(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, family="binomial")
olog.sat.step2 <- glm(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
olog.sat.final <- glm(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, family="binomial")

olog.sat.base$se<-vcovHC(olog.sat.base, type="HC1")
olog.sat.step1$se<-vcovHC(olog.sat.step1, type="HC1")
olog.sat.step2$se<-vcovHC(olog.sat.step2, type="HC1")
olog.sat.final$se<-vcovHC(olog.sat.final, type="HC1")


# robust se check
olog.sat.base.r <- glmrob(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
olog.sat.step1.r <- glmrob(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, family="binomial")
olog.sat.step2.r <- glmrob(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
olog.sat.final.r <- glmrob(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, family="binomial")

olog.sat.base.r.sum <- summary(olog.sat.base.r)
olog.sat.step1.r.sum <- summary(olog.sat.step1.r)
olog.sat.step2.r.sum <- summary(olog.sat.step2.r)
olog.sat.final.r.sum <- summary(olog.sat.final.r)




#################
# Null Findings #
#################

mydata$useful_information.f <- factor(mydata$useful_information.n, labels=c("No useful information", "Not too much information", "Some information", "A great deal of useful information"))
mydata$candidate_satisfaction.f <- factor(mydata$candidate_satisfaction, labels=c("Not at all satisfied", "Not very satisfied", "Don't know", "Fairly satisfied", "Very satisfied"))

# logit
log.fair <- glm(fairness1 ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")

log.praise <- glm(praise ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
log.int <- glm(newint ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")

# ordered logit
olog.info <- polr(useful_information.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, Hess = TRUE)

# logit candidate choice satisfaction
mydata$cand_sat_logit[mydata$candidate_satisfaction.f=="Not at all satisfied"] <- 0
mydata$cand_sat_logit[mydata$candidate_satisfaction.f=="Not very satisfied"] <- 0
mydata$cand_sat_logit[mydata$candidate_satisfaction.f=="Don't know"] <- 0
mydata$cand_sat_logit[mydata$candidate_satisfaction.f=="NA"] <- 0
mydata$cand_sat_logit[mydata$candidate_satisfaction.f=="Fairly satisfied"] <- 1
mydata$cand_sat_logit[mydata$candidate_satisfaction.f=="Very satisfied"] <- 1

log.cand.sat <- glm(cand_sat_logit ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mayor_vote, data=mydata, family="binomial")

# Recoding candidate choice satisfaction with missings
attach(mydata)
mydata$cand.choice.recode[q16==NA] <- 1
mydata$cand.choice.recode[mydata$q16==1] <- 5
mydata$cand.choice.recode[mydata$q16==2] <- 4
mydata$cand.choice.recode[mydata$q16==3] <- 2
mydata$cand.choice.recode[mydata$q16==4] <- 1
mydata$cand.choice.recode[mydata$q16==5] <- 3
mydata$cand.choice.recode[mydata$q16==8] <- 3
mydata$cand.choice.recode[mydata$q16==9] <- 3
detach(mydata)

mydata$cand.choice.recode.f <- factor(mydata$cand.choice.recode, labels=c("Not at all satisfied", "Not very satisfied", "Don't know", "Fairly satisfied", "Very satisfied"))
mydata$satall.f <- factor(mydata$satall)
mydata$recodecouncil.f <- factor(mydata$recodecouncil)
mydata$recodemayor.f <- factor(mydata$recodemayor)

olog.cand <- polr(satall.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mayor_vote + mobilization, data=mydata, Hess = TRUE)
reg.cand <- lm(satall ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mayor_vote + mobilization, data=mydata)


#######################
# Create LaTeX Tables #
#######################
library(stargazer)
stargazer(olog.pos.base, olog.pos.step1, olog.pos.step2, olog.pos.final,
          title="Voter Perceptions of Campaign Negativity: Ordered Logit Estimates", align=TRUE,
          dep.var.labels=c("Negative Campaigning, 1 = a lot more, 5 = a lot less"),
          se = c(olog.pos.base$se, olog.pos.step1$se, olog.pos.step2$se,olog.pos.final$se),
          ord.intercepts=TRUE, no.space=TRUE,
          notes = c("Unstandardized ordered logistic regression coefficients with robust standard errors in parentheses. Significance levels based on two-tailed tests."),
          covariate.labels=c("RCV", "White", "Age", "Male", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Political Interest", "Mobilization", "City Satisfaction"))

stargazer(log.pos.base, log.pos.step1, log.pos.step2, log.pos.final,
          title="Voter Perceptions of Campaign Negativity: Logit Estimates", align=TRUE,
          dep.var.labels=c("Negative Campaigning, 1 = less, 0 = other"),  no.space=TRUE,
          notes = c("Unstandardized logistic regression coefficients with robust standard errors in parentheses. Significance levels based on two-tailed tests."),
          covariate.labels=c("RCV", "White", "Age", "Male", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Political Interest", "Mobilization", "City Satisfaction"))

stargazer(log.fair, log.praise, log.int, olog.info, reg.cand,
          title="Null and Negative Findings", align=TRUE,  no.space=TRUE,
          notes = c("Unstandardized logistic regression coefficients with robust standard errors in parentheses. Unstandardized ordered logistic regression coefficients with robust standard errors in parentheses. Significance levels based on two-tailed tests."),
          dep.var.labels=c("Fairness", "Candidate Praise", "Interesting Election", "Useful Info.", "Candidate Choice Satisfaction"),
          covariate.labels=c("RCV", "White", "Age", "Male", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Political Interest", "Mayoral Election", "Mobilization"))

stargazer(olog.crit.base, olog.crit.step1,olog.crit.step2, olog.crit.final, 
          title="Voter Perceptions of Candidates Criticizing: Ordered Logit Estimates", align=TRUE,
          dep.var.labels=c("1 = not at all, 4 = a great deal of the time"),
          ord.intercepts=TRUE,  no.space=TRUE,
          keep.stat=c("ll","n","aic"),
          notes = c("Unstandardized ordered logistic regression coefficients with robust standard errors in parentheses. Significance levels based on two-tailed tests."),
          covariate.labels=c("RCV", "White", "Age", "Male", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Political Interest", "Mobilization", "City Satisfaction"))

stargazer(log.crit.base, log.crit.step1, log.crit.step2, log.crit.final,
          title="Voter Perceptions of Candidates Criticizing: Logit Estimates", align=TRUE,
          dep.var.labels=c("1 = criticizing, 0 = other"),  no.space=TRUE,
          notes = c("Unstandardized logistic regression coefficients with robust standard errors in parentheses. Significance levels based on two-tailed tests."),
          covariate.labels=c("RCV", "White", "Age", "Male", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Political Interest", "Mobilization", "City Satisfaction"))

stargazer(olog.sat.base, olog.sat.step1, olog.sat.step2, olog.sat.final, 
          title="Voter Satisfaction with Overall Conduct of Local Campaigns: Ordered Logit Estimates", align=TRUE,
          dep.var.labels=c("Voter Satisfaction, 1 = not at all, 4 = very"),
          notes = c("Unstandardized ordered logistic regression coefficients with robust standard errors in parentheses. Significance levels based on two-tailed tests."),
          ord.intercepts=TRUE,  no.space=TRUE,
          covariate.labels=c("RCV", "White", "Age", "Male", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Political Interest", "Mobilization", "City Satisfaction"))

stargazer(log.sat.base, log.sat.step1, log.sat.step2, log.sat.final, 
          title="Voter Satisfaction with Overall Conduct of Local Campaigns: Logit Estimates", align=TRUE,
          dep.var.labels=c("Voter Satisfaction, 1 = satisfied, 0 = other"),  no.space=TRUE,
          notes = c("Unstandardized logistic regression coefficients with robust standard errors in parentheses. Significance levels based on two-tailed tests."),
          covariate.labels=c("RCV", "White", "Age", "Male", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Political Interest", "Mobilization", "City Satisfaction"))




stargazer(log.pref.base, log.pref.step1, log.pref.step2, log.pref.final, 
          title="Logistic Regression of Preference for RCV System", align=TRUE,
          dep.var.labels=c("Use RCV In Other Cities"),  no.space=TRUE,
          notes = c("Unstandardized logistic regression coefficients with robust standard errors in parentheses. Significance levels based on two-tailed tests."),
          covariate.labels=c("RCV", "White", "Age", "Male", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Political Interest", "Mobilization", "City Satisfaction"))


# tables using robust standard errors

apsrtable(olog.pos.base, olog.pos.step1, olog.pos.step2, olog.pos.final,
  se=c("robust"),align=c("center"),
  digits=2, coef.names=c("Intercept","RCV","White","Age","Male",
                         "Education","Employed","Married","Democrat",
                         "Republican","Electoral Winner","Political Interest",
                         "Mobilization","City Satisfaction"),
  caption=c("Voter Perceptions of Campaign Negativity: Ordered Logit Estimates"),
  notes=list("Unstandardized ordered logistic regression coefficients with robust standard errors in parantheses.", "Significance levels based on two-tailed tests.", stars.note)
  )

apsrtable(log.pos.base, log.pos.step1, log.pos.step2, log.pos.final,
          se=c("robust"),align=c("center"),
          digits=2, coef.names=c("Intercept","RCV","White","Age","Male",
                                 "Education","Employed","Married","Democrat",
                                 "Republican","Electoral Winner","Political Interest",
                                 "Mobilization","City Satisfaction"),
          caption=c("Voter Perceptions of Campaign Negativity: Logit Estimates"),
          notes=list("Unstandardized logistic regression coefficients with robust standard errors in parantheses.", "Significance levels based on two-tailed tests.", stars.note)
)

apsrtable(olog.crit.base, olog.crit.step1, olog.crit.step2, olog.crit.final,
          se=c("robust"),align=c("center"),
          digits=2, coef.names=c("Intercept","RCV","White","Age","Male",
                                 "Education","Employed","Married","Democrat",
                                 "Republican","Electoral Winner","Political Interest",
                                 "Mobilization","City Satisfaction"),
          caption=c("Voter Perceptions of Candidates Criticizing: Ordered Logit Estimates"),
          notes=list("Unstandardized ordered logistic regression coefficients with robust standard errors in parantheses.", "Significance levels based on two-tailed tests.", stars.note)
)

apsrtable(log.crit.base, log.crit.step1, log.crit.step2, log.crit.final,
          se=c("robust"),align=c("center"),
          digits=2, coef.names=c("Intercept","RCV","White","Age","Male",
                                 "Education","Employed","Married","Democrat",
                                 "Republican","Electoral Winner","Political Interest",
                                 "Mobilization","City Satisfaction"),
          caption=c("Voter Perceptions of Candidates Criticizing: Logit Estimates"),
          notes=list("Unstandardized logistic regression coefficients with robust standard errors in parantheses.", "Significance levels based on two-tailed tests.", stars.note)
)

apsrtable(olog.sat.base, olog.sat.step1, olog.sat.step2, olog.sat.final,
          se=c("robust"),align=c("center"),
          digits=2, coef.names=c("Intercept","RCV","White","Age","Male",
                                 "Education","Employed","Married","Democrat",
                                 "Republican","Electoral Winner","Political Interest",
                                 "Mobilization","City Satisfaction"),
          caption=c("Voter Satisfaction with Overall Conduct of Local Campaigns: Ordered Logit Estimates"),
          notes=list("Unstandardized ordered logistic regression coefficients with robust standard errors in parantheses.", "Significance levels based on two-tailed tests.", stars.note)
)

apsrtable(log.sat.base, log.sat.step1, log.sat.step2, log.sat.final,
          se=c("robust"),align=c("center"),
          digits=2, coef.names=c("Intercept","RCV","White","Age","Male",
                                 "Education","Employed","Married","Democrat",
                                 "Republican","Electoral Winner","Political Interest",
                                 "Mobilization","City Satisfaction"),
          caption=c("Voter Satisfaction with Overall Conduct of Local Campaigns: Logit Estimates"),
          notes=list("Unstandardized logistic regression coefficients with robust standard errors in parantheses.", "Significance levels based on two-tailed tests.", stars.note)
)

################
# Jack-knifing #
################

data.nolowell <- mydata[mydata$xcity!="LOWELL" , ]
data.noworcester <- mydata[mydata$xcity!="WORCESTER" , ]
data.noboston <- mydata[mydata$xcity!="BOSTON" , ]
data.noseattle <- mydata[mydata$xcity!="SEATTLE" , ]
data.nodesmoines <- mydata[mydata$xcity!="DES MOINES" , ]
data.notulsa <- mydata[mydata$xcity!="TULSA" , ]
data.nocedarrapids <- mydata[mydata$xcity!="CEDAR RAPIDS" , ]


# ordered logit positive campaigning
olog.nolowell <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + 
                        newedu + employed + married + democrat + republican + 
                        electoral_winner + polint + mobilization + city_satisfaction, 
                      data=data.nolowell, Hess = TRUE)

olog.noworc <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + 
                      newedu + employed + married + democrat + republican + 
                      electoral_winner + polint + mobilization + city_satisfaction, 
                    data=data.noworcester, Hess = TRUE)

olog.noboston <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + 
                        newedu + employed + married + democrat + republican + 
                        electoral_winner + polint + mobilization + city_satisfaction, 
                      data=data.noboston, Hess = TRUE)

olog.noseattle <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + 
                         newedu + employed + married + democrat + republican + 
                         electoral_winner + polint + mobilization + city_satisfaction, 
                       data=data.noseattle, Hess = TRUE)

olog.nodesmoines <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + 
                           newedu + employed + married + democrat + republican + 
                           electoral_winner + polint + mobilization + city_satisfaction, 
                         data=data.nodesmoines, Hess = TRUE)

olog.notulsa <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + 
                       newedu + employed + married + democrat + republican + 
                       electoral_winner + polint + mobilization + city_satisfaction, 
                     data=data.notulsa, Hess = TRUE)

olog.nocedarrapids <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + 
                             newedu + employed + married + democrat + republican + 
                             electoral_winner + polint + mobilization + city_satisfaction, 
                           data=data.nocedarrapids, Hess = TRUE)

pr <- profile(olog.nocedarrapids)
plot(pr)
pairs(pr)

require(stargazer)
stargazer(olog.nolowell, olog.noworc, olog.noboston, 
          olog.noseattle, olog.nodesmoines, 
          olog.notulsa, olog.nocedarrapids,
          title="Voter Perceptions of Campaign Negativity: Ordered Logit Estimates",
          align=TRUE, no.space=TRUE,
          covariate.labels=c("RCV","White","Age","Male",
                             "Education","Employed","Married",
                             "Democrat","Republican","Electoral Winner",
                             "Political Interest","Mobilization","City Satisfaction"),
          notes = c("Unstandardized ordered logistic regression coefficients with robust standard errors in parentheses.","Significance levels based on two-tailed tests"))


# CEM
cem.dat <- mydata[c(87,131,99,100,130,101,102,97,98,119,132,109,90,134)]
cem.data <- cem.dat[!is.na(cem.dat$logit_positive.n),]
Le <-data.frame(na.omit(cem.data))

require(cem)
vars <- c("age","newedu","newwhite","male","employed","democrat","republican")
imbalance(group=Le$rcv_voting, data=Le[vars])

mat <- cem(treatment = "rcv_voting", data=Le)

q1.grp <- list(c("CAMBRIDGE","LOWELL","WORCESTER"), c("MINNEAPOLIS","BOSTON","SEATTLE","TULSA"), c("ST. PAUL","CEDAR RAPIDS","DES MOINES"))

mat1 <- cem(treatment = "rcv_voting", data = Le, drop = "logit_positive.n",
            keep.all=TRUE, grouping = q1.grp)

# automated progressive coarsening
tab <- relax.cem(mat1, mydata, depth=3, perc=0.3)
est <- att (mat1, logit_positive.n ~ rcv_voting + newwhite + age + male  + 
              newedu + employed + married + democrat + republican + 
              electoral_winner + polint + mobilization + city_satisfaction, 
            data=Le, model="logit")

# CEM with only demographics:
require(cem)
cem.dat.dem <- mydata[c(87,131,99,100,130,101,102,97,98,132,134)]
cem.data.dem <- cem.dat.dem[!is.na(cem.dat.dem$logit_positive.n),]
cemdem <-data.frame(na.omit(cem.data.dem))
vars <- c("age","newedu","newwhite","male","employed","democrat","republican")
imbalance(group=cemdem$rcv_voting, data=cemdem[vars])

q1.grp <- list(c("CAMBRIDGE","LOWELL","WORCESTER"), c("MINNEAPOLIS","BOSTON","SEATTLE","TULSA"), 
               c("ST. PAUL","CEDAR RAPIDS","DES MOINES"))

mat.dem <- cem(treatment = "rcv_voting", data = cemdem, drop = "logit_positive.n",
            keep.all=TRUE, grouping = q1.grp)
est <- att (mat.dem, logit_positive.n ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + polint, data=cemdem, model="logit")

###
# 1) .pdf of G. King document on CEM
# 2) code w/ results (imbalance and SATT)
# 
