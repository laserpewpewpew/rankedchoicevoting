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

# housekeeping recodes
mydata$newedu[mydata$education=="incomplete"] <- 1
mydata$newedu[mydata$education=="highschool"] <- 2
mydata$newedu[mydata$education=="somecollege"] <- 3
mydata$newedu[mydata$education=="collegegraduate"] <- 4
mydata$newedu[mydata$education=="post-graduate"] <- 5

mydata$newwhite[mydata$white=="non-white"] <- 0
mydata$newwhite[mydata$white=="white"] <- 1

########################
# Positive Campaigning #
########################

# create factor variable for ologit
mydata$less_negative[mydata$q34b==1] <- 5 
mydata$less_negative[mydata$q34b==2] <- 4
mydata$less_negative[mydata$q34b>=3] <- 0 
mydata$less_negative[mydata$q34b==NA] <- 0

mydata$more_negative[mydata$q34a==NA] <- 0
mydata$more_negative[mydata$q34a==1] <- 1
mydata$more_negative[mydata$q34a==2] <- 2
mydata$more_negative[mydata$q34a>=3] <- 0

mydata$positive_campaigning <- mydata$less_negative + mydata$more_negative
mydata$positive_campaigning[mydata$positive_campaigning==0] <- 3

mydata$positive_campaigning.f <- factor(mydata$positive_campaigning, labels=c("A lot more negative", "A little more negative", "About the same", "A little less negative", "A lot less negative"))
mydata$logit_positive[mydata$positive_campaigning==1] <- 0
mydata$logit_positive[mydata$positive_campaigning==2] <- 0
mydata$logit_positive[mydata$positive_campaigning==3] <- 0
mydata$logit_positive[mydata$positive_campaigning==4] <- 1
mydata$logit_positive[mydata$positive_campaigning==5] <- 1

# logit
log.pos.base <- glm(logit_positive ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
log.pos.step1 <- glm(logit_positive ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting, data=mydata, family="binomial")
log.pos.step2 <- glm(logit_positive ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting + mobilization, data=mydata, family="binomial")
log.pos.final <- glm(logit_positive ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting + mobilization + city_satisfaction, data=mydata, family="binomial")

# ordered logit
olog.pos.base <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, Hess = TRUE)
olog.pos.step1 <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting, data=mydata, Hess = TRUE)
olog.pos.step2 <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting + mobilization, data=mydata, Hess = TRUE)
olog.pos.final <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting + mobilization + city_satisfaction, data=mydata, Hess = TRUE)

library(stargazer)
stargazer(log.pos.base, log.pos.step1, log.pos.step2, log.pos.final, olog.pos.base, olog.pos.step1,olog.pos.step2, olog.pos.final,
          title="Positive Campaigning", no.space=TRUE,
          dep.var.labels=c("Dichotomous Less Negative Campaigning", "Ordinal Less Negative Campaigning"),
          covariate.labels=c("RCV", "White", "Age", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Interesting", "Mobilization", "City Satisfaction"))

##################
# RCV Preference #
##################

# logit
log.pref.base <- glm(rcv_pref ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
log.pref.step1 <- glm(rcv_pref ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting, data=mydata, family="binomial")
log.pref.step2 <- glm(rcv_pref ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting + mobilization, data=mydata, family="binomial")
log.pref.final <- glm(rcv_pref ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting + mobilization + city_satisfaction, data=mydata, family="binomial")

library(stargazer)
stargazer(log.pref.base, log.pref.step1, log.pref.step2, log.pref.final, title="Logistic Regression of Preference for RCV System", 
          no.space=TRUE,
          dep.var.labels=c("Use RCV In Other Cities"),
          covariate.labels=c("RCV", "White", "Age", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Interesting", "Mobilization", "City Satisfaction"))


##################
# Less Criticism #
##################
mydata$ocrit.f <- factor(mydata$ocrit, labels=c("A great deal of time", "Some of the time", "Don't know", "Not too much", "Not at all"))

# logit
log.crit.base <- glm(no_criticize ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
log.crit.step1 <- glm(no_criticize ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting, data=mydata, family="binomial")
log.crit.step2 <- glm(no_criticize ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting + mobilization, data=mydata, family="binomial")
log.crit.final <- glm(no_criticize ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting + mobilization + city_satisfaction, data=mydata, family="binomial")

# ordered logit
olog.crit.base <- polr(ocrit.f ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, Hess = TRUE)
olog.crit.step1 <- polr(ocrit.f ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting, data=mydata, Hess = TRUE)
olog.crit.step2 <- polr(ocrit.f ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting + mobilization, data=mydata, Hess = TRUE)
olog.crit.final <- polr(ocrit.f ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting + mobilization + city_satisfaction, data=mydata, Hess = TRUE)

library(stargazer)
stargazer(log.crit.base, log.crit.step1, log.crit.step2, log.crit.final, olog.crit.base, olog.crit.step1,
          olog.crit.step2, olog.crit.final, title="Less Criticism Between Candidates", no.space=TRUE,
          dep.var.labels=c("Dichotomous Less Criticism", "Ordinal Less Criticism"),
          covariate.labels=c("RCV", "White", "Age", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Interesting", "Mobilization", "City Satisfaction"))

#########################
# Campaign Satisfaction #
#########################

mydata$campaign_satisfaction.f <- factor(mydata$campaign_satisfaction, labels=c("Not at all satisfied", "Not very satisfied", "No opinion/Don't know", "Fairly satisfied", "Very satisfied"))
mydata$logit_satisfied[mydata$campaign_satisfaction==1] <- 0
mydata$logit_satisfied[mydata$campaign_satisfaction==2] <- 0
mydata$logit_satisfied[mydata$campaign_satisfaction==3] <- 0
mydata$logit_satisfied[mydata$campaign_satisfaction==4] <- 1
mydata$logit_satisfied[mydata$campaign_satisfaction==5] <- 1

# logit
log.sat.base <- glm(logit_satisfied ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
log.sat.step1 <- glm(logit_satisfied ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting, data=mydata, family="binomial")
log.sat.step2 <- glm(logit_satisfied ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting + mobilization, data=mydata, family="binomial")
log.sat.final <- glm(logit_satisfied ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting + mobilization + city_satisfaction, data=mydata, family="binomial")

# ordered logit
olog.sat.base <- polr(campaign_satisfaction.f ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, Hess = TRUE)
olog.sat.step1 <- polr(campaign_satisfaction.f ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting, data=mydata, Hess = TRUE)
olog.sat.step2 <- polr(campaign_satisfaction.f ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting + mobilization, data=mydata, Hess = TRUE)
olog.sat.final <- polr(campaign_satisfaction.f ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting + mobilization + city_satisfaction, data=mydata, Hess = TRUE)

library(stargazer)
stargazer(log.sat.base, log.sat.step1, log.sat.step2, log.sat.final, olog.sat.base, olog.sat.step1,
          olog.sat.step2, olog.sat.final, title="Satisfaction with Conduct of Campaigns", no.space=TRUE,
          dep.var.labels=c("Dichotomous Satisfaction", "Ordinal Satisfaction"),
          covariate.labels=c("RCV", "White", "Age", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Interesting", "Mobilization", "City Satisfaction"))

#################
# Null Findings #
#################

mydata$useful_information.f <- factor(mydata$useful_information, labels=c("No useful information", "Not too much information", "No opinion/Don't know", "Some information", "A great deal of useful information"))
mydata$candidate_satisfaction.f <- factor(mydata$candidate_satisfaction, labels=c("Not at all satisfied", "Not very satisfied", "No opinion/Don't know", "Fairly satisfied", "Very satisfied"))

# logit
log.fair <- glm(fairness ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
log.praise <- glm(praise ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting, data=mydata, family="binomial")

# ordered logit
olog.info <- polr(useful_information.f ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, Hess = TRUE)
olog.cand <- polr(candidate_satisfaction.f ~ rcv_voting + newwhite + age + newedu + employed + married + democrat + republican + electoral_winner + interesting, data=mydata, Hess = TRUE)

library(stargazer)
stargazer(log.fair, log.praise, olog.info, olog.cand,
          title="Null and Negative Findings", no.space=TRUE,
          dep.var.labels=c("Fairness", "Candidate Praise", "Useful Info.", "Candidate Choice Satisfaction"),
          covariate.labels=c("RCV", "White", "Age", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Interesting", "Mobilization", "City Satisfaction"))
