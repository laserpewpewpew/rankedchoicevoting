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
stargazer(log.pos.base, log.pos.step1, log.pos.step2, log.pos.final, olog.pos.base, olog.pos.step1,
          olog.pos.step2, olog.pos.final, title="Positive Campaigning", no.space=TRUE,
          dep.var.labels=c("Dichotomous Less Negative Campaigning", "Ordinal Less Negative Campaigning"),
          covariate.labels=c("RCV", "White", "Age", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Interesting", "Mobilization", "City Satisfaction"))

stargazer(fit.final, nbfit.final, olr.final, title="Robust Results Across Dependent Variable Treatment", align=TRUE, 
          dep.var.labels=c("Political Participation", "Participation Factor"),
          covariate.labels=c("Religious Motivation", "Mostly Religious Cmty.", "Age", "Political Knowledge", "Political Interest", "Married", "Income", "Education", "Religious Motivation * Mostly Rel. Cmty."),
          omit.stat=c("LL","ser","f", "theta"),
          no.space=FALSE
)
##################
# RCV Preference #
##################

# logit



# ordered logit

##################
# Less Criticism #
##################

# logit



# ordered logit

#########################
# Campaign Satisfaction #
#########################

# logit



# ordered logit


#################
# Null Findings #
#################

# logit



# ordered logit