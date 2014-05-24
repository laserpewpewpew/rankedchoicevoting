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

mydata$polint[mydata$q26=="Hardly at all?"] <- 1
mydata$polint[mydata$q26=="Only now and then, or"] <- 2
mydata$polint[mydata$q26=="Some of the time"] <- 3
mydata$polint[mydata$q26=="Most of the time"] <- 4

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

# ordered logit
olog.pos.base <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, Hess = TRUE)
olog.pos.step1 <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, Hess = TRUE)
olog.pos.step2 <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, Hess = TRUE)
olog.pos.final <- polr(positive_campaigning.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, Hess = TRUE)

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

# ordered logit
olog.crit.base <- polr(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, Hess = TRUE)
olog.crit.step1 <- polr(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, Hess = TRUE)
olog.crit.step2 <- polr(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, Hess = TRUE)
olog.crit.final <- polr(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, Hess = TRUE)

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

# ordered logit
olog.sat.base <- polr(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, Hess = TRUE)
olog.sat.step1 <- polr(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, Hess = TRUE)
olog.sat.step2 <- polr(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, Hess = TRUE)
olog.sat.final <- polr(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, Hess = TRUE)

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
          title="Ordered Logit of Positive Campaigning", align=TRUE,
          dep.var.labels=c("Ordinal Less Negative Campaigning"),
          ord.intercepts=TRUE, no.space=TRUE,
          notes = c("Unstandardized ordered logistic regression coefficients with robust standard errors in parentheses. Significance levels based on two-tailed tests."),
          covariate.labels=c("RCV", "White", "Age", "Male", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Political Interest", "Mobilization", "City Satisfaction"))

stargazer(log.pos.base, log.pos.step1, log.pos.step2, log.pos.final,
          title="Logistic Regression of Positive Campaigning", align=TRUE,
          dep.var.labels=c("Dichotomous Less Negative Campaigning"),  no.space=TRUE,
          notes = c("Unstandardized logistic regression coefficients with robust standard errors in parentheses. Significance levels based on two-tailed tests."),
          covariate.labels=c("RCV", "White", "Age", "Male", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Political Interest", "Mobilization", "City Satisfaction"))

stargazer(log.fair, log.praise, log.int, olog.info, reg.cand,
          title="Null and Negative Findings", align=TRUE,  no.space=TRUE,
          notes = c("Unstandardized logistic regression coefficients with robust standard errors in parentheses. Unstandardized ordered logistic regression coefficients with robust standard errors in parentheses. Significance levels based on two-tailed tests."),
          dep.var.labels=c("Fairness", "Candidate Praise", "Interesting Election", "Useful Info.", "Candidate Choice Satisfaction"),
          covariate.labels=c("RCV", "White", "Age", "Male", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Political Interest", "Mayoral Election", "Mobilization"))

stargazer(log.crit.base, log.crit.step1, log.crit.step2, log.crit.final,
          title="Logistic Regression of Less Criticism Between Candidates", align=TRUE,
          dep.var.labels=c("Dichotomous Less Criticism"),  no.space=TRUE,
          notes = c("Unstandardized logistic regression coefficients with robust standard errors in parentheses. Significance levels based on two-tailed tests."),
          covariate.labels=c("RCV", "White", "Age", "Male", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Political Interest", "Mobilization", "City Satisfaction"))

stargazer(olog.crit.base, olog.crit.step1,olog.crit.step2, olog.crit.final, 
          title="Ordered Logit of Less Criticism Between Candidates", align=TRUE,
          dep.var.labels=c("Ordinal Less Criticism"),
          ord.intercepts=TRUE,  no.space=TRUE,
          notes = c("Unstandardized ordered logistic regression coefficients with robust standard errors in parentheses. Significance levels based on two-tailed tests."),
          covariate.labels=c("RCV", "White", "Age", "Male", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Political Interest", "Mobilization", "City Satisfaction"))

stargazer(log.sat.base, log.sat.step1, log.sat.step2, log.sat.final, 
          title="Logistic Regression of Satisfaction with Conduct of Campaigns", align=TRUE,
          dep.var.labels=c("Dichotomous Satisfaction"),  no.space=TRUE,
          notes = c("Unstandardized logistic regression coefficients with robust standard errors in parentheses. Significance levels based on two-tailed tests."),
          covariate.labels=c("RCV", "White", "Age", "Male", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Political Interest", "Mobilization", "City Satisfaction"))

stargazer(olog.sat.base, olog.sat.step1, olog.sat.step2, olog.sat.final, 
          title="Ordered Logit of Satisfaction with Conduct of Campaigns", align=TRUE,
          dep.var.labels=c("Ordinal Satisfaction"),
          notes = c("Unstandardized ordered logistic regression coefficients with robust standard errors in parentheses. Significance levels based on two-tailed tests."),
          ord.intercepts=TRUE,  no.space=TRUE,
          covariate.labels=c("RCV", "White", "Age", "Male", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Political Interest", "Mobilization", "City Satisfaction"))

stargazer(log.pref.base, log.pref.step1, log.pref.step2, log.pref.final, 
          title="Logistic Regression of Preference for RCV System", align=TRUE,
          dep.var.labels=c("Use RCV In Other Cities"),  no.space=TRUE,
          notes = c("Unstandardized logistic regression coefficients with robust standard errors in parentheses. Significance levels based on two-tailed tests."),
          covariate.labels=c("RCV", "White", "Age", "Male", "Education", "Employed", "Married", "Democrat", "Republican", "Electoral Winner", "Political Interest", "Mobilization", "City Satisfaction"))

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

require(stargazer)
stargazer(olog.nolowell, olog.noworc, olog.noboston, 
          olog.noseattle, olog.nodesmoines, 
          olog.notulsa, olog.nocedarrapids,
          title="Voter Perceptions of Campaign Negativity: Ordered Logit Estimates",
          align=TRUE, no.space=TRUE,
          covariate.labels=c("RCV","White","Age","Male",
                             "Education","Employed","Maried",
                             "Democrat","Republican","Electoral Winner",
                             "Political Interest","Mobilization","City Satisfaction"),
          notes = c("Unstandardized ordered logistic regression coefficients with robust standard errors in parentheses.","Significance levels based on two-tailed tests"))


# CEM
cem.dat <- mydata[c(87,108,130,131,99,100,101,102,97,98,119,132,109,90)]
cem.data <- cem.dat[!is.na(cem.dat$positive_campaigning.f),]

require(cem)
vars <- c("newwhite","age","male","newedu","employed",
          "married","democrat","republican","electoral_winner",
          "polint","mobilization","city_satisfaction")
imbalance(group=mydata$rcv_voting, data=mydata[vars])

mat <- cem(treatment = "rcv_voting", data=mydata, drop="positive_campaigning")

# automated progressive coarsening
tab <- relax.cem(mat, cem.data, depth=1, perc=0.3)
est <- att(mat, positive_campaigning ~ rcv_voting, data=mydata)

