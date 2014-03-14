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
log.pos.base <- glm(logit_positive ~ rcv_voting + newwhite + age + male + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
log.pos.step1 <- glm(logit_positive ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, family="binomial")
log.pos.step2 <- glm(logit_positive ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
log.pos.final <- glm(logit_positive ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, family="binomial")

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
mydata$ocrit.f <- factor(mydata$ocrit, labels=c("A great deal of time", "Some of the time", "Don't know", "Not too much", "Not at all"))

# logit
log.crit.base <- glm(no_criticize ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
log.crit.step1 <- glm(no_criticize ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, family="binomial")
log.crit.step2 <- glm(no_criticize ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
log.crit.final <- glm(no_criticize ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, family="binomial")

# ordered logit
olog.crit.base <- polr(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, Hess = TRUE)
olog.crit.step1 <- polr(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, Hess = TRUE)
olog.crit.step2 <- polr(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, Hess = TRUE)
olog.crit.final <- polr(ocrit.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, Hess = TRUE)

#########################
# Campaign Satisfaction #
#########################

mydata$campaign_satisfaction.f <- factor(mydata$campaign_satisfaction, labels=c("Not at all satisfied", "Not very satisfied", "Don't know", "Fairly satisfied", "Very satisfied"))
mydata$logit_satisfied[mydata$campaign_satisfaction==1] <- 0
mydata$logit_satisfied[mydata$campaign_satisfaction==2] <- 0
mydata$logit_satisfied[mydata$campaign_satisfaction==3] <- 0
mydata$logit_satisfied[mydata$campaign_satisfaction==4] <- 1
mydata$logit_satisfied[mydata$campaign_satisfaction==5] <- 1

# logit
log.sat.base <- glm(logit_satisfied ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, family="binomial")
log.sat.step1 <- glm(logit_satisfied ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, family="binomial")
log.sat.step2 <- glm(logit_satisfied ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
log.sat.final <- glm(logit_satisfied ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, family="binomial")

# ordered logit
olog.sat.base <- polr(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner, data=mydata, Hess = TRUE)
olog.sat.step1 <- polr(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint, data=mydata, Hess = TRUE)
olog.sat.step2 <- polr(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, Hess = TRUE)
olog.sat.final <- polr(campaign_satisfaction.f ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization + city_satisfaction, data=mydata, Hess = TRUE)

#################
# Null Findings #
#################

mydata$useful_information.f <- factor(mydata$useful_information, labels=c("No useful information", "Not too much information", "Don't know", "Some information", "A great deal of useful information"))
mydata$candidate_satisfaction.f <- factor(mydata$candidate_satisfaction, labels=c("Not at all satisfied", "Not very satisfied", "Don't know", "Fairly satisfied", "Very satisfied"))

# logit
log.fair <- glm(fairness ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")

log.praise <- glm(praise ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")
log.int <- glm(interesting ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + mobilization, data=mydata, family="binomial")

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

#############################
# Creating Frequency Tables #
#############################

q34a.table <- table(mydata$q34a)
q34b.table <- table(mydata$q34b)
olog.pos.table <- table(mydata$positive_campaigning)
log.pos.table <- table(mydata$logit_positive)

q35.table <- table(mydata$q35)
fair.table<- table(mydata$fairness)

q31.table <- table(mydata$q31)
praise.table <- table(mydata$praise)

q28.table<-table(mydata$q28)
int.table<-table(mydata$interesting)

q30.table<-table(mydata$q30)
olog.useful.table <- table(mydata$useful_information)

q16.table <- table(mydata$q16)
q18.table <- table(mydata$q18)
recodemayor.table <- table(mydata$recodemayor)
recodecouncil.table <- table(mydata$recodecouncil)
satall.table <- table(mydata$satall)

q32.table <- table(mydata$q32)
no_criticize.table <- table(mydata$no_criticize)
ocrit.table <- table(mydata$ocrit)

q29.table <- table(mydata$q29)
log.sat.table <- table(mydata$logit_satisfied)
olog.sat.table <- table(mydata$campaign_satisfaction)

q37.table <- table(mydata$q37)
rcvpref.table <- table(mydata$rcv_pref)

qd3.table <- table(mydata$qd3)
newwhite.table <- table(mydata$newwhite)

male.table <- table(mydata$male)

qd2.table <- table(mydata$qd2)
newedu.table <- table(mydata$newedu)

qd6.table <- table(mydata$qd6)
employed.table <- table(mydata$employed)

qd5.table <- table(mydata$qd5)
married.table <- table(mydata$married)

qd1.table <- table(mydata$qd1)
democrat.table <- table(mydata$democrat)
republican.table <- table(mydata$republican)

q17.table <- table(mydata$q17)
q19.table <- table(mydata$q19)
electoralwinner.table <- table(mydata$electoral_winner)

q26.table <- table(mydata$q26)
polint.table <- table(mydata$polint)

q22.table <- table(mydata$q22)
mobilization.table <- table(mydata$mobilization)

q27.table <- table(mydata$q27)
citysatisfaction.table <- table(mydata$city_satisfaction)

write.table(q34a.table, file="C:/Users/kellengracey/Desktop/testfolder/testfile.txt", sep ="\t")
write.table(q34b.table, file="C:/Users/kellengracey/Desktop/testfolder/q34b.txt", sep ="\t")
write.table(olog.pos.table, file="C:/Users/kellengracey/Desktop/testfolder/ologpos.txt", sep ="\t")
write.table(log.pos.table, file="C:/Users/kellengracey/Desktop/testfolder/logpos.txt", sep ="\t")
write.table(q35.table, file="C:/Users/kellengracey/Desktop/testfolder/q35.txt", sep ="\t")
write.table(fair.table, file="C:/Users/kellengracey/Desktop/testfolder/fair.txt", sep ="\t")
write.table(q31.table, file="C:/Users/kellengracey/Desktop/testfolder/q31.txt", sep ="\t")
write.table(praise.table, file="C:/Users/kellengracey/Desktop/testfolder/praise.txt", sep ="\t")
write.table(q28.table, file="C:/Users/kellengracey/Desktop/testfolder/q28.txt", sep ="\t")
write.table(int.table, file="C:/Users/kellengracey/Desktop/testfolder/interesting.txt", sep ="\t")
write.table(q30.table, file="C:/Users/kellengracey/Desktop/testfolder/q30.txt", sep ="\t")
write.table(olog.useful.table, file="C:/Users/kellengracey/Desktop/testfolder/usefulinfo.txt", sep ="\t")
write.table(q16.table, file="C:/Users/kellengracey/Desktop/testfolder/q16.txt", sep ="\t")
write.table(q18.table, file="C:/Users/kellengracey/Desktop/testfolder/q18.txt", sep ="\t")
write.table(recodemayor.table, file="C:/Users/kellengracey/Desktop/testfolder/recodemayor.txt", sep ="\t")
write.table(recodecouncil.table, file="C:/Users/kellengracey/Desktop/testfolder/recodecouncil.txt", sep ="\t")
write.table(satall.table, file="C:/Users/kellengracey/Desktop/testfolder/satall.txt", sep ="\t")
write.table(q32.table, file="C:/Users/kellengracey/Desktop/testfolder/q32.txt", sep ="\t")
write.table(no_criticize.table, file="C:/Users/kellengracey/Desktop/testfolder/nocriticize.txt", sep ="\t")
write.table(ocrit.table, file="C:/Users/kellengracey/Desktop/testfolder/ocrit.txt", sep ="\t")
write.table(q29.table, file="C:/Users/kellengracey/Desktop/testfolder/q29.txt", sep ="\t")
write.table(log.sat.table, file="C:/Users/kellengracey/Desktop/testfolder/logsat.txt", sep ="\t")
write.table(olog.sat.table, file="C:/Users/kellengracey/Desktop/testfolder/ologsat.txt", sep ="\t")
write.table(q37.table, file="C:/Users/kellengracey/Desktop/testfolder/q37.txt", sep ="\t")
write.table(rcvpref.table, file="C:/Users/kellengracey/Desktop/testfolder/rcvpref.txt", sep ="\t")
write.table(qd3.table, file="C:/Users/kellengracey/Desktop/testfolder/qd3.txt", sep ="\t")
write.table(newwhite.table, file="C:/Users/kellengracey/Desktop/testfolder/newwhite.txt", sep ="\t")
write.table(male.table, file="C:/Users/kellengracey/Desktop/testfolder/male.txt", sep ="\t")
write.table(qd2.table, file="C:/Users/kellengracey/Desktop/testfolder/qd2.txt", sep ="\t")
write.table(newedu.table, file="C:/Users/kellengracey/Desktop/testfolder/newedu.txt", sep ="\t")
write.table(qd6.table, file="C:/Users/kellengracey/Desktop/testfolder/qd6.txt", sep ="\t")
write.table(employed.table, file="C:/Users/kellengracey/Desktop/testfolder/employed.txt", sep ="\t")
write.table(qd5.table, file="C:/Users/kellengracey/Desktop/testfolder/qd5.txt", sep ="\t")
write.table(married.table, file="C:/Users/kellengracey/Desktop/testfolder/married.txt", sep ="\t")
write.table(qd1.table, file="C:/Users/kellengracey/Desktop/testfolder/qd1.txt", sep ="\t")
write.table(democrat.table, file="C:/Users/kellengracey/Desktop/testfolder/democrat.txt", sep ="\t")
write.table(republican.table, file="C:/Users/kellengracey/Desktop/testfolder/republican.txt", sep ="\t")
write.table(q17.table, file="C:/Users/kellengracey/Desktop/testfolder/q17.txt", sep ="\t")
write.table(q19.table, file="C:/Users/kellengracey/Desktop/testfolder/q19.txt", sep ="\t")
write.table(electoralwinner.table, file="C:/Users/kellengracey/Desktop/testfolder/electoralwinner.txt", sep ="\t")
write.table(q26.table, file="C:/Users/kellengracey/Desktop/testfolder/q26.txt", sep ="\t")
write.table(polint.table, file="C:/Users/kellengracey/Desktop/testfolder/polint.txt", sep ="\t")
write.table(q22.table, file="C:/Users/kellengracey/Desktop/testfolder/q22.txt", sep ="\t")
write.table(mobilization.table, file="C:/Users/kellengracey/Desktop/testfolder/mobilization.txt", sep ="\t")
write.table(q27.table, file="C:/Users/kellengracey/Desktop/testfolder/q27.txt", sep ="\t")
write.table(citysatisfaction.table, file="C:/Users/kellengracey/Desktop/testfolder/citysatisfaction.txt", sep ="\t")


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
