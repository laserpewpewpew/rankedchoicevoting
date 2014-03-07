###################################
# Kellen Gracey                   #
# Department of Political Science #
# University of Iowa              #
# Ranked Choice Voting Results    #
###################################


canddata <- read.csv("candidatercv.csv")

# Housekeeping recodes

canddata$white[canddata$C4race==0] <- 0
canddata$white[canddata$C4race==1] <- 1
canddata$white[canddata$C4race==2] <- 0
canddata$white[canddata$C4race==3] <- 0
canddata$white[canddata$C4race==4] <- 0
canddata$white[canddata$C4race==5] <- 0
canddata$white[canddata$C4race==6] <- 0
canddata$white[canddata$C4race==NA] <- 0


canddata$employed[canddata$C1emp==1] <- 1
canddata$employed[canddata$C1emp==2] <- 1
canddata$employed[canddata$C1emp==3] <- 0
canddata$employed[canddata$C1emp==4] <- 0
canddata$employed[canddata$C1emp==5] <- 0
canddata$employed[canddata$C1emp==6] <- 0
canddata$employed[canddata$C1emp==7] <- 0
canddata$employed[canddata$C1emp==NA] <- 0

canddata$female[canddata$C6female==1] <- 1
canddata$female[canddata$C6female==0] <- 0

canddata$newage <- as.numeric(as.character(canddata$age))
canddata$spending <- as.numeric(as.character(canddata$A10totspend))

canddata$educ[canddata$C3educ==1] <- 1
canddata$educ[canddata$C3educ==2] <- 2
canddata$educ[canddata$C3educ==3] <- 3
canddata$educ[canddata$C3educ==4] <- 4
canddata$educ[canddata$C3educ==5] <- 5
canddata$educ[canddata$C3educ==6] <- 6

canddata$lessnegative[canddata$B6negcamps==1] <- 1
canddata$lessnegative[canddata$B6negcamps==2] <- 2
canddata$lessnegative[canddata$B6negcamps==3] <- 3
canddata$lessnegative[canddata$B6negcamps==4] <- 4
canddata$lessnegative[canddata$B6negcamps==5] <- 5
canddata$lessnegative[canddata$B6negcamps==NA] <- 3

canddata$fairness[canddata$B10fair==1] <- 1
canddata$fairness[canddata$B10fair==0] <- 0
canddata$fairness[canddata$B10fair==NA] <- 0

canddata$lessnegative.f <- factor(canddata$lessnegative, labels=c("A lot more negative", "A little more negative", "About the same", "A little less negative", "A lot less negative"))


#################
# Less Negative #
#################

olog.pos <- polr(lessnegative.f ~ rcv + female + newage + white + educ + employed + fairness , data=canddata)
summary(olog.pos)

############
# Fairness #
############

log.fair <- glm(fairness ~ rcv + female + newage + white + educ + employed + spending, data=canddata, family="binomial")
summary(log.fair)