###################################
# Kellen Gracey                   #
# Department of Political Science #
# University of Iowa              #
# Ranked Choice Voting Results    #
###################################


canddata <- read.csv("candidatercv.csv")

# Housekeeping recodes

canddata$white[canddata$C4race==1] <- 1
canddata$white[canddata$C4race>=1] <- 0

canddata$employed[canddata$C1emp==1] <- 1
canddata$employed[canddata$C1emp==2] <- 1
canddata$employed[canddata$C1emp>=3] <- 0

canddata$female[canddata$C6female==1] <- 1
canddata$female[canddata$C6female==0] <- 0
canddata$female[canddata$C6female==NA] <- 0

canddata$lessnegative[canddata$B6negcamps==1] <- 1
canddata$lessnegative[canddata$B6negcamps==2] <- 2
canddata$lessnegative[canddata$B6negcamps==3] <- 3
canddata$lessnegative[canddata$B6negcamps==4] <- 4
canddata$lessnegative[canddata$B6negcamps==5] <- 5
canddata$lessnegative[canddata$B6negcamps==NA] <- 3

canddata$lessnegative.f <- factor(canddata$lessnegative, labels=c("A lot more negative", "A little more negative", "About the same", "A little less negative", "A lot less negative"))

olog.pos <- polr(lessnegative.f ~ rcv + C6female + age + white + C3educ + employed + B10fair, data=canddata, Hess=TRUE)