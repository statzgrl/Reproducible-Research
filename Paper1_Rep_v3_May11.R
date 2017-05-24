############### PAPER 1/5 ###################

##DATA SOURCES##

#NACCHO data obtained from: http://nacchoprofilestudy.org/data-requests/, "2013 Profile Study"
#RUCA data obtained from lead author

##CODE INFORMATION##

#Written by: Sarah Wondmeneh and Yiqiang Zhao#
#Github Link: #

############################################

##CREATE DATASET## 

##Open Packages##

library(foreign)
library(magrittr)
library(PredictABEL)
library(weights)
library(descr)

#import 2013 NACCHO Data

LHD_2013 <- read.spss(file.choose(), use.value.labels = TRUE)
LHD_2013 <- as.data.frame(LHD_2013)

#import RUCA code

paper1data <- read.csv(file.choose(), header = TRUE)
ruca <- data.frame(nacchoid = character(2000))
ruca$ruca_cat <- paper1data$ruca_3_cat_RC
ruca$nacchoid <- paper1data$nacchoid

#merge datasets

LHD_2013_ruca <- merge(ruca, LHD_2013,by="nacchoid")

#select module 1 only
LHD_2013_m1 <- LHD_2013_ruca[LHD_2013_ruca$c0module == "Core + Module 1" & is.na(LHD_2013_ruca$c0module) == F, ]

##INPUT RELEVANT VARIABLES##

#Create data frame#
paper1 <- data.frame(population = character(490))

#weight
paper1$weight01 <- LHD_2013_m1$c0moduleweight1

#Decision to seek PHAB accreditation
#Authors omitted LHDs applying through state
#so recoded this to NA
paper1$PHAB[LHD_2013_m1$m2q401 == "undecided"] <- "not seeking"
paper1$PHAB[LHD_2013_m1$m2q401 == "not applying"] <- "not seeking"
paper1$PHAB[LHD_2013_m1$m2q401 == "plans to apply, no statement"]<-"not seeking"
paper1$PHAB[LHD_2013_m1$m2q401 == "submitted application" |LHD_2013_m1$m2q401 == "submitted statement of intent" ] <- "seeking"
paper1$PHAB[LHD_2013_m1$m2q401 == "SHA will apply for LHD"]<- NA
paper1$PHAB<-as.factor(paper1$PHAB)

#rurality
#CODING FOR Rurality: 1 = rural, 2 = micropolitan, 3 = urban

paper1$rurality<-LHD_2013_m1$ruca_cat
paper1$rurality<-as.factor(paper1$rurality)
paper1$rurality <- relevel(paper1$rurality, ref="1")

#Completion of CHA
paper1$CHA[LHD_2013_m1$c7q147 == "Yes_<3 years"] <- "Yes"
paper1$CHA[LHD_2013_m1$c7q147 == "Yes_3-5 years"] <- "Yes"
paper1$CHA[LHD_2013_m1$c7q147 == "Yes_5+ years"] <- "No"
paper1$CHA[LHD_2013_m1$c7q147 == "No_next year"] <- "No"
paper1$CHA[LHD_2013_m1$c7q147 == "No"] <- NA
paper1$CHA<-as.factor(paper1$CHA)

#Completion of Community health improvement plan 
paper1$CHIP[LHD_2013_m1$c7q149 == "Yes_<3 years"] <- "Yes"
paper1$CHIP[LHD_2013_m1$c7q149 == "Yes_3-5 years"] <- "Yes"
paper1$CHIP[LHD_2013_m1$c7q149 == "Yes_5+ years"] <- "No"
paper1$CHIP[LHD_2013_m1$c7q149 == "No_next year"] <- "No"
paper1$CHIP[LHD_2013_m1$c7q149 == "No"] <- NA
paper1$CHIP<-as.factor(paper1$CHIP)

#agency wide strategic plan
paper1$stplan[LHD_2013_m1$c7q217 == "Yes_<3 years"] <- "Yes"
paper1$stplan[LHD_2013_m1$c7q217 == "Yes_3-5 years"] <- "Yes"
paper1$stplan[LHD_2013_m1$c7q217 == "Yes_5+ years"] <- "No"
paper1$stplan[LHD_2013_m1$c7q217 == "No_next year"] <- "No"
paper1$stplan[LHD_2013_m1$c7q217 == "No"] <- NA
paper1$stplan<-as.factor(paper1$stplan)

#Local board of health
paper1$LBH<- LHD_2013_m1$c2q301 %>% as.factor()

#LBH adopt regulations
paper1$LBHadoptreg[LHD_2013_m1$c2q8a == "checked"] <- "Yes"
paper1$LBHadoptreg[is.na(LHD_2013_m1$c2q8a) == T] <- NA
paper1$LBHadoptreg[LHD_2013_m1$c2q8a == "unchecked"] <- "No"
paper1$LBHadoptreg<-as.factor(paper1$LBHadoptreg)

#governance
paper1$governance <- LHD_2013_m1$c0govcat %>% as.factor()
paper1$governance <- relevel(paper1$governance, ref=" shared")
summary(paper1$governance)

#epidemiologist
paper1$epidemiologist<-LHD_2013_m1$c5q47a

#public health physicians
paper1$PHphysician<-LHD_2013_m1$c5q44a

#information systems specialists
paper1$ITspecialist<-LHD_2013_m1$c5q50a

#agency admin highest degree
paper1$degree[(LHD_2013_m1$c4q31a=='checked'|LHD_2013_m1$c4q31b=='checked')==TRUE] <- 'Associates'

paper1$degree[(LHD_2013_m1$c4q32a=='checked'|
                 LHD_2013_m1$c4q32b=='checked'|LHD_2013_m1$c4q32c=='checked'|
                 LHD_2013_m1$c4q32d=='checked')==TRUE] <- 'Bachelors'

paper1$degree[(LHD_2013_m1$c4q33e=='checked'|LHD_2013_m1$c4q33f=='checked'|
                 LHD_2013_m1$c4q33a=='checked'|LHD_2013_m1$c4q33b=='checked'|
                 LHD_2013_m1$c4q33c=='checked')==TRUE] <- 'Masters'

paper1$degree[(LHD_2013_m1$c4q34a=='checked'|LHD_2013_m1$c4q34b=='checked'|
                 LHD_2013_m1$c4q34c=='checked'|LHD_2013_m1$c4q34d=='checked'|
                 LHD_2013_m1$c4q34e=='checked'|LHD_2013_m1$c4q34f=='checked'|
                 LHD_2013_m1$c4q34g=='checked'|LHD_2013_m1$c4q34h=='checked')==TRUE] <- 'Professional/Doctoral'

paper1$degree<-as.factor(paper1$degree)

#current QI activities 
paper1$QI<-LHD_2013_m1$m1q301

#population 
paper1$population <- LHD_2013_m1$c0population

#revenue
paper1$revenue <- LHD_2013_m1$c3q16/1000000
paper1$PerCaprevenue <-(LHD_2013_m1$c3q16)/(LHD_2013_m1$c0population)

#FTE
paper1$FTE<-LHD_2013_m1$c5q37/(LHD_2013_m1$c0population/10000)

##Exclude LHDs pursing accreditation via state##
#drop PHAB state
paper1final <-subset(paper1, !(is.na(paper1$PHAB)))

############################################

###TABLE 1. DESCRIPTIVE TABLE###

wpct(paper1final$rurality, weight=paper1final$weight01)
wpct(paper1final$CHA, weight=paper1final$weight01)
wpct(paper1final$CHIP, weight=paper1final$weight01)
wpct(paper1final$stplan, weight=paper1final$weight01)
wpct(paper1final$LBH, weight=paper1final$weight01)
wpct(paper1final$LBHadoptreg, weight=paper1final$weight01)
wpct(paper1final$governance, weight=paper1final$weight01)
wpct(paper1final$epidemiologist, weight=paper1final$weight01)
wpct(paper1final$PHphysician, weight=paper1final$weight01)
wpct(paper1final$ITspecialist, weight=paper1final$weight01)
wpct(paper1final$degree, weight=paper1final$weight01)
wpct(paper1final$QI, weight=paper1final$weight01)

wtd.mean(paper1final$population, weight=paper1final$weight01, na.rm=T)
sqrt(wtd.var(paper1final$population, weight=paper1final$weight01, na.rm=T))

wtd.mean(paper1final$revenue, weight=paper1final$weight01, na.rm=T)
sqrt(wtd.var(paper1final$revenue, weight=paper1final$weight01, na.rm=T))

wtd.mean(paper1final$FTE, weight=paper1final$weight01, na.rm=T)
sqrt(wtd.var(paper1final$FTE, weight=paper1final$weight01, na.rm=T))

###TABLE II. LHD CHARACTERISTICS BY ACCREDITATION-SEEKING DECISION###
crosstab(paper1final$rurality, paper1final$PHAB, weight=paper1final$weight01, prop.c=T)
crosstab(paper1final$CHA, paper1final$PHAB, weight=paper1final$weight01, prop.c=T)
crosstab(paper1final$CHIP, paper1final$PHAB, weight=paper1final$weight01, prop.c=T)
crosstab(paper1final$stplan, paper1final$PHAB, weight=paper1final$weight01, prop.c=T)
crosstab(paper1final$LBH, paper1final$PHAB, weight=paper1final$weight01, prop.c=T)
crosstab(paper1final$LBHadoptreg, paper1final$PHAB, weight=paper1final$weight01, prop.c=T)
crosstab(paper1final$governance, paper1final$PHAB, weight=paper1final$weight01, prop.c=T)
crosstab(paper1final$epidemiologist, paper1final$PHAB, weight=paper1final$weight01, prop.c=T)
crosstab(paper1final$PHphysician, paper1final$PHAB, weight=paper1final$weight01, prop.c=T)
crosstab(paper1final$ITspecialist, paper1final$PHAB, weight=paper1final$weight01, prop.c=T)
crosstab(paper1final$degree, paper1final$PHAB, weight=paper1final$weight01, prop.c=T)
crosstab(paper1final$QI, paper1final$PHAB, weight=paper1final$weight01, prop.c=T)

wtd.mean(paper1final$population[paper1final$PHAB=="seeking"], weight=paper1final$weight01[paper1final$PHAB=="seeking"], na.rm=T)
sqrt(wtd.var(paper1final$population[paper1final$PHAB=="seeking"], weight=paper1final$weight01[paper1final$PHAB=="seeking"], na.rm=T))
wtd.mean(paper1final$population[paper1final$PHAB=="not seeking"], weight=paper1final$weight01[paper1final$PHAB=="not seeking"], na.rm=T)
sqrt(wtd.var(paper1final$population[paper1final$PHAB=="not seeking"], weight=paper1final$weight01[paper1final$PHAB=="not seeking"], na.rm=T))

wtd.mean(paper1final$revenue[paper1final$PHAB=="seeking"], weight=paper1final$weight01[paper1final$PHAB=="seeking"], na.rm=T)
sqrt(wtd.var(paper1final$revenue[paper1final$PHAB=="seeking"], weight=paper1final$weight01[paper1final$PHAB=="seeking"], na.rm=T))
wtd.mean(paper1final$revenue[paper1final$PHAB=="not seeking"], weight=paper1final$weight01[paper1final$PHAB=="not seeking"], na.rm=T)
sqrt(wtd.var(paper1final$revenue[paper1final$PHAB=="not seeking"], weight=paper1final$weight01[paper1final$PHAB=="not seeking"], na.rm=T))

wtd.mean(paper1final$FTE[paper1final$PHAB=="seeking"], weight=paper1final$weight01[paper1final$PHAB=="seeking"], na.rm=T)
sqrt(wtd.var(paper1final$FTE[paper1final$PHAB=="seeking"], weight=paper1final$weight01[paper1final$PHAB=="seeking"], na.rm=T))
wtd.mean(paper1final$FTE[paper1final$PHAB=="not seeking"], weight=paper1final$weight01[paper1final$PHAB=="not seeking"], na.rm=T)
sqrt(wtd.var(paper1final$FTE[paper1final$PHAB=="not seeking"], weight=paper1final$weight01[paper1final$PHAB=="not seeking"], na.rm=T))

############################################

##TABLE 4 MULTIVARIATE LOGISTIC REGRESSION##

#Run Regression
#warning occurs with output likely due to overdispersion
model <- glm(PHAB ~ rurality + PerCaprevenue + stplan + governance + LBH + epidemiologist +
               PHphysician + ITspecialist,
             data = paper1, weights = weight01,
             family = "binomial",
             na.action="na.omit")

#Obtain OR, 95% CI and p-value
ORmultivariate(model)

#Model Fit: % Correctly Predicted       
prob1 <- predict(model, newdata = paper1, type = "response")
prob1 <- prob1[!is.na(prob1)]
pred1 <- rep("No", length(prob1))
pred1[prob1>0.5] <- "Yes"

table(paper1$PHAB[as.numeric(names(prob1))], pred1)   #86.9% correctly predicted

##FORESTPLOT OF ORIGINAL VS REPRODUCED##
dat2<-read.csv(file.choose())
dat2 <-subset(dat2, !(is.na(dat2$paper1OR)))
dat2$paper1IV <- factor(dat2$paper1IV, levels=c('Information systems specialist (ref=no)',
                                                'Public health physician (ref=no)',
                                                'Epidemiologist (ref=no)',
                                                'Local board(s) of health (ref=no)',
                                                'Local governance',
                                                'State governance',
                                                'Shared governance (ref)',
                                                'Agency-wide strategic plan (ref=no)',
                                                'Per capita revenue',
                                                'Urban',
                                                'Micropolitan',
                                                'Rural (ref)'))

#install and open ggplot2 and stringr libraries
orspaper1<-ggplot(dat2, aes(x = paper1IV, y = paper1OR,  ymin = paper1LCI, ymax = paper1UCI)) +
  geom_pointrange(aes(col=factor(dat2$paper1REP)), size=.75, position=position_dodge(width=.50)) +
  ylab("Odds ratio & 95% CI") + geom_hline(aes(yintercept = 1)) +
  scale_y_log10(breaks=c(.1,1,10), labels=c("0.1","1.0","10.0")) +
  xlab("Local Health Department Characteristic") + scale_color_manual(values=c("black","gray70"),
                                         name="")+
  theme(legend.position="top", plot.title = element_text(hjust = 0.5))+
  coord_flip()
orspaper1
