############### PAPER 1/5 ###################

##Beatty KE, Erwin PC, Brownson RC, Meit M, Fey J (2017) JPHMP##
#PubMed Link: https://www.ncbi.nlm.nih.gov/pubmed/28079646#

##DATA SOURCES##

#NACCHO data obtained from: http://nacchoprofilestudy.org/data-requests/, "2013 Profile Study"
#RUCA data obtained from Kate Beatty

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

beattydata <- read.csv(file.choose(), header = TRUE)
ruca <- data.frame(nacchoid = character(2000))
ruca$ruca_cat <- beattydata$ruca_3_cat_RC
ruca$nacchoid <- beattydata$nacchoid

#merge datasets

LHD_2013_ruca <- merge(ruca, LHD_2013,by="nacchoid")

#select module 1 only
LHD_2013_m1 <- LHD_2013_ruca[LHD_2013_ruca$c0module == "Core + Module 1" & is.na(LHD_2013_ruca$c0module) == F, ]

##INPUT RELEVANT VARIABLES##

#Create data frame#
Beatty <- data.frame(population = character(490))

#weight
Beatty$weight01 <- LHD_2013_m1$c0moduleweight1

#Decision to seek PHAB accreditation
#Authors omitted LHDs applying through state
#so recoded this to NA
Beatty$PHAB[LHD_2013_m1$m2q401 == "undecided"] <- "not seeking"
Beatty$PHAB[LHD_2013_m1$m2q401 == "not applying"] <- "not seeking"
Beatty$PHAB[LHD_2013_m1$m2q401 == "plans to apply, no statement"]<-"not seeking"
Beatty$PHAB[LHD_2013_m1$m2q401 == "submitted application" |LHD_2013_m1$m2q401 == "submitted statement of intent" ] <- "seeking"
Beatty$PHAB[LHD_2013_m1$m2q401 == "SHA will apply for LHD"]<- NA
Beatty$PHAB<-as.factor(Beatty$PHAB)

#rurality
#CODING FOR Rurality: 1 = rural, 2 = micropolitan, 3 = urban

Beatty$rurality<-LHD_2013_m1$ruca_cat
Beatty$rurality<-as.factor(Beatty$rurality)
Beatty$rurality <- relevel(Beatty$rurality, ref="1")

#Completion of CHA
Beatty$CHA[LHD_2013_m1$c7q147 == "Yes_<3 years"] <- "Yes"
Beatty$CHA[LHD_2013_m1$c7q147 == "Yes_3-5 years"] <- "Yes"
Beatty$CHA[LHD_2013_m1$c7q147 == "Yes_5+ years"] <- "No"
Beatty$CHA[LHD_2013_m1$c7q147 == "No_next year"] <- "No"
Beatty$CHA[LHD_2013_m1$c7q147 == "No"] <- NA
Beatty$CHA<-as.factor(Beatty$CHA)

#Completion of Community health improvement plan 
Beatty$CHIP[LHD_2013_m1$c7q149 == "Yes_<3 years"] <- "Yes"
Beatty$CHIP[LHD_2013_m1$c7q149 == "Yes_3-5 years"] <- "Yes"
Beatty$CHIP[LHD_2013_m1$c7q149 == "Yes_5+ years"] <- "No"
Beatty$CHIP[LHD_2013_m1$c7q149 == "No_next year"] <- "No"
Beatty$CHIP[LHD_2013_m1$c7q149 == "No"] <- NA
Beatty$CHIP<-as.factor(Beatty$CHIP)

#agency wide strategic plan
Beatty$stplan[LHD_2013_m1$c7q217 == "Yes_<3 years"] <- "Yes"
Beatty$stplan[LHD_2013_m1$c7q217 == "Yes_3-5 years"] <- "Yes"
Beatty$stplan[LHD_2013_m1$c7q217 == "Yes_5+ years"] <- "No"
Beatty$stplan[LHD_2013_m1$c7q217 == "No_next year"] <- "No"
Beatty$stplan[LHD_2013_m1$c7q217 == "No"] <- NA
Beatty$stplan<-as.factor(Beatty$stplan)

#Local board of health
Beatty$LBH<- LHD_2013_m1$c2q301 %>% as.factor()

#LBH adopt regulations
Beatty$LBHadoptreg[LHD_2013_m1$c2q8a == "checked"] <- "Yes"
Beatty$LBHadoptreg[is.na(LHD_2013_m1$c2q8a) == T] <- NA
Beatty$LBHadoptreg[LHD_2013_m1$c2q8a == "unchecked"] <- "No"
Beatty$LBHadoptreg<-as.factor(Beatty$LBHadoptreg)

#governance
Beatty$governance <- LHD_2013_m1$c0govcat %>% as.factor()
Beatty$governance <- relevel(Beatty$governance, ref=" shared")
summary(Beatty$governance)

#epidemiologist
Beatty$epidemiologist<-LHD_2013_m1$c5q47a

#public health physicians
Beatty$PHphysician<-LHD_2013_m1$c5q44a

#information systems specialists
Beatty$ITspecialist<-LHD_2013_m1$c5q50a

#agency admin highest degree
Beatty$degree[(LHD_2013_m1$c4q31a=='checked'|LHD_2013_m1$c4q31b=='checked')==TRUE] <- 'Associates'

Beatty$degree[(LHD_2013_m1$c4q32a=='checked'|
                 LHD_2013_m1$c4q32b=='checked'|LHD_2013_m1$c4q32c=='checked'|
                 LHD_2013_m1$c4q32d=='checked')==TRUE] <- 'Bachelors'

Beatty$degree[(LHD_2013_m1$c4q33e=='checked'|LHD_2013_m1$c4q33f=='checked'|
                 LHD_2013_m1$c4q33a=='checked'|LHD_2013_m1$c4q33b=='checked'|
                 LHD_2013_m1$c4q33c=='checked')==TRUE] <- 'Masters'

Beatty$degree[(LHD_2013_m1$c4q34a=='checked'|LHD_2013_m1$c4q34b=='checked'|
                 LHD_2013_m1$c4q34c=='checked'|LHD_2013_m1$c4q34d=='checked'|
                 LHD_2013_m1$c4q34e=='checked'|LHD_2013_m1$c4q34f=='checked'|
                 LHD_2013_m1$c4q34g=='checked'|LHD_2013_m1$c4q34h=='checked')==TRUE] <- 'Professional/Doctoral'

Beatty$degree<-as.factor(Beatty$degree)

#current QI activities 
Beatty$QI<-LHD_2013_m1$m1q301

#population 
Beatty$population <- LHD_2013_m1$c0population

#revenue
Beatty$revenue <- LHD_2013_m1$c3q16/1000000
Beatty$PerCaprevenue <-(LHD_2013_m1$c3q16)/(LHD_2013_m1$c0population)

#FTE
Beatty$FTE<-LHD_2013_m1$c5q37/(LHD_2013_m1$c0population/10000)

##Exclude LHDs pursing accreditation via state##
#drop PHAB state
Beattyfinal <-subset(Beatty, !(is.na(Beatty$PHAB)))

############################################

###TABLE 1. DESCRIPTIVE TABLE###

wpct(Beattyfinal$rurality, weight=Beattyfinal$weight01)
wpct(Beattyfinal$CHA, weight=Beattyfinal$weight01)
wpct(Beattyfinal$CHIP, weight=Beattyfinal$weight01)
wpct(Beattyfinal$stplan, weight=Beattyfinal$weight01)
wpct(Beattyfinal$LBH, weight=Beattyfinal$weight01)
wpct(Beattyfinal$LBHadoptreg, weight=Beattyfinal$weight01)
wpct(Beattyfinal$governance, weight=Beattyfinal$weight01)
wpct(Beattyfinal$epidemiologist, weight=Beattyfinal$weight01)
wpct(Beattyfinal$PHphysician, weight=Beattyfinal$weight01)
wpct(Beattyfinal$ITspecialist, weight=Beattyfinal$weight01)
wpct(Beattyfinal$degree, weight=Beattyfinal$weight01)
wpct(Beattyfinal$QI, weight=Beattyfinal$weight01)

wtd.mean(Beattyfinal$population, weight=Beattyfinal$weight01, na.rm=T)
sqrt(wtd.var(Beattyfinal$population, weight=Beattyfinal$weight01, na.rm=T))

wtd.mean(Beattyfinal$revenue, weight=Beattyfinal$weight01, na.rm=T)
sqrt(wtd.var(Beattyfinal$revenue, weight=Beattyfinal$weight01, na.rm=T))

wtd.mean(Beattyfinal$FTE, weight=Beattyfinal$weight01, na.rm=T)
sqrt(wtd.var(Beattyfinal$FTE, weight=Beattyfinal$weight01, na.rm=T))

###TABLE II. LHD CHARACTERISTICS BY ACCREDITATION-SEEKING DECISION###
crosstab(Beattyfinal$rurality, Beattyfinal$PHAB, weight=Beattyfinal$weight01, prop.c=T)
crosstab(Beattyfinal$CHA, Beattyfinal$PHAB, weight=Beattyfinal$weight01, prop.c=T)
crosstab(Beattyfinal$CHIP, Beattyfinal$PHAB, weight=Beattyfinal$weight01, prop.c=T)
crosstab(Beattyfinal$stplan, Beattyfinal$PHAB, weight=Beattyfinal$weight01, prop.c=T)
crosstab(Beattyfinal$LBH, Beattyfinal$PHAB, weight=Beattyfinal$weight01, prop.c=T)
crosstab(Beattyfinal$LBHadoptreg, Beattyfinal$PHAB, weight=Beattyfinal$weight01, prop.c=T)
crosstab(Beattyfinal$governance, Beattyfinal$PHAB, weight=Beattyfinal$weight01, prop.c=T)
crosstab(Beattyfinal$epidemiologist, Beattyfinal$PHAB, weight=Beattyfinal$weight01, prop.c=T)
crosstab(Beattyfinal$PHphysician, Beattyfinal$PHAB, weight=Beattyfinal$weight01, prop.c=T)
crosstab(Beattyfinal$ITspecialist, Beattyfinal$PHAB, weight=Beattyfinal$weight01, prop.c=T)
crosstab(Beattyfinal$degree, Beattyfinal$PHAB, weight=Beattyfinal$weight01, prop.c=T)
crosstab(Beattyfinal$QI, Beattyfinal$PHAB, weight=Beattyfinal$weight01, prop.c=T)

wtd.mean(Beattyfinal$population[Beattyfinal$PHAB=="seeking"], weight=Beattyfinal$weight01[Beattyfinal$PHAB=="seeking"], na.rm=T)
sqrt(wtd.var(Beattyfinal$population[Beattyfinal$PHAB=="seeking"], weight=Beattyfinal$weight01[Beattyfinal$PHAB=="seeking"], na.rm=T))
wtd.mean(Beattyfinal$population[Beattyfinal$PHAB=="not seeking"], weight=Beattyfinal$weight01[Beattyfinal$PHAB=="not seeking"], na.rm=T)
sqrt(wtd.var(Beattyfinal$population[Beattyfinal$PHAB=="not seeking"], weight=Beattyfinal$weight01[Beattyfinal$PHAB=="not seeking"], na.rm=T))

wtd.mean(Beattyfinal$revenue[Beattyfinal$PHAB=="seeking"], weight=Beattyfinal$weight01[Beattyfinal$PHAB=="seeking"], na.rm=T)
sqrt(wtd.var(Beattyfinal$revenue[Beattyfinal$PHAB=="seeking"], weight=Beattyfinal$weight01[Beattyfinal$PHAB=="seeking"], na.rm=T))
wtd.mean(Beattyfinal$revenue[Beattyfinal$PHAB=="not seeking"], weight=Beattyfinal$weight01[Beattyfinal$PHAB=="not seeking"], na.rm=T)
sqrt(wtd.var(Beattyfinal$revenue[Beattyfinal$PHAB=="not seeking"], weight=Beattyfinal$weight01[Beattyfinal$PHAB=="not seeking"], na.rm=T))

wtd.mean(Beattyfinal$FTE[Beattyfinal$PHAB=="seeking"], weight=Beattyfinal$weight01[Beattyfinal$PHAB=="seeking"], na.rm=T)
sqrt(wtd.var(Beattyfinal$FTE[Beattyfinal$PHAB=="seeking"], weight=Beattyfinal$weight01[Beattyfinal$PHAB=="seeking"], na.rm=T))
wtd.mean(Beattyfinal$FTE[Beattyfinal$PHAB=="not seeking"], weight=Beattyfinal$weight01[Beattyfinal$PHAB=="not seeking"], na.rm=T)
sqrt(wtd.var(Beattyfinal$FTE[Beattyfinal$PHAB=="not seeking"], weight=Beattyfinal$weight01[Beattyfinal$PHAB=="not seeking"], na.rm=T))

############################################

##TABLE 4 MULTIVARIATE LOGISTIC REGRESSION##

#Run Regression
model <- glm(PHAB ~ rurality + PerCaprevenue + stplan + governance + LBH + epidemiologist +
               PHphysician + ITspecialist,
             data = Beatty, weights = weight01,
             family = "quasibinomial",
             na.action="na.omit")

#Obtain OR, 95% CI and p-value
ORmultivariate(model)

#Model Fit: % Correctly Predicted       
prob1 <- predict(model, newdata = Beatty, type = "response")
prob1 <- prob1[!is.na(prob1)]
pred1 <- rep("No", length(prob1))
pred1[prob1>0.5] <- "Yes"

table(Beatty$PHAB[as.numeric(names(prob1))], pred1)   #86.9% correctly predicted

##FORESTPLOT OF ORIGINAL VS REPRODUCED##
dat2<-read.csv(file.choose())
dat2 <-subset(dat2, !(is.na(dat2$beattyOR)))
dat2$beattyIV <- factor(dat2$beattyIV, levels=c('Information systems specialist (ref=no)',
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
orsbeatty<-ggplot(dat2, aes(x = beattyIV, y = beattyOR,  ymin = beattyLCI, ymax = beattyUCI)) +
  geom_pointrange(aes(col=factor(dat2$beattyREP)), size=.75, position=position_dodge(width=.50)) +
  ylab("Odds ratio & 95% CI") + geom_hline(aes(yintercept = 1)) +
  scale_y_log10(breaks=c(.1,1,10), labels=c("0.1","1.0","10.0")) +
  xlab("Local Health Department Characteristic") + scale_color_manual(values=c("black","gray70"),
                                         name="")+
  theme(legend.position="top", plot.title = element_text(hjust = 0.5))+
  coord_flip()
orsbeatty
