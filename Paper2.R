############### PAPER 2/5 ###################

##DATA SOURCES##

#NACCHO data obtained from: http://nacchoprofilestudy.org/data-requests/, "2013 Profile Study"

##CODE INFORMATION##

#Written by: Sarah Wondmeneh and Yiqiang Zhao#
#Github Link: https://github.com/statzgrl/Reproducible-Research/blob/master/Paper2.R

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

#select module 1 only (n=490)
LHD_2013_m1 <- LHD_2013[LHD_2013$c0module == "Core + Module 1" & is.na(LHD_2013$c0module) == F, ]

##INPUT VARIABLES##

#Create dataframe
paper2 <- data.frame(population = character(490))

#weight
paper2$weight01 <- LHD_2013_m1$c0moduleweight1

# population
paper2$population <- LHD_2013_m1$c0population

# Governance   
paper2$governance <- LHD_2013_m1$c0govcat %>% as.factor()
paper2$governance <- relevel(paper2$governance, ref="local")

#Agency-wise strategic plan 
paper2$stplan[LHD_2013_m1$c7q217 == "Yes_<3 years"] <- "Yes5y"
paper2$stplan[LHD_2013_m1$c7q217 == "Yes_3-5 years"] <- "Yes5y"
paper2$stplan[LHD_2013_m1$c7q217 == "Yes_5+ years"] <- "NoSPnot5y"
paper2$stplan[LHD_2013_m1$c7q217 == "No_next year"] <- "NoNextYear"
paper2$stplan[LHD_2013_m1$c7q217 == "No"] <- "NoSPnot5y"
paper2$stplan[is.na(LHD_2013_m1$c7q217) == T] <- "NoSPnot5y"
paper2$stplan<-as.factor(paper2$stplan)
paper2$stplan <- relevel(paper2$stplan, ref="NoSPnot5y")

#cha 
paper2$CHA[LHD_2013_m1$c7q147 == "Yes_<3 years"] <- "Yes5y"
paper2$CHA[LHD_2013_m1$c7q147 == "Yes_3-5 years"] <- "Yes5y"
paper2$CHA[LHD_2013_m1$c7q147 == "Yes_5+ years"] <- "NoCHAnot5y"
paper2$CHA[LHD_2013_m1$c7q147 == "No_next year"] <- "NoNextYear"
paper2$CHA[LHD_2013_m1$c7q147 == "No"] <- "NoCHAnot5y"
paper2$CHA[is.na(LHD_2013_m1$c7q147) == T] <- "NoCHAnot5y"
paper2$CHA <- as.factor(paper2$CHA)

#CHIP 
paper2$CHIP[LHD_2013_m1$c7q149 == "Yes_<3 years"] <- "Yes5y"
paper2$CHIP[LHD_2013_m1$c7q149 == "Yes_3-5 years"] <- "Yes5y"
paper2$CHIP[LHD_2013_m1$c7q149 == "Yes_5+ years"] <- "NoCHIPnot5y"
paper2$CHIP[LHD_2013_m1$c7q149 == "No_next year"] <- "NoNextYear"
paper2$CHIP[LHD_2013_m1$c7q149 == "No"] <- "NoCHIPnot5y"
paper2$CHIP[is.na(LHD_2013_m1$c7q149) == T] <- "NoCHIPnot5y"
paper2$CHIP <- as.factor(paper2$CHIP)

#Top executive had MD degree 
paper2$MD[LHD_2013_m1$c4q34a == "checked"] <- "Yes"
paper2$MD[is.na(LHD_2013_m1$c4q34a) == T] <- "No"
paper2$MD[LHD_2013_m1$c4q34a == "unchecked"] <- "No"
paper2$MD <- as.factor(paper2$MD)

#Local Board of Health  
paper2$LBH <- LHD_2013_m1$c2q301
paper2$LBH<-as.factor(paper2$LBH)
paper2$LBH<- relevel(paper2$LBH, ref="yes")

#LHD worked with other organizationsfor CHA 
paper2$CHAcollab[LHD_2013_m1$m13q405 == "Not involved"|LHD_2013_m1$m13q405 == "No programs"] <- "Not involved no program" 
paper2$CHAcollab[LHD_2013_m1$m13q405 == " Networking"|LHD_2013_m1$m13q405 == " Coordinating"
               |LHD_2013_m1$m13q405 == "Cooperating"] <- "Networking, Coordinating or Cooperating" 
paper2$CHAcollab[LHD_2013_m1$m13q405 == " Collaborating"]<-"Collaborating"
paper2$CHAcollab<-as.factor(paper2$CHAcollab)
paper2$CHAcollab<- relevel(paper2$CHAcollab, ref="Not involved no program")

#Per capita expenditures quartiles  
LHD_2013_m1$percapexp <- LHD_2013_m1$c3q15/LHD_2013_m1$c0population
paper2$expenditure[LHD_2013_m1$percapexp <= 22.80] <- "<22.80"
paper2$expenditure[(LHD_2013_m1$percapexp <= 37.88) & (LHD_2013_m1$percapexp > 22.80)] <- "22.81-37.88"
paper2$expenditure[(LHD_2013_m1$percapexp <= 65.02) & (LHD_2013_m1$percapexp > 37.88)] <- "37.89-65.02"
paper2$expenditure[LHD_2013_m1$percapexp > 65.02] <- ">65.02"
paper2$expenditure[is.na(LHD_2013_m1$c3q15)==T]<-"Not reported"
paper2$expenditure <- as.factor(paper2$expenditure)
paper2$expenditure<- relevel(paper2$expenditure, ref=">65.02")

#LHD level of engagement in accredidation 
LHD_2013_m1$m2q401<-as.character(LHD_2013_m1$m2q401)
paper2$engagementallcat<-LHD_2013_m1$m2q401
trim(paper2$engagementallcat)
paper2$engagement[LHD_2013_m1$m2q401 == "undecided"|LHD_2013_m1$m2q401 == "not applying"] <- "No" 
paper2$engagement[LHD_2013_m1$m2q401 == "achieved"|LHD_2013_m1$m2q401 == "submitted application"
                |LHD_2013_m1$m2q401 == "submitted statement of intent"|LHD_2013_m1$m2q401 == "plans to apply, no statement"
                |LHD_2013_m1$m2q401 == "SHA will apply for LHD"] <- "Yes" 
paper2$engagement[is.na(LHD_2013_m1$m2q401) == T]<-"No" 
paper2$engagement<-as.factor(paper2$engagement)
paper2$engagement <- relevel(paper2$engagement, ref="No")

############################################

###TABLE 1. DESCRIPTIVE STATISTICS###

table(paper2$engagementallcat)
wpct(paper2$engagementallcat, weight=paper2$weight01)
table(paper2$governance)
wpct(paper2$governance, weight=paper2$weight01)
table(paper2$CHA)
wpct(paper2$CHA, weight=paper2$weight01)
table(paper2$CHIP)
wpct(paper2$CHIP, weight=paper2$weight01)
table(paper2$stplan)
wpct(paper2$stplan, weight=paper2$weight01)
table(paper2$MD)
wpct(paper2$MD, weight=paper2$weight01)
table(paper2$LBH)
wpct(paper2$LBH, weight=paper2$weight01)
table(paper2$CHAcollab)
wpct(paper2$CHAcollab, weight=paper2$weight01)
table(paper2$expenditure)
wpct(paper2$expenditure, weight=paper2$weight01)

############################################

##TABLE 3 MULTIVARIATE LOGISTIC REGRESSION##

#Run Regression
model1 <- glm(engagement ~ population + governance + stplan + CHA + CHIP + 
                MD + LBH + CHAcollab + expenditure,
              data = paper2, weights = weight01,
              family = "binomial",
              na.action="na.omit")

#Obtain OR, 95% CI and p-value
ORmultivariate(model1)

#Model Fit: $ Correctly Predicted
prob1 <- predict(model1, newdata = paper2, type = "response")
prob1 <- prob1[!is.na(prob1)]
pred1 <- rep("No", length(prob1))
pred1[prob1>0.5] <- "Yes"
table(paper2$engagement[as.numeric(names(prob1))], pred1) # 70.5% accuracy

##FORESTPLOT OF ORIGINAL VS REPRODUCED##

dat2<-read.csv(file.choose())
dat2 <-subset(dat2, !(is.na(dat2$paper2OR)))
dat2$paper2IV <- factor(dat2$paper2IV, levels=c('>$65.02 (ref)',
                                              '$37.89-65.02',
                                              '$22.81-37.88',
                                              '<$22.80',
                                              'Per capita expenditures quartiles: Not reported',
                                              'Not involved no program (ref)',
                                              'Collaborating',
                                              'To accomplish CHA: Networking, coordinating, or cooperating',
                                              'Yes local board of health (ref)',
                                              'LHD had local board of health: No',
                                              'No (ref)',
                                              'Top executive had MD degree: Yes',
                                              'No CHIP or not within 5 y (ref)',
                                              'No, but plan CHIP next year',
                                              'CHIP completed: Yes within 5 y',
                                              'No CHA or not within 5 y (ref)',
                                              'No, but plan CHA next year',
                                              'CHA performed: Yes within 5 y ',
                                              'No SP or not within 5 y (ref)',
                                              'No, but plan SP next year',
                                              'Strategic plan: Yes within 5 y',
                                              'Local (ref)',
                                              'Shared',
                                              'Governance category: State',
                                              'Population'))
                                              
library(ggplot2)
orspaper2<-ggplot(dat2, aes(x = paper2IV, y = paper2OR,  ymin = paper2LCI, ymax = paper2UCI)) +
  geom_pointrange(aes(col=factor(dat2$paper2REP)), size=.75, position=position_dodge(width=.50)) +
  ylab("Odds ratio & 95% CI") + geom_hline(aes(yintercept = 1)) +
  scale_y_log10() +
  xlab("Local Health Department Characteristic") + scale_color_manual(values=c("black","gray70"),
                                                                      labels=c("Original", "Reproduced"),
                                                                      name="")+
  theme(legend.position="top", plot.title = element_text(hjust = 0.5))+
  coord_flip()
orspaper2
