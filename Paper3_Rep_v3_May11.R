############### PAPER 3/5 ###################

##DATA SOURCES##

#NACCHO data obtained from: http://nacchoprofilestudy.org/data-requests/, "2013 Profile Study"

##CODE INFORMATION##

#Written by: Sarah Wondmeneh and Yiqiang Zhao#
#Github Link: #

############################################

###CREATE DATASET###

##Open Packages##

library(foreign)
library(magrittr)
library(PredictABEL)
library(weights)
library(descr)
library(car)

#import 2013 NACCHO Data

LHD_2013 <- read.spss(file.choose(), use.value.labels = TRUE)
LHD_2013 <- as.data.frame(LHD_2013)

#select module 1 only
LHD_2013_m1 <- LHD_2013[LHD_2013$c0module == "Core + Module 1" & is.na(LHD_2013$c0module) == F, ]

##INPUT VARIABLES##

#Create data frame#
paper3 <- data.frame(population = character(490))

#weight
paper3$weight01 <- LHD_2013_m1$c0moduleweight1

#population
paper3$population <- recode(LHD_2013_m1$c0population,"
                           1:49999 = '<50000';
                           50000:499999 = '50000-499999';
                           else = '500000+'") %>% as.factor()

summary(paper3$population)

#geographic region - based on Census region classification
#West: WA, OR, HI, CA, AK, AZ, CO, ID, NM, MT, UT, NV, WY
#Midwest: ND, SD, NE, KS, MN, IA, MO, WI, IL, IN, MI, OH
#Northeast: NY, PA, NJ, ME, VT, NH, MA, CT, RI
#South: OK, TX, AR, LA, MS, AL, TN, KY, GA, FL, WV, VA, NC, SC, MD, DC, DE

paper3$region <- recode(LHD_2013_m1$c0state,"
                              'NY' = 'Northeast';'PA' = 'Northeast';'NJ' = 'Northeast';'ME' = 'Northeast';
                              'VT' = 'Northeast';'NH' = 'Northeast';'MA' = 'Northeast';'CT' = 'Northeast';'MA' = 'Northeast';
                              'ND' = 'Midwest';'SD' = 'Midwest';'NE' = 'Midwest';'KS' = 'Midwest';'MN' = 'Midwest';
                              'IA' = 'Midwest';'MO' = 'Midwest';'WI' = 'Midwest';'IL' = 'Midwest';'IN' = 'Midwest';
                              'MI' = 'Midwest';'OH' = 'Midwest';
                              'WA' = 'West';'OR' = 'West';'HI' = 'West';'CA' = 'West';'AK' = 'West';'AZ' = 'West';
                              'CO' = 'West';'ID' = 'West';'NM' = 'West';'MT' = 'West';'UT' = 'West';'NV' = 'West';'WY' = 'West';
                             else = 'South'") %>% as.factor()

paper3$region <- relevel(paper3$region, ref="Northeast")

# Jurisdiction type 
paper3$jurisdiction <- recode(LHD_2013_m1$c0jurisdiction,"
                             'city        ' = 'city';
                             'county      ' = 'county';
                             else = 'other'") %>% as.factor()

# Governance 
paper3$governance <- LHD_2013_m1$c0govcat %>% as.factor()

# Local board of health 
paper3$local_board <- LHD_2013_m1$c2q301


# FTE per 100,000 population 
paper3$FTE[LHD_2013_m1$c5q37/(LHD_2013_m1$c0population/100000) <= 27.92 & is.na(LHD_2013_m1$c5q37) == F] <- "Quartile 1"
paper3$FTE[LHD_2013_m1$c5q37/(LHD_2013_m1$c0population/100000) <= 45.95 & LHD_2013_m1$c5q37/(LHD_2013_m1$c0population/100000) >= 27.93] <- "Quartile 2"
paper3$FTE[LHD_2013_m1$c5q37/(LHD_2013_m1$c0population/100000) <= 73.49 & LHD_2013_m1$c5q37/(LHD_2013_m1$c0population/100000) >= 45.96] <- "Quartile 3"
paper3$FTE[LHD_2013_m1$c5q37/(LHD_2013_m1$c0population/100000) > 73.49] <- "Quartile 4"

## Performance improvement efforts (< 5 years)
# Community health assessment 
paper3$health_assmt[LHD_2013_m1$c7q147 == "Yes_<3 years"] <- "Yes"
paper3$health_assmt[LHD_2013_m1$c7q147 == "Yes_3-5 years"] <- "Yes"
paper3$health_assmt[LHD_2013_m1$c7q147 == "Yes_5+ years"] <- "No"
paper3$health_assmt[LHD_2013_m1$c7q147 == "No_next year"] <- "No"
paper3$health_assmt[LHD_2013_m1$c7q147 == "No"] <- "No"

# Community health improvement plan (<5 years) 
paper3$health_impplan[LHD_2013_m1$c7q149 == "Yes_<3 years"] <- "Yes"
paper3$health_impplan[LHD_2013_m1$c7q149 == "Yes_3-5 years"] <- "Yes"
paper3$health_impplan[LHD_2013_m1$c7q149 == "Yes_5+ years"] <- "No"
paper3$health_impplan[LHD_2013_m1$c7q149 == "No_next year"] <- "No"
paper3$health_impplan[LHD_2013_m1$c7q149 == "No"] <- "No"

# PHAB accrediation status corret 
paper3$PHAB[LHD_2013_m1$m2q401 == "undecided"] <- "Undicided"
paper3$PHAB[LHD_2013_m1$m2q401 == "not applying"] <- "Decided not to apply"
paper3$PHAB[LHD_2013_m1$m2q401 == "submitted application" | LHD_2013_m1$m2q401 == "SHA will apply for LHD"
           | LHD_2013_m1$m2q401 == "plans to apply, no statement" | LHD_2013_m1$m2q401 == "submitted statement of intent" ] <- "Engaged/intend to engage"

paper3$PHAB<-as.factor(paper3$PHAB)
paper3$PHAB <- relevel(paper3$PHAB, ref="Engaged/intend to engage")

# Use of core competencies 
paper3$core_comp[LHD_2013_m1$m6q204b == "checked"] <- "No"
paper3$core_comp[is.na(LHD_2013_m1$m6q204b) == T] <- NA
paper3$core_comp[LHD_2013_m1$m6q204b == "unchecked"] <- "Yes"

# Use of commmunity guide 
paper3$guide[LHD_2013_m1$c11q301 == "Have not used"|LHD_2013_m1$c11q301 == "Do not know" ] <- "No/do not know" 
paper3$guide[LHD_2013_m1$c11q301 == "Some areas"|LHD_2013_m1$c11q301 == "All relevant areas" ] <- "Consistently/some areas" 
paper3$guide[is.na(LHD_2013_m1$c11q301) == T] <- NA
paper3$guide<-as.factor(paper3$guide)

paper3$guide <- relevel(paper3$guide, ref="No/do not know")

##Collaboration
# Any Partnerships for land use  
paper3$landpartner[LHD_2013_m1$m13q408 == "Not involved"|LHD_2013_m1$m13q408 == "No programs"] <- "No" 
paper3$landpartner[LHD_2013_m1$m13q408 == " Networking"|LHD_2013_m1$m13q408 == " Coordinating"
                  |LHD_2013_m1$m13q408 == "Cooperating"|LHD_2013_m1$m13q408 == " Collaborating" ] <- "Yes" 
paper3$landpartner[is.na(LHD_2013_m1$m13q408) == T] <- NA
paper3$landpartner<-as.factor(paper3$landpartner)

#Cross-jurisdictional sharting 
paper3$cjsharing<- LHD_2013_m1$m8q219 %>% as.factor()

##Obesity/chronic disease policy/advocacy activities
#Community level urban design and land use policies  
paper3$PApolicies[LHD_2013_m1$c12q402a == "checked"] <- "Yes"
paper3$PApolicies[LHD_2013_m1$c12q402a == "unchecked"] <- "No"
paper3$PApolicies[is.na(LHD_2013_m1$c12q402a) == T] <- "No"
paper3$PApolicies<-as.factor(paper3$PApolicies)

#active transportation options 

paper3$transportation[LHD_2013_m1$c12q402b == "checked"] <- "Yes"
paper3$transportation[LHD_2013_m1$c12q402b == "unchecked"] <- "No"
paper3$transportation[is.na(LHD_2013_m1$c12q402b) == T] <- "No"
paper3$transportation<-as.factor(paper3$transportation)

#expanding access to recreational facilities  
paper3$recaccess[LHD_2013_m1$c12q402e == "checked"] <- "Yes"
paper3$recaccess[LHD_2013_m1$c12q402e == "unchecked"] <- "No"
paper3$recaccess[is.na(LHD_2013_m1$c12q402e) == T] <- "No"
paper3$recaccess<-as.factor(paper3$recaccess)

##TABLE 1: STUDY SAMPLE CHARACTERISTICS##

#unweighted %
freq(paper3$population)
freq(paper3$jurisdiction)
freq(paper3$governance)
freq(paper3$local_board)
freq(paper3$FTE)
freq(paper3$health_assmt)
freq(paper3$health_impplan)
freq(paper3$PHAB)
freq(paper3$core_comp)
freq(paper3$guide)
freq(paper3$cjsharing)
freq(paper3$landpartner)
freq(paper3$PApolicies)
freq(paper3$transportation)
freq(paper3$recaccess)

#weighted %
wpct(paper3$population, weight=paper3$weight01)
wpct(paper3$jurisdiction, weight=paper3$weight01)
wpct(paper3$governance, weight=paper3$weight01)
wpct(paper3$local_board, weight=paper3$weight01)
wpct(paper3$FTE, weight=paper3$weight01)
wpct(paper3$health_assmt, weight=paper3$weight01)
wpct(paper3$health_impplan, weight=paper3$weight01)
wpct(paper3$PHAB, weight=paper3$weight01)
wpct(paper3$core_comp, weight=paper3$weight01)
wpct(paper3$guide, weight=paper3$weight01)
wpct(paper3$cjsharing, weight=paper3$weight01)
wpct(paper3$landpartner, weight=paper3$weight01)
wpct(paper3$PApolicies, weight=paper3$weight01)
wpct(paper3$transportation, weight=paper3$weight01)
wpct(paper3$recaccess, weight=paper3$weight01)

##TABLE 3 MULTIVARIATE LOGISTIC REGRESSION##

#Model 1: Community level urban design and land use
model1 <- glm(PApolicies ~ population + region + jurisdiction + governance + local_board + 
                FTE + health_assmt + health_impplan + PHAB + core_comp + guide + cjsharing + landpartner,
              data = paper3, weights = weight01,
              family = "binomial",
              na.action="na.omit")

#Obtain OR, 95% CI and p-value
ORmultivariate(model1)

#Model Fit: % Correctly Predicted       
prob1 <- predict(model1, newdata = paper3, type = "response")
prob1 <- prob1[!is.na(prob1)]
pred1 <- rep("No", length(prob1))
pred1[prob1>0.5] <- "Yes"

table(paper3$PApolicies[as.numeric(names(prob1))], pred1)   #86.9% correctly predicted

#Model 2: Active transportation options
model2 <- glm(transportation ~ population + region + jurisdiction + governance + local_board + 
                FTE + health_assmt + health_impplan + PHAB + core_comp + guide + cjsharing + landpartner,
              data = paper3, weights = weight01,
              family = "binomial",
              na.action="na.omit")

#Obtain OR, 95% CI and p-value
ORmultivariate(model2)

#Model Fit: % Correctly Predicted       
prob2 <- predict(model2, newdata = paper3, type = "response")
prob2 <- prob2[!is.na(prob2)]
pred2 <- rep("No", length(prob2))
pred2[prob2>0.5] <- "Yes"

table(paper3$transportation[as.numeric(names(prob2))], pred2)   #86.9% correctly predicted


#Model 3: Expanding access to recreation facilities 
model3 <- glm(recaccess ~ population + region + jurisdiction + governance + local_board + 
                FTE + health_assmt + health_impplan + PHAB + core_comp + guide + cjsharing + landpartner,
              data = paper3, weights = weight01,
              family = "binomial",
              na.action="na.omit")

#Obtain OR, 95% CI and p-value
ORmultivariate(model3)

#Model Fit: % Correctly Predicted       
prob3 <- predict(model3, newdata = paper3, type = "response")
prob3 <- prob3[!is.na(prob3)]
pred3 <- rep("No", length(prob3))
pred3[prob3>0.5] <- "Yes"

table(paper3$recaccess[as.numeric(names(prob3))], pred3)   #86.9% correctly predicted

#MODELS HAVE VERY NARROW CI COMPARED TO ORIGINAL
#SEEMS LIKE PERHAPS WEIGHTS WERE NOT USED AFTER ALL
#HERE ARE THE MODELS WITHOUT WEIGHTS
#Models 1-3 WITHOUT WEIGHTS 
model1NW <- glm(PApolicies ~ population + region + jurisdiction + governance + local_board + 
                FTE + health_assmt + health_impplan + PHAB + core_comp + guide + cjsharing + landpartner,
              data = paper3,
              family = "binomial",
              na.action="na.omit")
model2NW <- glm(transportation ~ population + region + jurisdiction + governance + local_board + 
                  FTE + health_assmt + health_impplan + PHAB + core_comp + guide + cjsharing + landpartner,
                data = paper3,
                family = "binomial",
                na.action="na.omit")
model3NW <- glm(recaccess ~ population + region + jurisdiction + governance + local_board + 
                FTE + health_assmt + health_impplan + PHAB + core_comp + guide + cjsharing + landpartner,
              data = paper3,
              family = "binomial",
              na.action="na.omit")
ORmultivariate(model1NW)
ORmultivariate(model2NW)
ORmultivariate(model3NW)

##FORESTPLOT OF ORIGINAL VS REPRODUCED##
library(ggplot2)
dat2<-read.csv(file.choose())
dat2$paper3IV <- factor(dat2$paper3IV, levels=c('Partnerships for land use (ref=no)',
                                              'Cross-jurisdictional sharing (ref=no)',
                                              'Use of community guide (ref=no)',
                                              'Use of core competencies (ref=no)',
                                              'Undecided',
                                              'Decided not to apply',
                                              'PHAB accreditation status: Engaged intend to engage (ref)',
                                              'Community health improvement plan (<5 years): Yes (ref=no)',
                                              'Community health assessment (<5 years): Yes (ref=no)',
                                              'Quartile 4',
                                              'Quartile 3',
                                              'Quartile 2',
                                              'FTE per 100,000 population: Quartile 1 (ref)',
                                              'Local BOH (ref=no)',
                                              'Shared',
                                              'Local',
                                              'Governance type: State (ref)',
                                              'Other',
                                              'County',
                                              'Jurisdiction: City (ref)',
                                              'West',
                                              'South',
                                              'Midwest',
                                              'Geographic region: Northeast (ref)',
                                              '500,000+',
                                              '50,000 - 499,999',
                                              'Population: <50,000 (ref)'))

#Model 1 original with reproduced
dat3 <-subset(dat2, !(is.na(dat2$paper3OR1)))
orspaper31<-ggplot(dat3, aes(x = paper3IV, y = paper3OR1,  ymin = paper3LCI1, ymax = paper3UCI1)) +
  geom_pointrange(aes(col=factor(dat3$paper3REP1)),position=position_dodge(width=.50)) +
  ylab("Odds ratio & 95% CI") + geom_hline(aes(yintercept = 1)) +
  scale_y_log10() +
  xlab("Local Health Department Characteristic") + scale_color_manual(values=c("black","gray70","gray40"),
                                                                      name="")+
  guides(fill=guide_legend(ncol=2))+
  theme(legend.position="top")+
  coord_flip()
orspaper31

#Model 2 with original, reproduced, and reproduced without weights
dat4 <-subset(dat2, !(is.na(dat2$paper3OR2)))
orspaper32<-ggplot(dat4, aes(x = paper3IV, y = paper3OR2,  ymin = paper3LCI2, ymax = paper3UCI2)) +
  geom_pointrange(aes(col=factor(dat4$paper3REP2)), position=position_dodge(width=.50)) +
  ylab("Odds ratio & 95% CI") + geom_hline(aes(yintercept = 1)) +
  scale_y_log10() +
  xlab("Local Health Department Characteristic") + scale_color_manual(values=c("black","gray60","gray30"),
                                                                      name="")+
  theme(legend.position="top")+
  coord_flip()
orspaper32

#Model 3 with original, reproduced, and reproduced without weights
dat5 <-subset(dat2, !(is.na(dat2$paper3OR3)))
orspaper33a<-ggplot(dat4, aes(x = paper3IV, y = paper3OR3,  ymin = paper3LCI3, ymax = paper3UCI3)) +
  geom_pointrange(aes(col=factor(dat5$paper3REP3)), position=position_dodge(width=.50)) +
  ylab("Odds ratio & 95% CI") + geom_hline(aes(yintercept = 1)) +
  scale_y_log10() +
  xlab("Local Health Department Characteristic") + scale_color_manual(values=c("black","gray60","gray30"),
                                                                      name="")+
  theme(legend.position="top", plot.title = element_text(hjust = 0.5))+
  coord_flip()
orspaper33a

#Model 3 separate for original vs. reproduced and original vs. reproduced without weights
dat5 <-subset(dat2, !(is.na(dat2$paper3OR3)))
dat6 <-subset(dat5, dat5$paper3REP3!="ReproducedWithoutWeights")
orspaper33<-ggplot(dat6, aes(x = paper3IV, width=20, y = paper3OR3,  ymin = paper3LCI3, ymax = paper3UCI3)) +
  geom_pointrange(aes(col=factor(dat6$paper3REP3)),size=.75, position=position_dodge(width=.50)) +
  ylab("Odds ratio & 95% CI") + geom_hline(aes(yintercept = 1)) +
  scale_y_log10() +
  xlab("Local Health Department Characteristic") + scale_color_manual(values=c("black","gray70"),
                                                                      name="")+
  theme(legend.position="top", plot.title = element_text(hjust = 0.5))+
  coord_flip()
orspaper33
dat7 <-subset(dat2, dat2$paper3REP3!="Reproduced")
orspaper33b<-ggplot(dat7, aes(x = paper3IV, width=20, y = paper3OR3,  ymin = paper3LCI3, ymax = paper3UCI3)) +
  geom_pointrange(aes(col=factor(dat7$paper3REP3)),size=.75, position=position_dodge(width=.50)) +
  ylab("Odds ratio & 95% CI") + geom_hline(aes(yintercept = 1)) +
  scale_y_log10() +
  xlab("Local Health Department Characteristic") + scale_color_manual(values=c("black","gray70"),
                                                                      name="")+
  theme(legend.position="top", plot.title = element_text(hjust = 0.5))+
  coord_flip()
orspaper33b


