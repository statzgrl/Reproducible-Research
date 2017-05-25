############### PAPER 5/5 ###################

##DATA SOURCES##

#NACCHO data obtained from: http://nacchoprofilestudy.org/data-requests/, "2013 Profile Study"

##CODE INFORMATION##

#Written by: Sarah Wondmeneh and Yiqiang Zhao#
#Github Link: https://github.com/statzgrl/Reproducible-Research/blob/master/Paper5.R

############################################

###CREATE DATASET###

##Open Packages##

library(foreign)
library(car)
library(magrittr)
library(mice)
library(PredictABEL)
library(descr)

#import 2013 NACCHO Data
profile2013 <- read.spss(file.choose(),to.data.frame = T)

#subset into core variables only
core <- grepl('^c',names(profile2013), ignore.case = T)
core[1] <- T
profile2013_core <- profile2013[,core]

##INPUT VARIABLES##

paper5 <- NA

#population 
paper5$population <- recode(profile2013_core$c0population,
                        "NA = NA;                       
                        1:24999 = '<25000';
                        25000:49999 = '25000-49999';
                        50000:99999 = '50000-99999';
                        100000:499999 = '100000-499999';
                        else = '500000+'")
paper5[1] <- NULL

# governance
paper5$governance_type <- profile2013_core$c0govcat

# budget cut
paper5$budget <- recode(profile2013_core$c10q301,"c('less than')='with budget cuts';c('greater than','same')='without budget cuts';else=NA")

# BOH with authority related variable
# BOH_0 to BOH_8 represent Local BOH characteristics in the table
paper5$BOH_0 <- profile2013_core$c2q301
paper5$BOH_1 <- profile2013_core$c2q6a
paper5$BOH_2 <- profile2013_core$c2q7a
paper5$BOH_3 <- profile2013_core$c2q8a
paper5$BOH_4 <- profile2013_core$c2q9a
paper5$BOH_5 <- profile2013_core$c2q10a
paper5$BOH_6 <- profile2013_core$c2q11a
paper5$BOH_7 <- profile2013_core$c2q14a
paper5$BOH_8 <- profile2013_core$c2q15a
paper5$BOH_9 <- ifelse((profile2013_core$c6q75a=='checked'|
                      profile2013_core$c6q76a=='checked'|
                      profile2013_core$c6q77a=='checked'|
                      profile2013_core$c6q78a=='checked'|
                      profile2013_core$c6q79a=='checked')==T,'checked','unchecked')

# total expenditures
paper5$total_exp <- recode(profile2013_core$c3q15, 
                       "NA=NA;
                       1:499999='<$500000';
                       500000:999999='$500000-$999999';
                       1000000:4999999='$1000000-$4999999';
                       5000000:9999999='$5000000-$9999999';
                       else = '$10000000+'")

# expenditures per capita
paper5$per_capita_exp <- profile2013_core$c3q15/profile2013_core$c0population
paper5$per_capita_exp_cat[paper5$per_capita_exp<20] <- "<$20"
paper5$per_capita_exp_cat[paper5$per_capita_exp<=34.99&paper5$per_capita_exp>=20] <- "$20-$34.99"
paper5$per_capita_exp_cat[paper5$per_capita_exp<=44.99&paper5$per_capita_exp>=35] <- "$35-$44.99"
paper5$per_capita_exp_cat[paper5$per_capita_exp<=54.99&paper5$per_capita_exp>=45] <- "$45-$54.99"
paper5$per_capita_exp_cat[paper5$per_capita_exp>=55] <- '$55+'

# mean% revenues 
paper5$localrev <- profile2013_core$c3q17p/profile2013_core$c3q16 * 100
paper5$Medicaidrev<-profile2013_core$c3q17r/profile2013_core$c3q16 * 100
paper5$Federalrev<-profile2013_core$c3q17qe/profile2013_core$c3q16 * 100

# weight variables
paper5$weight01 <- profile2013_core$c0coreweight_s
paper5$weight02 <- profile2013_core$c0coreweight_p

#set as data frame
paper5 <- as.data.frame(paper5)

#subset data to exclude NAs, n=1874#
paper5_1<-subset(paper5, 
             !(is.na(paper5$budget)|
                 is.na(paper5$governance_type)|
                 is.na(paper5$BOH_0)))

#relevel variables as appropriate

paper5_1$budget <- relevel(paper5_1$budget, ref="without budget cuts")

paper5_1$governance_type <- factor(paper5_1$governance_type, levels = c("state","local"," shared"))

paper5_1$population <- factor(paper5_1$population, levels = c("<25000","25000-49999","50000-99999","100000-499999","500000+"))
paper5_1$population <- relevel(paper5_1$population, ref="<25000")

paper5_1$BOH_0 <- factor(paper5_1$BOH_0, levels = c("no","yes"))
paper5_1$BOH_1 <- factor(paper5_1$BOH_1, levels = c("unchecked","checked"))
paper5_1$BOH_2 <- factor(paper5_1$BOH_2, levels = c("unchecked","checked"))
paper5_1$BOH_3 <- factor(paper5_1$BOH_3, levels = c("unchecked","checked"))
paper5_1$BOH_4 <- factor(paper5_1$BOH_4, levels = c("unchecked","checked"))
paper5_1$BOH_5 <- factor(paper5_1$BOH_5, levels = c("unchecked","checked"))
paper5_1$BOH_6 <- factor(paper5_1$BOH_6, levels = c("unchecked","checked"))
paper5_1$BOH_7 <- factor(paper5_1$BOH_7, levels = c("unchecked","checked"))
paper5_1$BOH_8 <- factor(paper5_1$BOH_8, levels = c("unchecked","checked"))
paper5_1$BOH_9 <- factor(paper5_1$BOH_9, levels = c("unchecked","checked"))

paper5_1$BOH_1[is.na(paper5_1$BOH_1)] <- "unchecked"
paper5_1$BOH_2[is.na(paper5_1$BOH_2)] <- "unchecked"
paper5_1$BOH_3[is.na(paper5_1$BOH_3)] <- "unchecked"
paper5_1$BOH_4[is.na(paper5_1$BOH_4)] <- "unchecked"
paper5_1$BOH_5[is.na(paper5_1$BOH_5)] <- "unchecked"
paper5_1$BOH_6[is.na(paper5_1$BOH_6)] <- "unchecked"
paper5_1$BOH_7[is.na(paper5_1$BOH_7)] <- "unchecked"
paper5_1$BOH_8[is.na(paper5_1$BOH_8)] <- "unchecked"
paper5_1$BOH_9[is.na(paper5_1$BOH_9)] <- "unchecked"

paper5_1$total_exp <- factor(paper5_1$total_exp, levels = c("<$500000","$500000-$999999","$1000000-$4999999","$5000000-$9999999","$10000000+"))
paper5_1$total_exp <- relevel(paper5_1$total_exp, ref="<$500000")

paper5_1$per_capita_exp_cat <- factor(paper5_1$per_capita_exp_cat, levels = c("<$20","$20-$34.99","$35-$44.99","$45-$54.99","$55+"))
paper5_1$per_capita_exp_cat <- relevel(paper5_1$per_capita_exp_cat, ref="<$20")

#############################################

##TABLE 3 DESCRIPTIVE STATISTICS##

CrossTable(paper5_1$population, paper5_1$budget, prop.c = T, prop.r = F, prop.t = F,  prop.chisq = F)
CrossTable(paper5_1$governance_type, paper5_1$budget, prop.c = T, prop.r = F, prop.t = F,  prop.chisq = F)
CrossTable(paper5_1$BOH_0, paper5_1$budget, prop.c = T, prop.r = F, prop.t = F,  prop.chisq = F)
CrossTable(paper5_1$BOH_1, paper5_1$budget, prop.c = T, prop.r = F, prop.t = F,  prop.chisq = F)
CrossTable(paper5_1$BOH_2, paper5_1$budget, prop.c = T, prop.r = F, prop.t = F,  prop.chisq = F)
CrossTable(paper5_1$BOH_3, paper5_1$budget, prop.c = T, prop.r = F, prop.t = F,  prop.chisq = F)
CrossTable(paper5_1$BOH_4, paper5_1$budget, prop.c = T, prop.r = F, prop.t = F,  prop.chisq = F)
CrossTable(paper5_1$BOH_5, paper5_1$budget, prop.c = T, prop.r = F, prop.t = F,  prop.chisq = F)
CrossTable(paper5_1$BOH_6, paper5_1$budget, prop.c = T, prop.r = F, prop.t = F,  prop.chisq = F)
CrossTable(paper5_1$BOH_7, paper5_1$budget, prop.c = T, prop.r = F, prop.t = F,  prop.chisq = F)
CrossTable(paper5_1$BOH_8, paper5_1$budget, prop.c = T, prop.r = F, prop.t = F,  prop.chisq = F)
CrossTable(paper5_1$BOH_9, paper5_1$budget, prop.c = T, prop.r = F, prop.t = F,  prop.chisq = F)
CrossTable(paper5_1$total_exp, paper5_1$budget, prop.c = T, prop.r = F, prop.t = F,  prop.chisq = F)
CrossTable(paper5_1$per_capita_exp_cat, paper5_1$budget, prop.c = T, prop.r = F, prop.t = F,  prop.chisq = F)
by(paper5_1$localrev, paper5_1$budget, mean, na.rm = T)
by(paper5_1$Medicaidrev, paper5_1$budget, mean, na.rm = T)
by(paper5_1$Federalrev, paper5_1$budget, mean, na.rm = T)

#############################################

##TABLE 4 LOGISTIC REGRESSION: MODEL 2##

#Run regression
model2 <- glm(budget ~ population + governance_type + BOH_0 + BOH_1 + BOH_2 + BOH_3 + BOH_4 + BOH_5 + BOH_6 + BOH_7 + BOH_8 + BOH_9 +
                per_capita_exp_cat + total_exp + localrev + Medicaidrev + Federalrev,
              data = paper5_1, weights = weight02,
              family = "binomial",
              na.action="na.omit")

#Obtain OR, 95% CI and p-value
ORmultivariate(model2)

#Model Fit: % Correctly Predicted       
prob1 <- predict(model2, newdata = paper5, type = "response")
prob1 <- prob1[!is.na(prob1)]
pred1 <- rep("No", length(prob1))
pred1[prob1>0.5] <- "yes"

table(paper5$budget[as.numeric(names(prob1))], pred1) 

length(pred1)
length(paper5$budget[as.numeric(names(prob1))])  #28.9% correctly predicted 

##FORESTPLOT OF ORIGINAL VS REPRODUCED##
library(ggplot2)
dat2<-read.csv(file.choose())

#subset data to include only observations with odds ratios
#for paper5 study
dat2 <-subset(dat2, !(is.na(dat2$paper5OR2)))

#re-order factor levels to be the same as manuscript
dat2$paper5IV <- factor(dat2$paper5IV, levels=c('Federal',
                                        'Medicaid and Medicare',
                                        'Mean % of revenues from major sources: Local',
                                        '$55+',
                                        '$45 - 54.99',
                                        '$35-44.99',
                                        '$20 -34.99',
                                        'Expenditures per capita: <$20 (ref)',
                                        '$10,000,000+',
                                        '$5,000,000 - 9,999,999',
                                        '$1,000,000 - $4,999,999',
                                        '$500,000 - 999,999',
                                        'Total expenditures: <$500,000 (ref)',
                                        'Providing clinical services',
                                        'BOH with authority to set policies, goals and priorities',
                                        'BOH with authority to advise the LHD or elected officials',
                                        'BOH with authority to request PH levy',
                                        'BOH with authority to impose taxes for PH',
                                        'BOH with authority to set and impose fees',
                                        'BOH with authority to adopt PH regulations',
                                        'BOH with authority to approve LHD budget',
                                        'BOH with authority to hire or fire agency head',
                                        'Local BOH: Having a local BOH',
                                        'Shared governance',
                                        'Local governance',
                                        'Governance type: State governance (ref)',
                                        '500,000+',
                                        '100,000 - 499,999',
                                        '50,000 - 99,999',
                                        '25,000 - 49,999',
                                        'Population: <25,000 (ref)'))

#plot original and reproduced OR and 95% CI
orspaper52<-ggplot(dat2, aes(x = paper5IV, y = paper5OR2,  ymin = paper5LCI2, ymax = paper5UCI2)) +
  geom_pointrange(aes(col=factor(dat2$paper5REP2)), size=.75, position=position_dodge(width=.50)) +
  ylab("Odds ratio & 95% CI") + geom_hline(aes(yintercept = 1)) +
  scale_y_log10() +
  xlab("Local Health Department Characteristic") + scale_color_manual(values=c("black","gray70"),
                                                                      labels=c("Original", "Reproduced"),
                                                                      name="")+
  theme(legend.position="top", plot.title = element_text(hjust = 0.5))+
  coord_flip()
orspaper52

#subset for Model 1 results only
#drop unneeded factor levels so they don't appear on graph
dat2 <-subset(dat2, !(is.na(dat2$paper5OR)))
dat2$paper5IV<-droplevels(dat2$paper5IV)

#plot original and reproduced OR and 95% CI
orspaper5<-ggplot(dat2, aes(x = paper5IV, y = paper5OR,  ymin = paper5LCI, ymax = paper5UCI)) +
  geom_pointrange(aes(col=factor(dat2$paper5REP)), size=.75, position=position_dodge(width=.50)) +
  ylab("Odds ratio & 95% CI") + geom_hline(aes(yintercept = 1)) +
  scale_y_log10() +
  xlab("Local Health Department Characteristic") + scale_color_manual(values=c("black","gray70"),
                                                                      name="")+
  theme(legend.position="top", plot.title = element_text(hjust = 0.5))+
  coord_flip()
orspaper5
