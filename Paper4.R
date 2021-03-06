############### PAPER 4/5 ###################

##DATA SOURCES##

#NACCHO data obtained from: http://nacchoprofilestudy.org/data-requests/, "2013 Profile Study"

##CODE INFORMATION##

#Written by: Sarah Wondmeneh and Yiqiang Zhao#
#Github Link: https://github.com/statzgrl/Reproducible-Research/blob/master/Paper4.R

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
LHD_2013 <- read.spss(file.choose(), use.value.labels = TRUE)
LHD_2013 <- as.data.frame(LHD_2013)

#select Module 2 only 
LHD_2013 <- LHD_2013[LHD_2013$c0module == "Core + Module 2" & is.na(LHD_2013$c0module) == F, ]

##INPUT VARIABLES##

#Outcome Variable
paper4 <- NULL
paper4$healthcare[LHD_2013$m14q401c == "unchecked" & LHD_2013$m14q304c == "unchecked"] <- "No" 
paper4$healthcare[LHD_2013$m14q401c == "checked" | LHD_2013$m14q304c == "checked"] <- "Yes" 
paper4$healthcare <- as.factor(paper4$healthcare)
paper4$healthcare <- relevel(paper4$healthcare, ref="No")

paper4 <- as.data.frame(paper4)

#weight

paper4$weight<-LHD_2013$c0moduleweight2

# Jurisdiction population
paper4$population <- recode(LHD_2013$c0population,"
                          1:49999 = '<50000';
                          50000:499999 = '50000-499999';
                          else = '500000+'") %>% as.factor()

# Jurisdiction type
paper4$jurisdiction <- as.factor(LHD_2013$c0jurisdiction)
paper4$jurisdiction <- recode(paper4$jurisdiction,"
                            'city-county ' = 'City-county/multicounty';
                            'city        ' = 'City/multicity';
                            'county      ' = 'County';
                            'multi-city  ' = 'City/multicity';
                            'multi-county' = 'City-county/multicounty'") %>% as.factor
paper4$jurisdiction <- relevel(paper4$jurisdiction, ref = "County")

# Centralised governance
paper4$governance <- recode(LHD_2013$c0govcat, "
                          'state' = 'centralised';
                          'local' = 'non-centralised';
                          ' shared' = 'non-centralised'") %>% as.factor
paper4$governance <- relevel(paper4$governance, ref = "non-centralised")

# FTEs per 10,000 people
paper4$FTE <- LHD_2013$c5q37/(LHD_2013$c0population/10000)

#Per capita expenditure 
paper4$PerCapitaExp <- LHD_2013$c3q15/LHD_2013$c0population
paper4$PerCapitaExplog <- log(paper4$PerCapitaExp)

#full time director 
paper4$FullTimeDirector <- recode(LHD_2013$c4q302, "
                                'full-time' = 'Yes';
                                'part-time' = 'No'") %>% as.factor

# paper4$tenure <- LHD_2013$c4q24
spss2date <- function(x) as.Date(x/86400, origin = "1582-10-14")
paper4$startdate <- spss2date(LHD_2013$c4q24)
paper4$startdate <-as.Date(paper4$startdate)

paper4$enddate<-rep("2013-04-01", length(505))
paper4$enddate <-as.Date(paper4$enddate)
paper4$tenure <- as.numeric(difftime(paper4$enddate, paper4$startdate, units="days"))
paper4$tenure <-paper4$tenure/365
paper4$tenure<-paper4$tenure-0.28

# With local board of health
paper4$LocalBoard <- recode(LHD_2013$c2q301,"
                          'no' = 'No';
                          'yes' = 'Yes'") %>% as.factor

# Completed community health assessment in the past three years
paper4$HealthAssess <- recode(LHD_2013$c7q147,"
                            'Yes_<3 years' = 'Yes';
                            'Yes_3-5 years' = 'No';
                            'Yes_5+ years' = 'No';
                            'No_next year' = 'No';
                            'No' = 'No'")

# Geographic location
paper4$region <- recode(LHD_2013$c0state,"
                      'NY' = 'Northeast';'PA' = 'Northeast';'NJ' = 'Northeast';'ME' = 'Northeast';
                      'VT' = 'Northeast';'NH' = 'Northeast';'MA' = 'Northeast';'CT' = 'Northeast';'MA' = 'Northeast';
                      'ND' = 'Midwest';'SD' = 'Midwest';'NE' = 'Midwest';'KS' = 'Midwest';'MN' = 'Midwest';
                      'IA' = 'Midwest';'MO' = 'Midwest';'WI' = 'Midwest';'IL' = 'Midwest';'IN' = 'Midwest';
                      'MI' = 'Midwest';'OH' = 'Midwest';
                      'WA' = 'West';'OR' = 'West';'HI' = 'West';'CA' = 'West';'AK' = 'West';'AZ' = 'West';
                      'CO' = 'West';'ID' = 'West';'NM' = 'West';'MT' = 'West';'UT' = 'West';'NV' = 'West';'WY' = 'West';
                      else = 'South'") %>% as.factor()

# Disparity total socre
LHD_2013$m18q146a[is.na(LHD_2013$m18q146a) == T] <- "unchecked"
LHD_2013$m18q146b[is.na(LHD_2013$m18q146b) == T] <- "unchecked"
LHD_2013$m18q146c[is.na(LHD_2013$m18q146c) == T] <- "unchecked"
LHD_2013$m18q146d[is.na(LHD_2013$m18q146d) == T] <- "unchecked"
LHD_2013$m18q146e[is.na(LHD_2013$m18q146e) == T] <- "unchecked"
LHD_2013$m18q146f[is.na(LHD_2013$m18q146f) == T] <- "unchecked"
LHD_2013$m18q146g[is.na(LHD_2013$m18q146g) == T] <- "unchecked"
LHD_2013$m18q146h[is.na(LHD_2013$m18q146h) == T] <- "unchecked"
LHD_2013$m18q146j[is.na(LHD_2013$m18q146j) == T] <- "unchecked"
LHD_2013$m18q146i[is.na(LHD_2013$m18q146i) == T] <- "unchecked"

paper4$disparity_a <- ifelse(LHD_2013$m18q146a == "checked", 1, 0)
paper4$disparity_b <- ifelse(LHD_2013$m18q146b == "checked", 1, 0)
paper4$disparity_c <- ifelse(LHD_2013$m18q146c == "checked", 1, 0)
paper4$disparity_d <- ifelse(LHD_2013$m18q146d == "checked", 1, 0)
paper4$disparity_e <- ifelse(LHD_2013$m18q146e == "checked", 1, 0)
paper4$disparity_f <- ifelse(LHD_2013$m18q146f == "checked", 1, 0)
paper4$disparity_g <- ifelse(LHD_2013$m18q146g == "checked", 1, 0)
paper4$disparity_h <- ifelse(LHD_2013$m18q146h == "checked", 1, 0)
paper4$disparity_j <- ifelse(LHD_2013$m18q146j == "checked", 1, 0)
paper4$disparity_i <- ifelse(LHD_2013$m18q146i == "checked", 1, 0)

paper4$disparity <- with(paper4, disparity_a + disparity_b + disparity_c + disparity_d + disparity_e + disparity_f + disparity_g + disparity_h + disparity_j)

############################################

##TABLE 1: LHD CHARACTERISTICS##

crosstab(paper4$healthcare, paper4$population, weight=paper4$weight, prop.c = T)
crosstab(paper4$healthcare, paper4$jurisdiction, weight=paper4$weight, prop.c = T)
crosstab(paper4$healthcare, paper4$governance, weight=paper4$weight, prop.c = T)
crosstab(paper4$healthcare, paper4$LocalBoard, weight=paper4$weight, prop.c = T)
crosstab(paper4$healthcare, paper4$HealthAssess, weight=paper4$weight, prop.c = T)
crosstab(paper4$healthcare, paper4$region, weight=paper4$weight, prop.c = T)
crosstab(paper4$healthcare, paper4$FullTimeDirector, weight=paper4$weight, prop.c = T)

weighted.mean(paper4$FTE[paper4$healthcare=="No"], paper4$weight[paper4$healthcare=="No"], na.rm=T)
weighted.mean(paper4$FTE[paper4$healthcare=="Yes"], paper4$weight[paper4$healthcare=="Yes"], na.rm=T)

weighted.mean(paper4$PerCapitaExp[paper4$healthcare=="No"], paper4$weight[paper4$healthcare=="No"], na.rm=T)
weighted.mean(paper4$PerCapitaExp[paper4$healthcare=="Yes"], paper4$weight[paper4$healthcare=="Yes"], na.rm=T)

weighted.mean(paper4$tenure[paper4$healthcare=="No"], paper4$weight[paper4$healthcare=="No"], na.rm=T)
weighted.mean(paper4$tenure[paper4$healthcare=="Yes"], paper4$weight[paper4$healthcare=="Yes"], na.rm=T)

weighted.mean(paper4$disparity[paper4$healthcare=="No"], paper4$weight[paper4$healthcare=="No"], na.rm=T)
weighted.mean(paper4$disparity[paper4$healthcare=="Yes"], paper4$weight[paper4$healthcare=="Yes"], na.rm=T)

############################################

##TABLE 2: LOGISTIC REGRESSION##

model <- glm(healthcare ~ population + jurisdiction + governance + FTE + PerCapitaExplog + FullTimeDirector + tenure + LocalBoard + HealthAssess + region + disparity, 
             data = paper4,
             family = "binomial",
             na.action="na.omit")

#Obtain OR, 95% CI and p-value
ORmultivariate(model)

#Model Fit: % Correctly Predicted       
prob1 <- predict(model, newdata = paper4, type = "response")
prob1 <- prob1[!is.na(prob1)]
pred1 <- rep("No", length(prob1))
pred1[prob1>0.5] <- "Yes"

table(paper4$healthcare[as.numeric(names(prob1))], pred1) #77.0% correctly predicted

##FORESTPLOT OF ORIGINAL VS REPRODUCED##

dat2<-read.csv(file.choose())

#subset data to include only observations with odds ratios
#for paper4 study
dat2 <-subset(dat2, !(is.na(dat2$paper4OR)))

#re-order factor levels to be the same as manuscript
dat2$paper4IV <- factor(dat2$paper4IV, levels=c('Disparity total score',
                                            'West',
                                            'South',
                                            'NE',
                                            'Geographic location (Midwest ref)',
                                            'Completed CHA in the past 3 years',
                                            'With local board of health',
                                            'Director tenure',
                                            'Full time director',
                                            'Per capita expenditure (log)($)',
                                            'FTEs per 10,000 people',
                                            'Centralised governance',
                                            'City/multicity',
                                            'City-county/multicounty ',
                                            'Jurisdiction type (county ref)',
                                            '500,000+',
                                            '50,000 - 499,999',
                                            'Jurisdiction population (<50,000 ref)'))

library(ggplot2)
orspaper4<-ggplot(dat2, aes(x = paper4IV, y = paper4OR,  ymin = paper4LCI, ymax = paper4UCI)) +
  geom_pointrange(aes(col=factor(dat2$paper4REP)), size=.75, position=position_dodge(width=.50)) +
  ylab("Odds ratio & 95% CI") + geom_hline(aes(yintercept = 1)) +
  scale_y_log10() +
  xlab("Local Health Department Characteristic") + scale_color_manual(values=c("black","gray70"),
                                                                      name="")+
  theme(legend.position="top", plot.title = element_text(hjust = 0.5))+
  coord_flip()
orspaper4
