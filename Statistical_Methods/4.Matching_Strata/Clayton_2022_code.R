
##Replication code for Clayton, de Kadt, Dumas, "Daughters do not affect political beliefs in a new democracy." 

rm(list=ls())
set.seed(1234)

library(AER)
library(texreg)
library(ggplot2)
library(MASS)
library(equivalence)

# load the data
#setwd("/Users/amandaclayton/Dropbox/South_Africa_daughtereffects/code")
setwd("/Users/claytoa/Dropbox/South_Africa_daughtereffects/code")

sas <- read.csv('final_sa_data.csv')
sas <- subset(sas, !(is.na(sas$age)))

sas08 <- read.csv('final_sa08_data.csv')

##Figure 1 

sas08$num_agree_gender_binary <- ifelse(sas08$num_agree_gender >= 4, 1, 0) # 6 questions, those who score 4, 5, or 6 receive a one, those who score 0, 1, 2, 3 receive a 0. 

sums.daughter <- c(mean(sas[sas$age <= 35,]$anc_support[which(sas[sas$age <= 35,]$first_daughter==1)], na.rm = T),
                   mean(sas08[sas08 $age <= 35,]$num_agree_gender_binary[which(sas08[sas08$age <= 35,]$first_daughter==1)], na.rm = T),
                   mean(sas[sas$age <= 35,]$abortion_income_binary[which(sas[sas$age <= 35,]$first_daughter==1)]),
                   mean(sas[sas$age <= 35,]$abortion_defect_binary[which(sas[sas$age <= 35,]$first_daughter==1)]),
                   mean(sas[sas$age <= 35,]$pref_hiring_binary[which(sas[sas$age <= 35,]$first_daughter==1)]))

sums.son <- c(mean(sas[sas$age <= 35,]$anc_support[which(sas[sas$age <= 35,]$first_daughter==0)], na.rm = T),
              mean(sas08[sas08$age <= 35,]$num_agree_gender_binary[which(sas08[sas08$age <= 35,]$first_daughter==0)], na.rm = T),
              mean(sas[sas$age <= 35,]$abortion_income_binary[which(sas[sas$age <= 35,]$first_daughter==0)]),
              mean(sas[sas$age <= 35,]$abortion_defect_binary[which(sas[sas$age <= 35,]$first_daughter==0)]),
              mean(sas[sas$age <= 35,]$pref_hiring_binary[which(sas[sas$age <= 35,]$first_daughter==0)]))

#basic t-tests 

t.test(anc_support ~ first_daughter, data = sas[sas$age <= 35,]) #SE of difference: 0.01574901
t.test(num_agree_gender_binary ~ first_daughter, data = sas08[sas08$age <= 35,]) #SE of difference: 0.04334639
t.test(abortion_income_binary ~ first_daughter, data = sas[sas$age <= 35,])  # SE of difference: 0.01124662
t.test(abortion_defect_binary ~ first_daughter, data = sas[sas$age <= 35,])  # SE of difference: 0.01492583
t.test(pref_hiring_binary ~ first_daughter, data = sas[sas$age <= 35,])  #SE of difference: 0.01449723

par(mar = c(5.1, 8.8, 4.1, 2.3))

barplot(rbind(sums.daughter, sums.son), beside = T, horiz = T ,
        xlim = c(0, 1),
        names.arg = c('ANC support', 'Gender scale',
                      'Abortion low-income', 'Abortion birth defect',
                      'Preferential hiring'), las = 2,
        col = c('gray65', 'gray15'),
        main = 'Gender Attitudes by Sex of First Child')

legend(x = .7, y = 4, legend = c('Son', 'Daughter'),
       fill = c('gray15', 'gray65'), cex = 1)

#Add error bars 
segments(x0 = sums.daughter[1] - 0.01574901, y0 = 1.5, x1 = sums.daughter[1] + 0.01574901, y1 = 1.5)            
segments(x0 = sums.son[1] - 0.01574901, y0 = 2.5, x1 = sums.son[1] + 0.01574901, y1 = 2.5, col="grey")            

segments(x0 = sums.daughter[2] - 0.04334639, y0 = 4.5, x1 = sums.daughter[2] + 0.04334639, y1 = 4.5)            
segments(x0 = sums.son[2] - 0.04334639, y0 = 5.5, x1 = sums.son[2] + 0.04334639, y1 = 5.5, col="grey")    

segments(x0 = sums.daughter[3] - 0.01124662, y0 = 7.5, x1 = sums.daughter[3] + 0.01124662, y1 = 7.5)            
segments(x0 = sums.son[3] - 0.01124662, y0 = 8.5, x1 = sums.son[3] + 0.01124662, y1 = 8.5, col="grey")    

segments(x0 = sums.daughter[4] - 0.01492583, y0 = 10.5, x1 = sums.daughter[4] + 0.01492583, y1 = 10.5)            
segments(x0 = sums.son[4] - 0.01492583, y0 = 11.5, x1 = sums.son[4] + 0.01492583, y1 = 11.5, col="grey")    

segments(x0 = sums.daughter[5] - 0.01449723, y0 = 13.5, x1 = sums.daughter[5] + 0.01449723, y1 = 13.5)            
segments(x0 = sums.son[5] - 0.01449723, y0 = 14.5, x1 = sums.son[5] + 0.01449723, y1 = 14.5, col="grey")    

### TABLE 2 ###

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter + survey, sas[sas$age <= 35,])
pref.women.lm.cov <- lm(pref_hiring_binary ~ first_daughter + black + 
                          coloured + indian + age + sex + urban + survey, sas[sas$age <= 35,])

abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, sas[sas$age <= 35,])
abortion.defect.lm.cov <- lm(abortion_defect_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, sas[sas$age <= 35,])

abortion.income.lm <- lm(abortion_income_binary ~ first_daughter + survey, sas[sas$age <= 35,])
abortion.income.lm.cov <- lm(abortion_income_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, sas[sas$age <= 35,])

anc.lm <- lm(anc_support ~ first_daughter + survey, sas[sas$age <= 35,])
anc.lm.cov <- lm(anc_support ~ first_daughter + black + 
                   coloured + indian + age + sex + urban + survey, sas[sas$age <= 35,] )

gender.scale.lm <- (lm(num_agree_stand ~ first_daughter , sas08[sas08$age <= 35,] ))

gender.scale.lm.cov <- (lm(num_agree_stand ~ first_daughter + age + 
                             black + indian + coloured + sex + urban, sas08[sas08$age <= 35,] ))


screenreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)

texreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)


##SI analyses 

##SI C - balance diagnostics 
##Figure C.2 

sas$older <- ifelse(sas$age >= 35, 1, 0)


sas.treat <- subset(sas, sas$first_daughter == 1)
sas.control <- subset(sas, sas$first_daughter == 0)

epsilon1 <- .10
tost1 <- tost(sas.treat$sex, 
              sas.control$sex, 
              alpha = 0.05, epsilon = epsilon1)
est1 <- tost1$estimate[1] - tost1$estimate[2]
interval1 <- tost1$tost.interval

epsilon2 <- .10
tost2 <- tost(sas.treat$black, 
              sas.control$black, 
              alpha = 0.05, epsilon = epsilon2)
est2 <- tost2$estimate[1] - tost2$estimate[2]
interval2 <- tost2$tost.interval

epsilon3 <- .10
tost3 <- tost(sas.treat$coloured, 
              sas.control$coloured, 
              alpha = 0.05, epsilon = epsilon3)
est3 <- tost3$estimate[1] - tost3$estimate[2]
interval3 <- tost3$tost.interval

epsilon4 <- .10
tost4 <- tost(sas.treat$white, 
              sas.control$white,  
              alpha = 0.05, epsilon=epsilon4)
est4 <- tost4$estimate[1] - tost4$estimate[2]
interval4 <- tost4$tost.interval


epsilon5 <- .10
tost5 <- tost(sas.treat$older, 
              sas.control$older,  
              alpha = 0.05, epsilon=epsilon5)
est5 <- tost5$estimate[1] - tost5$estimate[2]
interval5 <- tost5$tost.interval


# create a plot

par(mar = c(5.1, 8.8, 4.1, 2.1))

plot(1000, ylim = c(.5, 5.5), xlim = c(-.15, .15), main = 'Balance Tests (TOST intervals)',
     xlab = 'Effect of having a daughter', ylab = '', yaxt = 'n')
axis(side = 2, at = c(1, 2, 3, 4, 5), labels= c('Female', 'Black', 'Coloured',
                                                'White', 'Older than median'), las = 2)

abline(v = 0)


points(y = c(1, 2, 3, 4, 5), x = c(est1, est2, est3, est4, est5), pch = 16)
 
abline(v = .05, lty = 2)
abline(v = -.05, lty = 2)
abline(v = .1, lty = 2)
abline(v = -.1, lty = 2)
abline(v = .15, lty = 2)
abline(v = -.15, lty = 2)


segments(y0 = c(1, 2, 3, 4, 5), y1 = c(1, 2, 3, 4, 5), 
         x0 = c(interval1[1], interval2[1], interval3[1],
                interval4[1], interval5[1]),
         x1 = c(interval1[2], interval2[2], interval3[2],
                interval4[2], interval5[2]), 
         lwd = 2)


###SI D
###Table D1 

##models calcuated above. Full regressin table for Table 2 in main manuscript 

screenreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)

texreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)

##Table D2
#removing all covariates and fixed effects 

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter, sas[sas$age <= 35,])

abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter, sas[sas$age <= 35,])

abortion.income.lm <- lm(abortion_income_binary ~ first_daughter, sas[sas$age <= 35,])

anc.lm <- lm(anc_support ~ first_daughter, sas[sas$age <= 35,])

gender.scale.lm <- (lm(num_agree_stand ~ first_daughter , sas08[sas08$age <= 35,] ))

screenreg(list(pref.women.lm, abortion.defect.lm, abortion.income.lm, anc.lm, gender.scale.lm), digits = 3)

texreg(list(pref.women.lm, abortion.defect.lm, abortion.income.lm, anc.lm, gender.scale.lm), digits = 3)

##For E.Alternative Estimation Approach: Generalized Linear Models 

sas$pref_hiring_ordered<-recode(sas$pref_hiring_women, " 'strongly disagree' = 1; 'disagree' = 2; 'agree' = 3; 'strongly agree' = 4; else = NA  ") #higher values more agreement


sas$abortion_defect_ordered <-recode(sas$abortion_defect, " 'always wrong' = 1; 'almost always wrong' = 2; 'wrong only sometimes' = 3; 'not wrong at all' = 4; else = NA  ") #higher values more agreement

sas$abortion_income_ordered <-recode(sas$abortion_income, " 'always wrong' = 1; 'almost always wrong' = 2; 'wrong only sometimes' = 3; 'not wrong at all' = 4; else = NA  ") #higher values more agreement

pref.women.probit.cov <- polr(as.factor(pref_hiring_ordered) ~ first_daughter + black + 
                          coloured + indian + age + sex + urban + survey, sas[sas$age <= 35,])

abortion.defect.probit.cov <- polr(as.factor(abortion_defect_ordered) ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, sas[sas$age <= 35,])

abortion.income.probit.cov <- polr(as.factor(abortion_income_ordered) ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, sas[sas$age <= 35,])

anc.logit.cov <- glm(anc_support ~ first_daughter + black + 
                   coloured + indian + age + sex + urban + survey, family=binomial(link='logit'), sas[sas$age <= 35,] )

gender.scale.probit.cov <- (polr(as.factor(num_agree_gender) ~ first_daughter + age + 
                             black + indian + coloured + sex + urban, sas08[sas08$age <= 35,] ))


#Table E.3

screenreg(list(pref.women.probit.cov, abortion.defect.probit.cov, abortion.income.probit.cov, anc.logit.cov, gender.scale.probit.cov), digits = 3)

texreg(list(pref.women.probit.cov, abortion.defect.probit.cov, abortion.income.probit.cov, anc.logit.cov, gender.scale.probit.cov), digits = 3)


#SI F: Subsetting to single child househods 

#Table F4

SAS_single<-subset(sas, how_many_kids ==1)
SAS08_single<-subset(sas08, how_many_kids ==1)

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter + survey, SAS_single[SAS_single $age <= 35,])
pref.women.lm.cov <- lm(pref_hiring_binary ~ first_daughter + black + 
                          coloured + indian + age + sex + urban + survey, SAS_single[SAS_single $age <= 35,])

abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, SAS_single[SAS_single $age <= 35,])
abortion.defect.lm.cov <- lm(abortion_defect_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, SAS_single[SAS_single $age <= 35,])

abortion.income.lm <- lm(abortion_income_binary ~ first_daughter + survey, SAS_single[SAS_single $age <= 35,])
abortion.income.lm.cov <- lm(abortion_income_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, SAS_single[SAS_single $age <= 35,])

anc.lm <- lm(anc_support ~ first_daughter + survey, SAS_single[SAS_single $age <= 35,])
anc.lm.cov <- lm(anc_support ~ first_daughter + black + 
                   coloured + indian + age + sex + urban + survey, SAS_single[SAS_single $age <= 35,] )

gender.scale.lm <- (lm(num_agree_stand ~ first_daughter , SAS08_single[SAS08_single $age <= 35,] ))

gender.scale.lm.cov <- (lm(num_agree_stand ~ first_daughter + age + 
                             black + indian + coloured + sex + urban, SAS08_single[SAS08_single $age <= 35,] ))


##Table F.4

screenreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)

texreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)


#Table F5: Any daugther specification 

##Any Daughter 

sas08<-subset(sas08, how_many_kids > 0)

sas08$any_daughters <- ifelse(sas08$how_many_daughters >= 1, 1, 0)


pref.women.lm <- lm(pref_hiring_binary ~ any_daughters + survey, sas[sas$age <= 35,])
pref.women.lm.cov <- lm(pref_hiring_binary ~ any_daughters + black + 
                          coloured + indian + age + sex + urban + survey, sas[sas$age <= 35,])

abortion.defect.lm <- lm(abortion_defect_binary ~ any_daughters + survey, sas[sas$age <= 35,])
abortion.defect.lm.cov <- lm(abortion_defect_binary ~ any_daughters + black + 
                               coloured + indian + age + sex + urban + survey, sas[sas$age <= 35,])

abortion.income.lm <- lm(abortion_income_binary ~ any_daughters + survey, sas[sas$age <= 35,])
abortion.income.lm.cov <- lm(abortion_income_binary ~ any_daughters + black + 
                               coloured + indian + age + sex + urban + survey, sas[sas$age <= 35,])

anc.lm <- lm(anc_support ~ any_daughters + survey, sas[sas$age <= 35,])
anc.lm.cov <- lm(anc_support ~ any_daughters + black + 
                   coloured + indian + age + sex + urban + survey, sas[sas$age <= 35,] )

gender.scale.lm <- (lm(num_agree_stand ~ any_daughters , sas08[sas08$age <= 35,] ))

gender.scale.lm.cov <- (lm(num_agree_stand ~ any_daughters + age + 
                             black + indian + coloured + sex + urban, sas08[sas08$age <= 35,] ))


##Table F.5

screenreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)

texreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)


##SI G	Separating Respondents by Gender  
#men v. women 

menSAS<-subset(sas, sex==0)
menSAS08<-subset(sas08, sex==0)

womenSAS<-subset(sas, sex==1)
womenSAS08<-subset(sas08, sex==1)

#for men

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter + survey, menSAS[menSAS $age <= 35,])
pref.women.lm.cov <- lm(pref_hiring_binary ~ first_daughter + black + 
                          coloured + indian + age + sex + urban + survey, menSAS[menSAS $age <= 35,])

abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, menSAS[menSAS $age <= 35,])
abortion.defect.lm.cov <- lm(abortion_defect_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, menSAS[menSAS $age <= 35,])

abortion.income.lm <- lm(abortion_income_binary ~ first_daughter + survey, menSAS[menSAS $age <= 35,])
abortion.income.lm.cov <- lm(abortion_income_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, menSAS[menSAS $age <= 35,])

anc.lm <- lm(anc_support ~ first_daughter + survey, menSAS[menSAS $age <= 35,])
anc.lm.cov <- lm(anc_support ~ first_daughter + black + 
                   coloured + indian + age + sex + urban + survey, menSAS[menSAS $age <= 35,] )

gender.scale.lm <- (lm(num_agree_stand ~ first_daughter , menSAS08[menSAS08 $age <= 35,] ))

gender.scale.lm.cov <- (lm(num_agree_stand ~ first_daughter + age + 
                             black + indian + coloured + sex + urban, menSAS08[menSAS08 $age <= 35,] ))

#Table G.6

screenreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)

texreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)


#for women

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter + survey, womenSAS[womenSAS $age <= 35,])
pref.women.lm.cov <- lm(pref_hiring_binary ~ first_daughter + black + 
                          coloured + indian + age + sex + urban + survey, womenSAS[womenSAS $age <= 35,])

abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, womenSAS[womenSAS $age <= 35,])
abortion.defect.lm.cov <- lm(abortion_defect_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, womenSAS[womenSAS $age <= 35,])

abortion.income.lm <- lm(abortion_income_binary ~ first_daughter + survey, womenSAS[womenSAS $age <= 35,])
abortion.income.lm.cov <- lm(abortion_income_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, womenSAS[womenSAS $age <= 35,])

anc.lm <- lm(anc_support ~ first_daughter + survey, womenSAS[womenSAS $age <= 35,])
anc.lm.cov <- lm(anc_support ~ first_daughter + black + 
                   coloured + indian + age + sex + urban + survey, womenSAS[womenSAS $age <= 35,] )

gender.scale.lm <- (lm(num_agree_stand ~ first_daughter , womenSAS08[womenSAS08 $age <= 35,] ))

gender.scale.lm.cov <- (lm(num_agree_stand ~ first_daughter + age + 
                             black + indian + coloured + sex + urban, womenSAS08[womenSAS08$age <= 35,] ))

##Table G.7

screenreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)

texreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)

##SI H: Urban v. rural respondents 

#urban v. rural 

urbanSAS<-subset(sas, urban==1)
urbanSAS08<-subset(sas08, urban==1)

ruralSAS<-subset(sas, urban ==0)
ruralSAS08<-subset(sas08, urban ==0)

#for urban

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter + survey, urbanSAS[urbanSAS $age <= 35,])
pref.women.lm.cov <- lm(pref_hiring_binary ~ first_daughter + black + 
                          coloured + indian + age + sex + urban + survey, urbanSAS[urbanSAS $age <= 35,])

abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, urbanSAS[urbanSAS $age <= 35,])
abortion.defect.lm.cov <- lm(abortion_defect_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, urbanSAS[urbanSAS $age <= 35,])

abortion.income.lm <- lm(abortion_income_binary ~ first_daughter + survey, urbanSAS[urbanSAS $age <= 35,])
abortion.income.lm.cov <- lm(abortion_income_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, urbanSAS[urbanSAS $age <= 35,])

anc.lm <- lm(anc_support ~ first_daughter + survey, urbanSAS[urbanSAS $age <= 35,])
anc.lm.cov <- lm(anc_support ~ first_daughter + black + 
                   coloured + indian + age + sex + urban + survey, urbanSAS[urbanSAS $age <= 35,] )

gender.scale.lm <- (lm(num_agree_stand ~ first_daughter , urbanSAS08[urbanSAS08 $age <= 35,] ))

gender.scale.lm.cov <- (lm(num_agree_stand ~ first_daughter + age + 
                             black + indian + coloured + sex + urban, urbanSAS08[urbanSAS08 $age <= 35,] ))


#Table H.8

screenreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)

texreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)


#for rural

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter + survey, ruralSAS[ruralSAS $age <= 35,])
pref.women.lm.cov <- lm(pref_hiring_binary ~ first_daughter + black + 
                          coloured + indian + age + sex + urban + survey, ruralSAS[ruralSAS $age <= 35,])

abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, ruralSAS[ruralSAS $age <= 35,])
abortion.defect.lm.cov <- lm(abortion_defect_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, ruralSAS[ruralSAS $age <= 35,])

abortion.income.lm <- lm(abortion_income_binary ~ first_daughter + survey, ruralSAS[ruralSAS $age <= 35,])
abortion.income.lm.cov <- lm(abortion_income_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, ruralSAS[ruralSAS $age <= 35,])

anc.lm <- lm(anc_support ~ first_daughter + survey, ruralSAS[ruralSAS $age <= 35,])
anc.lm.cov <- lm(anc_support ~ first_daughter + black + 
                   coloured + indian + age + sex + urban + survey, ruralSAS[ruralSAS $age <= 35,] )

gender.scale.lm <- (lm(num_agree_stand ~ first_daughter , ruralSAS08[ruralSAS08 $age <= 35,] ))

gender.scale.lm.cov <- (lm(num_agree_stand ~ first_daughter + age + 
                             black + indian + coloured + sex + urban, ruralSAS08[ruralSAS08$age <= 35,] ))

#Table H.9

screenreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)

texreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)

#SI 1: Changing the age cutpoint 

#45 and younger

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter + survey, sas[sas$age <= 45,])
pref.women.lm.cov <- lm(pref_hiring_binary ~ first_daughter + black + 
                          coloured + indian + age + sex + urban + survey, sas[sas$age <= 45,])

abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, sas[sas$age <= 45,])
abortion.defect.lm.cov <- lm(abortion_defect_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, sas[sas$age <= 45,])

abortion.income.lm <- lm(abortion_income_binary ~ first_daughter + survey, sas[sas$age <= 45,])
abortion.income.lm.cov <- lm(abortion_income_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, sas[sas$age <= 45,])

anc.lm <- lm(anc_support ~ first_daughter + survey, sas[sas$age <= 45,])
anc.lm.cov <- lm(anc_support ~ first_daughter + black + 
                   coloured + indian + age + sex + urban + survey, sas[sas$age <= 45,] )

gender.scale.lm <- (lm(num_agree_stand ~ first_daughter , sas08[sas08$age <= 45,] ))

gender.scale.lm.cov <- (lm(num_agree_stand ~ first_daughter + age + 
                             black + indian + coloured + sex + urban, sas08[sas08$age <= 45,] ))


#Table I.10

screenreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)

texreg(list(pref.women.lm, pref.women.lm.cov, abortion.defect.lm, abortion.defect.lm.cov, abortion.income.lm, abortion.income.lm.cov, gender.scale.lm, gender.scale.lm.cov, anc.lm, anc.lm.cov), digits = 3)




