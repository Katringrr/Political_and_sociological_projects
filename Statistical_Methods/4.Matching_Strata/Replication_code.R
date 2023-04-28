rm(list=ls())
set.seed(1234)

library(AER)
library(texreg)
library(ggplot2)
library(MASS)
library(equivalence)
library(cobalt)
library(MatchIt)
library(sjPlot)
library(rbounds)

#-------------------------------------------------------------------------#
#                            Работа с данными
#-------------------------------------------------------------------------#

# Загрузка данных

sas <- read.csv('final_sa_data.csv')
sas_NA <- subset(sas, !(is.na(sas$age)))

sas08 <- read.csv('final_sa08_data.csv')
sas08_NA <- subset(sas08, !(is.na()))


# Выборка (контрольная == 0 (2012), эксперементальная == 1 (2030))

sas.35 <- subset(sas_NA, age <= 35)
summary(sas_NA[sas_NA$age <= 35,]$first_daughter)
summary(sas_NA$first_daughter)

# Создание гендерной шкалы на основе 6 вопросов (4, 5,6 ==1; 0, 1, 2, 3 == 0)
sas08$num_agree_gender_binary <- ifelse(sas08$num_agree_gender >= 4, 1, 0) 

#-------------------------------------------------------------------------#
#                       Графическое изучение датасета
#-------------------------------------------------------------------------#

# Y (pref_hiring_binary, abortion_income_binary, abortion_defect_binary, anc_support, num_agree_gender_binary)
# D - first_daughter
# at - survey
# X (black, coloured, indian, age, sex, urban)

ggplot(sas_NA[sas_NA$age <= 35,], aes(as.factor(pref_hiring_binary))) + geom_bar() +
  theme_minimal() +
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("Против", "За")) +
  ylab("Количество людей") + 
  xlab("Льготный наем")

ggplot(subset(sas[sas$age <= 35,],!is.na(abortion_income_binary)), aes(as.factor(abortion_income_binary))) + geom_bar() +
  theme_minimal() +
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("Против", "За")) +
  ylab("Количество людей") + 
  xlab("Аборты (фин.)")

ggplot(subset(sas[sas$age <= 35,],!is.na(abortion_defect_binary)), aes(as.factor(abortion_defect_binary))) + 
  geom_bar() +
  theme_minimal() +
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("Против", "За")) +
  ylab("Количество людей") + 
  xlab("Аборты (мед.)")

ggplot(subset(sas[sas$age <= 35,],!is.na(anc_support)), aes(as.factor(anc_support))) + geom_bar() +
  theme_minimal() +
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("Не поддерживают", "Поддерживают")) +
  ylab("Количество людей") + 
  xlab("Поддержка АНК")

#-------------------------------------------------------------------------#
#                            Первые оценки
#-------------------------------------------------------------------------#

## Смотрим на разницу в средних по каждой зависимой переменной

# sums.daughter - средний уровень поддержки каждого из 5-ти Y среди тех, у кого есть дочь

sums.daughter <- c(mean(sas[sas$age <= 35,]$anc_support[which(sas[sas$age <= 35,]$first_daughter==1)], na.rm = T),
                   mean(sas08[sas08 $age <= 35,]$num_agree_gender_binary[which(sas08[sas08$age <= 35,]$first_daughter==1)], na.rm = T),
                   mean(sas[sas$age <= 35,]$abortion_income_binary[which(sas[sas$age <= 35,]$first_daughter==1)]),
                   mean(sas[sas$age <= 35,]$abortion_defect_binary[which(sas[sas$age <= 35,]$first_daughter==1)]),
                   mean(sas[sas$age <= 35,]$pref_hiring_binary[which(sas[sas$age <= 35,]$first_daughter==1)]))

# sums.son - средний уровень поддержки каждого из 5-ти Y среди тех, у кого есть сын

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
        names.arg = c('Поддержка АНК', 'Гендерная шкала',
                      'Аборты (фин.)', 'Аборты (мед.)',
                      'Льготный наем'), las = 2,
        col = c('gray65', 'gray15'),
        main = 'Гендерные установки родителей')

legend(x = .7, y = 4, legend = c('Сын', 'Дочь'),
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

#-------------------------------------------------------------------------#
#                            Проверка баланса ковариат
#-------------------------------------------------------------------------#

# Моя проверка 

new.names <- c(black = "Темнокожий",
               coloured =  "Цветной",
               indian =  "Индиец",
               age =  "Возраст",
               sex =  "Пол",
               urban = "Гор. житель",
               survey = "Опрос")

# Для 35
love.plot(first_daughter ~ black + coloured + indian + age + sex + urban, 
          data = sas_NA[sas_NA$age <= 35,], abs = T,
          method = rep("matching"), 
          line = FALSE,     
          binary = "std",     
          threshold = 0.1,
          var.names = new.names,
          sample.names = c("Выборка <=35"),
          title = NULL,
          themes = theme (legend.title = element_blank()))
# Для всех
love.plot(first_daughter ~ black + coloured + indian + age + sex + urban, 
          data = sas_NA, abs = T,
          method = rep("matching"), 
          line = FALSE,     
          binary = "std",     
          threshold = 0.1,
          var.names = new.names,
          sample.names = c("Все наблюдения"),
          title = NULL,
          themes = theme (legend.title = element_blank()))


# Проверка авторов

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


#-------------------------------------------------------------------------#
#                 Оценка эффекта с и без ковариат (версия авторов)
#-------------------------------------------------------------------------#

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter + survey, sas[sas$age <= 35,])
pref.women.lm.cov <- lm(pref_hiring_binary ~ first_daughter + black + 
                          coloured + indian + age + sex + urban + survey, sas[sas$age <= 35,])
summary(pref.women.lm)
summary(pref.women.lm.cov)

abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, sas[sas$age <= 35,])
abortion.defect.lm.cov <- lm(abortion_defect_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, sas[sas$age <= 35,])
summary(abortion.defect.lm)
summary(abortion.defect.lm.cov)

abortion.income.lm <- lm(abortion_income_binary ~ first_daughter + survey, sas[sas$age <= 35,])
abortion.income.lm.cov <- lm(abortion_income_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, sas[sas$age <= 35,])
summary(abortion.income.lm)
summary(abortion.income.lm.cov)

anc.lm <- lm(anc_support ~ first_daughter + survey, sas[sas$age <= 35,])
anc.lm.cov <- lm(anc_support ~ first_daughter + black + 
                   coloured + indian + age + sex + urban + survey, sas[sas$age <= 35,] )
summary(anc.lm)
summary(anc.lm.cov)

gender.scale.lm <- (lm(num_agree_stand ~ first_daughter , sas08[sas08$age <= 35,] ))
gender.scale.lm.cov <- (lm(num_agree_stand ~ first_daughter + age + 
                             black + indian + coloured + sex + urban, sas08[sas08$age <= 35,] ))
summary(gender.scale.lm)
summary(gender.scale.lm.cov)

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

#Без ковариат и fix efect

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter, sas[sas$age <= 35,])


abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter, sas[sas$age <= 35,])


abortion.income.lm <- lm(abortion_income_binary ~ first_daughter, sas[sas$age <= 35,])


anc.lm <- lm(anc_support ~ first_daughter, sas[sas$age <= 35,])

gender.scale.lm <- (lm(num_agree_stand ~ first_daughter , sas08[sas08$age <= 35,] ))

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

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

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

#-------------------------------------------------------------------------#
#                       SI F: Subsetting to single child househods 
#-------------------------------------------------------------------------#

#Table F4 - создание датасета, где семьи только с одним ребенком

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

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

##Any Daughter - создание датасета, где семьи с одной или более дочерью

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

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

##SI G	Separating Respondents by Gender 

#men v. women 

menSAS<-subset(sas, sex==0)
menSAS08<-subset(sas08, sex==0)

womenSAS<-subset(sas, sex==1)
womenSAS08<-subset(sas08, sex==1)

#for men - эффект для отцов

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

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)


#for women - эффект для матерей

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

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

##SI H: Urban v. rural respondents 

#urban v. rural 

urbanSAS<-subset(sas, urban==1)
urbanSAS08<-subset(sas08, urban==1)

ruralSAS<-subset(sas, urban ==0)
ruralSAS08<-subset(sas08, urban ==0)

#for urban - эффект для проживающих в городе

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

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)


#for rural - эффект для проживающих в сельской местности

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

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

#45 and younger - смена порога возраста

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

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

#-------------------------------------------------------------------------#
#                            Мэтчинг
#-------------------------------------------------------------------------#

### Nearest Matching - плохо

match_near <- matchit(first_daughter ~ black + coloured + indian + age + sex + urban + survey, 
                      data = sas_NA[sas_NA$age <= 35,], method = "nearest",
                      ratio = 3)
bal.tab(match_near)

### Nearest Mahalanobis Matching - тоже не оч

match_near_mahal <- matchit(first_daughter ~ black + coloured + indian + age + sex + urban + survey, 
                            data = sas_NA[sas_NA$age <= 35,], method = "nearest", distance = "mahalanobis",
                            ratio = 3)

bal.tab(match_near_mahal)

### Optimal Matching - намного лучше

match_optimal <- matchit(first_daughter ~ black + coloured + indian + age + sex + urban + survey, 
                         data = sas_NA[sas_NA$age <= 35,], method = "optimal",
                         ratio = 3)

bal.tab(match_optimal)

### CEM Matching - идеально

match_cem <- matchit(first_daughter ~ black + coloured + indian + age + sex + urban + survey, 
                     data = sas_NA[sas_NA$age <= 35,], method = "cem", k2k = TRUE,
                     k2k.method = "mahalanobis",
                     cutpoints = list(AGE = "q6", AGE2 = "q6"),
                     estimand = "ATT")

bal.tab(match_cem)

### Genetic Matching - оч плохо, даже хуже чем было

match_gen <- matchit(first_daughter ~ black + coloured + indian + age + sex + urban + survey, 
                     data = sas_NA[sas_NA$age <= 35,], method = "genetic", distance = "mahalanobis",
                     pop.size = 300,
                     ratio = 3)

bal.tab(match_gen)

# Графическое сравнение всех мэтчингов

love.plot(first_daughter ~ black + coloured + indian + age + sex + urban, 
          data = sas_NA[sas_NA$age <= 35,], abs = T,
          method = rep("matching",1), 
          weights = list(Near = get.w(match_near),
                         CEM = get.w(match_cem),
                         Mah = get.w(match_near_mahal),
                         Opt = get.w(match_optimal)), 
          line = FALSE,     
          binary = "std",     
          threshold = 0.1,
          var.names = new.names,
          sample.names = c("Без мэтчинга", "Ближ.сосед", "Страты" ,"Махаланобис", "Оптимальный"),
          title = NULL,
          themes = theme (legend.title = element_blank()))




#-------------------------------------------------------------------------#
#                            Оценка после мэтчинг
#-------------------------------------------------------------------------#

# Создаем новый датасет на базе мэтчинга

sas.match <- match.data(match_cem)

#-------------------------------------------------------------------------#
#                            Первые оценки
#-------------------------------------------------------------------------#

## Смотрим на разницу в средних по каждой зависимой переменной

# sums.daughter - средний уровень поддержки каждого из 5-ти Y среди тех, у кого есть дочь

sums.daughter <- c(mean(sas.match[sas.match$age <= 35,]$anc_support[which(sas.match[sas.match$age <= 35,]$first_daughter==1)], na.rm = T),
                   mean(sas08[sas08 $age <= 35,]$num_agree_gender_binary[which(sas08[sas08$age <= 35,]$first_daughter==1)], na.rm = T),
                   mean(sas.match[sas.match$age <= 35,]$abortion_income_binary[which(sas.match[sas.match$age <= 35,]$first_daughter==1)]),
                   mean(sas.match[sas.match$age <= 35,]$abortion_defect_binary[which(sas.match[sas.match$age <= 35,]$first_daughter==1)]),
                   mean(sas.match[sas.match$age <= 35,]$pref_hiring_binary[which(sas.match[sas.match$age <= 35,]$first_daughter==1)]))

# sums.son - средний уровень поддержки каждого из 5-ти Y среди тех, у кого есть сын

sums.son <- c(mean(sas.match[sas.match$age <= 35,]$anc_support[which(sas.match[sas.match$age <= 35,]$first_daughter==0)], na.rm = T),
              mean(sas08[sas08$age <= 35,]$num_agree_gender_binary[which(sas08[sas08$age <= 35,]$first_daughter==0)], na.rm = T),
              mean(sas.match[sas.match$age <= 35,]$abortion_income_binary[which(sas.match[sas.match$age <= 35,]$first_daughter==0)]),
              mean(sas.match[sas.match$age <= 35,]$abortion_defect_binary[which(sas.match[sas.match$age <= 35,]$first_daughter==0)]),
              mean(sas.match[sas.match$age <= 35,]$pref_hiring_binary[which(sas.match[sas.match$age <= 35,]$first_daughter==0)]))

#basic t-tests 

t.test(anc_support ~ first_daughter, data = sas.match[sas.match$age <= 35,]) #SE of difference: 0.01574901
t.test(num_agree_gender_binary ~ first_daughter, data = sas08[sas08$age <= 35,]) #SE of difference: 0.04334639
t.test(abortion_income_binary ~ first_daughter, data = sas.match[sas.match$age <= 35,])  # SE of difference: 0.01124662
t.test(abortion_defect_binary ~ first_daughter, data = sas.match[sas.match$age <= 35,])  # SE of difference: 0.01492583
t.test(pref_hiring_binary ~ first_daughter, data = sas.match[sas.match$age <= 35,])  #SE of difference: 0.01449723

par(mar = c(5.1, 8.8, 4.1, 2.3))

barplot(rbind(sums.daughter, sums.son), beside = T, horiz = T ,
        xlim = c(0, 1),
        names.arg = c('Поддержка АНК', 'Гендерная шкала',
                      'Аборты (фин.)', 'Аборты (мед.)',
                      'Льготный наем'), las = 2,
        col = c('gray65', 'gray15'),
        main = 'Гендерные установки родителей')

legend(x = .7, y = 4, legend = c('Сын', 'Дочь'),
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

#-------------------------------------------------------------------------#
#                            Проверка баланса ковариат
#-------------------------------------------------------------------------#

# Моя проверка 

love.plot(first_daughter ~ black + coloured + indian + age + sex + urban + survey, 
          data = sas_NA[sas_NA$age <= 35,], abs = T,
          method = rep("matching"), 
          line = FALSE,     
          binary = "std",     
          threshold = 0.1)

#-------------------------------------------------------------------------#
#                 Оценка эффекта с и без ковариат (после мэтчинга)
#-------------------------------------------------------------------------#

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter + survey, sas.match[sas.match$age <= 35,])
pref.women.lm.cov <- lm(pref_hiring_binary ~ first_daughter + black + 
                          coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 35,])
summary(pref.women.lm)
summary(pref.women.lm.cov)

abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, sas.match[sas.match$age <= 35,])
abortion.defect.lm.cov <- lm(abortion_defect_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 35,])
summary(abortion.defect.lm)
summary(abortion.defect.lm.cov)

abortion.income.lm <- lm(abortion_income_binary ~ first_daughter + survey, sas.match[sas.match$age <= 35,])
abortion.income.lm.cov <- lm(abortion_income_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 35,])
summary(abortion.income.lm)
summary(abortion.income.lm.cov)

anc.lm <- lm(anc_support ~ first_daughter + survey, sas.match[sas.match$age <= 35,])
anc.lm.cov <- lm(anc_support ~ first_daughter + black + 
                   coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 35,] )
summary(anc.lm)
summary(pref.women.lm.cov)

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

#-------------------------------------------------------------------------#
#                 Оценка эффекта с и без ковариат (версия авторов - glm)
#-------------------------------------------------------------------------#


pref.women.lm <- glm(pref_hiring_binary ~ first_daughter + survey, sas.match[sas.match$age <= 35,],family = "binomial")
pref.women.lm.cov <- glm(pref_hiring_binary ~ first_daughter + black + 
                          coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 35,],family = "binomial")


abortion.defect.lm <- glm(abortion_defect_binary ~ first_daughter + survey, sas.match[sas.match$age <= 35,],family = "binomial")
abortion.defect.lm.cov <- glm(abortion_defect_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 35,],family = "binomial")

abortion.income.lm <- glm(abortion_income_binary ~ first_daughter + survey, sas.match[sas.match$age <= 35,],family = "binomial")
abortion.income.lm.cov <- glm(abortion_income_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 35,],family = "binomial")

anc.lm <- glm(anc_support ~ first_daughter + survey, sas.match[sas.match$age <= 35,],family = "binomial")
anc.lm.cov <- glm(anc_support ~ first_daughter + black + 
                   coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 35,],family = "binomial" )

gender.scale.lm <- (glm(num_agree_stand ~ first_daughter , sas08[sas08$age <= 35,],family = "binomial" ))

gender.scale.lm.cov <- (glm(num_agree_stand ~ first_daughter + age + 
                             black + indian + coloured + sex + urban, sas08[sas08$age <= 35,],family = "binomial" ))

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

#-------------------------------------------------------------------------#
#                            Анализ чувствительности
#-------------------------------------------------------------------------#

# Анализ чувствительности (интервалы значений пи-величины для разных значений параметра Гамма)

psens(mach.out, Gamma=1.5, GammaInc=.1)
psens(cem.out, Gamma=1.5, GammaInc=.1)

# Разброс точечных оценок Ходжеса-Леманна

hlsens(mach.out, Gamma=1.5, GammaInc=.1, .1)
hlsens(cem.out, Gamma=1.5, GammaInc=.1, .1)

#-------------------------------------------------------------------------#
#                            Проверка других теоретичских нюансов
#-------------------------------------------------------------------------#

##For E.Alternative Estimation Approach: Generalized Linear Models 

sas.match$pref_hiring_ordered<-recode(sas.match$pref_hiring_women, " 'strongly disagree' = 1; 'disagree' = 2; 'agree' = 3; 'strongly agree' = 4; else = NA  ") #higher values more agreement


sas.match$abortion_defect_ordered <-recode(sas.match$abortion_defect, " 'always wrong' = 1; 'almost always wrong' = 2; 'wrong only sometimes' = 3; 'not wrong at all' = 4; else = NA  ") #higher values more agreement

sas.match$abortion_income_ordered <-recode(sas.match$abortion_income, " 'always wrong' = 1; 'almost always wrong' = 2; 'wrong only sometimes' = 3; 'not wrong at all' = 4; else = NA  ") #higher values more agreement

pref.women.probit.cov <- polr(as.factor(pref_hiring_ordered) ~ first_daughter + black + 
                                coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 35,])

abortion.defect.probit.cov <- polr(as.factor(abortion_defect_ordered) ~ first_daughter + black + 
                                     coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 35,])

abortion.income.probit.cov <- polr(as.factor(abortion_income_ordered) ~ first_daughter + black + 
                                     coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 35,])

anc.logit.cov <- glm(anc_support ~ first_daughter + black + 
                       coloured + indian + age + sex + urban + survey, family=binomial(link='logit'), sas.match[sas.match$age <= 35,])

gender.scale.probit.cov <- (polr(as.factor(num_agree_gender) ~ first_daughter + age + 
                                   black + indian + coloured + sex + urban, sas08[sas08$age <= 35,] ))

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

#Table F4 - создание датасета, где семьи только с одним ребенком

SAS_single<-subset(sas.match, how_many_kids ==1)
SAS08_single<-subset(sas.match, how_many_kids ==1)

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

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

##Any Daughter - создание датасета, где семьи с одной или более дочерью

sas08<-subset(sas.match, how_many_kids > 0)

sas08$any_daughters <- ifelse(sas.match$how_many_daughters >= 1, 1, 0)


pref.women.lm <- lm(pref_hiring_binary ~ any_daughters + survey, sas.match[sas.match$age <= 35,])
pref.women.lm.cov <- lm(pref_hiring_binary ~ any_daughters + black + 
                          coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 35,])

abortion.defect.lm <- lm(abortion_defect_binary ~ any_daughters + survey, sas.match[sas.match$age <= 35,])
abortion.defect.lm.cov <- lm(abortion_defect_binary ~ any_daughters + black + 
                               coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 35,])

abortion.income.lm <- lm(abortion_income_binary ~ any_daughters + survey, sas.match[sas.match$age <= 35,])
abortion.income.lm.cov <- lm(abortion_income_binary ~ any_daughters + black + 
                               coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 35,])

anc.lm <- lm(anc_support ~ any_daughters + survey, sas.match[sas.match$age <= 35,])
anc.lm.cov <- lm(anc_support ~ any_daughters + black + 
                   coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 35,] )

gender.scale.lm <- (lm(num_agree_stand ~ any_daughters , sas08[sas08$age <= 35,] ))

gender.scale.lm.cov <- (lm(num_agree_stand ~ any_daughters + age + 
                             black + indian + coloured + sex + urban, sas08[sas08$age <= 35,] ))

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

##SI G	Separating Respondents by Gender 

#men v. women 

menSAS<-subset(sas.match, sex==0)
menSAS08<-subset(sas08, sex==0)

womenSAS<-subset(sas.match, sex==1)
womenSAS08<-subset(sas08, sex==1)

#for men - эффект для отцов

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

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)


#for women - эффект для матерей

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

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

##SI H: Urban v. rural respondents 

#urban v. rural 

urbanSAS<-subset(sas.match, urban==1)
urbanSAS08<-subset(sas08, urban==1)

ruralSAS<-subset(sas.match, urban ==0)
ruralSAS08<-subset(sas08, urban ==0)

#for urban - эффект для проживающих в городе

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

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)


#for rural - эффект для проживающих в сельской местности

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

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

#45 and younger - смена порога возраста

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter + survey, sas.match[sas.match$age <= 45,])
pref.women.lm.cov <- lm(pref_hiring_binary ~ first_daughter + black + 
                          coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 45,])

abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, sas.match[sas.match$age <= 45,])
abortion.defect.lm.cov <- lm(abortion_defect_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 45,])

abortion.income.lm <- lm(abortion_income_binary ~ first_daughter + survey, sas.match[sas.match$age <= 45,])
abortion.income.lm.cov <- lm(abortion_income_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 45,])

anc.lm <- lm(anc_support ~ first_daughter + survey, sas.match[sas.match$age <= 45,])
anc.lm.cov <- lm(anc_support ~ first_daughter + black + 
                   coloured + indian + age + sex + urban + survey, sas.match[sas.match$age <= 45,] )

gender.scale.lm <- (lm(num_agree_stand ~ first_daughter , sas08[sas08$age <= 45,] ))

gender.scale.lm.cov <- (lm(num_agree_stand ~ first_daughter + age + 
                             black + indian + coloured + sex + urban, sas08[sas08$age <= 45,] ))

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

# Без ограничений по возрасту

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter + survey, sas.match)
pref.women.lm.cov <- lm(pref_hiring_binary ~ first_daughter + black + 
                          coloured + indian + age + sex + urban + survey, sas.match)


abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, sas.match)
abortion.defect.lm.cov <- lm(abortion_defect_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, sas.match)

abortion.income.lm <- lm(abortion_income_binary ~ first_daughter + survey, sas.match)
abortion.income.lm.cov <- lm(abortion_income_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, sas.match)

anc.lm <- lm(anc_support ~ first_daughter + survey, sas.match)
anc.lm.cov <- lm(anc_support ~ first_daughter + black + 
                   coloured + indian + age + sex + urban + survey, sas.match)

gender.scale.lm <- (lm(num_agree_stand ~ first_daughter , sas08))

gender.scale.lm.cov <- (lm(num_agree_stand ~ first_daughter + age + 
                             black + indian + coloured + sex + urban, sas08))

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

# Семьи black

blackSAS<-subset(sas.match, black==1)
blackSAS08<-subset(sas08, black==1)

noblackSAS<-subset(sas.match, black ==0)
noblackSAS08<-subset(sas08, black ==0)

# эффект для black

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter + survey, blackSAS[blackSAS $age <= 35,])
pref.women.lm.cov <- lm(pref_hiring_binary ~ first_daughter + black + 
                          coloured + indian + age + sex + urban + survey, blackSAS[blackSAS $age <= 35,])

abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, blackSAS[blackSAS $age <= 35,])
abortion.defect.lm.cov <- lm(abortion_defect_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, blackSAS[blackSAS $age <= 35,])

abortion.income.lm <- lm(abortion_income_binary ~ first_daughter + survey, blackSAS[blackSAS $age <= 35,])
abortion.income.lm.cov <- lm(abortion_income_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, blackSAS[blackSAS $age <= 35,])

anc.lm <- lm(anc_support ~ first_daughter + survey, blackSAS[blackSAS $age <= 35,])
anc.lm.cov <- lm(anc_support ~ first_daughter + black + 
                   coloured + indian + age + sex + urban + survey, blackSAS[blackSAS $age <= 35,] )

gender.scale.lm <- (lm(num_agree_stand ~ first_daughter , blackSAS08[blackSAS08 $age <= 35,] ))

gender.scale.lm.cov <- (lm(num_agree_stand ~ first_daughter + age + 
                             black + indian + coloured + sex + urban, blackSAS08[blackSAS08 $age <= 35,] ))

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

# эффект

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter + survey, noblackSAS[noblackSAS $age <= 35,])
pref.women.lm.cov <- lm(pref_hiring_binary ~ first_daughter + black + 
                          coloured + indian + age + sex + urban + survey, noblackSAS[noblackSAS $age <= 35,])

abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, noblackSAS[noblackSAS $age <= 35,])
abortion.defect.lm.cov <- lm(abortion_defect_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, noblackSAS[noblackSAS $age <= 35,])

abortion.income.lm <- lm(abortion_income_binary ~ first_daughter + survey, noblackSAS[noblackSAS $age <= 35,])
abortion.income.lm.cov <- lm(abortion_income_binary ~ first_daughter + black + 
                               coloured + indian + age + sex + urban + survey, noblackSAS[noblackSAS $age <= 35,])

anc.lm <- lm(anc_support ~ first_daughter + survey, noblackSAS[noblackSAS $age <= 35,])
anc.lm.cov <- lm(anc_support ~ first_daughter + black + 
                   coloured + indian + age + sex + urban + survey, noblackSAS[noblackSAS $age <= 35,] )

gender.scale.lm <- (lm(num_agree_stand ~ first_daughter , noblackSAS08[noblackSAS08 $age <= 35,] ))

gender.scale.lm.cov <- (lm(num_agree_stand ~ first_daughter + age + 
                             black + indian + coloured + sex + urban, noblackSAS08[noblackSAS08 $age <= 35,] ))

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)

#-------------------------------------------------------------------------#
#                            Провекра чувствительности
#-------------------------------------------------------------------------#

#---------------------------------------------------------------#
#                       Оценка эффекта
#---------------------------------------------------------------#



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

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm,gender.scale.lm)
tab_model(pref.women.lm.cov,abortion.defect.lm.cov,abortion.income.lm.cov,anc.lm.cov,gender.scale.lm.cov)
tab_model(pref.women.lm.cov,pref.women.lm.cov2)

#Мы можем использовать тест Розенбаума, так как мэтчинг был без замещения (соотвествтеи количества наблюдений в контрольной и группе воздействия: 1 к 1)



pref.women.lm.cov.sensitivity <- sensemakr(model = pref.women.lm.cov, 
                                treatment = "first_daughter",
                                benchmark_covariates = c("black", "coloured", "indian", "age", "sex", "urban"),
                                kd = 1:3)

pref.women.lm.cov.sensitivity
plot(pref.women.lm.cov.sensitivity)


