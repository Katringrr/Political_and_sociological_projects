#-------------------------------------------------------------------------#
#                            Работа с данными
#-------------------------------------------------------------------------#

# Загрузка данных

sas <- read.csv('final_sa_data.csv')
sas_NA <- subset(sas, !(is.na(sas$age)))

# Перекодировка

sas_NA$first_daughter <- as.factor(sas_NA$first_daughter)
sas$first_daughter <- as.factor(sas$first_daughter)

#-------------------------------------------------------------------------#
#                            Создание страт 
#-------------------------------------------------------------------------#
sas.35 <- subset(sas_NA, age <= 35)

# Всего наблюдений (на основании страт) - 4029 (из 4 042 изначально)

#--------------------------------------------------#
#                 Женщины
#--------------------------------------------------#
summary(as.factor(sas_NA[sas_NA$age <=35,]$sex))
summary(as.factor(sas_NA$sex))

#---------------------------------------#
#                 До 25 лет
#---------------------------------------#
# 976
## Темнокожие
# Город
wtg35 <- subset(sas_NA, sex==1 & age <=25 & black==1 & urban ==1) # 344 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wtg35) # -0.06
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wtg35) # 0
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wtg35) # 0.02
anc.lm <- lm(anc_support ~ first_daughter + survey, wtg35) # 0.05

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
# Село
wts35 <- subset(sas_NA, sex==1 & age <=25 & black==1 & urban ==0) # 427 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wts35) # -0.02
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wts35) # -0.01
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wts35) # -0.01
anc.lm <- lm(anc_support ~ first_daughter + survey, wts35) # 0.04

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
## Белые
# Город
wwg35 <- subset(sas_NA, sex==1 & age <=25 & white==1 & urban ==1) # 32 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wwg35) # 0.22
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wwg35) # -0.04
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wwg35)# 0.03
anc.lm <- lm(anc_support ~ first_daughter + survey, wwg35) # 0

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
# Село
wws35 <- subset(sas_NA, sex==1 & age <=25 & white==1 & urban ==0) # 0

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wws35) 
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wws35)
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wws35)
anc.lm <- lm(anc_support ~ first_daughter + survey, wws35)

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
## Цветные
# Город
wcg35 <- subset(sas_NA, sex==1 & age <=25 & coloured==1 & urban ==1) # 110 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wcg35)# 0.13
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wcg35)#-0.03
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wcg35)# -0.11
anc.lm <- lm(anc_support ~ first_daughter + survey, wcg35)#-0.07

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
# Село
wcs35 <- subset(sas_NA, sex==1 & age <=25 & coloured==1 & urban ==0) # 20 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wcs35)#0.08
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wcs35)#0.21
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wcs35)#0.03
anc.lm <- lm(anc_support ~ first_daughter + survey, wcs35)#0.46

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
## Индийки
# Город
wig35 <- subset(sas_NA, sex==1 & age <=25 & indian==1 & urban ==1) # 43 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wig35)# -0.08
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wig35)#-0.03
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wig35)#-0.14
anc.lm <- lm(anc_support ~ first_daughter + survey, wig35)#-0.03

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
# Село
wis35 <- subset(sas_NA, sex==1 & age <=25 & indian==1 & urban ==0) # 0 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wis35)
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wis35)
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wis35)
anc.lm <- lm(anc_support ~ first_daughter + survey, wis35)

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)

#---------------------------------------#
#                  26-35 лет
#---------------------------------------#
# 2155
## Темнокожие
# Город
wtg35 <- subset(sas_NA, sex==1 & age <=35 & age >25 & black==1 & urban ==1) # 695 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wtg35)# -0.05
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wtg35)#-0.03
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wtg35)# -0.07
anc.lm <- lm(anc_support ~ first_daughter + survey, wtg35)# 0.01

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
# Село
wts35 <- subset(sas_NA, sex==1 & age <=35 & age >25 & black==1 & urban ==0) # 716 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wts35)#0.02
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wts35)#-0.01
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wts35)#-0.05
anc.lm <- lm(anc_support ~ first_daughter + survey, wts35)#-0.01

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
## Белые
# Город
wwg35 <- subset(sas_NA, sex==1 & age <=35 & age >25 & white==1 & urban ==1) # 159 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wwg35)#-0.12
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wwg35)#-0.03
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wwg35)#-0.04
anc.lm <- lm(anc_support ~ first_daughter + survey, wwg35)# 0.02

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
# Село
wws35 <- subset(sas_NA, sex==1 & age <=35 & age >25 & white==1 & urban ==0) # 8 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wws35)#0
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wws35)#-0.5
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wws35)#0
anc.lm <- lm(anc_support ~ first_daughter + survey, wws35)#0

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
## Цветные
# Город
wcg35 <- subset(sas_NA, sex==1 & age <=35 & age >25 & coloured==1 & urban ==1) # 294 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wcg35)#-0.01
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wcg35)#0
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wcg35)#0.04
anc.lm <- lm(anc_support ~ first_daughter + survey, wcg35)#-0.04

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
# Село
wcs35 <- subset(sas_NA, sex==1 & age <=35 & age >25 & coloured==1 & urban ==0) # 66 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wcs35)#-0.01
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wcs35)# 0.02
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wcs35)# 0.22
anc.lm <- lm(anc_support ~ first_daughter + survey, wcs35)#-0.15

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
## Индийки
# Город
wig35 <- subset(sas_NA, sex==1 & age <=35 & age >25 & indian==1 & urban ==1) # 212 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wig35)# 0.07
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wig35)# 0
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wig35)#-0.04
anc.lm <- lm(anc_support ~ first_daughter + survey, wig35)#-0.03

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
# Село
wis35 <- subset(sas_NA, sex==1 & age <=35 & age >25 & indian==1 & urban ==0) # 5 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wis35)#-0
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wis35)# 0
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wis35)# 1
anc.lm <- lm(anc_support ~ first_daughter + survey, wis35)#-1

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
#--------------------------------------------------#
#                 Мужчины
#--------------------------------------------------#
summary(as.factor(sas_NA[sas_NA$age <=35,]$sex))
summary(as.factor(sas_NA$sex))


#---------------------------------------#
#                 До 25 лет
#---------------------------------------#
# 178
## Темнокожие
# Город
wtg35 <- subset(sas_NA, sex==0 & age <=25 & black==1 & urban ==1) # 51 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wtg35)# 0.12
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wtg35)#-0.1
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wtg35)#-0.16
anc.lm <- lm(anc_support ~ first_daughter + survey, wtg35)#0.05

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
# Село
wts35 <- subset(sas_NA, sex==0 & age <=25 & black==1 & urban ==0) # 72 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wts35)#-0.09
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wts35)#-0.15
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wts35)#-0.06
anc.lm <- lm(anc_support ~ first_daughter + survey, wts35)#-0.01

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
## Белые
# Город
wwg35 <- subset(sas_NA, sex==0 & age <=25 & white==1 & urban ==1) # 9 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wwg35) # -0.5
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wwg35)#0
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wwg35)#0.5
anc.lm <- lm(anc_support ~ first_daughter + survey, wwg35)#0

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
# Село
wws35 <- subset(sas_NA, sex==0 & age <=25 & white==1 & urban ==0) # 2 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wws35)#
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wws35)#
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wws35)#
anc.lm <- lm(anc_support ~ first_daughter + survey, wws35)#

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
## Цветные
# Город
wcg35 <- subset(sas_NA, sex==0 & age <=25 & coloured==1 & urban ==1) # 24 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wcg35)#0.01
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wcg35)#0.05
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wcg35)#0.05
anc.lm <- lm(anc_support ~ first_daughter + survey, wcg35)#-0.11

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
# Село
wcs35 <- subset(sas_NA, sex==0 & age <=25 & coloured==1 & urban ==0) # 10 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wcs35)#1
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wcs35)#-0.57
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wcs35)#-0.29
anc.lm <- lm(anc_support ~ first_daughter + survey, wcs35)#-0.14

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
## Индийки
# Город
wig35 <- subset(sas_NA, sex==0 & age <=25 & indian==1 & urban ==1) # 10 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wig35)# 1
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wig35)# 0.71
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wig35)# 1
anc.lm <- lm(anc_support ~ first_daughter + survey, wig35)#0.29

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
# Село
wis35 <- subset(sas_NA, sex==0 & age <=25 & indian==1 & urban ==0) # 0 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wis35)
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wis35)
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wis35)
anc.lm <- lm(anc_support ~ first_daughter + survey, wis35)

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)


#---------------------------------------#
#                  25-35 лет
#---------------------------------------#
# 720
## Темнокожие
# Город
wtg35 <- subset(sas_NA, sex==0 & age <=35 & age >25 & black==1 & urban ==1) # 210 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wtg35)# -0.04
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wtg35)# -0.04
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wtg35)#-0.01
anc.lm <- lm(anc_support ~ first_daughter + survey, wtg35)# -0.01

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
# Село
wts35 <- subset(sas_NA, sex==0 & age <=35 & age >25 & black==1 & urban ==0) # 220 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wts35)# 0.01
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wts35)# -0.1
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wts35)# -0.01
anc.lm <- lm(anc_support ~ first_daughter + survey, wts35)#-0.17

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
## Белые
# Город
wwg35 <- subset(sas_NA, sex==0 & age <=35 & age >25 & white==1 & urban ==1) # 82 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wwg35)# 0.05
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wwg35)# 0.01
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wwg35)#-0.09
anc.lm <- lm(anc_support ~ first_daughter + survey, wwg35)#-0.01

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
# Село
wws35 <- subset(sas_NA, sex==0 & age <=35 & age >25 & white==1 & urban ==0) # 9 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wws35)# -0
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wws35)# 0
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wws35)# 0
anc.lm <- lm(anc_support ~ first_daughter + survey, wws35)#0

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
## Цветные
# Город
wcg35 <- subset(sas_NA, sex==0 & age <=35 & age >25 & coloured==1 & urban ==1) # 76 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wcg35)# 0.03
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wcg35)# 0.06
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wcg35)#-0.14
anc.lm <- lm(anc_support ~ first_daughter + survey, wcg35)# -0.17

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
# Село
wcs35 <- subset(sas_NA, sex==0 & age <=35 & age >25 & coloured==1 & urban ==0) # 35 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wcs35)# 0.15
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wcs35)# -0.15
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wcs35)# -0,03
anc.lm <- lm(anc_support ~ first_daughter + survey, wcs35)# -0.21

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
## Индийки
# Город
wig35 <- subset(sas_NA, sex==0 & age <=35 & age >25 & indian==1 & urban ==1) # 87 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wig35)# 0.07
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wig35)# 0.08
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wig35)# -0.02
anc.lm <- lm(anc_support ~ first_daughter + survey, wig35)# 0.01

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)
# Село
wis35 <- subset(sas_NA, sex==0 & age <=35 & age >25 & indian==1 & urban ==0) # 1 наблюдений

pref.women.lm <- lm(pref_hiring_binary ~ first_daughter+ survey, wis35)#0
abortion.income.lm <- lm(abortion_income_binary ~ first_daughter+ survey, wis35)#0
abortion.defect.lm <- lm(abortion_defect_binary ~ first_daughter + survey, wis35)#
anc.lm <- lm(anc_support ~ first_daughter + survey, wis35)#0

tab_model(pref.women.lm,abortion.defect.lm,abortion.income.lm,anc.lm)