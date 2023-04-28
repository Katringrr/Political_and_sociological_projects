## Загружаем датасет

load("seminar2.RData")

# gender - пол (male - м, female - ж); 
# ethnicity - этническая принадлежность (афроамериканцы, латиноамериканцы, другое); 
# score - набранный на тесте балл; 
# fcollege - наличие высшего образования у отца (no - нет, yes - есть); 
# mcollege - наличие высшего образования у матери (no - нет, yes - есть); 
# education - число лет обучения; 
# income - доход (low - ниже 25000 USD в год, high - выше 25000 USD в год).

## Загружаем библиотеки

library(sjPlot)
library(AER)
library(stargazer)

## Оцените эффект продолжительности обучения (education) на результаты итогового теста (score), используя данные об образовании родителей в качестве инструментальных переменных. Сравните с наивной оценкой с помощью линейной регрессии.


# Оценка со всеми предикторами
m1 <- lm(score ~ education + gender + ethnicity + income + fcollege + mcollege, grad)
tab_model(m1)
round(summary(m1)$coef,2)
# Оценка без будущих инструментальных переменных
m5 <- lm(score ~ education + gender + ethnicity + income, grad)
tab_model(m5)
round(summary(m5)$coef,2)

## МНК - регрессия


# Образование матери (количество лет) как инструмент оценки эффекта образования студента 
# Этап 1
m2 <- lm(education ~  mcollege + gender + ethnicity + income, grad)
tab_model(m2)
round(summary(m2)$coef,2)
# Этап 2
educationHat <- fitted(m2)
m3 <- lm(score ~ educationHat + gender + ethnicity  + income, grad)
tab_model(m3)
round(summary(m3)$coef,2)


# Образование отца (количество лет) как инструмент оценки эффекта образования студента
# Этап 1
m4 <- lm(education ~ fcollege + gender + ethnicity + income, grad)
tab_model(m4)
round(summary(m4)$coef,2)
# Этап 2
educationHat <- fitted(m4)
m5 <- lm(score ~ educationHat + gender + ethnicity  + income, grad)
tab_model(m5)
round(summary(m5)$coef,2)


# Образование матери отца (количество лет) как инструмент оценки эффекта образования студента
# Этап 1
m6 <- lm(education ~  mcollege + fcollege + gender + ethnicity + income, grad)
tab_model(m6)
round(summary(m6)$coef,2)
# Этап 2
educationHat <- fitted(m6)
m7 <- lm(score ~ educationHat + gender + ethnicity  + income, grad)
tab_model(m7)
round(summary(m7)$coef,2)


## Ivreg метод

m1 <- lm(score ~ education + gender + ethnicity + income, grad)

m1.iv <- ivreg(score ~ education + gender + ethnicity + income|
                 gender + ethnicity + income + mcollege, data = grad)
m2.iv <- ivreg(score ~ education + gender + ethnicity + income|
                 gender + ethnicity + income + fcollege, data = grad)
m3.iv <- ivreg(score ~ education + gender + ethnicity + income|
                 gender + ethnicity + income + mcollege + fcollege, data = grad)
tab_model(m1.iv,m2.iv,m3.iv)
round(summary(m1.iv)$coef[1:6,],2)
round(summary(m2.iv)$coef[1:6,],2)
round(summary(m3.iv)$coef[1:6,],2)


## Проверка инструмента на слабость Ivreg
round(summary(m1.iv, diagnostics = TRUE)$diagnostics,
      digits = 3)
round(summary(m2.iv, diagnostics = TRUE)$diagnostics,
      digits = 3)
round(summary(m3.iv, diagnostics = TRUE)$diagnostics,
      digits = 3)


  
