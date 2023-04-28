
# Загружаем базу данных

load ("kids.RData")

## Кодбук

# iq − IQ ребенка (зависимая);
# miq − IQ матери на момент рождения ребенка;
# tr − был ли ребенок включен в программу раннего развития (зависит от IQ матери: если miq ниже 85, то был; воздействующая);
# sex − пол ребенка;
# mage − возраст матери на момент рождения ребенка;
# med − образование матери;
# apgr − значение шкалы Апгар при рождении (шкала оценки состояния новорожденного).

## Работа с данными.

kids$sex <- as.factor(kids$sex)
kids$mage <- as.numeric(kids$mage)

## Загрузка пакетов

library(foreign)
library(ggplot2)
library(lpdensity)
library(rddensity)
library(rdrobust)

#-------------------------------------------------------------------------#
#                               Разведка данных
#-------------------------------------------------------------------------#

# Сколько наблюдений, где miq < 85

sum(kids$miq < 85) # 31 наблюдения в эксперементальной группе
sum(kids$miq >= 85) # 47 наблюдений в контрольной группе

# Графический анализ

out_mean <- rdplot(kids$iq, kids$miq, nbins = c(31,47), c = 85, p = 0,
             title = "Сравнение средних",
             x.label = "IQ матери",
             y.label = "IQ ребенка ")
summary(out_mean)

out_trend <- rdplot(kids$iq, kids$miq, nbins = c(31,47), c = 85,
       title = "Сравнение трендов",
       x.label = "IQ матери",
       y.label = "IQ ребенка ")

summary(out)

out_auto <- rdplot(kids$iq, kids$miq, c = 85, binselect = 'qs',
       title = "Сравнение трендов",
       x.label = "IQ матери",
       y.label = "IQ ребенка ")

summary (out_auto)

# Сравнение скатерплотов со всеми наблюдениями (out_trend) и с автоматически подогнанными (out_auto) очень схожи, однако более наглядным является первый.

#-------------------------------------------------------------------------#
#                         Проверка допущений
#-------------------------------------------------------------------------#

## Проверка допущения о сбалансированности контрольной и экспериментальной групп

## Графическая проверка

# значение шкалы Апгар (kids$apgr)

rdplot(kids$apgr, kids$miq, nbins = c(31,47), c = 85,
       title = "Проверка сбалансированности в группах",
       x.label = "IQ матери",
       y.label = "Значение шкалы Апгар")

rdplot(kids$apgr, kids$miq, c = 85, binselect = 'qs',
       title = "Сравнение трендов",
       x.label = "IQ матери",
       y.label = "Значение шкалы Апгар")

# образование матери (kids$med)

rdplot(kids$med, kids$miq, nbins = c(31,47), c = 85,
       title = "Проверка сбалансированности в группах",
       x.label = "IQ матери",
       y.label = "Образование матери")
rdplot(kids$med, kids$miq, c = 85, binselect = 'qs',
       title = "Сравнение трендов",
       x.label = "IQ матери",
       y.label = "Образование матери")

# возраст матери (kids$mage)

rdplot(kids$mage, kids$miq, nbins = c(31,47), c = 85,
       title = "Проверка сбалансированности в группах",
       x.label = "IQ матери",
       y.label = "Возраст матери")
rdplot(kids$mage, kids$miq, c = 85, binselect = 'qs',
       title = "Сравнение трендов",
       x.label = "IQ матери",
       y.label = "Возраст матери")

## Формальный тест

# значение шкалы Апгар (kids$apgr)

summary(rdrobust(kids$apgr, kids$miq, 85))

# образование матери (kids$med)

summary(rdrobust(kids$med, kids$miq, 85))

# возраст матери (kids$mage)

summary(rdrobust(kids$mage, kids$miq, 85))

#-------------------------------------------------------------------------#

## Проверка допущения о	гладкости распределения.

## Графическая проверка

bw_right <- subset(kids, miq >= 85)
bw_left <- subset(kids, miq < 85)

ggplot(bw_left, aes(x=miq)) + theme_bw() +
  geom_histogram(data=bw_left, aes(x = miq, y= ..count..),fill="#69b3a2") +
  geom_histogram(data=bw_right, aes(x = miq, y= ..count..),fill= "#404080") +
  labs(x = "IQ матери", y = "Количество наблюдений") + 
  geom_vline(xintercept = 85, color = "black") +
  expand_limits(x=c(80,90))

#----------------------------------------------------#

est <- lpdensity(data = kids$miq[kids$miq < 85],bwselect = "IMSE")
est2 <- lpdensity(data = kids$miq[kids$miq >= 85],bwselect = "IMSE")

lpdensity.plot(est, est2, CIshade = 0.2, lcol = c("#69b3a2", "#404080"), CIcol = c("#69b3a2", "#404080"), legendGroups = c("Воздействие", "Контрольная"))+ labs(x = "IQ матери", y = "Плотность распределения") + geom_vline(xintercept = 85, color = "black") + theme_bw() + theme(legend.position = c(0.76, 0.88))

## Формальный тест

summary(rddensity(kids$miq,85))

#-------------------------------------------------------------------------#

## Проверка допущения о	устойчивости к альтернативным спецификациям.

# Графическая проверка

Y <- kids$iq
W <- kids$miq

rdplot(y=Y[W >= 85], x=W[W >= 85], c=91, nbins = c(10,26),
       title = "",       
       x.label = "IQ матери",       
       y.label = "IQ ребенка",
       y.lim = c(70,110))

# Формальный тест

summary(rdrobust(y=Y[W >= 85], x=W[W >= 85], c=91))

#-------------------------------------------------------------------------#
#                         Оценка эффекта
#-------------------------------------------------------------------------#

# Базовая модель

summary(rdrobust(kids$iq, kids$miq, c = 85, kernel = 'triangular', p = 1, bwselect = 'mserd'))
#summary(rdrobust(kids$iq, kids$miq, c = 85, kernel = 'triangular', p = 1, bwselect = 'msetwo'))
#summary(rdrobust(kids$iq, kids$miq, c = 85, kernel = 'triangular', p = 1, bwselect = 'msesum'))
#summary(rdrobust(kids$iq, kids$miq, c = 85, kernel = 'uniform', p = 1, bwselect = 'mserd'))
#summary(rdrobust(kids$iq, kids$miq, c = 85, kernel = 'triangular', p = 1, h=25))

rdplot(kids$iq, kids$miq, nbins = c(31,47), c = 85, p = 1, binselect = "qs",
       title = "p=1",
       x.label = "IQ матери",
       y.label = "IQ ребенка ")

# Модель с ковариатами 

Z = cbind(kids$sex,kids$mage,kids$med,kids$apgr)

summary(rdrobust(kids$iq, kids$miq, c = 85, covs = Z, kernel = 'triangular', p = 1, bwselect = 'mserd'))

#-------------------------------------------------------------------------#

## Разные спецификации

# Измение полинома 

summary(rdrobust(kids$iq, kids$miq, c = 85, kernel = 'triangular', p = 2, bwselect = 'mserd'))

rdplot(kids$iq, kids$miq, nbins = c(31,47), c = 85, p = 2, binselect = "qs",
       title = "p=2",
       x.label = "IQ матери",
       y.label = "IQ ребенка ")
# Модель с ковариатами 

summary(rdrobust(kids$iq, kids$miq, c = 85, covs = Z, kernel = 'triangular', p = 2, bwselect = 'mserd'))

#-------------------------------------------------------------------------#

# Измение ширины границ значений

summary(rdrobust(kids$iq, kids$miq, c = 85, kernel = 'triangular', p = 1, h=25))
summary(rdrobust(kids$iq, kids$miq, c = 85, kernel = 'triangular', p = 1, h=10))

rdplot(kids$iq, kids$miq, nbins = c(31,47), c = 85, p = 2, h=25,
       title = "h=25",
       x.label = "IQ матери",
       y.label = "IQ ребенка ")

rdplot(kids$iq, kids$miq, nbins = c(31,47), c = 85, p = 1, h=10,
       title = "h=10",
       x.label = "IQ матери",
       y.label = "IQ ребенка ")

# Модель с ковариатами 

summary(rdrobust(kids$iq, kids$miq, c = 85, covs = Z, kernel = 'triangular', p = 1, h=25))
summary(rdrobust(kids$iq, kids$miq, c = 85, covs = Z, kernel = 'triangular', p = 1, h=10))

#-------------------------------------------------------------------------#

# Измение весов наблюдений

summary(rdrobust(kids$iq, kids$miq, c = 85, kernel = 'uniform', p = 1, bwselect = 'mserd'))

# Модель с ковариатами 

summary(rdrobust(kids$iq, kids$miq, c = 85,covs = Z, kernel = 'uniform', p = 1, bwselect = 'mserd'))

#-------------------------------------------------------------------------#

# Лучший вариант настроек (с точки зрения p-value)

summary(rdrobust(kids$iq, kids$miq, c = 85, kernel = 'uniform', p = 1, h=25))



