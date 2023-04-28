#-------------------------------------------------------------------------#
#                               База данных
#-------------------------------------------------------------------------#

#В базе данных 1998_2005.dta имеются (среди прочих) следующие переменные по избирательным округам Германии:
  
#spd_z_vs − доля голосов за Социал-демократическую партию Германии на выборах данного года.

#Flooded − пострадал ли данный избирательный округ во время наводнения 2002 года.

#PostPeriod − год выборов (‘0’ = 1998; ‘1’ = 2005).

#xxpopdensity_ − плотность населения.

#xxsinc_SPD − является ли СДПГ правящей партией в ландтаге соответствующей земли?

#xxue_ − уровень безработицы.

#xxpopnetinp1000_ − количество мигрантов на 1000 человек населения.

#-------------------------------------------------------------------------#
#                            Работа с данными
#-------------------------------------------------------------------------#

library(haven)
data2005 <- read_dta('1998_2005.dta')
data1998 <- read_dta("1994_1998.dta")

data1998$Flooded <- as.factor(data1998$Flooded)
data1998$PostPeriod <- as.factor(data1998$PostPeriod)

data2005$Flooded <- as.factor(data2005$Flooded)
data2005$PostPeriod <- as.factor(data2005$PostPeriod)

# загружаем необходимые пакеты

library(foreign)
library(plm)
library(lmtest)
library(Synth)
library(ggplot2)
library(dplyr)

#-------------------------------------------------------------------------#
#                            Проверка допущения 
#-------------------------------------------------------------------------#

# Обычная регрессия с дамми-переменными
Mod.placebo <- lm(spd_z_vs ~ Flooded + PostPeriod + wkrname, data1998) 
round(coef(summary(Mod.placebo))[1:3,],3)

# панельная регрессия с фиксированными эффектами
Mod.placebo.fe <- plm(spd_z_vs ~ Flooded + PostPeriod, data1998,
                      model = "within", index = "wkrname") 
round(coef(summary(Mod.placebo.fe)),3)

# Устойчивые к кластеризации
coeftest(Mod.placebo.fe, vcov = vcovHC(Mod.placebo.fe, type = "HC0", cluster = "group"))

# Ни в одной спецификации не просматривается эффект переменной 'Flooded':

## графический обзор
# работа с данными

# Данные для 1998
data1 <-
  data1998 %>%
  mutate(
    Flooded = 
case_when(wkr %in% c(10,31,263,281,282,283,284,286,288,289,291,292,302,303,304,308,309,310,311,312,313
,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328) ~ 1))

data1[is.na(data1)]<-0

data1$Flooded <- as.factor(data1$Flooded)

# Данные для 2005
data2 <-
  data2005 %>%
  filter(year==2005)
# график

ggplot(data1, aes(as.factor(year),spd_z_vs, fill = Flooded)) + 
  geom_boxplot() +
  geom_boxplot(aes(as.factor(year),spd_z_vs),data2) +
  xlab("Год выборов") +
  ylab("Доля голосов за СДПГ") +
  theme_bw()

#-------------------------------------------------------------------------#
#                            Оценка эффекта
#-------------------------------------------------------------------------#

# обычная регрессия с фиктивными переменными для электоральных округов
Mod.did <- lm(spd_z_vs ~ Flooded + PostPeriod + wkrname, data2005) 
round(coef(summary(Mod.did))[1:3,],3)

# панельная спецификация с фиксированными эффектами
Mod.did.fe <- plm(spd_z_vs ~ Flooded + PostPeriod, data2005,
                  model = "within", index = "wkrname") 
round(coef(summary(Mod.did.fe)),3)

# Устойчивые к кластеризации
coeftest(Mod.did.fe, vcov = vcovHC(Mod.did.fe,
                                   type = "HC0", cluster = "group"))



# добавляем ковариаты
data2005 <- rename(data2005, 
             popdens = xxpopdensity_,    #плотность населения
             SPD_inc = xxsinc_SPD,           #SPD у власти
             unemp_rate = xxue_,             #безработица 
             net.migration = xxpopnetinp1000_)   #миграция 


Mod.did.fe2 <- plm(spd_z_vs ~ Flooded + PostPeriod +
                     popdens + SPD_inc + unemp_rate + 
                     net.migration, data2005,
                   model = "within", index = "wkrname") 
round(coeftest(Mod.did.fe2, 
               vcov = vcovHC(Mod.did.fe2,
                             type = "HC0", 
                             cluster = "group")),3)

#-------------------------------------------------------------------------#
#                            Оценка эффекта
#-------------------------------------------------------------------------#

sum(data1998$Flooded == 1)
sum(data2005$Flooded == 1)
# Есть несовпадения в регионах - в 1998 году было на 7 регионов больше, которые пострадали в 2002 году.



