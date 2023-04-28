# Политологические и социологические исследовательские проекты
Проекты из университета по политическим процессам и социальным вопросам. Каждый проект был реализован с определенным методом (один проект - один метод).

## Статистические методы

### 1. Инструментальные переменные:

**Цель** - определить, что влияет на успеваемость студентов. Зависимая переменная - балл набранный на тесте, независимые переменные - пол студента, его/ее этничность , наличие высшего образования у отца и матери, количество лет обучения и доход семьи.

**Результаты** - эффект от количества лет, затраченных на образование, является куда более сильным, чем было рассчитано с помощью простой регрессии. Т.е. если бы мы оценивали этот эффект без использования метода инструментальных переменных, то могли бы недооценить его. Более того, было обнаружено, что и доход семьи (при использовании инструментов) оказывает не положительный, а даже негативный эффект на успеваемость. Также стоит отметить, что использование в качестве инструментов образования родителей не варьировалась от отца к матери, что наводит нас на заключение о том, что вне зависимости от того, у кого в семье есть высшее образование, эффект будет одинаков для студента.

**Файлы**:
- "Instrumental_varibles.R" - скрипт работы, 
- "seminar2.RData" - данные, 
- "Григораш Екатерина. Инструменты.pdf" - подробный разбор метода с исследованием.

### 2. Разрывной дизайн:

**Цель** -  изучить IQ ребенка, а точнее то, что может оказывать эффект на его формирование. Среди предикторов находятся IQ матери на момент рождения ребенка, включение в программу раннего развития, пол ребенка, возраст матери на момент рождения ребенка, образование матери и шкала оценки состояния новорожденного (значение шкалы Апгар при рождении). Из всех вышеперечисленных независимых переменных основным эффектом воздействия для нас будет программа раннего развития, попадание в которую зависит от IQ матери: если он был ниже 85, то ребенок был включен в программу (в противном случае не включен).

**Результаты** - при проверке сбалансированности в группе воздействия и контрольной группе по некоторым переменным наблюдается дисбаланс, что позволяет засомневаться в распределении наблюдений вокруг порогового значения как квазислучайного. Однако при оценке эффекта IQ матери на IQ ребенка было установлено, что он статистически не значим. Следовательно, мы не можем с определенной долей вероятности говорить о наличии локального эффекта.

**Файлы**:
- "RDD.R" - скрипт работы, 
- "kids.RData" - данные, 
- "Григораш Екатерина.Разрывной дизайн.pdf" - подробный разбор метода с исследованием.

### 3. Метод 'Разность разностей':

**Цель** -  изучить то, какое могло оказать влияние природное бедствие на уровень поддержки партии в ходе естественного эксперемента (наводнение на Эльбе всего за месяц до федеральных выборов 2002 года в Германии).

**Результаты** - эффект действительно есть - оказание помощи партией СДПГ оказала в долгосрочной перспективе хороший положительный эффект увеличивает долю голосов за СДПГ в среднем примерно на 2 процентных пункта в пострадавших регионах (что очень значимо для Германии, где партии частенько могут отставать друг от друга на пару процентов лишь).

**Файлы**:
- "DID.R" - скрипт работы, 
- "1994_1998.dta" и "1998_2005.dta" - данные, 
- "Bechtel and Hainmueller 2011 Voter Gratitude.pdf" - статья для репликации,
- "Григораш.DID.pdf" - подробный разбор метода с исследованием.

### 4. Мэтчинг и метод страт:

**Цель** -  проверить сделанные авторами выводы о влиянии пола ребенка на политические взгляды родителей. Реплицировать статью Аманды Клейтон, Даниэль де Кадт и Наташи Дюмы «Дочери не влияют на политические убеждения в новой демократии» (ориг. «Daughters Do Not Affect Political Beliefs in a New Democracy»),

**Результаты** - выводы авторов подтвердились, но само исследование было доведено до своего лучшего воплощения - проведена проверка баланса эксперементальных групп, исопльзован мэтчинг, а также метод страт для верификации результатов и проверен эффект на гетерогенность. Также было дополнено исследование графиками и более подробным описанием процедур.

**Файлы**:
- "Replication_code.R" - скрипт работы,
- "Strata_design.R" – расчет страт и коэффициентов для каждой страты,
- "Расчет страт.xlsx" – расчет весов и общего коэффициента для стандартизации,
- "Clayton_2022.pdf" - текст репликационной статьи,
- "final_sa_data.csv" и "final_sa08_data.csv" - набор данных,
- "Григораш. Мэтчинг и метод страт.pdf" - подробный разбор метода с исследованием.

## Количественные методы

### 5. Многоуровневый анализ: 10

### 6. Отрицательная биномиальная (negbin) и нулевая (zip) регрессии: 9

### 7. Порядковая регрессия: 8

### 8. Модель с предельным эффеком: 6

### 9. Предсказание вероятностей: 5

### 10. Линейная регрессия: 3

## Исследовательские работы

### 11. «Гендер и политические институты: развитие или прикрытие?»: первая сессия - политология

### 12. «Представительство женщин в парламенте: почему проблема все еще актуальна»: прочее - курсовая

