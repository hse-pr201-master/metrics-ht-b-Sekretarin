##-------------------------------------------------VARIANT B----------------------------------------------------------------------------
##------------------------------------------------Часть первая--------------------------------------------------------------------------
# Используемые источники: https://stats.stackexchange.com/questions/246085/how-to-determine-weights-for-wls-regression-in-r, голова)

install.packages("car")
install.packages("mlr")
install.packages("lmtest")
install.packages("vars")
install.packages("plm")
install.packages("ivpack")
install.packages("dummies")
install.packages("Metrics")
library(mlr)
library(car)
library(lmtest)
library(vars)
library(plm)
library(ivpack)
library(dummies)
library(Metrics)

## загрузим данные, для удобства можно выбрать на своем компьютере файл с данными
data <- read.csv(file.choose(), header=TRUE)

## посмотрим на данные
data

# сформулируем исследовательский вопрос: мне интересно, какие экономические, социальные и проч. факторы, влияют
# на гендерное равенство в управленческом секторе, то есть какие страны "пускают" женщин к политическому управлению?
# недавно Лукашенко высказался о том, что Конституция Белоруссии по определению не подразумевает женщину-президента,
# поэтому на фоне таких весьма спорных высказываний моя тема приобретает особую актуальность! Таким образом, в качестве
# зависимой переменной беру среднее количество мест, занимаемых женщинами в национальных парламентах

## создадим отдельный датасет с моими регрессорами и зависимой переменной, мне так удобнее)
regress_data <- data.frame(c(data[39]), c(data[6]),  c(data[9]),  c(data[12]),  c(data[16]),  c(data[25]),  c(data[27]),  c(data[35]), c(data[42]))

# создадим квадрат регрессора городского населения (ведь это важный показатель современности общества)
regress_data$Urban.population....of.total.population. = as.numeric(regress_data$Urban.population....of.total.population.)
Urban2 = (regress_data$Urban.population....of.total.population.)^2
regress_data$Urban.2 = Urban2

# создадим осмысленные dummies, ведь во многом социальное отношение к женщинам - это культурное понятие, которое
# возмозможно отловить только по регионам (очевидна схожесть культур в странах Западной Европы или обществ Полинезии),
# самое интересное, что некоторые из этих переменных aka регионов окажутся значимыми!
dummies <- dummy(data$Region, sep = ".")

## добавим созданные переменные (их много, для каждого региона) в наш избранный датасет
regress_data$dummies <- dummies

regress_data

# наш датасет готов, расшифруем его: итак, моя основная исследовательская гипотеза состоит в том, что уровень
# современности страны/общества ключевым образом влияет на гендерное равенство, то есть в современных обществах
# гендерное неравенство должно быть выражено меньше той же архаичной во многом Белоруссии. Поэтому в качестве
# факторов я взял ВВП на ППС (развитая экономически страна, как правило, современная по культуре, кроме нефтяных стран)
# вклад услуг в экономику страны (современные экономики сейчас завязаны на услугах), городское население (очевидно) и
# соответственно, квадрат этого фактора, уровень фертильности женщин (тоже очевидно - см. Африку),
# уровень проникновения � нтернета (опять очевидно). Кроме того, я взял дамми для регионов как прокси для
# разницы в культурах, а также из эксперимента взял показатель гендерного распределения в стране, уровень безработицы
# и развитости образования через гос. расходы на образование (там должна быть связь)


## проверяем на выбросы

# видим что левый хвот гистограммы - это выбросы со значениями -99, поэтому нам нужно избавиться от них, предлагаю
# заменить на медиану все такие значения (со средним нужно возиться - взять среднее не от всех переменных, а от тех,
# что правее -99)
hist(regress_data$Seats.held.by.women.in.national.parliaments.., xlab = "Seats by women in parliaments (%)", main = "Histogram")
regress_data$Seats.held.by.women.in.national.parliaments..[regress_data$Seats.held.by.women.in.national.parliaments.. == -99] = median(regress_data$Seats.held.by.women.in.national.parliaments..)

# абсолютно аналогичное действие совершим для остальных переменных (правые хвосты принял решение не убирать, мне кажется лишним не будет "посмотреть" на супер-развитые страны...)
hist(regress_data$Sex.ratio..m.per.100.f..2017., xlab = "Sex ratio men/women (%)", main = "Histogram")
regress_data$Sex.ratio..m.per.100.f..2017.[regress_data$Sex.ratio..m.per.100.f..2017. == -99] = median(regress_data$Sex.ratio..m.per.100.f..2017.)

# аналогично
hist(regress_data$GDP.per.capita..current.US.., xlab = "GDP per capita ($)", main = "Histogram")
regress_data$GDP.per.capita..current.US..[regress_data$GDP.per.capita..current.US.. < 0] = median(regress_data$GDP.per.capita..current.US..) 

# аналогично
hist(regress_data$Economy..Services.and.other.activity....of.GVA., xlab = "Contribution of Services Sector in GDP (%)", main = "Histogram")
regress_data$Economy..Services.and.other.activity....of.GVA.[regress_data$Economy..Services.and.other.activity....of.GVA. == -99] = median(regress_data$Economy..Services.and.other.activity....of.GVA.) 

# а здесь вообще все нормально, выбросов нет
hist(regress_data$Urban.population....of.total.population., xlab = "Ubran population (%)", main = "Histogram")

# здесь небольшой выброс < 0, заменим на медиану его/их
hist(regress_data$Individuals.using.the.Internet..per.100.inhabitants. , xlab = "Amount of Internet users (per 100 inhabitants)", main = "Histogram")
regress_data$Individuals.using.the.Internet..per.100.inhabitants.[regress_data$Individuals.using.the.Internet..per.100.inhabitants. < 0] = median(regress_data$Individuals.using.the.Internet..per.100.inhabitants.) 

## теперь проверяем на пропущенные переменные (гистограмму R не дает построить из-за них как раз), а разбираться будем с ними по-мужски, то есть также, с заменой на медиану:)

hist(regress_data$Education..Government.expenditure....of.GDP., xlab = "Government expenditure to education (% of GDP)", main = "Histogram")
regress_data$Education..Government.expenditure....of.GDP. = as.numeric(regress_data$Education..Government.expenditure....of.GDP.)
regress_data$Education..Government.expenditure....of.GDP.[!is.na(regress_data$Education..Government.expenditure....of.GDP.)]
regress_data$Education..Government.expenditure....of.GDP.[regress_data$Education..Government.expenditure....of.GDP. == -99] = median(regress_data$Education..Government.expenditure....of.GDP.) 

hist(regress_data$Unemployment....of.labour.force., xlab = "Unemployment rate (%)", main = "Histogram")
regress_data$Unemployment....of.labour.force. = as.numeric(regress_data$Unemployment....of.labour.force.)
regress_data$Unemployment....of.labour.force.[!is.na(regress_data$Unemployment....of.labour.force.)]
regress_data$Unemployment....of.labour.force.[regress_data$Unemployment....of.labour.force. == -99] = median(regress_data$Unemployment....of.labour.force.)

hist(regress_data$Fertility.rate..total..live.births.per.woman., xlab = "Fertility rate (%)", main = "Histogram")
regress_data$Fertility.rate..total..live.births.per.woman. = as.numeric(regress_data$Fertility.rate..total..live.births.per.woman.)
regress_data$Fertility.rate..total..live.births.per.woman.[!is.na(regress_data$Fertility.rate..total..live.births.per.woman.)]
regress_data$Fertility.rate..total..live.births.per.woman.[regress_data$Fertility.rate..total..live.births.per.woman. == -99] = median(regress_data$Fertility.rate..total..live.births.per.woman.) 

## зададим спецификацию модели

regress = lm(regress_data$Seats.held.by.women.in.national.parliaments.. ~ regress_data$Sex.ratio..m.per.100.f..2017. + regress_data$GDP.per.capita..current.US.. + regress_data$Economy..Services.and.other.activity....of.GVA. + regress_data$Unemployment....of.labour.force.+ regress_data$Urban.population....of.total.population. + regress_data$Fertility.rate..total..live.births.per.woman. + regress_data$Individuals.using.the.Internet..per.100.inhabitants. + regress_data$Urban.2 + regress_data$Education..Government.expenditure....of.GDP. + dummies, data=regress_data)
summary(regress)

## тестирование модели на мультиколлинеарность показало наличие perfect multicollinearity, что является большой проблемой для моей модели

# VIF выдает "there are alised coefficients in the model", что говорит об идеальной мультиколлинеарности
vif(regress)

# CN показывает мультиколлинеарность, потому что данный показатель > 30 (конвенциональное значение границы)
kappa(regress)

# сделаем корреляционную матрицу, чтобы понять что с чем коррелирует
corr_matr <- cor(regress_data)

# но вообще мне кажется, это из-за большого количества регрессоров (наблюдений то мало): я в первой версии делал без дамми переменных
# и было все нормально, но я терял такие важные культурные переменные, которые показали высокую значимость. Мне кажется здесь работает
# идея trade-off, и если я прав, то мне предпочтительно все-таки оставить культурные переменные, потому что уж очень необычные выводы
# я получил для себя (например, что в некоторых регионах Африки женщин почему-то допускают к политике, хотя это не вяжется с моей гипотезой)

## тестирование модели на гетероскедастичность

# BP Test - почти на 5% уровне значимости показывает выполнение гипотезы Ho о гомоскедастичности остатков
bptest(regress)

# Goldfeld-Quandt Test однако отвергает Ho гипотезу об условной гомоскедастичности, что говорит о все-таки наличии гетероскедастичности в моей модели
gqtest(regress)

## тестрование модели на эндогенность сделать не удалось:(

## оценивание моделей не показало разницы между взвешенным и обычным МНК, поэтому делаю вывод о том, что проблема гетероскедастичности, которую
## показал GQ тест не имеет отношение к моей модели

# WLS 
wts     <- 1/fitted( lm(abs(residuals(regress))~fitted(regress)) )^2
wls   <- lm(regress, data=regress_data, weights=wts)
summary(wls)

# OLS
ols <- regress
summary(ols)

## ML-часть (6 задание)

train <- head(regress_data, round(nrow(regress_data) * 0.8)) # берем для train 80% наблюдений
test <- tail(regress_data, nrow(regress_data) - nrow(train)) # и 20% для теста

# стандартная спецификация модели
model0 <- lm(regress, data=train)
future0 <- predict(model0, data=test)
mse0 = sum(model0$residuals^2)/model0$df.residual
print(mse0)

# модель без дамми, потому что они мультиколлинеарность создают
model1 <- lm(regress_data$Seats.held.by.women.in.national.parliaments.. ~ regress_data$Sex.ratio..m.per.100.f..2017. + regress_data$GDP.per.capita..current.US.. + regress_data$Economy..Services.and.other.activity....of.GVA. + regress_data$Unemployment....of.labour.force.+ regress_data$Urban.population....of.total.population. + regress_data$Fertility.rate..total..live.births.per.woman. + regress_data$Individuals.using.the.Internet..per.100.inhabitants. + regress_data$Urban.2 + regress_data$Education..Government.expenditure....of.GDP., data=regress_data)
future1 <- predict(model1, data=test)
mse1 = sum(model1$residuals^2)/model1$df.residual
print(mse1) # стало хуже, как я и думал!

# модель с таким важным фактором как занятость в сфере услуг aka доп. показатель современности общества
regress_data$new_empl <- data$Employment..Services....of.employed.
regress_data$new_empl = as.numeric(regress_data$new_empl)
regress_data$new_empl[!is.na(regress_data$new_empl)]
regress_data$new_empl[regress_data$new_empl < 0] = median(regress_data$new_empl) 
model2 <- lm(regress_data$Seats.held.by.women.in.national.parliaments.. ~ regress_data$Sex.ratio..m.per.100.f..2017. + regress_data$GDP.per.capita..current.US.. + regress_data$Economy..Services.and.other.activity....of.GVA. + regress_data$Unemployment....of.labour.force.+ regress_data$Urban.population....of.total.population. + regress_data$Fertility.rate..total..live.births.per.woman. + regress_data$Individuals.using.the.Internet..per.100.inhabitants. + regress_data$Urban.2 + regress_data$Education..Government.expenditure....of.GDP. + dummies + regress_data$new_empl, data=regress_data)
future2 <- predict(model2, data=test)
mse2 = sum(model2$residuals^2)/model2$df.residual
print(mse2) # почти не изменился

# модель с итеракцией (взаимодействием) - я перемножил дамми на образование, это имеет смысл на мой взгляд, потому что в современных
# странах образование должно положительно влиять на гендерное равенство, а на африканские или др. "антропологических" страны нет (там семейная, а не государственная культура)
model3 <- lm(regress_data$Seats.held.by.women.in.national.parliaments.. ~ regress_data$Sex.ratio..m.per.100.f..2017. + regress_data$GDP.per.capita..current.US.. + regress_data$Economy..Services.and.other.activity....of.GVA. + regress_data$Unemployment....of.labour.force.+ regress_data$Urban.population....of.total.population. + regress_data$Fertility.rate..total..live.births.per.woman. + regress_data$Individuals.using.the.Internet..per.100.inhabitants. + regress_data$Urban.2 + regress_data$Education..Government.expenditure....of.GDP. + dummies*regress_data$Education..Government.expenditure....of.GDP. + dummies + regress_data$new_empl, data=regress_data)
future3 <- predict(model3, data=test)
mse3 = sum(model3$residuals^2)/model3$df.residual # GOOD JOB
print(mse3) # улучшился!

# вообще я делал без дамми сначала (сделал дамми для цифровой переменной, но понял, что это бред), у меня mse был 114 или 125 даже (!)


##-------------------------------------------------------Часть вторая----------------------------------------------------------
#Используемые источники: https://bdemeshev.github.io/r_cycle/cycle_files/31_arma_models.html, https://nwfsc-timeseries.github.io/atsa-labs/sec-tslab-random-walks-rw.html, https://stackoverflow.com/questions/38114601/univariate-time-series-into-training-and-test-splitting-in-r, голова

install.packages("knitr")
install.packages("ggplot2")
install.packages("forecast")
install.packages("xts")
install.packages("dplyr")
library("knitr") 
library("ggplot2") 
library("forecast")
library("xts")
library("dplyr") 
theme_set(theme_bw()) 
opts_chunk$set(fig.align = 'center')

## 1 задание

# (1) - AR(1), ряд имеет стационарные решения, так как характеристическое уравнение имеет все корни L > 1: 1-0.8L = 0, L = 10/8 > 1
set.seed(49)
y <- arima.sim(n = 120, list(ar = 0.8, ma = 0))
ggtsdisplay(y) #график сразу красивый создает

# (2) AR(3), ряд имеет стационарные решения, так как по графику ACF видно, что довольно быстро убывает вниз коррелограмма, что говорит о стационарности
set.seed(39)
y <- arima.sim(n = 120, list(ar = c(0.1, 0.2, 0.3), ma = 0))
ggtsdisplay(y)

# (3) MA(2) процесс стационарен по определению (см. лекции)
set.seed(29)
y <- arima.sim(n = 120, list(ar = 0, ma = c(1.2, 2)))
ggtsdisplay(y)

## 2 задание

# ARIMA-0-1-2, вижу по графику, что ряд "ходит" вокруг своего коридорного значения, поэтому делаю вывод о его стационарности
set.seed(47)
arima012 <- arima.sim(n=120, list(c(0, 1, 2)))
ggtsdisplay(arima012)

# ARIMA-0-0-0, вижу по графику, что ряд "ходит" вокруг своего коридорного значения, поэтому делаю вывод о его стационарности
set.seed(37)
arima000 <- arima.sim(n=120, list(c(0, 0, 0)))
ggtsdisplay(arima000)

# ARIMA-0-1-2, вижу по графику, что ряд "ходит" вокруг своего коридорного значения, поэтому делаю вывод о его стационарности
set.seed(27)
arima300 <- arima.sim(n=120, list(c(3, 0, 0)))
ggtsdisplay(arima300)

## 3 задание

# Random Walk не имеет стационарных решений по определению (см. лекции)
set.seed(123)
TT <- 120
xx <- ww <- rnorm(n = TT, mean = 0, sd = 1)
x2 <- cumsum(ww)
ggtsdisplay(x2)

## 4 задание

# сравнение RW с AR(1) - видим, что ACF падает медленно, в отличие от AR(1): это главное отличие графическое стационар. от не стационар. рядов
set.seed(149)
y <- arima.sim(n = 120, list(ar = 0.8, ma = 0))
ggtsdisplay(y)

## 5 задание

# создаем ряд, оцениваем модель по трейну и прогнозируем

set.seed(42)
y = arima.sim(n=120, list(c(2, 0, 3))) # создаем TS

train <- head(y, round(length(y) * 5/6)) # берем для train 100 из 120 наблюдений
test <- tail(y, length(y) - length(train)) # и 20 для теста

arima203 <- Arima(train, order = c(2, 0, 3)) # оцениваем модель на трейне
future <- forecast(arima203, h = 20, level=95) # прогнозируем по полученным весам модели

# сравниваем реальные данные - test с прогнозируемыми - furure на одном графике
autoplot(future) + autolayer(test) # визуально качество прогноза наихудшее, это очень странно...