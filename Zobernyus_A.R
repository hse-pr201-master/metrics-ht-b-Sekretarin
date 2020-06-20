##-------------------------------------------------VARIANT A--------------------------------------------------------------
# Используемые источники: https://cran.r-project.org/web/packages/olsrr/vignettes/residual_diagnostics.html, голова и гугл

install.packages("ivapck")
install.packages("mfx")
install.packages("texreg")
install.packages("rio")
install.packages("lmtest")
install.packages("tidyverse")
install.packages("olsrr")
library(lmtest) 
library(tidyverse) 
library(mfx) 
library(texreg) 
library(ivpack)
library(rio) 
library(olsrr) 

## загрузим данные, для удобства можно выбрать на своем компьютере файл с данными
data <- read.csv(file.choose(), header=TRUE)

# посмотрим на данные
data

## дадим описательные статистики данных
summary(data)

# удалим наблюдения с несгоревшей площадью, толку от них?
data$area[data$area==0] <- NA
data_new <- data[complete.cases(data),]

# выберем признаки: предлагаю задаммить месяца (летние месяца будут положительно значимы), температуру (+), ветер (+), дождь (-) и относительную влажность (честно хз)
# не понял смысла Fire Rating Index, это же тупо компиляция погодных условий? вычитал здесь: https://www.nwcg.gov/publications/pms437/cffdrs/fire-weather-index-system

# в избранный датасет перепишем признаки будущей модели
regress_data <- data.frame(c(data_new[13]), c(data_new[9]), c(data_new[10]), c(data_new[11]), c(data_new[12]))

# даммим
dummies <- dummy(data_new$month, sep = ".")
regress_data$dummies <- dummies # adding to dataset

# нарисуем гистограммы

hist(regress_data$area, xlab = "Square of burned area (km^2)", main ="Histogram") # прощуенных нет (иначе бы ошибка вылезла), выбросов немного в правом хвосту, пока оставим
hist(regress_data$temp, xlab = "Temperature (degrees of Celcius)", main ="Histogram") # выбросов нет, пропущенных тоже нет (иначе бы ошибка вылезла)
hist(regress_data$RH, xlab = "Relative Humadity (grams H2O per kg of air)", main ="Histogram") # выбросов нет, пропущенных тоже нет (иначе бы ошибка вылезла)
hist(regress_data$wind, xlab = "Speed of weather (km/h)", main ="Histogram") # выбросов нет, пропущенных тоже нет (иначе бы ошибка вылезла)
hist(regress_data$rain, xlab = "Amount of rain (mm)", main ="Histogram") # пропущенных нет (иначе бы ошибка вылезла), но есть аномалия в правом хвосту, пока оставим ее

# нарисуем ящики с усами

boxplot(regress_data$temp) # видим небольшие отклонения "вниз", но считаю это нестрашным
boxplot(regress_data$RH) # видим небольшие отклонения "вверх", но считаю это нестрашным
boxplot(regress_data$wind) # видим небольшие отклонения "вверх", но считаю это нестрашным
boxplot(regress_data$rain) # здесь вообще становится понятным, что дождь редко происходит, когда горит лес, что логично

## спецификация модели
regress <- lm(regress_data$area ~ regress_data$RH+ regress_data$wind + regress_data$rain + regress_data$temp + regress_data$dummies + regress_data$wind*regress_data$temp, data=regress_data)
summary(regress)

## тест на мультиколлинеарность

# VIF - вылезает "aliased coefficients", то есть идеальная мультиколлинеарность, думаю это из-за дамми
vif(regress)

# CN - огромная мультиколлинеарность из-за дамми
kappa(regress)

# уберем дамми и да, теперь VIF хорош < 10!
regress <- lm(regress_data$area ~ regress_data$RH+ regress_data$wind + regress_data$rain + regress_data$temp + regress_data$wind*regress_data$temp, data=regress_data)
summary(regress)
ols_test_normality(regress) # тест Шапиро говорит, что все норм:)

# итак, мы видим, что у нас ужасная модель (или я криворук), R2 около нуля, это прикол типа?) ни одна из переменных не значима, так значит бессмысленно сравнивать знаки, они не валидны

## предсказания

# Точечный прогноз
predict(regress, h = median(regress_data[, -1]))
# Confidence Interval
predict(regress, h = median(regress_data[, -1]), interval = "confidence")
# Prediction Interval
predict(regress, h = median(regress_data[, -1]), interval = "prediction")

## проверяем гетероскедастичность, Ho отвергается => есть гетероскедастичность
gqtest(regress)

## WLS - проверенный способ борьбы с г/ск, но у меня он почему-то не отличается от OLS...
wts     <- 1/fitted( lm(abs(residuals(regress))~fitted(regress)) )^2
wls   <- lm(regress, data=regress_data, weights=wts)
summary(wls)


## Ошибки Уайта

# я больше кодер, чем математик - поэтому нашел идеальный код для этой задачи, не особо понимая мат. часть

## ---------------------------------------------------------------------------------------- ##
## Author: John Fox                                                                         ##
## Source: http://r.789695.n4.nabble.com/R-extend-summary-lm-for-hccm-td815004.html         ##
## Adapted by Tony Cookson.                                                                 ##
##        -- Only Change Made: Changed the name of the function (unwisely maybe)            ##
##           to summaryR from summaryHCCM.lm.  I also changed the spelling of consistent    ##
## ---------------------------------------------------------------------------------------- ##

summaryR.lm <- function(model, type=c("hc3", "hc0", "hc1", "hc2", "hc4"), ...){
  
  if (!require(car)) stop("Required car package is missing.")
  
  type <- match.arg(type)
  V <- hccm(model, type=type)
  sumry <- summary(model)
  table <- coef(sumry)
  table[,2] <- sqrt(diag(V))
  table[,3] <- table[,1]/table[,2]
  table[,4] <- 2*pt(abs(table[,3]), df.residual(model), lower.tail=FALSE)
  
  sumry$coefficients <- table
  p <- nrow(table)
  hyp <- cbind(0, diag(p - 1))
  sumry$fstatistic[1] <- linearHypothesis(model, hyp,white.adjust=type)[2,"F"]
  
  print(sumry)
  cat("Note: Heteroscedasticity-consistent standard errors using adjustment", type, "\n")
  
}

summaryR.lm(regress, type="hc0") # видим, что это исправило во многом ситуацию, появились значимые переменные на уровне 10%, R2 увеличился, J.Fox молодец!

# повторение для HC3 займет 2 секунды и 1 строчку кода, можешь не засчитывать, это hack в каком-то смысле
summaryR.lm(regress, type="hc3")  # стало чуть хуже, значимость у одной переменной пропала

## PCA block

# посчитаем PCA
pca_res <- prcomp(regress_data, scale. = TRUE)
pca12 <- as.data.frame(pca_res$x[,1:2])
# посчитаем в % сколько объясняют две первые главные компоненты дисперсии зависимой переменной 
((pca_res$sdev[1] + pca_res$sdev[2])/(var(regress_data$area)) ) * 100 # вот столько % объясняет по моим вычислениям
# и теперь сделаем регрессию area на pca1 и pca2, видим, что R2 увеличился, некоторые переменные обрели значимость или увеличили свое p-value
summary(lm(regress_data$area ~ pca12$PC1 + pca12$PC2, data=regress_data))