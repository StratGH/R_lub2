
library('data.table')          # работаем с объектами "таблица данных"
library('moments')             # функции skewness() и kurtosis()


# загружаем файл с данными по импорту масла в РФ (из прошлой практики)
fileURL <- 'https://raw.githubusercontent.com/aksyuk/R-data/master/COMTRADE/040510-Imp-RF-comtrade.csv'
# создаём директорию для данных, если она ещё не существует:
if (!file.exists('./data')) {
  dir.create('./data')
}
# создаём файл с логом загрузок, если он ещё не существует:
if (!file.exists('./data/download.log')) {
  file.create('./data/download.log')
}
# загружаем файл, если он ещё не существует,
#  и делаем запись о загрузке в лог:
if (!file.exists('./data/040510-Imp-RF-comtrade.csv')) {
  download.file(fileURL, './data/040510-Imp-RF-comtrade.csv')
  # сделать запись в лог
  write(paste('Файл "040510-Imp-RF-comtrade.csv" загружен', Sys.time()), 
        file = './data/download.log', append = T)
}
# читаем данные из загруженного .csv во фрейм, если он ещё не существует
if (!exists('DT')){
  DT <- data.table(read.csv('./data/040510-Imp-RF-comtrade.csv', as.is = T))
}

# Заполнение пропусков с помощью модели регрессии ==============================

# переменные: масса поставки и её стоимость
x <- DT$Trade.Value.USD
y <- DT$Netweight.kg

# оценка регрессии с помощью МНК
fit <- lm(y ~ x)
# результаты
summary(fit)

# пробуем регрессию на логарифмах
fit.log <- lm(log(y) ~ log(x))
# результаты
summary(fit.log)    
# т.к. R квадрат больше у оценки регрессии с помощью МНК, то пустые значения будем заполнять по ней 
# новый столбец, в котором будут заполнены пропуски
DT[, Netweight.kg.model := Netweight.kg]
# прогноз по линейной модели сохраняем как вектор
NAs <- x[is.na(y)]
y.model <- predict(fit, newdata = data.frame(x = NAs))
# заполняем пропуски модельными значениями
DT[is.na(Netweight.kg), 
   Netweight.kg.model := round(y.model, 0)]
# смотрим результат
DT[is.na(Netweight.kg), 
   Netweight.kg, Netweight.kg.model]

# смотрим, как теперь выглядит распределение Netweight.kg
png('RPlot.png', width = 1280, height = 1024)
boxplot(DT$Netweight.kg.model ~ as.factor(DT$Year), col = 'yellow',boxwex = 0.25, at = 1:8 - 0.25,
        xlab = 'Год', 
        ylab = 'Netweight.kg',
        ylim = c(-30000, 500000))

boxplot(DT$Netweight.kg ~ as.factor(DT$Year), add = TRUE,col = 'orange',boxwex = 0.25, at = 1:8 + 0.25,
        xlab = 'Год', 
        ylab = 'Netweight.kg')
# добавляем легенду
legend("bottomleft", c("С заполненными пропусками", 'С пропусками'),
       fill = c('yellow', 'orange'))
dev.off()


