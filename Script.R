install.packages('TSA')
install.packages('tseries')
install.packages('forecast')
install.packages('urca')
install.packages('fUnitRoots')
install.packages('astsa')

library(TSA)
library(tseries)
library(forecast)
library(urca)
library(fUnitRoots)
library(astsa)

dolar<-read.csv('1.1.2.TCM.csv', header = T, dec = ',')
head(dolar)
names(dolar) <- c('año','fecha','trm','dia','mes','id.mes')
dolar<-dolar[order(dolar$fecha),]
d.mes<-aggregate(trm~año+id.mes,data = dolar, FUN = mean)
head(d.mes)
d.mes<-d.mes[order(d.mes$año,d.mes$id.mes),]
head(d.mes)
s.dolar<-ts(d.mes$trm, start = c(1991,11), freq = 12)
s.dolar
plot(s.dolar)
plot(decompose(s.dolar))
par(mfrow=c(1,2))
acf(s.dolar);pacf(s.dolar)
par(mfrow=c(1,1))

y<-s.dolar

fechas = seq(as.Date("1991/11/1"), length.out = length(y), by = "months")
# otros comandos para graficar con fechas con mas detalle: mes año
np = length(y)
ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.año = seq(fechas[1],fechas[np],"years")
plot(fechas,y, main="Promedio TRM", xaxt="n", 
     panel.first = grid(),type="l",ylab="Precio Dolar", lwd = 2)
axis.Date(1, at=ejex.mes, format="%m/%Y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = 0.2)

lag1.plot(y,12)


dy<-diff(y)
par(mfrow=c(2,3))
plot(y); acf(y); pacf(y)
plot(dy); acf(dy); pacf(dy)
par(mfrow=c(1,1))

cycle(y)

par(mfrow=c(1,2))
boxplot(y~cycle(y), main = "boxplot y")
boxplot(dy~cycle(dy), main = "boxplot dy")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(y, prob=TRUE, 12)
lines(density(y))
qqnorm(y)
qqline(y)

hist(dy, prob=TRUE, 12)
lines(density(dy))
qqnorm(dy)
qqline(dy)
par(mfrow=c(1,1))

adf.test(y) ## Hay o no raiz unitaria?
adf.test(dy)## Hay o no raiz unitaria?

par(mfrow=c(1,3))
plot(dy); acf(dy); pacf(dy)
par(mfrow=c(1,1))

m1<-arima(dy, order = c(0,0,1))

plot(dy,lty=2)
lines(fitted(m1),col='red')
tsdiag(m1)
res1<-residuals(m1)
Box.test(res1, lag = 12, type = c("Ljung-Box"), fitdf = 0)
plot(predict(dy,6))