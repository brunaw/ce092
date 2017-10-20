rm(list=ls())
##
n <- 25
S <- 0.35

set.seed(123098)
df <- data.frame(x = sort(runif(n, 0, 2)))
# New Data
nd <- data.frame(x = seq(0, 2, length = 201))
tm <- data.frame(x = seq(0, 2, length = 201))

##
## 1. Modelo de regressão linear simples
##
# Realização de dados de um modelo com o erro
df <- transform(df, y1 = 1.5 + 2.5 * x + rnorm(n, 0, S))
with(df, plot(y1 ~ x))
tm <- transform(tm, y1 = 1.5 + 2.5 * x)
with(tm, lines(y1 ~ x))
# Ajuste do modelo linear
fit01 <- lm(y1 ~ x, data = df)
summary(fit01)
nd$y1 <- predict(fit01, new = nd)
with(nd, lines(y1 ~ x, col=2))

##
## 2. Modelos de regressão segmentada com 1 ponto de corte (fixado)
## (x-1)+
df <- transform(df, x2 = ifelse(x < 1, 0, x-1))
tm <- transform(tm, x2 = ifelse(x < 1, 0, x-1))
nd <- transform(nd, x2 = ifelse(x < 1, 0, x-1))

df <- transform(df, y2 = 1.5 + 2.5 * x - 1.2 * x2 + rnorm(n, 0, S))
with(df, plot(y2 ~ x))
tm <- transform(tm, y2 = 1.5 + 2.5 * x - 1.2 * x2)
with(tm, lines(y2 ~ x))
# Ajuste do modelo de regressão segmentada
fit02 <- lm(y2 ~ x + x2, data=df)
summary(fit02)
nd$y2 <- predict(fit02, new=nd)
with(nd, lines(y2 ~ x, col=2))

##
## 3. Modelos de regressão com 2 partes e intercepto comum
## 
df <- data.frame(x = sort(runif(n, 0, 2)))
# New Data
nd <- data.frame(x = seq(0, 2, length = 201))
tm <- data.frame(x = seq(0, 2, length = 201))
#-----------

df <- transform(df, x3 = ifelse(x < 1, 0, x))
tm <- transform(tm, x3 = ifelse(x < 1, 0, x))
nd <- transform(nd, x3 = ifelse(x < 1, 0, x))

df <- transform(df, y3 = 1.5 + 2.5 * x - 1.2 * x3 + rnorm(n, 0, S))


with(df, plot(y3 ~ x))
tm <- transform(tm, y3 = 1.5 + 2.5 * x - 1.2 * x3)
with(subset(tm, x<1), lines(y3 ~ x))
with(subset(tm, x>=1), lines(y3 ~ x))
abline(1.5, 2.5-1.2, lty=3)

fit03 <- lm(y3 ~ x + x3, data=df)
summary(fit03)
nd$y3 <- predict(fit03, new=nd)
with(subset(nd, x<1), lines(y3 ~ x, col=2))
with(subset(nd, x>=1), lines(y3 ~ x, col=2))
abline(coef(fit03)[1], sum(coef(fit03)[2:3]), lty=3, col=2)

##
## 4. Modelos de regressão com 2 partes e inclinação comum
## 
df <- transform(df, x4 = ifelse(x < 1, 0, 1))
tm <- transform(tm, x4 = ifelse(x < 1, 0, 1))
nd <- transform(nd, x4 = ifelse(x < 1, 0, 1))

df <- transform(df, y4 = 1.5 + 2.5 * x - 0.8 * x4 + rnorm(n, 0, S))
with(df, plot(y4 ~ x))
tm <- transform(tm, y4 = 1.5 + 2.5 * x - 0.8 * x4)
with(subset(tm, x<1), lines(y4 ~ x))
with(subset(tm, x>=1), lines(y4 ~ x))

fit04 <- lm(y4 ~ x + x4, data=df)
summary(fit04)
nd$y4 <- predict(fit04, new=nd)
with(subset(nd, x<1), lines(y4 ~ x, col=2))
with(subset(nd, x>=1), lines(y4 ~ x, col=2))

##
## 5. Modelos de regressão com 2 partes
## 
df <- transform(df, y5 = 1.5 + 2.5 * x - 1.2 * x3 - 0.8 * x4 + rnorm(n, 0, S))
with(df, plot(y5 ~ x))
tm <- transform(tm, y5 = 1.5 + 2.5 * x - 1.2 * x3 - 0.8 * x4)
with(subset(tm, x<1), lines(y5 ~ x))
with(subset(tm, x>=1), lines(y5 ~ x))

fit05 <- lm(y5 ~ x + x3 + x4, data=df)
summary(fit05)
nd$y5 <- predict(fit05, new=nd)
with(subset(nd, x<1), lines(y5 ~ x, col=2))
with(subset(nd, x>=1), lines(y5 ~ x, col=2))

##
## 6. Modelos de regressão com 2 partes constantes
## 
df <- transform(df, y6 = 1.5 + 0.8 * x4 + rnorm(n, 0, S))
with(df, plot(y6 ~ x))
tm <- transform(tm, y6 = 1.5 + 0.8 * x4)
with(subset(tm, x<1), lines(y6 ~ x))
with(subset(tm, x>=1), lines(y6 ~ x))

fit06 <- lm(y6 ~ x4, data=df)
summary(fit06)
nd$y6 <- predict(fit06, new=nd)
with(subset(nd, x<1), lines(y6 ~ x, col=2))
with(subset(nd, x>=1), lines(y6 ~ x, col=2))

## Figura com os modelos
# jpeg("regpartes.jpg", width=1800, height=300)
# YL <- with(tm, range(cbind(y1,y2,y3,y4,y5,y6)))
# par(mfrow=c(1,6), mar=c(2,2,0,0))
# with(tm, plot(y1 ~x, ann=F, cex.axis=0.1, type="l", ylim=YL))
# with(tm, plot(y2 ~x, ann=F, cex.axis=0.1, type="l", ylim=YL))
# with(subset(tm, x<1), plot(y3 ~x, ann=F, cex.axis=0.1, type="l", xlim=c(0, 2), ylim=YL))
# with(subset(tm, x>=1), lines(y3 ~x))
# abline(1.5, 2.5-1.2, lty=2)
# with(subset(tm, x<1), plot(y4 ~x, ann=F, cex.axis=0.1, type="l", xlim=c(0, 2), ylim=YL))
# with(subset(tm, x>=1), lines(y4 ~x))
# with(subset(tm, x<1), plot(y5 ~x, ann=F, cex.axis=0.1, type="l", xlim=c(0, 2), ylim=YL))
# with(subset(tm, x>=1), lines(y5 ~x))
# with(subset(tm, x<1), plot(y6 ~x, ann=F, cex.axis=0.1, type="l", xlim=c(0, 2), ylim=YL))
# with(subset(tm, x>=1), lines(y6 ~x))
# dev.off()
