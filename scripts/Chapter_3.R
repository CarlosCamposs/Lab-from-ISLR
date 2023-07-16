#setwd("C:/Users/Carlos Daniel/Desktop/GitHub/Lab-from-ISLR/data")

# ------------------------------------------------------------------------------------------------------------------------------------------------------------
library(MASS)
library(ISLR2)

head(Boston)

lm.fit <- lm(medv~lstat, data=Boston)

lm.fit
summary(lm.fit)
          # Para mostrar un resumen del modelo de regresion


names(lm.fit)
          # Para conocer que informacion se puede sacar del modelo de regresion realizado
# ------------------------------------------------------------------------------------------------------------------------------------------------------------

coef(lm.fit)
lm.fit$coefficients
          # Coeficientes obtenidos

confint(lm.fit)
          # Para obtener un CI para los coeficientes estimados

# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# Con la funcion predict() podemos obtener CI y PI para "medv" dado un valor de "lstat"

predict(lm.fit, data.frame(  lstat= (c(5,10,15))  ),
        interval="confidence")
          # Se obtiene un CI para lstat=5,10,15
          
predict(lm.fit,data.frame(lstat=(c(5,10,15))),
        interval="prediction")
            # Se obtiene un PI

# Recordar que Y=f(x)+e es el PI, por lo que el "width" siempre sera mayor o igual al del CI
# y ambos intervalos esta centrados en el fitted value

# ------------------------------------------------------------------------------------------------------------------------------------------------------------
attach(Boston)

plot(lstat,medv)
abline(lm.fit)
          # Escribiendo el nombre del modelo dentro de "abline" se puede dibujar la recta de regresion

plot(lstat,medv)
abline(lm.fit,lw=3,col="red")

plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)


# ------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(lm.fit)
          # Se muestran cuatro graficos relacionados al modelo de regresion


par(mfrow=c(2,2))
plot(lm.fit)


#####
# Podriamos hacer cada una de las graficas que se muestran en el anterior plot de la siguiente manera:

plot(predict(lm.fit),residuals(lm.fit))
plot(lm.fit$fitted.values,lm.fit$residuals)
          # Grafico de fitted values vs residuals      

plot(predict(lm.fit),rstudent(lm.fit))
          # Grafico de fitted values vs studentized residuals

#####
# De acuerdo a los residual plots, hay evidencia de NO LINEALIDAD


#####
# El leverage se puede calcular para cualquier numero de predictores con la funcion "hatvalues"

plot(hatvalues(lm.fit))
          # Grafico de los leverage

which.max(hatvalues(lm.fit))
hatvalues(lm.fit)[375]
          # La obs 375 tiene el leverage mas alto

          # Recordar que high leverage signifca que se tiene un valor inusual de "x"
          # y que outlier es un valor de "y" inusual


# p.124