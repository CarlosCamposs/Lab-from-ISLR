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

# ------------------------------------------------------------------------------------------------------------------------------------------------------------

lm.fit <- lm(medv~lstat+age,data = Boston)
summary(lm.fit)


#####
# Para evitar typear todas las variables de Boston en el modelo de regresion podemos hacer uso del siguiente short-hand

lm.fit <- lm(medv~., data=Boston)
summary(lm.fit)


#####
# Podemos acceder a los "componentes individuales" del summary que se muestra arriba
?summary.lm


summary(lm.fit)$r.sq
          # Es el R2

summary(lm.fit)$sigma
          # Es el RSE=sqrt(MSE)

# ------------------------------------------------------------------------------------------------------------------------------------------------------------
library(car)

#####
# Con la funcion vif() podemos calcular los Variance Inflation Factors

vif(lm.fit)
          # Recordar que VIF>10 supone que existe colinealidad, o equivalentemente
          # un valor de R2_j > 0.9


#####
# Para excluir una sola variable del modelo de regresion podemos hacerlo de la siguiente forma

lm.fit <- lm(medv~.-age,data=Boston)
summary(lm.fit)

#####
# Alternativamente, se puede hacer uso de la funcion update()

lm.fit1 <- update(lm.fit,~.-age)

# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# Interaction terms

#####
# Podemos incluir interaction terms con la sintaxis: x1:x2
# Y al usar x1*x2 agrega x1, x2 y el interaction term
          # Este es un principio que se platica en el libro


summary(lm(medv~lstat*age,data=Boston))


# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# Non-linear transformations of the predictors

#####
# Podemos incluir terminos no lineales como X^2 si usamos la funcion I(X^2)

lm.fit2 <- lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
          # El termino cuadratico tiene un p-value pequenio, quiere decir que existe una mejora en el modelo


#####
# Usaremos anova() para ver que tanto mejora el modelo

lm.fit <- lm(medv~lstat)


anova(lm.fit,lm.fit2)
          # Desarrolla una hypothesis test comparando los dos modelos

          # H0: The two models fit the data equally well
          # H1: Full model is superior

          # Se tiene que F=135 y p-value es muy pequenio, entonces 
          # el modelo con el termino cuadratico es mejor

#####
# Veamos las mejoras que se obtuvo en el modelo
par(mfrow=c(2,2))
plot(lm.fit2)

##### 
# Para crear polinomios de un orden mas grande podemos usar la funcion poly()
# dentro de lm()

lm.fit5 <- lm(medv~poly(lstat,5))
summary(lm.fit5)
          # Si se sigue investigando, un orden mayor a 5 no genera p-values significativos

# La funcion poly() ORTAGONALIZA los predictores



#####
# Tambien podemos usar una log transformation

summary(lm(medv~log(rm),data=Boston))


# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# Qualitative predictors

#####
# Para variables cualitativas, R genera variables dummy 
# (en nuestro data set un ejemplo seria Shelveloc: Bad, Medium, Good)


head(Carseats)

#####
# Creamos un primer modelo
lm.fit <- lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)
          # Agregamos dos interaction terms


#####
# Con la funcion contrast() se puede ver el coding que usa R para las variables dummy
attach(Carseats)
contrasts(ShelveLoc)

# Dummy varible: ShelveLocGood
    # 1 if the shelving location is Good
    # 0 otherwise

# Dummy varible: ShelveLocMedium
    # 1 if the shelving location is Medium
    # 0 otherwise

# A Bad ShelvingLoc corresponde a que ambas variables dummy tomen valor cero, esto se debe
# a que una variable con 3 niveles necesita unicamente 2 variables

# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# Writting functions

    LoadLibraries <- function(){
        library(ISLR2)
        library(MASS)
        print("The libraries have been loaded.")
    }


LoadLibraries
          # Nos dice que aparece en la funcion

# ------------------------------------------------------------------------------------------------------------------------------------------------------------
