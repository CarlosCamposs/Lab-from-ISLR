# setwd("C:/Users/Carlos Daniel/Desktop/Lab from ISLR/data")

Auto <- read.table("Auto.data")
View(Auto)
head(Auto)

# ------------------------------------------------------------------------------------------------------------------------------------------------------------

Auto <- read.table("Auto.data",header=T, na.strings="?",stringsAsFactors = T)
View(Auto)

        # 1. stringsAsFactors = T 
        # Indica que cualquier variable con caracteres debe ser interpretada
        # como variable cualitativa

        # 2. na.strings="?"
        # Estamos diciendo que donde exista "?" debe ser tratado como dato NA

# ------------------------------------------------------------------------------------------------------------------------------------------------------------

Auto <- read.csv("Auto.csv", na.strings = "?",stringsAsFactors = T)
View(Auto)

dim(Auto)
        # 397 observaciones
        # 9 columnas

# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# Usamos la funcion na.omit() para remover aquellos registros que tengan valores NA

Auto <- na.omit(Auto)
dim(Auto)
        # 392 obs, 9 columnas

# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# Usamos la funcion names() para revisar el nombre de las variables
names(Auto)

# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# Usamos attach() para hacer que R tenga disponibles las variables del dataset por su nombre
attach(Auto)

plot(cylinders,mpg)

cylinders<-as.factor(cylinders)
          # Como tiene pocos valores cylinders, lo convertimos a cualitativa (sus valores los tomamos como factores)

plot(cylinders,mpg)
          # Como x=cylinders ya es una variable cualitativa, la grafica se convertira automaticamente en un boxplot

plot(cylinders,mpg,col='red',varwidth=T,xlab='cylinders',ylab='MPG')

# ------------------------------------------------------------------------------------------------------------------------------------------------------------

# Histograma
hist(mpg,col=2,breaks=15)
          # col=2 tiene el mismo efecto que col='red'

# Scatterplot matrix de cada par de variables
pairs(Auto)

pairs(~mpg+displacement+horsepower+weight+acceleration)
          # Muestra los scatterplot pero solo de estas variables

# ------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(horsepower,mpg)
identify(horsepower,mpg,name)
          # Con la funcion identify podemos saber el "name" de alguna observacion que aparezca en el plot,
          # los argumentos que se pasan son el eje x, eje y y la variable de interes


summary(Auto)
summary(mpg)


q()       # Para cerrar sesion
          
