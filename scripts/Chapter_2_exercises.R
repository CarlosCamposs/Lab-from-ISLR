setwd("C:/Users/Carlos Daniel/Desktop/GitHub/Lab-from-ISLR/data")
# ------------------------------------------------------------------------------------------------------------------------------------------------------------

############### 
# Ex. 8

# (a)
college <- read.csv("../data/College.csv",stringsAsFactors = T)


# (b)
View(college)


rownames(college)<-college[,1]
View(college)
  

college <- college[,-1]
View(college)


# (c)
summary(college)
pairs(college[,1:10])


plot(college$Private,college$Outstate)


Elite <- rep("No",nrow(college))
Elite[college$Top10perc>50] <- "Yes" # En aquellas posicines donde se cumpla la condicion, se les pone un "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college,Elite)


summary(Elite)
plot(college$Elite,college$Outstate)
          # No: 699, Yes: 78

par(mfrow=c(2,2))
hist(college$Books,main='Books',col='cadetblue2')
hist(college$Accept,main='Accept',col='coral2')
hist(college$Room.Board,main = 'Room.Board',col='darkorchid3')
hist(college$Expend,main='Expend',col='lightskyblue3')


par(mfrow=c(1,1))
plot(college$Outstate, college$Grad.Rate)
          # High tuition correlates to high graduation rate.

plot(college$Accept / college$Apps, college$S.F.Ratio)
          # Colleges with low acceptance rate tend to have low S:F ratio.

plot(college$Top10perc, college$Grad.Rate)
          # Colleges with the most students from top 10% perc don't necessarily have
          # the highest graduation rate. Also, rate > 100 is erroneous!


############### 
# Ex. 9
Auto <- read.csv("../data/Auto.csv", na.strings = "?",stringsAsFactors = T)
Auto <- na.omit(Auto)

# (a)
summary(Auto)
          # quantitative: mpg, displacement, horsepower, weight, acceleration
          # qualitative: origin, name. Se podria considerar  cylinders, year como qualitative por los "pocos" valores que toman

# (b)
sapply(Auto[, 1:7], range)

# (c)
sapply(Auto[,1:7],mean)
sapply(Auto[,1:7],sd)

# (d)
Auto2 <- Auto[-c(10:85),]
sapply(Auto2[,1:7],range)
sapply(Auto2[,1:7],mean)
sapply(Auto2[,1:7],sd)

# (e)
pairs(Auto)

plot(Auto$mpg, Auto$weight)
          # Heavier weight correlates with lower mpg.

plot(Auto$mpg, Auto$cylinders)
          # More cylinders, less mpg.

plot(Auto$mpg, Auto$year)
          # Cars become more efficient over time.

# (f)
          # Displacement, horsepower, weight


############### 
# Ex. 10

# (a)
library(ISLR2)
?Boston
View(Boston)
          # 506 rows, 13 columns

# (b)
plot(Boston$age, Boston$crim)
          # Entre mas vieja la casa, mas crimen

# (c)
pairs(Boston)


