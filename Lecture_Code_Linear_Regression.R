# This is the package you need to access the data in the wooldridge textbook 
install.packages("wooldridge")
library(wooldridge)

# This is the simulations that I showed you in Lecture 14 
set.seed(220)
x1 = rnorm(1000)
x2 = rnorm(1000)
u = rnorm(1000)
y = 0.5 + 0.3*x1 + u
reg1 = lm(y~x1) # Reg y ~ x 
summary(reg1)
y2 = 0.5 + 0.3*x1 + 0.4*x2 + u 
reg2 = lm(y2 ~ x1) # Reg y ~ x1, but we omit x2 - x2 is NOT correlated with x1, so its ok
summary(reg2) 
x2 = 0.2 * x1 +rnorm(1000)
y2 = 0.5 + 0.3*x1 + 0.4*x2 + u 
reg3 = lm(y2 ~ x1) # Reg y ~ x1, but we omit x2 - x2 IS correlated with x1, so now we have endogeneity 
summary(reg3)
reg4 = lm(y2 ~ x1 + x2) # We can overcome omitted variable bias if we include the omitted variable into a multiple reg. 
summary(reg4)

# log log model
reg_loglog = lm(log(ceosal1$salary)~log(ceosal1$sales))
summary(reg_loglog)
anova(reg_loglog)

# Crime model with binary outcome as y (Linear Probability Model)
summary(crime1$narr86)
arr86 = ifelse(crime1$narr86==0,0,1)
reg_LPM = lm(arr86~crime1$pcnv+crime1$avgsen+crime1$tottime+crime1$ptime86+crime1$qemp86) 
summary(reg_LPM)

# Wage Example 
reg_wage = lm(lwage ~ educ + exper + tenure,data=wage1) 
summary(reg_wage)

# Math10 Example 
reg_math10 = lm(math10~totcomp+staff+enroll,data=meap93)
summary(reg_math10)

# Hedonic Regression Example
reg_price = lm(lprice~lnox+log(dist)+rooms+stratio,hprice2) 
  
# This is if you want to reproduce the fancy 3D Plot 
library("car")
library("rgl")

data(iris)
head(iris)
z <- iris$Sepal.Length
x<- iris$Sepal.Width
y <- iris$Petal.Length

scatter3d(formula, data)
scatter3d(x, y, z)
# 3D plot with the regression plane
scatter3d(x = sep.l, y = pet.l, z = sep.w)


