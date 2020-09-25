#library(MASS)
#library(ISLR)

# Ler e transformar os dados 
dados <- read.table("prostate.txt")
dim(dados)
summary(dados[,-9])
View(dados)
dados[,1:8] <- scale(dados[,1:8],TRUE,TRUE)
View(dados)

# Simple Linear Regression
attach(dados)

plot(lcavol, lpsa)

lm.fit = lm(lpsa ~ lcavol) 

lm.fit

summary(lm.fit)

names(lm.fit)

coef(lm.fit)

confint(lm.fit)

predict(lm.fit, data.frame(lcavol=c(5,10,15)),
        interval="confidence") # intervalo de confiança em torno das médias das predições

predict(lm.fit, data.frame(lcavol=c(5,10,15)), 
        interval="prediction") # intervalo de confiança em torno de um único valor

plot(lcavol, lpsa, pch=19, cex=0.4)
abline(lm.fit)
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col="red")

plot(lcavol, lpsa, col="red")
plot(lcavol, lpsa, pch=20)
plot(lcavol, lpsa, pch="+")

plot(1:20,1:20, pch=1:20)
par(mar = rep(4, 4))
par(mfrow=c(2,2))
plot(lm.fit)

# Multiple Linear Regression

lm.fit=lm(lpsa ~ lcavol + age)
summary(lm.fit)

lm.fit=lm(lpsa~., data=dados[,1:8])
summary(lm.fit)

# What if we would like to perform a regression using all of the variables but one? 
# For example, in the above regression output, age has a high p-value
lm.fit1=lm(lpsa~.-age, data=dados[,1:8])
summary(lm.fit1)

lm.fit1=update(lm.fit, ~.-age)

# Interaction Terms -----------------------------------
# It is easy to include interaction terms in a linear
# model using the lm() func- tion. 

summary(lm(lpsa~lcavol*age,data=dados[,1:8]))

  # Non-linear Transformations of the Predictors --------

lm.fit2=lm(lpsa~lcavol+I(lcavol^2))
summary(lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit=lm(lpsa~lcavol)

# The anova() function performs a hypothesis test comparing the two models. 
# The null hypothesis is that the two models fit the data equally well, and the 
# alternative hypothesis is that the full model is superior. 
anova(lm.fit,lm.fit2)

# Here the F-statistic is 135 and the associated p-value is virtually zero. 
# This provides very clear evidence that the model containing the predictors 
# lcavol and lcavol2 is far superior to the model that only contains the predictor lcavol.

par(mfrow=c(2,2))
plot(lm.fit2)

# using the poly() function to create the polynomial within lm(). 
# For example, the following command produces a fifth-order polynomial fit:
lm.fit5=lm(lpsa~poly(lcavol,5))

summary(lm.fit5)
names(dados)

# Of course, we are in no way restricted to using polynomial transformations of
# the predictors. Here we try a log transformation.
  
library(glmnet)

#### Ridge Regression ################

x <- model.matrix(lpsa~.,dados[,1:8])[,-1]  #matriz de preditores
y <- dados$lpsa                            # variável resposta

fit.ridge <- glmnet(x, y, alpha = 0, lambda = 2)
coef(fit.ridge)                # valores dos coeficientes estimados
y_predicted <- predict(fit.ridge,newx = x)
mean(y_predicted-y)^2 # MSE 

fit.ridge2 <- glmnet(x, y, alpha = 0, lambda = 5)
coef(fit.ridge2)                
y_predicted2 <- predict(fit.ridge2,newx = x)
mean(y_predicted2-y)^2

fit.ridge3 <- glmnet(x, y, alpha = 0, lambda = 10)
coef(fit.ridge3)                
y_predicted3 <- predict(fit.ridge3,newx = x)
mean(y_predicted3-y)^2


###### LASSO  #########################

fit.lasso <- glmnet(x,y, alpha = 1, lambda = 0.1)
y_hat <-  predict(fit.lasso,newx = x)
coef(fit.lasso)
mean(y_hat-y)^2

fit.lasso2 <- glmnet(x,y,alpha = 1, lambda = 0.5)
y_hat2 <-  predict(fit.lasso2,newx = x)
coef(fit.lasso2)
mean(y_hat2-y)^2

fit.lasso2 <- glmnet(x,y,alpha = 1, lambda = 1)
y_hat2 <-  predict(fit.lasso2,newx = x)
coef(fit.lasso2)
mean(y_hat2-y)^2 # MSE


#### Ridge com CV

grid <- 10^seq(10,-3,length=100)


cv_ridge <- cv.glmnet(x, y, alpha = 0, lambda = grid) #validação cruzada 
best_lam <- cv_ridge$lambda.min # melhor lambda de acordo com o menor erro de validação

ridge.mod <- glmnet(x, y,alpha=0,lambda=best_lam)
coef(ridge.mod)  #coeficientes com o lambda ótimo

y_preds <-predict(ridge.mod,s=best_lam,newx = x)
mean(y_preds-y)^2

######### Lasso com CV

cv_lasso <- cv.glmnet(x, y, alpha = 1, lambda = grid) #validação cruzada 
best_lam <- cv_lasso$lambda.min # melhor lambda de acordo com o menor erro de validação

lasso.mod <- glmnet(x, y,alpha=1,lambda=best_lam)
coef(lasso.mod)  #coeficientes com o lambda ótimo

y_preds <-predict(lasso.mod,s=best_lam,newx = x)
mean(y_preds-y)^2


######### Seleção de variáveis

names(dados)

library(leaps)

# Seleção do melhor subconjunto
#Para cada k ∈{0,1,2,...,p} escolhemos o subconjunto de tamanhokque dá 
#a menor somados quadrados dos resíduos.

regset <- regsubsets(lpsa~., data=dados[,1:8])
summa <- summary(regset) #Por padrão, regsubsets () relata apenas os resultados até o melhor modelo de oito variáveis. 
names(summa)
summa$rsq # R^2

par(mfrow = c (2 ,2) )
plot(summa$rss,xlab ="Number of Variables", ylab ="RSS",type ="l")
plot(summa$adjr2 , xlab =" Number of Variables " ,ylab =" Adjusted RSq",type ="l")
which.max(summa$adjr2) # maior R^2 ajustado 
points(7,summa$adjr2[7] , col =" red " , cex =2 , pch =20)

plot(summa$cp,xlab =" Number of Variables ",ylab ="Cp",type = "l")
which.min(summa$cp )
points(5,summa$cp[5] , col =" red " , cex =2 , pch =20)

plot(summa$bic,xlab =" Number of Variables ",ylab ="BIC",type = "l")
which.min(summa$bic)
points(3,summa$bic[3] , col =" red " , cex =2 , pch =20)

plot(regset,scale = "r2")
#plot(regset,scale ="adjr2") 
#plot(regset,scale ="Cp")
#plot(regset,scale ="bic") 

coef(regset,3)


## Seleção progressiva (forward)

fit.fwd <- regsubsets(lpsa~., nvmax=8, method ="forward",data=dados[,1:8])
summary(fit.fwd)

### Seleção regressiva (backward)

fit.back <- regsubsets(lpsa~., nvmax=8, method ="backward",data=dados[,1:8])
summary(fit.back)


library(MASS)

step(lm(lpsa~.,data=dados[,1:8]),direction="both")
stepAIC(lm(lpsa~.,data=dados[,1:8]),direction="both")
  
