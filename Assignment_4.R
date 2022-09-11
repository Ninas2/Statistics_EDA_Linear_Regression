setwd("C:\\Users\\ninas\\R\\RPackages")

.libPaths('C:\\Users\\ninas\\R\\RPackages')
library('psych')
library('corrplot')
library(nortest)
library('randtests')
library('lmtest')
library(car)
require('glmnet')

#Question 1 - Read file and interpret descriptives
House_sales <- read.csv("C:/Users/ninas/OneDrive/Desktop/MSc Business Analytics/1st Quarter/Statistics for BA 1/Statistics_Lab_Assignment4/usdata", sep = "")
str(House_sales)
summary(House_sales)


#Question 2 - Update data types for variables
sum(is.na(House_sales))
House_sales$PRICE <- as.numeric(House_sales$PRICE)
House_sales$SQFT <- as.numeric(House_sales$SQFT)
House_sales$AGE <- as.numeric(House_sales$AGE)
House_sales$FEATS <- as.numeric(House_sales$FEATS)
House_sales$NE <- factor(House_sales$NE, levels = c(0,1), labels = c('No','Yes'))
House_sales$COR <- factor(House_sales$COR, levels = c(0,1), labels = c('No','Yes'))
str(House_sales)


#Question 3 - Perform descriptive analysis for each variable
index <- sapply(House_sales, class) == 'numeric'
House_num <- House_sales[,index]
#functions to show the variables' descriptives
summary(House_sales)
describe(House_num)
n <- nrow(House_sales)

#visualize numeric variables with histograms
numnames <- c('Price', 'Square Feet', 'Age', 'Features')
par(mfrow = c(2,2))
for (i in 1:3){
  hist(House_num[,i], main = numnames[i], xlab = numnames[i]
       ,col = 'lightblue')
}
plot(table(House_num[,4])/n, type='h', xlim=range(House_num[,4])+c(-1,1)
     , main='Features', ylab='Relative frequency', xlab = 'Features')

#visualize factor variables
factnames <- c('Northeast', 'Corner')
par(mfrow = c(1,1))
House_factors <- House_sales[,c(5,6)]
barplot(sapply(House_factors,table)/n, horiz=T, las=1, col=2:3, ylim=c(0,8), cex.names=1.3)
legend('top', fil=2:3, legend=c('No','Yes'), ncol=2, bty='n',cex=1.5)


#Question 4 - Conduct pairwise comparisons and interpret the results
cor_table <- cor(House_num)
cor_table <- round(cor_table,2)
index <- c(1, 2, 4, 3)
cor_table <- cor_table[index, index]
#corrplots for numeric variables
par(mfrow = c(1,2))
corrplot(cor_table, method = 'number')
corrplot(cor_table, method = 'ellipse')

#boxplots for factors vs prices
for (j in 1:2){
  boxplot(House_sales$PRICE ~ House_sales[,j+4], col = 'lightblue', ylab = 'Price', xlab = factnames[j])
  abline(lm(House_sales$PRICE ~ House_sales[,j+4]), col = 2, lty = 2)
}


#Question 5 - Construct a full model
full_model <- lm(PRICE~., data = House_sales)
constant_model <- lm(PRICE~1, data = House_sales)
summary(full_model)
summary(constant_model)
anova(full_model, constant_model)
#adjusted R-squared =  86,4% - good fit for the model


#Question 6 - Find the best model using stepwise methods
#AIC Methods
model1 <- step(full_model, direction = 'both')
summary(model1)
model2 <- step(full_model, direction = 'backward')
summary(model2)
#model 3 ends up in a different model - the constant model
model3 <- step(constant_model, direction = 'forward')
summary(model3)
model4 <- step(constant_model, scope=list(lower=constant_model,upper=full_model), direction = 'forward')
summary(model4)
model5 <- step(constant_model, scope=list(lower=constant_model,upper=full_model), direction = 'both')
summary(model5)


#Question 7 - Interpret the best model
#transform square feet to square meters
House_num$SQM <- House_num$SQFT/10.764
House_num <- House_num[,-2]
#center the variables
House_num2 <- as.data.frame(scale(House_num, center = TRUE, scale = F))
House_num2$PRICE<-House_num$PRICE

#recreate the best model with centered covariates
model11 <- lm(PRICE~.-AGE, data=House_num2)
summary(model11)
model11$coefficients

#trying to remove the intercept
model12 <- lm(PRICE~.-1-AGE, data=House_num2)
summary(model12)
true.r2 <- 1-sum(model11$res^2)/((n-1)*var(House_num2$PRICE)); true.r2


#Question 8 - Check the assumptions of the model
#multi-collinearity of the x variables
vif(model11) #no multi collinearity

#normality of the residuals
par(mfrow = c(1,1))
plot(model11, which = 2) 
shapiro.test(model11$residuals)
ks.test(model11$residuals, 'pnorm', mean(model11$residuals), sd(model11$residuals))

#Costant variance 
Stud.residuals <- rstudent(model11)
yhat <- fitted(model11)
par(mfrow=c(1,3))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)
plot(yhat, Stud.residuals^(1/2))
abline(h=sqrt(2), col=2, lty=2)

ncvTest(model11)

yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(model11)~yhat.quantiles)
par(mfrow = c(1,1))
boxplot(rstudent(model11)~yhat.quantiles, col = 'lightblue', ylab = 'Studentized residuals'
        ,xlab = 'Fitted Values Quantiles')

#Non-linearity
residualPlot(model11, type='rstudent')
residualPlots(model11, plot=F, type = "rstudent")

#Independence 
plot(rstudent(model11), type='l', ylab = 'Studentized Residuals')
runs.test(model11$res)
dwtest(model11)
durbinWatsonTest(model11)


#Question 9 - Conduct Lasso and compare the results with the stepwise method
House_sales$SQFT <- House_sales$SQFT/10.764
names(House_sales)[2] <- 'SQM'
full_model1 <- lm(PRICE~., data = House_sales)
summary(full_model1)

full_matrix <- model.matrix(full_model1)[,-1]
lasso <- glmnet(full_matrix, House_sales$PRICE)
par(mfrow = c(1,1))
plot(lasso, xvar = "lambda", label = T)
#Use cross validation to find a reasonable value for lambda 
lasso1 <- cv.glmnet(full_matrix, House_num2$PRICE, alpha = 1)
lasso1$lambda
lasso1$lambda.min
lasso1$lambda.1se
plot(lasso1)
coef(lasso1, s = "lambda.min")
coef(lasso1, s = "lambda.1se")

