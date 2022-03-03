#3.8.This question involves the use of simple linear regression on the Auto data set.
Auto<-read.csv("C:\\Users\\sowmy\\OneDrive\\Documents\\ISL\\Auto.csv", header= TRUE, na.strings = "?")
library(MASS)
head(Auto)
#a.Use the lm() function to perform a simple linear regression with mpg as the response and horsepower as the predictor. Use the
#summary() function to print the results
auto.lin.fit = lm(mpg ~ horsepower, data = Auto)
summary(auto.lin.fit)
#i.Is there a relationship between the predictor and the response?
predict(auto.lin.fit, data.frame(horsepower = 98), interval = "prediction")
predict(auto.lin.fit,data.frame(horsepower= 98),interval="confidence")
#Plugging in a horsepower value of 98 gives a predicted mpg of 24.46708. The 95% confidence interval for this prediction is (23.97308, 24.96108)
#and the 95% prediction interval is (14.8094, 34.12467)

#2.Plot the response and the predictor. Use the abline() function to display the least squares regression line.
attach(Auto)
plot(horsepower,mpg)
abline(auto.lin.fit, lwd = 3, col = "red")

#3.Use the plot() function to produce diagnostic plots of the least squares regression fit. Comment on any problems you see with
#the fit.
par(mfrow = c(2, 2))
plot(auto.lin.fit)
#when looking at the Residuals vs. Leverage plot, there are some high leverage points (remember that after dropping the rows with null values, there 
#are 392 observations in the data set, giving an average leverage value of  2/392???0.0051 ) which also have high standardized residual values (greater than 2),
#which is also of concern for the simple linear regression model. 


#9.This question involves the use of multiple linear regression on the
#Auto data set.
Auto<-read.csv("C:\\Users\\sowmy\\OneDrive\\Documents\\ISL\\Auto.csv", header= TRUE, na.strings = "?")
library(MASS)
head(Auto)
#a.Produce a scatterplot matrix which includes all of the variables in the data set.
#Since origin and name are categorical columns, this are excluding from the scatterplot matrix.
pairs(~mpg + cylinders + displacement + horsepower + weight + acceleration + year, Auto)

#b.Compute the matrix of correlations between the variables using
#the function cor(). You will need to exclude the name variable, cor() which is qualitative
Auto$name<-NULL
cor(Auto,method = c("pearson"))

#c.Use the lm() function to perform a multiple linear regression
#with mpg as the response and all other variables except name as
#the predictors. Use the summary() function to print the results.
lm.fit<-lm(mpg~.,data=Auto)
summary(lm.fit)
#c.i.Is there a relationship between the predictors and the response?
#Since the F-statistic is 224.5, giving a p-value of essentially zero for the null hypothesis  H0:??j=0 for all j ,
#there is strong evidence to believe that there is a relationship between the predictors and the response.
#ii.The predictors that appear to have a statistically significant relationship to the response mpg are displacement with a p-value 
#of 0.001863, and weight, year, originEuropean, and originJapanese with p-values of essentially zero.

#d.Use the plot() function to produce diagnostic plots of the linear
#regression fit. Comment on any problems you see with the fit.
par(mfrow = c(2,2))
plot(lm.fit)
#Do the residual plots suggest any unusually large outliers? Does
#the leverage plot identify any observations with unusually high leverage?
# Ans:The first graph shows that there is a non-linear relationship between the responce and the predictors; 
#The second graph shows that the residuals are normally distributed and right skewed;
#The third graph shows that the constant variance of error assumption is not true for this model; 
#The fourth graphs shows that there are no leverage points. However, there on observation that stands out as a potential leverage point


#e.Use the * and : symbols to fit linear regression models with interaction effects. Do any interactions
#appear to be statistically significant?
lm.fit = lm(mpg ~.-name+displacement:weight, data = Auto)
summary(lm.fit)
mpg.fit.all.interactions = lm(mpg ~ (. - name)^2, data = Auto)
summary(mpg.fit.all.interactions)
mpg.fit.reduced.interactions = update(mpg.fit.all.interactions, ~ . - horsepower:origin)
summary(mpg.fit.reduced.interactions)
mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:origin)
summary(mpg.fit.reduced.interactions)
mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:displacement)
summary(mpg.fit.reduced.interactions)
summary(lm(mpg ~ . + cylinders:displacement - name, data = Auto))

#f.Try a few different transformations of the variables, such as log(X),
#???X, X2. 
par(mfrow = c(2, 2))
plot(Auto$displacement, Auto$mpg)
plot(Auto$horsepower, Auto$mpg)
plot(Auto$weight, Auto$mpg)
plot(Auto$acceleration, Auto$mpg)
lm.fit = lm(mpg ~.-name+I((displacement)^2)+log(displacement)+displacement:weight, data = Auto)
summary(lm.fit)
par(mfrow = c(2, 2))
plot(lm(mpg ~ acceleration, data = Auto))
par(mfrow = c(2, 2))
plot(lm(mpg ~ log(acceleration), data = Auto))
displacement.linear = lm(mpg ~ displacement, data = Auto)
summary(displacement.linear)
displacement.quadratic = lm(mpg ~ poly(displacement, 2), data = Auto)
summary(displacement.quadratic)
anova(displacement.linear, displacement.quadratic)


#10.This question should be answered using the Carseats data set.
Carseats<-read.csv("C:\\Users\\sowmy\\OneDrive\\Documents\\ISL\\Carseats.csv", header= TRUE, na.strings = "?")
head(Carseats)
#a.Fit a multiple regression model to predict Sales using Price,Urban, and US.
str(Carseats)
lm.fit = lm(Sales ~ Price+Urban+US, data= Carseats)
summary(lm.fit)

#b.Provide an interpretation of each coefficient in the model. Be
#careful-some of the variables in the model are qualitative?

##Ans:Price: suggests a relationship between price and sales given the low p-value of the t-statistic. 
#The coefficient states a negative relationship between Price and Sales: as Price increases, Sales decreases. 
#When price increases by $1000 and other predictors are held constant, sales decrease by 54.459 unit sales.
#UrbanYes: The linear regression suggests that there is not enough evidence for arelationship between the location of 
#the store and the number of sales based.
#In otherwords, when price increases by $1000, the number of carseats sold decrease by 54,459.
#A store's sale is not affected by whether or not it is in a Urban area.
#A store in the US sales 1200 more carseats (in average) than a store that is abroad.
#The coefficient of -0.054459 for Price means that, for a given location (i.e. fixed values of Urban and US), 
#increasing the price of a car seat by $1 results in a decrease of sales by approximately 54.46 units, on average, in the model.

#c.Write out the model in equation form, being careful to handle the qualitative variables properly.
# Sales = 13.04 + -0.05 Price + -0.02 UrbanYes + 1.20 USYes

#d.For which of the predictors can you reject the null hypothesis H0 : ??j = 0?
##Ans: The p-values for the intercept, Price, and USYes are all essentially zero, which provides strong evidence to reject 
#the null hypothesis  H0:??j=0  for those predictors.
###The predictor 'Urban'. Its p-value is not statistically significant with a value of 0.936.

#e. On the basis of your response to the previous question, fit a
#smaller model that only uses the predictors for which there is
#evidence of association with the outcome.
lm.fit2 = lm(Sales ~ Price+US, data= Carseats)
summary(lm.fit2)

#f.How well do the models in (a) and (e) fit the data?
#Based on the RSE and R^2 of the linear regressions, they both fit the data similarly, 
#with linear regression from (e) fitting the data slightly better.
#Based on their respective R-square values(in summary tables), these two models are mediocre 
#(only 24% change in response explained).

#g.Using the model from (e), obtain 95 % confidence intervals for the coefficient(s).
confint(Carseats)
carseats.fit.1 = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(carseats.fit.1)
carseats.fit.2 = lm(Sales ~ Price + US, data = Carseats)
summary(carseats.fit.2)
par(mfrow = c(2, 2))
plot(carseats.fit.1)
par(mfrow = c(2, 2))
plot(carseats.fit.2)
confint(carseats.fit.2)

#h.Is there evidence of outliers or high leverage observations in the model from (e)?
par(mfrow=c(2,2))
plot(lm.fit2)  
#Based on the Normal.q-q pot and the Residuals vs Leverage plot, there are no evidence of such points.
#The Residuals vs Leverage graph presents many high leverage points, but the most leverage points are not the outliners detect above.


