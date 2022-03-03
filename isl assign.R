#2.8.This exercise relates to the College data set, which can be found in
#the file College.csv on the book website. It contains a number of
#variables for 777 different universities and colleges in the US. The variables are

#a.Use the read.csv() function to read the data into R. Call the
#loaded data college. Make sure that you have the directory set
#to the correct location for the data.
College<-read.csv("C:\\Users\\sowmy\\OneDrive\\Documents\\ISL\\College.csv", header= TRUE)
View(College)
#b.Look at the data using the View() function. You should notice
#that the first column is just the name of each university. We don't
#really want R to treat this as data. However, it may be handy to have these names for later
rownames(College) = College[, 1]
View(College)
College <- College[, -1]
View (College)
#c.i.Use the summary() function to produce a numerical summary of the variables in the data set.
summary(College)
#ii.Use the pairs() function to produce a scatterplot matrix of the first ten columns or variables of the data. Recall that
#you can reference the first ten columns of a matrix A using A[,1:10].
College[,1] = as.numeric(factor(College[,1]))
pairs(College[, 1:10])
#Use the plot() function to produce side-by-side boxplots of
#Outstate versus Private.
plot(Outstate ~ Private, data = College, 
     xlab = "Private University", 
     ylab = "Tuition in $")
Elite <- rep ("No", nrow (College))
Elite[College$Top10perc > 50] <- " Yes "
Elite <- as.factor (Elite)
College <- data.frame (College , Elite)
College$Elite <- as.factor(ifelse(College$Top10perc > 50, "Yes", "No"))
summary(Elite)
plot(Outstate ~ Elite, data = College, 
     xlab = "Elite University", 
     ylab = "Tuition in $")
par(mfrow=c(2,2))
hist(College$Apps, xlab = "Applications Received", main = "")
hist(College$perc.alumni, col=2, xlab = "% of alumni who donate", main = "")
hist(College$S.F.Ratio, col=3, breaks=10, xlab = "Student/faculty ratio", main = "")
hist(College$Expend, breaks=100, xlab = "Instructional expenditure per student", main = "")
#EXPLORING DATA
# university with the most students in the top 10% of class
#lowest acceptance rate
## High tuition correlates to high graduation rate
row.names(College)[which.max(College$Top10perc)]
acceptance_rate <- College$Accept / College$Apps
row.names(College)[which.min(acceptance_rate)]
plot(Grad.Rate ~ Outstate, data = College) 
AcceptPerc = College$Accept / College$Apps * 100
College = data.frame(College, AcceptPerc)
par(mfrow = c(1, 2))
plot(College$Private, College$AcceptPerc, xlab = "Private", ylab = "Acceptance Rate")
plot(College$Elite, College$AcceptPerc, xlab = "Elite", ylab = "Acceptance Rate")






#9. This exercise involves the Auto data set studied in the lab. Make sure
#that the missing values have been removed from the data
Auto<-read.csv("C:\\Users\\sowmy\\OneDrive\\Documents\\ISL\\Auto.csv", header= TRUE, na.strings = "?")
Auto = na.omit(Auto)
dim(Auto)
#a.Which of the predictors are quantitative, and which are qualitative?
head(Auto)
#b.What is the range of each quantitative predictor? You can answer this using the range() function
?range
range(Auto$mpg)
range(Auto$cylinders)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)
#c.What is the mean and standard deviation of each quantitative predictor?
colMeans(Auto[, 1:7])
apply(Auto[, 1:7], MARGIN = 2, FUN = "sd")
#d.Now remove the 10th through 85th observations. What is the range, mean, and standard deviation of each predictor in the
#subset of the data that remains?
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "range")
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "mean")
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "sd")
par(mfrow = c(2, 2))
plot(Auto$displacement, Auto$mpg, xlab = "Engine displacement (cubic inches)", ylab = "Miles per gallon")
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")
plot(Auto$weight, Auto$mpg, xlab = "Car weight (pounds)", ylab = "Miles per gallon")
plot(Auto$year, Auto$mpg, xlab = "Model Year", ylab = "Miles per gallon")
#e.Using the full data set, investigate the predictors graphically,
#using scatterplots or other tools of your choice. Create some plots
#highlighting the relationships among the predictors. Comment on your findings
par(mfrow = c(2, 2))
plot(Auto$weight, Auto$acceleration, xlab = "Car weight (pounds)", ylab = "0 to 60mph time (seconds)")
plot(Auto$cylinders, Auto$acceleration, xlab = "Number of engine cylinders", ylab = "0 to 60mph time (seconds)")
plot(Auto$displacement, Auto$acceleration, xlab = "Engine displacement (cubic inches)", ylab = "0 to 60mph time (seconds)")
plot(Auto$horsepower, Auto$acceleration, xlab = "Horsepower", ylab = "0 to 60mph time (seconds)")
#f.Suppose that we wish to predict gas mileage (mpg) on the basis
#of the other variables. Do your plots suggest that any of the
#other variables might be useful in predicting mpg? Justify your answer.
Auto$origin[Auto$origin == 1] = "American"
Auto$origin[Auto$origin == 2] = "European"
Auto$origin[Auto$origin == 3] = "Japanese"
Auto$origin = as.factor(Auto$origin)  
plot(Auto$origin, Auto$mpg, xlab = "Country of origin", ylab = "Miles per gallon")






#10. This exercise involves the Boston housing data set.
#a.To begin, load in the Boston data set. The Boston data set is part of the ISLR2 library.
Boston<-read.csv("C:\\Users\\sowmy\\OneDrive\\Documents\\ISL\\Boston.csv", header= TRUE, na.strings = "?")
head(Boston)
#How many rows are in this data set? How many columns? What do the rows and columns represent?
#Ans:The corrected Boston data set has 506 rows and 20 columns. Each row represents a particular tract of land within the city of Boston.
#Columns are Town, townno,tract,lon,lat,medv,cmedv,crim,zn,indus,chas,nox,rm,age,dis,rad,tax,ptratio,b,lstat.

#b.Make some pairwise scatterplots of the predictors (columns) in
#this data set. Describe your findings.
data(Boston, package = "MASS")
help(Boston, package = "MASS")
dim(Boston)
summary(Boston)
pairs(Boston)
#c.Are any of the predictors associated with per capita crime rate? If so, explain the relationship.
# Older homes, more crime
plot(crim ~ age, data = Boston, log = "xy")
# Closer to work-area, more crime
plot(crim ~ dis, data = Boston) 
# Closer to work-area, more crime
plot(crim ~ dis, data = Boston, log = "xy")
# Higher index of accessibility to radial highways, more crime
plot(crim ~ rad, data = Boston, log = "xy")
# as box plots, since rad appears to be categorical
plot(crim ~ as.factor(rad),
     log = "y",
     data = Boston,
     xlab = "Accessibility to radial highways",
     ylab = "log of crime")
# Higher tax rate, more crime
plot(crim ~ tax, log = "xy", data = Boston)
# Higher pupil:teacher ratio, more crime
plot(crim ~ ptratio, log = "xy", data = Boston)
cor(Boston)
#d.Do any of the census tracts of Boston appear to have particularly
#high crime rates? Tax rates? Pupil-teacher ratios? Comment on the range of each predictor.
# to have a crime rate > 20, reaching to above 80
par(mfrow=c(1,3))
hist(Boston$crim[Boston$crim > 1], breaks=25)
# there is a large divide between suburbs with low tax rates and a peak at 660-680
hist(Boston$tax, breaks=25)
#ratio
hist(Boston$ptratio, breaks=25)
#e.How many of the census tracts in this data set bound the Charles river?
sum(Boston$chas == 1)
#f.What is the median pupil-teacher ratio among the towns in this data set?
median(Boston$ptratio)
#g.Which census tract of Boston has lowest median value of owneroccupied homes? What are the values of the other predictors
#for that census tract, and how do those values compare to the
#overall ranges for those predictors? Comment on your findings
t(subset(Boston, medv == min(medv)))
summary(Boston)
#h.In this data set, how many of the census tracts average more than seven rooms per dwelling? More than eight rooms per
#dwelling? Comment on the census tracts that average more than eight rooms per dwelling
sum(Boston$rm > 7)
sum(Boston$rm > 8)
#Suburbs that average more than eight rooms per dwelling
summary(subset(Boston, rm > 8))






