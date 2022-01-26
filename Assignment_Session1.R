#################################################################
#Chapter 2 Question 2
#################################################################
#a.
#Regression Problem
#Interested in inference
#n=500, p = 3

#a.
#Classification Problem
#Interested in Prediction
#n=20, p = 13

#a.
#Classification Problem
#Interested in Prediction
#n=52, p = 3

#################################################################
#Chapter 2 Question 10
#################################################################

library(MASS)

dim(Boston)
#a.
?Boston
View(Boston)
#506 observations
#14 columns
#b.
pairs(Boston)

#c
plot(Boston$age, Boston$crim)
#Crimes are more where the owner occupied units are old

plot(Boston$dis, Boston$crim)
#Crimes are more where weighted mean of distances are less

#d
hist(Boston$crim[Boston$crim>1], breaks=10)
#Taking crime rate greater that 1 because lesser than one doesn't make any sense
#There around 8 suburbs which have crime rate greater than 30

hist(Boston$tax, breaks = 10)
#There are around 6-8 suburbs at high tax rates i.e 700-711 tax values but there
#are around 130 suburbs at tax rates 650 - 700

hist(Boston$ptratio, breaks=25)
#looking at histogram we can say 2-3 suburbs have higher pupil teacher ratio

range(Boston$crim)
length(unique(Boston$crim))
#0 to 88.98, very wide range
range(Boston$tax)
length(unique(Boston$tax))
# 187 to 711, not as wide of a range as crime rate
range(Boston$ptratio)
length(unique(Boston$ptratio))
# 12.6 to 22.0, not as wide of a range as crime rate

#e
nrow(Boston[Boston$chas == 1, ])
#There are 35 suburbs bounds to Charles River

#f
median(Boston$ptratio)
#19.05

#g
t(subset(Boston, medv == min(Boston$medv)))
summary(Boston)
#Suburb 399 and 406 are has lowest median value if owner occupied homes
#Most of the variable are either above 3rd Quartile or at minimum value
#Felt not the best place to live as the houses are very old, crime rates is above
#quartile, tax rate is high

#h
nrow(Boston[Boston$rm > 7,])
#64 suburbs

nrow(Boston[Boston$rm > 8,])
#13 Suburbs
df <- Boston[Boston$rm > 8,]
summary(df)
summary(Boston)
#less crime rates compared to whole list, lstat is low and medv is higher, by
#seeing this we can say its a good area to leave
################################################################################
#Chapter 3 Question 3
################################################################################
#a -> option iii is the correct answer
#b
#salary = 50+20*gpa+0.07*iq+35*gender+0.01*gpa*iq−10*gpa*gender
#=50+20*4+0.07*110+35*1+0.01*4*110−10*4*1
#=137.1
#False
################################################################################
#Chapter 3 Question 10
################################################################################
#a.
install.packages("ISLR")
library('ISLR')
str(Carseats)

sales_lm <- lm(Sales ~ Price + Urban + US, data = Carseats)

#b.
summary(sales_lm)
#Price = -0.054, so keeping Urban and US fixed,1 unit of price increases then 
#sales will -0.054 units
#Urban = -0.022, since P value of T-test is high, cannot find the relationship b/w
#sales and Urban
#US = 1.2,by keeping Price and Urban fixed, store being in US is a change in sales
#1.2 units

#c
#Sales=13.043469 - 0.054459*Price - 0.021916*Urban + 1.200573*US
#Where Urban = 1 if the store is in Urban or Urban = 0, 
#US = 1 if the store is in US else US = 0

#d
#We can reject the null hypothesis for Price and US

#e
sales_lm2 <- lm(Sales ~ Price + US, data = Carseats)

#f
summary(sales_lm2)
#R-squared value for both models is 0.2393, both models have same variance

#g
confint(sales_lm2, level = 0.95)

#h
plot(sales_lm2)
#we can see few outliers/residuals in between quantile -3 to -2 and 2 to 3
###############################################################################
#Chapter 3 Question 15
###############################################################################
#a
for(i in 2:ncol(Boston)){
  print(summary(lm(crim ~ Boston[ ,i], data=Boston)))
}
#Looking the summary of all the individual predictors, All seems to be significant

#b
summary(lm(crim ~ ., data = Boston ))
#zn, dis, rad, black, medv for these predictors we can remove the null hypothesis
#less information to revome null hypothesis for other predictors


