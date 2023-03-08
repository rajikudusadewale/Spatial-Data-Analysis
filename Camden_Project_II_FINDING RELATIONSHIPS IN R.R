##########  FINDING RELATIONSHIPS IN R #############
Census.Data <- read.csv('practical_data.csv')

#means of statistically identifying and measuring bivariate and 
# multivariate relationships in R
# Runs a Pearson's correlation
cor.test(Census.Data$Unemployed, Census.Data$Qualification)

# Runs a Spearman's correlation
cor.test(Census.Data$Unemployed, Census.Data$Qualification, method="spearman")

#### to produce a correlation pair-wise matrix in R
# need to remove ID col

# creates a data1 object which does not include the 1st column from the original data 
data1 <- Census.Data[,2:5]

# creates correlation matrix
cor(data1)

#We can use the round() function to round our results to 2 decimal places.
round(cor(data1),2)

#### TO PLOT HEATMAP USING QPLOT() FUNC ####

library(reshape2)
corr <- cor(data1)

# creates a qplot from our corr matrix
qplot(x=Var1, y=Var2, data=melt(corr), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1))
library(corrplot)

# creates a lower triangular corrplot from our corr matrix. enable plot very easy
corrplot(corr, type="lower", tl.col="black", tl.srt=45)


####### REGRESSION ANALYSIS ##########
# It plots a single straight line of predicted values as the model for a relationship

# runs a regressions model (y ~ x, data frame)
model_1 <- lm(Qualification~ Unemployed, Census.Data)
plot(Census.Data$Unemployed, Census.Data$Qualification, xlab="% Unemployed", 
     ylab="% With a Qualification") + abline (model_1)
summary(model_1)

# we can now predict Y since we know the intercept, and slope, and given X
# using the notation Y = b0 + B1x to calc manually. 
# it can be predicted using predict () func
predict(model_1, data.frame(Unemployed=10), interval = 'confidence')
#  fit      lwr      upr
# 29.10228 26.87376 31.33079

####    R SQUARED   ##### The residuals = expt/pred value - observed value
# R2 ranges from 0 to 1, the higher it is,the better the model prediction

#The below example will produce a 95% confidence interval for the model
confint(model_1, level= 0.95)

# a multiple regression model  # using more than 1 independent var can enahance a model accuracy
model_2 <- lm(Qualification ~ Unemployed + White_British, Census.Data)

summary(model_2)








