# Binary Tutorial from Data Camp ------------------------------------------------------
# https://www.datacamp.com/community/tutorials/logistic-regression-R

# Interpretation
# Categorical Covariate:
# Statistical significance means that being in [category], as opposed to
# being in the reference category, is associated with much higher/lower
# reported likelihood of being in the outcome category as compared to the
# outcome reference category

# Continuous Covariate:
# Statistical significance means that a higher/lower value of the IV is
# associated with higher/lower reported likelihood of being in the outcome
# category as compared to the outcome reference category

# Use the Smarket dataset
library(dplyr)
if (!require(ISLR)) install.packages("ISLR"); library(ISLR)
names(Smarket)
head(Smarket)
summary(Smarket)

#* Visualizations ---------------------------------------------------------------
# Histogram
par(mfrow=c(1,8))
for(i in 1:8) {
  hist(Smarket[,i], main = names(Smarket[i]))
}
# Box and Whisker
par(mfrow=c(1:8))
for(i in 1:8) {
  boxplot(Smarket[,i], main = names(Smarket[i]))
}

#* Look for missing data patterns -----------------------------------------------
if (!require(Amelia)) install.packages("Amelia"); library(Amelia)
if (!require(mlbench)) install.packages("mlbench"); library(mlbench)
missmap(Smarket, col = c("blue","red"), legend = F)

#* Correlation Matrix -----------------------------------------------------------
if (!require(corrplot)) install.packages("corrplot"); library(corrplot)
correlations <- cor(Smarket[,1:8])
dev.off()
corrplot(correlations, method = "circle")

# Look at the distributions using our dependent variable as colors
# A clear split would suggest a difference on the dependent variable
pairs(Smarket, col = Smarket$Direction)

# Density Distribution
if (!require(caret)) install.packages("caret"); library(caret)
x <- Smarket[,1:8]
y <- Smarket[,9]
scales <- list(x = list(relation = "free"), y=list(relation="free"))
featurePlot(x=x,y=y,plot = "density",scales=scales)


# The bottom line is that our dependent variable is not predicted well
# by any one variable. So, a logistic regression model is required to
# identify a strong model of prediction. 

#* Logistic Regression ----------------------------------------------------------
# family = binomial is what tells R to perform a logistic
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket,family = binomial)
summary(glm.fit)               

# Predicted Classifications
glm.probs <- predict(glm.fit, type = "response")
glm.probs[1:5] # Look at the first 5

# Use the probabilities to make a prediction
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")

# Confusion matrix of prediction with actual data
table(glm.pred,Smarket$Direction)
mean(glm.pred == Smarket$Direction)
# Our prediction is pretty bad, we are right about 50% of the time. 

#* Training and Test Datasets ---------------------------------------------------
Smarket$is_training = Smarket$Year < 2005
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data = Smarket,
               family = binomial,
               subset = is_training # Uses only the training data
               )
glm.probs <- predict(glm.fit,
                     newdata = Smarket[!Smarket$is_training,], # Use the validation data
                     type = "response" # Predict the probabilities
                     )
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")
table(glm.pred,Smarket$Direction[!Smarket$is_training])
mean(glm.pred == Smarket$Direction[!Smarket$is_training])
# The prediction got worse, due to overfitting.


# Reduce Overfitting. 
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3, 
               data = Smarket,
               family = binomial,
               subset = is_training # Uses only the training data
)
glm.probs <- predict(glm.fit,
                     newdata = Smarket[!Smarket$is_training,], # Use the validation data
                     type = "response" # Predict the probabilities
)
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")
table(glm.pred,Smarket$Direction[!Smarket$is_training])
mean(glm.pred == Smarket$Direction[!Smarket$is_training])

# View the final model
summary(glm.fit)



# Ordinal Logistics Tutorial from Princton ===============================================
# https://www.princeton.edu/~otorres/LogitR101.pdf
if (!require(foreign)) install.packages("foreign"); library(foreign) # read.dta, brings in the labels as factors
if (!require(haven)) install.packages("haven"); library(haven) # read_dta
if (!require(MASS)) install.packages("MASS"); library(MASS)

mydata <- read.dta("https://dss.princeton.edu/training/Panel101.dta") 

# Running the ordered logit model
m1 <- MASS::polr(opinion ~ x1 + x2 +x3, data = mydata, Hess = TRUE) # HESS is required for Standard Errors
summary(m1)
m1.coef <- data.frame(coef(summary(m1)))
m1.coef$pval = round((pnorm(abs(m1.coef$t.value), lower.tail = FALSE) * 2),2)

# Create the Regression table
if (!require(stargazer)) install.packages("stargazer"); library(stargazer)
# stargazer::stargazer(m1, type = "html",out="m1.htm")
stargazer(m1, type = "text")

# Predicted Probabilities
m1.pred <- predict(m1, type = "probs")
summary(m1.pred)

# Pick specific values to view the probabilities
setup1 <- data.frame(x1 = rep(mean(mydata$x1),2), # x1 at the mean
                     x2 = rep(mean(mydata$x2),2), # x2 at the mean
                     x3 = c(1,2) # At specific values of x3
                     )
setup1[,c("pred.prob.value")] <- predict(m1, newdata = setup1,type = "probs")

setup1[,c("pred.prob.label")] <- predict(m1, newdata = setup1,type = "class")

# Marginal Effects
if (!require(erer)) install.packages("erer"); library(erer)
m1_error <- erer::ocME(m1, x.mean=TRUE)
x$out # For t and p-values

# Multinomial Logit Model ------------------------------------------------------
if (!require(foreign)) install.packages("foreign"); library(foreign)
if (!require(nnet)) install.packages("nnet"); library(nnet)
if (!require(stargazer)) install.packages("stargazer"); library(stargazer)

mydata = read.dta("http://www.ats.ucla.edu/stat/data/hsb2.dta")

table(mydata$ses)

# Change the reference category to be middle
mydata$ses2 = relevel(mydata$ses, ref = "middle")

multi1 = nnet::multinom(ses2 ~ science + socst + female, data=mydata)

summary(multi1)

# Use Stargazer to get the p-values
stargazer::stargazer(multi1,type="html",out = "multi1.htm")

# Relative Risk Ratios
multi1.rrr = exp(coef(multi1))

multi1.rrr

stargazer(multi1, type="html", coef=list(multi1.rrr), p.auto=FALSE, out="multi1rrr.htm")

allmean <- data.frame(science = rep(mean(mydata$science),2),
                      socst = rep(mean(mydata$socst),2),
                      female = c("male","female"))
allmean[, c("pred.prob.value")] <- predict(multi1, newdata=allmean, type="probs")
allmean[, c("pred.prob.label")] <- predict(multi1, newdata=allmean, type="class")


