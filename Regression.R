## Step1: Load libraries
library(e1071)
library(MASS) # used for variable selection
library(car) # used for checking Multicollinearity
library(corrplot) # to plot correlation matrix among variables

## Step 2: Import data

df <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data",header = F)
View(df)
headers = read.csv("regression_headers.csv",header = F,stringsAsFactors = F)
nrow(headers)
colnames(df) <- headers
View(df)
## Why all the header names added only to first column ?
## Ans. beacause headers is a dataframe and needs to be converted to vector before assigning it as column names of dataframe
headers = unlist(headers)
colnames(df) <- headers
View(df)
df <- df [,!colnames(df) == "CHAS"]
# or df <- df[,-4]

sum(is.na(df)) # simple code to check if there are any blank values in the data. In exisintg data there are none hence output is  0

# Step 3: Create data partition for training & test data

## 75% of the sample size
smp_size <- floor(0.75 * nrow(df))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ] # randomly created training set
test <- df[-train_ind, ]# randomly created test set

# Step 4: Explore the training data to find visible patterns
summary(train)


fit <- lm(MEDV ~.,data = train) # ~. means select remaining columns as my features or dependent variables for linear regression model
summary(fit)

# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 

# 4.2 diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

# 4.3 Checking Multicollinearity among the factors
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?
correlations <- cor(train[-13])
corrplot(correlations, method="number")
# Multicolinearity can be reduced by removing some of the highly correlated variables from the model. Multiicollinearity induces instability to the model thereby making it inaccurate to predict target variable

# Step 5: Test our model
predictions <- predict(fit,newdata = test)
error <- predictions - test[,13]
# Function that returns Root Mean Squared Error
rmse <- function(x)
{
  sqrt(mean(x^2))
}
rmse(error)




# Tip: It is always advisable to view the predicted values within a range rather than a number. This gives perspective to the user of variation that can happen in the prediction of the outcome.

predict_interval <- predict(fit,test,interval = "prediction")
View(predict_interval)
test <- cbind(test,predict_interval)
View(test)
write.csv(x = test,file = "predictions.csv")

######## Feature Transformation - using log transformation
train_transf = as.data.frame(lapply(train,function(x) log(x)))
train_transf = as.data.frame(lapply(train_transf,function(x) ifelse(x < 0,0,x)))
#train_transf$MEDV = train$MEDV

#hist(train_transf$INDUS)
#hist(train$INDUS)

fit <- lm(MEDV ~.,data = train_transf)
summary(fit)
fit.opt = stepAIC(fit)
summary(fit.opt)

# Step 5: Test our model
test_transf = as.data.frame(lapply(test,function(x) log(x)))
test_transf = as.data.frame(lapply(test_transf,function(x) ifelse(x < 0,0,x)))
#test_transf$MEDV = test$MEDV

predictions <- predict(fit.opt,newdata = test_transf)
error_transf <- predictions - test_transf[,13]
# Function that returns Root Mean Squared Error
rmse_transf <- function(x)
{
  sqrt(mean(x^2))
}
rmse_transf(error_transf)

#View(predictions)
predictions_transformed = exp(predictions)
test <- cbind(test,predictions_transformed)
View(test)

# Create a new metric to publish accuracy of model
test$tenper = ifelse(test$predictions_transformed < 1.1 * test$MEDV & test$predictions_transformed > 0.9*test$MEDV , 1 ,0)

print(paste('Predictions around 10% of actual value :', sum(test$tenper) / nrow(test)*100,'%'))


test$fifteenper = ifelse(test$predictions_transformed < 1.15 * test$MEDV & test$predictions_transformed > 0.85*test$MEDV , 1 ,0)

print(paste('Predictions around 15% of actual value :', sum(test$fifteenper) / nrow(test)*100,'%'))


test$twentyper = ifelse(test$predictions_transformed < 1.20 * test$MEDV & test$predictions_transformed > 0.80 * test$MEDV , 1 ,0)

print(paste('Predictions around 20% of actual value :', sum(test$twentyper) / nrow(test)*100,'%'))


write.csv(x = test,file = "predictions.csv")

