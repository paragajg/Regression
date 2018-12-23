# Step1 Load Libraries
library(e1071)
library(MASS) # used for variable selection
library(car) # used for checking Multicollinearity
library(corrplot) # to plot correlation matrix among variables

# Step2: Import data
df = read.csv("Assignment_regression.csv")

#Step3: Explore data
summary(df)

# Step4 Preprocessing if needed
df$metro = as.factor(df$metro)

# Step5 Split data into trainning and testing set
## 75% of the sample size
smp_size <- floor(0.70 * nrow(df))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ] # randomly created training set
test <- df[-train_ind, ]# randomly created test set

# Step 4: Explore the training data to find visible patterns
summary(train)

# Step 5: Training the model

model = lm(formula = tenure ~.,data = train)
summary(model)

## Assumptions for multicollinearity and randomness in residuals
vif(model) # variance inflation factors 
sqrt(vif(model)) > 2 # problem?
correlations <- cor(train[-c(6,9)])
corrplot(correlations, method="number")

# Step6: Predict model on test data
income_predictions = predict(model,test)
head(income_predictions,5)

# Step7: Adding predictions and extracting data
error <- income_predictions - test[,9]
# Function that returns Root Mean Squared Error
rmse <- function(x)
{
  sqrt(mean(x^2))
}
rmse(error)


# Tip: It is always advisable to view the predicted values within a range rather than a number. This gives perspective to the user of variation that can happen in the prediction of the outcome.

predict_interval <- predict(model,test,interval = "prediction")
View(predict_interval)
test <- cbind(test,predict_interval)
View(test)
write.csv(x = test,file = "predictions.csv")

