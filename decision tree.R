# data exploratory
houses<-read.csv("housing_price_dataset.csv")
head(houses)
summary(houses)
any(is.na(houses))
summary(houses$Neighborhood)
summary(houses$Price)
houses$Price <- round(houses$Price, 2)
head(houses)
summary(houses$Price)

install.packages("rpart")
install.packages("rpart.plot")

# install and load required package
library(rpart)
library(rpart.plot)

# read dataset from CSV file
houses <- read.csv("housing_price_dataset.csv", stringsAsFactors = TRUE)

# convert var Neighborhood to factor of 0,1,2
houses$Neighborhood <- factor(houses$Neighborhood, levels = c("Rural", "Suburb", "Urban"), labels = c(0, 1, 2))

set.seed(123) # set seed number for reproducibility

# split data for training and testing sets
data_split <- sample.int(n = nrow(houses), size = floor(nrow(houses)*0.8))
train_data <- houses[data_split,]
test_data <- houses[-data_split,]

# build regression decision tree model using training data
model_rpart <- rpart(Price ~ ., data = train_data, method = "anova")
print(model_rpart) # print the details of trained model

# visualizing the decision tree-plotting the tree
rpart.plot(model_rpart, extra = 101, main = "House Price Prediction" )

# make predictions on test data
predictions_rpart <- predict(model_rpart, test_data)
predicted_prices <- predict(model_rpart, test_data)

# calculate residuals error (substracting actual prices from predicted prices)
residuals <- test_data$Price - predicted_prices
squared_residuals <- residuals^2 # calculate squared_residuals
mean_squared_error <- mean(squared_residuals) # calculate mean_squared_error
rmse <- sqrt(mean_squared_error) # calculate root for mean_squared_error

# display results for rmse and mean_squared_error
print(rmse)
print(mean_squared_error)