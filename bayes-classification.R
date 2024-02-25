library('e1071')
library(e1071)
set.seed(123)
sample_indices <- sample(1:nrow(houses), 0.8 * nrow(houses))
train_data <- houses[sample_indices,]
test_data <- houses[-sample_indices,]
nb <- naiveBayes(Price ~ SquareFeet + Bedrooms + Bathrooms + Neighborhood + YearBuilt, data = train_data)
pred <- predict(nb, test_data)
cm <- table(pred, test_data$Price)
acc <- sum(diag(cm)) / sum(cm)