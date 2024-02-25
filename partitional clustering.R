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

install.packages("ggplot2")
houses$Neighborhood_numeric <- as.numeric(as.factor(houses$Neighborhood))
summary(houses$Neighborhood_numeric) # summary statistics for variable Neighborhood
summary(houses$Neighborhood) # summary statistics for variable Neighborhood
summary(houses)

# histograms for various features in the dataset
hist(houses$SquareFeet, col = 'grey')
hist(houses$Bedrooms, col = 'green')
hist(houses$Bathrooms, col = 'pink')
hist(houses$Price, col = 'yellow')
hist(houses$Neighborhood_numeric, col = 'red')
hist(houses$YearBuilt, col = 'blue')

# create scatterplot using ggplot2
library(ggplot2)
ggplot(houses, aes(x = YearBuilt, y = Price, color = factor(Neighborhood_numeric), size = SquareFeet)) + geom_point(aes(shape = factor(Bedrooms)), alpha = 0.7) +
  labs(title = "House Price Prediction Plot",
       x = "Year Built",
       y = "Price",
       color = "Neighborhood Numeric",
       size = "Square Feet",
       shape = "Bedrooms") +
  theme_minimal()
k <- 3
k_3<-kmeans(houses[, c("SquareFeet", "Bedrooms", "Bathrooms", "Price", "Neighborhood_numeric", "YearBuilt")],3) # perform k-means clustering with k=3
cluster_centers <- k_3$centers
clusters <- k_3$cluster
clus<-cbind(houses,clus2=k_3$cluster) # combine cluster information with original dataset

# create scatter plot of SquareFeet against Price, colored by cluster
plot(clus$SquareFeet, clus$Price, col=k_3$cluster, pch=k_3$cluster, main = "SquareFeet and Price", xlim=c(0,3000), ylim=c(0,300000), xlab= "SquareFeet", ylab="Price")

predicted_prices <- cluster_centers[clusters, "Price"]

# Calculate Mean Squared Error (MSE) and Root Mean Squared Error (RMSE)
true_prices <- houses$Price
mse <- mean((true_prices - predicted_prices)^2)
rmse <- sqrt(mse)

# Print MSE and RMSE
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

plot(housing$SquareFeet, housing$Price, asp=1)
two <- kmeans(housing, 2)
three <- kmeans(housing, 3)
plot(clus$SquareFeet, clus$Price, col=two$cluster, asp=1, pch=two$cluster, main="Square feet house and Price", xlab="Sq ft", ylab="Price")
plot(clus$SquareFeet, clus$Price, col=three$cluster, asp=1, pch=three$cluster, main="Square feet house and Price", xlab="Sq ft", ylab="Price")