data <- read.csv("BostonHousing.csv",header = TRUE)
#print(data)
print(paste0("The number of null observations:", sum(is.null(data))))
cat("\n")
print(paste0("The total number of observations:",nrow(data)))
cat("\n")

print("Maximum of the values for each column:")
colMax <- function(data) sapply(data, max,na.rm=TRUE)
print(colMax(data))
cat("\n")

print("Minimum of the values for each column:")
colMin <- function(data) sapply(data, min,na.rm=TRUE)
print(colMin(data))
cat("\n")

print("Mean of the values for each column")
colMeans(x=data, na.rm = TRUE)
print(colMeans(data))
cat("\n")

print("The standard deviation of the values for each column")
print(sapply(data, sd, na.rm = TRUE))
cat("\n")

print("The number of unique values in each column:")
print(sapply(data, function(x) length(unique(x))))
cat("\n")


print("Pairwise correlation heatmap for all the attributes of the dataset")
cormat <- round(cor(data),2)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()
cat("\n")

print("Prediction of MEDV using regression model=>")
set.seed(12345)

train_md <- lm(medv~., data=data)
predict_md <- predict(train_md, newdata = data[,-14])
plot(predict_md,data$medv, xlab = "Predicted Price" ,ylab="Actual Price")

len=length(data$medv)
diff <- (predict_md - data$medv)

MSE <- sum(diff^2)/len
MAE <- sum(abs(diff))/len
RSE <- sqrt(MSE)

print(paste0("MSE:",MSE))
print(paste0("MAE:",MAE))
print(paste0("RSE:",RSE))
#print(data.frame(coef = round(md$coefficients,2)))


