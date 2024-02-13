library(tidyverse)
library(dplyr)
library(tidyr)
library(caret)
library(e1071)
library(ggplot2)
library(caTools)

#load the dataset
supermarket_sales <-read.csv("C:\\Users\\USER\\OneDrive\\Documents\\Second year\\Data programming with  R\\data sets\\Data set 14\\supermarket_sales.csv")

attach(supermarket_sales)
str(supermarket_sales)

#check the raws and columns count
dim(supermarket_sales)

#check the first 5 raws
head(supermarket_sales)

#check the last 5 raws
tail(supermarket_sales)

#summary dataset
summary(supermarket_sales)

# Identify missing values
missing_values <- is.na(supermarket_sales)

# Summarize missing values
colSums(missing_values)

#remove rows with missing values
na.omit(supermarket_sales)

#Check for Duplicate Rows
duplicate_rows <- supermarket_sales[duplicated(supermarket_sales), ]

#Display Duplicate Rows
print(duplicate_rows)

# Remove duplicates
unique_supermarket_sales <- unique(supermarket_sales)

# Display cleaned data
print(unique_supermarket_sales)

#after the clean data & check for the raws and columns
dim(unique_supermarket_sales) #no null values


#convert to numeric
supermarket_sales$branch<-as.numeric(factor(supermarket_sales$branch))
supermarket_sales$branch

supermarket_sales$city<-as.numeric(factor(supermarket_sales$city))
supermarket_sales$city

supermarket_sales$customer_type<-as.numeric(factor(supermarket_sales$customer_type))
supermarket_sales$customer_type

supermarket_sales$gender_customer<-as.numeric(factor(supermarket_sales$gender_customer))
supermarket_sales$gender_customer

supermarket_sales$payment_method<-as.numeric(factor(supermarket_sales$payment_method))
supermarket_sales$payment_method

supermarket_sales$product_line<-as.numeric(factor(supermarket_sales$product_line))
supermarket_sales$product_line
#boxplots to check outliers
boxplot(supermarket_sales$quantity,horizontal = TRUE)
boxplot(supermarket_sales$unit_cost,horizontal = TRUE)
boxplot(supermarket_sales$revenue,horizontal = TRUE) #outliers found
boxplot(supermarket_sales$gross_income,horizontal = TRUE)#outliers found
boxplot(supermarket_sales$rating,horizontal = TRUE)
boxplot(supermarket_sales$cogs,horizontal = TRUE)#outliers found
boxplot(supermarket_sales$gm_pct,horizontal = TRUE)

#removing outliers

quantile(supermarket_sales$revenue,0.75)+1.5*IQR(supermarket_sales$revenue)
supermarket_sales$revenue[which(supermarket_sales$revenue>991)]<-c(supermarket_sales$revenue[which(supermarket_sales$revenue>991)]*2)
quantile(supermarket_sales$gross_income,0.75)+1.5*IQR(supermarket_sales$gross_income)
supermarket_sales$gross_income[which(supermarket_sales$gross_income>47)]<-c(supermarket_sales$gross_income[which(supermarket_sales$gross_income>47)]*2)
quantile(supermarket_sales$cogs,0.75)+1.5*IQR(supermarket_sales$cogs)
supermarket_sales$cogs[which(supermarket_sales$cogs>944)]<-c(supermarket_sales$cogs[which(supermarket_sales$cogs>944)]*2)

outliers<-boxplot(supermarket_sales$revenue,plot=FALSE)$out
supermarket_sales<-supermarket_sales[-which(supermarket_sales$revenue %in% outliers),]
#all the outliers was removed after removing the outliers in the revenue column

#removing unwanted columns

supermarket_sales<-supermarket_sales[,-c(1,11)]
summary(supermarket_sales)
dim(supermarket_sales)
#removing time column
supermarket_sales <- supermarket_sales[, !colnames(supermarket_sales) %in% "time"]

#depth analysis

boxplot(supermarket_sales$rating,
        main="Customer ratings",
        xlab="Rating",
        horizontal = TRUE,
        col="red",
        border="black"
)
#Histogram of Ratings
ggplot(supermarket_sales, aes(x=rating)) +
  geom_histogram(binwidth=0.5, fill="steelblue",color="black")+
  labs(x="Rating", y="Count", title="Histogram of Ratings")

avg_rating <- aggregate(supermarket_sales$rating, by=list(Product_Line=supermarket_sales$product_line), FUN=mean)

#Bar graph of average ratings per product line
# Rename the columns for clarity
colnames(avg_rating) <- c("Product_Line", "Average_Rating")

# Create the barplot
ggplot(avg_rating, aes(x=Product_Line, y=Average_Rating)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="Product Line", y="Average Rating", title="Average Rating per Product Line")

#Bargraph for average ratings per customer type
avg_rating <- aggregate(supermarket_sales$rating, by=list(Customer_Type=supermarket_sales$customer_type), FUN=mean)

# Rename the columns for clarity
colnames(avg_rating) <- c("Customer_Type", "Average_Rating")

# Create the barplot
ggplot(avg_rating, aes(x=Customer_Type, y=Average_Rating)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x="Customer Type", y="Average Rating", title="Average Rating per Customer Type")

#Average rating by each branch(Pie chart)
# Calculate average rating for each branch
avg_rating_branch <-  supermarket_sales%>%
  group_by(branch) %>%
  summarise(avg_rating = mean(rating, na.rm = TRUE))

# Create pie chart
ggplot(avg_rating_branch, aes(x = "", y = avg_rating, fill = branch)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Average Rating per Branch") +
  theme(legend.title = element_blank()) +
  geom_text(aes(label = paste0(round(avg_rating, 2))), position = position_stack(vjust = 0.5))


#data split
split_ratio<-sample.split(supermarket_sales,SplitRatio=0.75)
train<-subset(supermarket_sales,split_ratio==TRUE)
test<-subset(supermarket_sales,split_ratio==FALSE)

#Linear regression model
set.seed(200)
linear_model <- lm(rating ~ cogs, data = train)

model_summary <- summary(linear_model)

# Extract relevant information
RSE <- sqrt(model_summary$sigma)  # RSE
rsquared <- model_summary$r.squared  # R-squared
adjusted_rsquared <- model_summary$adj.r.squared  # Adjusted R-squared
f_statistic <- model_summary$fstatistic[1]  # F-statistic
summary(linear_model)$coef

#testing the model
predictions <- predict(linear_model, newdata = test)
predictions

mse <- mean((test$rating - predictions)^2)  # Mean Squared Error
mae <- mean(abs(test$rating - predictions))  # Mean Absolute Error
rse <- sqrt(sum(residuals(linear_model, newdata = test)^2) / (length(residuals(linear_model, newdata = test)) - 2))  # Residual Standard Error

# Print the evaluation metrics
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Residual Standard Error (RSE):", rse, "\n")
colnames(supermarket_sales)



#clustering
set.seed(1000)
cluster_results <- kmeans(supermarket_sales,center = 3,nstart =25)
cluster_results
table(cluster_results$cluster,supermarket_sales$payment_method)
# Identify constant variables
constant_vars <- sapply(supermarket_sales, function(x) length(unique(x)) == 1)

# Remove constant variables
supermarket_sales <- supermarket_sales[, !constant_vars]
library(cluster)
clusplot(supermarket_sales, cluster_results$cluster, color = TRUE, shade = TRUE, labels = 1, lines = 0)



#naivebayes classifier
set.seed(100)
model<-naiveBayes(product_line~.,data=train)

summary(test)
predicted<-predict(model,newdata=test)

#confusion matrix
table(predicted)
matrix<-table(test$product_line,predicted)
confusionMatrix(matrix)

# Build a decision tree model to predict the customer gender
library(rpart)
library(rpart.plot)
library(skimr)
skim(supermarket_sales)
split <- createDataPartition(y=supermarket_sales$gender_customer, p=0.75,list=FALSE)
train <-supermarket_sales[split,]
test<-supermarket_sales[,-split]


set.seed(15000)
dec_tree<-rpart(formula=gender_customer~.,
                data=train,
                method="class",
                xval=10
)
#Drawing the tree
rpart.plot(dec_tree,yesno=TRUE)



# Build a linear regression model to predict the sales revenue
set.seed(1000)
linear_model <- lm(revenue ~ ., data = supermarket_sales)

model_summary <- summary(linear_model)

# Extract relevant information
RSE <- sqrt(model_summary$sigma)  # RSE
rsquared <- model_summary$r.squared  # R-squared
adjusted_rsquared <- model_summary$adj.r.squared  # Adjusted R-squared
f_statistic <- model_summary$fstatistic[1]  # F-statistic
summary(linear_model)$coef

#testing the model
predictions <- predict(linear_model, newdata = test)
predictions

mse <- mean((test$rating - predictions)^2)  # Mean Squared Error
mae <- mean(abs(test$rating - predictions))  # Mean Absolute Error
rse <- sqrt(sum(residuals(linear_model, newdata = test)^2) / (length(residuals(linear_model, newdata = test)) - 2))  # Residual Standard Error

# Print the evaluation metrics
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Residual Standard Error (RSE):", rse, "\n")
