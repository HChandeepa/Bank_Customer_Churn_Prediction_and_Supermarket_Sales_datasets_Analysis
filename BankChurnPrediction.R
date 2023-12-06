#importing the data set
dataset<-read.csv("F:\\Year 2 Sem 1\\R programming\\My projects\\Assignment\\datasets\\Bank Customer Churn Prediction.csv")
dataset

#count missing values
sum(is.na(dataset))
#remove rows with missing values
na.omit(dataset)
#removing duplicates in the dataset
unique(dataset)
#summary dataset
summary(dataset)

library(ggplot2)
#bar plot to visualize 'gender' distribution within 'country'
ggplot(dataset, aes(x = country, fill = gender)) +
  geom_bar(position = "stack") +       
  xlab("Country") +
  ylab("Count") +
  ggtitle("Gender Distribution Across Countries")

dataset$country<-as.numeric(factor(dataset$country))
dataset$country

dataset$gender<-as.numeric(factor(dataset$gender))
dataset$gender

#boxplots to check outliers
boxplot(dataset$age,horizontal = TRUE)#outlier found
boxplot(dataset$credit_score,horizontal = TRUE)#outliers found
boxplot(dataset$tenure,horizontal = TRUE)
boxplot(dataset$balance,horizontal = TRUE)
boxplot(dataset$credit_card,horizontal = TRUE)
boxplot(dataset$active_member,horizontal = TRUE)

# Calculate statistics for 'age' column
age_stats <- boxplot.stats(dataset$age)
# Remove outliers from 'age' column
dataset <-dataset[!(dataset$age %in% age_stats$out), ]
# Calculate statistics for 'credit_score' column
credit_score_status<-boxplot.stats(dataset$credit_score)
# Remove outliers from 'credit_score' column
dataset<-dataset[!(dataset$credit_score%in%credit_score_status$out),]

#boxplots after removing outliers
boxplot(dataset$age,horizontal = TRUE)
boxplot(dataset$credit_score,horizontal = TRUE)
#-------------------------------------------------------------------------------#
# Calculate statistics for 'age' column
age_stats <- boxplot.stats(dataset$age)
# Remove outliers from 'age' column
dataset <-dataset[!(dataset$age %in% age_stats$out), ]
boxplot(dataset$age,horizontal = TRUE)
# Calculate statistics for 'age' column
age_stats <- boxplot.stats(dataset$age)
# Remove outliers from 'age' column
dataset <-dataset[!(dataset$age %in% age_stats$out), ]
boxplot(dataset$age,horizontal = TRUE)
#-------------------------------------------------------------------------------#

# Calculate statistics for 'credit_score' column
credit_score_status<-boxplot.stats(dataset$credit_score)
# Remove outliers from 'credit_score' column
dataset<-dataset[!(dataset$credit_score%in%credit_score_status$out),]
boxplot(dataset$credit_score,horizontal = TRUE)
#-------------------------------------------------------------------------------#
library(e1071)
library(caret)
library(caTools)

#Naive baye's classifier
set.seed(123)
split_ratio<-sample.split(dataset,SplitRatio = 0.75)
training_dataset<-subset(dataset,split_ratio==TRUE)
testing_dataset<-subset(dataset,split_ratio==FALSE)

model1<-naiveBayes(churn~.,data = training_dataset)
predicted_results<-predict(model1,newdata = testing_dataset)

accuracy1<-sum(predicted_results==testing_dataset$churn)/nrow(testing_dataset)
print(round(accuracy1,2))

table(predicted_results)

matrix<-table(testing_dataset$churn,predicted_results)
confusionMatrix(matrix)

#linear regression model
# Assuming 'churn' is the target variable, you may need to split the dataset again as linear regression works differently than classification.
set.seed(123)
split_ratio <- sample.split(dataset, SplitRatio = 0.75)
training_dataset <- subset(dataset, split_ratio == TRUE)
testing_dataset <- subset(dataset, split_ratio == FALSE)

# Train the linear regression model
linear_model <- lm(churn ~ ., data = training_dataset)

# Predict using the model on the test dataset
linear_predictions <- predict(linear_model, newdata = testing_dataset)

# Calculate accuracy using confusion matrix
linear_predictions <- ifelse(linear_predictions > 0.5, 1, 0)  # Assuming binary classification
linear_accuracy <- sum(linear_predictions == testing_dataset$churn) / nrow(testing_dataset)
print(round(linear_accuracy, 2))

# Confusion matrix
linear_matrix <- table(testing_dataset$churn, linear_predictions)
confusionMatrix(linear_matrix)



unique_country<-unique(dataset$country)
print(unique_country)

country_counts <- table(dataset$country)
print(country_counts)

#pie chart of the countries
lbls <- c("Spain", "Germany", "France")
slices <- c(2331, 2357, 4709)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")


