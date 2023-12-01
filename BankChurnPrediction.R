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


set.seed(123)
split_ratio<-sample.split(dataset,SplitRatio = 0.7)
training_dataset<-subset(dataset,split_ratio==TRUE)
testing_dataset<-subset(dataset,split_ratio==FALSE)

model<-naiveBayes(churn~.,data = training_dataset)
predicted_results<-predict(model,newdata = testing_dataset)

accuracy<-sum(predicted_results==testing_dataset$churn)/nrow(testing_dataset)
print(round(accuracy,2))

table(predicted_results)

matrix<-table(testing_dataset$churn,predicted_results)
confusionMatrix(matrix)
