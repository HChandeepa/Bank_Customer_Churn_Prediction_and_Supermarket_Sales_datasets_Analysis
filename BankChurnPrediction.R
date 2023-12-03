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

#K-means clustering
set.seed(150)
cluster_results<-kmeans(dataset[,2:12],centers = 3,nstart = 25)
cluster_results

table(cluster_results$cluster,dataset$churn)

library(cluster)
clusplot(dataset,cluster_results$cluster,color = T,shade = T,labels = 1,lines = 0)
table(cluster_results$cluster,dataset.class)


library(ggplot2)
#bar plot to visualize 'gender' distribution within 'country'
ggplot(dataset, aes(x = country, fill = gender)) +
  geom_bar(position = "stack") +       
  xlab("Country") +
  ylab("Count") +
  ggtitle("Gender Distribution Across Countries")



