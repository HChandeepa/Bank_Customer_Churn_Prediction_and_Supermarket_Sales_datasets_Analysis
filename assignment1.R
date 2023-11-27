#load the dataset 
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

#remove unnwanted columns
columns_to_keep <- c(2,3, 4,5,6,7,8,9,10,11,12)
dataset <- dataset[, columns_to_keep]
dataset
#convert values to numeric using label encoding
dataset$country<-as.numeric(factor(dataset$country))
dataset$country

dataset$gender<-as.numeric(factor(dataset$gender))
dataset$gender

dataset

library(caTools)
library(caret)
#splitting the dataset in training and testing
split_ratio<-sample.split(dataset,SplitRatio = 0.8)
training_dataset<-subset(dataset,split_ratio==TRUE)
dim(training_dataset)

testing_dataset<-subset(dataset,split_ratio==FALSE)
dim(testing_dataset)

#feature scaling
# Identify the numeric columns to scale
numeric_columns <- c("credit_score","country","gender","age","tenure","balance","products_number","credit_card","active_member","estimated_salary")
numeric_columns<-c(2,3, 4,5,6,7,8,9,10,11,12)

# Scale the numeric columns in the training dataset
training_dataset[, columns_to_keep] <- scale(training_dataset[,columns_to_keep])

# Scale the same columns in the testing dataset using the mean and standard deviation from the training dataset
testing_dataset[, numeric_columns] <- scale(testing_dataset[, numeric_columns], center = attr(training_dataset[, numeric_columns], "scaled:center"), scale = attr(training_dataset[, numeric_columns], "scaled:scale"))
# Check the column names of your dataset
names(training_dataset)
# or
colnames(training_dataset)


#boxplots to check outliers
boxplot(data$age,horizontal = TRUE)#outlier found
boxplot(dataset$credit_score,horizontal = TRUE)#outliers found
boxplot(dataset$tenure,horizontal = TRUE)
boxplot(dataset$balance,horizontal = TRUE)
boxplot(dataset$credit_card,horizontal = TRUE)
boxplot(dataset$active_member,horizontal = TRUE)

'
#calculate z scors for age and credit score
z_scores_age<-scale(modified_data$age)
z_scores_credit_score<-scale(modified_data$credit_score)

#set a threshold for  z score 
threshold<-3

#identify outliers based on z-score
outliers_age<- abs(z_scores_age)>threshold
outliers_credit_score<-abs(z_scores_credit_score)>threshold

#combine outliers for both columns
combined_outliers<-outliers_age|outliers_credit_score

#remove rows with outliers
modified_data_filtered<-modified_data[!combined_outliers]'


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



#visualizing data
library(ggplot2)
#counts of countries
country_counts<-table(dataset$country)
print(country_counts)

'# Example data
dataset<- data.frame(
  country = c("France", "Spain", "Germany", "Spain", "Germany", "France", "France", "Germany", "Spain", "France")
)'


# Create a bar plot with different colors for specific countries and display their names

# Convert 'country' column to factor if it's not already

ggplot(dataset, aes(x = country, fill = country)) +
  geom_bar(color = "black") +
  xlab("Countries") +
  ylab("Frequencies") +
  ggtitle("Countries in the Bank")

#histogram related to the ages
ggplot(dataset,aes(x=age))+
  geom_histogram(color="black",binwidth = 2,fill="blue")+
  xlab("Ages")+
  ylab("Frequencies")+
  ggtitle("Age ranges")
#tenure of the bank
ggplot(dataset,aes(x=tenure))+
  geom_histogram(color="black",binwidth = 3,fill="skyblue")+
  xlab("Tenure")+
  ylab("Frequencies")+
  ggtitle("Tenure of the bank")
#histogram for balance column 
ggplot(dataset,aes(x=balance))+
  geom_histogram(color="black",binwidth = 10000,fill="green")+
  xlab("Balance")+
  ylab("Frequencies")+
  ggtitle("Balances of the accounts")

#histogram for products number
ggplot(dataset,aes(x=products_number))+
  geom_histogram(color="black",binwidth = 0.5,fill="purple")+
  xlab("products number")+
  ylab("Frequencies")+
  ggtitle("Products")
#genders in the churn

dataset$gender <- factor(dataset$gender)

ggplot(dataset, aes(x = gender, fill = gender)) +
  geom_bar() +
  xlab("Genders") +
  ylab("Frequencies") +
  ggtitle("Gender Distribution") +
  scale_fill_manual(values = c("blue", "pink"))
#credit card users
credit_card_counts<-table(dataset$credit_card)
print(credit_card_counts)


ggplot(dataset, aes(x = credit_card, fill = credit_card)) +
  geom_bar() +
  xlab("Credit card") +
  ylab("Frequencies") +
  ggtitle("Credit card users in the bank") 

