library(tidyverse)
library(dplyr)
library(tidyr)
library(caret)
library(e1071)
library(ggplot2)

#load the dataset
supermarket_sales <-read.csv("C:\\Users\\HP\\Desktop\\supermarket_sales.csv")
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

library(ggplot2)
#bar plot to visualize 'gender' distribution within 'country'
ggplot(supermarket_sales, aes(x = city, fill = gender_customer)) +
  geom_bar(position = "stack") +       
  xlab("City") +
  ylab("Count") +
  ggtitle("Gender Distribution Across City")

library(ggplot2)
#bar plot to visualize 'gender' distribution within 'country'
ggplot(supermarket_sales, aes(x = branch, fill = gender_customer)) +
  geom_bar(position = "stack") +       
  xlab("branch") +
  ylab("Count") +
  ggtitle("Branch Distribution Across Gender")

library(ggplot2)
#bar plot to visualize 'gender' distribution within 'country'
ggplot(supermarket_sales, aes(x = city, fill = payment_method)) +
  geom_bar(position = "stack") +       
  xlab("City") +
  ylab("Count") +
  ggtitle("Payment Method Distribution Across City")

library(ggplot2)
#bar plot to visualize 'gender' distribution within 'country'
ggplot(supermarket_sales, aes(x = rating, fill = gender_customer)) +
  geom_bar(position = "stack") +       
  xlab("City") +
  ylab("Count") +
  ggtitle("rating Distribution Across gender")

library(ggplot2)
#bar plot to visualize 'gender' distribution within 'country'
ggplot(supermarket_sales, aes(x = branch, fill = rating)) +
  geom_bar(position = "stack") +       
  xlab("branch") +
  ylab("Count") +
  ggtitle("branch Distribution Across rating")

#scatter plot for Gender vs Age Scatter Plot
ggplot(supermarket_sales, aes(x = city, y = gross_income)) +
  geom_point() +
  xlab("city") +
  ylab("Gender") +
  ggtitle("Gender vs Age Scatter Plot")


#bar plot to visualize credit card distribution within genders
ggplot(supermarket_sales,aes(x=branch,fill=unit_cost))+
  geom_bar(position = "stack")+
  xlab("gender")+
  ylab("credit card")+
  ggtitle("credit card distribution across the country")

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

#boxplots to check outliers
boxplot(supermarket_sales$quantity,horizontal = TRUE)
boxplot(supermarket_sales$unit_cost,horizontal = TRUE)
boxplot(supermarket_sales$revenue,horizontal = TRUE) #outliers found
boxplot(supermarket_sales$gross_income,horizontal = TRUE)#outliers found
boxplot(supermarket_sales$rating,horizontal = TRUE)
boxplot(supermarket_sales$cogs,horizontal = TRUE)#outliers found
boxplot(supermarket_sales$gm_pct,horizontal = TRUE)




