## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Check working directory:
getwd()

# Change  current directory.
setwd(dir='/Users/Claire/Documents/LSE Data Analytics/Course 3/Final Assignment/LSE_DA301_assignment_files')

# Install and import Tidyverse.
library(tidyverse)

# Import the data set.
sales_data <- read.csv('turtle_sales.csv', header = TRUE)

# Print the data frame and sense check the data:
sales_data
View(sales_data)
str(sales_data)
typeof(sales_data)
class(sales_data)
dim(sales_data)
as_tibble(sales_data)
# There are 352 rows and 9 columns.

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales_data2 <- select(sales_data, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
View(sales_data2)

# View the descriptive statistics.
summary(sales_data2)

# Convert 'Product' to factor (IDs are unique to product, but product
# may be on several different platforms):
sales_data3 <- mutate(sales_data2,
                 Product = as.factor(Product))

summary(sales_data3)
View(sales_data3)

# To check all Platform and Product values to ensure all valid and no NA values:
table(sales_data3$Platform)
table(sales_data3$Product)
# There are no NA values.

# Clean data frame created for all data to analyse later:
all_data <- mutate(sales_data,
                   Product = as.factor(Product))

# Data viewed to check format and categories to understand content:
View(all_data)
table(all_data$Ranking)
table(all_data$Year)
table(all_data$Genre)
table(all_data$Publisher)
table(all_data$Platform)

################################################################################

# 2. Review plots to determine insights into the data set.

# Scatterplots of the sales data to interpret spread:

qplot(y=Global_Sales, data=sales_data3)
qplot(y=NA_Sales, data=sales_data3)
qplot(y=EU_Sales, data=sales_data3)

# The scatterplots for each region show an outlier in terms of sales (i.e. a
# point on the graph where sales have been much higher than others).  The data
# is subset to see what this value is:
subset(sales_data3,Global_Sales >= 67)
subset(sales_data3,NA_Sales >= 34)
subset(sales_data3,EU_Sales >= 23)
# The value appears to be product 107 on the Wii platform, of the 'Sports' genre
# which appears to have sold a lot more than other products.
# Interesting!  Could this be a result of the pandemic, i.e. people buying a
# Wii fit game to keep fit at home?  It that why it is so high?  It's also quite
# Family-friendly perhaps, or appealing to a wider age group due to it's
# interactive nature as a platform.

# Histogram of global sales.
qplot(Global_Sales, data=sales_data3, bins = 30)
# The histogram shows that global sales per product are usually below 20 
# million.

# Histogram of EU sales
qplot(EU_Sales, data=sales_data3, bins = 30)

# Histogram of NA sales
qplot(NA_Sales, data=sales_data3, bins = 30)

sum(all_data$Global_Sales)  # Total global sales = 1877.81
sum(all_data$NA_Sales)  # Total North American sales = 885.62
sum(all_data$EU_Sales)  # Total EU sales = 578.61
# This suggests that sales are greater in NA, but also indicates that the global
# sales include other sales that were neither labelled as EU or NA.  This is confirmed
# by the metadata for Global_Sales.

# Barplot to see which are most popular genres.
qplot(Genre, data=all_data, geom='bar')
# Shooter and action seem to be the most popular genres.

# Barplot to see which are most popular platforms.
qplot(Platform, data=all_data, geom='bar')
# X360 is the platform with the most sales globally.

# View statistics and generate a boxplot to identify any outliers:
summary(all_data)
boxplot(all_data$Global_Sales)

# IQR of Global_Sales:
6.435 - 1.115

# Lower limit of Global_Sales:
1.115 - 1.5 * 5.32

# Upper limit of Global_Sales:
6.435 + 1.5 * 5.32

# Using the above calculations, a Dataframe without outliers in the global 
# sales data is created for later use:
all_data_no_outliers <- filter(all_data,
                  Global_Sales <= 14.415)

View(all_data_no_outliers)


###############################################################################

# 3. Observations and insights

## There are additional sales not accounted for in global sales that do not form
## part of EU or NA; we can't tell what products these relate to or indeed the
## sales location.  North America has greater sales total than the EU, and product
## 107 has by far the most sales - it is not clear whether this is an accurate data
## point or not, but is indeed the highest selling in both the EU and NA and is
## an interesting point to note.




###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
View(sales_data3)

# View the descriptive statistics.
summary(sales_data3)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
aggregate(Global_Sales~Product+Platform, sales_data3, sum)

df_sales_platform <- all_data %>% group_by(Product, Platform, Genre) %>%
  summarise(sum_eu_sales=sum(EU_Sales),
            sum_na_sales=sum(NA_Sales),
            sum_global_sales=sum(Global_Sales),
            .groups='drop')

df_sales <- sales_data3 %>% group_by(Product) %>%
  summarise(sum_eu_sales=sum(EU_Sales),
            sum_na_sales=sum(NA_Sales),
            sum_global_sales=sum(Global_Sales),
            .groups='drop')

# View the data frame.
View(df_sales_platform)
View(df_sales)
# The aggregated data shows that:
#  Product 107 is the top seller in the EU, NA and globally.
#  Otherwise, best-selling global products are 123, 195, 231
#  Best-selling in the EU are 195, 231, 399
#  Best-selling in NA are 123, 326, 254

# Explore the data frame.
dim(df_sales)
head(df_sales)
as_tibble(df_sales)

# Dataframe is subset to include the top 10 products only, first by sorting the
# data into descending order and then by only selecting those in the top 10
# for charting.
sorted_df_platform <- arrange(df_sales_platform, desc(sum_global_sales))

View(sorted_df_platform)

top10products = filter(sorted_df_platform, sum_global_sales>20.30)


# Charts are then created showing the top 10 products and their platform/genre  Bar
# charts are best for this and colours are used to also demonstrate the platform/genre they
# relate to.  The charts have also been made interactive, to show the total sales on a mouse
# hover:

#Import plotly
library(plotly)

productchart <- ggplot(top10products, aes(x=Product, y=sum_global_sales, fill=Platform)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top 10 Products and Their Platform")
ggplotly(productchart)

# Chart showing the top 10 products and their genre:
genrechart <- ggplot(top10products, aes(x=Product, y=sum_global_sales, fill=Genre)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top 10 Products by Genre")
ggplotly(genrechart)

# The same is then done for both the EU and NA sales.

# EU sales:
# EU sales are sorted from highest sales to lowest:
sorted_df_platform_eu <- arrange(df_sales_platform, desc(sum_eu_sales))

View(sorted_df_platform_eu)

# Then only top 10 products are filtered into a new dataframe for charting:
top10productseu = filter(sorted_df_platform_eu, sum_eu_sales>6.58)

euproductchart <- ggplot(top10productseu, aes(x=Product, y=sum_eu_sales, fill=Platform)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top 10 Products by Platform in the EU")
ggplotly(euproductchart)

eugenrechart <- ggplot(top10productseu, aes(x=Product, y=sum_eu_sales, fill=Genre)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top 10 Products by Genre in the EU")
ggplotly(eugenrechart)

# Just top genres and top platforms in the EU are charted:
ggplot(all_data, aes(x=Genre, y=EU_Sales)) + 
  geom_bar(stat = "identity")

ggplot(all_data, aes(x=Platform, y=EU_Sales)) + 
  geom_bar(stat = "identity")

# NA Sales:
sorted_df_platform_na <- arrange(df_sales_platform, desc(sum_na_sales))

View(sorted_df_platform_na)

top10productsna = filter(sorted_df_platform_na, sum_na_sales>9.33)

naproductchart <- ggplot(top10productsna, aes(x=Product, y=sum_na_sales, fill=Platform)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top 10 Products by Platform in North America")
ggplotly(naproductchart)

nagenrechart <- ggplot(top10productsna, aes(x=Product, y=sum_na_sales, fill=Genre)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top 10 Products by Genre in North America")
ggplotly(nagenrechart)

# Just top genres and top platforms in NA are charted:
ggplot(all_data, aes(x=Genre, y=NA_Sales)) + 
  geom_bar(stat = "identity")

ggplot(all_data, aes(x=Platform, y=NA_Sales)) + 
  geom_bar(stat = "identity")
###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots

qqnorm(sales_data3$Global_Sales,
       col='blue',
       xlab="z Value",
       ylab='Sales (per Million $)')

qqline(sales_data3$Global_Sales,
       col='red',
       lwd=2) 

qqnorm(sales_data3$NA_Sales,
       col='blue',
       xlab="z Value",
       ylab='Sales (per Million $)')
       
qqline(sales_data3$NA_Sales,
        col='red',
        lwd=2) 

qqnorm(sales_data3$EU_Sales,
        col='blue',
        xlab="z Value",
        ylab='Sales (per Million $)')
              
qqline(sales_data3$EU_Sales,
        col='red',
        lwd=2) 

# The data appears to have heavy tails.

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library (moments)

# Perform Shapiro-Wilk test.
shapiro.test(sales_data3$Global_Sales)
shapiro.test(sales_data3$EU_Sales)
shapiro.test(sales_data3$NA_Sales)
# The test indicates that the data is not normally-distributed, with a very
# small p-value for all three sales data.


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales_data3$Global_Sales) 
kurtosis(sales_data3$Global_Sales)
# Skewness is 4.046 and kurtosis is 32.64, indicating positive skewness and
# heavy tails.

## 3d) Determine correlation
# Determine correlation.
cor(sales_data3$Global_Sales, sales_data3$NA_Sales) # 0.935
cor(sales_data3$Global_Sales, sales_data3$EU_Sales) # 0.878
cor(sales_data3$EU_Sales, sales_data3$NA_Sales) # 0.706
# There is quite strong positive correlation between the data sets, but this is 
# expected as regards the relationship between NA/EU data and the global data,
# as they are a component of this.  EU and NA sales data are also quite correlated.

# The same is now carried out using the dataframe created earlier where outliers were
# removed:

# 4. Determine the normality of the data set.

## 4a) Create Q-Q Plots

qqnorm(all_data_no_outliers$Global_Sales,
       col='blue',
       xlab="z Value",
       ylab='Sales (per Million $)')

qqline(all_data_no_outliers$Global_Sales,
       col='red',
       lwd=2) 

qqnorm(all_data_no_outliers$NA_Sales,
       col='blue',
       xlab="z Value",
       ylab='Sales (per Million $)')

qqline(all_data_no_outliers$NA_Sales,
       col='red',
       lwd=2) 

qqnorm(all_data_no_outliers$EU_Sales,
       col='blue',
       xlab="z Value",
       ylab='Sales (per Million $)')

qqline(all_data_no_outliers$EU_Sales,
       col='red',
       lwd=2) 

# The data still appears to have heavy tails but less so than in the previous
# analysis.

## 4b) Perform Shapiro-Wilk test

shapiro.test(all_data_no_outliers$Global_Sales)
shapiro.test(all_data_no_outliers$EU_Sales)
shapiro.test(all_data_no_outliers$NA_Sales)
# The test indicates that the data is still not normally-distributed, with a very
# small p-values still (well below p > 0.05).


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(all_data_no_outliers$Global_Sales) # 0.715
kurtosis(sales_data3$Global_Sales) # 32.64
# The data is less skewed, although kurtosis remains the same.

## 3d) Determine correlation
# Determine correlation.
cor(all_data_no_outliers$Global_Sales, all_data_no_outliers$NA_Sales) # 0.891
cor(all_data_no_outliers$Global_Sales, all_data_no_outliers$EU_Sales) # 0.832
cor(all_data_no_outliers$EU_Sales, all_data_no_outliers$NA_Sales) # 0.614
# Data is still highly correlated, albeit less so than using the previous
# dataset.


###############################################################################

# 4. Observations and insights
# Your observations and insights here...

## Wii appears to be the most popular platform, although it is not clear how the
## top product (107 on Wii) was recorded and whether this was an outlier or not.
## Different products tend to be popular in each region with little pattern, although
## there clearly are sales outside of these regions that are included in the global
## sales figures.  The data is not normally-distributed and is positively skewed
## with heavy tails.  Some further information could improve reliability of the data; 
## it appears that only computer games are included in the data, whereas Turtle Games
## also sells other products.  The full sales data would potentially make the 
## data more reliable for analysis.


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
View(sales_data3)
View(df_sales)

# Determine a summary of the data frame.
summary(sales_data3)
summary(df_sales)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.

# Using the original data:
globalmodelna <- lm(Global_Sales~NA_Sales,
             data=sales_data3)

globalmodelna

summary(globalmodelna)
# R squared is 87.41%

globalmodeleu <- lm(Global_Sales~EU_Sales,
                   data=sales_data3)

globalmodeleu

summary(globalmodeleu)
# R squared is 77.01%
# NA sales do, however, make up more of the global sales total that EU data.

# Using the sum dataframe (df_sales):
summodeleu <- lm(sum_global_sales~sum_eu_sales,
                data=df_sales)

summodeleu

summary(summodeleu)
# R squared is 72.01%.

summodelna <- lm(sum_global_sales~sum_na_sales,
                data=df_sales)

summodelna

summary(summodelna)
# R squared is 83.95%.


## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(sales_data3$Global_Sales, sales_data3$NA_Sales)
plot(sales_data3$Global_Sales, sales_data3$EU_Sales)
plot(sales_data3$NA_Sales, sales_data3$EU_Sales)

plot(df_sales$sum_global_sales, df_sales$sum_na_sales)
plot(df_sales$sum_global_sales, df_sales$sum_eu_sales)
plot(df_sales$sum_na_sales, df_sales$sum_eu_sales)

# View EU residuals on a plot.
plot(summodeleu$residuals)

# Plot the relationship with base R graphics.
plot(df_sales$sum_eu_sales, df_sales$sum_global_sales)
coefficients(summodeleu)

# Add line-of-best-fit.
abline(coefficients(summodeleu))

# View NA residuals on a plot.
plot(summodelna$residuals)

# Plot the relationship with base R graphics.
plot(df_sales$sum_na_sales, df_sales$sum_global_sales)
coefficients(summodelna)

# Add line-of-best-fit.
abline(coefficients(summodelna))

# Complete a log transformation with dplyr's mutate() function.
# This is applied to sum_global_sales.
df_sales <- mutate(df_sales, 
              logGlobal=log(sum_global_sales))

# View new object with new variable.
df_sales


# Create a new model using logGlobal for the EU data.
summodeleu2 <- lm(logGlobal~sum_eu_sales,
             data=df_sales)

# View full regression table.
summary(summodeleu2)

# Plot the relationship between EU sales and logGlobal.
plot(df_sales$sum_eu_sales, df_sales$logGlobal)


# Add a line-of-best fit to existing plot.
abline(coefficients(summodeleu2))
# Worse model generated (R squared = 62%)

# The same is now done for the NA data.
summodelna2 <- lm(logGlobal~sum_na_sales,
                  data=df_sales)

# View full regression table.
summary(summodelna2)

# Plot the relationship between EU sales and logGlobal.
plot(df_sales$sum_na_sales, df_sales$logGlobal)

# Add a line-of-best fit to existing plot.
abline(coefficients(summodelna2))
# Also a worse model with 67.31% R squared.

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.

# Import the psych package.
library(psych)

# Multiple linear regression model.
multimodel = lm(sum_global_sales~sum_eu_sales+sum_na_sales,
            data=df_sales)

summary(multimodel)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
prediction_values <- data.frame(sum_na_sales=c(34.02, 3.93, 2.73, 2.26, 22.08),
                 sum_eu_sales=c(23.80, 1.56, 0.65, 0.97, 0.52))

View(prediction_values)

predict(multimodel,
        newdata=prediction_values)

# Add the values to the prediction_values data frame.
prediction_values$sum_global_sales <- predict(multimodel,
                                newdata=prediction_values)

View(prediction_values)

# To find actual values for comparison to the predicted values:
subset(df_sales, sum_eu_sales==0.52)
###############################################################################

# 5. Observations and insights
# Your observations and insights here...
## In terms of linear regression, models using the sum of sales per product produced
## the best models (72% and 84% for EU and NA data respectively).  It seems logical
## that the NA data would be a better model than EU because it appears to make up a
## larger percentage of the global data.  The models could potentially be more useful
## for business predictions if data on all the sales (not just computer games) were 
## available.  However, the multiple linear regression model seems to work most 
## effectively at predicting values, given that the predicted global sales by product 
## values match closely to the actual values in the data set (e.g. actual value 67.85,
## predicted value 68.06)


###############################################################################
###############################################################################




