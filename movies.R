#cleaning the environment

rm(list= ls())

#load packages

library(tidyverse)
library(janitor)
library(skimr)
library(reshape)
library(lubridate)
library(dplyr)
library(scales)
library(ggplot2)


#load data

data <- read.csv("C:/Users/USER/Downloads/movies.csv")

# Look at the structure of the data

str(data)

# checking missing value

table(is.na(data))
any(is.na(data))

#correct data type
data$budget <- as.integer(data$budget)
data$gross <-  as.integer(data$gross)

# correct the released year

data_df <- data%>% mutate(relased_year = substr(released,1,4))
str(data_df)

#removing duplicates

distinct(sort_df)

# summary of the data
summary(data_df)

#sort data by gross

sort_df <- data_df[order(data_df$gross, decreasing = TRUE, na.last = NA),  ]

head(sort_df)

# scatter plot budget with gross

my_graph <- sort_df %>% ggplot(aes(x=budget, y=gross))+ 
  geom_point() + labs(x = 'Budget of firm', 
                      y = 'gross revenue', 
                      title = 'budget vs gross',
                      colour = "gear", subtitle = "Relationship  between budget and Revenue") + 
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = dollar)
my_graph

#adding fitted value 

fit_data <- my_graph + stat_smooth(method = "lm", colour = "blue", se = FALSE, size = 1)
fit_data

# extracting the numeric columns
Num_data <- select_if(sort_df,is.numeric)
head(Num_data)
#summary of numeric
summary(Num_data$score)



#checking correlation
coMatrix <- cor(Num_data)
head(coMatrix)

melt_1 <- melt(coMatrix)

ggp <- ggplot(melt_1, aes(X1,X2))+geom_tile(aes(fill = value))+ 
  labs(x = 'Movies Features',y = 'Movies Features', title = 'Correlation Between Numeric  variables for Movies Features')
ggp

#extracting character columns
char_data <- select_if(sort_df, is.character)
head(char_data)


#Correlation between Gross Earnings and Character variable


boxplot(gross~ rating, data= sort_df, main='Gross Earnings vs. Rating', xlab='Rating', ylab='Gross') 
by(sort_df$gross, sort_df$rating, summary) 

boxplot(gross~star, data=sort_df, main='Gross Earnings vs. Star', xlab='Star', ylab='Gross')
by(sort_df$gross, sort_df$star, summary)

boxplot(gross~genre, data=sort_df, main='Gross Earnings vs. Genre', xlab='Genre', ylab='Gross')
by(sort_df$gross, sort_df$genre, summary)

boxplot(gross~country, data=sort_df, main='Gross Earnings vs. Country', xlab='Country', ylab='Gross')
by(sort_df$gross, sort_df$country, summary)


