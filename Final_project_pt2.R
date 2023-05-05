# Data Wrangling Final Project!

# Name: Ching Chiu & Minaswini Deshmukh

### Library imports 

library(dplyr)
library(stringr)

df_1 <- read.csv()
df_2 <- read.csv()

### The code goes here

df_1 <- read.csv('Red.csv')
df_2 <- read.csv('Rose.csv')
df_3 <- read.csv('Sparkling.csv')
df_4 <- read.csv('White.csv')

types_df <- rbind(df_1, df_2, df_3, df_4)

ratings_df <- read.csv('winemag-data_first150k.csv')

### The code goes here 

# changing US to 'United States' to merge based on country

ratings_df$country <- gsub('US', 'United States', ratings_df$country)

# merging based on country, region, and winery 

# df <- merge(ratings_df, types_df, by.x = 'country', by.y = 'Country')
