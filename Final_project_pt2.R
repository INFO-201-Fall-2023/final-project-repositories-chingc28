# Data Wrangling Final Project!

# Name: Ching Chiu & Minaswini Deshmukh

### Library imports 

library(dplyr)
library(stringr)
### data is from here 

# disaster: https://www.kaggle.com/datasets/headsortails/us-natural-disaster-declarations
# gdp: https://apps.bea.gov/regional/downloadzip.cfm
# cost of disaster: https://www.kaggle.com/datasets/christinezinkand/us-billiondollar-weather-and-climate-disasters

### The code goes here

# reading csv files 
gdp_df <- read.csv('SAGDP1__ALL_AREAS_1997_2022.csv')
disaster_df <- read.csv('us_disaster_declarations.csv')

### cleaning up files before merging 

## disaster 
# removing unwanted columns
columns_dis <- names(disaster_df)
unwanted <- c(columns_dis[9:23])
disaster_df <- select(disaster_df, -unwanted)

# selecting incidents from 1997 and beyond and removing duplicates 
disaster_df <- disaster_df[disaster_df$fy_declared >= 1997, ]
disaster_df <- disaster_df[!duplicated(disaster_df$fema_declaration_string), ]
disaster_df <- disaster_df[disaster_df$state != 'PR', ]

# changing state abbreviation to state names 

dis_names <- disaster_df$state
dis_index <- match(dis_names, state.abb)
letter_to_name <- state.name[dis_index]
disaster_df$state <- letter_to_name

## gdp
# removing unwanted columns

columns_gdp <- names(gdp_df)
unwanted_gdp <- c(columns_gdp[3:4])
gdp_df <- select(gdp_df, -unwanted_gdp)

# only selecting the rows that contain gdp values 

gdp_df <- gdp_df[gdp_df$LineCode < 5, ]
gdp_df <- gdp_df[gdp_df$LineCode != 2, ]
gdp_df <- select(gdp_df, -names(gdp_df)[3:4])

# joining both data, it is okay that there are duplicate columns of GDP 

df <- right_join(gdp_df, disaster_df, by = c('GeoName' = 'state'))
