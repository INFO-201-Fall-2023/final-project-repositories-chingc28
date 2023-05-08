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

# only selecting the rows that contain gdp values, row 1 and 4

# select lines less than 5 and not equal to column 2 and 3
gdp_df <- gdp_df[gdp_df$LineCode < 5 & !(gdp_df$LineCode %in% c(2,3)), ]

# joining both data, it is okay that there are duplicate columns of GDP 

df <- right_join(gdp_df, disaster_df, by = c('GeoName' = 'state'))

# adding a new categorical data: Whether a state is in a tornado alley
# there are many different interpretations of what states belong in the Tornado Alley, 
# but we will use the most common states included within the Tornado Alley. These states are
# Texas, Oklahoma, Kansas, Nebraska, South Dakota, Indiana, Missouri, Iowa, Illinois, Ohio
# Florida 
# source: https://judy.co/blogs/content/what-states-are-in-tornado-alley

tornado_alley_col <- c()
hurricane <- 
for (i in 1:nrow(df)){
  if (df$GeoName[i] %in% c('Texas', 'Oklahoma', 'Kansas', 'Nebraska', 'South Dakota', 'Indiana',
                          'Missouri', 'Iowa', 'Illinois', 'Ohio', 'Florida')){
    tornado_alley_col[i] <- 'Yes'}
    else{
      tornado_alley_col[i] <- 'No'
    }
  }

# adding the vector as a column in the data 

df$tornado_alley <- tornado_alley_col

# adding a numerical column: Change in GDP from 1997 to 2022 
# changing the x2022 column to nums because its in char, so we can do math 

df$X2022 <- as.numeric(df$X2022)

gdp_overtime <- c()
for (i in 1:nrow(df)){
  
  gdp_overtime[i] <- df$X2022[i] - df$X1997[i]
  
}
# having NA is okay because some rows aren't GDP so its not relevant 
# add vector to the dataframe

df$gdp_change <- gdp_overtime

# write to csv file 
write.csv(df, file = 'unified_dataframe.csv', row.names = FALSE)

# summary table 
# we want to create a summary table of each state and total weather events occured each year
# however, we only want the rows with Real GDP cause we don't want repeat data
# columns wanted: 'state', 'year', 'total storm', 'total compensation' gdp',
# 'change in gdp from prev year'
# second summary table: 'tornado alley states', 'year', 'tornado', 'change in tornado from prev yr'


