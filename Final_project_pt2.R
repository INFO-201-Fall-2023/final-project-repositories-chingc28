# Data Wrangling Final Project!

# Name: Ching Chiu & Minaswini Deshmukh

### Library imports 

library(dplyr)
library(stringr)
library(tidyr)
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

########## write to csv file 
write.csv(df, file = 'unified_dataframe.csv', row.names = FALSE)

########## summary table 
# we want to create a summary table of each state and total weather events occured each year

# summary columns: 'state', 'category: gdp or total storm' 'total hurricane', 'total severe storm' '1997', '1998'... etc 
# we don't want biological values, covid isn't a weather event.

# filtering 
# we don't want covid19 incidents and only want gdp data rows
weather_df <- filter(df, incident_type != 'Biological', LineCode == 1)

# we want to groupby the state
grouped_df <- group_by(weather_df, GeoName, fy_declared)
sum_df <- summarize(grouped_df, sum_storm = n())

# reshaping the dataframe 
sum_df <- pivot_wider(sum_df, names_from = fy_declared, values_from = sum_storm)

#add categories column 
storm <- rep("storm", times = 50)
sum_df$category <- storm 

# sort the columns and put na values as 0 
sum_df <- sum_df[, sort(colnames(sum_df))]
sum_df <- sum_df[, c(ncol(sum_df), 1:(ncol(sum_df)-1))]
sum_df[is.na(sum_df)] <- 0 

# making a df for gdp, names(sum_df)[2:31] selects the relevant columns
sum_gdp <- distinct(df, GeoName, .keep_all = TRUE)
sum_gdp <- sum_gdp[, c(2, 7:32)]

# renaming the column to merge
years <- 1997:2022
for (i in 2:27){
  names(sum_gdp)[i] <- years[i - 1]
}

sum_df <- rbind(sum_df, sum_gdp)
gdp_category <- rep('gdp', times = 50)
sum_df[51:100, 'category'] <- gdp_category

# im gonna deal with the NA values for the 2nd half of this summary table later 
