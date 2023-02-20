
#title: NYC Property Sales Project
#author: Kuntal Bhar
#As we know New York City real state is among the most expensive cities in the world.We uses NYC Property Sales Dataset provided by the New York City Department of Finance to do our analysis.
  
#Packages used
knitr::opts_chunk$set(echo = FALSE)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(modelr)) install.packages("modelr", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")


#NOTE:
#In case there is a problem in downloading the csv file from code 
#Step1: Downloaded the file nyc-rolling-sales.csv dataset was manual from Kaggle www.kaggle.com/new-york-city/nyc-property-sales OR
#OR download file from https://github.com/kuntalbhar/edx-nyc-project where I have uploaded the csv Kaggle site along R,RMD and Pdf
#Step2: Save it in save the file nyc-rolling-sales.csv in same same folder were we are R and RMD File

dl <- "nyc-rolling-sales.csv"
if(!file.exists(dl))
  download.file("https://raw.githubusercontent.com/kuntalbhar/edx-nyc-project/main/nyc-rolling-sales.csv", dl)


#reading from the csv. Used fread of data.table for faster reading of the csv file 
nyc_orig_data <- as_data_frame(fread("nyc-rolling-sales.csv"))
class(nyc_orig_data)

#dimension of the read file
dim(nyc_orig_data)

#show column name present in the file
names(nyc_orig_data)


#Removed the first column 'V1' 
nyc_curr_data<- as_data_frame(nyc_orig_data[,-1])

#Separate the columns in 'BUILDING CLASS CATEGORY'
nyc_curr_data <- nyc_curr_data %>%
  separate(col = "BUILDING CLASS CATEGORY", into = c("BUILDING CLASS CATEGORY NUMBER",
                                                     "BUILDING CLASS CATEGORY"), sep = 3) %>%
  separate(col = "SALE DATE", into = c("SALE DATE", "TIME"), sep = " ")

#Removed columns - 'EASE-MENT', 'TIME' (column #8,23)
nyc_curr_data <- nyc_curr_data[,c(-8,-23)]

#adding column BUILDING AGE
nyc_curr_data <- nyc_curr_data %>% 
  mutate(`BUILDING AGE` = 2017 - `YEAR BUILT`)

#display column 
names(nyc_curr_data)

#Removing the Duplicates in the data frame, nyc_property 
nyc_curr_data %>% filter(duplicated(nyc_curr_data) == TRUE) %>% nrow()

#Removing the Duplicates in the data frame, nyc_property 
nyc_curr_data <- unique(nyc_curr_data)
dim(nyc_curr_data)

#Dataset contains 83783 observations and 22 variables.


### Column Type Conversion 

#Structure of current data
str(nyc_curr_data)

#Data Type conversions to the Data set

#factor conversion
colPos <- c(1,3,4,5,8,11,18,19)
nyc_curr_data %<>% mutate_at(colPos, funs(factor(.)))

#change id's to name of BOROUGH column
levels(nyc_curr_data$BOROUGH) <- c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")

#numeric conversion
num <- c(15,16,17,20)
nyc_curr_data %<>% mutate_at(num, funs(as.numeric(.)))

#char conversion
chr <- c(6,7)
nyc_curr_data %<>% mutate_at(chr, funs(as.character(.)))

#date conversion
nyc_curr_data$`SALE DATE` <- ymd(nyc_curr_data$`SALE DATE`)

#display modified structure
str(nyc_curr_data)

### Zeros And Missing Values  

#number observations column  introduce NA's on data types conversation
sum(is.na(nyc_curr_data))
#List of  introduce NA's on data types conversation
colSums(is.na(nyc_curr_data))

#compare it with SALE PRICE original data on - and 0's
sum(nyc_orig_data$`SALE PRICE` == "-") 
sum(is.na(nyc_curr_data$`SALE PRICE`)) 

sum(nyc_orig_data$`SALE PRICE` == 0) 
nyc_curr_data %>% filter(`SALE PRICE` == 0) %>% nrow() 


#### Lets look at columns SALE PRICE,LAND SQUARE FEET and GROSS SQUARE FEET

# Missing & Zero values in 'Sale Price' - 28.86% of the original dataset
nyc_defect_data <- nyc_curr_data %>% filter(nyc_curr_data$`SALE PRICE` == 0 | is.na(nyc_curr_data$`SALE PRICE`))
nyc_defect_data  %>% nrow() 

# Base Dataset - (Sale Price = 0 or NULL) - 71.13% of original dataset. nyc_clean_data dataset will be usesfor doing the analysis requiring Sale price values.
nyc_clean_data <- nyc_curr_data %>% filter((!nyc_curr_data$`SALE PRICE` == 0) & !is.na(nyc_curr_data$`SALE PRICE`))
nyc_clean_data %>% nrow() 

#get the count of zerow's observation on column LAND SQUARE FEET and  GROSS SQUARE FEET
nyc_clean_data %>% filter(nyc_clean_data$`LAND SQUARE FEET` == 0 | nyc_clean_data$`GROSS SQUARE FEET` == 0) %>% nrow()

#get the summary of BUILDING CLASS CATEGORY with zero's observation on LAND SQUARE FEET and GROSS SQUARE FEET
sqf_data <- nyc_clean_data %>% filter(nyc_clean_data$`LAND SQUARE FEET` == 0 | nyc_clean_data$`GROSS SQUARE FEET` == 0)
summary(sqf_data$`BUILDING CLASS CATEGORY`)

#get the count of zero and NA observations on column LAND SQUARE FEET 
nyc_clean_data %>% filter(nyc_clean_data$`LAND SQUARE FEET` == 0 | is.na(nyc_clean_data$`LAND SQUARE FEET`)) %>% nrow()

#get the count of zero and NA observations on column GROSS SQUARE FEET 
nyc_clean_data %>% filter(nyc_clean_data$`GROSS SQUARE FEET` == 0 | is.na(nyc_clean_data$`GROSS SQUARE FEET`)) %>% nrow()

nyc_data <- nyc_clean_data 

# converting zero to NA for column LAND SQUARE FEET
nyc_data$`LAND SQUARE FEET`[nyc_data$`LAND SQUARE FEET` == 0] <- NA

# converting zero to NA for column LAND SQUARE FEET
nyc_data$`GROSS SQUARE FEET`[nyc_data$`GROSS SQUARE FEET` == 0] <- NA

#### Lets look at columns RESIDENTIAL UNITS, COMMERCIAL UNITS and TOTAL UNITS


#get the count of zero observations on column GRESIDENTIAL UNITS 
nyc_clean_data %>% filter(nyc_clean_data$`RESIDENTIAL UNITS` == 0) %>% nrow()

#get the count of zero observations on column COMMERCIAL UNITS 
nyc_clean_data %>% filter(nyc_clean_data$`COMMERCIAL UNITS` == 0) %>% nrow()

#get the count of zero observations on column GRESIDENTIAL UNITS 
nyc_clean_data %>% filter(nyc_clean_data$`TOTAL UNITS` == 0) %>% nrow()

nyc_clean_data %>% filter(nyc_clean_data$`TOTAL UNITS` != nyc_clean_data$`RESIDENTIAL UNITS` +
                              nyc_clean_data$`COMMERCIAL UNITS`) %>% nrow()

#get the count of zero observations on column GRESIDENTIAL UNITS 
nyc_clean_data %>% filter(nyc_clean_data$`YEAR BUILT` == 0) %>% nrow()

#get the count of zero observations on column COMMERCIAL UNITS 
nyc_clean_data %>% filter(nyc_clean_data$`COMMERCIAL UNITS` == 0) %>% nrow()

#get the count of zero observations on column GRESIDENTIAL UNITS 
nyc_clean_data %>% filter(nyc_clean_data$`TOTAL UNITS` == 0) %>% nrow()

#print levels and missing or zero observations for category columns
Column<-c("BOROUGH",
          "TAX CLASS AT PRESENT",
          "BUILDING CLASS CATEGORY NUMBER",
          "BUILDING CLASS AT PRESENT", 
          "BUILDING CLASS AT TIME OF SALE",
          "ZIP CODE",
          "YEAR BUILT"  )
Level<-c(nlevels(nyc_data$`BOROUGH`),
          nlevels(nyc_data$`TAX CLASS AT PRESENT`),
          nlevels(nyc_data$`BUILDING CLASS CATEGORY NUMBER`),
          nlevels(nyc_data$`BUILDING CLASS AT PRESENT`),
          nlevels(nyc_data$`BUILDING CLASS AT TIME OF SALE`),
          nlevels(nyc_data$`ZIP CODE`),
          n_distinct(nyc_data$`YEAR BUILT`)
        )


MissingOrZero<-c(sum(nyc_data$`BOROUGH`==""),
          sum(nyc_data$`TAX CLASS AT PRESENT`==""),
          sum(nyc_data$`BUILDING CLASS CATEGORY NUMBER`==""),
          sum(nyc_data$`BUILDING CLASS AT PRESENT`==""),
          sum(nyc_data$`BUILDING CLASS AT TIME OF SALE`==""),
          sum(nyc_data$`ZIP CODE`==0),
          sum(nyc_data$`YEAR BUILT`==0)
        )

df <- data.frame(Column, Level, MissingOrZero)
df %>% knitr::kable()

#Print the levels
"TAX CLASS AT PRESENT"
levels(nyc_data$`TAX CLASS AT PRESENT`)

"BUILDING CLASS CATEGORY NUMBER"
levels(nyc_data$`BUILDING CLASS CATEGORY NUMBER`)

"BUILDING CLASS AT PRESENT"
levels(nyc_data$`BUILDING CLASS AT PRESENT`)

print("BUILDING CLASS AT TIME OF SALE")
levels(nyc_data$`BUILDING CLASS AT TIME OF SALE`)

print("ZIP CODE")
levels(nyc_data$`ZIP CODE`)

print("YEAR BUILT")
unique(nyc_data$`YEAR BUILT`)


## Data Details

variable.type <- lapply(nyc_curr_data, class)
variable.description <- c("The name of the borough in which the property is located", 
                          "Neighbourhood name", "Building class category code to identify similar properties", 
                          "Building class category title to identify similar properties", 
                          "Assigned tax class of the property in the city - Classes 1, 2, 3 or 4", 
                          "Sub-division of the borough on which real properties are located", 
                          "Sub-division of a Tax Block, used to uniquely represent the property location",
                          "Used to describe a property's constructive use", "Property's street address", 
                          "Property's apartment number", "Property's postal code", 
                          "Number of residential units at the listed property",
                          "Number of commercial units at the listed property",
                          "Total number of units at the listed property", 
                          "Land area of the property listed in square feet", 
                          "Total area of all the floors of a building", "Property's construction year", 
                          "Assigned tax class of the property in the city at the time of sale",
                          "Used to describe a property's constructive use at the time of sale",
                          "Price paid for the property", "Date of property sale", "Age of the Building")

variable.name <- colnames(nyc_data)

nyc_datadesc <- as_data_frame(cbind(variable.name, variable.type, variable.description))
colnames(nyc_datadesc) <- c("Column Name","Data Type","Column Description")
kable(nyc_datadesc)


# ANALYSIS/METHODS

## Descriptive Analysis


#Lets check the clean current dataset that we have created.

dim(nyc_data)


#### SALE PRICE 

#apply the quantile function to get the distribution
quantile(nyc_data$`SALE PRICE`, probs = seq(from = 0, to = 1, by = .1))

nyc_data %>% filter(`SALE PRICE` <= 1000) %>% nrow()

#Adding these rows to deffect datse
nyc_data <-  nyc_curr_data %>% filter(`SALE PRICE` <= 1000) %>% 
  bind_rows(nyc_data)

#Update current dataset
nyc_data <- nyc_data %>% filter(!`SALE PRICE` <= 1000)

#apply quantile to check this data distribution
quantile(nyc_data$`SALE PRICE`, probs = seq(from = 0, to = 1, by = .1))

#### LAND SQUARE FEET

# get the summary
summary(nyc_data$`LAND SQUARE FEET`)
#apply quantile to check this data distribution
quantile(nyc_data$`LAND SQUARE FEET`, probs = seq(from = 0, to = 1, by = .1), na.rm = TRUE)

#### GROSS SQUARE FEET

# get the summary
summary(nyc_data$`GROSS SQUARE FEET`)
#apply quantile to check this data distribution
quantile(nyc_data$`GROSS SQUARE FEET`, probs = seq(from = 0, to = 1, by = .1), na.rm = TRUE)

#### RESIDENTIAL UNITS , COMMERCIAL UNITS and TOTAL UNITS


# get the summary RESIDENTIAL UNITS
summary(nyc_data$`RESIDENTIAL UNITS`)
#lets check for 5 units
nyc_data %>% filter(`RESIDENTIAL UNITS` > 5) %>% nrow()

# get the summary RESIDENTIAL UNITS
summary(nyc_data$`COMMERCIAL UNITS`)
#lets check for 5 units
nyc_data %>% filter(`COMMERCIAL UNITS` > 5) %>% nrow()

#lets check for 5 units
nyc_data %>% filter(`TOTAL UNITS` > 5) %>% nrow()

#### BUILDING AGE

# get the summary BUILDING AGE
summary(nyc_data$`BUILDING AGE`)
# get the summary YEAR BUILT
summary(nyc_data$`YEAR BUILT`)

#number of BUILDING AGE properties over the mean age
nyc_data %>% filter(`BUILDING AGE` > 205) %>% nrow()

#number of BUILDING AGEproperties over the mean age and YEAR BUILT is not NA and Zero
nyc_data %>% filter(`BUILDING AGE` > 205 & `YEAR BUILT` != 0) %>% 
  arrange(desc(`BUILDING AGE`)) %>% nrow()



## Visualization Analysis

#### BOROUG


ggplot(data = nyc_curr_data, aes(x = `BOROUGH`)) +
  geom_bar() +
  ggtitle("Most In-Demand Borough in NYC", subtitle = "Borough-wise # of property sales in NYC") +
  scale_y_continuous("# of Property Sales", labels = scales::comma) +
  scale_x_discrete("Borough")



ggplot(data = nyc_data, aes(x = `BOROUGH`, y = mean(`SALE PRICE`) )) +
  geom_bar(stat = "identity") +
  ggtitle("Most Expensive Borough in NYC", subtitle = "Borough-wise Avg Property Sale Price in NYC") +
  scale_y_continuous("Avg Sale Price", labels = scales::dollar) +
  scale_x_discrete("Borough")

#### NEIGHBORHOOD


#create data frame based on descending order
df1 <- as.data.frame(table(nyc_curr_data$BOROUGH, nyc_curr_data$NEIGHBORHOOD))
names(df1) <- c('BOROUGH','NEIGHBORHOOD', 'Freq')
df1 <- df1 %>% arrange(desc(Freq)) %>% head(15)

#plot Top Neighborhood by sales price
ggplot(df1, aes(x = `NEIGHBORHOOD`, y = `Freq`, fill = `BOROUGH`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Most-in demand Neighborhood in NYC", subtitle = "Top Neighborhoods by Number") +
  theme(legend.position = "bottom") +
  scale_y_continuous("# of Sales", labels = scales::comma) +
  scale_x_discrete("Neighborhood") 

##filter data based on descending order
df2 <- 
  nyc_data %>% group_by(BOROUGH, NEIGHBORHOOD) %>% 
  summarise(MeanSP = mean(`SALE PRICE`)) %>% 
  arrange(desc(MeanSP)) %>% head(15)

#plot Most Expensive Neighborhoods by Avg Price
ggplot(data = df2, aes(x = `NEIGHBORHOOD`, y = MeanSP, fill = `BOROUGH`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "bottom") +
  ggtitle("Most Expensive Neighborhoods in NYC", 
          subtitle = "Top Neighborhoods by Avg Price") +
  scale_y_continuous("Avg Sale Price", labels = scales::dollar) +
  scale_x_discrete("Neighborhood") 

##filter data 
df2 <- 
  nyc_data %>% group_by(BOROUGH, NEIGHBORHOOD) %>% 
  summarise(MeanSP = mean(`SALE PRICE`)) %>% 
  arrange(MeanSP) %>% head(15)

#plot Top Neighborhoods by the lowest avg. Price
ggplot(data = df2, aes(x = `NEIGHBORHOOD`, y = MeanSP, fill = `BOROUGH`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "bottom") +
  ggtitle("Least Expensive Neighborhoods in NYC", subtitle = "Top Neighborhoods by the lowest avg. Price") +
  scale_y_continuous("Avg Sale Price", labels = scales::dollar) +
  scale_x_discrete("Neighborhood") 


#### BUILDING CLASS CATEGORY

##filter data 
df1 <- as.data.frame(table(nyc_curr_data$BOROUGH, nyc_curr_data$`BUILDING CLASS CATEGORY`))
names(df1) <- c('BOROUGH','BUILDING CLASS CATEGORY', 'Freq')
df1 <- df1 %>% group_by(BOROUGH) %>% arrange(desc(Freq)) %>% head(10)

##plot Most In-Demand Building Type class by Borough
ggplot(df1, aes(x = `BOROUGH`, y = `Freq`, fill = `BUILDING CLASS CATEGORY`)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Most In-Demand Building Type's in NYC by Borough", subtitle = "Top Building Type's sold in NYC") +
  scale_y_continuous("# of Sales", labels = scales::comma) +
  scale_x_discrete("Borough") 

##filter data 
df2 <- 
  nyc_data %>% group_by(BOROUGH, `BUILDING CLASS CATEGORY`) %>% 
  summarise(MeanSP = mean(`SALE PRICE`)) %>% 
  arrange(desc(MeanSP)) %>% head(10)

#plot Most Expensive Building Types by Borough
ggplot(data = df2, aes(x = `BUILDING CLASS CATEGORY`, y = MeanSP, fill = `BOROUGH`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "bottom") +
  ggtitle("Most Expensive Building Types in NYC", subtitle = "Top Property Type's by Value in NYC") +
  scale_y_continuous("Avg Sale Price", labels = scales::dollar) +
  scale_x_discrete("Building Type") 


##filter data 
df2 <- 
  nyc_data %>% group_by(BOROUGH, `BUILDING CLASS CATEGORY`) %>% 
  summarise(MeanSP = mean(`SALE PRICE`)) %>% 
  arrange(MeanSP) %>% head(10)

#plot Least Expensive Building Types by Borough
ggplot(data = df2, aes(x = `BUILDING CLASS CATEGORY`, y = MeanSP, fill = `BOROUGH`)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme(legend.position = "bottom") +
  ggtitle("Least Expensive Buildings in NYC", subtitle = "Lowest Types of Property by Value in NYC in 2016") +
  scale_y_continuous("Avg Sale Price", labels = scales::dollar) +
  scale_x_discrete("Building Type") 

#### BOROUGH BY TAX CLASS

#plot nuber of Property Tax Class wise property by Borough
ggplot(data = nyc_curr_data, aes(x = `TAX CLASS AT TIME OF SALE`)) +
  geom_bar() +
  facet_wrap(~ BOROUGH) + 
  ggtitle("Borough-wise Sold Property Tax Class") +
  scale_y_continuous("# of Property Sold", labels = scales::comma) +
  scale_x_discrete("Tax Class")

#plot value of Property Tax Class wise property by Borough
ggplot(data = nyc_data, aes(x = `TAX CLASS AT TIME OF SALE`, y = mean(`SALE PRICE`))) +
  geom_bar(stat = "identity") +
  facet_wrap(~ BOROUGH) + 
  ggtitle("Borough-wise Sold Property Tax Class") +
  scale_y_continuous("Avg Property Value", labels = scales::dollar) +
  scale_x_discrete("Tax Class")

####  PROPERTY BY SQUARE FEET (LAND and GROSS)

#plot Land Square Feet across Boroughs
ggplot(data = nyc_data, aes(x = `BOROUGH`, y = log(`LAND SQUARE FEET`), fill = nyc_data$BOROUGH)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  ggtitle("Land Square Feet across Boroughs", subtitle = "Borough-wise Distribution of Avg Land Square Feet") +
  scale_y_continuous("Avg. Land Square Footage", labels = scales::comma) +
  scale_x_discrete("Borough") +
  coord_flip()

#plot Gross Square Feet across Boroughs
ggplot(data = nyc_data, aes(x = `BOROUGH`, y = log(`GROSS SQUARE FEET`), fill = nyc_data$BOROUGH)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  ggtitle("Gross Square Feet across Boroughs", subtitle = "Borough-wise Distribution of Avg Gross Square Feet") +
  scale_y_continuous("Abg. Gross Square Footage", labels = scales::comma) +
  scale_x_discrete("Borough") +
  coord_flip()

#plot pattern Land Square Feet vs sales price by Boroughs
ggplot(data = nyc_data, aes(x = log(`LAND SQUARE FEET`), y = log(`SALE PRICE`), color = `BOROUGH`)) +
  geom_jitter() +
  geom_smooth(method = "lm", colour="black", size=0.5, linetype = "dashed") +
  theme(legend.position = "bottom") +
  facet_wrap(~ BOROUGH) +
  ggtitle("Price Vs Land Square Footage in NYC", 
          subtitle = "Distribution of Sale Price vs Land Square feet Borough-wise") +
  scale_y_continuous("Property Sale Price", labels = scales::dollar) +
  scale_x_continuous("Land Square Footage", labels = scales::comma) 

#plot pattern Gross Square Feet vs sales price by Boroughs
ggplot(data = nyc_data, aes(x = log(`GROSS SQUARE FEET`), y = log(`SALE PRICE`), color = `BOROUGH`)) +
  geom_jitter() +
  geom_smooth(method = "lm", colour="black", size=0.5, linetype = "dashed") +
  theme(legend.position = "bottom") +
  facet_wrap(~ BOROUGH) +
  ggtitle("Price Vs Land Square Footage in NYC", 
          subtitle = "Distribution of Sale Price vs Land Square feet Borough-wise") +
  scale_y_continuous("Property Sale Price", labels = scales::dollar) +
  scale_x_continuous("Land Square Footage", labels = scales::comma) 

#filter data

df1 <- nyc_data %>% filter(`LAND SQUARE FEET` != 0) %>%
      mutate(PriceLSF = `SALE PRICE`/`LAND SQUARE FEET`) %>%
      group_by(`BOROUGH`, `NEIGHBORHOOD`) %>%
      summarise(MeanPriceLSF = mean(PriceLSF, na.rm = TRUE)) %>%
      arrange(desc(MeanPriceLSF)) %>% head(15)

ggplot(data = df1, aes(x = `NEIGHBORHOOD`, y = MeanPriceLSF, fill = `BOROUGH`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "bottom") +
  ggtitle("NYC Price per Square Feet", subtitle = "Top Price/sqft Neighborhood") +
  scale_y_continuous("Price/sqft", labels = scales::dollar) +
  scale_x_discrete("Neighborhood") 


#filter data

df2 <- nyc_data %>% filter(`LAND SQUARE FEET` != 0) %>%
  mutate(PriceLSF = `SALE PRICE`/`LAND SQUARE FEET`) %>%
  group_by(`BOROUGH`, `NEIGHBORHOOD`) %>%
  summarise(MeanPriceLSF = mean(PriceLSF, na.rm = TRUE)) %>%
  arrange(MeanPriceLSF) %>% head(15)

ggplot(data = df2, aes(x = `NEIGHBORHOOD`, y = MeanPriceLSF, fill = `BOROUGH`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "bottom") +
  ggtitle("MYC Price per Square Feet", subtitle = "Lowest Price/sqft Neighborhood") +
  scale_y_continuous("Price/ sqft", labels = scales::dollar) +
  scale_x_discrete("Neighborhood") 


#### BUILDING AGE

#filter data
df3 <- 
  nyc_data %>%filter(`BUILDING AGE` <=300)  

#plot building age across borough
ggplot(data = df3, aes(x = `BOROUGH`, y = `BUILDING AGE`, fill = `BOROUGH`)) +
  geom_boxplot() +
  coord_flip() +
  ggtitle("Age of Properties sold in NYC", subtitle = "Distribution of building age in NYC") +
  scale_y_continuous("Building Age", labels = scales::comma) +
  scale_x_discrete("Borough") 


#plot sale price of borough across building age
ggplot(data = df3, aes(x = `BUILDING AGE`, y = log(`SALE PRICE`))) +
  geom_point(aes(col = df3$BOROUGH)) +
  geom_smooth(method = "lm") +
  theme(legend.position = "bottom") +
  ggtitle("Price of Oldest Properties sold in NYC", subtitle = "Oldest buildings in NYC in 2016") +
  scale_y_continuous("Property Value Distribution", labels = scales::dollar) +
  scale_x_continuous("Building Age") 



## Prediction Analysis

library(caret)
# creating the new dataset

#transforming sale date into months 
nyc_data$`SALE MONTH` <- as.factor(months(nyc_data$`SALE DATE`))

#removing all unwanted columns that are not needed for prediction 
nyc_predtion <- nyc_data[, -c(3, 5, 6, 7, 8, 9, 10, 17, 19, 21)]
nyc_predtion$NEIGHBORHOOD <- as.factor(nyc_predtion$NEIGHBORHOOD)

nyc_predtion <- nyc_predtion[c(1:10, 12,13,11)]

str(nyc_predtion)

# split the data into training nyc_pred_train and test set nyc_pred_test in 80-20% ratio
set.seed(101)
#index <- sample(nrow(nyc_predtion),nrow(nyc_predtion)*0.80)

index = createDataPartition(nyc_predtion$`SALE PRICE`, p = 0.80, list = FALSE) 
nyc_pred_train <- nyc_predtion[index,]
nyc_pred_test <- nyc_predtion[-index,]


### Method 1: Correlation

#Corealtion Table 1
library(corrplot)
#compute correlation  
distance_corr <- vapply(nyc_pred_train[c(1:12)], 
                        function(x) { cor(nyc_pred_train$`SALE PRICE`, as.numeric(x), use = "pairwise.complete.obs") }, 
                        FUN.VALUE = numeric(1))
effect_corr <- vapply(distance_corr, function(x) { ifelse(x >= 0, "Positive", "Negative")}, 
                      FUN.VALUE = character(1))
#get the name
var_corr <- names(nyc_pred_train[c(1:12)])

# create correlation table
table1 <- data.frame(var_corr, abs(distance_corr), effect_corr)
table1 <- table1[order(-abs(distance_corr)),]
names(table1) <- c("Column/Variable", "Correlation Size", "Correlation Effect")
kable(table1)

#Corealtion Table 2

#applying the TOTAL UNITS instead of RESIDENTIAL UNITS and COMMERCIAL UNITS. And LAND SQUARE FEET instead of LAND SQUARE FEET and GROSS SQUARE FEET
temp <- nyc_predtion[,c(1:4,10,12)]    
num <- c(1:6)
temp %<>% mutate_at(num, funs(as.numeric(.)))
#plot table of correlations
corrplot(cor(temp), type = "upper",order = "hclust", 
         tl.col = "black", tl.srt = 45)

#plot table of correlations
round(cor(temp), 3)


#Corealtion Table 3

#Correlate numeric predictors
correl <- cor(nyc_predtion[sapply(nyc_predtion, is.numeric)], use = "pairwise.complete.obs")
#plot the corelations chart
corrplot(correl, type = "upper",order = "AOE", 
         tl.col = "black", tl.srt = 45)

#plot table of correlations
round(correl, 3)

### Method 2: Single Linear Regressions

#apply single linear regressions SALE PRICE price with BOROUGH
#library(semEff)
slr_model <- lm(nyc_pred_train$`SALE PRICE` ~ nyc_pred_train$BOROUGH)

reg <- summary(slr_model) 

#F-statistic p-Value
print("p-value")
pf(reg$fstatistic[1],reg$fstatistic[2],reg$fstatistic[3],lower.tail = FALSE)
reg
#Adjusted R Squared 
print("Adjusted r squared")
reg$r.squared

#apply single linear regressions SALE PRICE price with NEIGHBORHOOD

slr_model <- lm(nyc_pred_train$`SALE PRICE` ~ nyc_pred_train$NEIGHBORHOOD)
#print summary
reg <- summary(slr_model) 

#F-statistic p-Value
print("p-value")
pf(reg$fstatistic[1],reg$fstatistic[2],reg$fstatistic[3],lower.tail = FALSE)

#Adjusted R Squared 
print("Adjusted r squared")
reg$r.squared

nyc_pred_train$N_BLOOMFIELD = ifelse(nyc_pred_train$NEIGHBORHOOD == "BLOOMFIELD", 1,0)
nyc_pred_train$N_FASHION = ifelse(nyc_pred_train$NEIGHBORHOOD == "FASHION", 1,0)
nyc_pred_train$`N_JAVITS CENTER` = ifelse(nyc_pred_train$NEIGHBORHOOD == "JAVITS CENTER", 1,0)
nyc_pred_train$`N_MIDTOWN CBD` = ifelse(nyc_pred_train$NEIGHBORHOOD == "MIDTOWN CBD", 1,0)
nyc_pred_train$`N_OTHERS` = ifelse((nyc_pred_train$NEIGHBORHOOD != "MIDTOWN CBD") &
                                                       (nyc_pred_train$NEIGHBORHOOD != "JAVITS CENTER") &
                                                       (nyc_pred_train$NEIGHBORHOOD != "FASHION") &
                                                       (nyc_pred_train$NEIGHBORHOOD != "BLOOMFIELD"), 1,0)

# Removing the original Neighborhood predictor
nyc_pred_train <- nyc_pred_train[,-2]

#apply single linear regressions SALE PRICE price with BUILDING CLASS CATEGORY and p-value
slr_model <- lm(nyc_pred_train$`SALE PRICE` ~ nyc_pred_train$`BUILDING CLASS CATEGORY`)
#print summary
reg <- summary(slr_model) 

#F-statistic p-Value
print("p-value")
pf(reg$fstatistic[1],reg$fstatistic[2],reg$fstatistic[3],lower.tail = FALSE)

#Adjusted R Squared 
print("Adjusted r squared")
reg$r.squared

#apply single linear regressions SALE PRICE price withTAX CLASS AT TIME OF SALE and p-value
slr_model <- lm(nyc_pred_train$`SALE PRICE` ~ nyc_pred_train$`TAX CLASS AT TIME OF SALE`)
#print summary
reg <- summary(slr_model) 
reg
#F-statistic p-Value
print("p-value")
pf(reg$fstatistic[1],reg$fstatistic[2],reg$fstatistic[3],lower.tail = FALSE)

#Adjusted R Squared 
print("Adjusted r squared")
reg$r.squared
 
#apply single linear regressions SALE PRICE price with SALE MONTH and p-value
slr_model <- lm(nyc_pred_train$`SALE PRICE` ~ nyc_pred_train$`SALE MONTH`)
#print summary
reg <- summary(slr_model) 

#F-statistic p-Value
print("p-value")
pf(reg$fstatistic[1],reg$fstatistic[2],reg$fstatistic[3],lower.tail = FALSE)

#Adjusted R Squared 
print("Adjusted r squared")
reg$r.squared

### Method 3: Multi Linear Regressions

#We will removd the SALE MONTH as this variable is not significant
attach(nyc_pred_train)
full_mlr_model <- lm(`SALE PRICE` ~ . -`SALE MONTH` , data = nyc_pred_train)
reg <- summary(full_mlr_model)
gl<- glance(full_mlr_model) 
gl

print("MSE")
mean(reg$residuals^2)

attach(nyc_pred_train)
full_mlr_model <- lm(`SALE PRICE` ~ . -`SALE MONTH` -`ZIP CODE`  , data = nyc_pred_train)
reg <- summary(full_mlr_model)

  
gl<- glance(full_mlr_model) 
gl

print("MSE")
mean(reg$residuals^2)



