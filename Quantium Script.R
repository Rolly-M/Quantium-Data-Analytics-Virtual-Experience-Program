## Lets Start by preparing our working environment 

install.packages("tidyverse")
library(tidyverse)

install.packages("lubridate")
library(lubridate)

install.packages("data.table")
library(data.table)

install.packages("dplyr")
library(dplyr)

install.packages("padr")
library(padr)


## Setting the work directory and uploading our data sets


setwd(dir = "D:/Google Data Analytics/The Forage Projects/Quantium/Datasets")

library(readr)
library(readxl)
purchase_behaviour <- read_csv("QVI_purchase_behaviour.csv")
Transactions <- read_excel("QVI_transaction_data.xlsx")

## Viewing the data 

View(Transactions)
View(purchase_behaviour)


## Examining the data sets to view data structure and samples 

str(Transactions)
str(purchase_behaviour)

## Date column in Transactions DF is in num so lets change it to date format 

Transactions$DATE <- as.Date(Transactions$DATE, origin = "1899-12-30")

glimpse(Transactions$PROD_NAME)

## Cleaning the data set to remove Blanks or Unusefull columns 
summary(Transactions$PROD_NAME)


productWords <- data.table(unique(Transactions$PROD_NAME))   ##  Examine the words in PROD_NAME to see if there are any incorrect entries
setnames(productWords, 'words')

##productWords_new <- productWords %>% 
##  gsub('[0-9]+g', '',productWords$words) %>% 
  
  

## There are salsa products in the dataset but we are only interested in the chips category, so let's remove these.

Transactions <- Transactions %>% 
  dplyr::filter(!grepl("Salsa", Transactions$PROD_NAME))

## Summarise the data to check for nulls and possible outliers

summary(Transactions$STORE_NBR)
summary(Transactions$LYLTY_CARD_NBR)
summary(Transactions$TXN_ID)
summary(Transactions$PROD_NBR)
summary(Transactions$PROD_QTY)
summary(Transactions$TOT_SALES)

## Filter the dataset to find the outlier

Outlier <- filter(Transactions, PROD_QTY == "200")

filter(Transactions, LYLTY_CARD_NBR == 226000)

## Filter out the customer based on the loyalty card number

Transactions <- filter(Transactions, LYLTY_CARD_NBR != 226000)

## Re-examine transaction data

summary(Transactions$STORE_NBR)
summary(Transactions$LYLTY_CARD_NBR)
summary(Transactions$TXN_ID)
summary(Transactions$PROD_NBR)
summary(Transactions$PROD_QTY)
summary(Transactions$TOT_SALES)

## Count the number of transactions by date

Num_Trans_PerDay <- Transactions %>% 
  group_by(DATE) %>% 
  summarise(num_trans = n())

## Create a sequence of dates and join this to the count of transactions by date

Transactions <- pad(Transactions)

Num_Trans_PerDay2 <- Transactions %>% 
  group_by(DATE) %>% 
  summarise(num_trans = n())



## Setting plot themes to format graphs


theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))


## Plot transactions over time

ggplot(Num_Trans_PerDay2, aes(x = DATE, y = num_trans)) +
  geom_line() +
  labs(x = "Date", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

## Filter to December and look at individual days


ggplot(Num_Trans_PerDay2[Num_Trans_PerDay2$DATE > "2018-12-01" & Num_Trans_PerDay2$DATE < "2018-12-31", ]) +
  geom_line( aes(x = DATE, y = num_trans)) +
  labs(x = "Day", y = "Number of transactions", title = "December Transactions over time") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

## We can work this out by taking the digits that are in PROD_NAME

productWords_new <- as.numeric(productWords[1, ])

Pack_size <- regmatches(Transactions$PROD_NAME, gregexpr("[[:digit:]]+", Transactions$PROD_NAME))
Pack_size1 <- as.data.frame(do.call(rbind,Pack_size))
Pack_size1 <- Pack_size1 %>% 
  rename(Sizes = V1)
  
## Let's plot a histogram of PACK_SIZE since we know that it is a categorical variable and not a continuous variable even though it is numeric.

Pack_size2 <- as.data.frame(Pack_size1)

ggplot(data = Pack_size2) + geom_histogram(mapping = aes(x = Sizes,), stat = "count") + 
  labs(title = "Chips Pack Sizes", x = "Sizes", y = "Number of packs")

##Pack sizes created look reasonable. Now to create brands, we can use the first word in PROD_NAME to work out the brand name

Brand_Name <- as.data.frame(sub(" .*", "",  productWords$words))

## Clean brand names

Brand_Name <- unique(Brand_Name)



#### Examining customer data


head(purchase_behaviour, 15) #shows the first 10 rows
str(purchase_behaviour) # shows a summary of the dataframe
summary(purchase_behaviour) #statistical summary of the data



## Merge transaction data to customer data

data <- merge(Transactions, purchase_behaviour, all.x = TRUE)


## Checking for any Nulls in Data

which(is.na(data))

## Total sales by LIFESTAGE and PREMIUM_CUSTOMER

Total_Sales <- data %>% 
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>% 
  summarise(Sales = sum(TOT_SALES, na.rm = TRUE)) %>% 
  na.omit()


ggplot(data = Total_Sales) + geom_mosaic(mapping = (aes(x = LIFESTAGE, fill = PREMIUM_CUSTOMER))) +
  labs(title = " Total sales by LIFESTAGE", x = "LifeStage", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


## Number of customers by LIFESTAGE and PREMIUM_CUSTOMER


Number_Customers <- data %>% 
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>% 
  summarise(Customers = n()) %>% 
  na.omit()


ggplot(data = Number_Customers) + geom_col(aes(x = LIFESTAGE, y = Customers)) + facet_wrap(~PREMIUM_CUSTOMER) +
  labs(title = " Number of Customers by LIFESTAGE by Categorie", x = "LifeStage", y = "Number Of Customers") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))



## Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER



Average_Units <- data %>% 
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>% 
  summarise(Quantity = mean(PROD_QTY, na.rm = TRUE)) %>% 
  na.omit()

ggplot(data = Average_Units) + geom_col(aes(x = LIFESTAGE, y = Quantity)) + facet_wrap(~PREMIUM_CUSTOMER) +
  labs(title = " Average units per Customers by LIFESTAGE by Categorie", x = "LifeStage", y = "Average Units Bought") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))



## Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER


Average_Price <- data %>% 
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>% 
  summarise(Price = mean(PRO, na.rm = TRUE)) %>% 
  na.omit()

### Perform an independent t-test between mainstream vs premium and budget midage and young singles and couples

t.test(data$LIFESTAGE ~ PREMIUM_CUSTOMERS, data = data, var.equal = TRUE, paired = FALSE)
























