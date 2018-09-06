
#DESCRIPTION OF DATA

#InvoiceNo: Invoice number. Nominal, a 6-digit integral number uniquely assigned to each transaction. If this code starts with letter 'c', it indicates a cancellation. 
#StockCode: Product (item) code. Nominal, a 5-digit integral number uniquely assigned to each distinct product. 
#Description: Product (item) name. Nominal. 
#Quantity: The quantities of each product (item) per transaction. Numeric.	
#InvoiceDate: Invice Date and time. Numeric, the day and time when each transaction was generated. 
#UnitPrice: Unit price. Numeric, Product price per unit in sterling. 
#CustomerID: Customer number. Nominal, a 5-digit integral number uniquely assigned to each customer. 
#Country: Country name. Nominal, the name of the country where each customer resides.

#EXTRA
#Date: Date in YYYY-mm-dd format
#Time: Time in HH:MM
#TotalSpent : A column of all the totalspent per row, qty * unit price = total spent for that row.
##########################################################################################################

#Reading the data
library(stringr)
library(tidyverse)
library(dplyr)
retail_data <- read.csv("online_retail.csv",header=TRUE) 
head(retail_data)
dim(retail_data) #contains 541909 obs, 8 columns
length(unique(retail_data$CustomerID)) #4373 unique customer IDs


###### DATA CLEANING PORTION ###########################################################################

#Creating a Date & Time column
retail_data$Time <- format(as.POSIXct(retail_data$InvoiceDate,format="%d/%m/%Y %I:%M"),"%I:%M")
retail_data$Date <- format(as.POSIXct(retail_data$InvoiceDate,format="%d/%m/%Y %I:%M"),"%Y-%m-%d")
retail_data$InvoiceDate<- NULL

#Creating a TotalSpent column
retail_data$TotalSpent = retail_data$Quantity * retail_data$UnitPrice

#Exploring the occurence for each items
number <- table(retail_data$Description)
number <- data.frame(number)
colnames(number) <- c('Desciption', 'Freq')
number <- number[sort(number$Desciption),]

#remove all the problematic descriptions
number2 <- number[-c(14:28,58:59,152:156,167,174,179:186,321,323,598,603,649,655,695,
                     742:744,883,918,921,954:957,987,1001:1013,1061,1069:1070,1137:1151,
                     1153,1196,1293,1297,1418,1420:1426,1536,1681:1682,1790,1836:1838,
                     1839:1840,1901,1964,2068,2085:2087,2145:2146,2151,2155,2170,2187,
                     2188,2256:2258,2261,2263,2288:2290,2312,2343,2378,2380,2465,2817:2818,
                     2830:2831,2877:2879,3031:3033,3113:3114,3132,3134:3136,3434:3435,
                     3570,3582:3590,3635:3636,3703:3704,3728:3729,3734:3738,3739,3744,
                     3835,3976:3978,3986,3989:3997,4167:4181),]
new_retail_data <- retail_data[retail_data$Description %in% number2[,1],]

library(dplyr)
missingCust <- new_retail_data %>% filter(is.na(CustomerID))
knownCust <- new_retail_data %>% filter(!is.na(CustomerID))
intersect(missingCust$InvoiceNo,knownCust$InvoiceNo)

#Since the intersect of the InvoiceNo for those with missing and known CustomerID is empty,
# there are no rows with missing CustomerID that can be filled using InvoiceNo.

#Below we split the dataset into those with descriptions and those without. 
missingDctn <- new_retail_data %>% filter(Description == "")
retail_data_w_description <- new_retail_data %>% filter(Description != "")
intersect(missingDctn$StockCode, retail_data_w_description$StockCode)

retail_data_DescandCID <- retail_data_w_description %>% filter(!is.na(CustomerID))
retail_data_w_description_wo_CID <- retail_data_w_description %>% filter(is.na(CustomerID))
#retail_data_w_description is split into those with CID and those without.
#retail_data_DescandCID is data that has BOTH Description and CID. (Use for Analysis where CustomerID is needed)


#For the data without descriptions, we fill in the descriptions using StockCode.
missingDctn %>% filter(!is.na(CustomerID))
#There are no products that are missing description and have a CustomerID.
dim(missingDctn)
#1454 rows without Descriptions

unknownDctn <- unique(subset(missingDctn,missingDctn$Description == "", "StockCode"))
foundDctn <- unique(subset(new_retail_data,StockCode %in% unknownDctn$StockCode & new_retail_data$Description != ""))
for(i in unknownDctn$StockCode) {
  if (i %in% foundDctn$StockCode) {
    missingDctn[missingDctn$StockCode == i,"Description"] <- subset(foundDctn,StockCode %in% i,"Description")[1,]
  }  
}
missingDctn %>% filter(Description == "") %>% dim()
#There are still 124 rows without Descriptions.

#Removing rows without descriptions
retail_filled_descriptions <- missingDctn %>% filter(Description != "")

retail_filled_descriptions %>% filter(UnitPrice == 0) %>% dim()
retail_filled_descriptions %>% dim()
#All 1330 rows in retail_filled_description have UnitPrice = 0.

unknownPrice <- unique(subset(retail_filled_descriptions,retail_filled_descriptions$UnitPrice == 0, "StockCode"))
foundPrice <- unique(subset(new_retail_data,StockCode %in% unknownPrice$StockCode & new_retail_data$UnitPrice != 0)) %>% arrange(StockCode,desc(Date, Time))
for(i in unknownPrice$StockCode) {
  if (i %in% foundPrice$StockCode) {
    retail_filled_descriptions[retail_filled_descriptions$StockCode == i,"UnitPrice"] <- subset(foundPrice,StockCode %in% i,"UnitPrice")[1,]
  }  
}

retail_filled_descriptions %>% filter(UnitPrice == 0) %>% dim()
#There are still 26 rows with UnitPrice = 0.

retail_filled <- retail_filled_descriptions %>% filter(UnitPrice!= 0)
#This is the data with filled descriptions and price

#Update the TotalSpent column
retail_filled$TotalSpent = retail_filled$Quantity * retail_filled$UnitPrice

retail_wo_CID <- rbind(retail_filled, retail_data_w_description_wo_CID)
#This is the all the data without CustomerID.
#Combine this with retail_data_DescandCID to obtain data that can be used for analysis that does not require CustomerID.

#Removing those with InvoiceNo. starting with "C"
retail_data_DescandCID <- retail_data_DescandCID %>% filter(!str_detect(InvoiceNo,"C"))

#Removing those with very large quantity (>12000)
retail_data_DescandCID <- retail_data_DescandCID %>% filter(Quantity <= 12000)

#Removing those with quantity > 1600 & Unitprice==0
retail_data_DescandCID <- retail_data_DescandCID %>% filter(Quantity<1600 && UnitPrice!=0)

#Converting the Date column to a "DATE" class.
retail_data_DescandCID$Date <- as.Date(retail_data_DescandCID$Date)

#(start)dec 2010 - march 2011(end)
rfm_data_1 <- retail_data_DescandCID %>% filter(Date <= as.Date("2011-03-31"))

#(start)april 2011 - july 2011(end)
rfm_data_2 <- retail_data_DescandCID %>% filter(Date >= as.Date("2011-04-01")) %>% filter(Date < as.Date("2011-07-01")) 

#(start)aug 2011 - dec 2011(end)
rfm_data_3 <- retail_data_DescandCID %>% filter(Date >= as.Date("2011-07-01"))

#####################################
#                                   #
#            RFM Model              #
#                                   #
#####################################

#Now, for each customer, we find the days between each subsequent purchase, the total number of visits made, and amount spent.
total_visits <- NULL
total_amount <- NULL
since_prev <- NULL
for (id in unique(retail_data$CustomerID)) {
  total_visits <- c(total_visits, sum(retail_data$CustomerID == id))
  total_amount <- c(total_amount, sum(retail_data$totalspent[retail_data$CustomerID == id]))
  since_prev <- c(since_prev, min(as.numeric(as.Date("2011-11-01") - retail_data$Date[retail_data$CustomerID == id])))
}

head(total_visits)
length(total_visits) #4372 unique visits(i.e unique customer ID)
head(total_amount)

customer_retail_data <- data.frame(id=unique(retail_data$CustomerID),
                                   total_visits=total_visits,
                                   total_amount=total_amount,
                                   since_prev=since_prev)
head(customer_retail_data)

customers <- data.frame(cid = unique(retail_data$CustomerID))

#The following commands assign the recency, frequency, and monetary value rating on a scale of 1-5 with 5 being the most recent,
#most frequent, most monetary value, and 1 being the least recent, least frequent and least monetary value.

#New R, F, M-score assigning function.
map_quantiles <- function(vect, num_groups=5) { 
  ranks <- order(vect)
  result <- numeric(length(vect))
  one_unit <- floor(length(vect) / num_groups)
  for (index in 1:num_groups) { 
    if (index == num_groups) { 
      result[(index - 1) * one_unit < ranks] <- index 
    } else {
      result[(index - 1) * one_unit < ranks & ranks <= index * one_unit] <- index 
    }
  } 
  result
}

customers$recency <- 6 - map_quantiles(customer_retail_data$since_prev)

customers$frequency <- map_quantiles(customer_retail_data$total_visits)

customers$amount <- map_quantiles(customer_retail_data$total_amount)

#The RFM score is then a concatenation of the above three scores. Here is its calculation:
customers$rfm <- (customers$recency*100 + customers$frequency*10 + customers$amount)
head(customers)

