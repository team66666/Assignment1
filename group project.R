#####################################
#                                   #
#           Reading Data            #
#                                   #
#####################################
library(dplyr)
library(tidyverse)
library(stringr)
retail_data <- read.csv("online_retail.csv",header=TRUE)
head(retail_data)
dim(retail_data) #contains 541909 obs, 8 columns
length(unique(retail_data$CustomerID)) #4373 unique customer IDs



#####################################
#                                   #
#          Data Cleaning            #
#                                   #
#####################################

#Convert CustomerID to factor
retail_data$CustomerID <- as.factor(retail_data$CustomerID)

#Convert Invoice date to character class first 
invoicedate <- as.character(retail_data$InvoiceDate)

#Creating a Date & Time column 
retail_data$Time <- format(as.POSIXct(invoicedate,format="%d/%m/%Y %H:%M"),"%H:%M")
retail_data$Date <- format(as.POSIXct(invoicedate,format="%d/%m/%Y %H:%M"),"%Y-%m-%d")
retail_data$InvoiceDate <- NULL

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
                     1839:1840,1901,1964,2068,2085:2087,2145:2146,2155,2170,2187,
                     2188,2256:2258,2261,2263,2288:2290,2312,2343,2378,2380,2465,2817:2818,
                     2830:2831,2877:2879,3031:3033,3113:3114,3132,3134:3136,3434:3435,
                     3570,3582:3590,3635:3636,3703:3704,3728:3729,3734:3738,3739,3744,
                     3835,3976:3978,3986,3989:3997,4167:4181),]
new_retail_data <- retail_data[retail_data$Description %in% number2[,1],]

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

retail_filled <- retail_filled_descriptions %>% filter(UnitPrice!= 0)
#This is the data with filled descriptions and price

#Update the TotalSpent column
retail_filled$TotalSpent = retail_filled$Quantity * retail_filled$UnitPrice

retail_wo_CID <- rbind(retail_filled, retail_data_w_description_wo_CID)
#This is the all the data without CustomerID.
#Combine this with retail_data_DescandCID to obtain data that can be used for analysis that does not require CustomerID

#Removing those with very large quantity (>12000)
retail_data_DescandCID <- retail_data_DescandCID %>% filter(Quantity <= 12000)

#Removing those with quantity > 1600 & Unitprice==0
retail_data_DescandCID <- retail_data_DescandCID %>% filter(Quantity<1600 && UnitPrice!=0)

#Removing those with InvoiceNo. starting with "C"
retail_data_wo_cancelled <- retail_data_DescandCID %>% filter(!str_detect(InvoiceNo,"C"))

#Converting the Date column to a "DATE" class.
retail_data_DescandCID$Date <- as.Date(retail_data_DescandCID$Date) #for M
retail_data_wo_cancelled$Date <- as.Date(retail_data_wo_cancelled$Date) #for R & F 

#(start)dec 2010 - march 2011(end)
rfm_data_1_M <- retail_data_DescandCID %>% filter(Date <= as.Date("2011-03-31"))
rfm_data_1_RF <- retail_data_wo_cancelled %>% filter(Date <= as.Date("2011-03-31"))

#(start)april 2011 - july 2011(end)
rfm_data_2_M <- retail_data_DescandCID %>% filter(Date >= as.Date("2011-04-01")) %>% filter(Date < as.Date("2011-07-01")) 
rfm_data_2_RF <- retail_data_wo_cancelled %>% filter(Date >= as.Date("2011-04-01")) %>% filter(Date < as.Date("2011-07-01")) 

#(start)aug 2011 - dec 2011(end)
rfm_data_3_M <- retail_data_DescandCID %>% filter(Date >= as.Date("2011-07-01"))
rfm_data_3_RF <- retail_data_wo_cancelled %>% filter(Date >= as.Date("2011-07-01"))


#####################################
#                                   #
# Data Analysis with Visualisation  #
#                                   #
#####################################
library(ggplot2)

#Top 10 most popular item sold for this retailer: 
data1 <- retail_wo_CID %>% filter(!str_detect(InvoiceNo,"C"))
full_data <- rbind(retail_data_wo_cancelled,data1)
mostpops <- full_data %>% group_by(Description) %>% summarise(NumberSold = sum(Quantity))
mostpops <- mostpops[order(-mostpops$NumberSold),]
mostpops <- mostpops[1:10,]
ggplot(mostpops,aes(x=mostpops$Description,y=mostpops$NumberSold,fill=mostpops$Description)) +
  geom_bar(stat="identity") + ylab("Number Sold") + xlab("Description") + title("Top 10 items sold")

#Total revenue earned for each country. Cancelled transactions are included in the calculation
revenue_per_country_data <- retail_data_wo_cancelled %>% group_by(Country) %>% summarise(TotalRevenue = sum(TotalSpent))
revenue_per_country_data <- revenue_per_country_data[order(-revenue_per_country_data$TotalRevenue),]

#plot the barchart for top 10 countries with highest revenue
top10revenue <- revenue_per_country_data[1:10,]
ggplot(data=top10revenue,
       aes(y=top10revenue$TotalRevenue,x=top10revenue$Country,fill=top10revenue$Country)) +
        geom_bar(stat="identity") +
        xlab("Country") + 
        ylab("Total Revenue") + 
        labs(title="Total Revenue Per Country(Top 10)", fill="Country")

#plot the barchart for the bottom 10 countries with lowest revenue
bottom10revenue <- revenue_per_country_data[27:37,]
ggplot(data=bottom10revenue,
       aes(y=bottom10revenue$TotalRevenue,x=bottom10revenue$Country,fill=bottom10revenue$Country)) +
      geom_bar(stat="identity") +
      xlab("Country") + 
      ylab("Total Revenue") + 
      labs(title="Total Revenue Per Country(Bottom 10)", fill="Country")


retail_wo_CID <- rbind(retail_filled, retail_data_w_description_wo_CID)
retail_wo_CID$Date <- as.Date(retail_wo_CID$Date)
combined_data <- rbind(retail_wo_CID,retail_data_DescandCID)
combined_data <- combined_data %>% filter(!str_detect(InvoiceNo,"C"))

# Visualisation of top 5 countries by number of orders
countries <- combined_data %>% group_by(Country) %>% summarise(count=n()) %>% arrange(desc(count)) 
top_4 <- countries %>% top_n(n = 4, count)
others <- countries[5:38,]
s <- sum(others$count)
levels(others$Country) <- c(levels(others$Country),"Others")
others <- rbind(c("Others", s),others)[1,]
countries <- rbind(top_4,others)


ggplot(countries, aes(x=1, fill=reorder(Country,count),y=count)) + geom_bar(stat = "identity") +
  xlab("") +
  ylab("Count") + theme(axis.ticks = element_blank(),axis.text.x = element_blank()) +
  labs(fill="Country") +  scale_fill_brewer(palette="Set1")


#Visualisation of Distribution of Number of Orders by Month
library(lubridate)
by_months <- combined_data %>% mutate(month=month(Date))  
by_months <- by_months %>% group_by(month) %>% summarise(count=n())
by_months$month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
by_months$month = factor(by_months$month, levels = month.abb)
ggplot(by_months) + geom_bar(aes(x=month,y=count), stat="identity")



#####################################
#                                   #
#               RFM                 #
#                                   #
#####################################

#FIRST PERIOD: Dec 2010 - March 2011

# The following code sorts the transactions by date.
rfm_data_1_RF <- rfm_data_1_RF[order(rfm_data_1_RF$Date),]
rfm_data_1_M <- rfm_data_1_M[order(rfm_data_1_M$Date),]

head(rfm_data_1_RF) 

################################################################################
# Now, for each customer, we find the days between each subsequent purchase,
# the total number of visits made, and amount spent.
total_visits_1 <- NULL
total_amount_1 <- NULL
since_prev_1 <- NULL

for (id in unique(rfm_data_1_RF$CustomerID)) {
  total_visits_1 <- c(total_visits_1, sum(rfm_data_1_RF$CustomerID == id))
  since_prev_1 <- c(since_prev_1, min(as.numeric(as.Date("2018/09/07")
                                                 - rfm_data_1_RF$Date[rfm_data_1_RF$CustomerID == id])))
  total_amount_1 <- c(total_amount_1, sum(rfm_data_1_M$TotalSpent[rfm_data_1_M$CustomerID == id]))
}

#This means that those that only have cancelled orders will be left out in RFM calculation.


customer_data_1 <- data.frame(id=unique(rfm_data_1_RF$CustomerID),
                              total_visits=total_visits_1,
                              total_amount=total_amount_1,
                              since_prev=since_prev_1)
head(customer_data_1)
################################################################################
customers_1 <- data.frame(cid = unique(customer_data_1$id))
# The following commands assign the recency, frequency, and monetary value
# rating on a scale of 1-5 with 5 being the most recent, most frequent, most
# monetary value, and 1 being the least recent, least frequent and least
# monetary value.

# New R, F, M-score assigning function.
map_quantiles <- function(vect, num_groups=5) {
  ranks <- rank(vect)
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

customers_1$recency <- 6 - map_quantiles(customer_data_1$since_prev)

customers_1$frequency <- map_quantiles(customer_data_1$total_visits)

customers_1$amount <- map_quantiles(customer_data_1$total_amount)

# The RFM score is then a concatenation of the above three scores. Here is its
# calculation:
customers_1$rfm <- (customers_1$recency*100
                    + customers_1$frequency*10
                    + customers_1$amount)
head(customers_1)




################################################################################################
# SECOND PERIOD: April 2011 - July 2011


# The following code sorts the transactions by date.
rfm_data_2_RF <- rfm_data_2_RF[order(rfm_data_2_RF$Date),]
rfm_data_2_M <- rfm_data_2_M[order(rfm_data_2_M$Date),]

head(rfm_data_2_RF) 

################################################################################
# Now, for each customer, we find the days between each subsequent purchase,
# the total number of visits made, and amount spent.
total_visits_2 <- NULL
total_amount_2 <- NULL
since_prev_2 <- NULL

for (id in unique(rfm_data_2_RF$CustomerID)) {
  total_visits_2 <- c(total_visits_2, sum(rfm_data_2_RF$CustomerID == id))
  since_prev_2 <- c(since_prev_2, min(as.numeric(as.Date("2018/09/07")
                                                 - rfm_data_2_RF$Date[rfm_data_2_RF$CustomerID == id])))
  total_amount_2 <- c(total_amount_2, sum(rfm_data_2_M$TotalSpent[rfm_data_2_M$CustomerID == id]))
}

#This means that those that only have cancelled orders will be left out in RFM calculation.


customer_data_2 <- data.frame(id=unique(rfm_data_2_RF$CustomerID),
                              total_visits=total_visits_2,
                              total_amount=total_amount_2,
                              since_prev=since_prev_2)
head(customer_data_2)
################################################################################
customers_2 <- data.frame(cid = unique(customer_data_2$id))

customers_2$recency <- 6 - map_quantiles(customer_data_2$since_prev)

customers_2$frequency <- map_quantiles(customer_data_2$total_visits)

customers_2$amount <- map_quantiles(customer_data_2$total_amount)

# The RFM score is then a concatenation of the above three scores. Here is its
# calculation:
customers_2$rfm <- (customers_2$recency*100
                    + customers_2$frequency*10
                    + customers_2$amount)
head(customers_2)



##############################################################################################
# THIRD PERIOD: Aug 2011 - Dec 2011


# The following code sorts the transactions by date.
rfm_data_3_RF <- rfm_data_3_RF[order(rfm_data_3_RF$Date),]
rfm_data_3_M <- rfm_data_3_M[order(rfm_data_3_M$Date),]

head(rfm_data_3_RF) 

################################################################################
# Now, for each customer, we find the days between each subsequent purchase,
# the total number of visits made, and amount spent.
total_visits_3 <- NULL
total_amount_3 <- NULL
since_prev_3 <- NULL

for (id in unique(rfm_data_3_RF$CustomerID)) {
  total_visits_3 <- c(total_visits_3, sum(rfm_data_3_RF$CustomerID == id))
  since_prev_3 <- c(since_prev_3, min(as.numeric(as.Date("2018/09/07")
                                                 - rfm_data_3_RF$Date[rfm_data_3_RF$CustomerID == id])))
  total_amount_3 <- c(total_amount_3, sum(rfm_data_3_M$TotalSpent[rfm_data_3_M$CustomerID == id]))
}

#This means that those that only have cancelled orders will be left out in RFM calculation.


customer_data_3 <- data.frame(id=unique(rfm_data_3_RF$CustomerID),
                              total_visits=total_visits_3,
                              total_amount=total_amount_3,
                              since_prev=since_prev_3)
head(customer_data_3)
################################################################################
customers_3 <- data.frame(cid = unique(customer_data_3$id))

customers_3$recency <- 6 - map_quantiles(customer_data_3$since_prev)

customers_3$frequency <- map_quantiles(customer_data_3$total_visits)

customers_3$amount <- map_quantiles(customer_data_3$total_amount)

# The RFM score is then a concatenation of the above three scores. Here is its
# calculation:
customers_3$rfm <- (customers_3$recency*100
                    + customers_3$frequency*10
                    + customers_3$amount)
head(customers_3)


###############################################################################################
#Combining the rfm from each period

#All the RFM values 

combined_rfm <- merge(customers_1, customers_2 ,by = "cid", all.x = TRUE)
combined_rfm <- merge(combined_rfm, customers_3, by = "cid", all.x = TRUE)
combined_rfm[is.na(combined_rfm)] <- 0
combined_rfm <- combined_rfm[,c(1,5,9,13)]
names(combined_rfm) <- c("CustomerID","RFM1", "RFM2","RFM3")


########### Segment RFM 1


customers_1$recency  <- as.numeric(as.character((customers_1$recency )))
customers_1$frequency <- as.numeric(as.character((customers_1$frequency)))
customers_1$amount <- as.numeric(as.character((customers_1$amount)))


segment <- function(customers){
  segment=character(nrow(customers))
  for(t in 1:nrow(customers)){
  r <- customers[t,]$recency 
  f <- customers[t,]$frequency
  m <- customers[t,]$amount
  if(r %in% seq(4,5) & f %in% seq(4,5) & m %in% seq(4,5)){
    segment[t] = "Champions"
  }else if (r %in% seq(2,5) & f %in% seq(3,5) & m %in% seq(3,5)){
    segment[t] = "Loyal Customers"
  }else if (r %in% seq(3,5) & f %in% seq(1,3) & m %in% seq(1,3)){
    segment[t] = "Potential Loyalist"
  }else if (r %in% seq(4,5) & f %in% seq(0,1) & m %in% seq(0,1)){
    segment[t] = "Recent Customers"
  }else if (r %in% seq(3,4) & f %in% seq(0,1) & m %in% seq(0,1)){
    segment[t] = "Promising"
  }else if (r %in% seq(2,3) & f %in% seq(2,3) & m %in% seq(2,3)){
    segment[t] = "Need Attention"
  }else if (r %in% seq(2,3) & f %in% seq(0,2) & m %in% seq(0,2)){
    segment[t] = "About To Sleep"
  }else if (r %in% seq(0,2) & f %in% seq(2,5) & m %in% seq(2,5)){
    segment[t] = "At Risk"
  }else if (r %in% seq(0,1) & f %in% seq(4,5) & m %in% seq(4,5)){
    segment[t] = "Can't Lose Them"
  }else if (r %in% seq(1,2) & f %in% seq(1,2) & m %in% seq(1,2)){
    segment[t] = "Hibernating"
  }else if (r %in% seq(0,2) & f %in% seq(0,2) & m %in% seq(0,2)){
    segment[t] = "Lost"
  }
  else{
    segment[t] = "Others"
  }
  }
  return(segment)
}

segments1 <- segment(customers_1)

segments1 <- as.data.frame(segments1)
segments1 <- data.frame(CID = customers_1$cid,segments1)
count1 <- group_by(segments1, Segment = segments1) %>% summarise(count1 = n())

########### Segment RFM 2


customers_2$recency  <- as.numeric(as.character((customers_2$recency )))
customers_2$frequency <- as.numeric(as.character((customers_2$frequency)))
customers_2$amount <- as.numeric(as.character((customers_2$amount)))

segments2 <- segment(customers_2)


segments2 <- as.data.frame(segments2)
segments2 <- data.frame(CID = customers_2$cid,segments2)
count2 <- group_by(segments2, Segment = segments2) %>% summarise(count2 = n())


########### Segment RFM 3

segments3=character(nrow(customers_3))
customers_3$recency  <- as.numeric(as.character((customers_3$recency )))
customers_3$frequency <- as.numeric(as.character((customers_3$frequency)))
customers_3$amount <- as.numeric(as.character((customers_3$amount)))

segments3 <- segment(customers_3)
segments3 <- as.data.frame(segments3)
segments3 <- data.frame(CID = customers_3$cid,segments3)
count3 <- group_by(segments3, Segment = segments3) %>% summarise(count3 = n())


####### Overall RFM 

rfm_data_M <- retail_data_DescandCID 
rfm_data_RF <- retail_data_wo_cancelled 

# The following code sorts the transactions by date.
rfm_data_RF <- rfm_data_RF[order(rfm_data_RF$Date),]
rfm_data__M <- rfm_data_M[order(rfm_data_M$Date),]

head(rfm_data_RF) 

################################################################################
# Now, for each customer, we find the days between each subsequent purchase,
# the total number of visits made, and amount spent.
total_visits <- NULL
total_amount <- NULL
since_prev <- NULL

for (id in unique(rfm_data_RF$CustomerID)) {
  total_visits <- c(total_visits, sum(rfm_data_RF$CustomerID == id))
  since_prev <- c(since_prev, min(as.numeric(as.Date("2018/09/07")
                                                 - rfm_data_RF$Date[rfm_data_RF$CustomerID == id])))
  total_amount <- c(total_amount, sum(rfm_data_M$TotalSpent[rfm_data_M$CustomerID == id]))
}

#This means that those that only have cancelled orders will be left out in RFM calculation.


customer_data <- data.frame(id=unique(rfm_data_RF$CustomerID),
                              total_visits=total_visits,
                              total_amount=total_amount,
                              since_prev=since_prev)


head(customer_data)
################################################################################
customers_all<- data.frame(cid = unique(customer_data$id))
# The following commands assign the recency, frequency, and monetary value
# rating on a scale of 1-5 with 5 being the most recent, most frequent, most
# monetary value, and 1 being the least recent, least frequent and least
# monetary value.


customers_all$recency <- 6 - map_quantiles(customer_data$since_prev)

customers_all$frequency <- map_quantiles(customer_data$total_visits)

customers_all$amount <- map_quantiles(customer_data$total_amount)

# The RFM score is then a concatenation of the above three scores. Here is its
# calculation:
customers_all$rfm <- (customers_all$recency*100
                    + customers_all$frequency*10
                    + customers_all$amount)
head(customers_all)

########### Segment overall RFM

segments_all=character(nrow(customers_all))
customers_all$recency  <- as.numeric(as.character((customers_all$recency )))
customers_all$frequency <- as.numeric(as.character((customers_all$frequency)))
customers_all$amount <- as.numeric(as.character((customers_all$amount)))

segments_all <- segment(customers_all)
segments_all <- as.data.frame(segments_all)
segments_all <- data.frame(CID = customers_all$cid,segments_all)
count_all <- group_by(segments_all, Segment = segments_all) %>% summarise(Count = n())


#####################################
#                                   #
#           VISUALISATION           #
#                                   #
#####################################

#### Overall RFM :

library(ggplot2)


levels <- as.factor(c("Champions", "Loyal Customers", "Potential Loyalist", "Need Attention", "About To Sleep","At Risk", "Hibernating",  "Others"))


count_all$Segment <- factor(count_all$Segment, levels = levels)

bp<- ggplot(count_all, aes(x="", y=Count, fill=Segment))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie + theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) +
  geom_text(aes(label = percent(Count/sum(Count))), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")

###### Plotting RFMs by Period 

counts <- merge(count1 ,count2 ,by = "Segment")
counts <- merge(counts, count3, by = "Segment")


forplotting <- counts %>% gather('count1':'count3',key = 'Period', value = 'Count')
forplotting$Segment <- factor(forplotting$Segment, levels = levels)


ggplot(forplotting, aes(x = Segment, y = percent(Count/cumsum(Count)))) +
  geom_col(aes(fill = Period), position = "dodge")+
  labs(title = "Segments of customers based on RFM scores", x = "Segments", y = "Percentage of Segment")+
  scale_fill_manual(labels = c("1st Dec 2010 - 31st March 2011", "1st April 2011 - 31st July 2011", "1st August 2011 - 31st December 2011"), values = c("royalblue4", "hotpink3", "yellow3"))




#### Average RFM Score across the Periods


combined_rfm$RFM1 <- as.numeric(combined_rfm$RFM1)
combined_rfm$RFM2 <- as.numeric(combined_rfm$RFM2)
combined_rfm$RFM3 <- as.numeric(combined_rfm$RFM3)

trend <- data.frame(Period = c("1st Dec 2010 - 31st March 2011", "1st April 2011 - 31st July 2011", "1st August 2011 - 31st December 2011"), AverageRFMScore = c(mean(combined_rfm$RFM1), mean(combined_rfm$RFM2), mean(combined_rfm$RFM3)))
trend$Period <- factor(trend$Period, levels =  c("1st Dec 2010 - 31st March 2011", "1st April 2011 - 31st July 2011", "1st August 2011 - 31st December 2011"))
ggplot(trend, aes(x = Period,y = AverageRFMScore)) + geom_line(aes(group = 1)) + geom_point() + labs(title= "Average RFM Scores across the periods", y = "Average RFM Scores")


###### RFM heatmap

customer_data$rf <- (customers_all$recency*10
                      + customers_all$frequency)

customer_data$rf <- as.factor(as.character(customer_data$rf))
rfmeanm <- customer_data %>% group_by(rf) %>% summarise(Mean = mean(total_amount))
rfmeanm$f <- as.factor(as.character(floor(as.numeric(as.character(rfmeanm$rf))/10)))
rfmeanm$r <- as.factor(as.character(as.numeric(as.character(rfmeanm$rf)) - 10* as.numeric(as.character(rfmeanm$f))))


ggplot(data = rfmeanm, mapping = aes(x = rfmeanm$f, y = rfmeanm$r, fill = rfmeanm$Mean))+
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white",
                       high = ("blue"), name = "Mean Monetary 
  Value per RF")+
  labs(title = "RFM Heat Map", y = "Recency", x= "Frequency" )



############## List of customers that only purchase from us once in the entire duration

customers_once <- customer_data %>% filter (total_visits == 1)
customers_once 

#####################################
#                                   #
#                MBA                #
#                                   #
#####################################

library(arules)
library(arulesViz)

retail_items <- retail_data_DescandCID %>% select(Description,InvoiceNo)


library(plyr)
trans <- ddply(retail_data_DescandCID,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))


trans$InvoiceNo <- NULL

trans$Date <- NULL

colnames(trans) <- c("items")




write.csv(trans,"market_basket_transactions.csv", quote = FALSE, row.names = TRUE)


tr <- read.transactions('market_basket_transactions.csv', format = 'basket', sep=',')


`trObj<-as(dataframe.dat,"transactions")`

summary(tr)
itemFrequencyPlot(tr,topN=20,type="absolute", main = "Top 20 Absolute Item Frequency Plot")

# Parameter Specification: min_sup=0.01 and min_confidence=0.8 values with 10 items as max of items in a rule.
association.rules <- apriori(tr, parameter = list(supp = 0.01, conf=0.8,maxlen=10))
summary(association.rules)

inspect(sort(association.rules, by = "lift")[1:10])
plot(rules[1:10],method = "graph",
     control = list(type = "Items"))

# We can also find out Customers who bought SUGAR also bought.... :

t.association.rules <- apriori(tr, parameter = list(supp=0.01, conf=0.8, maxlen = 10),appearance = list(lhs="SUGAR",default="rhs"))
inspect(sort(t.association.rules, by = "lift"))

#### TOP 10 RULES WITH min_sup - 0.01 and min_confidence = 0.8

top10rules <- head(association.rules, n = 10, by = "lift")
plot(top10rules, method = "graph",   control = list(type = "Items"))
plot(top10rules,method="paracoord")




####################################
#                                   #
#             Latency               #
#                                   #
#####################################
latency <-retail_data_wo_cancelled %>% as.tibble() %>% select(CustomerID, InvoiceNo, TotalSpent, Date)
latency <- latency %>% group_by(InvoiceNo) %>% mutate(TotalAmount = sum(TotalSpent))
latency$TotalSpent <- NULL

latency2 <- latency %>% unique() %>%arrange(Date)%>%group_by(CustomerID)%>%
  mutate(purchase_number=rank(Date,ties.method = "first"),total_purchases=n()) %>%filter(total_purchases>1)

find_avg_latency <- function(greater_purchase_num) {
  as.numeric((latency2%>%filter(total_purchases>=greater_purchase_num)%>%
                summarise(latency=nth(Date,greater_purchase_num)-nth(Date,greater_purchase_num-1))%>%summarise(mean(latency)))[[1]])}
one_to_two <- find_avg_latency(2)
two_to_three <- find_avg_latency(3)
three_to_four <- find_avg_latency(4)
four_to_five <- find_avg_latency(5) 
five_to_six <- find_avg_latency(6)

print(c(one_to_two, two_to_three, three_to_four, four_to_five,five_to_six))

between_purchases <- c("1 and 2", "2 and 3", "3 and 4", "4 and 5")
average_latency <- c(one_to_two, two_to_three, three_to_four, four_to_five,five_to_six)
cumulative_latency <- cumsum(average_latency)

latency_data <- tribble(
  ~Purchases,    
  "1 and 2",       
  "2 and 3",       
  "3 and 4",      
  "4 and 5",      
  "5 and 6"      
)
latency_data <- latency_data %>% cbind(average_latency) %>% cbind(cumulative_latency)
ggplot(data = latency_data) +
  geom_bar(mapping = aes(x = Purchases, y = average_latency), stat = "identity")
ggplot(data = latency_data) +
  geom_bar(mapping = aes(x = Purchases, y = cumulative_latency), stat = "identity")


#Latency for customers WE WANT TO WIN BACK
winback_segment <- segments_all %>% filter(segments_all %in% c("At Risk","Hibernating","About To Sleep","Need Attention"))
winback_data <- subset(latency2,latency2$CustomerID %in% winback_segment$CID)

find_avg_latency <- function(greater_purchase_num) {
  as.numeric((winback_data%>%filter(total_purchases>=greater_purchase_num)%>%
                summarise(latency=nth(Date,greater_purchase_num)-nth(Date,greater_purchase_num-1))%>%summarise(mean(latency)))[[1]])}

one_to_two_wb <- find_avg_latency(2)
two_to_three_wb <- find_avg_latency(3)
three_to_four_wb <- find_avg_latency(4)
four_to_five_wb <- find_avg_latency(5) 
five_to_six_wb <- find_avg_latency(6)
print(c(one_to_two_wb, two_to_three_wb, three_to_four_wb, four_to_five_wb,five_to_six_wb))

between_purchases <- c("1 and 2", "2 and 3", "3 and 4", "4 and 5")
average_latency_wb <- c(one_to_two_wb, two_to_three_wb, three_to_four_wb, four_to_five_wb,five_to_six_wb)
cumulative_latency_wb <- cumsum(average_latency)

latency_data_wb <- tribble(
  ~Purchases,    
  "1 and 2",       
  "2 and 3",       
  "3 and 4",      
  "4 and 5",      
  "5 and 6"      
)

latency_data_wb <- latency_data_wb %>% cbind(average_latency_wb) %>% cbind(cumulative_latency_wb)
ggplot(data = latency_data_wb) +
  geom_bar(mapping = aes(x = Purchases, y = average_latency_wb), stat = "identity")
ggplot(data = latency_data_wb) +
  geom_bar(mapping = aes(x = Purchases, y = cumulative_latency_wb), stat = "identity")

