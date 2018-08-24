# Assignment1

# DESCRIPTION OF DATA
-InvoiceNo: Invoice number. Nominal, a 6-digit integral number uniquely assigned to each transaction. If this code starts with letter 'c', it indicates a cancellation. 
-StockCode: Product (item) code. Nominal, a 5-digit integral number uniquely assigned to each distinct product. 
-Description: Product (item) name. Nominal. 
-Quantity: The quantities of each product (item) per transaction. Numeric.	
-InvoiceDate: Invice Date and time. Numeric, the day and time when each transaction was generated. 
-UnitPrice: Unit price. Numeric, Product price per unit in sterling. 
-CustomerID: Customer number. Nominal, a 5-digit integral number uniquely assigned to each customer. 
-Country: Country name. Nominal, the name of the country where each customer resides.

Extra columns:
- Date: Date in YYYY-mm-dd format
- Time: Time in HH:MM
- TotalSpent : A column of all the totalspent per row, qty * unit price = total spent for that row.


# Reading the data
retail_data <- read.csv("online_retail.csv",header=TRUE) 
head(retail_data)
dim(retail_data) #contains 541909 obs, 8 columns
length(unique(retail_data$CustomerID)) #4373 unique customer IDs


# DATA CLEANING PORTION

Creating a Date & Time column
retail_data$Time <- format(as.POSIXct(retail_data$InvoiceDate,format="%d/%m/%Y %I:%M"),"%I:%M")
retail_data$Date <- format(as.POSIXct(retail_data$InvoiceDate,format="%d/%m/%Y %I:%M"),"%Y-%m-%d")

Creating a TotalSpent column
retail_data$TotalSpent = retail_data$Quantity * retail_data$UnitPrice

########################################################################################################


# RFM MODEL(Run only when data is fully cleaned, & amend accordingly if theres a need to)

//Now, for each customer, we find the days between each subsequent purchase, the total number of visits made, and amount spent.
total_visits <- NULL
total_amount <- NULL
since_prev <- NULL
for (id in unique(retail_data$CustomerID)) {
  total_visits <- c(total_visits, sum(retail_data$CustomerID == id))
  total_amount <- c(total_amount, sum(retail_data$totalspent[retail_data$CustomerID == id]))
  since_prev <- c(since_prev, min(as.numeric(as.Date("2011-11-01")
                                             - retail_data$Date[retail_data$CustomerID == id])))
}

head(total_visits)
length(total_visits) #4372 unique visits(i.e uniquee customer ID)
head(total_amount)

customer_retail_data <- data.frame(id=unique(retail_data$CustomerID),
                                   total_visits=total_visits,
                                   total_amount=total_amount,
                                   since_prev=since_prev)
head(customer_retail_data)



customers <- data.frame(cid = unique(retail_data$CustomerID))
The following commands assign the recency, frequency, and monetary value rating on a scale of 1-5 with 5 being the most recent, most frequent, most monetary value, and 1 being the least recent, least frequent and least monetary value.

//New R, F, M-score assigning function.
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

// The RFM score is then a concatenation of the above three scores. Here is its calculation:
customers$rfm <- (customers$recency*100
                  + customers$frequency*10
                  + customers$amount)
head(customers)
##########################################################################################################
