library(textdata)
library(tidytext)
library(dplyr)
library(stringr)
library(janeaustenr)
library(tidyr)
library(readxl)
library(ggplot2)
library(tidyverse)

setwd("C:/Users/Gdot7/OneDrive/Documents/RScripts/ConsumerComplaint")

complaints <- read.csv("Consumer_Complaints.csv")

complaints_copy <- complaints


ggplot(complaints, aes(x = Product)) +
  geom_bar(fill = "skyblue", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  labs(x = "Product", y = "Count", title = "Count of Each Product")


ggplot(complaints, aes(x = Sub.product)) +
  geom_bar(fill = "skyblue", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  labs(x = "Product", y = "Count", title = "Count of Each Product")

complaints$Product <- ifelse(complaints$Product %in% c("Consumer Loan", "Payday loan", "Student loan"), "Loan", complaints$Product)

complaints$Sub.product <- ifelse(complaints$Sub.product %in% c("Vehicle lease", "Vehicle loan" , "Auto"), "Vehicle", complaints$Sub.product)

complaints$Sub.product <- ifelse(complaints$Sub.product %in% c("Credit repair", "Personal line of credit"), "Personal Credit", complaints$Sub.product)

complaints$Sub.product <- ifelse(complaints$Sub.product %in% c("Conventional fixed mortgage", "Conventional adjustable mortgage (ARM)" , "FHA mortgage", "Other mortgage" , "Second mortgage" , "VA mortgage" , "Reverse mortgage"), "Mortgage", complaints$Sub.product)

complaints$Sub.product <- ifelse(complaints$Sub.product %in% c("Domestic (US) money transfer", "International money transfer"), "Money transfer", complaints$Sub.product)

complaints$Sub.product <- ifelse(complaints$Sub.product %in% c("Cashing a check without an account"), "Check cashing", complaints$Sub.product)

complaints$Sub.product <- ifelse(complaints$Sub.product %in% c("Federal student loan" , "Federal student loan servicing" , "Non-federal student loan"), "Student loan", complaints$Sub.product)

complaints$Sub.product <- ifelse(complaints$Sub.product %in% c("Electronic Benefit Transfer / EBT card" , "General purpose card" , "Payroll card", "Transit card","Other special purpose card","ID prepaid card","Gift or merchant card"), "Specialty Card", complaints$Sub.product)

ggplot(complaints, aes(x = Company)) +
  geom_bar(fill = "skyblue", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  labs(x = "Company", y = "Count", title = "Count of Each Company")


get_sentiments("nrc")
get_sentiments("bing")

complaints <- complaints %>%
  distinct()

complaints$Consumer.complaint.narrative[complaints$Consumer.complaint.narrative == ""] <- "NA_text"


complaints_processed <- complaints %>%
  mutate(Consumer.complaint.narrative = tolower(Consumer.complaint.narrative)) %>%
  mutate(Consumer.complaint.narrative = gsub("[[:punct:]]", "", Consumer.complaint.narrative))

# Tokenize text
complaints_tokens <- complaints_processed %>%
  unnest_tokens(word, Consumer.complaint.narrative)

# Load lexicons
data("nrc")
data("bing")

# Perform sentiment analysis using "nrc"
sentiments_nrc <- inner_join(complaints_tokens, get_sentiments("nrc"), by = "word")

# Count sentiment occurrences
sentiment_counts_nrc <- sentiments_nrc %>%
  count(sentiment)

# Perform sentiment analysis using "bing"
sentiments_bing <- inner_join(complaints_tokens, get_sentiments("bing"), by = "word")

# Count sentiment occurrences
sentiment_counts_bing <- sentiments_bing %>%
  count(sentiment)
