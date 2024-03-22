library(textdata)
library(tidytext)
library(dplyr)
library(stringr)
library(janeaustenr)
library(tidyr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(wordcloud)

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

complaints_tokens <- complaints_processed %>%
  unnest_tokens(word, Consumer.complaint.narrative)

data("nrc")
data("bing")

sentiments_nrc <- inner_join(complaints_tokens, get_sentiments("nrc"), by = "word")

sentiment_counts_nrc <- sentiments_nrc %>%
  count(sentiment)

sentiments_bing <- inner_join(complaints_tokens, get_sentiments("bing"), by = "word")

sentiment_counts_bing <- sentiments_bing %>%
  count(sentiment)

# Merging Products used and sentiment
merged_data_nrc <- merge(sentiment_counts_nrc, complaints_processed[, c("Consumer.complaint.narrative", "Product")], by.x = "sentiment", by.y = "Consumer.complaint.narrative", all.x = TRUE)
merged_data_bing <- merge(sentiment_counts_bing, complaints_processed[, c("Consumer.complaint.narrative", "Product")], by.x = "sentiment", by.y = "Consumer.complaint.narrative", all.x = TRUE)
merged_data_nrc <- merge(sentiment_counts_nrc, complaints_processed[, c("Product")], by.x = "sentiment", by.y = "Consumer.complaint.narrative", all.x = TRUE)

# Plotting sentiments
ggplot(sentiment_counts_nrc, aes(x = Product, y = n, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sentiment Analysis (NRC) by Product",
       x = "Product",
       y = "Count") +
  theme_minimal()


ggplot(merged_data_bing, aes(x = Product, y = n, fill = sentiment)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Sentiment Analysis (Bing) by Product",
       x = "Product",
       y = "Count") +
  theme_minimal()

words <- sentiment_counts_nrc$sentiment
counts <- sentiment_counts_nrc$n

word_counts <- data.frame(word = words, freq = counts)

wordcloud(words = word_counts$word, freq = word_counts$freq, min.freq = 1,
          max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

wordcloud(words = complaints_tokens$word, min.freq = 5, random.order = FALSE)
