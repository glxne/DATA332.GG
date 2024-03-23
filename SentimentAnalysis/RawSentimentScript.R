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
ggplot(sentiment_counts_nrc, aes(x = sentiment, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Sentiment Analysis",
       x = "Sentiment",
       y = "Count") +
  theme_minimal()


words <- sentiment_counts_nrc$sentiment
counts <- sentiment_counts_nrc$n

word_counts <- data.frame(word = words, freq = counts)

wordcloud(words = word_counts$word, freq = word_counts$freq, min.freq = 1,
          max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

filtered_words <- complaints_tokens$word[!(complaints_tokens$word %in% c("natext"))]

merged_data <- merge(sentiment_counts_nrc, complaints_processed, by.x = "sentiment", by.y = "Consumer.complaint.narrative", all = TRUE)

ggplot(merged_data, aes(x = Product, fill = sentiment)) +
  geom_bar() +
  labs(title = "Sentiment Analysis by Product",
       x = "Product",
       fill = "Sentiment") +
  theme_minimal()

aggregated_data <- aggregate(n ~ Product + sentiment, data = merged_data, FUN = sum)

ggplot(aggregated_data, aes(x = Product, y = n, fill = sentiment)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Sentiment Analysis by Product",
       x = "Product",
       y = "Count",
       fill = "Sentiment") +
  theme_minimal()


aggregated_data <- aggregate(n ~ Product + sentiment, data = merged_data, FUN = sum)

sentiment_counts_nrc$sentiment <- tolower(sentiment_counts_nrc$sentiment)
complaints_processed$Consumer.complaint.narrative <- tolower(complaints_processed$Consumer.complaint.narrative)

merged_data <- merge(sentiment_counts_nrc, complaints_processed, by.x = "sentiment", by.y = "Consumer.complaint.narrative", all = TRUE)

sentiment_counts_by_product <- merged_data %>%
  group_by(Product, sentiment) %>%
  summarise(count = sum(n))

ggplot(sentiment_counts_by_product, aes(x = Product, y = count, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Chart of Sentiment Counts by Product",
       x = "Product",
       y = "Count",
       fill = "Sentiment") +
  theme_minimal()

pivot_table <- table(sentiments_nrc$sentiment, sentiments_nrc$Product)

ggplot(data = as.data.frame(pivot_table)) +
  geom_tile(aes(x = Var2, y = Var1, fill = Freq)) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Sentiment-Product Correlation Heat Map",
       x = "Product",
       y = "Sentiment")

sentiment_counts <- sentiments_nrc %>%
  group_by(Product, sentiment) %>%
  summarise(count = n()) %>%
  arrange(Product)

ggplot(data = sentiment_counts, aes(x = Product, y = count, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment Distribution by Product",
       x = "Product",
       y = "Count",
       fill = "Sentiment") +
  theme_minimal()

sentiments_nrc2 <- sentiments_nrc

sentiments_nrc2

sentiments_nrc2$sentiment <- ifelse(sentiments_nrc2$sentiments %in% c("anger", "disgust", "fear", "sadness", "surprise"), "negative", sentiments_nrc2$sentiments)
sentiments_nrc2$sentiment <- ifelse(sentiments_nrc2$sentiments %in% c("joy", "trust", "anticipation"), "positive", sentiments_nrc2$sentiments)

map_sentiment <- function(sentiment) {
  if (sentiment_word %in% c("anger", "disgust", "fear", "negative", "sadness", "anticipation")) {
    return("negative")
  } else if (sentiment %in% c("joy", "positive", "trust")) {
    return("positive")
  } else {
    return(NA)
  }
}

sentiments_nrc2$category <- ifelse(sentiments_nrc2$sentiment %in% c("anger", "disgust", "fear", "negative", "sadness", "anticipation"),
                                  "negative",
                                  ifelse(sentiments_nrc2$sentiment %in% c("joy", "positive", "trust"),
                                         "positive",
                                         NA))

ggplot(sentiments_nrc2, aes(x = Product, fill = category)) +
  geom_bar(position = "stack") +
  labs(title = "Sentiment Distribution by Product",
       x = "Product",
       y = "Count",
       fill = "Sentiment Category") +
  theme_minimal()

sentiments_nrc$Issue <- ifelse(sentiments_nrc$Issue %in% c("Overlimit fee", "Charged fees or interest I didn't expect", "Other fee", "Balance transfer fee", "Late fee", "Cash advance fee", "Unexpected/Other fees", "Fee", "Excessive fees"),
                          "Fees", sentiments_nrc$Issue)
sentiments_nrc$Issue <- ifelse(sentiments_nrc$Issue %in% c("Can't contact lender", "Lender repossessed or sold the vehicle", "Lender damaged or destroyed vehicle", "Lender sold the property", "Dealing with my lender or servicer"),
                               "Lender related", sentiments_nrc$Issue)
sentiments_nrc$Issue <- ifelse(sentiments_nrc$Issue %in% c("Billing disputes", "Billing statement"),
                               "Billing", sentiments_nrc$Issue)
sentiments_nrc$Issue <- ifelse(sentiments_nrc$Issue %in% c("Closing/Cancelling account", "Account opening, closing, or management", "Account terms and changes", "Can't stop charges to bank account", "Managing, opening, or closing account", "Sale of account"),
                               "Account related", sentiments_nrc$Issue)
sentiments_nrc$Issue <- ifelse(sentiments_nrc$Issue %in% c("Loan servicing, payments, escrow account", "Received a loan I didn't apply for", "Loan modification, collection, foreclosure", "Managing the loan or lease", "Taking out the loan or lease", "Shopping for a loan or lease", "Applied for loan/did not receive money", "Can't repay my loan"),
                               "Loan", sentiments_nrc$Issue)
sentiments_nrc$Issue <- ifelse(sentiments_nrc$Issue %in% c("Credit decision / Underwriting","Unable to get credit report/credit score", "Credit determination", "Credit line increase/decrease", "Credit monitoring or identity protection", "Credit reporting company's investigation", "Shopping for a line of credit", "Improper use of my credit report", "Incorrect information on credit report", "Managing the line of credit"),
                               "Credit", sentiments_nrc$Issue)
sentiments_nrc$Issue <- ifelse(sentiments_nrc$Issue %in% c("Customer service/Customer relations", "Customer service / Customer relations"),
                               "Customer service", sentiments_nrc$Issue)
pt_issueproduct <- table(sentiments_nrc$Issue, sentiments_nrc$Product)

ggplot(data = as.data.frame(pt_issueproduct), aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Correlation Between Issue and Product",
       x = "Product",
       y = "Issue")
