# Sentiment Analysis - Gianni G. <span style="font-size:larger;">✅</span>

# Overview  <span style="font-size:15;">
In this project I analyzed consumer complaint data using text mining and sentiment analysis. I created 3 charts using the data to show information I thought to be signficant.

# Data Cleaning Process
1. I started by cleaning the Product column
   - I first did a ggplot of the data so I can easily find overlapping values that can fall under the same category. Wasn't too bad, I decided to put all the different types of loans under "Loans"

```
ggplot(complaints, aes(x = Product)) +
  geom_bar(fill = "skyblue", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  labs(x = "Product", y = "Count", title = "Count of Each Product")

complaints$Product <- ifelse(complaints$Product %in% c("Consumer Loan", "Payday loan", "Student loan"), "Loan", complaints$Product)

```






