# Sentiment Analysis - Gianni G. <span style="font-size:larger;">✅</span>

# Overview  <span style="font-size:15;">
In this project I analyzed consumer complaint data using text mining and sentiment analysis. I created 3 charts using the data to show information I thought to be signficant. I will be including a raw file of my R script that I used which will include everything. I will also include the original .csv file used.
*Note: I was not able to upload the data .csv file to my repo because it exceeds 25MB.
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

2. Did the same with the Sub.product column
   - More to clean than the product column
```
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
```

- More cleaning was when I implemented the sentiments into the date which is under the 3rd chart

# Charts

1. I first made a wordcloud of the emotion associated with the words each sentiment. Results came from nrc lexicon. Positive and Negative were about the same in terms of number but when you include the other words (most being negative), negativity does exceed positive which is expected.
![image](https://github.com/glxne/DATA332.GG/assets/159860384/665497f3-1672-409b-85db-2a0964d05691)
```
words <- sentiment_counts_nrc$sentiment
counts <- sentiment_counts_nrc$n
word_counts <- data.frame(word = words, freq = counts)
wordcloud(words = word_counts$word, freq = word_counts$freq, min.freq = 1,
          max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
```

2. I created a bar graph using ggplot2 to show the frequency of the Sentiments
![image](https://github.com/glxne/DATA332.GG/assets/159860384/f14ca676-c86f-43b5-839c-db696f63a8a5)
```
ggplot(sentiment_counts_nrc, aes(x = sentiment, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Sentiment Analysis",
       x = "Sentiment",
       y = "Count") +
  theme_minimal()
```

3. I created a pivot table of the sentiment values and product to see if there was a correlation as they were easily accessible to me with the current dataframe I was using. It also made it easier to make a heat map to chart the data.
![image](https://github.com/glxne/DATA332.GG/assets/159860384/911d2a55-719c-49c0-90b4-4bb8ba1000c0)
```
pivot_table <- table(sentiments_nrc$sentiment, sentiments_nrc$Product)

ggplot(data = as.data.frame(pivot_table)) +
  geom_tile(aes(x = Var2, y = Var1, fill = Freq)) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Sentiment-Product Correlation Heat Map",
       x = "Product",
       y = "Sentiment")
```
4. I also created a segmented bar graph to show each of the feelings in each Product that was being complained
![image](https://github.com/glxne/DATA332.GG/assets/159860384/5299ce66-fc22-4b9f-8600-6b02701e6cc1)
*5e+05 being 500,000, etc.
```
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
```
Mortgage seems to have the biggest as it is the most reported Product. One thing I found interesting was that each bar has a somewhat equal size in each sentiment compared to the overall size of the bar. This goes for every bar (for the most part).

# Finding a way to make the data more clean/tidy
- I realized that many of the smaller sentiment words that were used can contribute to a bigger word - Positive or Negative. I decided to change most of the sentiments to be either positive or negative. I did this by using these functions:
```
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
```
*Sidenote: I decided to include anticipation under the "negative" connatation because I believe that is the right place to put it in this context. I also excluded "surprise" because I couldn't decide to put it under negative or positive as it applies to both equally. It is shown as NA in the data and as seen below:
![image](https://github.com/glxne/DATA332.GG/assets/159860384/7cb7dd13-9c9b-4882-99e1-1e64171f7a79)
```
ggplot(sentiments_nrc2, aes(x = Product, fill = category)) +
  geom_bar(position = "stack") +
  labs(title = "Sentiment Distribution by Product",
       x = "Product",
       y = "Count",
       fill = "Sentiment Category") +
  theme_minimal()
```
- Slightly more negative than positive sentiments is expected but I am surprised at how close they are

5. Lastly, I wanted to see if there is a correlation between issue and product so first I cleaned the data (somewhat) just to make the heat map more readable:
```
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
```
- I then created a pivot table of the two values:
```
pt_issueproduct <- table(sentiments_nrc$Issue, sentiments_nrc$Product)
```
I then made a heat map of the data:
![image](https://github.com/glxne/DATA332.GG/assets/159860384/44f81d2a-72ea-4dd8-b6c8-ba5e9b329026)
```
ggplot(data = as.data.frame(pt_issueproduct), aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Correlation Between Issue and Product",
       x = "Product",
       y = "Issue")
```
Based on this heat map, it appear that most of the problems are Lender related, especially with Mortgage. I expect it to be more frequent with mortgage because it is the "Product" people have the most to say about, negative and positive, and it seems to be the one people mostly have problems with, especially with Lenders. So it's likely the Lenders are the biggest contribution to this issue with mortgage.




  


