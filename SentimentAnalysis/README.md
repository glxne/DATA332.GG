# Sentiment Analysis - Gianni G. <span style="font-size:larger;">✅</span>

# Overview  <span style="font-size:15;">
In this project I analyzed consumer complaint data using text mining and sentiment analysis. I created 3 charts using the data to show information I thought to be signficant. I will be including a raw file of my R script that I used which will include everything. I will also include the original .csv file used.

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
- I realized that many of the smaller sentiment words that were used can contribute to a bigger word - Positive or Negative. I decided to change most of the sentiments to be either positive or negative

  


