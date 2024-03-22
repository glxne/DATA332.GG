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



# Charts

![image](https://github.com/glxne/DATA332.GG/assets/159860384/665497f3-1672-409b-85db-2a0964d05691)




