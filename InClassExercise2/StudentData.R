library(dplyr)
library(readxl)
library(ggplot2)

setwd("C:/Users/Gdot7/OneDrive/Documents/RScripts/StudentData")
students <- read_excel("Student.xlsx")
registration <- read_excel("Registration.xlsx")
course <- read_excel("Course.xlsx")

course_registration <- left_join(course, registration, by = "Instance ID")

course_reg_student <- left_join(course_registration, students, by = "Student ID")

majors_df <- course_reg_student %>%
  group_by(Title) %>%
  summarize(count = n())

ggplot(majors_df, aes(x = Title, y = count)) +
  geom_col() +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))

course_reg_student[c('DOB.Year', 'DOB.Month', 'DOB.Date')] <- str_split_fixed(course_reg_student$`Birth Date`, '-', 3)

birthyear_df <- course_reg_student %>%
  group_by(DOB.Year) %>%
  summarize(count = n())

ggplot(birthyear_df, aes(x = DOB.Year, y = count)) +
  geom_col() + 
  theme(axis.text = element_text(angle = 90, vjust = .5, hjust = 1))

df_major_totalcost <- course_reg_student %>%
  group_by(Title, `Payment Plan`) %>%
  summarize(sum(`Total Cost`))

df_balancedue_major <- course_reg_student %>%
  group_by(Title) %>%
  summarize(sum(`Balance Due`))

df_major_totalcost <- course_reg_student %>%
  group_by(Title, `Payment Plan`) %>%
  summarize(total_cost = sum(`Total Cost`))

ggplot(df_major_totalcost, aes(x = Title, y = total_cost, fill = `Payment Plan`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Cost per Major Segmented by Payment Plan", x = "Major", y = "Total Cost") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.title = element_blank())

df_balance_due_major <- course_reg_student %>%
  group_by(Title, `Payment Plan`) %>%
  summarize(total_balance_due = sum(`Balance Due`))

ggplot(df_balance_due_major, aes(x = Title, y = total_balance_due, fill = `Payment Plan`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Balance Due per Major Segmented by Payment Plan", x = "Major", y = "Total Balance Due") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.title = element_blank())

