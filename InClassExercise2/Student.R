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
  group_by(Title) %>%
  summarize(sum(`Total Cost`))

df_balancedue_major <- course_reg_student %>%
  group_by(Title) %>%
  summarize(sum(`Balance Due`))
