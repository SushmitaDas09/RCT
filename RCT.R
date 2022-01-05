
#install.packages("data.table")
library(data.table)
#install.packages("stargazer")
library(stargazer)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("pwr")
library(pwr)
#install.packages("plm")
library(plm)
#install.packages("AER")
library(AER)
#install.packages("MatchIt")
library(MatchIt)


#clear environment
rm(list=ls());gc()

#load dataset
MyData<-fread('data_cleaned_new.csv', verbose = F)



#MyData <- MyData[!(MyData$willingness == "" | is.na(MyData$willingness)), ]

#simple ols without any control variables
first_model  <- lm(willingness ~ treatment, data=MyData)

#adding time one spends browsing online as a control variable
second_model  <- lm(willingness ~ treatment + factor(time_spent_day) , data=MyData)


# adding time one spends browsing online, demographics like gender, age, ethnicity as controls

third_model  <- lm(willingness ~ treatment+ factor(time_spent_day)+ factor(gender)+factor(age)+ factor(ethnicity), data=MyData)


stargazer(first_model, second_model, third_model,
          title="Regression",
          type="text",  covariate.labels = 
            c("treatment", "Time Spent: 1 - 2 hours", "Time Spent: 2 - 4 hours", "Time Spent: 4 - 6 hours", "Time Spent: More than 6 hours", 
              "Gender: Female", "Gender: Prefer not to answer", "Age: 18 - 25 years",
              "Age: 26 - 40 years", "Age: 40+ years", "Ethnicity: Black or African American", "Ethicity: Asian", "Etnicity: Hispanic or Latino", "Ethnicity: Other", 
              "Ethnicity: Prefer not to answer"),
          model.numbers=FALSE,
          column.labels=c("first",
                          "second" ,"third"))





# control variable frequency



#MyData <- MyData[!(MyData$willingness == "" | is.na(MyData$willingness)), ]

#simple ols 
first_model  <- lm(frequency ~ treatment, data=MyData)
stargazer(first_model, title="Regression",type="text", model.numbers=FALSE)



#HE


HE_model <- lm(willingness
                          ~ treatment +
                            I(frequency_binary>0) +
                            treatment:I(frequency_binary>0),
                          data = MyData)
stargazer(HE_model, title="Regression",type="text", model.numbers=FALSE)

HE_model <- lm(willingness
               ~ treatment +
                 frequency_binary +
                 treatment:frequency_binary,
               data = MyData)
stargazer(HE_model, title="Regression",type="text", model.numbers=FALSE)


HE_model <- lm(willingness
               ~ treatment +
                 I(time_spent_binary>0) +
                 treatment:I(time_spent_binary>0),
               data = MyData)
stargazer(HE_model, title="Regression",type="text", model.numbers=FALSE)

