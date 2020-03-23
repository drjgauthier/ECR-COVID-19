library(jsonlite)
library(tidyverse)
test_filepath<-"https://raw.githubusercontent.com/IBM/Dataset-Epidemiologic-Investigation-COVID19/master/ECR-COVID-19/test.txt"
train_filepath<-"https://raw.githubusercontent.com/IBM/Dataset-Epidemiologic-Investigation-COVID19/master/ECR-COVID-19/train.txt"
valid_filepath<-"https://raw.githubusercontent.com/IBM/Dataset-Epidemiologic-Investigation-COVID19/master/ECR-COVID-19/valid.txt"

#Import test dataset
content<-readLines(test_filepath)
res<-lapply(content,fromJSON)
str(res,list.len=10)
tibble_df <- tibble(casereport = res)
str(tibble_df)
test_df <- tibble_df %>% 
  unnest_auto(casereport)%>% 
  unnest_wider(events)

#Import train dataset
content<-readLines(train_filepath)
str(content)
glimpse(content)
res<-lapply(content,fromJSON)
str(res,list.len=10)
tibble_df <- tibble(casereport = res)
str(tibble_df)
train_df <- tibble_df %>% 
  unnest_auto(casereport) %>% 
  unnest_wider(events)

#Import valid dataset
content<-readLines(valid_filepath)
str(content)
glimpse(content)
#this will take a while
res<-lapply(content,fromJSON)
str(res,list.len=10)
tibble_df <- tibble(casereport = res)
str(tibble_df)
valid_df <- tibble_df %>% 
  unnest_auto(casereport) %>% 
  unnest_wider(events)

#Merge three files
df<-bind_rows(test_df,train_df,valid_df)

#Create death and inpatient variables
df %>% 
  mutate(death = str_detect(text,"死"),
         inpatient = str_detect(type,"Inpatient")) -> df
df$age <- str_extract(df$text,"[:digit:][:digit:]岁")
df$age <- str_replace(df$age,"岁","")

df %>% mutate(inpatient=case_when(
      inpatient == "FALSE" ~ 0 ,
      inpatient == "TRUE" ~ 1),
      age=as.numeric(as.character(age))) %>% 
      filter(!is.na(age),!is.na(death)) -> df
glimpse(df)

