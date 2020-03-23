require(jsonlite)
test_filepath<-"https://raw.githubusercontent.com/IBM/Dataset-Epidemiologic-Investigation-COVID19/master/ECR-COVID-19/test.txt"
train_filepath<-"https://raw.githubusercontent.com/IBM/Dataset-Epidemiologic-Investigation-COVID19/master/ECR-COVID-19/train.txt"
valid_filepath<-"https://raw.githubusercontent.com/IBM/Dataset-Epidemiologic-Investigation-COVID19/master/ECR-COVID-19/valid.txt"
#method A: read each line and convert
content<-readLines(test_filepath)
str(content)
glimpse(content)
#this will take a while
res<-lapply(content,fromJSON)
str(res,list.len=10)
tibble_df <- tibble(casereport = res)
str(tibble_df)
test_df <- tibble_df %>% 
  unnest_auto(casereport)%>% 
  unnest_wider(events)

content<-readLines(train_filepath)
str(content)
glimpse(content)
#this will take a while
res<-lapply(content,fromJSON)
str(res,list.len=10)
tibble_df <- tibble(casereport = res)
str(tibble_df)
train_df <- tibble_df %>% 
  unnest_auto(casereport) %>% 
  unnest_wider(events)

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

df %>% 
  mutate(death = str_detect(text,"死"),
         inpatient = str_detect(type,"Inpatient")) -> df

df %>% group_by(death) %>% count()
df %>% group_by(inpatient) %>% count()

df$age <- str_extract(df$text,"[:digit:][:digit:]岁")
df %>% separate(age,sep="岁",c("age", "B")) %>% 
  mutate(inpatient=case_when(
    inpatient == "FALSE" ~ 0 ,
    inpatient == "TRUE" ~ 1),
    age=as.numeric(as.character(age))) %>% 
  filter(!is.na(age),!is.na(death))-> df
glimpse(df)
library(rms)

fit<- glm(death~rcs(age,7),
          family="binomial",data=df)
AIC(fit)
s<-data.frame(predict(fit,se.fit=TRUE))
df<-bind_cols(df,s)

df$logit_upper <- df$fit + 1.96*df$se.fit
df$logit_lower <- df$fit - 1.96*df$se.fit
df$prob <- exp(df$fit)/(1 + exp(df$fit))
df$prob_upper <- exp(df$logit_upper)/(1 + exp(df$logit_upper))
df$prob_lower <- exp(df$logit_lower)/(1 + exp(df$logit_lower))
df$prob_lower[df$prob_lower<0] <- 0
ggplot(df,aes(x=age)) + 
  geom_line(aes(y=prob))+
  geom_ribbon(aes(ymin=prob_lower,ymax=prob_upper),alpha=0.1)+
  labs(x="Age",y="Probability of hospitalization")+
  scale_y_continuous(breaks=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80),
                     labels=c("0.10","0.20","0.30","0.40","0.50","0.60","0.70","0.80"))+
  theme(legend.title = element_blank(),
        legend.text = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.x=element_text(size=12),
        axis.title.y = element_text(size=15),
        axis.text.y=element_text(size=12))+
  theme_bw()
