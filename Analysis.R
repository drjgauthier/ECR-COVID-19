library(rms)
fit<- glm(death~rcs(age,4),
          family="binomial",data=df)
summary(fit)
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
  # scale_y_continuous(breaks=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80),
  #                    labels=c("0.10","0.20","0.30","0.40","0.50","0.60","0.70","0.80"))+
  theme(legend.title = element_blank(),
        legend.text = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.x=element_text(size=12),
        axis.title.y = element_text(size=15),
        axis.text.y=element_text(size=12))+
  theme_bw()

