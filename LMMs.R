getwd()
setwd('C:/Users/user/Desktop/Collective_Intelligence_M2/Notes/Data science3/Week 7')
library(tidyverse)
library(broom)
library(lme4)
library(dplyr)
library(ggplot2)
install.packages("Matrix")
library(Matrix)

#load the data
?sleepstudy# check the dataset information in detail
data(sleepstudy)
head(sleepstudy)

#data structure & EDA
str(sleepstudy)

# plot
ggplot(sleepstudy, aes(Days, Reaction, group=Subject, colour=Subject)) +
  geom_point() + 
  facet_wrap(~Subject, ncol=6) + 
  scale_x_continuous(limits=c(0, 10),breaks=c(0,10)) +
  theme_minimal()

ggplot(sleepstudy, aes(Days, Reaction, group=Subject, colour=Subject)) +
  geom_point() + 
  facet_wrap(~Subject, ncol=6) + 
  scale_x_continuous(limits=c(0, 10),breaks=c(0,10)) +
  geom_smooth(method=lm)+
  theme_minimal()


ggplot(sleepstudy, aes(Days, Reaction, group=Subject, colour=Subject)) +
  geom_boxplot()+
  ggtitle("Boxplot")

####
summary(sleepstudy)
summary(sleepstudy$Reaction)
summary(sleepstudy$Days)
# Summarize
sleepstudy %>%
  group_by(Subject) %>%
  summarise(
    n = n(),
    mean = mean(Reaction),
    sd = sd(Reaction)
  )
#we can visualize the trend of reaction time for each particpant over days
ggplot(sleepstudy, aes(y=Reaction, x=Days, colour=Subject))+
  geom_point()+
  geom_line(aes(group=Subject))+
  scale_x_continuous(breaks=0:9)

## models let"s run 2 models 'mixed effect models as data is non independent'
# our first model will be that people reaction time differ from one to another and there is an effect of days in general
model1 <- lmer(Reaction ~ 1 + Days + (1 | Subject), sleepstudy)
summary(model1)
# let"s plot our first model using ggplot
ggplot(sleepstudy, aes(Days, Reaction, group=Subject, colour=Subject)) + 
  geom_point() + 
  geom_line(aes(y=fitted(model1))) + 
  facet_wrap(~Subject, ncol=9) + 
  scale_x_continuous(limits=c(0, 10),breaks=c(0,10)) +
  theme_minimal()

#second model with be that there is the effect of days but this effect differs from one participant to another 

model2= lmer(Reaction~Days + (1+Days|Subject),data=sleepstudy)
summary(model2)

#second model lets plot it and see

ggplot(sleepstudy, aes(Days, Reaction, group=Subject, colour=Subject)) + 
  geom_point() + 
  geom_line(aes(y=fitted(model2))) + 
  facet_wrap(~Subject, ncol=9) + 
  scale_x_continuous(limits=c(0, 10),breaks=c(0,10)) +
  theme_minimal()

#let's compare the 2 models using anova and see
anova(model1,model2)
# look at AIC and BIC

#residuals

prediction = fitted(model2)
res <- resid(model2)
plot(prediction, res)
#add a horizontal line at 0 
abline(0,0)

#create Q-Q plot for residuals
qqnorm(res)
#add a straight diagonal line to the plot
qqline(res) 
#Create density plot of residuals
plot(density(res))
