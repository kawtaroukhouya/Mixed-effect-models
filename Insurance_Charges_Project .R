set.seed(123)
library(tidyverse)
library(Hmisc)
library(ggplot2)
library(plotrix)
library("corrplot")

# Read in the data
setwd("/Users/ghizlanerehioui/Desktop/Project ED")
insurance <- read.csv("insurance.csv")

######################################################################
' DATASET'
###################################################################### 
head(insurance)
View(insurance)
summary(insurance)
describe(insurance)

# insurance$smoker to binary
insurance <- insurance %>% mutate(smoker_bin = as.factor(ifelse(smoker == 'yes', 1, 0)))

# Categorize based on bmi
insurance$bmi_cat <- as.factor(ifelse(insurance$bmi >= 30, 1, ifelse(insurance$bmi <= 18.5, -1,0)))

# Center the bmi
insurance$bmi_centered <- insurance$bmi - mean(insurance$bmi) # mean of our sample, but realistically, it does not make sense because our sample might have too many outliers or skewed, so we might consider a better alternative which is the bmi category.

# Gender as factor
table(insurance$gender)
insurance$gender_fct <- as.factor(ifelse(insurance$gender == 'female', 0, 1))

# Center age
insurance$age_centered <- insurance$age - 18

insurance$region_fcts <- as.factor(ifelse(insurance$region == 'northeast', 1, ifelse(insurance$region == 'northwest', 2,ifelse(insurance$region == 'southeast', 3,4))))

# UNITS

######################################################################
' EDA'
###################################################################### 
# correlation matrix
corr_mtrx <- rcorr(as.matrix(insurance[! names(insurance) %in% c('gender','age_centered','smoker','region', 'bmi_cat', 'bmi_centered')]))
corr_mtrx$r >= 0.9
# No multicollinearity between our 
corrplot(corr_mtrx$r, tl.col ="black", 
         title = 'Correlation Matrix')

# plots
hist(insurance$charges) # ADD PART 1 EDA
boxplot(insurance$charges)

gender_dist <- table(insurance$gender)
piepercent <- round(100*gender_dist/sum(gender_dist), 1)
pie3D(gender_dist, labels = piepercent, explode = 0.1, main = "Gender pie chart")  
hist(as.numeric(insurance$smoker_bin))
hist(insurance$children)
hist(insurance$age)
region_dist <- table(insurance$region)
piepercent <- round(100*region_dist/sum(region_dist), 1)
pie3D(region_dist, labels = piepercent, explode = 0.1, main = "Region pie chart") # color regions names
hist(insurance$bmi)  # ADD PART 1 EDA

# ADD PART 1 EDA
# pie charts of gender, smokers, regions

# ADD PART 1 EDA
# scatter plot of bmi vs. charges

# ADD PART 2 EDA
# Split females vs. males charges
# maybe we can split based on gender and have as independent vars. smoking, bmi (may be confounds due to gender differences) 
# Box plot 
ggplot(insurance, aes(x=smoker, y=charges, color=gender)) + geom_boxplot() + ggtitle("Boxplot of charges by gender and smoker") # ADD PART 2 EDA

ggplot(insurance, aes(x=smoker, y=charges, color=region)) + geom_boxplot() + ggtitle("Boxplot of charges by gender and region") # ADD PART 2 EDA

ggplot(insurance, aes(x=smoker, y=charges, color=bmi_cat)) + geom_boxplot() + ggtitle("Boxplot of charges by bmi category and smoker") # ADD PART 2 EDA


# Scatter plots
ggplot(insurance, aes(x=age, y=charges, color= smoker)) + geom_point() + ggtitle("Scatterplot of charges by smoker and age") # ADD PART 2
table(insurance$bmi_cat)

# what does this graph suggest?? --> for every age, we have many ppl with +1, some with 0 and very few with -1
ggplot(insurance, aes(x=age_centered, y=bmi_centered, color=bmi_cat)) + geom_point() + ggtitle("Scatterplot of charges by bmi category and smoker")

# ADD PART 2 EDA 
ggplot(insurance, aes(x=bmi, y=charges, color=smoker)) + geom_point() + ggtitle("Scatterplot of charges by bmi and smoker") # add line splits for bmi ranges


# Split females vs. males charges
table(insurance$gender)
# Split smoker vs. non-smoker charges
table(insurance$smoker)
# Split charges for regions
table(insurance$region)
# Split charges for regions
table(insurance$age)
# Split charges for regions
table(insurance$children)

table(insurance$bmi_cat,insurance$age)

# Split females vs. males charges for smoker vs. non-smoker
table(insurance$gender, insurance$smoker)
# Split regions' charges for smoker vs. non-smoker
table(insurance$region, insurance$smoker)

# on average males, pay more: 
insurance %>% group_by(gender) %>% summarise(avg = mean(charges))

# group by bmi and gender for average charges
insurance %>% group_by(bmi_cat) %>% summarise(avg = mean(charges))
insurance %>% group_by(bmi_cat,gender) %>% summarise(avg = mean(charges))

table(insurance$region, insurance$smoker)

table(insurance$bmi_cat, insurance$smoker)
insurance %>% group_by(bmi_cat,smoker) %>% summarise(avg = mean(charges))

insurance %>% group_by(bmi_cat,children) %>% summarise(avg = mean(charges))

insurance %>% group_by(smoker, gender) %>% summarise(avg = mean(bmi))

######################################################################
' A LM WITH ALL VARIABLES WITHOUT INTERACTIONS'
######################################################################
# Linear model with all variables without interactions
ml_no_int <- glm(insurance$charges ~ insurance$children + insurance$smoker + insurance$gender + insurance$bmi_centered + insurance$age_centered + insurance$region)
summary(ml_no_int)
# A childless female of age 18 with a average bmi, doesn't smoke, and lives in the northeast, pays 3085.7 dollars
# A childless male of age 18 with an average bmi, doesn't smoke, and lives in the northeast, pays 3085.7 + -131.3 dollars: 

# ANOTHER ATTEMPT WITH ONLY 3 HIGHLY CORRELATED VARS TO charges
ml_no_int <- glm(insurance$charges ~ insurance$smoker + insurance$bmi_centered + insurance$age_centered)
summary(ml_no_int)

######### BMI
# Representing charges as a polynomial of bmi 
ply_chr_bmi<- glm(insurance$charges ~ insurance$bmi_centered 
                  + I(insurance$bmi_centered^2))
summary(ply_chr_bmi)
# We plot the charges to see if they fit as a polynomial function of the bmi 
ggplot(insurance, aes(x=bmi_centered, y=charges))+
  geom_point()+
  geom_smooth(method=lm, formula = y ~ poly(x,2))+
  theme_classic()+
  ggtitle("Charges as a polynomial of the centered bmi")

ggplot(insurance, aes(x=bmi, y=charges))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()
# The charges is not well represented with a polynomial function nor a linear function of the bmi 

# Plotting
# We see that smokers with a high bmi ..........
ggplot(insurance, aes(x = bmi_centered, y = charges, color = smoker)) +
  geom_point() +
  labs(title = "Charges by centered BMI and smoker", x = "centered bmi", y = "Charges", color = "smoker") +
  geom_smooth(method="lm")+
  theme_minimal()

####### here we can conclude that it is possible to model this as a linear regression (maybe with interaction with smoker)
'MODEEEEEEEL'

######### AGE AS A POLYNOMIAL
# Representing charges as a polynomial of age 
ply_chr_age<- glm(insurance$charges ~ insurance$age_centered + I(insurance$age_centered^2))
summary(ply_chr_age)
# We plot the charges to see if they fit as a polynomial function of the age 
ggplot(insurance, aes(x=age_centered, y=charges))+
  geom_point()+
  geom_smooth(method=lm, formula = y ~ poly(x,2))+
  theme_classic()

############ AGE
# Charges increase by age 
# Charges increase by age and is more volatile for overweight 
ggplot(insurance, aes(x = age, y = charges, color = bmi_cat)) +
  geom_point() +
  labs(title = "Charges by BMI category and Age", x = "Age", y = "Charges", color = "BMI category") +
  theme_minimal()


# CHARGES - AGE 
# charges by increase by age
# charges increase by age even more for smokers
ggplot(insurance, aes(x = age, y = charges, color = smoker)) +
  geom_point() +
  labs(title = "Charges by smoker and Age", x = "Age", y = "Charges", color = "Smoker") +
  geom_smooth(method="lm")+
  theme_minimal()

####### here we can conclude that it is possible to model this as a linear regression (maybe with interaction with smoker)
'MODEEEEEEEL'

######## Log regression
lm_log_all <- lm(log(insurance$charges) ~ insurance$children + insurance$smoker_bin + as.factor(insurance$gender) + insurance$bmi + insurance$age + as.factor(insurance$region))
summary(lm_log_all)

######## Log regression with only most corre.
lm_log_all <- lm(log(insurance$charges) ~ insurance$smoker_bin + insurance$bmi_centered + insurance$age_centered)
summary(lm_log_all)

ggplot(insurance, aes(x = bmi_centered, y = log(charges), color = smoker)) +
  geom_point() +
  labs(title = "Charges by smoker and Age", color = "Smoker") +
  geom_smooth(method="lm")+
  theme_minimal()

# Other variables to control for:
# salary
# type of job and job risk
# cons.price.indx or living expenses in the region



























######################################################################
######################################################################
######################################################################
######################################################################
' OLD STUFF'
######################################################################
######################################################################
######################################################################
v1 <- insurance %>% group_by(children) %>% summarise(avg = mean(charges))
vect1<- v1$avg*table(insurance$children)/sum(table(insurance$children))
######################################################################
' SIMPLE LINEAR REGRESSION MODELS OF THE INDIVIDUAL VARABIABLES'
######################################################################s
glm(insurance$charges ~ insurance$children * insurance$smoker_bin * as.factor(insurance$gender) * insurance$bmi * insurance$age * as.factor(insurance$region), family=binomial(link='logit'))
glm(insurance$charges ~ insurance$children + insurance$smoker_bin + as.factor(insurance$gender) + insurance$bmi + insurance$age + as.factor(insurance$region), family=binomial(link='logit'))

summary(lm(log(insurance$charges) ~ insurance$children * insurance$smoker_bin * as.factor(insurance$gender) * insurance$bmi * insurance$age * as.factor(insurance$region)))


# Effect of children on charges
ml_ch <- lm(insurance$charges ~ insurance$children)
summary(ml_ch)
# p-value: 0.01285 --> There is a sig rel. btw charges and number of children
# 12522.5 dollars is the average estimated charge for a person with 0 children
# 683.1 dollars is the estimated average increase in charge for each additional child

# Effect of smoking on charges
ml_sm <- lm(insurance$charges ~ insurance$smoker_bin)
summary(ml_sm)
# p-value: < 2.2e-16 --> There is a very sig rel. btw charges and smoking: smoker is riskier
# 8434.3 dollars is the estimated average charge for a non-smoker
# 23616.0 dollars is the estimated average increase in charge for a smoker

# Effect of gender on charges
'MAKE BINARY'
ml_gr <- lm(insurance$charges ~ insurance$gender)
summary(ml_gr)
# p-value: 0.03613 --> There is a less sig rel. btw charges and gender: possibly related to number of children
# 12569.6 dollars is the estimated average charge for a female 
# 12569.6 dollars is the estimated average increase in charge for a male
# NOT INTUIIVE BUT IF WE COMBINE I WITH SMOKING, IT MAKES SENSE SINCE MEN SMOKE MORE

# Effect of bmi on charges
ml_bm_cent <- lm(insurance$charges ~ insurance$bmi_centered)
summary(ml_bm_cent)

# Effect of age on charges
ml_ag <- lm(insurance$charges ~ insurance$age)
summary(ml_ag)

#confounds, causal rel. btw variables, what variables causes another (DIAGRAM)

######################################################################
' SIMPLE LINEAR REGRESSION MODELS OF ALL INDIVIDUAL VARABIABLES'
######################################################################
# Effect of children and smoking on charges without interactions
ml_no_int <- lm(insurance$charges ~ insurance$children + insurance$smoker + insurance$gender + insurance$bmi + insurance$age + insurance$region)
summary(ml_no_int)

ml_no_int <- lm(insurance$charges ~ insurance$children + insurance$smoker + insurance$gender + insurance$bmi_centered + insurance$age_centered + insurance$region)
summary(ml_no_int)

# Since gender is a confound variable that can impact bmi, smoking: we will be spliting data and model based on gender
female <- insurance[which(insurance$gender=="female"),]
male <- insurance[which(insurance$gender=="male"),]

ml_no_int_fem <- lm(female$charges ~ female$children + female$smoker_bin + female$bmi_centered + female$age_centered + female$region)
summary(ml_no_int_fem)

ml_no_int_mal <- lm(male$charges ~ male$children + male$smoker_bin + male$bmi_centered + male$age_centered + male$region)
summary(ml_no_int_mal)



######################################################################
' SIMPLE LINEAR REGRESSION MODELS OF THE INDIVIDUAL VARABIABLES'
######################################################################
# Effect of children and smoking on charges without interactions
ml_no_int <- lm(insurance$charges ~ insurance$children + insurance$smoker + insurance$gender)
summary(ml_no_int)
# Without interactions, the p-value: < 2.2e-16, showing a very big significance btw charges and the two variables.

# ?????
confint(ml_no_int, level = 0.97)
insurance <- cbind(insurance,predict(ml_no_int,interval = 'confidence'))
ggplot(insurance,aes(x=as.numeric(insurance$children),y=as.numeric(insurance$charges),color=as.factor(insurance$smoker)))+
  geom_point()+
  geom_line(aes(y=fit))+
  geom_ribbon(aes(ymin=lwr,ymax=upr,fill=as.factor(insurance$smoker)),alpha=0.2,color=NA)+
  theme_classic()+
  theme(plot.title= element_text(hjust=0.5))+
  scale_color_manual(values = c('#D9BF02','#AFD902'))+
  scale_fill_manual(values = c('#D9BF02','#AFD902'))+
  labs(x= 'children',
       y= 'charges',
       title = 'effect of number of children on insurance charges')+
  geom_vline(aes(xintercept=0))

######################################################################
' '
######################################################################
# Effect of children and smoking on charges with interactions
ml_int <- lm(insurance$charges ~ insurance$children * insurance$smoker * insurance$gender)
summary(ml_int)




#####
#####
insurance$bmi_stdz <- scale(insurance$bmi)


insurance$bmi_log<- log(insurance$bmi)
ggplot(insurance, aes(x=bmi_log, y=charges))+
  geom_point()+
  geom_smooth(method=lm, formula = y ~ poly(x,2))+
  theme_classic()


# , color=as.factor(gender)
female <- insurance[which(insurance$gender=="female"),]
male <- insurance[which(insurance$gender=="male"),]

ggplot(female, aes(x=bmi, y=charges))+
  geom_point()+
  geom_smooth(method=lm, formula = y ~ poly(x,2))+
  theme_classic()

ggplot(male, aes(x=bmi, y=charges))+
  geom_point()+
  geom_smooth(method=lm, formula = y ~ poly(x,2))+
  theme_classic()



#
ggplot(insurance, aes(x = age, y = charges, color = children)) +
  geom_point() +
  labs(title = "Charges by children and Age", x = "Age", y = "Charges", color = "Children") +
  theme_minimal()



#####
#####
######################################################################
' Extra: Correlation between variables'
######################################################################
# Check for Multicolinearity


######################################################################
' Extra: special cases'
######################################################################
# What if the person is a smoker but they have healthy habits, diet, and lifesyle?

# What if the person is a smoker but they have healthy habits, diet, and lifesyle?



