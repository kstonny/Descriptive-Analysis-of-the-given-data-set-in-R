getwd()
rm(mar)
getwd()
# 1. Read the dataset into R.

exp = read.csv("expenses (1).csv")

# 2. How many observations and variables are there?

#Ans: 1338 obs. of 7 variables

# 3. What are the different types of variables?

colnames(exp)
# ans: "age"-Continuous 
#"sex"- Categorical "bmi"-Continuous "children"-Categorical 
#"smoker"- Categorical   "region" - Categorical   "charges" - Continuous/target

# 4. What values are contained in the variables?

str(exp)

# ans: age: Integer - Continuous 
#      Sex - Factor - Categorical
#      bmi - Number - Continuous
#      children: Integer - Categorical
#      smoker  : Factor - Categorical
#      region  : Factor - Categorical
#      charges : number - Continuous

# 5. For numeric data, generate some summary statistics.

summary(exp)

# 6. Which variables are skewed?

# Ans: Childern - slightly positive skewed
#       Charger - positive skewed.


# 7. Find the frequency distribution of gender in the dataset.
# Ans: Female - 662 and male - 676

# 8. Find how many smokers and non-smokers are there in the dataset.
# ans: Non - Smokers - 1064, Smokers - 274

# 9. Find the frequency distribution of different regions in the dataset.
# ans: northeast:324  
#      northwest:325
#      southeast:364
#      southeast:364

# 10. Cross tabulate the columns sex, smoker and region columns
table(exp$sex,exp$smoker,exp$region)

# 11. Check for missing NA values in different columns of the dataset.
sum(is.na(exp))
which(is.na(exp$bmi))

# 12. Create a boxplot to check for outliers in age and bmi columns.
boxplot(exp$age,exp$bmi)

# 13. At which positions are the outliers lying?

which(is.na(exp$bmi))
which(is.na(exp))

# 14. Visualize the data - univariate analysis - draw a histogram for bmi.
hist(exp$bmi)

# 15. Create a scatter plot for age and charges.

points(exp$age,exp$charges)

library(ggplot2)

ggplot(exp,aes(x=age,y=charges))+geom_point()

# 16. Do males generally have higher expenses than females?

plot(exp$sex,exp$charges)

#ans: Yes.

# 17. Is there any relationship between region and medical expenses?
install.packages("corrplot")
library(corrplot)
?cor
c = cor(exp$charges,exp$region)
c= cor(exp)
corrplot(c,method = 'number')
ggpairs(exp)

# 18. Do patients with less or no children have lower medical expenses?

table(exp$children,exp$charges)

plot(exp$children,exp$charges,col="Red")

#ans: patients with no children, one and three children.

# 19. Does smoking lead to higher medical expenses?

plot(exp$smoker,exp$charges)
#ans: Yes

# 20. What is the correlation between bmi and medical expenses?

cor(exp$bmi,exp$charges)

cor(exp)

#ans: No

# 21. Impute the outliers in age variable with 99th percentile.

boxplot(exp$age)

summary(exp$age)
#qqplot(exp$age,exp$bmi)

quantile(exp$age,p=(1:100)/100)
library(dplyr)
which(is.na(exp$age))
#which(nrow(exp$age>64))
exp$age<-ifelse(exp$age>64,64,exp$age)
boxplot(exp$age)

exp$age1 = ifelse(exp$age <= 80,1,0)
#age11 = exp(exp$age1==0)
#rm(age11)
which(exp$age1==0)
exp=exp[-(15:16),]
exp = select(exp,-age1)

# 22. Impute missing values in bmi variable with mean.
exp$bmi = as.numeric(exp$bmi)
tapply(exp$bmi,exp$smoker,mean)
imp<-which(is.na(exp$bmi))
mean(exp[imp,"Average"])
#rm(Imp)(
ind <- which(is.na(exp$bmi)) #<- mean(exp$bmi)
ind <- 30.67

which(is.na(exp$bmi))<- mean(exp$bmi)
summary(exp$bmi)
# 23. Transform qualitative data into quantitative data. Create dummy variables for Gender.

exp = exp%>%mutate(sex1 = ifelse(sex=="male",1,0))
exp=select(exp,-sex)

#24. Partition the data (70/30) into training and validation datasets.

library(caTools)
sample = sample.split(exp,SplitRatio = 0.7)
train <- subset(exp, sample == TRUE)
test <- subset(exp, sample == FALSE)

str(train)
