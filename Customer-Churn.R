# Customer Churn Analysis
# Data Source: https://community.ibm.com/community/user/ibmcommunity/home

getwd()

install.packages("corrplot")
install.packages("ggthemes")
install.packages("party")

# Loading the packages required
require(plyr)
require(corrplot)
require(ggplot2)
require(gridExtra)
require(ggthemes)
require(caret)
require(MASS)
require(randomForest)
require(party)

## Loading and clean the data ##
churn <- read.csv('../Cap06/Telco-Customer-Churn.csv')
View(churn)
# the target attribute is churn column
str(churn)

# using the sapply to determinate the missing values in each column
sapply(churn, function(x) sum(is.na(x)))
# we have 11 missing values in column Total Charges, let remove all rows with missing value
?complete.cases
churn <- churn[complete.cases(churn),]

# Changing the answer "No internet service" to "No"
str(churn)
# Will change the columns from 10 to 15.
cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[, cols_recode1][,i] <- as.factor(mapvalues(churn[,cols_recode1][,i], c("No internet service"), c("No")))
}

# Changing the answer "No phone service" to "No"
# Will change the column number 8
?as.factor
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, c("No phone service"), c("No")))

# Considering the minimum contract period is 1 month and maximum is 72 months, we can group in 5 tenures:
# “0-12 Month”, “12–24 Month”, “24–48 Month”, “48–60 Month”,“> 60 Month”
min(churn$tenure); max(churn$tenure)

group_tenure <- function(tenure) {
  if(tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

?sapply
# sapply: Apply a Function over a List or Vector
churn$ternure_group <- sapply(churn$tenure, group_tenure)
churn$ternure_group <- as.factor(churn$ternure_group)

# changing the column SeniorCitizen from 0/1 to No/Yes
churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen, c("0","1"), c("No","Yes")))

# Removing columns we wont use to this analysis
churn$customerID <- NULL
churn$tenure <- NULL

View(churn)

## Explorate Analysis and source selection ##

# Understanding the correlation between numerical variables
numeric.var <- sapply(churn, is.numeric)
numeric.var

corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, method="number", main="\n\ncorrelation between numerical variables")
# Monthly and TotalCharge have strong correlation, to prevent overfitting lets remove TotalCharges
churn$TotalCharges <- NULL

# Plotting bars graph to categoricals variables
?theme_minimal
p1 <- ggplot(churn, aes(gender)) + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5) + xlab("Gender") + ylab("Percentual") +ggtitle("Gender") + coord_flip() + theme_light()
p2 <- ggplot(churn, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p3 <- ggplot(churn, aes(x=Partner)) + ggtitle("Partner") + xlab("Parceiros") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p4 <- ggplot(churn, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependentes") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3,p4, ncol=2)

p5 <- ggplot(churn, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Telefonia") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p6 <- ggplot(churn, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Múltiplas Linhas") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p7 <- ggplot(churn, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p8 <- ggplot(churn, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
grid.arrange(p5, p6, p7, p8, ncol=2)

p9 <- ggplot(churn, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p10 <- ggplot(churn, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p11 <- ggplot(churn, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p12 <- ggplot(churn, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
grid.arrange(p9, p10, p11, p12, ncol=2)


p13 <- ggplot(churn, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p14 <- ggplot(churn, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p15 <- ggplot(churn, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p16 <- ggplot(churn, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p17 <- ggplot(churn, aes(x=ternure_group)) + ggtitle("Tenure Group") + xlab("Tenure Group") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p17
grid.arrange(p13, p14, p15, p16, p17, ncol=2)

# Will keep all the categorical variables to analysis, since they all have wild distribution.

## Predict Model ##
# Linear Regression

# Split the data to trainning (70%) and test(30%)
?createDataPartition
intrain <- createDataPartition(churn$Churn, p=0.7, list=FALSE)
# .Random.seed is an integer vector, containing the random number generator (RNG) state for random number generation in R. It can be saved and restored, but should not be altered by the user.
set.seed(2017)
# Generating two dataframs from intrain matrix. Testing and Trainning data frames.
trainning <- churn[intrain,]
testing <- churn[-intrain,]

# Checking if the split is correct
dim(testing)/dim(churn); dim(trainning)/dim(churn)

# Trainning the linear regression model
# Fit Model
?glm
LogModel <- glm(Churn ~ ., family=binomial(link="logit"), data=trainning)
print(summary(LogModel))

str(trainning)

# Analise de Variancia - ANOVA, to check the attributes more relevant to discovery the customer churn
?anova
anova(LogModel, test="Chisq")

View(churn)

# Three main resources are: Contract, ternure_group and PaperlessBilling














