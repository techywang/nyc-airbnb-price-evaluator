# Stats 4630 project

# set working directory
# getwd()
# setwd("/Users/ivanziyue/Desktop/stat_4630/project")
# import library
library("dplyr")
library("corrplot")
library(ISLR)
library(MASS)
library(klaR)
library(ICS)


# Milestone 2 - Defining Objectives
# exploratory analysis with ny airbnb data set
nyc_airbnb <- read.csv("AB_NYC_2019.csv", header = TRUE)
head(nyc_airbnb)
summary(nyc_airbnb)

# graphical summary Figure 1
library(ggplot2)
my_plot <- ggplot(data = nyc_airbnb[nyc_airbnb$price < 700,], aes(price, fill = room_type)) + geom_histogram(alpha = .2, bins = 10) + labs(title = "Room Type Frequencies at\n Different Price Levels" ) + xlab(label = "Price") + ylab(label = "Count") 
my_plot + scale_fill_discrete(name = "Room Type", labels = c("Entire Home or Apartment", "Private Room", "Shared Room"))

# graphical summary Figure 2 
less70 <- nyc_airbnb[nyc_airbnb$price < 700,]

ggplot(less70, aes(x=neighbourhood_group, y=price)) +
  geom_boxplot() + 
  labs(title="Prices of Airbnbs in NYC Neighborhoods",
       x="Neighborhood", y="Price")

# select out only the numeric part of the data
nyc_number <- na.omit(select_if(nyc_airbnb, is.numeric))
head(nyc_number)
# Rename columns in R
colnames(nyc_number)[colnames(nyc_number)=="calculated_host_listings_count"] <- "total_listings"
colnames(nyc_number)[colnames(nyc_number)=="minimum_nights"] <- "min_nights"
colnames(nyc_number)[colnames(nyc_number)=="number_of_reviews"] <- "#_reviews"
colnames(nyc_number)[colnames(nyc_number)=="availability_365"] <- "availability/yr"
# remove unnecessary columns
nyc_correl <- nyc_number[,c("price", "min_nights", "#_reviews", 
                    "total_listings", "availability/yr")]
# explore correlation of the quantitative data
corr <- cor(nyc_correl)
corr
# visualization
nyc_nummat <- as.matrix(as.data.frame(nyc_correl))
corrplot(corr, type = "upper", method = "ellipse")

# Milestone 3 - Regression Analysis
# data cleansing
head(nyc_airbnb)
# select only the important column
nyc_new <- nyc_airbnb[,c("neighbourhood_group","room_type","price",
                       "minimum_nights","number_of_reviews",
                       "availability_365","calculated_host_listings_count")]
# drop shared room
nyc_cleaned <- nyc_new[which(nyc_new$room_type == "Entire home/apt" 
                 | nyc_new$room_type == "Private room"),]
head(nyc_cleaned)
summary(nyc_cleaned$room_type)
# make neighbourhood_group and room_type categorical factors
nyc_cleaned$neighbourhood_group <- as.factor(nyc_cleaned$neighbourhood_group)
nyc_cleaned$room_type <- as.factor(nyc_cleaned$room_type)
head(nyc_cleaned)
# unselect price of zero value
nyc_cleaned <- nyc_cleaned[which(nyc_cleaned$price > 0 & nyc_cleaned$price < 700),]
# initial linear regression model
result_cleaned <- lm(price~neighbourhood_group+room_type+minimum_nights+number_of_reviews
                     +calculated_host_listings_count+availability_365, data=nyc_cleaned)
summary(result_cleaned)
# first regression diagnostics 
par(mfrow=c(2,2))
plot(result_cleaned)
# see possible transformation
library(MASS)
bc <- boxcox(result_cleaned)
lambda <- bc$x[which.max(bc$y)]
lambda
# drop staten island and minimum nights stayed to improve model
nyc_reduced <- nyc_cleaned[which(nyc_cleaned$neighbourhood_group != "Staten Island"),
                         -which(colnames(nyc_cleaned) == "minimum_nights")]
# transform price with exponent lambda using the boxcox transformation
nyc_reduced$price_lambda <- nyc_reduced$price**lambda
nyc_reduced <- nyc_reduced[!is.na(nyc_reduced$price_lambda),]
head(nyc_reduced)
# fit the second linear regression model
result_reduced <- lm(price_lambda~neighbourhood_group+room_type+number_of_reviews
                          +calculated_host_listings_count+availability_365, data=nyc_reduced)
summary(result_reduced)
# second regression diagnostics
par(mfrow=c(2,2))
plot(result_reduced)




install.packages("ROCR")
library(ROCR)
library(gplots)
################################
##Milestone 4 - Classification##
################################
set.seed(100)
##logistic regressions
nyc_reduced$room_type <- as.factor(nyc_reduced$room_type)
nyc_reduced$room_type<-droplevels(nyc_reduced$room_type)
summary(nyc_reduced$room_type)

sample.data<-sample.int(nrow(nyc_reduced), floor(.5*nrow(nyc_reduced)), replace = F)
train<-nyc_reduced[sample.data, ]
test<-nyc_reduced[-sample.data, ]

logistic_train<-glm(room_type~price_lambda+number_of_reviews
                    +calculated_host_listings_count+availability_365, data=train, 
                    famil=binomial) 
summary(logistic_train)
preds<-predict(logistic_train,newdata=test, type="response")
type<-prediction(preds, test$room_type)
roc_result<-performance(type,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve for Logistic Regression")
lines(x = c(0,1), y = c(0,1), col="red")
#auc
auc<-performance(type, measure = "auc")
auc

library(MASS)
library(klaR)
library(ROCR)
#lda
lda.train <- lda(room_type~price_lambda+number_of_reviews
                 +calculated_host_listings_count+availability_365, data=train)
lda.train
lda.test <- predict(lda.train,test)
preds<-lda.test$posterior[,2]
types<-prediction(preds, test$room_type)
roc_result<-performance(types,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve for LDA")
lines(x = c(0,1), y = c(0,1), col="red")

auc<-performance(types, measure = "auc")
auc

##logistic regressions with categorical variable
logistic_train<-glm(room_type~price_lambda+neighbourhood_group+number_of_reviews
                     +calculated_host_listings_count+availability_365, data=train, 
                     famil=binomial) 
summary(logistic_train)
preds<-predict(logistic_train,newdata=test, type="response")
type<-prediction(preds, test$room_type)
roc_result<-performance(type,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC for Logistic Regression with neighbourhood_group")
lines(x = c(0,1), y = c(0,1), col="red")
#auc
auc<-performance(type, measure = "auc")
auc


#k-fold 
reps<-10
d<-10
cv.error.k<-matrix(0,nrow=reps, ncol=d) ##store the k fold MSEs
deg<-c(0,reps) ##store the degree which gives lowest k fold MSE for each rep

for (i in 1:reps)
{

  for (j in 1:d)
    
  {
    glm.fit<-glm(room_type~poly(price_lamda+neighbourhood_group+number_of_reviews
                                +calculated_host_listings_count+availability_365,j),data=nyc_cleaned)
    cv.error.k[i,j]<-cv.glm(room_type,glm.fit, K=10)$delta[1] 
  }
  deg[i]<-which.min(cv.error.k[i,])
  
}
