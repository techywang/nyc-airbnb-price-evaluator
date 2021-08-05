# Stats 4630 project
# set working directory
getwd()
setwd("/Users/ivanziyue/Desktop/stat_4630/project")
# import library
install.packages('dplyr')
install.packages('corrplot')
install.packages('ISLR')
library("dplyr")
library("corrplot")
library(ISLR)

# Milestone 2 - Defining Objectives
# exploratory analysis with ny airbnb data set
nyc_airbnb <- read.csv("NYC_2019.csv", header = TRUE)
head(nyc_airbnb)
summary(nyc_airbnb)
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

# Milestone 4 - Classification
library(plyr)
library(ggplot2)
head(nyc_reduced)
# EDA - Bar Plot
ggplot(nyc_reduced, aes(x=room_type, y=price)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()
# EDA - Histogram
# for price
price_histo <- ggplot(nyc_reduced, aes(x=price, color=room_type)) +
  geom_histogram(fill="white", alpha=0.5, position="dodge") +
  theme_minimal()
mu_price <- ddply(nyc_reduced, "room_type", summarise, grp.mean=mean(price))
head(mu_price)
price_histo + geom_vline(data=mu_price, aes(xintercept=grp.mean, color=room_type),
                         linetype="dashed")
# for price lambda
price_lambda_histo <- ggplot(nyc_reduced, aes(x=price_lambda, color=room_type)) +
  geom_histogram(fill="white", alpha=0.5, position="dodge") +
  theme_minimal()
mu_price_lambda <- ddply(nyc_reduced, "room_type", summarise, grp.mean=mean(price_lambda))
head(mu_price_lambda)
price_lambda_histo + geom_vline(data=mu_price_lambda, aes(xintercept=grp.mean, color=room_type),
                         linetype="dashed")

# Modeling
# Logistic Regression
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
#k-fold 
result<-glm(room_type~price_lambda+number_of_reviews
            +calculated_host_listings_count+availability_365, data=nyc_reduced, 
            famil=binomial)
five.fold<-cv.glm(nyc_reduced, result, K=5)
five.fold$delta
ten.fold<-cv.glm(nyc_reduced, result, K=10)
ten.fold$delta

# import packages
library(MASS)
install.packages('klaR')
install.packages('ROCR')
install.packages('ipred')
library(klaR)
library(ROCR)
library(boot)
library(ipred)
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

# k-fold
cv.da <- function(object, newdata)
{
  return(predict(object, newdata = newdata)$class)
}
set.seed(5)
errorest(room_type~price_lambda+number_of_reviews
         +calculated_host_listings_count+availability_365, data=nyc_reduced,
         model=lda, estimator="cv", est.para=control.errorest(k=5),
         predict=cv.da)$err

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
# k-fold
result.new<-glm(room_type~price_lambda+neighbourhood_group+number_of_reviews
            +calculated_host_listings_count+availability_365, data=nyc_reduced, 
            famil=binomial)
five.fold.new<-cv.glm(nyc_reduced, result.new, K=5)
five.fold.new$delta
ten.fold.new<-cv.glm(nyc_reduced, result.new, K=10)
ten.fold.new$delta

# Milestone 6 - Trees
library(tree)
library(gbm)
library(randomForest)
ols <- lm(data = train, formula = price ~ . - price_lambda)
pred_ols <- predict(ols, newdata = test)
mse_ols <- mean((pred_ols - test$price)^2)
mse_ols# 6261.216

# Create Regression Tree
regression_tree <- tree(data = train, formula = price ~ . - price_lambda)
par(mfrow=c(1,1))
plot(regression_tree)
text(regression_tree, cex = .7, pretty = 0)
# Summarize result
summary(regression_tree)
# MSE of the reg tree
pred_tre <- predict(regression_tree, newdata = test)
mse_tre <- mean((pred_tre - test$price)^2)
mse_tre# 6352.634
# Find optimal size
set.seed(1)
cv.tre <- cv.tree(regression_tree, FUN=prune.tree)
trees.num.class<-cv.tre$size[which.min(cv.tre$dev)]
trees.num.class# 5 is the best size
# Prune the tree
prune_tre <- prune.tree(regression_tree, best=trees.num.class)
plot(prune_tre)
text(prune_tre, cex = .7)
# test mse of the pruned tree
pred_prune <- predict(prune_tre, newdata = test)
mse_prune <- mean((pred_prune - test$price)^2)
mse_prune# 6352.634
# Bagging
baggin <- randomForest(price ~ . - price_lambda, data=train, mtry=5, importance=TRUE)
baggin
pred_baggin <- predict(baggin, newdata = test)
bag_mse <- mean((pred_baggin - test$price)^2)
bag_mse # 6324.556
importance(baggin)
# Random Forest
rndm_forest <- randomForest(price ~ . - price_lambda, data = train, mtry = 3, importance = T )
rndm_forest
pred_rf <- predict(rndm_forest, newdata = test)
rf_mse <- mean((pred_rf - test$price)^2)
rf_mse # 5913.586
importance(rndm_forest)

##classification tree 
summary(nyc_reduced)
tree.class.train<-tree(room_type~price+neighbourhood_group+number_of_reviews+availability_365+calculated_host_listings_count, data=train)
summary(tree.class.train)
#graphic
jpeg("tree.jpg")
plot(tree.class.train)
text(tree.class.train, cex=0.75, pretty=0)
dev.off()
#prune
set.seed(100)
cv.class<-cv.tree(tree.class.train, FUN=prune.misclass)
cv.class
plot(cv.class$size, cv.class$dev,type='b')
##optimal tree size
trees.num.class<-cv.class$size[which.min(cv.class$dev)]
trees.num.class
prune.class<-prune.misclass(tree.class.train, best=trees.num.class)
jpeg("prune.jpg")
plot(prune.class)
text(prune.class, cex=0.75, pretty=0)
dev.off()
#bagging
bag.class<-randomForest(room_type~price+neighbourhood_group+number_of_reviews+availability_365+calculated_host_listings_count, data=train, mtry=5, importance=TRUE)
bag.class
##check which predictors are important
importance(bag.class)
jpeg("importance.jpg")
varImpPlot(bag.class)
dev.off()
## test error rate
y.test<-test[,"room_type"]
pred.bag<-predict(bag.class, newdata=test)
1-mean(y.test==pred.bag)
#random forests:
rf.class<-randomForest(room_type~price+neighbourhood_group+number_of_reviews+availability_365+calculated_host_listings_count, data=train, mtry=3, importance=TRUE)
rf.class
##important predictors
importance(rf.class)
jpeg("importance2.jpg")
varImpPlot(rf.class)
dev.off()
##test accuracy with Random Forest
pred.rf<-predict(rf.class, newdata=test)
1-mean(y.test==pred.rf)
#with 3 predictors
rf.class<-randomForest(room_type~price+neighbourhood_group+number_of_reviews+availability_365+calculated_host_listings_count, data=train, mtry=3, importance=TRUE)
rf.class
pred.rf<-predict(rf.class, newdata=test)
1-mean(y.test==pred.rf)
##according to the test error rate, using 3 predictors is better 






