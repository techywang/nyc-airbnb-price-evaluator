setwd("C:/Users/fpsto/Documents/School/Fall_2019/Stat_ML/Project/new-york-city-airbnb-open-data")
nyc_data <- read.csv("AB_NYC_2019.csv", header = T)
head(nyc_data)

library(MASS) ##for lda
library(klaR) ##for partimat
library(boot)
library(ipred) # for cross validation of lda and qda

# a. A description of any data cleaning / manipulation that needed to be performed in order
# to build your models.

# didnt do anything to the orginal dataset to clean



# b. Explain which predictors your group decided to include in LDA and QDA. You should
# use the same predictors for both models.
predictors = paste("price", "minimum_nights", "number_of_reviews", sep = " + ")
formula = paste ( "room_type", predictors, sep = "~")

# c. Compare the performance of both LDA and QDA, using the estimated test error rate
# using k fold cross-validation with k = 5 and k = 10. (please see the document for Lab
#5). What do these results tell you about the nature of the decision boundaries?
# five-fold cross validation

cv.da <- function(object, newdata)
{
  return(predict(object, newdata = newdata)$class)
}

set.seed(5)
errorest(as.formula(formula), data=nyc_data,
         model=lda, estimator="cv", est.para=control.errorest(k=5),
         predict=cv.da)$err
# [1] 0.2404336
errorest(as.formula(formula), data=nyc_data,
         model=lda, estimator="cv", est.para=control.errorest(k=10),
         predict=cv.da)$err
# [1] 0.2393496
errorest(as.formula(formula), data=nyc_data,
         model=qda, estimator="cv", est.para=control.errorest(k=5),
         predict=cv.da)$err
# [1] 0.4965743
errorest(as.formula(formula), data=nyc_data,
         model=qda, estimator="cv", est.para=control.errorest(k=10),
         predict=cv.da)$err
# [1] 0.4979855



 # i. A discussion about the output from the lda() function for LDA. Address what the
#coefficients of linear discriminants are telling you.
  
# lda 
lda.model <- lda(as.formula(formula), nyc_data)
my_file <- file("lda_model_output.txt")
sink(my_file)
lda.model
sink()
# ii. Create a histogram of the scores from the first linear discriminant for each class of
# the response variable (please see the document for Lab 4b). Comment on whether
# the first linear discriminant separates the classes well.
ldahist(data = predict(lda.model)$x[,1], g=nyc_data$room_type, xlim = c(-1, 1), h = .1)

# iii. Create some partition plots to display the decision boundaries for some of the predictors. Comment on what these partition plots are telling you.

partimat(as.formula(formula), nplots.vert=1, nplots.hor=1, data=nyc_data, method="lda")

#e. Using the whole data set, create some partition plots to display the decision boundaries
#for some of the predictors with QDA. Comment on what these partition plots are telling
#you.

partimat(as.formula(formula), nplots.vert=1, nplots.hor=1, data=nyc_data, method="qda")









