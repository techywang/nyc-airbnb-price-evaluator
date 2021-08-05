setwd("C:/Users/fpsto/Documents/School/Fall_2019/Stat_ML/Project/new-york-city-airbnb-open-data")
nyc_data <- read.csv("AB_NYC_2019.csv", header = T)

# take out coloumns not used in model building
nyc_data_0 <- nyc_data[ ,- which(names(nyc_data) %in% c("latitude", "longitude", "id", "host_id", "host_name","reviews_per_month"))]
# remove price  < 0
nyc_cleaned <- nyc_data[which(nyc_data$price > 0),]
train = nyc_cleaned


# create formula
price_adds = paste("neighbourhood_group", "room_type", "minimum_nights", "number_of_reviews", "calculated_host_listings_count", "availability_365", sep = " + ")
price_form = paste ( "price", price_adds, sep = "~")
# build lm model
price_lm_model <- lm(data = train, formula = as.formula(price_form) )
summary(price_lm_model)
par(mfrow=c(2,2))
plot(price_lm_model)


# Improve model
###############
# Old model failed to satisfy aussumptions, 
# hope to transform y variable to create a better model.

# boxcox
bc <- boxcox(price_lm_model)
lambda <- bc$x[which.max(bc$y)]

# predict price  ^ lambda
# new model formula
new_price_form = paste ( "price^(lambda)", price_adds, sep = "~")
# new model build
new_model <- lm(formula = as.formula(new_price_form), data = train)
summary(new_model)
par(mfrow=c(2,2))
plot(new_model)
