library(dplyr)

source("utils/cleaning.r")
source("utils/features.r")


train_data <- read.csv("data/train.csv")
test_data <- read.csv("data/test.csv")

train_data <- clean(train_data)
train_data <- features(train_data)
test_data <- clean(test_data)
test_data <- features(test_data)

# Split into training and validation set
validation_set <- filter(train_data, Date < as.Date("2008-1-1"))
train_set <- filter(train_data, Date >= as.Date("2008-1-1"))

# Prediction model based on just species. Return exactly the overall prevalence based on that species.

s_fit <- glm(WnvPresent ~ Species, train_set, family=binomial())
s_predictions <- predict(s_fit, validation_set, type="response")
print(auc(validation_set$WnvPresent, s_predictions))

sm_fit <- glm(WnvPresent ~ Species + Month, train_set, family=binomial())
sm_predictions <- predict(sm_fit, validation_set, type="response")
print(auc(validation_set$WnvPresent, sm_predictions))


# AUC Diagnostic

# Write out our first submission
#full_fit <- glm(WnvPresent ~ Species + Month, train_data, family=binomial())
#predictions <- predict(full_fit, test_data, type="response")
#submission <- cbind(test_data$Id, predictions)
#colnames(submission)<-c("Id","WnvPresent")
#write.csv(submission,"output/submission.csv", row.names=FALSE, quote=FALSE)