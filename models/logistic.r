train_data <- read.csv("data/train.csv")
test_data <- read.csv("data/test.csv")

clean <- function(.data) {
  .data$Date <- as.Date(.data$Date)
  
  # Doesn't happen in test data
  if (!is.null(.data$WnvPresent)) {
    .data$WnvPresent <- factor(.data$WnvPresent)
  }
  
  # For now let's replace unknown species with
  .data$Species[.data$Species == "UNSPECIFIED CULEX"] <- "CULEX ERRATICUS"
    
  .data
}

add_features <- function(.data) {
  .data$Month <- factor(months(.data$Date))
  
  .data
}

train_data <- clean(train_data)
train_data <- add_features(train_data)
test_data <- clean(test_data)
test_data <- add_features(test_data)

# Split into training and validation set
validation_set <- filter(train_data, Date < as.Date("2008-1-1"))
train_set <- filter(train_data, Date >= as.Date("2008-1-1"))

# Prediction model based on just species. Return exactly the overall prevalence based on that species.

s_fit <- glm(WnvPresent ~ Species, train_set, family=binomial())
s_predictions <- predict(s_fit, validation_set, type="response")
auc(validation_set$WnvPresent, s_predictions)

sm_fit <- glm(WnvPresent ~ Species + Month, train_set, family=binomial())
sm_predictions <- predict(sm_fit, validation_set, type="response")
auc(validation_set$WnvPresent, sm_predictions)


# AUC Diagnostic

# Write out our first submission
full_fit <- glm(WnvPresent ~ Species + Month, train_data, family=binomial())
predictions <- predict(full_fit, test_data, type="response")
submission <- cbind(test_data$Id, predictions)
colnames(submission)<-c("Id","WnvPresent")
write.csv(submission,"output/submission.csv", row.names=FALSE, quote=FALSE)
