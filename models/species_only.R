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

fit <- glm(WnvPresent ~ Species + Month, train_set, family=binomial())
predictions <- predict(fit, validation_set, type="response")

# AUC Diagnostic
auc(validation_set$WnvPresent, predictions)

# Write out our first submission
full_fit <- glm(WnvPresent ~ Species + Month, train_data, family=binomial())
predictions <- predict(fit, test_data, type="response")

submission <- cbind(test_data$Id, predictions)
write.csv(submissionFile,"species_only.csv",row.names=FALSE,quote=FALSE)
