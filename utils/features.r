# Feature engineering code
library(lubridate)

features <- function(.data) {
  .data$Month <- factor(month(.data$Date))
  .data$Year <- factor(year(.data$Date))
  
  .data
}
