# Data cleaning code

clean <- function(.data) {
  .data$Date <- as.Date(.data$Date)
  
  # Doesn't happen in test data
  if (!is.null(.data$WnvPresent)) {
    .data$WnvPresent <- factor(.data$WnvPresent)
  }
  
  # For now let's replace unknown species with
  .data$Species[.data$Species == "UNSPECIFIED CULEX"] <- "CULEX ERRATICUS"
  .data$WnvPresent <- .data$WnvPresent == "1"
  
  
  measurements <- measurements(.data)  
  traps <- traps(.data)
  
  list(measurements=measurements, traps=traps)
}

traps <- function(.data) {
  .data %>% select(Trap, Address, Block, Street, AddressNumberAndStreet, Latitude, Longitude, AddressAccuracy) %>% 
            unique %>% arrange(Trap)
}

measurements <- function(.data) {
  df <- .data %>% select(Trap, Date, Species, NumMosquitos, WnvPresent)
  
  df
}