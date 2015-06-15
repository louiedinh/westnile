# Let the underlying population proportion vary and just look at the distribution.

samples <- function(theta, sample_sizes) {
  # Assume each sample is of size 50
  res <- c()
  for(idx in seq_along(sample_sizes)) {
    res[idx] <- rbinom(n=1, size=sample_sizes[idx], theta)
  }
  
  res
}

run_simulation <- function(n, trap_counts, theta) {
  res = NULL
  for(i in 1:n) {
    experiment <- samples(theta=theta, sample_sizes=trap_counts)
    df <- data.frame(neg=sum(experiment == 0), pos=sum(experiment > 0))
    res = rbind(res, df)
  }
  
  res
}

run_simple <- function(theta) {
  # Looks like it should be ~.006
  species_data <- filter(trai, Species=="CULEX PIPIENS")
  presence <- table(temp$WnvPresent)
  names(presence) <- c("neg", "pos")
  print("Actual:")
  print(presence)
  
  print("Simulated:")
  print(run_simulation(10, species_data$NumMosquitos, theta))
}

##### Try 2 - likelihood function

likelihood <- function(data, theta) {
  # Calculates the P(data | theta)
  # = product of individual probabilities
  # The probability that a specific detection of wnv is
  # 1 - prob(no detection) and prob(no virus) = (1 - infection_rate)^n
  p_no_virus = (1 - theta)
  
  counts <- data$NumMosquitos
  wnv <- data$WnvPresent
  
  log_likelihood = 0
  p_detections = c()
  p_no_detections = c()
  for(idx in seq_along(counts)){
    count <- counts[idx]
    p_no_detection = p_no_virus ^ count
    p_detection = 1 - p_no_detection
    
    p_detections[idx] = p_detection
    p_no_detections[idx] = p_no_detection
  }
  
  df <- data.frame(neg=log(p_no_detections), pos=log(p_detections), data$WnvPresent, data$NumMosquitos)
  
  # Add the likelihood of presence on the detected with the absence in the non-detected
  likelihood = sum(df$pos[data$WnvPresent == 1]) + sum(df$neg[data$WnvPresent == 0])
        
  likelihood
}

run_likelihood <- function(species, year) {
  res = data.frame()
  
  for(y in year) {
    species_data <- filter(trai, Species==species, dYear==y)
    infection_rates = seq(.0001, .030, by=.001)
    likelihoods <- sapply(infection_rates, likelihood, data=species_data)
    row <- arrange(data.frame(species=species, year=y, p=infection_rates, likelihood=likelihoods), desc(likelihood))[1,]
    res <- rbind(res, row)
  }
  
  res
}


# Wow! They agree!
#print("Using naive simulation with theta=.006")
#run_simple(.0058)

print("Using likelihood")
p <- run_likelihood("CULEX PIPIENS", c(2007, 2009, 2011, 2013))
pr <- run_likelihood("CULEX PIPIENS/RESTUANS", c(2007, 2009, 2011, 2013))
r <- run_likelihood("CULEX RESTUANS", c(2007, 2009, 2011, 2013))

infection_rates <- rbind(p, pr, r)
infection_rates <- select(infection_rates, -likelihood)
# Results 
