# Let the underlying population proportion vary and just look at the distribution.

samples <- function(theta, sample_sizes) {
  # Assume each sample is of size 50
  res <- c()
  for(idx in seq_along(sample_sizes)) {
    res[idx] <- rbinom(n=1, size=sample_sizes[idx], theta)
  }
  
  res
}

run_simulation <- function(n, trap_counts) {
  theta = .006
  
  res = NULL
  for(i in 1:n) {
    experiment <- samples(theta=theta, sample_sizes=trap_counts)
    df <- data.frame(neg=sum(experiment == 0), pos=sum(experiment > 0))
    res = rbind(res, df)
  }
  
  res
}

# Looks like it should be ~.002
species_data <- filter(trai, Species=="CULEX PIPIENS")
presence <- table(temp$WnvPresent)
names(presence) <- c("neg", "pos")
print("Actual:")
print(presence)

print("Simulated:")
print(run_simulation(10, species_data$NumMosquitos))

