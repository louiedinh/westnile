# Let the underlying population proportion vary and just look at the distribution.

samples <- function(theta, n_samples) {
  # Assume each sample is of size 50
  rbinom(n_samples, 50, theta)
}

run_simulation <- function(n) {
  n_samples = 2700
  theta = .002
  
  res = NULL
  for(i in 1:n) {
    experiment <- samples(theta=theta, n_samples=n_samples)
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
print(run_simulation(10))

