

# Simulate some normal distributed data -----------------------------------


n = 30 # Number of simulated realizations
carrier = rep(c(0, 1), c(10, 20))
null.y = rnorm(n)
alt.y = rnorm(n, mean = carrier / 2)

# plot the two distributions
plot(density(null.y))
lines(density(alt.y), col = "blue")


# Means : T-test ------------------------------------------------------------------


# null hypothesis
t.test(null.y ~ carrier, var.equal = TRUE)

# alt hypothesis
t.test(alt.y ~ carrier, var.equal = TRUE)


# Means : Permutation Test ------------------------------------------------


null.diff = mean(null.y[carrier == 1]) - mean(null.y[carrier == 0])
alt.diff = mean(alt8.y[carrier == 1] - mean(alt.y[carrier == 0]))

# Function to resample and compute mean difference
one.test = function(x, y){
  xstar = sample(x)
  return(mean(y[xstar == 1]) - mean(y[xstar == 0]))
}

# 
many.truenull = replicate(1000, one.test(carrier, null.y))
many.falsenull = replicate(1000, one.test(carrier, alt.y))

hist(many.truenull)
abline(v = null.diff, lwd = 2, col = "purple")
mean(abs(many.truenull) > abs(null.diff))

hist(many.falsenull)
abline(v=alt.diff, lwd=2, col="purple")
mean(abs(many.falsenull) > abs(alt.diff))

