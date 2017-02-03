# clean workspace
rm(list = ls())


# Loading Packages --------------------------------------------------------


library(ggplot2)
library(dplyr)
library(gridExtra)


# Permutation Test Function -----------------------------------------------


perm_test = function(data, n_perm = 100){
  
  # Compute Kolomogorov Test Statistic
  D0 = ks.test(data[1:n_data / 2], data[n_data / 2 + 1:n_data])$statistic
  
  one_perm = function(data){
    n_data = length(data)
    data = sample(x = data, size = n_data)
    
    return(ks.test(data[1:n_data / 2], data[n_data / 2 + 1:n_data])$statistic)
  }
  
  # Crate vector with test statistics
  D = replicate(n = n_perm, one_perm(data))
  
  # Compute and return p-value
  return(mean(D >= D0)) # Does it make sence to use c() or just take the permutated test statistics
}


# Simulatiing KS-Perm-Test with Different Number Permutation --------------


# Function for Simulating KS-Perm-Test with predefined number of permutations
sim_perm_test = function(n_perm, data, n = 400){
  p_value_vec = replicate(n= n, perm_test(data, n_perm = n_perm))
  print(n_perm)
  return(data.frame(variance = var(p_value_vec),
                    mean_p = mean(p_value_vec), 
                    lower_ci = quantile(p_value_vec, 0.05),
                    upper_ci = quantile(p_value_vec, 0.95)))
}


# Create data for simulation ----------------------------------------------
# Set variables
n_data = 1000 
n_sim_perm = 1000

# Simulate the data
set.seed(123456)
X = rnorm(n = n_data / 2)
Y = rnorm(n = n_data / 2)
data = rbind(X, Y)


# Simulate different number of permutations -------------------------------
# Caution: Runs for about 12 hours!!!! Load csv with simulated data instead (perm_number_sim.csv)
# Start timing the simulation
start = proc.time()

# Create vector with permutation numbers to be simulated
n_perms_to_sim = (seq(from = 10, to = 1000, by  = 10))

# Simulate the different perm number and save results to matrix
per_sim_mat = matrix(n_perms_to_sim, ncol = 1)
mean_var_mat = matrix(unlist(lapply(per_sim_mat, FUN = sim_perm_test, data)), ncol = 4, byrow = TRUE)

# End timing the simulation and print time it took
end = proc.time()
print(end - start)

# Create Dataframe with results
sim_df = data.frame(cbind(per_sim_mat, mean_var_mat))
names(sim_df) = c("n_perm", "variance", "mean", "upper_bound", "lower_bound")

# Save simulation results to csvv
write.csv(sim_df, file = "R/perm_number_sim.csv")


# Simulate with different sample sizes -----------------------------------------


# Function for Simulating KS-Perm-Test with different sample sizes
perm_test_sample = function(sample_size = 1000, n_perm = 1000, dist = 'norm'){
  # All distribution are simulated with mean = 3 and sd = sqrt(3)
  if(dist == 'norm'){
    # Simulate the data from normal distribution
    X = rnorm(n = sample_size / 2, mean = 3, sd = sqrt(3))
    Y = rnorm(n = sample_size / 2, mean = 3,  sd = sqrt(3))
  }
  
  if(dist == 't'){
    # Simulate the data for t distribution
    X = rnorm(n = sample_size / 2, mean = 3, sd = sqrt(3))
    Y = rt(n = sample_size / 2, df = 1.5) + 3
  }
  
  if(dist == 'chisq'){
    # Simulate the data with chi squared distritution
    X = rnorm(n = sample_size / 2, mean = 3, sd = sqrt(3))
    Y = rchisq(n = sample_size / 2, df = 3)
  }
  
  if(dist == 'unif'){
    # Simulate the data with uniform distribution
    X = rnorm(n = sample_size / 2, mean = 3, sd = sqrt(3))
    Y = runif(n = sample_size / 2, min = 0, max = 6)
  }
  
  # Simulation function
  data = c(X, Y)
  
  # Compute KS perm test ith simulated data and give permutation number
  return(perm_test(data, n_perm = n_perm))
}

# Function for simulating different sample size
sim_sample = function(sample_size, dist){
  
  # Repeat the perm_test_sample 300 times
  p_value_vec = replicate(n= 300, perm_test_sample(sample_size, n_perm = 400, dist))
  
  # Compute and return varaince, mean and confidence intervals
  return(data.frame(variance = var(p_value_vec),
                    mean_p = mean(p_value_vec),
                    lower_ci = quantile(p_value_vec, 0.05),
                    upper_ci = quantile(p_value_vec, 0.95)))
}

# Create vector with permutation numbers to be simulated
sample_sizes = c(seq(10,100,10), seq(120,300, 20), seq(350, 600, 50), seq(700, 1500,100), 1800, 2000, 5000)


# Simulate different sample sizes for the four distributions --------------
set.seed(123456)

sample_sim_mat = matrix(sample_sizes, ncol = 1)
sample_norm = matrix(unlist(lapply(sample_sizes, FUN = function(x) sim_sample(x, dist = 'norm'))), ncol = 4, byrow = TRUE)
sample_t = matrix(unlist(lapply(sample_sizes, FUN = function(x) sim_sample(x, dist = 't'))), ncol = 4, byrow = TRUE)
sample_chisq = matrix(unlist(lapply(sample_sizes, FUN =function(x) sim_sample(x, dist = 'chisq'))), ncol = 4, byrow = TRUE)
sample_unif = matrix(unlist(lapply(sample_sizes, FUN = function(x) sim_sample(x, dist = 'unif'))), ncol = 4, byrow = TRUE)

# Save into dataframes
df_sample_norm = data.frame(cbind(sample_sim_mat, sample_norm))
df_sample_t = data.frame(cbind(sample_sim_mat, sample_t))
df_sample_chisq = data.frame(cbind(sample_sim_mat, sample_chisq))
df_sample_unif = data.frame(cbind(sample_sim_mat, sample_unif))

names(df_sample_norm) = c("sample_size", "variance", "mean", "upper_bound", "lower_bound")
names(df_sample_t) = c("sample_size", "variance", "mean", "upper_bound", "lower_bound")
names(df_sample_chisq) = c("sample_size", "variance", "mean", "upper_bound", "lower_bound")
names(df_sample_unif) = c("sample_size", "variance", "mean", "upper_bound", "lower_bound")

# Write data into csv
write.csv(df_sample_norm, file = "R/df_sample_norm.csv")
write.csv(df_sample_t, file = "R/df_sample_t.csv")
write.csv(df_sample_chisq, file = "R/df_sample_chisq.csv")
write.csv(df_sample_unif, file = "R/df_sample_unif.csv")


# Simulate different sample sizes -----------------------------------------

