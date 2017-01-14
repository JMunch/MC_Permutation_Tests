# Set parameters
n_data = 1000 # must be even
n_sim_perm = 100 # 1000

# Simulate the data
X = rnorm(n = n_data / 2)
Y = rnorm(n = n_data / 2)

# Simulation function
data = rbind(X, Y)


# Permutation test function -----------------------------------------------


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

# p-value
  return(mean(D >= D0)) # Does it make sence to use c() or just take the permutated test statistics
}


# Testing
#perm_test(data, 100)

# Simulation of different numbers of permutation --------------------------


sim_perm_test = function(n_perm, data){
  p_value_vec = replicate(n= 100, perm_test(data, n_perm = n_perm))
  print(n_perm)
  return(data.frame(variance = var(p_value_vec), mean_p = mean(p_value_vec), lower_ci = quantile(p_value_vec, 0.05), upper_ci = quantile(p_value_vec, 0.95)))
}


# Simulate different number of permutations
n_perms_to_sim = c(seq(from = 1, to = 500, by  = 5) #, 150, 200, 250, 300, 400, 500)
per_sim_mat = matrix(n_perms_to_sim, ncol = 1)
mean_var_mat = matrix(unlist(lapply(per_sim_mat, FUN = sim_perm_test, data)), ncol = 4, byrow = TRUE)
sim_df = data.frame(cbind(per_sim_mat, mean_var_mat))
names(sim_df) = c("n_perm", "variance", "mean", "upper_bound", "lower_bound")
sim_df
plot(n_perms_to_sim, sim_df$variance)
plot(n_perms_to_sim, sim_df$mean)

