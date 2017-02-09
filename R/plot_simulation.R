### Plots for permutation tests poster ###

library(ggplot2)
library(dplyr)
library(gridExtra)


# load data from simulations
sim_df = read.csv("r/perm_number_sim.csv")

df_samples <- list(df_sample_norm = read.csv("R/df_sample_norm.csv"),
                   df_sample_t = read.csv("R/df_sample_t.csv"),
                   df_sample_chisq = read.csv("R/df_sample_chisq.csv"),
                   df_sample_unif = read.csv("R/df_sample_unif.csv"))




# Set parameters
n_data = 1000 # must be even

# Simulate the data
set.seed(113)
X = rnorm(n = n_data / 2)
Y = rnorm(n = n_data / 2)

# Simulation function
values_H0 = c(X, Y)


perm_test_hist = function(data, n_perm = 100){
    
    # Compute Kolomogorov Test Statistic
    D0 = ks.test(data[1:n_data / 2], data[n_data / 2 + 1:n_data])$statistic
    
    one_perm = function(data){
        n_data = length(data)
        data = sample(x = data, size = n_data)
        
        return(ks.test(data[1:n_data / 2], data[n_data / 2 + 1:n_data])$statistic)
    }
    
    # Crate vector with test statistics
    D <- data.frame(values = replicate(n = n_perm, one_perm(data)))
    
    # p-value
    return(list(D, D0)) 
}


# simulate data for histogram (demonstates empirical p-value in KSP test)
hist_data <- perm_test_hist(values_H0, n_perm = 100000)

# histogram data
hist_df <- hist_data[[1]]

# extract test statistic
hist_teststat <- hist_data[[2]]


# Adjust pt() and dt() for shifted t-distribution (t-distribution that is not 
# centered around zero)
pt_custom <- function(x) {pt(x -3, 1.5)}
dt_custom <- function(x) {dt(x - 3, 1.5)}


# Visualize KS for different distributions

visualize_ks = function(n = 1000, dist = 'norm', seed = NULL){
    
    set.seed(seed)
    
    if(dist == 'norm'){
        # Simulate the data
        X = rnorm(n = n / 2, mean = 3, sd = sqrt(3))
        Y = rnorm(n = n / 2, mean = 3,  sd = sqrt(3))
    }
    
    if(dist == 't'){
        # Simulate the data
        X = rnorm(n = n / 2, mean = 3, sd = sqrt(3))
        Y = rt(n = n / 2, df = 1.5) + 3
    }
    
    if(dist == 'chisq'){
        # Simulate the data
        X = rnorm(n = n / 2, mean = 3, sd = sqrt(3))
        Y = rchisq(n = n / 2, df = 3)
    }
    
    if(dist == 'unif'){
        # Simulate the data
        X = rnorm(n = n / 2, mean = 3, sd = sqrt(3))
        Y = runif(n = n / 2, min = 0, max = 6)
    }
    
    ks_df <- data.frame(x = c(X, Y), group = rep(c("norm_1", paste(dist, 2, sep = "_")), each = n/2))
    
    
    
    d = ks.test(ks_df$x[1:(n/2)], ks_df$x[((n/2) + 1):n])$statistic
    
    e1 <- ecdf(ks_df$x[1:(n/2)])
    e2 <- ecdf(ks_df$x[((n/2) + 1):n])
    
    all_x <- sort(ks_df$x)
    d_position <- all_x[which.max(abs(e1(all_x) - e2(all_x)))]
    
    ks_df$group <- factor(ks_df$group, levels = unique(ks_df$group))
    
    
    g <- ggplot(ks_df, aes(x = x, colour = group)) + 
        stat_ecdf(geom = "step", size = 1) + 
        scale_x_continuous(limits = c(-4, 10)) +
        geom_segment(aes(x = d_position, xend = d_position, y = e1(d_position), yend = e2(d_position)), 
                     colour = "red", size = 1.3, lineend = "butt") + 
        scale_colour_manual(values = c("black", "grey"), labels = c("norm", dist)) +
        labs(x = "x", y = "ECD", colour = "") + 
        ggtitle("") + 
        theme_bw()  + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              legend.key = element_rect(colour = "black"), 
              plot.title = element_text(face="bold", hjust = .5)) + 
        theme(text = element_text(size = 16))
    
    return(g)
}


# set distribution vector
distributions <- c("norm", "t", "chisq", "unif")

# create plot to visualize KS test statistic under different distributions
ecd_plots <- lapply(distributions, visualize_ks, n = 500, seed = 44)

# arrange plots in a grid
grid.arrange(ecd_plots[[1]], ecd_plots[[2]], ecd_plots[[3]], ecd_plots[[4]])



# Empirical p-value

# plot histogram of simulated test statistics with originally observed test statistic
# --> demonstrates empirical p-value
emp_pval_plot <- ggplot(aes(x = values), data = hist_df) + 
    geom_histogram(fill = "white", colour = "black", bins = 70) + 
    geom_histogram(data = subset(hist_df, values >= hist_teststat),
                   colour="black", fill="darkgrey", bins = 70) + 
    geom_vline(aes(xintercept = hist_teststat, colour = " "), size = 1.5) +
    labs(x = "Test statistic", y = "Frequency", colour = "Observed\ntest statistic") + 
    ggtitle("") + 
    theme_bw()  + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "black"), 
          plot.title = element_text(face="bold", hjust = .5)) + 
    theme(text = element_text(size = 16))

emp_pval_plot




# Plot 4 No. of permutations


# plot variance reduction of empirical p-values under increasing sample size
ggplot(aes(x = n_perm, y = mean), data = sim_df) + 
    #geom_point() + 
    geom_line(size = 2) + 
    labs(x = "Number of permutations", y = "Mean empirical p-value") + 
    ggtitle("") + 
    theme_bw()  + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "black"), 
          plot.title = element_text(face="bold", hjust = .5)) +
    geom_errorbar(aes(x = n_perm, ymin = lower_bound, ymax = upper_bound)) +
    geom_ribbon(aes(ymin=lower_bound, ymax=upper_bound), alpha=0.25) + 
    theme(text = element_text(size = 16))




# Plot 5 sample size


# function to visualize impact of increasing sample size in KSP test under different distributions
visualize_sample_size <- function(data){
    
    p <- ggplot(aes(x = sample_size, y = mean), data = data) +
        geom_point(size = 1) + 
        geom_line() +
        geom_ribbon(aes(ymin=lower_bound, ymax=upper_bound), alpha=0.25) +
        #geom_errorbar(aes(x = sample_size, ymin = lower_bound, ymax = upper_bound)) +
        labs(x = "Sample size", y = "Mean empirical p-value") +  
        ggtitle("") + 
        theme_bw()  + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              legend.key = element_rect(colour = "black"), 
              plot.title = element_text(face="bold", hjust = .5)) + 
        theme(text = element_text(size = 16))
    
    return(p)
    
}

# apply visualization function on all four data sets
sample_size_plots <- lapply(df_samples, visualize_sample_size)

#arrange plots in a grid
grid.arrange(sample_size_plots[[1]], sample_size_plots[[2]], sample_size_plots[[3]], sample_size_plots[[4]], nrow = 2)




# In this section the distribution functions and density functions of the four 
# distributions are generated. These plots are used on the poster to support the 
# plots that demonstrate the KS test and the impact of increasing sample size


norm_norm_distr <- ggplot(data.frame(x = c(-4, 10)), aes(x = x)) +
    stat_function(fun = pnorm, args = list(mean = 3, sd = sqrt(3)), size = 1.2) +
    labs(x = "x", y = "F(x)") + 
    ggtitle("Normal vs. Normal") + 
    theme_bw()  + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "black"), 
          plot.title = element_text(face="bold", hjust = .5)) + 
    theme(text = element_text(size = 16))

norm_norm_distr



norm_t_distr <- ggplot(data.frame(x = c(-4, 10)), aes(x = x)) +
    stat_function(fun = pt_custom, size = 1.2) +
    stat_function(fun = pnorm, args = list(mean = 3, sd = sqrt(3)), size = 1.2)+
    labs(x = "x", y = "F(x)") + 
    ggtitle("Normal vs. Student's t") + 
    theme_bw()  + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "black"), 
          plot.title = element_text(face="bold", hjust = .5)) + 
    theme(text = element_text(size = 16))

norm_t_distr



norm_chisq_distr <- ggplot(data.frame(x = c(-4, 10)), aes(x = x)) +
    stat_function(fun = pchisq, args = list(df = 3), size = 1.2) +
    stat_function(fun = pnorm, args = list(mean = 3, sd = sqrt(3)), size = 1.2)+
    labs(x = "x", y = "F(x)") +  
    ggtitle("Normal vs. Chi-squared") + 
    theme_bw()  + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "black"), 
          plot.title = element_text(face="bold", hjust = .5)) + 
    theme(text = element_text(size = 16))

norm_chisq_distr



norm_unif_distr <- ggplot(data.frame(x = c(-4, 10)), aes(x = x)) +
    stat_function(fun = punif, args = list(min = 0, max = 6), size = 1.2) +
    stat_function(fun = pnorm, args = list(mean = 3, sd = sqrt(3)), size = 1.2)+
    labs(x = "x", y = "F(x)") + 
    ggtitle("Normal vs. Uniform") + 
    theme_bw()  + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "black"), 
          plot.title = element_text(face="bold", hjust = .5)) + 
    theme(text = element_text(size = 16))

norm_unif_distr



norm_norm_density <- ggplot(data.frame(x = c(-4, 10)), aes(x = x), size = 1.2) +
    stat_function(fun = dnorm, args = list(mean = 3, sd = sqrt(3)), size = 1.2) +
    labs(x = "x", y = "f(x)") + 
    ggtitle("Normal vs. Normal") + 
    theme_bw()  + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "black"), 
          plot.title = element_text(face="bold", hjust = .5)) + 
    theme(text = element_text(size = 16))

norm_norm_density 




norm_t_density <- ggplot(data.frame(x = c(-4, 10)), aes(x = x)) +
    stat_function(fun = dt_custom, size = 1.2) +
    stat_function(fun = dnorm, args = list(mean = 3, sd = sqrt(3)), size = 1.2) +
    labs(x = "x", y = "f(x)") + 
    ggtitle("Normal vs. Student's t") + 
    theme_bw()  + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "black"), 
          plot.title = element_text(face="bold", hjust = .5)) + 
    theme(text = element_text(size = 16))

norm_t_density



norm_chisq_density <- ggplot(data.frame(x = c(-4, 10)), aes(x = x)) +
    stat_function(fun = dchisq, args = list(df = 3), size = 1.2) +
    stat_function(fun = dnorm, args = list(mean = 3, sd = sqrt(3)), size = 1.2) +
    labs(x = "x", y = "f(x)") + 
    ggtitle("Normal vs. Chi-squared") + 
    theme_bw()  + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "black"), 
          plot.title = element_text(face="bold", hjust = .5)) + 
    theme(text = element_text(size = 16))

norm_chisq_density


norm_unif_density <- ggplot(data.frame(x = c(-4, 10)), aes(x = x)) +
    stat_function(fun = dunif, args = list(min = 0, max = 6), size = 1.2) +
    stat_function(fun = dnorm, args = list(mean = 3, sd = sqrt(3)), size = 1.2) +
    labs(x = "x", y = "f(x)") + 
    ggtitle("Normal vs. Uniform") + 
    theme_bw()  + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "black"), 
          plot.title = element_text(face="bold", hjust = .5)) + 
    theme(text = element_text(size = 16))

norm_unif_density


