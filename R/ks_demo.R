# Compute KS test statistic ----------------------------------------------


ks_test_sample = function(n = 1000, dist = 'norm'){
    
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
    
    return(data.frame(x = c(X, Y), group = rep(c("norm_1", paste(dist, 2, sep = "_")), each = n/2)))
}



n <- 100
ks_df <- ks_test_sample(n = n, dist = "norm")

# Compute Kolomogorov Test Statistic
d = ks.test(ks_df$x[1:(n/2)], ks_df$x[((n/2) + 1):n])$statistic

d_max_position <- which.max(abs(cumsum(ks_df$x[1:(n/2)]) - cumsum(ks_df$x[((n/2) + 1):n])))
ks_df$x[23]


# Plot KS test statistic --------------------------------------------------



ggplot(ks_df, aes(x = x, colour = group)) + 
    stat_ecdf(geom = "step") + 
    geom_segment(aes(x = d, xend = d, y = ecd1(max_d), yend = ecd2(max_d)), 
                 colour = "black", size = 1, lineend = "butt") + 
    scale_colour_manual(values = c("orange", "navyblue")) +
    labs(x = "x", y = "Cumulative distribution", colour = "Sample\ndistribution") + 
    ggtitle("ECDFs and KS-test value") + 
    theme_bw()  + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "black"), 
          plot.title = element_text(face="bold", hjust = .5))