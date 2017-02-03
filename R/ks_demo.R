# Compute KS test statistic ----------------------------------------------


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
    
    
    
    g <- ggplot(ks_df, aes(x = x, colour = group)) + 
        stat_ecdf(geom = "step", size = .8) + 
        scale_x_continuous(limits = c(-4, 10)) +
        geom_segment(aes(x = d_position, xend = d_position, y = e1(d_position), yend = e2(d_position)), 
                     colour = "red", size = 1.3, lineend = "butt") + 
        scale_colour_manual(values = c("black", "grey"), labels = c("norm", dist)) +
        labs(x = "x", y = "Empirical cumulative distribution", colour = "") + 
        ggtitle("") + 
        theme_bw()  + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              legend.key = element_rect(colour = "black"), 
              plot.title = element_text(face="bold", hjust = .5))
    
    return(g)
}


distributions <- c("norm", "t", "chisq", "unif")
ecd_plots <- lapply(distributions, visualize_ks, n = 500, seed = 44)
grid.arrange(ecd_plots[[1]], ecd_plots[[2]], ecd_plots[[3]], ecd_plots[[4]])



