### Plots für das Poster ###

library(ggplot2)
library(dplyr)


n <- 100

d1 <- rnorm(n, 0, 3)
d2 <- rnorm(n, 0, 4)

ecd1 <- ecdf(d1)
e1 <- ecd1(d1)

ecd2 <- ecdf(d2)
e2 <- ecd2(d2)



# Plot 1: ECDFs and KS-test value --------------------------------------------------------------


df <- data.frame(d = c(d1, d2),
                 group = gl(2, n))

max_d <- 4.5

ggplot(df, aes(x = d, colour = group)) + 
    stat_ecdf(geom = "step") + 
    geom_segment(aes(x = max_d, xend = max_d, y = ecd1(max_d), yend = ecd2(max_d)), 
                 colour = "black", size = 1, lineend = "butt") + 
    scale_colour_manual(values = c("orange", "navyblue")) +
    labs(x = "x", y = "Cumulative distribution", colour = "Sample\ndistribution") + 
    ggtitle("ECDFs and KS-test value") + 
    theme_bw()  + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "black"), 
          plot.title = element_text(face="bold", hjust = .5))





# Plot 2: Distributions of p-values by number of permutations ------------------------------------------------------------------

n <- 50
n_perm <- c(100, 500, 1000, 10000)

p1 <- rnorm(n, 0, 3)
p2 <- rnorm(n, 0, 2)
p3 <- rnorm(n, 0, 1.2)
p4 <- rnorm(n, 0, 1)

df <- data.frame(p = c(p1, p2, p3, p4),
                 n_perm = gl(4, n, labels = n_perm))

ggplot(aes(x = p, colour = n_perm), data = df) + 
    geom_line(stat="density", size = 1) + 
    scale_colour_manual(values = c("#f1a340", "#1f78b4", "#c51b7d", "#33a02c")) +
    labs(x = "x", y = "Probability density", colour = "Number of\npermutations") + 
    ggtitle("Distributions of p-values by number of permutations") + 
    theme_bw()  + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "black"), 
          plot.title = element_text(face="bold", hjust = .5))




# Plot 3: Results of permutation test i.e. empirical p-value --------------

n <- 10000
observed_value <- .3

df <- data.frame(test_statistics = rnorm(n, .2, .05))

ggplot(aes(x = test_statistics), data = df) + 
    geom_histogram(fill = "white", colour = "black", bins = 50) + 

      geom_histogram(data = subset(df, test_statistics >= observed_value),
                   colour="black", fill="darkgrey", bins = 50) + 
    geom_vline(aes(xintercept = observed_value, colour = "red"), size = 1) +
    labs(x = "Test statistic", y = "Frequency", colour = "Observed\ntest statistic") + 
    ggtitle("KS permutation test: Empirical p-value") + 
    theme_bw()  + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "black"), 
          plot.title = element_text(face="bold", hjust = .5))



    

# Plot 4: Variance by number of permutations ------------------------------

### Dieser Plot wird wahrscheinlich noch geändert, so dass sie Mittelwerte der 
### p-Werte mit Konfidenzintervallen gezeigt werden

n <- 100

variances <- sort(1/log(1:n), decreasing = TRUE)

df <- data.frame(variances = variances,
                 n_perm = 1:n)

ggplot(aes(x = n_perm, y = variances), data = df) + 
    geom_point() + 
    labs(x = "Number of permutations", y = "Variance") + 
    ggtitle("Variance reduction by number of permutations") + 
    theme_bw()  + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "black"), 
          plot.title = element_text(face="bold", hjust = .5))





# Plot 4 Version 2 --------------------------------------------------------

n <- 100

means <- runif(n, 0, .2)
n_perm = seq(from = 10, to = 1000, by = 10)
lower <- means - rev(seq(from = .01, to = 1, by = .01))
upper <- means + rev(seq(from = .01, to = 1, by = .01))

df <- data.frame(means = means,
                 n_perm = n_perm,
                 lower = lower,
                 upper = upper)


ggplot(aes(x = n_perm, y = means), data = df) + 
    geom_point() + 
    geom_errorbar(aes(x = n_perm, ymin = lower, ymax = upper)) +
    labs(x = "Number of permutations", y = "Mean test statistic") + 
    ggtitle("Variance reduction by number of permutations") + 
    theme_bw()  + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "black"), 
          plot.title = element_text(face="bold", hjust = .5))
    
    
    
    
   
    
    