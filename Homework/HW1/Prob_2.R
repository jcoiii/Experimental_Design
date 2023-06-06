rm()
#2a
set.seed(100)
n_sim <- 1000 
n_seq <- seq(2, 100, by = 4) 
power <- numeric(length(n_seq)) 

for (i in seq_along(n_seq)) {
  n <- n_seq[i] 
  reject <- numeric(n_sim)
  
  for (j in 1:nsim) {
    #samples generation
    x <- rnorm(n, mean = 35, sd = 3)
    y <- rnorm(n, mean = 33, sd = 3)
    
    res <- t.test(x, y)
    pval <- res$p.value
    
    reject[j] <- pval < 0.05
  }
  power[i] <- mean(reject)
}

plot(n_seq, power, type = "b", xlab = "Sample Size", ylab = "Power", 
     main = "t-test Power Curve")

#2b
library(pwr)

#parameters
alpha <- 0.05
mu.diff <- 2
sd <- 3
n_seq2 <- seq(2, 100, by = 4)
power <- numeric(length(n_seq2))

for (i in seq_along(n_seq2)) {
  n <- n_seq2[i] 
  power[i] <- pwr.t.test(n = n, d = mu.diff/sd, sig.level = alpha, 
                         type = "two.sample", alternative = "two.sided")$power
}
plot(n_seq2, power, type = "b", xlab = "Sample Size", ylab = "Power", 
     main = "Power Curve for t-test")