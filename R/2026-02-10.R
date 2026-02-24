library(ggplot2)

se <- function(n, sd) {
	sd / sqrt(n)
}

df <- data.frame(n = seq(10, 1000, by = 10))
df$se <- se(df$n, sd = 1)

ggplot(df, aes(x = n, y = se)) + geom_path() #+ geom_hline(yintercept = 0.05)


n <- 1e5
pop <- runif(n, 0, 1)

samp1 <- sample(pop, size = 50)
sd(samp1) / sqrt(length(samp1))
c(mean(samp1) - 1.96 * sd(samp1) / sqrt(length(samp1)),
  mean(samp1) + 1.96 * sd(samp1) / sqrt(length(samp1)))

set.seed(2112)
boot.samples <- numeric(1000) # 1,000 bootstrap samples
for(i in seq_along(boot.samples)) {
	tmp <- sample(samp1, size = length(samp1), replace = TRUE)
	boot.samples[i] <- mean(tmp)
}
c(mean(boot.samples) - 1.96 * sd(boot.samples),
  mean(boot.samples) + 1.96 * sd(boot.samples))


