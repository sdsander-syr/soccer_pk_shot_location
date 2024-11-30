r1 <- -0.749
r2 <- -0.997
se_r1 <- sqrt((1-r1^2)/2)
se_r2 <- sqrt((1-r1^2)/2)

n <- 30
resample1 <- rnorm(n,r1,se_r1)
resample2 <- rnorm(n,r2,se_r2)

mean_resample1 <- mean(resample1)
sd_sampling_distribution1 <- sd(resample1)/sqrt(n)

(t_value_mean_resample1 <- mean_resample1/(sd(resample1)/sqrt(n)))

mean_resample2 <- mean(resample2)
sd_sampling_distribution2 <- sd(resample2)/sqrt(n)

(t_value_mean_resample2 <- mean_resample2/(sd(resample2)/sqrt(n)))
