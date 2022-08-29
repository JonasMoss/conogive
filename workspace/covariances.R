tau = lapply(seq(5), function(x) c(-Inf, sort(rnorm(5)), Inf))
sigma = rep(1, 5)
lambda = 1:5
temp = cbind(lambda, sigma)/sqrt(lambda^2 + sigma^2)
lambda = temp[, 1]
sigma = temp[, 2]
n = 1000000
result = simulate(n, lambda, sigma, tau)
phi = cor(result$y_star, result$y_star)


cov_y(phi, tau)
cov(result$y, result$y)


phi = cor(result$y_star, result$y_star)
cov_yy_star(phi, tau)
cov(result$y, result$y_star)



