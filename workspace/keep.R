k = 8

tau = lapply(seq(k), function(x) c(-Inf, sort(rnorm(5)), Inf))
sigma = rep(1, k)
lambda = 1:k
temp = cbind(lambda, sigma)/sqrt(lambda^2 + sigma^2)
lambda = temp[, 1]
sigma = temp[, 2]
n = 1000
result = simulate(n, lambda, sigma, tau)

y = result$y
phi = lambda_to_phi(lambda)
mu = x_hat(y, phi, tau)

mu2 = x_hat2(y, tau)

i = 5
cor(result$y_star[, i], mu2[, i])
cor(result$y_star[, i], mu[, i])
cor(result$y_star[, i], result$y[, i])
plot(result$y_star[, i], mu[, i], ylim = c(-4, 4))
points(result$y_star[, i], mu2[, i], col = "red")
points(result$y_star[, i],  result$y[, i] - 3.5, col = "blue")
abline(a = 0, b = 1)


diag(cor(result$y_star, mu))
diag(cor(result$y_star, mu2))
diag(cor(result$y_star, result$y))


cov_y2 = function(phi, tau) {

  cov_ij = Vectorize(function(i, j) {
    if (i == j) {
      NA
    } else {
      phi_ij = matrix(1, nrow = 2, ncol = 2)
      phi_ij[1, 2] = phi[i, j]
      phi_ij[2, 1] = phi[i, j]
      g = Vectorize(function(k1, k2) {
        lower = c(tau[[i]][k2], tau[[j]][k1])
        upper = c(tau[[i]][k2+1], tau[[j]][k1+1])
        mvtnorm::pmvnorm(
          mean = c(0, 0),
          sigma = phi_ij,
          lower = c(-Inf, -Inf),
          upper = upper)
      })
      m1 = length(tau[[i]]) - 1
      m2 = length(tau[[j]]) - 1

      h1 = function(k) {
        pnorm(tau[[i]][k + 1])
      }

      h2 = function(k) {
        pnorm(tau[[j]][k + 1])
      }
      sum(outer(seq(m1), seq(m2), g)) - sum(sapply(1:m1, h1)) * sum(sapply(1:m2, h2))



    }
  })

  k = nrow(phi)
  outer(seq(k), seq(k), cov_ij)

}


n = 10^6
x = rnorm(n)
y = 2 * x + rnorm(n)
cor(x,y)^2
var(x)/var(y)


n_reps = 10000
n = 100
a = pi
x = replicate(n_reps, prod((a*runif(n))^(1/n)))
hist(x, freq = FALSE)
z = seq(0, 5, by = 0.001)
lines(z, dlnorm(z, -1 + log(a), 1/sqrt(n)))


n = 10000
mean(log(2*runif(n)))
-1 + log(2)
1



mu_ea = 2
mu_lib = 1
sigma_ea = 1
sigma_lib = 1.1

x = seq(0, 10, by = 0.01)
plot(x, dlnorm(x, mu_lib, sigma_lib), type = "l")
lines(x, dlnorm(x, mu_ea, sigma_ea), col = "blue")


x = seq(0, 50000, by = 1)
plot(x, plnorm(x, mu_ea, sigma_ea, lower.tail = FALSE)
     / plnorm(x, mu_lib, sigma_lib, lower.tail = FALSE), type = "l")

(exp(sigma_ea^2) - 1) * exp(2 * mu_ea + sigma_ea^2)
(exp(sigma_lib^2) - 1) * exp(2 * mu_lib + sigma_lib^2)


n = 500000
x = MASS::mvrnorm(n, c(0, 0), matrix(c(1, 0.5, 0.5, 1), nrow = 2, byrow = TRUE))
y = rnorm(n)
x2 = cbind(y, 2*y)

z = x + x2

z2 = MASS::mvrnorm(n, c(0, 0), matrix(c(2, 2.5, 2.5, 5), nrow = 2, byrow = TRUE))


par(mfrow = c(2, 1))
##### OPTION 1: hexbin from package 'hexbin' #######
library(hexbin)
# Create hexbin object and plot
plot(hexbin(z))
plot(hexbin(z2))
