z = rexp(500000) - 1
x = cbind(z + (rexp(500000) - 1),
          z + (rexp(500000) - 1),
          z + (rexp(500000) - 1),
          z + (rexp(500000) - 1))


lambda = 1
sigma = sqrt(2)
z = rnorm(500000)
x = cbind(z + sigma*rnorm(500000),
          z + sigma*rnorm(500000),
          z + sigma*rnorm(500000),
          z + sigma*rnorm(500000))

mean(x[, 1]^4)
3*lambda^4 + 6*lambda^2*sigma^2 + 3*sigma^4

mean(x[, 1]^3*x[, 2]^1)
3*lambda^4 + 3*lambda^2*sigma^2

mean(x[, 1]^2*x[, 2]^2)
sigma^4 + 2*lambda^2*sigma^2 + 3*lambda^4

mean(x[, 1]^2*x[, 2]*x[, 3])
lambda^2*sigma^2 + 3*lambda^4

mean(x[, 1]*x[, 2]*x[, 3]*x[, 4])
3*lambda^4
