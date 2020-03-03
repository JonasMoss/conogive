## Use the Agreeableness model again.
model <- ' y  =~ A1 + A2 + A3 + A4 + A5 '
#model <- ' y  =~ E1 + E2 + E3 + E4 + E5 '
fit <- lavaan::cfa(model, data = psychTools::bfi)
coefs <- lavaan::lavInspect(fit, what = "x")
lambda_ <- abs(c(coefs$lambda * sqrt(as.numeric(coefs$psi))))
sigma_ <- sqrt(diag(lavaan::lavInspect(fit, what = "x")$theta))
lambda <- lambda_/sqrt(lambda_^2 + sigma_^2)
sigma <- sigma_/sqrt(lambda_^2 + sigma_^2)

n <- 2709
x = simulate_tau(n, k = 5, lambda = lambda, sigma = sigma)
Sigma = tcrossprod(lambda, lambda) + diag(sigma^2)

# cutz = function(k) {
#   stopifnot(k > 2)
#   cuts = qnorm(seq(0, 1, length.out = k + 1))
#   y = apply(x, 2, .bincode, breaks = cuts)
#   omega_prime_hat(y, rho = Sigma, tau = cuts)
# }

omega_h(lambda, sigma)

n <- 500
x = simulate_tau(n, k = 5, lambda = lambda, sigma = sigma)
cuts = qnorm(seq(0, 1, length.out = 5))
y = apply(x, 2, .bincode, breaks = cuts)
cuts = psych::polychoric(y)$tau
Sigma = tcrossprod(lambda, lambda) + diag(sigma^2)


ordinal_H(lambda, sigma, cuts = qnorm(seq(0, 1, length.out = 100)))



par(las = 1, mar = c(5.1, 4.1, 4.1, 4.1))
plot(outs, ordinal_Hs, ylim = c(0.3, 0.8), pch = 20, log = "x",
     xlab = "Levels", ylab = "Reliability")
points(outs, ordinal_omegas, pch = 1)
abline(h = omega(lambda, sigma), lty = 2)
abline(h = omega_h(lambda, sigma))
labs = formatC(c(omega(lambda, sigma),
                 omega_h(lambda, sigma)), digits = 2)

axis(side = 4, at = c(omega(lambda, sigma), omega_h(lambda, sigma)),
     labels = labs)


plot(outs, ordinal_omegas)

x_hats = apply(y, 2, x_hat, cuts = cuts)
cov(x_hats)

k = ncol(x_hats)
t(replicate(k, cuts)) -> cuts



### ============================================================================
### Old stuff
### ============================================================================


set.seed(313)
sigma = runif(5)
lambda = rep(1, 5)
n = 1000

lambda_star = lambda/sqrt(lambda^2 + sigma^2)
sigma_star = sigma/sqrt(lambda^2 + sigma^2)

x = simulate_tau(n, k = 5, lambda = lambda, sigma = sigma)
Sigma = tcrossprod(lambda, lambda) + diag(sigma^2)

cuts = qnorm(seq(0, 1, length.out = 6))
y = apply(x, 2, .bincode, breaks = cuts)


corr = psych::polychoric(y)$rho
colnames(corr) <- c("X1", "X2", "X3", "X4", "X5")
rownames(corr) <- colnames(corr)
model = " y =~ X1 + X2 + X3 + X4 + X5"

fit = lavaan::sem(model = model, sample.cov = corr, sample.nobs = n)
coefs <- lavaan::lavInspect(fit, what = "x")

lambda <- abs(c(coefs$lambda * sqrt(as.numeric(coefs$psi))))
sigma <- sqrt(diag(lavaan::lavInspect(fit, what = "x")$theta))


set.seed(313)
sigma = rep(1, 5)
lambda = (1:5)

n = 10000
sims = replicate(n, {
  sigma = abs(rcauchy(5))
  lambda = rcauchy(5)
  omega(lambda, sigma) -omega_std(lambda, sigma)
})

n = 10000
sims2 = replicate(n, {
  sigma = abs(rcauchy(5))
  lambda = rcauchy(5)
  omega(lambda, sigma) -omega_std(lambda, sigma)
})

hist(sims)

lambda = c(0, 1, 10000000)
sigma = lambda

lambda_star = lambda/sqrt(lambda^2 + sigma^2)
sigma_star = sigma/sqrt(lambda^2 + sigma^2)

omega(lambda, sigma)
omega_std(lambda, sigma)


means = sapply(seq.int(length(cuts) - 1),
       function(i) truncnorm::etruncnorm(cuts[i], cuts[i+1]))

means2 = sapply(seq.int(length(cuts) - 1),
               function(i) {
                 -(dnorm(cuts[i + 1]) - dnorm(cuts[i]))/(
                   pnorm(cuts[i + 1]) - pnorm(cuts[i])
                 )
               })


x = seq(-3, 3, by = 0.01)
plot(x, dnorm(x, 0, 1))
abline(v = cuts)
abline(v = means, col = "red")


v = thurstone(lambda, sigma)


eigen(tcrossprod(v, v))
