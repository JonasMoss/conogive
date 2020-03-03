Sigma
lambda^2/(k*lambda^2 + sigma^2)^2
(t(rep(1, 5)) %*% Sigma %*% rep(1, 5)/k)^2
(k*lambda^2 + sigma^2)^2
(t(rep(1, 5)) %*% Sigma %*% rep(1, 5)/k)^2

t(rep(1, 5)) %*% Xi_theoretical(cuts, Sigma) %*% rep(1, 5)


alpha_std(Sigma)
k*k/(t(rep(1, 5)) %*% Sigma %*% rep(1, 5))*mean(lambda^2)

ordinal_omega(lambda, sigma, cuts = qnorm(seq(0, 1, length.out = 4)))

alpha_std(Sigma)^2 * t(rep(1, 5)) %*% Xi_theoretical(qnorm(seq(0, 1, length.out = 4)), Sigma) %*% rep(1, 5) /
  (k^2 * lambda^2)

alpha_std(Sigma) * (t(rep(1, 5)) %*% Xi_theoretical(qnorm(seq(0, 1, length.out = 4)), Sigma) %*% rep(1, 5)) / (t(rep(1, 5)) %*% Sigma %*% rep(1, 5))

alpha_std(Sigma)^2 / (k^2 * lambda^2)



k^2 * lambda^2 / (t(rep(1, 5)) %*% Sigma %*% rep(1, 5)) == alpha_std(Sigma)
