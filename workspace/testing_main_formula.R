### ============================================================================
### This file verifies the main formula with simulations.
###   1.) Verify the formula for the weights;
###   2.) Verify the formula for the risks.
### ============================================================================

Lambda = matrix(c(1, 0, 3,
                  0, 1, 4,
                  1, 4, 1,
                  1, 1, 2), nrow = 4, byrow = TRUE)

Gamma = matrix(c(  3, 0.5, 0.1,
                 0.5,   1, 0.1,
                 0.1, 0.1,   1), nrow = 3, byrow = TRUE)

Sigma = matrix(c(  1, 0.1, 0.1, 0.1,
                 0.1,   1, 0.1, 0.1,
                 0.1, 0.1,   1, 0.1,
                 0.1, 0.1, 0.1,   1), nrow = 4, byrow = TRUE)


tr = function(M) sum(diag(M))

true_weights = function(Lambda, Gamma, Sigma) {
  t(solve(Lambda %*% Gamma %*% t(Lambda) + Sigma, Lambda %*% Gamma))
}

true_risk = function(Lambda, Gamma, Sigma) {
  diag(Gamma) - diag(t(Lambda %*% Gamma) %*% solve(Lambda %*% Gamma %*% t(Lambda) + Sigma, Lambda %*% Gamma))
} 
  
total_risk = function(Lambda, Gamma, Sigma) {
  tr(Gamma) - tr(t(Lambda %*% Gamma) %*% solve(Lambda %*% Gamma %*% t(Lambda) + Sigma, Lambda %*% Gamma))
} 

true_R2 = function(Lambda, Gamma, Sigma) {
  diag(t(Lambda %*% Gamma) %*% solve(Lambda %*% Gamma %*% t(Lambda) + Sigma, Lambda %*% Gamma))/diag(Gamma)
}

total_R2 = function(Lambda, Gamma, Sigma) {
  sum(diag(t(Lambda %*% Gamma) %*% solve(Lambda %*% Gamma %*% t(Lambda) + Sigma, Lambda %*% Gamma)))/sum(diag(Gamma))
}

n   = 100000
Z   = MASS::mvrnorm(n = n, mu = rep(0, nrow(Gamma)), Sigma = Gamma)
eps = MASS::mvrnorm(n = n, mu = rep(0, nrow(Sigma)), Sigma = Sigma)
Y   = t(Lambda %*% t(Z) + t(eps))
summary(lm(Z[, 3] ~ Y[, 1] + Y[, 2] + Y[, 3] + Y[, 4]))

true_weights(Lambda, Gamma, Sigma)
true_R2(Lambda, Gamma, Sigma)

colMeans((Y %*% t(true_weights(Lambda, Gamma, Sigma)) - Z)^2)
total_risk(Lambda, Gamma, Sigma)
true_risk(Lambda, Gamma, Sigma)
