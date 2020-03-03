### ============================================================================
### This file verifies the sum score formulas.
###   1.) Verify the formula for the weights;
###   2.) Verify the formula for the risks.
### ============================================================================

risk_ss = function(lambda, sigma) {
  (1 + length(lambda)*mean(lambda)^2/mean(sigma^2))^-1 
}

alpha = function(lambda, sigma) {
  k = length(lambda)
  k/(k-1)*(1 - sum(sigma^2 + lambda^2)/(sum(lambda)^2 + sum(sigma^2)))
}

omega = function(lambda, sigma) {
  k = length(lambda)
  (1 + mean(sigma^2)/(k*mean(lambda)^2))^-1
}

Lambda = matrix(c(1, 0, 3,
                  0, 1, 4,
                  1, 4, 1,
                  1, 1, 2), nrow = 4, byrow = TRUE)

Gamma = matrix(c(3,   0.5, 0.1,
                 0.5,   1, 0.1,
                 0.1, 0.1,   1), nrow = 3, byrow = TRUE)

Sigma = matrix(c(  1, 0.1, 0.1, 0.1,
                 0.1,   1, 0.1, 0.1,
                 0.1, 0.1,   1, 0.1,
                 0.1, 0.1, 0.1,   1), nrow = 4, byrow = TRUE)

weights = function(Lambda, Gamma, Sigma, I, i) {
  
    c((I %*% Lambda %*% Gamma[ , i])/
      (I %*% ((Lambda %*% Gamma %*% t(Lambda)) + Sigma) %*% I))
}


risk = function(Lambda, Gamma, Sigma, I, i) {
  
  Gamma[i, i] - c((I %*% Lambda %*% Gamma[ , i])^2/
      (I %*% ((Lambda %*% Gamma %*% t(Lambda)) + Sigma) %*% I))
}


r2 = function(Lambda, Gamma, Sigma, I, i) {
  
  c((I %*% Lambda %*% Gamma[ , i])^2/
      (I %*% ((Lambda %*% Gamma %*% t(Lambda)) + Sigma) %*% I))/Gamma[i, i]
}


lambda = 2:5
Lambda = matrix(lambda, nrow = 4, byrow = TRUE)
sigma = 4:1
Sigma = diag(sigma^2)
Gamma = matrix(c(1), nrow = 1, byrow = TRUE)
k = 4
I = c(1, 1, 1, 1)
i = 1

weights(Lambda, Gamma, Sigma, c(1, 1, 1, 1), 1)
mean(lambda)*(mean(sigma^2) + k*mean(lambda)^2)^-1

risk(Lambda, Gamma, Sigma, I, i)
risk_ss(lambda, sigma)

r2(Lambda, Gamma, Sigma, c(1, 1, 1, 1), 1)
omega(lambda, sigma)
alpha(lambda, sigma)
