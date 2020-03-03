models = list(' y  =~ A1 + A2 + A3 + A4 + A5 ',
              ' y  =~ C1 + C2 + C3 + C4 + C5 ',
              ' y  =~ E1 + E2 + E3 + E4 + E5 ',
              ' y  =~ N1 + N2 + N3 + N4 + N5 ',
              ' y  =~ O1 + O2 + O3 + O4 + O5 ')

reliabilities = sapply(models, function(model) {
  fit <- lavaan::cfa(model, data = psychTools::bfi)
  coefs <- lavaan::lavInspect(fit, what = "x")
  lambda <- c(coefs$lambda * sqrt(as.numeric(coefs$psi)))
  sigma <- sqrt(diag(lavaan::lavInspect(fit, what = "x")$theta))
  omega_std(lambda, sigma) - alpha_std(abs(lambda) %*% t(abs(lambda)) + diag(sigma^2))
})


## Preparations.
model <- ' y  =~ A1 + A2 + A3 + A4 + A5 + O1 + O2 + O3 + O4 + O5'

fit <- lavaan::cfa(model, data = psychTools::bfi)
coefs <- lavaan::lavInspect(fit, what = "x")

lambda <- abs(c(coefs$lambda * sqrt(as.numeric(coefs$psi))))
sigma <- sqrt(diag(lavaan::lavInspect(fit, what = "x")$theta))
omega_std_true <- omega_std(lambda, sigma)
alpha_std_true <- psych::alpha(psychTools::bfi[, 1:5], check.keys=TRUE)$total$std.alpha
alpha_std_true - omega_std_true


lambda = runif(3)
sigma = runif(3)
w = 1/sqrt(lambda^2 + sigma^2)
bias(lambda, sigma, w = w)
omega_std(lambda, sigma) - alpha_std(tcrossprod(lambda, lambda) + diag(sigma^2))

a = t(w) %*% (tcrossprod(lambda, lambda) + diag(sigma^2)) %*% w
b = t(w) %*% diag(diag((tcrossprod(lambda, lambda) + diag(sigma^2)))) %*% w
k/(k-1)*(1 - b/a)
bias(lambda, sigma, w)
