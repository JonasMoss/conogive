n = 5000
lambda = 3
sigma = 6
z = rnorm(n)
x = cbind(-lambda*z + sigma*rnorm(n),
          -lambda*z + sigma*rnorm(n),
          lambda*z + sigma*rnorm(n),
          lambda*z + sigma*rnorm(n))

tr = function(mat) sum(diag(mat))

ML = function(x) {
  k = ncol(x)
  
  mean = colMeans(x)
  
  objective = function(p) {
    sigma = p[1]
    lambda = p[2]
    Sigma = diag(k)*sigma^2 + rep(lambda, k) %*% t(rep(lambda, k))
    -sum(mvtnorm::dmvnorm(x, mean = mean, sigma = Sigma, log = TRUE))
  } 
  
  S = cov(x)
  lambda_start = sqrt(abs((sum(S) - tr(S))/(k*(k-1))))
  sigma_start = sqrt((tr(S) - k*lambda_start^2)/k)
  
  ui = matrix(c(1, 0,
                0, 1), byrow = TRUE, nrow = 2)
  ci = c(0, 0)
  constrOptim(f = objective, theta = c(sigma = sigma_start , 
                                       lambda = lambda_start),
              ui = ui, ci = ci, grad = NULL)
  
}


ML(x)
S = cov(x)
sqrt((tr(S) - sum(S)/k)/(k-1))
sqrt((sum(S) - tr(S))/(k*(k-1)))

cont
f = function(a = 1) a +2
y <- f(a = x <- 5 -> y) - y -> x

y <- (rnorm(y <- 100000 -> x, 3 -> x, y <- x^2 -> x) - sqrt(x))/x -> x

x = rep(1, 100)
x -> attr(x, "cool") -> x

f = function(a = 1, b = 1, c = 1) {
  force(b)
  force(c)
  force(a)
}


rel_congen = function(lambda, sigma) {
  k = length(lambda)
  a = k * mean(lambda)^2
  b = mean(sigma^2)
  a/(a + b)
}

rel_h = function(lambda, sigma) {
  a = sum(lambda^2/sigma^2)
  b = 1
  a/(a + b)
}

rel_1 = function(lambda, sigma) {
  k = length(lambda)
  a = k*mean(lambda/sigma)^2
  b = 1
  a/(a + b)
}

rel_2 = function(lambda, sigma) {
  k = length(lambda)
  a = k * mean(lambda / sqrt(lambda^2 + sigma^2))^2
  b = mean(sigma^2 / (lambda^2 + sigma^2))
  a/(a + b)
}

rel = function(lambda, sigma) {
  c("sigma" = rel_1(lambda, sigma),
    "standardized" = rel_2(lambda, sigma),
    "congeneric" = rel_congen(lambda, sigma),
    "h" = rel_h(lambda, sigma))
}


k = 5
lambda = rep(1, k)
sigma = rep(1, k)
rel(lambda, sigma)

k = 5
lambda = rexp(k, 1)
sigma = rep(1, k)
rel(lambda, sigma)

k = 5
lambda = rep(1, k)
sigma = rexp(k, 1)
rel(lambda, sigma)


k = 5
lambda = rep(1, k)
sigma = rexp(k, 1)
rel(lambda, sigma)

k = 5
lambda = rexp(k, 1)
sigma = rexp(k, 1)
rel(lambda, sigma)


x = c(1, 1, 1, 2)
x = runif(13)
k = length(x)

mean(x^2)
sum(x^2/(x^2 + 1))/sum(1/(x^2 + 1))

sum(1/sqrt(x^2 + 1))^2
sum(1/(x^2 + 1))*k

x = c(1, 2)
sum(x*c(0.1, 0.9))
mean(x)
