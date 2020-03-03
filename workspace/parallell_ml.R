n = 5000
lambda = 3
sigma = 6
z = rnorm(n)
x = cbind(lambda*z + sigma*rnorm(n),
          lambda*z + sigma*rnorm(n),
          lambda*z + sigma*rnorm(n),
          lambda*z + sigma*rnorm(n))

tr = function(mat) sum(diag(mat))

ML = function(x) {
  k = ncol(x)
  
  objective = function(p) {
    mu = p[1:k] 
    sigma = p[k+1]
    lambda = p[k+2]
    Sigma = diag(k)*sigma^2 + rep(lambda, k) %*% t(rep(lambda, k))
    -sum(mvtnorm::dmvnorm(x, mean = mu, sigma = Sigma, log = TRUE))
  } 
  
  S = cov(x)
  lambda_start = sqrt((sum(S) - tr(S))/(k*(k-1)))
  sigma_start = sqrt((tr(S) - k*lambda_start^2)/k)
  optim(fn = objective, par = c(mu = colMeans(x), 
                       sigma = sigma_start , 
                       lambda = lambda_start))
  
}


ML(x)
S = cov(x)
k = ncol(x)
pars = ML(x)$par
pars[5]^2 + pars[6]^2
1/k*tr(cov(x))*(n - 1)/n

s_tot = mean(rowSums(x)^2)
ssi = sum(colMeans(x^2))
1/(k*(k-1))*(k*ssi - s_tot)


1/(k*n)*sum((x - colMeans(x))^2)




s_tot = mean(rowSums(x)^2)
ssi = sum(colMeans(x^2))
1/(k*(k-1))*(k*ssi - s_tot)

g_mean = mean(x)/n
sst = sum((rowSums(x) - g_mean)^2)/(n - 1)
ssi = tr(cov(x))
(n-1)/(n*(k*(k-1)))*(k*ssi - sst)

1/(k*n)*sum((x - colMeans(x))^2)

lambda_start = sqrt((sum(S) - tr(S))/(k*(k-1)))
sigma_start = sqrt((tr(S) - k*lambda_start^2)/k)



## Testing
sigma = pars[5]
lambda = pars[6]
S = cov(x)

## Firs part
c(sum(S)/k, lambda^2*k + sigma^2)
c(tr(S)/k, lambda^2 + sigma^2)


S = cov(x)
left = function(S, sigma, lambda) {
  2*sigma^(-3)*tr(S) - 
    2*lambda^2*(1/(sigma^3*(lambda^2*k+sigma^2)) + 
                1/(sigma*(lambda^2*k+sigma^2)^2))*sum(S)
}

left2 = function(S, sigma, lambda) {
  2*sigma^(-3)*tr(S) - 
    2*lambda^2*(k/(sigma^3*sum(S)) + 
                  k^2/(sigma*sum(S)^2))*sum(S)
}

right = function(S, sigma, lambda) {
  2*k/(lambda^2*k + sigma^2)/sigma*(lambda^2*(k - 1) + sigma^2)
}

right2 = function(S, sigma, lambda) {
  2*k^2/(sum(S))/sigma*(sum(S)/k - lambda^2)
}

left(S, sigma, lambda) 
left2(S, sigma, lambda)
right(S, sigma, lambda) 
right2(S, sigma, lambda) 

f = function(S, sigma, lambda) {
  1/(sum(S))*(sigma^2*sum(S) + lambda^2*sum(S))
}

f(S, sigma, lambda)

(sum(S) - tr(S))/(k*(k-1))
lambda^2
(tr(S) - sum(S)/k)/(k-1)
sigma^2



###
### Alpha testing. 
###

k/(k-1)*(1 - tr(S)/sum(S))
k^2*lambda^2/(k^2*lambda^2 + k*sigma^2)
