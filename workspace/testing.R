rel_alpha_std = function(lambda, sigma) {
  S = lambda %*% t(lambda) + diag(sigma ^ 2)
  alpha_std(S)
}

rel_alpha = function(lambda, sigma) {
  S = lambda %*% t(lambda) + diag(sigma ^ 2)
  alpha(S)
}

set.seed(313)
sims = replicate(10000, {
  k = 10
  lambda = rexp(k, 1)
  sigma = rep(1, k)
  rels = c(rel_congen(lambda, sigma), 
    rel_alpha_std(lambda, sigma),
    rel_alpha(lambda, sigma))
  rank(-rels)
})

rowMeans(sims)



x = runif(10,1,2)
mean(1/x)^2
1/mean(x^2)


k = 5
a = 10
x = runif(k, 1,2)
y = sqrt(1/(x + a))
#y = runif(10, 0, 1/x)

mean((1+x^2)^(-1/2))^2*k/mean((1 + x^2)^(-1))

mean(y)^2*mean(x^2)
mean(x^2*y^2)

mean(x)^2*mean(1/(x^2 + 1))
mean(x * (1/(x^2 + 1)^(1/2)))^2


k = 5
a = 10
sigma = runif(k, 0, 2)
sum(sigma*(1 + sigma^2)^(-1/2))^2
sum((1 + sigma^2)^(-1/2))^2*mean(sigma^2)
sum((1 + sigma^2)^(-1/2))^2*1/mean(1/sigma)^2




m = function(x, w = rep(1/length(x), length(x))) sum(x * w)/sum(w)
h = function(x, w = rep(1/length(x), length(x))) 1/sum(w/sum(w)/x)


replicate(10000,  {
  x = sort(runif(10, 0, 1))
  w = 1/(1 + x)^10
  m(x) > m(x, w) & m(x, w) > h(x, 1/w) & h(x, 1/w) > h(x)
}) -> res


replicate(100000,  {
  x = sort(runif(10, 0, 100))
  w = 1/(1 + x)
  m(x) > m(x, w) & m(x, w) > h(x)
}) -> res

sum(res)

replicate(100000,  {
  x = sort(runif(10, 0, 10))
  w = 1/(1 + x^2)^2
  rel_1(rep(1, length(x)), x) > rel_2(rep(1, length(x)), x)
}) -> res

sum(res)


x = sort(runif(10, 0, 10))
w = 1/(1 + x^2)
m(x^2)
m(x^2, w)
m(x, w)^2
h(x^2, w)
h(x^2, 1/w)
h(x^2)
h(x)^2
sum(1/w)
