set.seed(313)
sims = replicate(100000, {
  k = 5
  lambda = rexp(k, 1)
  sigma = rexp(k, 1)
  rank(-rel(lambda, sigma))
})

rowMeans(sims)
mean(sims[3, ] == 2)
mean(sims[2, ] > sims[3, ])

set.seed(313)
sims = replicate(100000, {
  k = 5
  lambda = rnorm(k, mean = 0, sd = 1)^2
  sigma = truncnorm::rtruncnorm(k, a = 0, b = Inf, mean = 0, sd = 1)
  rank(-rel(lambda, sigma))
})

rowMeans(sims)
mean(sims[1, ] == 2)
mean(sims[2, ] > sims[3, ])


set.seed(313)
sims = replicate(100000, {
  k = 5
  lambda = rep(1, k)
  sigma = rexp(k, 1)
  rank(-rel(lambda, sigma))
})

rowMeans(sims)
mean(sims[1, ] == 2)
mean(sims[2, ] == 3)
mean(sims[1, ] > sims[3, ])
