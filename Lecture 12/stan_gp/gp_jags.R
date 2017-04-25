gp_sq_exp_model = "model{
  y ~ dmnorm(mu, inverse(Sigma))

  for (i in 1:N) {
    mu[i] <- 0
  }

  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      Sigma[i,j] <- sigma2 * exp(- pow(l*d[i,j],2))
      Sigma[j,i] <- Sigma[i,j]
    }
  }

  for (k in 1:N) {
    Sigma[k,k] <- sigma2 + sigma2_w
  }

  sigma2   ~ dlnorm(0, 1)
  sigma2_w ~ dlnorm(0, 1)
  l        ~ dt(0, 2.5, 1) T(0,) # Half-cauchy(0,2.5)
}"

m = jags.model(
  textConnection(gp_sq_exp_model), 
  data = list(
    y = (d$y),
    d = dist(d$t) %>% as.matrix(),
    N = length(d$y)
  ),
  quiet = TRUE
)

update(m, n.iter=10000)#, progress.bar="none")

exp_cov_coda = coda.samples(
  m, variable.names=c("sigma2", "l", "sigma2_w"),
  n.iter=50000, thin=50
)
