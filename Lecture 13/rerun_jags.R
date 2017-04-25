library(rjags)
library(magrittr)
library(dplyr)

load("../Lecture 1/data/frn_example.Rdata")

pm25 = pm25 %>%
  mutate(Date = lubridate::mdy(Date)) %>%
  mutate(day  = (Date - lubridate::mdy("1/1/2007") + 1) %>% as.integer()) %>% 
  select(-POC) %>%
  setNames(., tolower(names(.)))

gp_exp_model = "model{
y ~ dmnorm(mu, inverse(Sigma))

for (i in 1:N) {
mu[i] <- beta[1]+ beta[2] * x[i] + beta[3] * x[i]^2
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

for (i in 1:3) {
beta[i] ~ dt(0, 2.5, 1)
}
sigma2_w ~ dnorm(10, 1/25) T(0,)
sigma2   ~ dnorm(10, 1/25) T(0,)
l        ~ dt(0, 2.5, 1) T(0,)
}"

  m = jags.model(
    textConnection(gp_exp_model), 
    data = list(
      y = pm25$pm25,
      x = pm25$day,
      d = dist(pm25$day) %>% as.matrix(),
      N = nrow(pm25)
    ),
    n.adapt=5000,
    quiet = FALSE
  )
  
  update(m, n.iter=10000)#, progress.bar="none")
  
  exp_cov_coda = coda.samples(
    m, variable.names=c("beta", "sigma2", "l", "sigma2_w"),
    n.iter=25000, thin=35#, progress.bar="none"
  )
  save(exp_cov_coda, file="gp_jags_new.Rdata")
