library(rstan)

fit = stan(file="stan_gp/gp_fit.stan", data=list(x=d$t,N=nrow(d),y=d$y),
           iter=500, chains=4)

fit_ss = extract(fit, permuted=TRUE)

traceplot(fit)


fit_ss$eta_sq %>% median()
# [1] 10.42861
fit_ss$rho_sq %>% median() %>% sqrt()
# [1] 3.743101