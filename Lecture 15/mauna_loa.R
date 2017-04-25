library(rjags)
library(magrittr)
library(dplyr)
library(ggplot2)

strip_attrs = function(obj)
{
  attributes(obj) = NULL
  obj
}

loa = data_frame(
  y = co2 %>% strip_attrs(),
  x = time(co2) %>% strip_attrs()
)

ggplot(loa, aes(x=x,y=y)) + geom_line()
    


m = jags.model(
  textConnection(gp_model), 
  data = list(
    mu = mean(loa$y) * rep(1, nrow(loa)),
    y = loa$y,
    #x = loa$x,
    d = dist(loa$x) %>% as.matrix(),
    per = 1,
    pi = pi
  ),
  inits = list(
    sigma2 = c(66, 2.4, 0.66, 0.18, 0.19)^2,
    l = 1/c(67, 90, 1.3, 1.2, 1.6/12),
    alpha = 0.78
  ),
  n.adapt=1000
)

save(m, file="init_model.Rdata")

update(m, n.iter=10000)#, progress.bar="none")

gp_coda = coda.samples(
  m, variable.names=c("sigma2", "l", "alpha"),
  n.iter=1000
)

save(gp_coda, file="fit_model.Rdata")
