crps_test = function(post,obs,n_pts = 1e6)
{
  F_post = ecdf(post)
  F_obs = ecdf(obs)

  d = c(obs,post)
  s = seq(min(d),max(d),len=n_pts)

  sum( (F_post(s) - F_obs(s))^2 ) * (max(d)-min(d)) / n_pts
}

calc_crps = function(post, obs)
{
  stopifnot(length(obs) == 1)

  if (is.na(obs))
  {
    return(NA)
  }

  x = sort(post)
  n = length(x)

  i = which(x > obs)[1] - 1
  if (is.na(i))
    i = n

  Fx = ecdf(x)(x)

  widths = x[-1]-x[-n]

  if (i == 0) { # obs to the left of all post samples
    widths = c(x[1]-obs,widths,0)
    Fx = c(0,Fx)
    crps = sum(widths * (1-Fx)^2)
  } else if (i==n) { # obs to the right of all post samples
    widths = c(widths, obs-x[n])
    crps = sum(widths * Fx^2)
  } else { # otherwise
    Fx = Fx[-n]

    below = which(1:(n-1) < i)
    above = which(1:(n-1) > i)

    crps = (  sum(widths[below] * Fx[below]^2)
            + sum(widths[above] * (1-Fx[above])^2)
            + (obs-x[i]) * Fx[i]^2
            + (x[i+1]-obs) * (1-Fx[i])^2 )
  }

  return(crps)
}