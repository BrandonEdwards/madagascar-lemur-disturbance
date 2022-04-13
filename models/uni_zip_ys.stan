data {
  int <lower=0> N;
  int <lower = 0> y[N];
  int <lower = 0> n_seasons;
  int <lower = 1> season[N];
  int <lower = 1> n_years;
  int <lower = 1> year[N];
}
parameters {
  real<lower=0, upper=1> theta[n_seasons, n_years];
  real<lower=0> lambda[n_seasons, n_years];
}
model {
  for (n in 1:N) {
    if (y[n] == 0) {
      target += log_sum_exp(bernoulli_lpmf(1 | theta[season[n], year[n]]),
                            bernoulli_lpmf(0 | theta[season[n], year[n]])
                            + poisson_lpmf(y[n] | lambda[season[n], year[n]]));
    } else {
      target += bernoulli_lpmf(0 | theta[season[n], year[n]])
      + poisson_lpmf(y[n] | lambda[season[n], year[n]]);
    }
  }
}
