data{
    int<lower=1> N;
    vector[N] x;
    vector[N] obsy;
    vector[N] erry;
}
transformed data{
    vector[N] vary;
    for (i in 1:N){
      vary[i] = fabs(erry[i]);
    }
}
parameters{
    real a;
    real b1;
    real<lower=0> sigma;
    vector[N] y;
}
transformed parameters{
    vector[N] mu;
    for ( i in 1:N ) {
        mu[i] = a + b1 * x[i];
    }
}
model{
    sigma ~ cauchy( 0 , 1 );    // Prior on scatter
    b1 ~ normal( 0 , 10 );      // prior on slope
    a ~ normal( 0 , 10 );       // prior on intercept
    y ~ normal( mu , sigma );
    obsy ~ normal( y , vary );
}
