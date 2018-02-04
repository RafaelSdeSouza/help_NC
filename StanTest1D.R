require(rstan)
## Calibrate the spectra and tell me 26Mg energies of peaks

data <- read.table("CalibrationPoints.dat",header=TRUE)

## Build a dataframe that stan understands
dlist <- list(
    N=length(data$Brho),
    x=data$Channel,
    obsy=data$Brho,
    erry=data$uBrho
)
## README!!
## Seems like stan has a trouble with small sigma...1e-6 fails.
##dlist$erry <- rep(1e-4,length(dlist$obsy))

## The MCMC model
model1d.stan <- stan(file="stantest1d.stan", data=dlist,
                     warmup=1000, iter=5000, chains=3, cores=3,
                     control=list(max_treedepth=20))
densiplot(model1d.stan, pars = c("a", "b1","sigma"), inc_warmup = F, nrow = 2)

pairs(model1d.stan, pars = c("a", "b1","sigma"))
## Visualize
library(shinystan)
my_sso <- launch_shinystan(model1d.stan)

