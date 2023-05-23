MakeDist <- function(m, s, l, u){
  freqs <- msm::ptnorm(seq(0, 100, 1),
                       mean = m, sd = s,
                       lower = l, upper = u)
  freqs <- freqs - lag(freqs, default = 0)
  freqs <- freqs/sum(freqs)
  freqs
}

# Functions for truncted normal distributions
# https://www.r-bloggers.com/truncated-normal-distribution/
mean.tnorm<-function(mu,sd,lower,upper){
  ##return the expectation of a truncated normal distribution
  lower.std=(lower-mu)/sd
  upper.std=(upper-mu)/sd
  mean=mu+sd*(dnorm(lower.std)-dnorm(upper.std))/
    (pnorm(upper.std)-pnorm(lower.std))
  return(mean)
}
var.tnorm<-function(mu,sd,lower,upper){
  ##return the variance of a truncated normal distribution
  lower.std=(lower-mu)/sd
  upper.std=(upper-mu)/sd
  variance=sd^2*(1+(lower.std*dnorm(lower.std)-upper.std*dnorm(upper.std))/
                   (pnorm(upper.std)-pnorm(lower.std))-((dnorm(lower.std)-dnorm(upper.std))/
                                                          (pnorm(upper.std)-pnorm(lower.std)))^2)
  return(variance)
}