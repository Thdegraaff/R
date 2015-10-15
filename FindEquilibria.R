findequilibria <- function(estoutput){
  # Finds for every iteration and neighborhood all equilibria
  #
  # Args:
  #   EstOutput: a list with outout from iteration2sls procedure
  #
  # Returns:
  #  countmat: A matrix with the number of equilibria for all iterations
  #  loweq: A matrix with all low equilibria for all iterations
  #  higheq: A matrix with all high equilibria for all iterations
  temp <- coef(estoutput$iv)["pfield"]*100 + coef(estoutput$iv)["interaction"]*100*addresdensity
  countmat <- matrix(0, nrow(estoutput$phi), ncol(estoutput$phi))
  loweq <- matrix(0, nrow(estoutput$phi), ncol(estoutput$phi))
  higheq <- matrix(0, nrow(estoutput$phi), ncol(estoutput$phi))
  for (j in 1:nrow(estoutput$phi)) {
    for (i in 1:length(addresdensity)) {
      fun <- function(x) (x - exp(estoutput$phi[j,i]+temp[i]*x)/(1+exp(estoutput$phi[j,i]+temp[i]*x)))
      uni <- uniroot.all(fun, c(0, 1))
      countmat[j,i] <- length(uni)
      loweq[j,i] <- head(uni,1)
      higheq[j,i] <- tail(uni,1)
    }
  }
  return(list("cmat"=countmat, "lmat"=loweq, "hmat"= higheq))
}