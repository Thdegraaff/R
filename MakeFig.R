makefig <- function(estoutput){
  # Plots all sigmoids for each neighborhood.
  #
  # Args:
  #   EstOutput: a list with outout from iteration2sls procedure
  #
  # Returns:
  #   A ggplot2 object
  EC <- seq(0,1,0.01)
  temp <- coef(estoutput$iv)["pfield"]*100 + coef(estoutput$iv)["interaction"]*100*addresdensity
  tempmat <- EC%*%t(temp) + rep(1,101)%*%(tail(estoutput$phi,1))
  ECmapping <- exp(tempmat)/(1+exp(tempmat))
  figdata <- data.frame(EC, ECmapping )
  figdata_long <- melt(figdata, id="EC")
  myplot <- ggplot(figdata_long, aes(x=EC,y=value, colour =variable))  + geom_line() +ylim(0,1)
  myplot <- myplot  + geom_line(aes(x=EC,y=EC), size =1, colour = "black") + theme_bw() +
    theme(legend.position = "none") + 
    labs(x = "IE", y = "f(IE)")

  return(myplot)
}