iteration2sls <- function(dataind, data, datatot, formiv, formols, formrqinst1, formrqinst2, formrq, youth = TRUE, initvalue=0){
  # Computes the iterated Bayer 2SLS procedure and gives for each iteration the 
  # instrument and the sigmoid function.
  #
  # Args:
  #   dataind: Individual dataset containing information on gender, age and ethnicity
  #   data: The neighborhood data containing info on neighborhood crime rate and 
  #         other characteristics
  #   datatot: The neighborhood data containing info on neighborhood crime rate and 
  #         other characteristics including the neighborhood with zero crime
  #   formiv:  The specification for the 2SLS procedure
  #   formols: The specification for the OLS estimation (for initialision of alpha 
  #             and beta parameters)
  #   formrqinst1: The specification for the control function of the first intrument
  #   formrqinst2: The specification for the control function of the second intrument 
  #   formrq: The specification for the median regression (with tau = 0.5)
  #   youth: Indicates whether sample is for youth only
  #   initvalue: coefficient for pfield and interaction for initial OLS
  #
  # Returns a list:
  #   iv: equilibrium IV estimation results
  #   phi: alpha and beta of the sigmaoid function for each iteration
  #   instrument: matrix of instruments for each iteration
  #   alpha_est: estimates of neighborhoodspecific level effects 
  #   olsout: initial OLS results
  #   rqout: median (quantile) regression output
  #
  ########################################################################################
  # Specific for all or for youth only (age2 disappears for the latter)
  ########################################################################################    
  if (youth) {
  dataind$hat1 <- dataind$bfemale*dataind$female + dataind$bforeign*dataind$foreign +  dataind$b_cons + 
    dataind$bcage20cat*dataind$cage20cat 
  }
  else {
    dataind$hat1 <- dataind$bfemale*dataind$female + dataind$bforeign*dataind$foreign +  dataind$b_cons + 
      dataind$bcage20cat*dataind$cage20cat + dataind$bcage20cat2*dataind$cage20cat2 
  }
  ########################################################################################
  # Initialisation of data and OLS
  ########################################################################################    
  datahat <- data
  datahat <- na.omit(datahat)  
  ols<-lm(formula=formols, data = datahat, weights = 1/se)
  print(summary(ols))
  
  ########################################################################################
  # Specific for all or for youth only (age2 disappears for the latter)
  ########################################################################################    
  datahat$alphahat <- ols$fitted.values
  datahat$alphahat <- datahat$alphahat - coef(ols)["pfield"]*datahat$pfield - coef(ols)["interaction"]*datahat$interaction 
  datahat$alphahat <- datahat$alphahat + initvalue*datahat$pfield + initvalue*datahat$interaction   
  datahat_temp <- left_join(datahat, dataind, by="pc4")
  datahat_temp <- select(datahat_temp, pc4, hat, hat1, frequency, alphahat, totalpop)
  datahat_temp$instrument <- exp(datahat_temp$hat1+datahat_temp$alphahat)/(1+exp(datahat_temp$hat1+datahat_temp$alphahat))
  datahat_temp <- datahat_temp %>% group_by(pc4) %>% summarize(instrument = weighted.mean(instrument,frequency), hatpc4=mean(hat1))
  datahat <- left_join(datahat_temp, data, by="pc4")
  datahat <- select(datahat, pc4, alpha, se, addrdens, oneperdens,oneparentdens, hatpc4,
                    perperhh, opleiding, 
                    socklasse,k_tweeverd, v_uit_perc, v_in_perc,
                    schooldens, perchouseown, shops, polavail_mean_2005, pfield, interaction, instrument,
                    onepermean, oneparentmean, perperhhmean,  
                    educationmean, socclassmean, twoearnmean, outmigmean, inmigmean, houseownmean,
                    polavailmean)
  criterium1 <- sum((datahat$pfield - datahat$instrument), na.rm=TRUE)
  instrument_old = datahat$instrument
  phimat <- rep(0, length(instrument_old))
  instrumentmat <- rep(0, length(instrument_old))
  ########################################################################################
  # We need the following two global variables to calculate figures 
  # and keep track of representative postcode with correct dimensions
  ########################################################################################
  addresdensity <<- datahat$addrdens
  postcode <<- datahat$pc4
  ########################################################################################
  # Start iteration
  ########################################################################################  
  while(criterium1 > 0.000001) {
    datahat$instrinter = datahat$instrument * datahat$addrdens
    iv<-ivreg(formula=formiv, data = datahat, weights = 1/se)
    alphahat <- iv$fitted.values
    instrument_eq <- datahat$instrument  
    
    datahat$alphahat_new <- alphahat - coef(iv)["pfield"]*datahat$pfield - coef(iv)["interaction"]*datahat$interaction
    datahat_temp <- left_join(datahat, dataind, by="pc4")
    datahat_temp <- select(datahat_temp, pc4, hat, hat1, frequency, alphahat_new, totalpop)
    datahat_temp <- datahat_temp %>% group_by(pc4) %>% summarize(hatpc4=weighted.mean(hat1,frequency))
    phi <- datahat_temp$hatpc4 + datahat$alphahat_new
    temp <- coef(iv)["pfield"]*100 + coef(iv)["interaction"]*100*datahat$addrdens
    for (i in 1:length(alphahat)) {
      fun <- function(x) (x - exp(phi[i]+temp[i]*x)/(1+exp(phi[i]+temp[i]*x)))
      uni <- uniroot.all(fun, c(0, 1)) # Make use of the awesome rootSolve package!!
      if (length(uni)==0) {
        uni <- 1
      }
        instrument_eq[i] <- min(uni)
      }
    datahat$instrument <- 1 * instrument_eq + 0 * datahat$instrument
    criterium1 <- sum((instrument_old - instrument_eq)^2, na.rm=TRUE)
    instrument_old <- instrument_eq
    print(paste("Criterium value is now : ", criterium1))
    phi <- datahat_temp$hatpc4 + datahat$alphahat_new
    phimat <- rbind(phimat, phi)
    instrumentmat <- rbind(instrumentmat, datahat$instrument)
  }
  ########################################################################################
  # Fill matrices for necessary information for later characteristics of the equilibria.
  ########################################################################################
  phimat <- phimat[-1,]
  instrumentmat <- instrumentmat[-1,]
  datatemp <<- datahat
  ########################################################################################
  # Find instruments for missing values
  # Note that this works because we first find the instrument based on previous estimates
  # and then in a later stage we do a quantile regression based on the total dataset
  # where the values for missing alpha's are imputed
  ########################################################################################
  datatot$instrument <- 0
  datatot$instrinter <- 0
  datatot <- na.omit(datatot)
  
  alphahat <- predict(iv,datatot)
  instrument_eq <- datatot$instrument
  datatot$alphahat_new <- alphahat - coef(iv)["pfield"]*datatot$pfield - coef(iv)["interaction"]*datatot$interaction
  datatot_temp <- left_join(datatot, dataind, by="pc4")
  datatot_temp <- select(datatot_temp, pc4, hat, hat1, frequency, alphahat_new, totalpop)
  datatot_temp <- datatot_temp %>% group_by(pc4) %>% summarize(hatpc4=weighted.mean(hat1,frequency))
  phi <- datatot_temp$hatpc4 + datatot$alphahat_new
  temp <- coef(iv)["pfield"]*100 + coef(iv)["interaction"]*100*datatot$addrdens
  for (i in 1:length(alphahat)) {
    fun <- function(x) (x - exp(phi[i]+temp[i]*x)/(1+exp(phi[i]+temp[i]*x)))
    uni <- uniroot.all(fun, c(0, 1))
    if (length(uni)==0) {
      uni <- 1
    }
    instrument_eq[i] <- min(uni)
  }
  datatot$instrument <- instrument_eq 
  
    # report the implied mean for missing crime rate as requested by referees
  phi <- datatot_temp$hatpc4 - 3.5
  ImpliedCrimeRate <- exp(phi)/(1+exp(phi))
  print(paste("The implied crime rate for missing neighborhood fixed efffects is:", mean(ImpliedCrimeRate)))
  
  datatot$instrinter <- datatot$instrument * datatot$addrdens
  datahat <- bind_rows(datahat, datatot)
  ########################################################################################
  # Quantile IV regression where datahat now exists of imputed values as well
  ########################################################################################
  rqinst1 <- rq(formrqinst1, tau=.5, data = datahat, weights = 1/se)
  rqinst2 <- rq(formrqinst2, tau=.5, data = datahat, weights = 1/se)
  datahat$v1 <- datahat$pfield  - predict.rq(rqinst1)
  datahat$v2 <- datahat$interaction  - predict.rq(rqinst2)
  rqoutput <- rq(formrq, tau=.5, data = datahat, weights = 1/se)
  ############################################################################
  ############## Below code only for first stage regressions #################
  ############################################################################
  #   f1 <- lm(pfield~addrdens + oneperdens +oneparentdens + perperhh + opleiding + socklasse + k_tweeverd + v_uit_perc + v_in_perc + 
  #              schooldens + perchouseown + shops + polavail_mean_2005+instrument+instrinter, data = datahat, weights = 1/se)
  #   print(summary(f1))
  #   f2 <- lm(interaction~addrdens + oneperdens +oneparentdens + perperhh + opleiding + socklasse + k_tweeverd + v_uit_perc + v_in_perc + 
  #              schooldens + perchouseown + shops + polavail_mean_2005+instrument+instrinter, data = datahat, weights = 1/se)
  #   print(summary(f2))  
  ############################################################################
  ############# Return output ################################################
  ############################################################################
  return(list("iv"= iv,"phi"=phimat, "instrument"=instrumentmat, 
              "alpha_est"=datahat$alpha,"olsout"=ols, "rqout"= rqoutput))
}