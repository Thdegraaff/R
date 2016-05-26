  ####################################################
  # Created:   	  April 04, 2016
  # By: 		  Thomas de Graaff
  # For: 		  Social Interaction and Crime paper
  # Last edited:  April 04, 2016
  # Description:  Estimates social interaction with 
  #               crime rates fixed
  ####################################################
  
  ####################################################
  # System parameters and packages 
  ####################################################
  library("dplyr")
  library("tidyr")
  library("foreign")
  library("ggplot2")
  library("rootSolve")
  library("AER")
  library("reshape2")
  library("quantreg")
  library("zoo")
  source("./prog/R/iterationDirectIV.R")
  source("./prog/R/CharacteristicsEq.R")
  source("./prog/R/MakeFig.R")
  source("./prog/R/FindEquilibria.R")
  ####################################################
  # Choose crimetype ("crime", "property" or "violent")
  ####################################################
  cr <- "crime"
  ####################################################
  # Choose whether estimation for only the youth
  ####################################################  
  youth <- 0
  ####################################################
  # Choose whether only for municipality averages
  ####################################################   
  mun <- 0
  ####################################################
  # Read and manipulate data (still manual selection!)
  ####################################################
  datatemp <- 0
  if (youth) {
    data <- read.csv("./Data/Thomas_data_PC4_crime_youth.csv", header=TRUE, sep = ",")
  } else {
    data <- read.csv("./Data/Thomas_data_PC4_crime.csv", header=TRUE, sep = ",")
  }
  data <- data %>%  # fill in crime type
            mutate(pfield = pfieldcrime, 
                   interaction = pfield * addrdens,
                   alpha = alpha_crime,
                   se = sealpha_crime
                   ) %>%
                   filter(!is.na(pfield))
  dataindividual <- read.dta(paste0("./Data/hat_any",cr,"2006.dta"))
  dataindividual_j <- read.dta(paste0("./Data/hat_any",cr,"2006_jongeren.dta"))
  data <- data %>%
    group_by(gemcode) %>%
    mutate(
      onepermean=weighted.mean(oneperdens, tot_bev, na.rm = TRUE),
      oneparentmean=weighted.mean(oneparentdens, tot_bev, na.rm = TRUE),
      perperhhmean=weighted.mean(perperhh, tot_bev, na.rm = TRUE),
      educationmean=weighted.mean(opleiding, tot_bev, na.rm = TRUE),
      socclassmean=weighted.mean(socklasse, tot_bev, na.rm = TRUE),
      twoearnmean=weighted.mean(k_tweeverd, tot_bev, na.rm = TRUE),
      outmigmean=weighted.mean(v_uit_perc, tot_bev, na.rm = TRUE),
      inmigmean=weighted.mean(v_in_perc, tot_bev, na.rm = TRUE),
      houseownmean=weighted.mean(perchouseown, tot_bev, na.rm = TRUE),	
      polavailmean=weighted.mean(polavail_mean_2005, tot_bev, na.rm = TRUE)
    )
  ### New dataset to be used for quantile regression, keep only the missing values
  data_total <- data %>% filter(is.na(alpha))
  data_total$alpha <- na.fill(data_total$alpha,-3.5)
  data_total$se <- na.fill(data_total$se, 1)  
  ####################################################
  # Spatial IV--only to check the spatial instrument once.
  # Not necessary for the rest of the application
  ####################################################
  #   formcrime <- alpha~addrdens + oneperdens + oneparentdens+
  #     perperhh + opleiding + socklasse + k_tweeverd + 
  #     v_uit_perc + v_in_perc + schooldens + perchouseown + shops + polavail_mean_2005+pfield+interaction |
  #     addrdens + oneperdens + oneparentdens +
  #     perperhh + opleiding + socklasse + k_tweeverd + v_uit_perc + v_in_perc + 
  #     schooldens + perchouseown + shops + polavail_mean_2005 + 
  #     Lagpfieldcrime1 + Lagpfieldcrime2 + Lagpfieldcrime1* addrdens + Lagpfieldcrime2* addrdens
  #   summary(ivreg(formula=formcrime, data = data, weights = 1/se))
  ####################################################
  # Fill in specification of model
  # Depending on own specification
  ####################################################
  forminit  <- alpha~addrdens + oneperdens +oneparentdens +
    perperhh + opleiding + socklasse + k_tweeverd + 
    v_uit_perc + v_in_perc + schooldens + perchouseown + shops + polavail_mean_2005+pfield+interaction
  formcrime <- alpha~addrdens + oneperdens + oneparentdens+
    perperhh + opleiding + socklasse + k_tweeverd + 
    v_uit_perc + v_in_perc + schooldens + perchouseown + shops + polavail_mean_2005+pfield+interaction |
    addrdens + oneperdens + oneparentdens +
    perperhh + opleiding + socklasse + k_tweeverd + v_uit_perc + v_in_perc + 
    schooldens + perchouseown + shops + polavail_mean_2005+instrument+instrinter
  forminitmun <- alpha~addrdens + schooldens + shops + onepermean + oneparentmean + perperhhmean + 
    educationmean  + socclassmean + twoearnmean + outmigmean + inmigmean  + houseownmean +
    polavailmean+pfield+interaction
  formcrimemun <- alpha ~ addrdens + schooldens + shops + onepermean + oneparentmean + perperhhmean + 
    educationmean  + socclassmean + twoearnmean + outmigmean + inmigmean  + houseownmean + polavailmean + pfield+interaction|
    addrdens + schooldens + shops + onepermean + oneparentmean + perperhhmean + 
    educationmean  + socclassmean + twoearnmean + outmigmean + inmigmean  + houseownmean + polavailmean + instrument + instrinter
  
  formhelprq1 <- pfield~addrdens + oneperdens +  oneparentdens +
    perperhh + opleiding + socklasse + k_tweeverd + 
    v_uit_perc + v_in_perc + schooldens + perchouseown + shops + polavail_mean_2005 + instrument + instrinter
  formhelprq2 <- interaction~addrdens + oneperdens + oneparentdens +
    perperhh + opleiding + socklasse + k_tweeverd + 
    v_uit_perc + v_in_perc + schooldens + perchouseown + shops + polavail_mean_2005+ instrument + instrinter  
  formrq <- alpha~addrdens + oneperdens +  oneparentdens +
    perperhh + opleiding + socklasse + k_tweeverd + 
    v_uit_perc + v_in_perc + schooldens + perchouseown + shops + polavail_mean_2005+
    pfield+interaction + poly(v1,4) + poly(v2,4)    
  
  formhelprq1mun <- pfield~addrdens + schooldens + shops + onepermean + oneparentmean +
    perperhhmean + educationmean  + socclassmean + twoearnmean + outmigmean + inmigmean  + houseownmean +
    polavailmean + instrument + instrinter
  formhelprq2mun <- interaction~addrdens + schooldens + shops + onepermean + oneparentmean +
    perperhhmean + educationmean  + socclassmean + twoearnmean + outmigmean + inmigmean  + houseownmean +
    polavailmean + instrument + instrinter  
  formrqmun <- alpha~addrdens + schooldens + shops + onepermean + oneparentmean +
    perperhhmean + educationmean  + socclassmean + twoearnmean + outmigmean + inmigmean  + houseownmean +
    polavailmean+pfield+interaction + poly(v1,4) + poly(v2,4)
  data_total<- select(data_total, pc4, alpha, se, addrdens, oneperdens, oneparentdens, 
                  perperhh, opleiding, 
                   socklasse,k_tweeverd, v_uit_perc, v_in_perc,
                   schooldens, perchouseown, shops, polavail_mean_2005, pfield, interaction, 
                   onepermean, oneparentmean, perperhhmean,  
                   educationmean, socclassmean, twoearnmean, outmigmean, inmigmean, houseownmean,
                   polavailmean)
  data <- select(data, pc4, alpha, se, addrdens, oneperdens, oneparentdens, 
                    perperhh, opleiding, 
                    socklasse,k_tweeverd, v_uit_perc, v_in_perc,
                    schooldens, perchouseown, shops, polavail_mean_2005, pfield, interaction, 
                    onepermean, oneparentmean, perperhhmean,  
                    educationmean, socclassmean, twoearnmean, outmigmean, inmigmean, houseownmean,
                    polavailmean)
  dataindividual$directions.foreign <- factor(dataindividual$foreign)
  dataindividual$foreign <- as.numeric(dataindividual$directions.foreign) - 1
  dataindividual_j$directions.foreign <- factor(dataindividual_j$foreign)
  dataindividual_j$foreign <- as.numeric(dataindividual_j$directions.foreign) - 1
  ####################################################
  # call functions to function iteration2sls()
  # to estimate the 2SLS procedure
  ####################################################
  if (youth) {
    if (mun) {
      output <- iteration2sls(dataindividual_j, data, data_total, formcrimemun, forminitmun, formhelprq1mun, formhelprq2mun, formrqmun, youth=TRUE, initvalue=0)
      summary(output$iv)
      # summary(output$rqout)      
    } else { 
      output <- iteration2sls(dataindividual_j, data, data_total, formcrime, forminit, formhelprq1, formhelprq2, formrq, youth=TRUE, initvalue=0)
      summary(output$iv)
      # summary(output$rqout)      
    }
  } else {
    if (mun) {
      output <- iteration2sls(dataindividual, data, data_total, formcrimemun, forminitmun, formhelprq1mun, formhelprq2mun, formrqmun, youth=FALSE, initvalue=0)
      summary(output$iv)
      # summary(output$rqout)      
    } else { 
      output <- iteration2sls(dataindividual, data, data_total, formcrime, forminit, formhelprq1, formhelprq2, formrq, youth=FALSE, initvalue=0)
      summary(output$iv)
      # summary(output$rqout)
    }
  }
  ####################################################
  # make figure
  #####################################################
  # myplot <- makefig(output)
  # pdf("./Paper/Version4/img/equilibriaTemp.pdf")
  # myplot
  # dev.off()
  ####################################################
  # Find all equilibria
  # And other outcome characteristics
  ####################################################
  #   matrices <- findequilibria(output)
  #   counteq(matrices$cmat)
  #   counteqlow(output$instrument)
  #   percclose(datatemp$pfield, output$instrument, 0.025)
  ####################################################
  # Simulation with initial values
  # check whehter initial values make an impact
  # Be careful; time consuming (> 5 minutes)
  ####################################################
  #       values <- seq(0, 1, 0.02)
  #     coefvector <- 0
  #     for (i in values){
  #       output <- iteration2sls(dataindividual_j, data, data_total, formcrime, forminit, formhelprq1, formhelprq2, formrq, youth=TRUE, initvalue=i)
  #       coefvector <- rbind(coefvector, output$iv$coefficients["pfield"])
  #     }
  #     coefvector <- coefvector[-1]
  #     dataplot <- data.frame(values, coefvector)
  #     p <- ggplot(data=dataplot, aes(x = values, y = coefvector))  +geom_point(shape = 1) +
  #       geom_smooth(method = "lm", formula = y~x, se = FALSE, colour = "black") + 
  #       scale_y_continuous(breaks = seq(0, 0.5, 0.00001), limits = c(0.1108,0.1109)) +
  #       labs(x = "Initial value of gamma", y = "Final estimation of gamma") + 
  #       theme_bw() +
  #       theme(panel.grid.minor = element_blank(),
  #             plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5))
  #     pdf("./Paper/Version4/img/initialvalues.pdf")
  #     p
  #     dev.off()