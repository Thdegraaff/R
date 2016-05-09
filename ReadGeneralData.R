####################################################
# Created: 		May 12, 2015
# By: 			Thomas de Graaff
# For: 			Crime paper
# Last edited: 	August 17, 2015
####################################################

####################################################
# System parameters and packages 
####################################################
library("dplyr")
library("tidyr")
library("foreign")
####################################################
# Read in data for all crime
####################################################
data <- read.csv("./Data/Thomas_data_PC4.csv", header=TRUE, sep = ",")
data <- data %>%
          mutate(
            addrdens   	    = (tot_hh/opptotaalhectare)/100,
            schooldens 		  = scholenbasisonderwijsper10000inw,
            percrentcontrol = percentagehuurtoeslagontvangersh,
            perchouseown	  = percvoorraadkoopsector/100,
            perchouserent	  = percvoorraadhuursector,
            percsocialrent	= percvoorraadsocialehuursector,
            percmigrants	  = tot_alloch/tot_bev,
            percwmigrants	  = w_alloch/tot_bev,
            percnwmigrants	= nw_alloch/tot_bev,
            shops			      = verkooppuntentotaalwinkels/100,
            perperhh		    = persperhh,
            oneperdens		  = eenpersoon/huishouden,
            oneparentdens	  = Eenouder/huishouden,
            perc_5y			    = l0_5jaar/tot_bev,
            perc5_10y		    = l5_10jaar/tot_bev,
            perc10_15y		  = l10_15jaar/tot_bev,
            perc15_20y		  = l15_20jaar/tot_bev,
            perc20_25y		  = l20_25jaar/tot_bev,
            perc25_30y		  = l25_30jaar/tot_bev,
            perc30_35y		  = l30_35jaar/tot_bev,
            perc35_40y		  = l35_40jaar/tot_bev,
            perc40_45y		  = l40_45jaar/tot_bev,
            perc45_50y		  = l45_50jaar/tot_bev,
            perc50_55y		  = l50_55_jaa/tot_bev,
            perc55_60y		  = l55_60jaar/tot_bev,
            perc60_65y		  = l60_65jaar/tot_bev,
            perc65_70y		  = l65_70jaar/tot_bev,
            perc70_75y		  = l70_75jaar/tot_bev,
            perc75_80y		  = l75_80jaar/tot_bev,
            perc80_85y		  = l80_85jaar/tot_bev,
            perc85_90y		  = l85_90jaar/tot_bev,
            perc90_95y		  = l90_95jaar/tot_bev,
            perc95_y		    = l95ofouder/tot_bev)

data <- data %>%
        group_by(gemcode) %>%
        mutate(
          onepermean=mean(oneperdens, na.rm = TRUE),
          oneparentmean=mean(oneparentdens, na.rm = TRUE),
          perperhhmean=mean(perperhh, na.rm = TRUE),
          educationmean=mean(opleiding, na.rm = TRUE),
          socclassmean=mean(socklasse, na.rm = TRUE),
          twoearnmean=mean(k_tweeverd, na.rm = TRUE),
          outmigmean=mean(v_uit_perc, na.rm = TRUE),
          inmigmean=mean(v_in_perc, na.rm = TRUE),
          houseownmean=mean(perchouseown, na.rm = TRUE),	
          polavailmean=mean(polavail_mean_2005, na.rm = TRUE)
        )

write.table(data, file = "./Data/Thomas_data_PC4_crime.csv", sep = ",")

# ####################################################
# # Read in data for all violent crime
# ####################################################
# data <- read.csv("./Data/Thomas_data_PC4_crime.csv", header=TRUE, sep = ",")
# data <- data %>%
#   mutate(
#     addrdens   	    = (tot_hh/opptotaalhectare)/100,
#     schooldens 		  = scholenbasisonderwijsper10000inw,
#     percrentcontrol = percentagehuurtoeslagontvangersh,
#     perchouseown	  = percvoorraadkoopsector/100,
#     perchouserent	  = percvoorraadhuursector,
#     percsocialrent	= percvoorraadsocialehuursector,
#     percmigrants	  = tot_alloch/tot_bev,
#     percwmigrants	  = w_alloch/tot_bev,
#     percnwmigrants	= nw_alloch/tot_bev,
#     shops			      = verkooppuntentotaalwinkels/100,
#     perperhh		    = persperhh,
#     oneperdens		  = eenpersoon/huishouden,
#     oneparentdens	  = Eenouder/huishouden,
#     perc_5y			    = l0_5jaar/tot_bev,
#     perc5_10y		    = l5_10jaar/tot_bev,
#     perc10_15y		  = l10_15jaar/tot_bev,
#     perc15_20y		  = l15_20jaar/tot_bev,
#     perc20_25y		  = l20_25jaar/tot_bev,
#     perc25_30y		  = l25_30jaar/tot_bev,
#     perc30_35y		  = l30_35jaar/tot_bev,
#     perc35_40y		  = l35_40jaar/tot_bev,
#     perc40_45y		  = l40_45jaar/tot_bev,
#     perc45_50y		  = l45_50jaar/tot_bev,
#     perc50_55y		  = l50_55_jaa/tot_bev,
#     perc55_60y		  = l55_60jaar/tot_bev,
#     perc60_65y		  = l60_65jaar/tot_bev,
#     perc65_70y		  = l65_70jaar/tot_bev,
#     perc70_75y		  = l70_75jaar/tot_bev,
#     perc75_80y		  = l75_80jaar/tot_bev,
#     perc80_85y		  = l80_85jaar/tot_bev,
#     perc85_90y		  = l85_90jaar/tot_bev,
#     perc90_95y		  = l90_95jaar/tot_bev,
#     perc95_y		    = l95ofouder/tot_bev)
# 
# data <- data %>%
#   group_by(gemcode) %>%
#   mutate(
#     onepermean=mean(oneperdens, na.rm = TRUE),
#     oneparentmean=mean(oneparentdens, na.rm = TRUE),
#     perperhhmean=mean(perperhh, na.rm = TRUE),
#     educationmean=mean(opleiding, na.rm = TRUE),
#     socclassmean=mean(socklasse, na.rm = TRUE),
#     twoearnmean=mean(k_tweeverd, na.rm = TRUE),
#     outmigmean=mean(v_uit_perc, na.rm = TRUE),
#     inmigmean=mean(v_in_perc, na.rm = TRUE),
#     houseownmean=mean(perchouseown, na.rm = TRUE),	
#     polavailmean=mean(polavail_mean_2005, na.rm = TRUE)
#   )
# 
# write.table(data, file = "./Data/Thomas_data_PC4_violent.csv", sep = ",")
# 
# ####################################################
# # Read in data for all property crime
# ####################################################
# data <- read.dta("./Data/Thomas_data_PC4_property.dta")
# data <- data %>%
#   mutate(
#     addrdens   	    = (tot_hh/opptotaalhectare)/100,
#     schooldens 		  = scholenbasisonderwijsper10000inw,
#     percrentcontrol = percentagehuurtoeslagontvangersh,
#     perchouseown	  = percvoorraadkoopsector/100,
#     perchouserent	  = percvoorraadhuursector,
#     percsocialrent	= percvoorraadsocialehuursector,
#     percmigrants	  = tot_alloch/tot_bev,
#     percwmigrants	  = w_alloch/tot_bev,
#     percnwmigrants	= nw_alloch/tot_bev,
#     shops			      = verkooppuntentotaalwinkels/100,
#     perperhh		    = persperhh,
#     oneperdens		  = eenpersoon/huishouden,
#     oneparentdens	  = Eenouder/huishouden,
#     perc_5y			    = l0_5jaar/tot_bev,
#     perc5_10y		    = l5_10jaar/tot_bev,
#     perc10_15y		  = l10_15jaar/tot_bev,
#     perc15_20y		  = l15_20jaar/tot_bev,
#     perc20_25y		  = l20_25jaar/tot_bev,
#     perc25_30y		  = l25_30jaar/tot_bev,
#     perc30_35y		  = l30_35jaar/tot_bev,
#     perc35_40y		  = l35_40jaar/tot_bev,
#     perc40_45y		  = l40_45jaar/tot_bev,
#     perc45_50y		  = l45_50jaar/tot_bev,
#     perc50_55y		  = l50_55_jaa/tot_bev,
#     perc55_60y		  = l55_60jaar/tot_bev,
#     perc60_65y		  = l60_65jaar/tot_bev,
#     perc65_70y		  = l65_70jaar/tot_bev,
#     perc70_75y		  = l70_75jaar/tot_bev,
#     perc75_80y		  = l75_80jaar/tot_bev,
#     perc80_85y		  = l80_85jaar/tot_bev,
#     perc85_90y		  = l85_90jaar/tot_bev,
#     perc90_95y		  = l90_95jaar/tot_bev,
#     perc95_y		    = l95ofouder/tot_bev)
# 
# data <- data %>%
#   group_by(gemcode) %>%
#   mutate(
#     onepermean=mean(oneperdens, na.rm = TRUE),
#     oneparentmean=mean(oneparentdens, na.rm = TRUE),
#     perperhhmean=mean(perperhh, na.rm = TRUE),
#     educationmean=mean(opleiding, na.rm = TRUE),
#     socclassmean=mean(socklasse, na.rm = TRUE),
#     twoearnmean=mean(k_tweeverd, na.rm = TRUE),
#     outmigmean=mean(v_uit_perc, na.rm = TRUE),
#     inmigmean=mean(v_in_perc, na.rm = TRUE),
#     houseownmean=mean(perchouseown, na.rm = TRUE),	
#     polavailmean=mean(polavail_mean_2005, na.rm = TRUE)
#   )
# 
# write.table(data, file = "./Data/Thomas_data_PC4_property.csv", sep = ",")