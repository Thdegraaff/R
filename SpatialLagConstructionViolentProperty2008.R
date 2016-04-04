####################################################################
# 4-Postcodes Nederland  2007              Last update 2011-04-15  # 
####################################################################
rm(list=ls())

require(foreign)
require(maptools)
require(spdep)
require(gpclib)
gpclibPermit()
require(MASS)
require(arm)

###########################################################
# Shapefile                                               #
###########################################################
SHAPEFILEFOLDER<- "I:/MSC/DATA/PC4GEO/Esri-shape/Pc4/"
SHAPEFILENAME<-"nlp4_r07.shp"

###########################################################
# Datafile                                                #
###########################################################
DATAFILEFOLDER<- "I:/MSC/SocialInteractions/"
DATAFILENAME<-"logit_pc4anycrime2008.dta"

###########################################################
# Read datafile                                           #
###########################################################
PC4Data<-read.dta(paste(DATAFILEFOLDER, DATAFILENAME, sep=""))
names(PC4Data)

###########################################################
# Read shapefile                                          #
###########################################################
PC4Polygons<-readShapeSpatial(paste(SHAPEFILEFOLDER, SHAPEFILENAME, sep=""))

###########################################################
# Link Block Polygons with Block data                     #
###########################################################
PC4Polygons@data = data.frame(PC4Polygons@data, PC4Data[match(PC4Polygons@data[,1], PC4Data[,1]),]) 

#plot(PC4Polygons)
#spplot(PC4Polygons[,"totalpop">5000], zcol="totalpop")
#spplot(PC4Polygons, zcol="pc4anycrime")

##########################################################
# Set missing totalpop/pc4anycrime/pc4anyviolent/pc4anyproperty to "0". 
##########################################################
PC4Polygons@data$totalpop[which(is.na(PC4Polygons@data$totalpop))]<-0
PC4Polygons@data$pc4anycrime[which(is.na(PC4Polygons@data$pc4anycrime))]<-0
PC4Polygons@data$pc4anyviolent[which(is.na(PC4Polygons@data$pc4anyviolent))]<-0
PC4Polygons@data$pc4anyproperty[which(is.na(PC4Polygons@data$pc4anyproperty))]<-0

############################################
# Create Queens lag neighbors lists
############################################
QueensLag1<-poly2nb(PC4Polygons)
QueensLag2<-nblag_cumul(nblag(QueensLag1, 2))

############################################
# Transform into a representation for the  #
#   sum lag functions                      #
############################################
PC4Queens1<-nb2listw(QueensLag1, glist=NULL, style="B", zero.policy=TRUE)
PC4Queens2<-nb2listw(QueensLag2, glist=NULL, style="B", zero.policy=TRUE)

############################################
# Transform into a representation for the  #
#   row-standardized lag functions        #
############################################
rstPC4Queens1<-nb2listw(QueensLag1, glist=NULL, style="W", zero.policy=TRUE)
rstPC4Queens2<-nb2listw(QueensLag2, glist=NULL, style="W", zero.policy=TRUE)

############################################
############################################
# Calculate Lagged values 
############################################
############################################

############################################
# Copy the dataframe (includes identifiers)
############################################
Laggedpop1<-lag.listw(PC4Queens1, as.numeric(PC4Polygons@data[,"totalpop"]), zero.policy=TRUE, NAOK=TRUE)
Laggedtotal1<-lag.listw(PC4Queens1, as.numeric(PC4Polygons@data[,"pc4anycrime"]), zero.policy=TRUE, NAOK=TRUE)
Laggedviolent1<-lag.listw(PC4Queens1, as.numeric(PC4Polygons@data[,"pc4anyviolent"]), zero.policy=TRUE, NAOK=TRUE)
Laggedproperty1<-lag.listw(PC4Queens1, as.numeric(PC4Polygons@data[,"pc4anyproperty"]), zero.policy=TRUE, NAOK=TRUE)
Laggedpop2<-lag.listw(PC4Queens2, as.numeric(PC4Polygons@data[,"totalpop"]), zero.policy=TRUE, NAOK=TRUE)
Laggedtotal2<-lag.listw(PC4Queens2, as.numeric(PC4Polygons@data[,"pc4anycrime"]), zero.policy=TRUE, NAOK=TRUE)
Laggedviolent2<-lag.listw(PC4Queens2, as.numeric(PC4Polygons@data[,"pc4anyviolent"]), zero.policy=TRUE, NAOK=TRUE)
Laggedproperty2<-lag.listw(PC4Queens2, as.numeric(PC4Polygons@data[,"pc4anyproperty"]), zero.policy=TRUE, NAOK=TRUE)

############################################
# Bind to original dataframe
############################################
PC4Polygons@data<-cbind(PC4Polygons@data, Laggedpop1, Laggedtotal1, Laggedviolent1, Laggedproperty1, 
                                          Laggedpop2, Laggedtotal2, Laggedviolent2, Laggedproperty2)
names(PC4Polygons@data)

write.dta(as.data.frame(PC4Polygons@data) ,file="I:/MSC/SocialInteractions/Laggedpc4delinquents2008.dta", convert.factors="numeric")

PC4Polygons@data[,17:21]