####################################################################
# 4-Postcodes Nederland  2007              Last update 2011-04-15  # 
####################################################################
require(foreign)
require(maptools)
require(spdep)
require(foreign)
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
DATAFILENAME<-"pc4anycrime.dta"

###########################################################
# Read datafile                                           #
###########################################################
PC4Data<-read.dta(paste(DATAFILEFOLDER, DATAFILENAME, sep=""))
names(PC4Data)

###########################################################
# Read shapefile                                          #
###########################################################
PC4Polygons<-readShapeSpatial(paste(SHAPEFILEFOLDER, SHAPEFILENAME, sep=""))
names(PC4Polygons@data)


###########################################################
# Link Block Polygons with Block data                     #
###########################################################
PC4Polygons@data = data.frame(PC4Polygons@data, PC4Data[match(PC4Polygons@data[,1], PC4Data[,1]),]) 


#plot(PC4Polygons)
#spplot(PC4Polygons[,"totalpop">5000], zcol="totalpop")
#spplot(PC4Polygons, zcol="pc4anycrime")

##########################################################
# Set missing totalpop and pc4anycrime to "0". 
#################
PC4Polygons@data$totalpop[which(is.na(PC4Polygons@data$totalpop))]<-0
PC4Polygons@data$pc4anycrime[which(is.na(PC4Polygons@data$pc4anycrime))]<-0


############################################
# Create Queens lag neighbors lists
############################################
QueensLag<-poly2nb(PC4Polygons)

############################################
# Transform into a representation for the  #
#   sum lag functions                      #
############################################
PC4Queens<-nb2listw(QueensLag, glist=NULL, style="B", zero.policy=TRUE)

############################################
# Transform into a representation for the  #
#   row-standardized lag functions        #
############################################
rstPC4Queens<-nb2listw(QueensLag, glist=NULL, style="W", zero.policy=TRUE)

############################################
############################################
# Calculate Lagged values 
############################################
############################################

############################################
# Copy the dataframe (includes identifiers)
############################################
Laggedpop<-lag.listw(PC4Queens, as.numeric(PC4Polygons@data[,"totalpop"]), zero.policy=TRUE, NAOK=TRUE)
Laggedtotal<-lag.listw(PC4Queens, as.numeric(PC4Polygons@data[,"pc4anycrime"]), zero.policy=TRUE, NAOK=TRUE)

#cbind(Laggedpop, PC4Polygons@data[,"totalpop"], Laggedpop / PC4Polygons@data[,"totalpop"])
#colnames(Lagged)<-paste("AGG", colnames(SP_Tracts@data), sep="")

############################################
# Bind to original dataframe
############################################
PC4Polygons@data<-cbind(PC4Polygons@data, Laggedpop, Laggedtotal)
names(PC4Polygons@data)


write.dta(as.data.frame(PC4Polygons@data) ,file="I:/MSC/SocialInteractions/Laggedpc4delinquents.dta", convert.factors="numeric")




PC4Polygons@data[,17:21]