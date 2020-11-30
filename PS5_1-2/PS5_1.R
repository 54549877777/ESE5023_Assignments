library("sp")
library("rgdal")
library("sf")
library("raster")

#1.1
wind_Jan <- raster("wc2.1_2.5m_wind/wc2.1_2.5m_wind_01.tif")
wind_Feb <- raster("wc2.1_2.5m_wind/wc2.1_2.5m_wind_02.tif")
wind_Mar <- raster("wc2.1_2.5m_wind/wc2.1_2.5m_wind_03.tif")
wind_Apr <- raster("wc2.1_2.5m_wind/wc2.1_2.5m_wind_04.tif")
wind_May <- raster("wc2.1_2.5m_wind/wc2.1_2.5m_wind_05.tif")
wind_Jun <- raster("wc2.1_2.5m_wind/wc2.1_2.5m_wind_06.tif")
wind_Jul <- raster("wc2.1_2.5m_wind/wc2.1_2.5m_wind_07.tif")
wind_Aug <- raster("wc2.1_2.5m_wind/wc2.1_2.5m_wind_08.tif")
wind_Sep <- raster("wc2.1_2.5m_wind/wc2.1_2.5m_wind_09.tif")
wind_Oct <- raster("wc2.1_2.5m_wind/wc2.1_2.5m_wind_10.tif")
wind_Nov <- raster("wc2.1_2.5m_wind/wc2.1_2.5m_wind_11.tif")
wind_Dec <- raster("wc2.1_2.5m_wind/wc2.1_2.5m_wind_12.tif")
prec_Jan <- raster("wc2.1_2.5m_prec/wc2.1_2.5m_prec_01.tif")
prec_Feb <- raster("wc2.1_2.5m_prec/wc2.1_2.5m_prec_02.tif")
prec_Mar <- raster("wc2.1_2.5m_prec/wc2.1_2.5m_prec_03.tif")
prec_Apr <- raster("wc2.1_2.5m_prec/wc2.1_2.5m_prec_04.tif")
prec_May <- raster("wc2.1_2.5m_prec/wc2.1_2.5m_prec_05.tif")
prec_Jun <- raster("wc2.1_2.5m_prec/wc2.1_2.5m_prec_06.tif")
prec_Jul <- raster("wc2.1_2.5m_prec/wc2.1_2.5m_prec_07.tif")
prec_Aug <- raster("wc2.1_2.5m_prec/wc2.1_2.5m_prec_08.tif")
prec_Sep <- raster("wc2.1_2.5m_prec/wc2.1_2.5m_prec_09.tif")
prec_Oct <- raster("wc2.1_2.5m_prec/wc2.1_2.5m_prec_10.tif")
prec_Nov <- raster("wc2.1_2.5m_prec/wc2.1_2.5m_prec_11.tif")
prec_Dec <- raster("wc2.1_2.5m_prec/wc2.1_2.5m_prec_12.tif")
srad_Jan <- raster("wc2.1_2.5m_srad/wc2.1_2.5m_srad_01.tif")
srad_Feb <- raster("wc2.1_2.5m_srad/wc2.1_2.5m_srad_02.tif")
srad_Mar <- raster("wc2.1_2.5m_srad/wc2.1_2.5m_srad_03.tif")
srad_Apr <- raster("wc2.1_2.5m_srad/wc2.1_2.5m_srad_04.tif")
srad_May <- raster("wc2.1_2.5m_srad/wc2.1_2.5m_srad_05.tif")
srad_Jun <- raster("wc2.1_2.5m_srad/wc2.1_2.5m_srad_06.tif")
srad_Jul <- raster("wc2.1_2.5m_srad/wc2.1_2.5m_srad_07.tif")
srad_Aug <- raster("wc2.1_2.5m_srad/wc2.1_2.5m_srad_08.tif")
srad_Sep <- raster("wc2.1_2.5m_srad/wc2.1_2.5m_srad_09.tif")
srad_Oct <- raster("wc2.1_2.5m_srad/wc2.1_2.5m_srad_10.tif")
srad_Nov <- raster("wc2.1_2.5m_srad/wc2.1_2.5m_srad_11.tif")
srad_Dec <- raster("wc2.1_2.5m_srad/wc2.1_2.5m_srad_12.tif")

#1.2 - 1.4
#取年平均
wind_year<-(wind_Jan+wind_Feb+wind_Mar+wind_Apr+wind_May+wind_Jun+wind_Jul+
                  wind_Aug+wind_Sep+wind_Oct+wind_Nov+wind_Dec)/12

prec_year <- (prec_Jan+prec_Feb+prec_Mar+prec_Apr+prec_May+prec_Jun+prec_Jul+
           prec_Aug+prec_Sep+prec_Oct+prec_Nov+prec_Dec)/12

srad_year <- (srad_Jan+srad_Feb+srad_Mar+srad_Apr+srad_May+srad_Jun+srad_Jul+
           srad_Aug+srad_Sep+srad_Oct+srad_Nov+srad_Dec)/12

China <- readOGR("China_map", "bou2_4p")
col <- terrain.colors(30)

wind_china<-crop(wind_year,China)
wind_china <- mask(wind_china,China,na.rm=T)
plot(wind_china,  main="wind in china", col=col)
contour(wind_china, add=T, levels=4,col="red")
#我最喜欢青藏高原作为风力发电厂

prec_china<-crop(prec_year,China)
prec_china <- mask(prec_china,China,na.rm=T)
plot(prec_china,  main="prec in china", col=col)
contour(prec_china, add=T,levels=100, col="red")

srad_china<-crop(srad_year,China)
srad_china <- mask(srad_china,China,na.rm=T)
plot(srad_china,  main="srad in china", col=col)
contour(srad_china, add=T,levels=16000, col="red")
#我最喜欢新疆和西藏作为光伏发电厂


