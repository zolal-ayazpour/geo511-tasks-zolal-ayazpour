library(raster)
library(rasterVis)
library(rgdal)
library(ggmap)
library(tidyverse)
library(knitr)
library(ncdf4)

setwd('/Users/zolalzzz/Documents/Main/PhD/Fall2019/SpatialDataScience/Git/geo511-tasks-zolal-ayazpour/week_10/case_study')

# Create afolder to hold the downloaded data
dir.create("data",showWarnings = F) #create a folder to hold the data

lulc_url="https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MCD12Q1.051_aid0001.nc?raw=true"
lst_url="https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MOD11A2.006_aid0001.nc?raw=true"

# download them
download.file(lulc_url,destfile="data/MCD12Q1.051_aid0001.nc")
download.file(lst_url,destfile="data/MOD11A2.006_aid0001.nc")

# Load data into R
lulc=stack("data/MCD12Q1.051_aid0001.nc",varname="Land_Cover_Type_1")
lst=stack("data/MOD11A2.006_aid0001.nc",varname="LST_Day_1km")

# Explore LULC data
plot(lulc)
lulc=lulc[[13]]
plot(lulc)

# Process landcover data
Land_Cover_Type_1 = c(
  Water = 0, 
  `Evergreen Needleleaf forest` = 1, 
  `Evergreen Broadleaf forest` = 2,
  `Deciduous Needleleaf forest` = 3, 
  `Deciduous Broadleaf forest` = 4,
  `Mixed forest` = 5, 
  `Closed shrublands` = 6,
  `Open shrublands` = 7,
  `Woody savannas` = 8, 
  Savannas = 9,
  Grasslands = 10,
  `Permanent wetlands` = 11, 
  Croplands = 12,
  `Urban & built-up` = 13,
  `Cropland/Natural vegetation mosaic` = 14, 
  `Snow & ice` = 15,
  `Barren/Sparsely vegetated` = 16, 
  Unclassified = 254,
  NoDataFill = 255)

lcd=data.frame(
  ID=Land_Cover_Type_1,
  landcover=names(Land_Cover_Type_1),
  col=c("#000080","#008000","#00FF00", "#99CC00","#99FF99", "#339966", "#993366", "#FFCC99", "#CCFFCC", "#FFCC00", "#FF9900", "#006699", "#FFFF00", "#FF0000", "#999966", "#FFFFFF", "#808080", "#000000", "#000000"),
  stringsAsFactors = F)
# colors from https://lpdaac.usgs.gov/about/news_archive/modisterra_land_cover_types_yearly_l3_global_005deg_cmg_mod12c1
kable(head(lcd))

# convert to raster (easy)
lulc=as.factor(lulc)

# update the RAT with a left join
levels(lulc)=left_join(levels(lulc)[[1]],lcd) ## Joining, by = "ID"

# plot it
gplot(lulc)+
  geom_raster(aes(fill=as.factor(value)))+
  scale_fill_manual(values=levels(lulc)[[1]]$col,
                    labels=levels(lulc)[[1]]$landcover,
                    name="Landcover Type")+
  coord_equal()+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(ncol=1,byrow=TRUE))

# Land Surface Temperature
plot(lst[[1:12]])

# Convert LST to Degrees C
offs(lst)=-273.15
plot(lst[[1:12]])

# Add Dates to Z (time) dimension
names(lst)[1:5]

tdates=names(lst)%>%
  sub(pattern="X",replacement="")%>%
  as.Date("%Y.%m.%d")

names(lst)=1:nlayers(lst)
lst=setZ(lst,tdates)



# Part 1: Extract timeseries for a point

#  define a new Spatial Point at a location.
lw = SpatialPoints(data.frame(x= -78.791547,y=43.007211))
projection(lw) <- "+proj=longlat"
lw = spTransform(lw,CRS(proj4string(lst)))

# Extract the LST data for that location
point_temperature = raster::extract(lst,lw,buffer=1000,fun=mean,na.rm=T)
point_temperature = t(point_temperature) #transpose it

# Extract the dates for each layer with getZ(lst)
point_date=getZ(lst)

# Combine them into a data.frame with the transposed raster values
point_inf=data.frame(point_date,point_temperature)

ggplot(point_inf,aes(x=point_date,y=point_temperature))+
  geom_point()+
  geom_smooth(span=0.02,n=811)+
  xlab("date")+
  ylab("Monthly Mean Land Surface Temperature")



# Part 2: Summarize weekly data to monthly climatologies

# First make a variable called tmonth by converting the dates to months
tmonth = as.numeric(format(getZ(lst),"%m"))

# Summarize the mean value per month per grid cell
lst_month = stackApply(lst,tmonth,fun=mean)

# Set the names of the layers to months
names(lst_month)=month.name

gplot(lst_month)+
  geom_raster(aes(fill=value))+
  facet_wrap(~variable)

# Calculate the monthly mean for the entire image
mean_monthly <- cellStats(lst_month,mean)
kable(mean_monthly)



# Part 3: Summarize Land Surface Temperature by Land Cover

# Resample lc to lst grid
lulc2 <- resample(lulc,lst,method="ngb")

# Extract the values from lst_month and lulc2 into a data.frame
lcds1=cbind.data.frame(
  values(lst_month),
  ID=values(lulc2[[1]]))%>% 
  na.omit() %>% 
  gather(key='month',value='value',-ID) %>%  #Gather the data into a ‘tidy’ format
  mutate(ID=as.numeric(ID)) %>% 
  mutate(month=factor(month,levels=month.name,ordered=T)) %>% 
  left_join(lcd) %>% 
  filter(landcover%in%c("Urban & built-up","Deciduous Broadleaf forest")) #to keep only landcover%in%c("Urban & built-up","Deciduous Broadleaf forest")

ggplot(lcds1,aes(month,value))+
  geom_jitter(alpha=0.2,width=0.3)+
  geom_violin(col="red")+
  facet_wrap(~landcover)

