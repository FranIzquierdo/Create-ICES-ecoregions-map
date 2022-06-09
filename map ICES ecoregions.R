#~~~~~~~~~~~~~~~~~~~~~~
# Map ICES ecoregions #
#~~~~~~~~~~~~~~~~~~~~~~
# Modified 09/06/2022 #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Francisco Izquierdo        #
# francisco.izqtar@gmail.com #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## load packages
library(rgdal)
library(ggplot2)
library(mapdata)
library("ggspatial")
library(ggpattern)
library("rnaturalearth")
library("rnaturalearthdata")
library(RColorBrewer)
library(colorRamps)

## create folder
dir_dat<-paste0(getwd(),"/data ecoregions")
dir.create(dir_dat)

## download ICES ecoregions shapefile
url<-"https://gis.ices.dk/shapefiles/ICES_ecoregions.zip"
file <- basename(url)
download.file(url , destfile=paste0(dir_dat,"/",file))

## unzip
unzip(paste0(dir_dat,"/",file), exdir=dir_dat)

## load ICES areas shapefile (in UTM)
areasUTM <- readOGR(dsn= dir_dat ,
                    # set layer name:
                    layer="ICES_ecoregions_20171207_erase_ESRI",
                    verbose=FALSE)

## change CRS to WGS84
areasWGS84 <- spTransform(areasUTM, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

## check
# plot(areasUTM, axes=T)
# plot(areasWGS84, axes=T)

## select world , world2Hires, world, worldHires
world = map_data("world") # table(world$region)

## fortify shapefiles for ggplot
areasWGS84 <- fortify(areasWGS84) 
world <- fortify(world) 

## set map limits 
lons = c(-32, 32)
lats = c(32, 80)

## create breaks and label vectors
ewbrks <- seq(lons[1], lons[2],10)
nsbrks <- seq(lats[1],lats[2],6)
ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste(-x, "ºW"), ifelse(x > 0, paste(x, "ºE"),x))))
nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste(x, "ºS"), ifelse(x > 0, paste(x, "ºN"),x))))

## there are some land polygons to avoid
areasWGS84<-subset(areasWGS84, areasWGS84$hole==FALSE)

## define sea and land colors
fill_land<-"#C1CDCD"
fill_sea<- "white" 

## set ecoregion name for each id
eco_names<-c( "0"="Greenland Sea",
              "1"="Bay of Biscay and the Iberian Coast",
              "10"="Greater North Sea",
              "11"="Artic Ocean",
              "12"="Iceland Sea",
              "13"="Barents Sea",
              "9"="Baltic Sea",
              "14"="Faroes",
              "15"="Norwegian Sea",
              "16"="Oceanic north-east Athlantic",
              "2"="Azores",
              "3"="Western Mediterranean Sea",
              "4"="Ionian and Central Mediterranean Sea",
              "5"="Black Sea",
              "6"="Adriatic Sea",
              "8"="Celtic Sea",
              "7"="Aegean-Levantine Sea")

## colour palette (17)
pal<-c("#FFFFFF","#97FFFF","#FAEBD7","#ffcc33","#CDC0B0",
       "#8B8378","#E0EEEE","#76EEC6","#458B74","#FFF2CD","#EEC186",
       "#FFE089","#7FFFD4","#F0FFFF","#BCB9A0","#528B8B","#79CDCD")

## create map
ggplot() +
  
  # ICES areas (sea) poly 
  geom_polygon(data = areasWGS84, aes(long, lat, group = group, fill=id),
               color = "darkgrey", size = 0.1, alpha=0.9)+
   scale_fill_manual(values=pal, labels=eco_names)+

  # world (land) poly
  geom_polygon(data = world, aes(x = long, y = lat, group = group), 
               fill= fill_land, color = "#838B8B") + # formatting
  ylab(" ") + xlab(" ") + 
  
  # configure projection and plot domain
  coord_sf(xlim = lons, ylim = lats)+
  scale_x_continuous(breaks=ewbrks, labels = ewlbls) +
  scale_y_continuous( breaks = nsbrks, labels = nslbls)+
  
  # theme custom
  theme_light()+
  theme(panel.background=element_rect(fill = "white", colour = "#2C3E4F"),
        plot.background= element_rect(fill = "white", colour = "#2C3E4F"),
        legend.key.size = unit(0.4, 'cm'))+
  
  # add caption
  labs(fill='ICES ecoregions',caption = "Francisco Izquierdo | https://github.com/FranIzquierdo")

## save
ggsave("map ICES ecoregions.jpeg", dpi=300, width=8, height=9)
