library(raster)
library(maptools)
library(rgdal)
library(rgeos)


### Note: spatstat not available on CRAN for R 3.2.5
### Archived version available here: https://cran.r-project.org/src/contrib/Archive/spatstat/
### Download version 1.45-2 for R 3.2.5
### Install using: install.packages("spatstat_1.45-2.tar.gz", repos=NULL, type="source") 

library(spatstat)

#### Load in London Ward shapefiles

#london <- readOGR(file.path(paths$data, "external", "E002", "London_Ward"))
dir <- file.path(paths$data, "external", "E002")
brt <- readOGR(dir, layer="London_Ward")
wgs84 <- '+proj=longlat +datum=WGS84'
brt <- spTransform(brt, CRS(wgs84))

# Several different map styles are available, preview overview is at:
# https://leaflet-extras.github.io/leaflet-providers/preview/

# These function as an argument to addProviderTiles(), with popular options being "Stamen.Toner", "CartoDB.DarkMatter", "OpenStreetMap.BlackAndWhite", "Stamen.Terrain", "CartoDB.Positron"
# Examples of how to map the shapefiles' polygons:
# 
# leaflet(brt) %>% 
#   addProviderTiles("Stamen.Toner") %>% 
#   addPolygons()
# 
# leaflet(brt) %>% 
#   addProviderTiles("CartoDB.DarkMatter") %>% 
#   addPolygons()
# 
# leaflet(brt) %>% 
#   addProviderTiles("OpenStreetMap.BlackAndWhite") %>% 
#   addPolygons()
# 
# leaflet(brt) %>% 
#   addProviderTiles("Stamen.Terrain") %>% 
#   addPolygons()

# Let's now try to combine the Timberland store locations with this type of map.
# get the locations of the Timberland store first
df.store <- load_store_master()
df.store_tbl <- df.store %>%
  filter(shop_brand == "TIMBERLAND")

# Turn this into a SpatialPointsDataFrame
# We can use a similar projection here as the shapefile with which we're trying to merge
sp_fw <- SpatialPointsDataFrame(coords=cbind(df.store_tbl$Longitude, df.store_tbl$Latitude), 
                                data = df.store_tbl, 
                                proj4string = CRS("+proj=longlat +datum=WGS84"))

# We could then draw an area enclosed by these locations with gConvexHUll
hull <- gConvexHull(sp_fw)
hull_xy <- spTransform(hull, CRS("+proj=longlat +datum=WGS84"))

leaflet(brt) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(weight=1.5, color="#AA", opacity=0.4, fillOpacity = 0.2) %>%
  addMarkers(data=df.store, popup=~paste(shop_text)) %>%
  addPolygons(data=hull_xy)


### When trying to assess which areas are theoretically covered by one store, we can make use of Voronoi / Dirichlet polygons
store_coords <- sp_fw@coords
# Define the spatial window in which we want to do this analysis
window <- owin(brt@bbox[1,], brt@bbox[2,])

d <- dirichlet(as.ppp(store_coords, window))
dsp <- as(d, "SpatialPolygons")
dsp_df <- SpatialPolygonsDataFrame(dsp, 
                                   data = data.frame(id = 1:length(dsp@polygons)))
proj4string(dsp_df) <- CRS("+proj=longlat +datum=WGS84")

dsp_df$area <- round((gArea(dsp_df, byid = TRUE) / 1000000), 1)
dsp_xy <- spTransform(dsp_df, CRS("+proj=longlat +datum=WGS84"))

leaflet(brt) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  #addPolygons(weight=1.5, color="#AA", opacity=0.4, fillOpacity = 0.2) %>%
  addMarkers(data=df.store_tbl, popup=~paste(shop_text)) %>%
  addPolygons(data=dsp_xy, color="#ffa500")




#LSOA
#dir <- "C:/Users/IBM_ADMIN/Downloads/Lower_Layer_Super_Output_Areas_December_2011_Full_Extent__Boundaries_in_England_and_Wales"
#lsoa <- readOGR(dir, layer="Lower_Layer_Super_Output_Areas_December_2011_Full_Extent__Boundaries_in_England_and_Wales")
# FIGURE OUT HOW TO OVERLAP WITH LONDON (brt)

#leaflet(lsoa) %>% 
  # addProviderTiles("CartoDB.Positron") %>% 
  # addPolygons(weight=1.5, color="#AA", opacity=0.4, fillOpacity = 0.2) %>%
  # addMarkers(data=df.store, popup=~paste(shop_text)) %>%
  # addPolygons(data=hull_xy)
  # 

df.ecomm <- load_ecomm_latlong_data()

df.int <- df.ecomm %>% 
  filter(BRAND == "C") %>% 
  group_by(latitude, longitude) %>% 
  dplyr::summarize(total_rev=sum(RETAILPRICE, na.rm=TRUE))

df.int %>% arrange(desc(total_rev)) %>% head(100) -> df.int


m <- leaflet(brt) %>% 
     addProviderTiles("CartoDB.Positron") %>% 
     addPolygons(weight=1.5, color="#AA", opacity=0.4, fillOpacity = 0.2) 

m %>% addCircles(data=df.int, weight = 5, radius=~sqrt(total_rev)*5, 
                 color="#ffa500", stroke = TRUE, fillOpacity = 0.8)


df.ecomm %>% 
  group_by(magento.order.nr, latitude, longitude) %>% dplyr::summarize(basket_size=n()) -> df.baskets

coords <- cbind(df.baskets$longitude, df.baskets$latitude)
df.sp <- SpatialPointsDataFrame(coords, as.data.frame(df.baskets))
proj4string(df.sp) <- CRS("+proj=longlat")
df.sp <- spTransform(df.sp, proj4string(brt))

df.sp_subset <- over(df.sp, brt) %>% filter(!is.na(NAME))

head(df.sp_subset)

df.sp_groups <- df.sp_subset %>%
  group_by(POLY_ID) %>%
  dplyr::summarize(avg_basketsize = mean(`df.sp$basket_size`, na.rm=TRUE))
#df.sp_1 <- head(df.sp, 3)
#leaflet(brt) %>% 
#  addProviderTiles("CartoDB.Positron") %>% 
#  addMarkers(data=df.sp_1) 
####

brt_2 <- sp::merge(brt, df.sp_groups)

leaflet(brt_2) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(weight=1.5, color="#AA", opacity=0.4, fill=~avg_basketsize, fillOpacity = 0.2) %>%
  addMarkers(data=df.store_tbl, popup=~paste(shop_text))

pal <- colorNumeric(
  palette = "Oranges",
  domain=brt_2@data$avg_basketsize
)

leaflet(brt_2) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(weight=1.5, color="#AA", opacity=0.4, fillColor=~pal(avg_basketsize), fillOpacity = 0.4, popup=~paste(avg_basketsize)) %>%
  addMarkers(data=df.store_tbl, popup=~paste(shop_text))
