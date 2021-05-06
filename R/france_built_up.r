
# How to download raster files from Copernicus land cover website
# load them into R, merge into a single raster, calculate average 
# weighted percentage of built-up areas for every French commune
# and make a pretty choropleth map using <150 lines of code

# Milos Popovic
# 5/6/2021

# load libraries
library(dplyr, quietly=T)
library(ggplot2, quietly=T) 
library(rgdal, quietly=T)
library(raster, quietly=T)
library(sf, quietly=T)
library(exactextractr, quietly=T)
library(gdalUtils, quietly=T)
library(reshape2, quietly=T)
library(classInt, quietly=T)

set.seed(20210429)

#download French communes
url <- "https://www.data.gouv.fr/fr/datasets/r/17062524-991f-4e13-9bf0-b410cc2216fd"
download.file(url, paste0(basename(url), ".zip"), mode="wb")
unzip("17062524-991f-4e13-9bf0-b410cc2216fd.zip")
fr <- st_read("communes-20210101.shp", stringsAsFactors = FALSE) %>% 
  st_transform(4326)  %>% 
  st_as_sf()

# bounding box for France
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
  bb <- st_sfc(
  st_polygon(list(cbind(
    c(-6, 10.2, 10.2, -6, -6), #x-coordinates (longitudes)
    c(41.2, 51.2, 51.2, 51.2, 41.2)  #y-coordinates (latitudes)
    ))),
  crs = crsLONGLAT) %>% st_bbox()

#DOWNLOAD RASTER FILES
#urls
u2018_1 <- "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2018/E000N60/E000N60_PROBAV_LC100_global_v3.0.1_2018-conso_BuiltUp-CoverFraction-layer_EPSG-4326.tif"
u2018_2 <- "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2018/W020N60/W020N60_PROBAV_LC100_global_v3.0.1_2018-conso_BuiltUp-CoverFraction-layer_EPSG-4326.tif"
#download
download.file(u2018_1, basename(u2018_1), mode="wb")
download.file(u2018_2, basename(u2018_2), mode="wb")

# merge
ras2018 <- c("E000N60_PROBAV_LC100_global_v3.0.1_2018-conso_BuiltUp-CoverFraction-layer_EPSG-4326.tif",
"W020N60_PROBAV_LC100_global_v3.0.1_2018-conso_BuiltUp-CoverFraction-layer_EPSG-4326.tif")
r2018 <- mosaic_rasters(gdalfile=ras2018,
              dst_dataset="2018_builtup_cover.tif",
              of="GTiff")
ras <- raster("2018_builtup_cover.tif")
crs(ras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# average values over shapefile
fr$id <- 1:max(nrow(fr)) # common id
f  <- exact_extract(ras, fr, "mean")
df <- as.data.frame(f)
names(df)[1] <- "value"
df$id <- 1:max(nrow(df)) # common id
d <- left_join(fr, df, by="id")

ni = classIntervals(d$value, 
				   n = 7, 
				   style = 'quantile')$brks
# this function uses above intervals to create categories
labels <- c()
for(i in 1:length(ni)){
    labels <- c(labels, paste0(round(ni[i], 0), 
                             "–", 
                             round(ni[i + 1], 0)))
}
labels <- labels[1:length(labels)-1]

# finally, carve out the categorical variable 
# based on the breaks and labels above
d$cat <- cut(d$value, 
              breaks = ni, 
              labels = labels, 
              include.lowest = T)
levels(d$cat) # let's check how many levels it has (6)

p <- ggplot() +
	geom_sf(data=subset(d, !is.na(value)), aes(fill = cat), 
		color=NA, 
		size=0) +
	coord_sf(crs = crsLONGLAT, xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"])) + 
    labs(x = "©2021 Milos Popovic https://milosp.info\n Data: Buchhorn, M.; Smets, B.; Bertels, L.;\nLesiv, M.; Tsendbazar, N.-E.; Masiliunas, D.;\nLinlin, L.; Herold, M.; Fritz, S. (2020). Copernicus Global Land Service: Land Cover 100m:\nCollection 3: epoch 2018: Globe (Version V3.0.1) [Data set]. Zenodo. DOI: 10.5281/zenodo.3518038", 
         y = NULL, 
         title = "Average size of built-up area in France (2018)", 
         subtitle = "", 
         caption = "") +
	scale_fill_manual(
		name="% of land area", 
		values = rev(c('#47181e', '#782932', 
						'#a93e48', '#d85a60', 
						'#fc807a', '#ffb89e', 
						'#fdead2')), # I used chroma.js to mix the colors
		labels=c("0–2",    "2–3",    "3–4",    "4–5",    "5–7",    "7–13",   ">13"),
      	drop = F)+
  	guides(color=F, fill = guide_legend(
         direction = "horizontal",
         keyheight = unit(1.15, units = "mm"),
         keywidth = unit(15, units = "mm"),
         title.position = 'top',
         title.hjust = 0.5,
         label.hjust = .5,
         nrow = 1,
         byrow = T,
         reverse = F,
         label.position = "bottom"
          )
    ) +
    theme_minimal() +
    theme(axis.line = element_blank(),
    	axis.text.x = element_blank(),
    	axis.text.y = element_blank(),
    	axis.ticks = element_blank(),
    	axis.title.x = element_text(size=9, color="grey60", hjust=0, vjust=30),
    	axis.title.y = element_blank(),
    	legend.position = c(.55, 1),
    	legend.text = element_text(size=9, color="grey20"),
    	legend.title = element_text(size=10, color="grey20"),
    	panel.grid.major = element_line(color = "white", size = 0.2),
    	panel.grid.minor = element_blank(),
    	plot.margin     =   unit(c(t=0, r=0, b=-5, l=0),"lines"),
  		plot.title = element_text(face="bold", size=20, color="#47181e", hjust=.5),
		plot.subtitle = element_text(size=12, color="grey20", hjust=.5),
    	plot.background = element_rect(fill = "white", color = NA), 
    	panel.background = element_rect(fill = "white", color = NA), 
    	legend.background = element_rect(fill = "white", color = NA),
    	panel.border = element_blank()) 

#don't forget to save your plot in png format
ggsave(filename="france_builtup_2018.png", width= 7, height= 7, dpi = 600, device='png', p)