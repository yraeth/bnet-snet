# Function to install and load packages
# https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
install_load <- function(x){
  for (i in x){
    if (!require(i, character.only = TRUE)){
      install.packages(i, dependencies = TRUE)
      require(i, character.only = TRUE)
    }
  }
}

# Install and load necessary packages
install_load(c("shp2graph", "sf", "sp", "maptools", "rgdal", "raster", "stringr", "concaveman"))

# Set working directory
workingpath <- "/path/to/working/directory"
setwd(workingpath)

# Define necessary file paths
shppath <- paste0(workingpath, "/building_networks")
temppath <- paste0(workingpath, "/temp")

# Define time steps of interest
time_steps <- c("1899")

# Loop through time steps
for (ts in time_steps){
  
  # Read shapefile and create graph
  zs_99 <- readShapeLines(paste0("bn_", ts, ".shp"))
  zn_99 <- readshpnw(zs_99)
  ig99 <- nel2igraph(zn_99[[2]], zn_99[[3]], weight = zn_99[[5]][["NEAR_DIST"]], eadf = zn_99[[5]])
  
  # Subset graph to components with at least 10 vertices
  components <- decompose(ig99, min.vertices = 9)
  want <- ig99 %>%
    components %>%
    groups %>%
    .[sapply(., length) >= 10]
  newG <- ig99 %>%
    {. - V(.)[! as.numeric(V(.)) %in% unlist(want)]}
  
  # Create settlment deliniation and write to file
  polygons_list <- lapply(newG, function(component){
    polygons <- concaveman(as.matrix(as.data.frame(vertex_attr(component))), concavity = 1, length_threshold = 0)
    sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(polygons)), ID = 1)), proj4string = CRS("+init=epsg:21781"))
    sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data = data.frame(ID = 1))
    yr <- substring(ts, 3, 4)
    sp_poly_df$ID <- paste0(yr, "_", round(sp_poly_df@polygons[[1]]@Polygons[[1]]@labpt[[1]]), "_", round(sp_poly_df@polygons[[1]]@Polygons[[1]]@labpt[[2]]))
    return(sp_poly_df)
  })
  main_poly_df <- do.call(rbind, polygons_list)
  writeOGR(main_poly_df, paste0(shppath, "/chull_", ts), layer = paste0("chull_", ts), driver = "ESRI Shapefile", overwrite_layer = TRUE)
  
  # Calculate centroids and buffer of concave hull
  chull <- st_read(paste0(shppath, "/chull_", ts, "/chull_", ts, ".shp"))
  centroid_hull <- st_centroid(chull)
  centroid_hull <- as(centroid_hull, "Spatial")
  writeOGR(centroid_hull, paste0(shppath, "/chull_", ts), layer = paste0("centroids_", ts), driver = "ESRI Shapefile", overwrite_layer = TRUE)
  chull_buffer <- st_buffer(chull, 10)
  chull_buffer <- as(chull_buffer, "Spatial")
  writeOGR(chull_buffer, paste0(shppath, "/chull_", ts), layer = paste0("chull_", ts, "_buffer"), driver = "ESRI Shapefile", overwrite_layer = TRUE)
  
}
