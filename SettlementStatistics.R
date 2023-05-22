library(sf)
library(rgeos)
library(data.table)
library(dplyr)
library(ggplot2)
library(DescTools)
library(rgeos)
library(ineq)
library(sfheaders)
library(REdaS)
library(tidyverse)


# Set options
options(scipen=999)

# define functions
schumms_shape_index <- function(perimeter, area) {
  index <- perimeter / sqrt(area)
  return(index)
}
convex_hull_area <- function(polygon) {
  chull_points <- chull(polygon)
  convex_hull_points <- rbind(polygon[chull_points, ], polygon[chull_points[1], ])
  convex_hull <- st_polygon(list(convex_hull_points))
  area <- st_area(convex_hull)
  return(area)
}
extract_coordinates <- function(x) {
  xy <- unlist(x)
  len <- length(xy)
  if (len %% 2 != 0) {
    xy <- xy[-len]
  }
  coords <- matrix(xy, ncol = 2, byrow = TRUE)
  return(coords)
}
gamma <- function(alpha1, alpha2) {
  rad_diff <- (alpha1 - alpha2) * pi / 180
  cos_abs <- abs(cos(rad_diff))
  acos_deg <- acos(cos_abs) * 180 / pi
  gamma_value <- 45 - abs(acos_deg - 45)
  return(gamma_value)
}



# Calculation of Settlement Statistics ------------------------------------

time_steps <- c("1899",
                "1918",
                "1933",
                "1959",
                "1970",
                "1978",
                "2020")
all <- NULL
for (ts in time_steps){
   # Set the generic paths to your shapefiles
  shapefile_path1 <- paste0("<path_to_your_files>/building_footrpints/bf_", ts, "_bpl.shp")
  shapefile_path2 <- paste0("<path_to_your_files>/building_networks/bn_", ts, "_id.shp")

  # Read the shapefile 1 (containing geometry) and convert the row names to integers
  shapefile1 <- st_read(shapefile_path1)
  footprints <- shapefile1
  footprints$FID <- as.integer(row.names(footprints))

  # Read shapefile 2 (attribute table only)
  attribute_table_only <- st_read(shapefile_path2, options = "RECORDS_ONLY")
  buildingnet <- attribute_table_only

  # Message to show that the reading process has completed
  print("Finished reading shapefiles")

  # Convert the data frames to data tables for optimized processing
  buildingnet_opt <- data.table(buildingnet)
  footprints_opt <- data.table(footprints)

  # Calculate the required variables
  footprints_opt$perimeter <- st_length(st_boundary(footprints))
  footprints_opt$area <- footprints_opt$SHAPE_Area
  footprints_opt$shape_index <- schumms_shape_index(footprints_opt$perimeter, footprints_opt$area)
  footprints_opt$compactness <- (4 * pi * footprints_opt$area) / (footprints_opt$perimeter^2)
  footprints_opt$aspect_ratio <- footprints_opt$elon
  footprints_opt$angular_orientation <- footprints_opt$angle

  # Function to get the specified columns from footprints_opt
  get_columns <- function(fid) {
    footprints_opt[fid+1 , .(compactness, area, shape_index, elon, angular_orientation)]
  }

  # Get the columns for IN_FID and NEAR_FID using the function
  in_columns <- get_columns(buildingnet_opt$IN_FID)
  near_columns <- get_columns(buildingnet_opt$NEAR_FID)

  # Assign the columns to buildingnet_opt
  buildingnet_opt[, c("in_compactness", "in_area", "in_sind", "in_aspect_ratio", "in_angle") := in_columns]
  buildingnet_opt[, c("near_compactness", "near_area", "near_sind", "near_aspect_ratio", "near_angle") := near_columns]

  # Calculate the differences / ratios
  buildingnet_opt[, shape_index_diff := abs(in_sind - near_sind)]
  buildingnet_opt[, compactness_diff := abs(in_compactness - near_compactness)]
  buildingnet_opt[, aspect_ratio_diff := abs(in_aspect_ratio - near_aspect_ratio)]
  buildingnet_opt[, size_diff := abs(in_area - near_area)]
  buildingnet_opt[, angle_diff := gamma(in_angle, near_angle)]
  buildingnet_opt[, angle_diff_cos := abs(cos((in_angle - near_angle) * pi / 180))]

  buildingnet_opt[, shape_index_ratio := pmin(in_sind, near_sind) / pmax(in_sind, near_sind)]
  buildingnet_opt[, compactness_ratio := pmin(in_compactness, near_compactness) / pmax(in_compactness, near_compactness)]
  buildingnet_opt[, aspect_ratio_ratio := pmin(in_aspect_ratio, near_aspect_ratio) / pmax(in_aspect_ratio, near_aspect_ratio)]
  buildingnet_opt[, size_ratio := pmin(in_area, near_area) / pmax(in_area, near_area)]
  
   # Copy results back to the original buildingnet data.frame
  buildingnet$in_compactness <- buildingnet_opt$in_compactness
  buildingnet$near_compactness <- buildingnet_opt$near_compactness
  buildingnet$in_area <- buildingnet_opt$in_area
  buildingnet$near_area <- buildingnet_opt$near_area
  buildingnet$in_sind <- buildingnet_opt$in_sind
  buildingnet$near_sind <- buildingnet_opt$near_sind
  buildingnet$in_angle <- buildingnet_opt$in_angle
  buildingnet$near_angle <- buildingnet_opt$near_angle
  buildingnet$in_aspect_ratio <- buildingnet_opt$in_aspect_ratio
  buildingnet$near_aspect_ratio <- buildingnet_opt$near_aspect_ratio

  buildingnet$shape_index_diff <- buildingnet_opt$shape_index_diff
  buildingnet$compactness_diff <- buildingnet_opt$compactness_diff
  buildingnet$aspect_ratio_diff <- buildingnet_opt$aspect_ratio_diff
  buildingnet$size_diff <- buildingnet_opt$size_diff
  buildingnet$angle_diff <- buildingnet_opt$angle_diff
  buildingnet$angle_diff_cos <- buildingnet_opt$angle_diff_cos

  buildingnet$size_ratio <- buildingnet_opt$size_ratio
  buildingnet$shape_index_ratio <- buildingnet_opt$shape_index_ratio
  buildingnet$compactness_ratio <- buildingnet_opt$compactness_ratio
  buildingnet$aspect_ratio_ratio <- buildingnet_opt$aspect_ratio_ratio

  # Create a summary of buildingnet grouping by 'ID' and calculating various statistics
  buildingnet_summary <- buildingnet %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(
      n_vertex = length(unique(IN_FID, NEAR_FID)),
      # Various summary statistics for each variable of interest
      IQR_shape_index_diff = IQR(shape_index_diff, na.rm = T),
      mad_shape_index_diff = mad(shape_index_diff, na.rm = T),
      median_shape_index_diff = median(shape_index_diff, na.rm = T),
      IQR_compactness_diff = IQR(compactness_diff, na.rm = T),
      mad_compactness_diff = mad(compactness_diff, na.rm = T),
      median_compactness_diff = median(compactness_diff, na.rm = T),
      IQR_aspect_ratio_diff = IQR(aspect_ratio_diff, na.rm = T),
      mad_aspect_ratio_diff = mad(aspect_ratio_diff, na.rm = T),
      median_aspect_ratio_diff = median(aspect_ratio_diff, na.rm = T),
      IQR_size_diff = IQR(size_diff, na.rm = T),
      mad_size_diff = mad(size_diff, na.rm = T),
      median_size_diff = median(size_diff, na.rm = T),
      IQR_angle_diff = IQR(angle_diff, na.rm = T),
      mad_angle_diff = mad(angle_diff, na.rm = T),
      median_angle_diff = median(angle_diff, na.rm = T),
      IQR_size_ratio = IQR(size_ratio, na.rm = T),
      mad_size_ratio = mad(size_ratio, na.rm = T),
      median_size_ratio = median(size_ratio, na.rm = T),
      IQR_shape_index_ratio = IQR(shape_index_ratio, na.rm = T),
      mad_shape_index_ratio = mad(shape_index_ratio, na.rm = T),
      median_shape_index_ratio = median(shape_index_ratio, na.rm = T),
      IQR_compactness_ratio = IQR(compactness_ratio, na.rm = T),
      mad_compactness_ratio = mad(compactness_ratio, na.rm = T),
      median_compactness_ratio = median(compactness_ratio, na.rm = T),
      IQR_aspect_ratio_ratio = IQR(aspect_ratio_ratio, na.rm = T),
      mad_aspect_ratio_ratio = mad(aspect_ratio_ratio, na.rm = T),
      median_aspect_ratio_ratio = median(aspect_ratio_ratio, na.rm = T)
)

# All calculations are done, print to indicate the completion of the task
print("Calculations finished")
  
  
  
  footprints_opt$FID <- footprints_opt$FID - 1
  footprints_opt <- footprints_opt %>%
    left_join(buildingnet_opt %>% select(ID, IN_), by = c("FID" = "IN_")) %>%
    left_join(buildingnet_opt %>% select(ID, NEAR), by = c("FID" = "NEAR"), suffix = c(".IN_", ".NEAR")) %>%
    mutate(ID = ifelse(is.na(ID.IN_), ID.NEAR, ID.IN_)) %>%
    select(-ID.IN_, -ID.NEAR)
  
    
  results$ID <- as.numeric(gsub("_", "", results$ID))
  buildingnet_summary$ID <- as.numeric(gsub("_", "", buildingnet_summary$ID))
  
  am20 <- subset(all_master2, all_master2$yr==ts)
  
  am20$ID <- am20$ids
  am20_updated <- am20 %>%
    left_join(results, by = "ID")
  
  am20_updated <- am20_updated %>%
    left_join(buildingnet_summary, by = "ID")
  
  all <- rbind(all, am20_updated)
  
}
