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
  # Set the paths to your shapefiles
  shapefile_path1 <- paste0("C:/Users/<username>/Documents/<project_folder>/building_footrpints/bf_", ts, "_bpl.shp")
  shapefile_path2 <- paste0("C:/Users/<username>/Documents/<project_folder>/building_networks/bn_", ts, "_id.shp")
  all_master2 <- read.csv("C:/Users/<username>/Documents/<project_folder>/settlement_network_types.csv")
  
  
  # Read shapefile 1 (with geometry)
  shapefile1 <- st_read(shapefile_path1)
  footprints <- shapefile1
  footprints$FID <- as.integer(row.names(footprints))
  
  
  # Read shapefile 2 (attribute table only)
  attribute_table_only <- st_read(shapefile_path2, options = "RECORDS_ONLY")
  buildingnet <- attribute_table_only
  
  print("finished reading shapefiles")
  
  # Create new objects for optimized processing
  buildingnet_opt <- data.table(buildingnet)
  footprints_opt <- data.table(footprints)
  
  # Calculate the required variables
  footprints_opt$perimeter <- st_length(st_boundary(footprints))
  footprints_opt$area <- footprints_opt$bf_area
  footprints_opt$shape_index <- schumms_shape_index(footprints_opt$perimeter, footprints_opt$area)
  footprints_opt$compactness <- (4 * pi * footprints_opt$area) / (footprints_opt$perimeter^2)
  footprints_opt$aspect_ratio <- footprints_opt$elon
  footprints_opt$angular_orientation <- footprints_opt$angle
  
  # Create a function to get the specified columns from footprints_opt
  get_columns <- function(fid) {
    footprints_opt[fid+1 , .(compactness, area, shape_index, elon, angular_orientation)]
  }
  
  # Get the columns for IN_FID and NEAR_FID using the function
  in_columns <- get_columns(buildingnet_opt$IN_)
  near_columns <- get_columns(buildingnet_opt$NEAR)
  
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
  
  buildingnet_summary <- buildingnet %>%
    group_by(ID) %>%
    summarise(
      mean_shape_index_diff = mean(shape_index_diff),
      median_shape_index_diff = median(shape_index_diff),
      gini_shape_index_diff = Gini(shape_index_diff),
      sd_shape_index_diff = sd(shape_index_diff),
      thiel_shape_index_diff = ineq(shape_index_diff, type = "Theil"),
      cv_shape_index_diff = sd(shape_index_diff) / mean(shape_index_diff),
      
      # compactness_diff
      mean_compactness_diff = mean(compactness_diff),
      median_compactness_diff = median(compactness_diff),
      gini_compactness_diff = Gini(compactness_diff),
      sd_compactness_diff = sd(compactness_diff),
      thiel_compactness_diff = ineq(compactness_diff, type = "Theil"),
      cv_compactness_diff = sd(compactness_diff) / mean(compactness_diff),      
      # aspect_ratio_diff
      mean_aspect_ratio_diff = mean(aspect_ratio_diff),
      median_aspect_ratio_diff = median(aspect_ratio_diff),
      gini_aspect_ratio_diff = Gini(aspect_ratio_diff),
      sd_aspect_ratio_diff = sd(aspect_ratio_diff),
      thiel_aspect_ratio_diff = ineq(aspect_ratio_diff, type = "Theil"),
      cv_aspect_ratio_diff = sd(aspect_ratio_diff) / mean(aspect_ratio_diff),   
      
      # size_diff
      mean_size_diff = mean(size_diff),
      median_size_diff = median(size_diff),
      gini_size_diff = Gini(size_diff),
      sd_size_diff = sd(size_diff),
      thiel_size_diff = ineq(size_diff, type = "Theil"),
      cv_size_diff = sd(size_diff) / mean(size_diff),
      
      # angle_diff
      mean_angle_diff = mean(angle_diff),
      median_angle_diff = median(angle_diff),
      gini_angle_diff = Gini(angle_diff),
      sd_angle_diff = sd(angle_diff),
      thiel_angle_diff = ineq(angle_diff, type = "Theil"),
      cv_angle_diff = sd(angle_diff) / mean(angle_diff),
      
      
      # angle_diff_cos
      mean_angle_cos_diff = 45-abs(rad2deg(acos(mean(angle_diff_cos)))-45),
      median_angle_cos_diff = 45-abs(rad2deg(acos(median(angle_diff_cos)))-45),
      gini_angle_cos_diff = 45-abs(rad2deg(acos(Gini(angle_diff_cos)))-45),
      sd_angle_cos_diff = 45-abs(rad2deg(acos(sd(angle_diff_cos)))-45),
      thiel_angle_cos_diff = 45-abs(rad2deg(acos(ineq(angle_diff_cos, type = "Theil")))-45),
      cv_angle_cos_diff = 45-abs(rad2deg(acos(sd(angle_diff_cos)))-45) /45-abs(rad2deg(acos(mean(angle_diff_cos)))-45),
      
      
      # size_ratio
      mean_size_ratio = mean(size_ratio),
      median_size_ratio = median(size_ratio),
      gini_size_ratio = Gini(size_ratio),
      sd_size_ratio = sd(size_ratio),
      thiel_size_ratio = ineq(size_ratio, type = "Theil"),
      cv_size_ratio = sd(size_ratio) / mean(size_ratio),
      
      # shape_index_ratio
      mean_shape_index_ratio = mean(shape_index_ratio),
      median_shape_index_ratio = median(shape_index_ratio),
      gini_shape_index_ratio = Gini(shape_index_ratio),
      sd_shape_index_ratio = sd(shape_index_ratio),
      thiel_shape_index_ratio = ineq(shape_index_ratio, type = "Theil"),
      cv_shape_index_ratio = sd(shape_index_ratio) / mean(shape_index_ratio),
      
      # compactness_ratio
      mean_compactness_ratio = mean(compactness_ratio),
      median_compactness_ratio = median(compactness_ratio),
      gini_compactness_ratio = Gini(compactness_ratio),
      sd_compactness_ratio = sd(compactness_ratio),
      thiel_compactness_ratio = ineq(compactness_ratio, type = "Theil"),
      cv_compactness_ratio = sd(compactness_ratio) / mean(compactness_ratio),
      
      # aspect_ratio_ratio
      mean_aspect_ratio_ratio = mean(aspect_ratio_ratio),
      median_aspect_ratio_ratio = median(aspect_ratio_ratio),
      gini_aspect_ratio_ratio = Gini(aspect_ratio_ratio),
      sd_aspect_ratio_ratio = sd(aspect_ratio_ratio),
      thiel_aspect_ratio_ratio = ineq(aspect_ratio_ratio, type = "Theil"),
      cv_aspect_ratio_ratio = sd(aspect_ratio_ratio) / mean(aspect_ratio_ratio)
    )
  
  
  
  footprints_opt$FID <- footprints_opt$FID - 1
  footprints_opt <- footprints_opt %>%
    left_join(buildingnet_opt %>% select(ID, IN_), by = c("FID" = "IN_")) %>%
    left_join(buildingnet_opt %>% select(ID, NEAR), by = c("FID" = "NEAR"), suffix = c(".IN_", ".NEAR")) %>%
    mutate(ID = ifelse(is.na(ID.IN_), ID.NEAR, ID.IN_)) %>%
    select(-ID.IN_, -ID.NEAR)
  
  
  print("beginning results")
  
  # Perform calculations for each ID
  results <- footprints_opt %>%
    group_by(FID) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(ID) %>%
    summarise(
      #Per Buildingfootprint
      mean_area = mean(area),
      median_area = median(area),
      gini_area = Gini(area),
      sd_area = sd(area),
      thiel_interdecil_area = ineq(area, type = "Theil"),
      cv_area = sd(area) / mean(area),
      mean_shape_index = mean(shape_index),
      median_shape_index = median(shape_index),
      gini_shape_index = Gini(shape_index),
      sd_shape_index = sd(shape_index),
      thiel_interdecil_shape_index = ineq(shape_index, type = "Theil"),
      cv_shape_index = sd(shape_index) / mean(shape_index),
      mean_compactness = mean(compactness),
      median_compactness = median(compactness),
      gini_compactness = Gini(compactness),
      sd_compactness = sd(compactness),
      thiel_interdecil_compactness = ineq(compactness, type = "Theil"),
      cv_compactness = sd(compactness) / mean(compactness),
      mean_aspect_ratio = mean(aspect_ratio),
      median_aspect_ratio = median(aspect_ratio),
      gini_aspect_ratio = Gini(aspect_ratio),
      sd_aspect_ratio = sd(aspect_ratio),
      thiel_interdecil_aspect_ratio = ineq(aspect_ratio, type = "Theil"),
      cv_aspect_ratio = sd(aspect_ratio) / mean(aspect_ratio)
    )      
  
  results <- as.data.frame(results)
  
  print("finished results")
  
  
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