library(data.table)
library(dplyr)
library(sf)
library(sfheaders)
library(tidyr)

# Set options
options(scipen=999)

# Calculate the settlement relationship types -----------------------------




time_steps <- c("1899")
phi_rev <- NULL
for (ts in time_steps){
  # Read the CSV file
  snpath <- paste0("C:/Users/yraeth/Documents/EMPHASES/4_projects/211104/Building_Network/settlement_net/df_",ts,"_90.csv")
  sn <- read_csv(snpath)
  
  # Split the 'Name' column into 'origin' and 'destination' columns
  sn <- sn %>%
    mutate(
      origin = str_split(Name, " - ", simplify = TRUE)[, 1],
      destination = str_split(Name, " - ", simplify = TRUE)[, 2]
    )
  
  
  # Remove rows where 'origin' and 'destination' are the same
  sn <- sn %>%
    filter(origin != destination)
  
  shapefile_path <- paste0("C:/Users/yraeth/Documents/EMPHASES/4_projects/211104/Building_Network/building_networks/bn_", ts, "_id.shp")
  bn <- st_read(shapefile_path)
  bn_dt <- as.data.table(bn)
  
  # Melt the data table into long format, grouping by ID and value
  bn_melted <- melt(bn_dt, id.vars = "ID", measure.vars = c("IN_", "NEAR"), variable.name = "source", value.name = "value")[, .(unique_count = uniqueN(value)), by = .(ID, value)]
  
  # Summarize the data table to get the unique counts for every ID
  bn_unique_counts <- bn_melted[, .(unique_count = sum(unique_count)), by = ID]
  
  
  
  bn_unique_counts <- bn_dt %>%
    select(ID, IN_, NEAR, AREA, AREA_1) %>%
    pivot_longer(cols = c(IN_, NEAR), names_to = "source", values_to = "value") %>%
    group_by(ID, value) %>%
    summarize(total_count = n(), total_area = sum(unique(c(AREA, AREA_1)))) %>%
    group_by(ID) %>%
    summarize(unique_count = n(), total_area = sum(total_area))
  sn <- merge(sn, bn_unique_counts, by.x = "origin", by.y="ID")
  sn <- sn %>%
    rename(origin_total_area = total_area)
  sn <- merge(sn, bn_unique_counts, by.x = "destination", by.y="ID")
  sn <- sn %>%
    rename(destination_total_area = total_area)
  sn <- sn %>% select(-unique_count.y)
  sn <- sn %>% select(-unique_count.x)
  
  unique_origins <- unique(sn$origin) # Get unique values in the "origin" column
  
  
  temp <- NULL
  sn <- as.data.table(sn)
  
  
  for (i in unique_origins) {
    origin_data <- sn[origin == i] # Subset data for current origin
    
    # Convert Total_time to minutes and round to nearest 5-minute interval
    origin_data[, Total_time_min := round(Total_time/60/5)*5]
    
    # Sort the data by Total_time_min and destination_total_area
    origin_data <- origin_data[order(Total_time_min, -destination_total_area)]
    
    # Find the maximum destination_total_area reachable at each 5-minute interval
    max_area <- origin_data[, .(max_area = max(destination_total_area)), by = Total_time_min]
    
    # For each interval, fill the max_area value with the maximum reachable value so far
    max_area[, max_area := cummax(max_area)]
    
    max_area_pruned <- max_area[Total_time_min <= 60]
    # Join the max_area_pruned table with the origin_total_area column from the origin_data table
    max_area_pruned[, origin_total_area := origin_data$origin_total_area[1]]
    
    max_area_pruned$mu <- max_area_pruned$origin_total_area / max_area_pruned$max_area
    
    if (nrow(max_area_pruned) < 2 | length(unique(max_area_pruned$Total_time_min)) < 2) { # check if max_area_pruned has fewer than 2 rows or only 1 unique value of Total_time_min
      phi <- 999
    } else {
      # Fit a linear regression model with mu as the response variable and Total_time_min as the predictor variable
      model <- lm(mu ~ Total_time_min, data = max_area_pruned)
      
      # Display the model summary
      summary(model)
      # Get the slope and intercept from the linear regression model
      slope <- coef(model)["Total_time_min"]
      intercept <- coef(model)["(Intercept)"]
      
      # Calculate the Total_time_min value where mu = 1
      # Calculate phi
      if (slope == 0 && min(max_area_pruned$mu) > 1) {
        phi <- 9999
      } else {
        phi <- (1 - intercept) / slope
      }
    }
    temp <- rbind(temp, data.frame(i = i, phi = phi))
  }
  
  
  # Remove duplicate rows based on the new key
  sn_unique <- sn %>%
    distinct(origin, .keep_all = TRUE)
  
  sn <- merge(sn_unique, temp, by.x = "origin", by.y="i")
  
  
  phi_rev <- rbind(phi_rev, sn) 
}

sn <- phi_rev

#clean up
sn[is.infinite(phi), phi := -9999]


# Create a new column with the specified conditions
sn <- sn %>%
  mutate(new_column = case_when(
    origin_count <= 100 & phi > 0                  ~ "a",
    origin_count <= 100 & phi > -400 & phi <= 0    ~ "b",
    origin_count <= 100 & phi <= -400              ~ "c",
    origin_count > 100 & phi > 0                   ~ "A",
    origin_count > 100 & phi > -400 & phi <= 0     ~ "B",
    origin_count > 100 & phi <= -400               ~ "C"
  ))

# Convert the new column to a factor with the specified order
sn$new_column <- factor(sn$new_column, levels = c("a", "b", "c", "A", "B", "C"), ordered = TRUE)

