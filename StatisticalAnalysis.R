library(dunn.test)
library(dplyr)
library(xtable)




# Define the parameters to be changed
years <- c(1899, 1918, 1933,1959,1970, 1978,2020) # add any other years you want to analyze
col_names <- c(
  "median_size_diff",
  "IQR_size_diff",
  "mad_size_diff",
 
  "median_angle_diff",
  "IQR_angle_diff",
  "mad_angle_diff",

  "median_aspect_ratio_diff",
  "IQR_aspect_ratio_diff",
  "mad_aspect_ratio_diff",

  "median_compactness_diff",
  "IQR_compactness_diff",
  "mad_compactness_diff",
 
  "median_shape_index_diff",
  "IQR_shape_index_diff",
  "mad_shape_index_diff",

  "median_size_ratio",
  "IQR_size_ratio",
  "mad_size_ratio",
 
  "median_aspect_ratio_ratio",
  "IQR_aspect_ratio_ratio",
  "mad_aspect_ratio_ratio",
 
  "median_compactness_ratio",
  "IQR_compactness_ratio",
  "mad_compactness_ratio",
  
  "median_shape_index_ratio",
  "IQR_shape_index_ratio",
  "mad_shape_index_ratio",
  
  "Fractal_Dimension",
  "fs_nnindex",
 
  "n_vertex"
 
) # add any other column names you want to analyze

stat_anal_df <- data.frame() # create an empty data frame to store the results


year ="2020"
col_name="median_size_diff"

for (year in years) {
  for (col_name in col_names) {
    # Filter the lowercase categories
    lowercase_df <- t %>% filter(NewCat %in% c("a", "b", "c")) #t is a df where every row is a settlement and the columns are the summary statistics, year, etc.
    yoi <- lowercase_df %>% filter(yr == year)
    
    group_a <- na.omit(as.numeric(yoi[[col_name]][yoi$NewCat == "a"]))
    group_b <- na.omit(as.numeric(yoi[[col_name]][yoi$NewCat == "b"]))
    group_c <- na.omit(as.numeric(yoi[[col_name]][yoi$NewCat == "c"]))
    
    n_a <- length(group_a)
    n_b <- length(group_b)
    n_c <- length(group_c)
    
    
    mean_a <-mean(group_a)
    mean_b <-mean(group_b)
    mean_c <-mean(group_c)
    
    median_a <-median(group_a)
    median_b <-median(group_b)
    median_c <-median(group_c)
    
    sd_a <-sd(group_a)
    sd_b <-sd(group_b)
    sd_c <-sd(group_c)
    
    # Perform Shapiro-Wilk tests
    shapiro_test_a <- shapiro.test(group_a)
    shapiro_test_b <- shapiro.test(group_b)
    shapiro_test_c <- shapiro.test(group_c)
    
    # Create variables to store test results
    kruskal_test_result <- NA
    dunn_test_result <- NA
    pairwise_ttest_results <- NA
    pairwise_ttest_results_ab <- NA
    pairwise_ttest_results_ac <- NA
    pairwise_ttest_results_bc <- NA
    dunn_test_result_ab <- NA
    dunn_test_result_ac <- NA
    dunn_test_result_bc <- NA
    
    # Check if any Shapiro-Wilk test is significant
    if (shapiro_test_a$p.value < 0.05 || shapiro_test_b$p.value < 0.05 || shapiro_test_c$p.value < 0.05) {
      # Perform Kruskal-Wallis test
      kruskal_test <- kruskal.test(list(group_a, group_b, group_c))
      
      # Check if Kruskal-Wallis test is significant
      if (kruskal_test$p.value < 0.05) {
        # Perform Dunn's test
        dunn_test <- dunn.test(list(group_a, group_b, group_c), method = "bonferroni")
        dunn_test_result_ab <- dunn_test$P.adjusted[1]
        dunn_test_result_ac <- dunn_test$P.adjusted[2]
        dunn_test_result_bc <- dunn_test$P.adjusted[3]
      } 
      kruskal_test_result <- kruskal_test$p.value
    } else {
      cat("All Shapiro-Wilk tests not significant. Performing pairwise t-tests.\n")
      # Perform pairwise t-tests with Bonferroni correction
      pairwise_ttest_results <- pairwise.t.test(
        yoi[[col_name]],
        yoi$NewCat,
        p.adjust.method = "bonferroni"
      )
      pairwise_ttest_results_ab <- pairwise_ttest_results$p.value[1,1]
      pairwise_ttest_results_ac <- pairwise_ttest_results$p.value[2,1]
      pairwise_ttest_results_bc <- pairwise_ttest_results$p.value[2,2]
    }
    
    # Create a data frame to store the results for this iteration
    iteration_results <- data.frame(
      year = year,
      metric = col_name,
      STA_P_A = shapiro_test_a$p.value,
      STA_P_B = shapiro_test_b$p.value,
      STA_P_C = shapiro_test_c$p.value,
      KWT_R = kruskal_test_result,
      DTR_AB = dunn_test_result_ab,
      DTR_AC = dunn_test_result_ac,
      DTR_BC = dunn_test_result_bc,
      PTTR_AB = pairwise_ttest_results_ab,
      PTTR_AC = pairwise_ttest_results_ac,
      PTTR_BC = pairwise_ttest_results_bc,
      Mean_A = mean_a,
      Mean_B = mean_b,
      Mean_C = mean_c,
      Median_A = median_a,
      Median_B = median_b,
      Median_C = median_c,
      SD_A = sd_a,
      SD_B = sd_b,
      SD_C = sd_c,
      N_a = n_a,
      N_b = n_b,
      N_c = n_c
    )
    
    # Append the iteration results to the main results data frame
    stat_anal_df <- rbind(stat_anal_df, iteration_results)
  }
}



stat_anal_df[,3:21] <- round(stat_anal_df[,3:21], digits = 5)


stat_anal_df <- stat_anal_df %>% 
  dplyr::mutate(statistic = str_split_fixed(metric, "_", 2)[, 1],
                indicator = str_split_fixed(metric, "_", 2)[, 2]) %>% 
  dplyr::select(year, metric, statistic, indicator, everything())

stat_anal_df <- stat_anal_df %>%
  mutate(sign = case_when(
    KWT_R < 0.001 ~ "***",
    KWT_R < 0.01  ~ "**",
    KWT_R < 0.05  ~ "*",
    TRUE          ~ ""
  ))

s <- subset(stat_anal_df, stat_anal_df$metric=="IQR_size_diff")
s <- subset(stat_anal_df, stat_anal_df$metric=="IQR_angle_diff")
s <- subset(stat_anal_df, stat_anal_df$metric=="IQR_compactness_diff")
s <- subset(stat_anal_df, stat_anal_df$metric=="IQR_aspect_ratio_diff")
s <- subset(stat_anal_df, stat_anal_df$metric=="IQR_shape_index_diff")



s <- subset(stat_anal_df, stat_anal_df$metric=="mad_size_diff")
s <- subset(stat_anal_df, stat_anal_df$metric=="mad_angle_diff")
s <- subset(stat_anal_df, stat_anal_df$metric=="mad_compactness_diff")
s <- subset(stat_anal_df, stat_anal_df$metric=="mad_aspect_ratio_diff")
s <- subset(stat_anal_df, stat_anal_df$metric=="mad_shape_index_diff")




s <- subset(stat_anal_df, stat_anal_df$metric=="median_size_diff")
s <- subset(stat_anal_df, stat_anal_df$metric=="median_angle_diff")
s <- subset(stat_anal_df, stat_anal_df$metric=="median_compactness_diff")
s <- subset(stat_anal_df, stat_anal_df$metric=="median_aspect_ratio_diff")
s <- subset(stat_anal_df, stat_anal_df$metric=="median_shape_index_diff")



s <- subset(stat_anal_df, stat_anal_df$metric=="fs_nnindex")

s <- subset(stat_anal_df, stat_anal_df$metric=="Fractal_Dimension")

s <- subset(stat_anal_df, stat_anal_df$metric=="n_vertex")



# Expand the data frame
df_expanded <- s %>%
  slice(rep(row_number(), each = 3))

# Add the Type1 and Type2 columns
df_expanded$Type1 <- rep(c("a", "a", "b"), times = nrow(s))
df_expanded$Type2 <- rep(c("b", "c", "c"), times = nrow(s))

# Add the p-value, median1, and median2 columns
df_expanded$DTR_p_value <- ifelse(df_expanded$Type1 == "a" & df_expanded$Type2 == "b", df_expanded$DTR_AB, NA)
df_expanded$DTR_p_value <- ifelse(df_expanded$Type1 == "a" & df_expanded$Type2 == "c", df_expanded$DTR_AC, df_expanded$DTR_p_value)
df_expanded$DTR_p_value <- ifelse(df_expanded$Type1 == "b" & df_expanded$Type2 == "c", df_expanded$DTR_BC, df_expanded$DTR_p_value)

df_expanded$median1 <- ifelse(df_expanded$Type1 == "a", df_expanded$Median_A, NA )
df_expanded$median1 <- ifelse(df_expanded$Type1 == "b", df_expanded$Median_B, df_expanded$median1)
df_expanded$median1 <- ifelse(df_expanded$Type1 == "c", df_expanded$Median_C, df_expanded$median1)

df_expanded$median2 <- ifelse(df_expanded$Type2 == "a", df_expanded$Median_A, NA )
df_expanded$median2 <- ifelse(df_expanded$Type2 == "b", df_expanded$Median_B, df_expanded$median2)
df_expanded$median2 <- ifelse(df_expanded$Type2 == "c", df_expanded$Median_C, df_expanded$median2)

df_expanded$n1 <- ifelse(df_expanded$Type1 == "a", df_expanded$N_a, NA )
df_expanded$n1 <- ifelse(df_expanded$Type1 == "b", df_expanded$N_b, df_expanded$n1)
df_expanded$n1 <- ifelse(df_expanded$Type1 == "c", df_expanded$N_c, df_expanded$n1)

df_expanded$n2 <- ifelse(df_expanded$Type2 == "a", df_expanded$N_a, NA )
df_expanded$n2 <- ifelse(df_expanded$Type2 == "b", df_expanded$N_b, df_expanded$n2)
df_expanded$n2 <- ifelse(df_expanded$Type2 == "c", df_expanded$N_c, df_expanded$n2)

new_cols = c('year', 'Type1', 'Type2', 'n1', 'n2', 'median1', 'median2', 'KWT_R', 'DTR_p_value')
df_print = df_expanded[new_cols]

# round the DTR_p_value column to 3 digits
df_print$DTR_p_value <- ifelse(df_print$DTR_p_value < 0.001, "\\textless{}0.001", 
                               round(df_print$DTR_p_value, 3))

df_print$KWT_R <- ifelse(df_print$KWT_R < 0.001, "\\textless{}0.001", 
                         round(df_print$KWT_R, 3))

df_print$Significance <- ifelse(df_print$DTR_p_value < 0.001, "***",
                                ifelse(df_print$DTR_p_value < 0.01, "**",
                                       ifelse(df_print$DTR_p_value < 0.05, "*", "")))
df_print$median1 <- round(df_print$median1, digits = 3)
df_print$median2 <- round(df_print$median2, digits = 3)


df_print$year <- as.character(df_print$year)

# Create LaTeX table
# Create LaTeX table
latex_table <- xtable(df_print,digits=3,
                      caption = "Your Table Caption", 
                      label = "tab:your_label",
                      align = c("rrrrr|rrrrrr"),
                      
                      include.rownames = FALSE)


print(latex_table, 
      sanitize.text.function = identity,
      hline.after = NULL,
      add.to.row = list(pos = list(0, seq(3, nrow(df_print), by = 3)),
                        command = c("\\midrule\n", "\\rule{0pt}{2.5ex}\n")),
      include.rownames = FALSE)
