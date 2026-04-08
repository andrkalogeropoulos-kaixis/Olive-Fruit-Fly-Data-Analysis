# =================================================================
# SCRIPT: correlation_analysis.R
# PURPOSE: Statistical correlation between populations and infestations
# =================================================================

run_correlation_analysis <- function(fly_counts, sampling) {
  
  # Aggregating fly counts by Municipality and Year
  agg_fly_counts <- aggregate(cbind(MALE, FEMALE, SYNOLO_DAKON) ~ DHMOS_DESCR + format(CHANGE_DATE, "%Y"), 
                              data = fly_counts, 
                              FUN = mean)
  
  colnames(agg_fly_counts)[2] <- "YEAR"
  
  # Aggregating infestations by Municipality and Year
  agg_infestations <- aggregate(PROSVOLES ~ DHMOS_DESCR + format(DEIGMA_DATE, "%Y"), 
                                data = sampling, 
                                FUN = mean)
  
  colnames(agg_infestations)[2] <- "YEAR"
  
  # Merging datasets
  combined_data <- merge(agg_fly_counts, agg_infestations, by = c("DHMOS_DESCR", "YEAR"))
  
  # Correlation calculation
  cor_value <- cor(combined_data$FEMALE, combined_data$PROSVOLES, use = "complete.obs")
  print(paste("Correlation Female Adults / Infestation:", round(cor_value, 2)))
  
  # Visualization
  plot(combined_data$FEMALE, combined_data$PROSVOLES,
       main = "Correlation: Female Adults vs Infestation",
       xlab = "Average Female Flies",
       ylab = "Average Infestation Rate",
       pch = 19, col = "darkgreen")
  
  abline(lm(PROSVOLES ~ FEMALE, data = combined_data), col = "red")
  
  return(combined_data)
}
