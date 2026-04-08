# =================================================================
# SCRIPT: infestation_types_plots.R
# PURPOSE: Visualizing infestation types (Eggs, Larvae, Pupae, Exits)
# =================================================================

generate_infestation_type_plots <- function(fly_counts, sampling) {
  
  dimoi <- names(table(sampling$DHMOS_DESCR))

  for (desc in dimoi) {
    years <- names(table(format(sampling[sampling$DHMOS_DESCR == desc, ]$DEIGMA_DATE, "%Y")))
    
    for (year in years) {
      
      aggr_deig_a <- sampling[sampling$DHMOS_DESCR == desc & format(sampling$DEIGMA_DATE, "%Y") == year, ]
      aggr_met_a <- fly_counts[fly_counts$DHMOS_DESCR == desc & format(fly_counts$CHANGE_DATE, "%Y") == year, ]

      if (nrow(aggr_deig_a) > 0) {
        
        # Area plot logic using ggplot2
        # Note: Using your original column names but English labels for the legend
        infest_plot <- ggplot(aggr_deig_a, aes(x = DEIGMA_DATE)) +
          geom_area(aes(y = AYGA + PRONYFH_A + PRONYFH_B + PRONYFH_G + NYFH + EXITS, fill = "Exits")) +
          geom_area(aes(y = AYGA + PRONYFH_A + PRONYFH_B + PRONYFH_G + NYFH, fill = "Pupae")) +
          geom_area(aes(y = AYGA + PRONYFH_A + PRONYFH_B + PRONYFH_G, fill = "Larvae G")) +
          geom_area(aes(y = AYGA + PRONYFH_A + PRONYFH_B, fill = "Larvae B")) +
          geom_area(aes(y = AYGA + PRONYFH_A, fill = "Larvae A")) +
          geom_area(aes(y = AYGA, fill = "Eggs")) +
          scale_fill_manual(values = c("Eggs"="#fee090", "Larvae A"="#e0f3f8", "Larvae B"="#abd9e9", 
                                       "Larvae G"="#74add1", "Pupae"="#4575b4", "Exits"="#313695"),
                            name = "Infestation Type") +
          labs(title = paste(desc, "-", year), x = "Date", y = "Count") +
          theme_minimal()

        if (nrow(aggr_met_a) > 0) {
          infest_plot <- infest_plot +
            geom_line(data = aggr_met_a, aes(x = CHANGE_DATE, y = SYNOLO_DAKON), color = "black") +
            geom_point(data = aggr_met_a, aes(x = CHANGE_DATE, y = SYNOLO_DAKON), color = "black")
        }

        ggsave(filename = paste("plots/inf_type_", year, "-", desc, ".jpg", sep = ""), 
               plot = infest_plot)
      }
    }
  }
}
