# =================================================================
# SCRIPT: pest_infestation_treatment_plots.R
# PURPOSE: Plotting pest populations, infestations and treatments
# =================================================================

generate_pest_impact_plots <- function(fly_counts, sampling, treatments) {

  dimoi <- names(table(fly_counts$DHMOS_DESCR))

  for (desc in dimoi) {
    years <- names(table(format(fly_counts[fly_counts$DHMOS_DESCR == desc, ]$CHANGE_DATE, "%Y")))
    
    for (year in years) {
      
      aggr_met_a <- fly_counts[fly_counts$DHMOS_DESCR == desc & format(fly_counts$CHANGE_DATE, "%Y") == year, ]
      aggr_deig_a <- sampling[sampling$DHMOS_DESCR == desc & format(sampling$DEIGMA_DATE, "%Y") == year, ]
      psek_a <- treatments[treatments$DHMOS_DESCR == desc & format(treatments$SPRAY_STDATE, "%Y") == year, ]

      if (nrow(aggr_met_a) > 0) {
        
        jpeg(paste("plots/", year, "-", desc, ".jpg", sep=''))

        plot(aggr_met_a$CHANGE_DATE, aggr_met_a$SYNOLO_DAKON, 
             type='l', col='blue', 
             xlab='Date', ylab='Fly Count')

        if (nrow(psek_a) > 0) {
          apply(psek_a, 1, function(psek_row) { 
            rect(as.Date(psek_row['SPRAY_STDATE']), 
                 min(aggr_met_a$SYNOLO_DAKON, na.rm=T), 
                 as.Date(psek_row['SPRAY_ENDATE']), 
                 max(aggr_met_a$SYNOLO_DAKON, na.rm=T), 
                 col='pink', border=NA) 
          })
        }

        points(aggr_met_a$CHANGE_DATE, aggr_met_a$SYNOLO_DAKON, pch=20)

        if (nrow(aggr_deig_a) > 0) {
          lines(aggr_deig_a$DEIGMA_DATE, aggr_deig_a$PROSVOLES, col='green')
          points(aggr_deig_a$DEIGMA_DATE, aggr_deig_a$PROSVOLES, col='green', pch=20)
        }

        title(paste(desc, '-', year))
        
        legend("topright", 
               c('Adult Population', 'Infestations', 'Treatments'), 
               col=c('blue', 'green', 'pink'), 
               lty=1, pch=20, cex=.7)

        dev.off()
      }
    }
  }
}
