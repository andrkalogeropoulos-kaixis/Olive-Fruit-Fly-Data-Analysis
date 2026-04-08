# =================================================================
# SCRIPT: weather_pest_correlation.R
# PURPOSE: Visualizing fly population, infestations, and weather data
# =================================================================

# We wrap your code in a function that uses the variables from main_analysis.R
generate_weather_pest_plots <- function(fly_counts, sampling, treatments, weather) {

  # Create a table of unique Municipality names 
  dimoi <- names(table(fly_counts$DHMOS_DESCR))

  for (desc in dimoi) {
    # Create a table of years for the specific Municipality
    years <- names(table(format(fly_counts[fly_counts$DHMOS_DESCR == desc, ]$CHANGE_DATE, "%Y")))
    
    for (year in years) {
      
      # Data Filtering 
      aggr_met_a <- fly_counts[fly_counts$DHMOS_DESCR == desc & format(fly_counts$CHANGE_DATE, "%Y") == year, ]
      aggr_deig_a <- sampling[sampling$DHMOS_DESCR == desc & format(sampling$DEIGMA_DATE, "%Y") == year, ]
      psek_a <- treatments[treatments$DHMOS_DESCR == desc & format(treatments$SPRAY_STDATE, "%Y") == year, ]
      
      # Weather data filtering (Matching by METEO_ID)
      meteo_id <- aggr_met_a$METEO_ID[1]
      meteo_a <- weather[weather$METEO_ID == meteo_id & format(weather$DATE, "%Y") == year, ]

      if (nrow(aggr_met_a) > 0) {
        
        # Save to JPEG
        jpeg(paste("plots/weather_", year, "-", desc, ".jpg", sep=''), width = 1000, height = 800)

        # Set up a 2x2 grid for plots (Your original layout)
        par(mfrow=c(2,2))

        # --- Plot 1: Fly Population & Treatments ---
        plot(aggr_met_a$CHANGE_DATE, aggr_met_a$SYNOLO_DAKON, 
             type='l', col='blue', xlab='Date', ylab='Fly Count',
             main='Population & Treatments')
        
        # Pink Rectangles for Treatments 
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
        
        # --- Plot 2: Temperature vs Fly Count ---
        plot(meteo_a$DATE, meteo_a$MEAN_TEMP, type='l', col='red', 
             xlab='Date', ylab='Temp (C)', main='Temperature Analysis')
        # Adding vertical lines on measurement dates (Your original abline logic)
        abline(v=aggr_met_a$CHANGE_DATE, lwd=.5, col='gray48')

        # --- Plot 3: Infestations ---
        if (nrow(aggr_deig_a) > 0) {
          plot(aggr_deig_a$DEIGMA_DATE, aggr_deig_a$PROSVOLES, 
               type='b', col='darkgreen', pch=20,
               xlab='Date', ylab='Infestation %', main='Fruit Infestation')
        } else {
          plot.new() # Empty plot if no data
        }

        # --- Plot 4: Humidity ---
        plot(meteo_a$DATE, meteo_a$OUT_HUM, type='l', col='cyan4', 
             xlab='Date', ylab='Humidity %', main='Relative Humidity')
        abline(v=aggr_met_a$CHANGE_DATE, lwd=.5, col='gray48')

        # Main Title for the whole page
        mtext(paste(desc, '-', year), side=3, outer=TRUE, line=-2, cex=1.2)

        dev.off()
      }
    }
  }
}
