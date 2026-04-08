# =================================================================
# SCRIPT: gender_distribution_plots.R
# PURPOSE: Visualizing Male vs Female fly populations, Wind Speed, and Rain
# =================================================================

generate_gender_weather_plots <- function(fly_counts, sampling, treatments, weather) {

  dimoi <- names(table(fly_counts$DHMOS_DESCR))

  for (desc in dimoi) {
    years <- names(table(format(fly_counts[fly_counts$DHMOS_DESCR == desc, ]$CHANGE_DATE, "%Y")))
    
    for (year in years) {
      
      # Data filtering
      aggr_met_a <- fly_counts[fly_counts$DHMOS_DESCR == desc & format(fly_counts$CHANGE_DATE, "%Y") == year, ]
      psek_a <- treatments[treatments$DHMOS_DESCR == desc & format(treatments$SPRAY_STDATE, "%Y") == year, ]
      
      # Weather data filtering
      meteo_id <- aggr_met_a$METEO_ID[1]
      meteo_a <- weather[weather$METEO_ID == meteo_id & format(weather$DATE, "%Y") == year, ]

      if (nrow(aggr_met_a) > 0) {
        
        jpeg(paste("plots/gender_weather_", year, "-", desc, ".jpg", sep=''), width = 1000, height = 800)

        par(mfrow=c(2,2))

        # --- Plot 1: Male vs Female Populations ---
        plot(aggr_met_a$CHANGE_DATE, aggr_met_a$FEMALE, type='l', col='red', 
             xlab='Date', ylab='Fly Count', main='Male vs Female Populations')
        lines(aggr_met_a$CHANGE_DATE, aggr_met_a$MALE, col='blue')
        
        # Treatment Rectangles
        if (nrow(psek_a) > 0) {
          apply(psek_a, 1, function(psek_row) { 
            rect(as.Date(psek_row['SPRAY_STDATE']), 0, 
                 as.Date(psek_row['SPRAY_ENDATE']), 
                 max(aggr_met_a$SYNOLO_DAKON, na.rm=T), 
                 col='pink', border=NA) 
          })
        }
        
        legend("topright", c('Females', 'Males', 'Treatments'), 
               col=c('red', 'blue', 'pink'), lty=1, cex=.7)

        # --- Plot 2: Wind Speed ---
        plot(meteo_a$DATE, meteo_a$HIGH_WIND_SPEED, type='n', 
             xlab='Date', ylab='Wind Speed (km/h)', main='High Wind Speed')
        
        # Highlight high wind periods (> 28.8 km/h) with pink rectangles
        rect(min(meteo_a$DATE), 28.8, max(meteo_a$DATE), max(meteo_a$HIGH_WIND_SPEED, na.rm=T), 
             col='pink', border=NA)
        
        lines(meteo_a$DATE, meteo_a$HIGH_WIND_SPEED, col='blue')
        abline(v=aggr_met_a$CHANGE_DATE, lwd=.5, col='gray48')

        # --- Plot 3: Rain ---
        plot(meteo_a$DATE, meteo_a$RAIN, type='l', 
             xlab='Date', ylab='Rain (mm)', main='Daily Precipitation')
        abline(v=aggr_met_a$CHANGE_DATE, lwd=.5, col='gray48')

        # Main Title
        mtext(paste(desc, '-', year), side=3, outer=TRUE, line=-2, cex=1.2)

        dev.off()
      }
    }
  }
}
