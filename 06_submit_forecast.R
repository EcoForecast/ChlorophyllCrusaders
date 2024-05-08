devtools::install_github('eco4cast/neon4cast')
library(ecoforecastR)

# Pull in forecast
source("06_forecast.R")

# Reformat the forecast so it fits the standard
start_date <- Sys.Date() + 1
end_date <- Sys.Date() + 30
sub_fcast <- data.frame()
for(i in 1:length(sites)){
  #fcast <- as.data.frame(forecasts)
  fcast <- as.data.frame(t(forecasts[[sites[i]]]))
  colnames(fcast) <- seq(1:1000)
  fcast$datetime <- seq(start_date, end_date, by = "+1 day")
  fcast$reference_datetime <- seq(start_date-1, end_date-1, by = "+1 day")
  n_fcast <- pivot_longer(fcast, cols=seq(1:1000))
  colnames(n_fcast) <- c("datetime", "reference_datetime", "parameter", "prediction")
  n_fcast$model_id <- "ChlorophyllCrusaders" 
  n_fcast$site_id <- sites[i] # change if we add more sites
  n_fcast$variable <- "gcc_90" 
  n_fcast$family <- "ensemble" # there are 1000
  n_fcast$duration <- "P1D" # daily forecasts
  n_fcast$project_id <- "neon4cast"
  if (dim(sub_fcast)[1] == 0 & dim(sub_fcast)[2] == 0){ 
    sub_fcast = n_fcast 
  } else {
    sub_fcast <- rbind(sub_fcast, n_fcast)
  }
}

sub_fcast <- sub_fcast |> relocate(project_id, model_id, datetime, reference_datetime, duration, site_id, family, parameter, variable, prediction)
year <- year(Sys.Date())
month <- month(Sys.Date())
day <- day(Sys.Date())
forecast_file <- paste0("phenology-", year, "-", month, "-", day, "-ChlorophyllCrusaders.csv.gz")
write.csv(n_fcast, forecast_file)
neon4cast::forecast_output_validator(forecast_file) # validated!

# Plot the ensembles; change site name for different ensembles
#pheno_means <- sub_fcast[sub_fcast$site_id == "RMNP",] %>% 
#  group_by(datetime) |> 
#  summarize(pred_mean = mean(prediction),.groups = "drop") |> 
#  select(datetime, pred_mean)

#plot(as.Date(pheno_means$datetime), pheno_means$pred_mean, type="l", ylim=c(0.25,0.50),
#     ylab="Mean ensemble GCC", xlab="Time", main="Prediction Ensemble means")
#ecoforecastR::ciEnvelope(pheno_means$datetime,N.IPDE.ci[1,],N.IPDE.ci[3,],col="darkgoldenrod1")
#lines(as.Date(pheno_means$datetime), pheno_means$pred_mean)

# Plot 10 random ensembles
#samp <- sample.int(nrow(forecasts$HARV), 10, replace=TRUE)
#plot(as.Date(pheno_means$datetime), forecasts$HARV[1,], type="b", col=1, ylim=c(0.25, 0.50),
#     main="10 random ensemble forecasts", ylab="GCC_90", xlab="Time")
#for(s in 2:10) {
#  lines(as.Date(pheno_means$datetime), forecasts$HARV[s,], type="b", col=s)
#}


# Metadata part

team_info <- list(team_name = "ChlorophyllCrusaders",
                  team_list = list(list(individualName = list(givenName = "Alice", 
                                                              surName = "Ni"),
                                        organizationName = "Boston University",
                                        electronicMailAddress = "aliceni@bu.edu"),
                                   list(individualName = list(givenName = "Andy", 
                                                              surName = "Wang"),
                                        organizationName = "Boston University",
                                        electronicMailAddress = "andyhanw@bu.edu"),
                                   list(individualName = list(givenName = "Nellie", 
                                                              surName = "Maloney"),
                                        organizationName = "Boston University",
                                        electronicMailAddress = "ekmalone@bu.edu"),
                                   list(individualName = list(givenName = "Sam", 
                                                              surName = "Wu"),
                                        organizationName = "Boston University",
                                        electronicMailAddress = "samwu@bu.edu"),
                                   list(individualName = list(givenName = "Zoie", 
                                                              surName = "Lee"),
                                        organizationName = "Boston University",
                                        electronicMailAddress = "zoiealee@bu.edu")
                  )
)


model_metadata = list(
  forecast = list(
    model_description = list(
      forecast_model_id =  "ChlorophyllCrusaders", 
      name = "Dynamic linear model green chromatic coordinate",
      type = "process",
      repository = "https://github.com/EcoForecast/ChlorophyllCrusaders/"
    ),
    initial_conditions = list(
      status = "absent"
    ),
    drivers = list(
      status = "absent"
      #complexity = 1, 
      #propagation = list(
      #  type = "ensemble",
      #  size = 1000)
    ),
    parameters = list(
      status = "data_driven",
      complexity = 2 # slope and intercept (per site)
    ),
    random_effects = list(
      status = "absent"
    ),
    process_error = list(
      status = "absent"
    ),
    obs_error = list(
      status = "absent"
    )
  )
)

# metadata_file <- neon4cast::generate_metadata(forecast_file, team_info$team_list, model_metadata)


# Submit forecast!!
neon4cast::submit(forecast_file = forecast_file, ask = FALSE)
