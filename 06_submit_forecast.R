devtools::install_github('eco4cast/neon4cast')
library(ecoforecastR)

# Pull in forecast
source("06_forecast.R")

# Reformate the forecast so it fits the standard
fcast <- as.data.frame(pheno_forecast)
fcast <- as.data.frame(t(pheno_forecast))
colnames(fcast) <- seq(1:1000)
start_date <- Sys.Date() + 1
end_date <- Sys.Date() + 30
fcast$datetime <- seq(start_date, end_date, by = "+1 day")
fcast$reference_datetime <- seq(start_date-1, end_date-1, by = "+1 day")
n_fcast <- pivot_longer(fcast, cols=seq(1:1000))
colnames(n_fcast) <- c("datetime", "reference_datetime", "parameter", "prediction")
n_fcast$model_id <- "chlorocrusaders" # perhaps change?
n_fcast$site_id <- "HARV" # change if we add more sites
n_fcast$variable <- "gcc" # I think this is the right name
n_fcast$family <- "ensemble" # there are 1000
n_fcast$duration <- "P1D" # daily forecasts
n_fcast$project_id <- "neon4cast"

n_fcast <- n_fcast |> relocate(project_id, model_id, datetime, reference_datetime, duration, site_id, family, parameter, variable, prediction)
year <- year(Sys.Date())
month <- month(Sys.Date())
day <- day(Sys.Date())
forecast_file <- paste0("phenology-", year, "-", month, "-", day, "-chlorocrusaders.csv.gz")
write.csv(n_fcast, forecast_file)
neon4cast::forecast_output_validator(forecast_file) # validated!

# Metadata part- A little unsure what to do here

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
      forecast_model_id =  "chlorocrusaders", 
      name = "Dynamic linear model green chromatic coordinate",
      type = "process",
      repository = "https://github.com/EcoForecast/ChlorophyllCrusaders/"
    ),
    initial_conditions = list(
      status = "abset"
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