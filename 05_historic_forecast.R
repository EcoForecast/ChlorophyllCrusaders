library(dplyr)
library(lubridate)
library(tidyr)
library(ecoforecastR)

sites = c("HARV", "RMNP", "DSNY", "SRER", "HEAL")
# deciduous broadleaf, evergreen needleleaf, grassland, shrubland, tundra 

# Function for downloading historic data

download_historic_data <- function(sites){
  URL = "https://data.ecoforecast.org/neon4cast-targets/phenology/phenology-targets.csv.gz"
  
  # Get gcc data
  data = readr::read_csv(URL, col_names = c("Date", "Siteid", "Variable", "Observation"))
  data.s = na.omit(data[data$Siteid %in% sites,]) # filter by sites
  gcc_90 = data.s[data.s$Variable == "gcc_90",]
  
  # Get historic temperature data
  weather_stage3_historic <- neon4cast::noaa_stage3()
  ds_historic <- weather_stage3_historic |> 
    dplyr::filter(site_id %in% sites) |>
    dplyr::collect()
  
  start_date <- Sys.Date() - lubridate::days(1096) # hindcasting 3 years
  end_date <- Sys.Date() - 1
  
  # currently not using precip, but it works 
  TMP_historic = ds_historic[ds_historic$variable == c("air_temperature"),]#, "precipitation_flux"),]
  
  data_historic <- TMP_historic %>% 
    mutate(datetime = lubridate::as_date(datetime)) %>%
    filter(datetime >= ymd(start_date) & datetime <= ymd(end_date)) %>% 
    group_by(datetime, site_id, parameter, variable) |> 
    summarize(prediction = mean(prediction),.groups = "drop") |> 
    select(datetime, site_id, variable, prediction, parameter)
  
  data_historic <- as.data.frame(data_historic) # save temperature data as dataframe
  data_historic$date <- date(data_historic$datetime)
  data_historic$date <- as.character(data_historic$date)
  
  # Get maximum temperature within each ensemble per day: 31 ensembles x 36 days
  temp_max_h <- data_historic %>% 
    group_by(date, variable, site_id) %>%
    summarize(prediction = max(prediction)) 

  # For multiple covariates
  vars <- pivot_wider(temp_max_h, names_from = variable, values_from = prediction)
  data1 <- gcc_90[which(gcc_90$Date == temp_max_h$date[1]):nrow(gcc_90), ]
  covariates <- vars[vars$date %in% data1$Date, ]
  covariates <- covariates[order(covariates$site_id),]
  covariates <- covariates[covariates$date %in% data1$Date, ]
  colnames(covariates) <- c("Date", "Siteid", "air_temperature")
  cov <- covariates
  
  data1 <- right_join(data1, select(cov, c("Date" = "Date","Siteid" = "Siteid"))) # subset by available temperature data
  
  # Add temperature as column in data dataframe
  data1$Temperature <- covariates$air_temperature - 273.15 # convert to celsius
  #data1$PcpFlux <- covariates$precipitation_flux
  data1$Observation <- as.numeric(data1$Observation)
  data1 <- drop_na(data1)
  
  return(data1)
}

# Function for running a DLM hindcast

historic_forecast <- function(data, sites) {
  models <- list()
  for(i in 1:length(sites)) { # run DLM per site
    dat <- data[data$Siteid == sites[i],]
    data2 <- list(x_ic = mean(dat$Observation, na.rm = T),
                  tau_ic = 1/sd(dat$Observation, na.rm = T),
                  a_obs = 1,
                  r_obs = 1,
                  a_add = 1,
                  r_add = 1,
                  n = length(nrow(dat)),
                  Observation = dat$Observation,
                  Temperature = dat$Temperature)
    
    # Run the DLM
    gcc.out <- ecoforecastR::fit_dlm(model=list(obs="Observation", fixed="~ 1 + X + Temperature"), data2)
    models[[sites[i]]] <- gcc.out
  }
  return(models)
}

# Conducting a historic forecast
h_data <- download_historic_data(sites)
out_sites <- historic_forecast(h_data, sites)

# Print the model
strsplit(out_sites$HARV$model,"\n",fixed = TRUE)[[1]]

# Plot the historic forecast: plotting rocky mountains site, change site name to get other sites
site_name = "HEAL"
plot_out <- as.matrix(out_sites[[site_name]]$predict) # get model predictions
ci_out <- apply(plot_out, 2, quantile, c(0.025,0.5,0.975)) # get model confidence intervals
plot(1:nrow(h_data[h_data$Siteid == site_name,]), ci_out[2,], type='n', ylab="GCC_90", xlab="Time") # create empty plot with proper axes
ecoforecastR::ciEnvelope(1:nrow(h_data[h_data$Siteid == site_name,]),ci_out[1,],ci_out[3,],col=ecoforecastR::col.alpha("lightBlue",0.75)) # plot model CI
points(1:nrow(h_data[h_data$Siteid == site_name,]), h_data[h_data$Siteid == site_name,]$Observation, pch="+",cex=0.5) # plot gcc_90 original points
points(1:nrow(h_data[h_data$Siteid == site_name,]), ci_out[2,], pch=19, cex=0.2) # plot model 50% CI

