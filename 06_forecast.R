library(dplyr)
library(lubridate)
library(tidyr)
#source("05_historic_forecast.R")

# Function for grabbing future forecast data based on the current date

download_future_met <- function() {
  weather_stage2 <- neon4cast::noaa_stage2(start_date = as.character(Sys.Date() - lubridate::days(2)))
  ds1 <- weather_stage2 |> 
    dplyr::filter(site_id == "HARV") |>
    dplyr::collect()
  
  TMP = ds1[ds1$variable == "air_temperature",]
  
  TMP <- TMP %>% 
    mutate(datetime = lubridate::as_date(datetime)) %>%
    group_by(datetime, site_id, parameter, variable) |> 
    summarize(prediction = mean(prediction),.groups = "drop") |> 
    select(datetime, site_id, variable, prediction, parameter)
  
  tmps <- as.data.frame(TMP) # save temperature data as dataframe
  
  # Get maximum temperature within each ensemble per day: 31 ensembles x 36 days
  #temp_max <- tmps %>% 
  #  group_by(datetime, parameter) %>%
  #  summarize(temp.max = max(prediction) - 273.15) # convert to celsius
  
  # Get mean of maximum temperatures across ensembles
  #temp_max <- temp_max %>%
  #  group_by(datetime) %>% 
  #  summarize(temp.mean = mean(temp.max))
  
  #temps <- as.data.frame(temp_max)
  
  # Define these outside the function to have them accessible to be used in forecast
  # Get dataframe: columns = ensemble number, rows = prediction by time step
  temps_ensemble <- as.data.frame(pivot_wider(tmps, names_from = parameter, values_from = prediction)[,4:34] - 273.15)
  # Get one maximum value for daily prediction
  #temps.max <- matrix(apply(temps_ensemble,1,max),1, 36)
  
  return(temps_ensemble)
}


# Forecasting Function

##` @param IC    Initial Conditions
##` @param temp  Temperature forecast
##` @param beta  Slope of temperature effect on gcc
##` @param alpha Site random effect
##` @param Q     Process error (default = 0 for deterministic runs)
##` @param n     Size of Monte Carlo ensemble
forecast <- function(IC, temp, betaTemp, betaX, betaI, Q, n){ 
  N <- matrix(NA, n, NT)
  Nprev <- IC       
  for(t in 1:NT){ # go through future time steps
    gcc_predict <- Nprev + betaI + (betaX*Nprev) + (betaTemp*temp[,t]) # linear model prediction
    N[,t] <- rnorm(n, gcc_predict, Q) # store prediction
    Nprev <- N[,t] # update IC
  }
  return(N)
}


