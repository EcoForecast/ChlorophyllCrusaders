source("05_historic_forecast.R")

# Function for grabbing future forecast data based on the current date

download_future_met <- function() {
  weather_stage2 <- neon4cast::noaa_stage2(start_date = as.character(Sys.Date()-2))
  ds1 <- weather_stage2 |> 
    dplyr::filter(site_id %in% sites) |>
    dplyr::collect()
  
  TMP = ds1[ds1$variable == "air_temperature",]
  
  TMP <- TMP %>% 
    mutate(datetime = lubridate::as_date(datetime)) %>%
    group_by(datetime, site_id, parameter, variable) |> 
    summarize(prediction = mean(prediction),.groups = "drop") |> 
    select(datetime, site_id, variable, prediction, parameter)
  
  tmps <- as.data.frame(TMP) # save temperature data as dataframe
  return(tmps)
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


# Making forecast
forecast_sites <- function(f_temps, models, sites){
  forecasts <- list()
  for (i in 1:length(sites)) {
    temps_ensemble <- as.data.frame(pivot_wider(f_temps[f_temps$site_id == sites[1],], 
                                                names_from = parameter, 
                                                values_from = prediction)[,4:34] - 273.15)
    temps_rot <- t(temps_ensemble)[,1:30]
    temps <- matrix(apply(temps_rot,2,mean), 1, 30) 
    gcc.out <- models[[sites[1]]] # going through each sites models for the params
    params <- as.matrix(gcc.out$params) # get model parameters
    param.mean <- apply(params, 2, mean)
    predicts <- as.matrix(gcc.out$predict) # get model predictions
    data <- gcc.out$data
    
    ci <- apply(predicts, 2, quantile, c(0.025,0.5,0.975))
    prow <- sample.int(nrow(params), Nmc, replace=TRUE)
    drow = sample.int(nrow(temps_rot), Nmc, replace=TRUE)
    Qmc <- 1 / sqrt(params[prow,"tau_add"])  ## convert from precision to standard deviation
    
    IC <- predicts
    pheno_forecast <- forecast(IC[prow, ncol(predicts)],
                               temp = temps_rot[drow,],
                               betaTemp = params[prow, "betaTemperature"],
                               betaX = params[prow, "betaX"],
                               betaI = params[prow, "betaIntercept"],
                               Q = Qmc,
                               n = Nmc)
    forecasts[[sites[i]]] <- pheno_forecast 
  }
  return(forecasts)
}

Nmc = 1000 # number of Monte Carlo draws
NT = 30 # number of time steps into the future

future_temps <- download_future_met()
forecasts <- forecast_sites(future_temps, out_sites, sites)





