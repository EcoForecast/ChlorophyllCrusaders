library(dplyr)
library(ecoforecastR)

# Grab scores from EFI website
all_results <- arrow::open_dataset("s3://anonymous@bio230014-bucket01/challenges/scores/parquet/project_id=neon4cast/duration=P1D/variable=gcc_90/model_id=ChlorophyllCrusaders?endpoint_override=sdsc.osn.xsede.org")
df <- all_results |> 
  dplyr::collect()

plot(df$observation, type="b", ylim=c(0.27, 0.48), main="Scores for HARV",
     ylab="GCC 90", xlab="Days of prediction", lwd=2)
lines(df$quantile02.5, col="green", type="b")
lines(df$quantile97.5, col="red", type="b")
lines(df$mean,  col="blue", type="b")
legend("top",legend=c("Upper 97.5","Lower 2.5","Mean","Observed"),
       col=c("red","green","blue","black"),lty=1,lwd=5)
