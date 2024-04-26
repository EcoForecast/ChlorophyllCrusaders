# Pull in forecast run and historic forecast for plotting
source("06_forecast.R")

# Set up plot function
s <- 1             ## Focal site for forward simulation
ylim = c(0.3,0.5)  ## set Y range on plot
N.cols <- c("black","red","green","blue","orange") ## set colors
trans <- 0.8       ## set transparancy
time = 1:(NT*2)    ## total time
time1 = 1:NT       ## calibration period
time2 = time1+NT   ## forecast period

plot.run <- function(){
  plot(time,time,type='n',ylim = ylim, ylab="gcc", xlab="Time steps", main="Uncertainty Partioning of GCC at HARV")
  ecoforecastR::ciEnvelope(time1, tail(ci[1,], n=30), tail(ci[3,], n=30), col=col.alpha("lightBlue",0.6))
  points(time1, tail(as.numeric(data$OBS), n = 30))
  lines(time1, tail(ci[2,], n=30), col="blue")
}


# Determinstic run
N.det_pheno <- forecast(IC = mean(IC[,ncol(predicts)]),
                        temp = temps,
                        betaTemp = param.mean["betaTemperature"],
                        betaX = param.mean["betaX"],
                        betaI = param.mean["betaIntercept"],
                        Q = 0,
                        n = 1)

# Initial condition uncertainty
N.I <- forecast(IC = IC[prow, 30],
                temp = temps,
                betaTemp = param.mean["betaTemperature"],
                betaX = param.mean["betaX"],
                betaI = param.mean["betaIntercept"],
                Q = 0,
                n = Nmc)
N.I.ci = apply(N.I,2,quantile,c(0.025,0.5,0.975))

# Parameter uncertainty
N.IP <- forecast(IC = IC[prow, 30],
                 temp = temps,
                 betaTemp = params[prow, "betaTemperature"],
                 betaX = params[prow, "betaX"],
                 betaI = params[prow, "betaIntercept"],
                 Q = 0,
                 n = Nmc)
N.IP.ci = apply(N.IP,2,quantile,c(0.025,0.5,0.975))

# Driver uncertainty
N.IPD <- forecast(IC = IC[prow, 30],
                  temp = temps_rot[drow,],
                  betaTemp = params[prow, "betaTemperature"],
                  betaX = params[prow, "betaX"],
                  betaI = params[prow, "betaIntercept"],
                  Q = 0,
                  n = Nmc)
N.IPD.ci = apply(N.IPD,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)

# Process uncertainty
N.IPDE <- pheno_forecast#forecast(IC=IC[prow, 30],
          #         temp = temps_rot[drow,],
          #         betaTemp = params[prow, "betaTemperature"],
          #         betaX = params[prow, "betaX"],
          #         betaI = params[prow, "betaIntercept"],
          #         Q = Qmc,
          #         n = Nmc)
N.IPDE.ci = apply(N.IPDE,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)

## Plot run with all the uncertainty partitioned- see corresponding RMD file for step-by-step
plot.run()
ecoforecastR::ciEnvelope(time2,N.IPDE.ci[1,],N.IPDE.ci[3,],col="darkgoldenrod1")
ecoforecastR::ciEnvelope(time2,N.IPD.ci[1,],N.IPD.ci[3,],col="orchid")
ecoforecastR::ciEnvelope(time2,N.IP.ci[1,],N.IP.ci[3,],col="pink")
ecoforecastR::ciEnvelope(time2, N.I.ci[1,], N.I.ci[3,], col="lightgreen")
legend("topleft", legend = c("Initial Condition", "Parameter", "Driver", "Process"), 
       col = c("lightgreen","pink","orchid","darkgoldenrod1"), lty=1, lwd=5)
lines(time2,N.det_pheno,lwd=1)

# Uncertainty Analysis

### calculation of variances
varI     <- apply(N.I,2,var)
varIP    <- apply(N.IP,2,var)
varIPD   <- apply(N.IPD,2,var)
varIPDE  <- apply(N.IPDE,2,var)
#varIPDEA <- apply(N.IPDEA,2,var)
varMat   <- rbind(varI,varIP,varIPD,varIPDE)#,varIPDEA)

## out-of-sample stacked area plot
V.pred.rel <- apply(varMat,2,function(x) {x/max(x)})
plot(time2,V.pred.rel[1,],ylim=c(0,1),type='n',main="Relative Variance: Out-of-Sample",ylab="Proportion of Variance",xlab="time")
ciEnvelope(time2,rep(0,ncol(V.pred.rel)),V.pred.rel[1,],col="lightgreen")
ciEnvelope(time2,V.pred.rel[1,],V.pred.rel[2,],col="pink")
ciEnvelope(time2,V.pred.rel[2,],V.pred.rel[3,],col="orchid")
ciEnvelope(time2,V.pred.rel[3,],V.pred.rel[4,],col="darkgoldenrod1")
#ciEnvelope(time2,V.pred.rel[4,],V.pred.rel[5,],col=N.cols[5])
legend("topleft",legend=c("Process","Driver","Parameter","InitCond"),
       col=c("lightgreen","pink","orchid","darkgoldenrod1"),
       lty=1,lwd=5)

## in-sample stacked area plot
V.pred.rel.in <- apply(varMat[-5,],2,function(x) {x/max(x)})
plot(time2,V.pred.rel.in[1,],ylim=c(0,1),type='n',main="Relative Variance: In-Sample",ylab="Proportion of Variance",xlab="time")
ciEnvelope(time2,rep(0,ncol(V.pred.rel.in)),V.pred.rel.in[1,],col="lightgreen")
ciEnvelope(time2,V.pred.rel.in[1,],V.pred.rel.in[2,],col="pink")
ciEnvelope(time2,V.pred.rel.in[2,],V.pred.rel.in[3,],col="orchid")
ciEnvelope(time2,V.pred.rel.in[3,],V.pred.rel.in[4,],col="darkgoldenrod1")
legend("topleft",legend=c("Process","Driver","Parameter","InitCond"),
       col=c("lightgreen","pink","orchid","darkgoldenrod1"),lty=1,lwd=5)

