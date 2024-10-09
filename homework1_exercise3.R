# 1.3 CAPM
#----------------------
library(lubridate)
library(readxl)
library(xts)

data_capm <- read_xlsx("./Assignment1/Data efficient_frontier_CAPM.xlsx")
dates <- ymd(data_capm$Date)
data_capm$Date <- dates
data_capm_xts <- as.xts(data_capm[,2:183])

exercise_1.3 <- function(data,time_splice){
  
  # subset the correct timeframe
  data_a <- data[time_splice]
  
  # create the plot and save it as plot1
  png("plot1.png",1000,700)
  plot.default((data_a$CAT-data_a$`One-month Treasury Yield`)~(data_a$SP500-data_a$`One-month Treasury Yield`),
               xlab = "S&P Excess return", ylab="CAT Excess return")
  abline(0,0)
  abline(v = 0)
  dev.off()
  
  # beta of the stock
  beta_cat <- lm((data_a$CAT-data_a$`One-month Treasury Yield`)~(data_a$SP500-data_a$`One-month Treasury Yield`))
  
  # beta of CAT is 0.74 (beta_cat$coefficients[2])
  
  conf_intervals <- confint(beta_cat, level = 0.95)
  
  # confinterval of slope is ( 0.2854 , 1.1951 ), and for intercept (-0.0239 , 0.0232)
  
  # the systematic risk squared is the variance of the market times the beta squared
  # https://www.fightfinance.com/resources/pricing/7c_total_systematic_and_idiosyncratic_variance.pdf
  # idisyncratic is asset variance minus beta squared times market variance
  
  sys_risk <- sqrt(var(data_a$SP500)*(beta_cat$coefficients[2])^2)
  idio_risk <- sqrt(var(data_a$CAT)-(beta_cat$coefficients[2])^2*var(data_a$SP500))
  
  #systematic risk is 0.0383 and idiosyncratic risk is 0.0896
  
  # to check if total vola and our risks match up:
  if((sd(data_a$CAT) == sqrt((beta_cat$coefficients[2])^2*var(data_a$SP500)+idio_risk^2)) != T){
    warning("Total vola not equal to sys + idio vola")
    print(sd(data_a$CAT))
    print(sqrt((beta_cat$coefficients[2])^2*var(data_a$SP500)+idio_risk^2))
  }
  # true so all risk is within those two
  
  # all stocks 
  #--------------------------------------------------
  
  betas <- c()
  
  for( i in 1:(ncol(data_a)-2)){
    betas[i] <- lm((data_a[,i+2]-data_a$`One-month Treasury Yield`)~
                     (data_a$SP500-data_a$`One-month Treasury Yield`))$coefficients[2]
  }
  
  png("plot2.png",1000,700)
  hist(betas,breaks = 10)
  dev.off()
  
  mean_betas <- mean(betas)
  var_betas <- var(betas)
  
  below_zero_betas <- any(betas < 0) # True
  
  # beta of S&P is 1
  
  avg_excess_returns_actual <- c()
  
  for(i in 1:(ncol(data_a)-2)){
    avg_excess_returns_actual[i] <- mean(data_a[,i+2]-data_a$`One-month Treasury Yield`)
  }
  
  capm_excess_returns <- c()
  for( i in 1:(ncol(data_a)-2)){
    capm_excess_returns[i] <- mean(data_a$`One-month Treasury Yield`
                                   +betas[i]*(data_a$SP500-data_a$`One-month Treasury Yield`))
  }
  
  # According to the CAPM these two should be the same
  
  sp_excess <- mean(data_a$SP500-data_a$`One-month Treasury Yield`)
  sp_excess_capm <- mean(data_a$`One-month Treasury Yield`+1*(data_a$SP500-data_a$`One-month Treasury Yield`))
  # treasury bill doesn't have any excess returns in reality
  # also the beta is 0 so there are no excess returns according to the CAPM either
  
  png("plot3.png",1000,700)
  plot(betas,avg_excess_returns_actual)
  dev.off()
  # yes generally higher betas are associated with higher excess returns
  
  beta_reg <- lm(avg_excess_returns_actual~betas)
  summary(beta_reg)
  r2_beta_reg <- summary(beta_reg)$r.squared
  # R^2 is 0.1588, so only 15.8% of the excess variance can be explained by the beta
  
  return(list(data = data_a,
              timeframe = time_splice,
              beta_cat = beta_cat$coefficients[1:2],
              beta_conf_int = conf_intervals,
              sys_risk= sys_risk,
              idio_risk=idio_risk,
              below_zero_betas = below_zero_betas,
              betas = betas,
              avg_betas = mean_betas,
              variance_betas = var_betas,
              avg_real_excess_ret=avg_excess_returns_actual,
              avg_capm_excess_ret = capm_excess_returns,
              sp_real_excess_ret=sp_excess,
              sp_capm_excess_ret=sp_excess_capm,
              r2_beta_regression=r2_beta_reg
              ))
  
}

orig_timeframe <- exercise_1.3(data_capm_xts,"1996-12/2001-11")
second_timeframe <- exercise_1.3(data_capm_xts,"2002/2006")
third_timeframe <- exercise_1.3(data_capm_xts,"2007/2011")
fourth_timeframe <- exercise_1.3(data_capm_xts,"2011/2016")


# for frequency table
factor_betas <- cut(orig_timeframe$betas,breaks=seq(-2.5,2.5,0.25))
table(factor_betas)

