###Function

## Required packages
library(ggplot2)
library(quantmod)
library(xts)

beta_regression <- function(ticker,benchmark,risk_free_r,start_date,end_date=NULL){

### Frame
  v_asset <- to.monthly(getSymbols(ticker, from= start_date, auto.assign = F, src = "yahoo"))
  v_benchmark <- to.monthly(getSymbols(benchmark, from = start_date, auto.assign = F, src = "yahoo"))
  v_risk_free_r <- to.monthly(getSymbols(risk_free_r, from = start_date, auto.assign = F, src = "FRED"))
  v_asset <- v_asset[,4]
  v_benchmark <- v_benchmark[,4]
  v_risk_free_r <- v_risk_free_r[,4]
  v_risk_free_r <- v_risk_free_r/100

  Ra <- (na.omit((lag(v_asset[,1])-v_asset[,1])/lag(v_asset[,1]))) - v_risk_free_r
  Rb <- (na.omit((lag(v_benchmark[,1])-v_benchmark[,1])/lag(v_benchmark[,1]))) -v_risk_free_r
  frame <- cbind(Ra,Rb)
  names(frame) <- c("Ra", "Rb")

  ## Regression
  mod1 <- lm(data = frame, Ra ~ Rb)
  summary(mod1)
  coef <- coef(mod1)
  eq <- paste0(ticker, ": Beta = ", round(coef[2],3), ", Intercept = ", round(coef[1],3))

  ##  Plot
  ggplot(data = frame)+
    aes(x =  Rb, y  = Ra)+
      geom_point()+
      geom_abline()+
    labs(x = paste0(benchmark, " Returns"),
         y  = paste0(ticker, " Returns"),
         title = eq,
        caption  = paste0("  Monthly from ", start_date, ", Risk Free Rate:", risk_free_r))+
    theme_minimal()
}

beta_regression("AMTD", "^GSPC", "DGS6MO", "1992-02-06")
