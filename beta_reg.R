myData <- read.csv("/Users/eliogruttadauria/Desktop/adjusted.csv", header = TRUE, sep = ";", dec = ",")
rf_rate <- 0.06
p_val <- 0.05

ttest <- function(reg, coefnum, val){
  co <- coef(summary(reg))
  tstat <- (co[coefnum,1]-val)/co[coefnum,2]
  prob <- (2 * pt(abs(tstat), reg$df.residual, lower.tail = FALSE))
  return(prob)
}
  
beta_regress <- function(firm, benchmark='VW', risk_free_r=0.06, plot = FALSE){
  
  Ra <- na.omit(myData[c('Date', firm)])
  Ra[firm] <- Ra[firm] - risk_free_r
  
  Rb <- na.omit(myData[c('Date', benchmark)])
  Rb[benchmark] <- Rb[benchmark] - risk_free_r
  
  frame <- merge(Ra, Rb, by='Date')
  frame['Date'] <- NULL
  
  names(frame) <- c("Ra", "Rb")

  ## Regression
  mod1 <- lm(data = frame, Ra ~ Rb)
  coef <- coef(mod1)
  eq <- paste0(firm, ": Beta = ", round(coef[2],3), ", Intercept = ", round(coef[1],3))
  
  print("-------------------------------------------------------------")
  print(firm)
  
  beta <- coef[2]
  cat("Beta is: ", beta, "\n")
  
  
  t_test_0 <- coef(summary(mod1))[2,4]
  cat("The probability of Beta being 0 is: ", t_test_0, "\n")
  
  if (t_test_0 < p_val) {
    cat("--> H0 is rejected with p:", p_val, "\n")
  } else {
    cat("--> H0 cannot be rejected with p:", p_val, "\n")
  }
  
  t_test_1 <- ttest(mod1, 2, 1)
  cat("The probability of Beta being 1 is: ", t_test_1, "\n")
  
  if (t_test_1 < p_val) {
    cat("--> H0 is rejected with p:", p_val, "\n")
  } else {
    cat("--> H0 cannot be rejected with p:", p_val, "\n")
  }
  
  r_squared <-summary(mod1)$r.squared
  cat("R squared: ", r_squared, "\n")
  
  ##  Plot
  if (plot) {
    ggplot(data = frame)+
      aes(x =  Rb, y  = Ra)+
      geom_point()+
      geom_abline()+
      labs(x = paste0(benchmark, " Returns"),
           y  = paste0(ticker, " Returns"),
           title = eq,
           caption  = paste0("Risk Free Rate:", risk_free_r))+
      theme_minimal()
  }
}

firms <- colnames(myData)[c(-1, -17, -18)]

for (firm in firms) {
  result <- beta_regress(firm)
}

