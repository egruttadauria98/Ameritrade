myData <- read.csv("/Users/eliogruttadauria/Desktop/adjusted.csv", header = TRUE, sep = ";", dec = ",")
rf_rate <- 0.06

ttest <- function(reg, coefnum, val, pval=0.05){
  co <- coef(summary(reg))
  tstat <- (co[coefnum,1]-val)/co[coefnum,2]
  prob <- (2 * pt(abs(tstat), reg$df.residual, lower.tail = FALSE))
  if (prob < pval) {
    print("REJECT H0: beta different from 1")
  } else {
    print("ACCEPT H0: not enough stat evidence")
  }
}

beta_regress <- function(firm,benchmark='VW',risk_free_r=0.06, plot = FALSE){
  Ra <- na.omit(myData[c('Date', firm)])
  Ra[firm] <- Ra[firm] - risk_free_r
  
  Rb <- na.omit(myData[c('Date', benchmark)])
  Rb[benchmark] <- Rb[benchmark] - risk_free_r
  
  frame <- merge(Ra, Rb, by='Date')
  frame['Date'] <- NULL
  
  names(frame) <- c("Ra", "Rb")

  ## Regression
  mod1 <- lm(data = frame, Ra ~ Rb)
  print(summary(mod1))
  coef <- coef(mod1)
  eq <- paste0(firm, ": Beta = ", round(coef[2],3), ", Intercept = ", round(coef[1],3))
  print(firm)
  print(coef)
  ttest(mod1, 2, 1)
  print(summary(mod1)$r.squared)
  
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
  beta_regress(firm)
}

