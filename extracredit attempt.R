# Extra Credit Attempt 

*Write a FUNCTION that takes as its arguments a dataframe, “d”, a linear model, “m” (as a character string, e.g., “logHR~logBM”), a user-defined confidence interval level, “conf.level” (with default = 0.95), and a number of bootstrap replicates, “n” (with default = 1000). Your function should return a dataframe that includes: beta coefficient names; beta coefficients, standard errors, and upper and lower CI limits for the linear model based on your entire dataset; and mean beta coefficient estimates, SEs, and CI limits for those coefficients based on your bootstrap.*
  
  ```{r - function}
bootsfurdayz <- function(d, m, conf.level = 0.95, n = 1000)
  
{ 
  #BOOTSTRAP
  
  #Containers for the coefficients
  sample_coef_intercept <- NULL
  sample_coef_slope <- NULL
  
  for (i in 1:1000) {
    #Creating a resampled dataset from the sample data
    d_boots = d[sample(1:nrow(d), nrow(d), replace = TRUE), ] #WITH replacement
    
    #Running the regression on these data
    d_boots <- lm(log(HomeRange_km2) ~ log(Body_mass_female_mean), data = d_boots) 
    
    #Saving the coefficients
    sample_coef_intercept <-
      c(sample_coef_intercept, d_boots$coefficients[1])
    
    sample_coef_slope <-
      c(sample_coef_slope, d_boots$coefficients[2])
  }
  
  #BOOT BETAS
  mean_slope <- mean(sample_coef_slope)
  mean_intercept <- mean(sample_coef_intercept)
  boots_betas <- list(Slope = as.numeric(mean_slope),Intercept = as.numeric(mean_intercept))     
  
  
  #BOOT SE
  se_slope <- se(sample_coef_slope)
  se_intercept <- se(sample_coef_intercept)
  boots_se <- list(Slope = as.numeric(se_slope),Intercept = as.numeric(se_intercept))        
  
  #BOOT CI
  #Slope
  slope.lower <- mean_slope - qnorm(0.95) * se_slope
  slope.upper <- mean_slope + qnorm(0.95) * se_slope
  ci_slope_boots <- list(Lower = as.numeric(slope.lower), Upper = as.numeric(slope.upper))
  
  #Intercept
  intercept.lower <- mean_intercept - qnorm(1 - 0.05/2) * se_intercept
  intercept.upper <- mean_intercept + qnorm(1 - 0.05/2) * se_intercept
  ci_intercept_boots <- list(Lower = as.numeric(intercept.lower), Upper = as.numeric(intercept.upper))
  
  ci_boots <- rbind(ci_slope_boots,ci_intercept_boots)
  ci_boots        
  
  #MODEL BETAS
  
  t <- coef(summary(m))
  t <- data.frame(unlist(t))
  colnames(t) <- c("Est", "SE", "t", "p")
  
  beta0 <- t$Est[1]
  beta1 <- t$Est[2]
  
  model_betas <- list(Model_Intercept = as.numeric(beta0), Model_Slope = as.numeric(beta1))
  
  #MODEL SE
  model_se <- coef(summary(m))[, "Std. Error"]
  
  #MODEL CI
  ci_model <- confint(m, level = 0.95)
  colnames(ci_model) <- c("Lower","Upper")
  
  
  #creating dataframe
  Slope <- c(mean_slope, beta1)
  Intercept <- c(mean_intercept, beta0)
  Standard.Error <- c(model_se, boots_se)
  Slope.Lower.CI <- c(ci_model$Lower[2], slope.lower)
  Slope.Upper.CI <- c(ci_model$Upper[2], slope.upper)
  Intercept.Lower.CI <- c(ci_model$Lower[1], intercept.lower)
  Intercept.Upper.CI <- c(ci_model$Upper[1], intercept.upper)
  
  boots.df <- data.frame(Slope, Intercept, Standard.Error, Slope.Lower.CI, Slope.Upper.CI,
                         Intercept.Lower.CI, Intercept.Upper.CI)
  
  return(boots.df)
  
}
```


