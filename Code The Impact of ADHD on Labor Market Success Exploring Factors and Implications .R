rm(list=ls())
gc()

library(dplyr)
library(foreign)
library(ggplot2)
library(sandwich)
library(stargazer)
library(WDI)
library(ipumsr)
library(tidyverse)
library(readstata13)
library(sandwich)
library(stargazer)
library(WDI)
library(doBy)
library(WDI)
library(XML)
library(lmtest)
library(plm)
library(nlme)
library(AER)

setwd("C:/Users/caget/Downloads/RStudio")
mydata <- read.csv(gzfile("206.gz"), header = TRUE)
summary(mydata)
-------------------------------------------------------------
#OLS REGRESSION WITH DUMMIES
    # create dummy variables for race
  mydata$race_white <- ifelse(mydata$RACENEW == 100, 1, 0)
mydata$race_black <- ifelse(mydata$RACENEW == 200, 1, 0)
mydata$race_native <- ifelse(mydata$RACENEW == 300, 1, 0)
mydata$race_asian <- ifelse(mydata$RACENEW == 400, 1, 0)
mydata$Hispanic<-ifelse(mydata$HISPETH<=11,1,0)
# create dummy variables for marital status
mydata$marstat_married <- ifelse(mydata$MARST < 14, 1, 0)
mydata$marstat_notmarried <- ifelse(mydata$MARST > 19, 1, 0)

#Create dummy variables for employment stats 
mydata$Employed <- ifelse(mydata$EMPSTAT<200, 1, 0)
#Create dummy variables for Health insurance stats 
mydata$Covered <- ifelse(mydata$HINOTCOVE ==1, 1, 0)

mydata$ADHD<-ifelse(mydata$ADDEV ==2, 1, 0)
------------------------------------------------------
# Basic Regression model
Reg1 <- lm(EARNIMP1 ~ ADHD  + EDUC  + SEX + AGE +Hispanic+Employed+
               race_white + race_black + race_native + race_asian +
               marstat_married+Covered+NCHILD,
             data = mydata)
Reg2 <- lm(Employed ~ EARNIMP1+ADHD  + EDUC  + SEX + AGE +Hispanic+
             race_white + race_black + race_native + race_asian +
             marstat_married+Covered+NCHILD,
           data = mydata)
summary(Reg1)
summary(Reg2)

# Run diagnostic tests on the OLS model
bptest(Reg1)
bptest(Reg2)

-----------------------------------------------------
  #QUICK ATTEMPT TO FIX HETEROSKALADODICTY:
  model <- lm(formula = EARNIMP1 ~ ADHD + EDUC + SEX + AGE + Hispanic + Employed +
                race_white + race_black + race_native + race_asian + marstat_married +
                Covered + NCHILD, data = mydata)

# Compute robust standard errors
robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))

# Compute t-values and p-values using the robust standard errors
t_values <- coef(model) / robust_se
p_values <- 2 * pt(abs(t_values), df.residual(model), lower.tail = FALSE)

# Update the coefficients table with the new t-values and p-values
coefficients_table <- cbind(coef(model), robust_se, t_values, p_values)
colnames(coefficients_table) <- c("Estimate", "Robust SE", "t-value", "Pr(>|t|)")
coefficients_table
summary(model)
bptest(model)

library(nlme)

# Estimate variance of residuals
residuals <- resid(model)
variance <- var(residuals)

# Fit WLS model using inverse variances as weights
wls_model <- gls(EARNIMP1 ~ ADHD + EDUC + SEX + AGE + Hispanic + Employed + race_white + race_black +
                   race_native + race_asian + marstat_married + Covered + NCHILD, 
                 data = mydata, weights = varIdent(form = ~1))
wls_model$fittedvalues<-fitted.values(wls_model)
wls_model$residuals<-residuals(wls_model)
plot(wls_model$fittedvalues, wls_model$residuals, 
     xlab = "Predicted Values", ylab = "Residuals", 
     main = "Residuals vs. Fitted Values Plot")
length(wls_model$fittedvalues)
length(wls_model$residuals)
summary(wls_model)
bptest(wls_model)

robust_model <- lm(EARNIMP1 ~ ADHD + EDUC + SEX + AGE + Hispanic + Employed + race_white + race_black + race_native + race_asian + marstat_married + Covered + NCHILD, data = mydata)
se <- sqrt(diag(vcovHC(robust_model)))
t_values <- summary(robust_model)$coefficients[, "t value"]
t_stats <- t_values/se

bptest(robust_model)

----------------------------------------------------------
  #ATTEMPT TWO: LOGGING
  
  model9 <- lm(formula = log(EARNIMP1) ~ (ADHD) + log(EDUC) + SEX + log(AGE) + Hispanic + Employed +
                race_white + race_black + race_native + race_asian + marstat_married +
                Covered + log(NCHILD), data = na.omit(mydata))
panel1 <- na.omit(panel1)  # Remove observations with missing values


  ----------------------------------------------
  # Run Fixed Effect Models
  panel1 <- pdata.frame(mydata, index=c("YEAR"))

  fe_ols1 <- lm(EARNIMP1 ~ ADHD  + EDUC  + SEX + AGE +Hispanic+Employed+
                 race_white + race_black + race_native + race_asian +
                 marstat_married+Covered+NCHILD+factor(OCC)+factor(YEAR),
               data = panel1)
summary(fe_ols1)
fe_ols1 <- lm(Employed ~ ADHD  + EDUC  + SEX + AGE +Hispanic+EARNIMP1+
                race_white + race_black + race_native + race_asian +
                marstat_married+Covered+NCHILD+factor(OCC)+factor(YEAR),
              data = panel1)
summary(fe_ols1)
  ----------------------------------------------------------------------
#One way Fixed effect within model
  fe_ols2 <- plm(EARNIMP1 ~ ADHD  + EDUC  + SEX + AGE +Hispanic+Employed+
                race_white + race_black + race_native + race_asian +
                marstat_married+Covered+NCHILD,index=c("OCC","YEAR"),
              data = panel1,model = "within")
summary(fe_ols2)

fe_ols7 <- plm(Employed ~ EARNIMP1+ADHD  + EDUC  + SEX + AGE +Hispanic+
                 race_white + race_black + race_native + race_asian +
                 marstat_married+Covered+NCHILD,index=c("OCC","YEAR"),
               data = panel1,model = "within")
summary(fe_ols7)
 ------------------------------------------------------ 
 #Interaction Terms
   fe_ols3 <- lm(EARNIMP1 ~ ADHD + EDUC + SEX + AGE +
                 race_white + race_black + race_native + race_asian +
                 marstat_married +Employed+
                 EDUC*race_white + EDUC*race_black + EDUC*race_native + EDUC*race_asian,
               data = mydata)
summary(fe_ols3)
----------------------------------------------------------------

   fe_ols8 <- plm(EARNIMP1 ~ ADHD + EDUC + SEX + AGE +
                 race_white + race_black + race_native + race_asian +Hispanic+
                 marstat_married +ADHD*race_white + ADHD*race_black + ADHD*race_native + ADHD*race_asian,
               data = mydata, index=c("YEAR","OCC"), model="within")
summary(fe_ols8)
--------------------------------------------------
 ---------------------------------------------------------
  
  #Male VS Female


  iv7<-ivreg(formula = EARNIMP1 ~ Employed + ADHD + EDUC + SEX + AGE + Hispanic + 
          race_white + race_black + race_native + race_asian + marstat_married + 
          Covered + NCHILD + ADHD:SEX, data = mydata)
  summary(iv7)
  ----------------------
