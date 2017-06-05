library(MSwM)
data <- read.csv(file.choose(), header = TRUE, sep = ",")
lwap <- as.ts(data$V1)
markovmod <- lm(lwap ~ 1, data)
summary(markovmod)

markov <- msmFit(markovmod, k = 2, sw = c(TRUE, TRUE, TRUE, TRUE, TRUE), p = 3, 
              control = list(trace = TRUE, maxiter = 200, tol = 10e-8, maxiterOuter = 25, maxiterInner = 50))

summary(markov)
#to get the filtered probabilities
markov@Fit@filtProb
#to get the smoothed probabilities
markov@Fit@smoProb
stop

#RESULT
Markov Switching Model

Call: msmFit(object = markovmod, k = 2, sw = c(TRUE, TRUE, TRUE, TRUE, TRUE), p = 3, 
             control = list(trace = TRUE, maxiter = 200, tol = 1e-07, 
                            maxiterOuter = 25, maxiterInner = 50))

AIC      BIC    logLik
1441.742 1496.053 -712.8708

Coefficients:
  
  Regime 1 
---------
  Estimate Std. Error t value  Pr(>|t|)    
(Intercept)(S) 9807.6570  1779.0136  5.5130 3.528e-08 ***
  lwap_1(S)         1.0264     0.1957  5.2448 1.565e-07 ***
  lwap_2(S)        -1.2726     0.3032 -4.1972 2.702e-05 ***
  lwap_3(S)        -0.6684     0.3895 -1.7160   0.08616 .  
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1909.711
Multiple R-squared: 0.8035

Standardized Residuals:
  Min          Q1         Med          Q3         Max 
-2848.27125  -615.13177   -58.90158   284.25019  5046.30341 

Regime 2 
---------
  Estimate Std. Error t value  Pr(>|t|)    
(Intercept)(S) 805.6506   441.0429  1.8267   0.06774 .  
lwap_1(S)        0.6928     0.0912  7.5965 3.042e-14 ***
  lwap_2(S)       -0.1887     0.0754 -2.5027   0.01232 *  
  lwap_3(S)        0.2603     0.0568  4.5827 4.590e-06 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1063.982
Multiple R-squared: 0.653

Standardized Residuals:
  Min            Q1           Med            Q3           Max 
-1645.1679044  -681.1158575     0.3116837   787.1139235  1882.7405680 

Transition probabilities:
  Regime 1  Regime 2
Regime 1 0.4693673 0.1720301
Regime 2 0.5306327 0.8279699
