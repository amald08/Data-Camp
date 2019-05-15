################################################################################
################################################################################
################################################################################
################################################################################
################ INTRODUCTION TO PORTFOLIO THEORY
library(PerformanceAnalytics)
library(zoo)
rm(list=ls())

setwd("C:/Escuela/Actuaría 9° Semestre/Datacamp_Coursera")
dir()

load("C:/Escuela/Actuaría 9° Semestre/Datacamp_Coursera/lab8.RData")
#pdf(file="Portfolio Theory Graphs.pdf")
ls()
head(returns_df)
tail(returns_df)

# Estimate the parameters: multivariate
mu_hat_annual <- apply(returns_df, 2, mean) * 12
sigma2_annual <- apply(returns_df, 2, var) * 12
sigma_annual <- sqrt(sigma2_annual)
cov_mat_annual <- cov(returns_df) * 12
cov_hat_annual <- cov(returns_df)[1,2] * 12
rho_hat_annual <- cor(returns_df)[1,2]

# The annual estimates of the CER model parameters for Boeing and Microsoft
mu_boeing <- mu_hat_annual["rboeing"]
mu_msft <- mu_hat_annual["rmsft"]
sigma2_boeing <-  sigma2_annual["rboeing"]
sigma2_msft <- sigma2_annual["rmsft"]
sigma_boeing <- sigma_annual["rboeing"]
sigma_msft <- sigma_annual["rmsft"]
sigma_boeing_msft <- cov_hat_annual
rho_boeing_msft <- rho_hat_annual

# Type ls() in the console to see variables
#ls()
# The ratio Boeing stock vs Microsoft stock (adds up to 1)
boeing_weights <- seq(from = -1, to = 2, by = 0.1)
msft_weights <- 1 - boeing_weights

# Portfolio parameters
mu_portfolio <- boeing_weights * mu_boeing + msft_weights * mu_msft
sigma2_portfolio <- boeing_weights ^ 2 * sigma2_boeing + msft_weights ^ 2 * sigma2_msft + 2 * boeing_weights * msft_weights * sigma_boeing_msft
sigma_portfolio <- sqrt(sigma2_portfolio)

# Plotting the different portfolios
plot(sigma_portfolio, mu_portfolio, type = "b", pch = 16, ylim = c(0, max(mu_portfolio)), xlim = c(0, max(sigma_portfolio)), xlab = expression(sigma[p]), ylab = expression(mu[p]), col = c(rep("green", 18), rep("red", 13)))
text(x = sigma_boeing, y = mu_boeing, labels = "Boeing", pos = 4)
text(x = sigma_msft, y = mu_msft, labels = "Microsoft", pos = 4)

# Annual risk-free rate of 3% per year for the T-bill
t_bill_rate <- 0.03


# Portfolio parameters
mu_portfolio_boeing_bill <- t_bill_rate + boeing_weights * (mu_boeing - t_bill_rate)
sigma_portfolio_boeing_bill <- boeing_weights * sigma_boeing

# Plot previous exercise
# Portfolio Combination Boeing and T-bills
points(sigma_portfolio_boeing_bill, mu_portfolio_boeing_bill, type = "b", col = "blue",pch=16)

# Sharpe ratio Boeing
# average return earned in excess of the risk-free rate per unit of volatility or total risk

sharp_ratio_boeing <-(mu_boeing-t_bill_rate)/sigma_boeing

###################################################################################################
###################################################################################################
#################################### GLOBALMIN.PORTFOLIO FUNCTION##################################
###################################################################################################
###################################################################################################
globalMin.portfolio <- function(er, cov.mat,shorts=T) 
  { 
    # Compute global minimum variance portfolio 
    # 
    # inputs: 
    # er	N x 1 vector of expected returns 
    # cov.mat	N x N return covariance matrix 
    # 
    # output is portfolio object with the following elements 
    # call	original function call 
    # er	portfolio expected return 
    # sd	portfolio standard deviation 
    # weights	N x 1 vector of portfolio weights 
    call <- match.call() 
    
    # 
    # check for valid inputs 
    # 
    asset.names <- names(er) 
    er <- as.vector(er)	# assign names if none exist 
    cov.mat <- as.matrix(cov.mat) 
    N<-length(er)
    if(N != nrow(cov.mat)) 
      stop("invalid inputs") 
    if(any(diag(chol(cov.mat)) <= 0)) 
      stop("Covariance matrix not positive definite") 
    # remark: could use generalized inverse if cov.mat is positive semi-definite 
    
    # 
    # compute global minimum portfolio 
    # 
    if(shorts==TRUE)
    {
      cov.mat.inv<-solve(cov.mat)
      one.vec<-rep(1,N)
      w.gmin<-rowSums(cov.mat.inv)/sum(cov.mat.inv)
      w.gmin<-as.vector(w.gmin)
    }
    else if (shorts==FALSE)
    {
      Dmat<-2*cov.mat
      dvec<-rep.int(0,N)
      Amat<-cbind(rep(1,N),diag(1,N))
      bvec<-c(1,rep(0,N))
      result<-solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
      w.gmin<-round(result$solution,6)
    }
    else{
      stop("short needs to be logical. For no-shorts, shorts=FALSE.")
    }
    names(w.gmin) <- asset.names 
    er.gmin <- crossprod(w.gmin,er) 
    sd.gmin <- sqrt(t(w.gmin) %*% cov.mat %*% w.gmin) 
    gmin.port <- list("call" = call, 
                  "er" = as.vector(er.gmin), 
                  "sd" = as.vector(sd.gmin), 
                  "weights" = w.gmin) 
    class(gmin.port) <- "portfolio" 
    gmin.port 
} 
# The global minimum variance portfolio
global_min_var_portfolio <- globalMin.portfolio(er=mu_hat_annual, cov.mat=cov_mat_annual)
global_min_var_portfolio


# Summary of global_min_var_portfolio that takes into account the annual risk-free rate of 3% per year
summary(global_min_var_portfolio, risk.free = 0.03)

# Portfolio weights Boeing and Microsoft
barplot(global_min_var_portfolio$weights,main="Portfolio Weights",ylab="Weight")

# Plot previous exercises
plot(sigma_portfolio, mu_portfolio, type = "b", pch = 16, ylim = c(0, max(mu_portfolio)), xlim = c(0, max(sigma_portfolio)), xlab = expression(sigma[p]), ylab = expression(mu[p]), col = c(rep("green", 18), rep("red", 13)))
text(x = sigma_boeing, y = mu_boeing, labels = "Boeing", pos = 4)
text(x = sigma_msft, y = mu_msft, labels = "MSFT", pos = 4)

# Plot the position of the global minimum variance portfolio
text(x = global_min_var_portfolio$sd, y = global_min_var_portfolio$er, labels = "Global min", pos = 2)
points(x = global_min_var_portfolio$sd, y =global_min_var_portfolio$er,col=1,pch=17)

############################################################################################################
############################################################################################################
################################## TANGENCY PORTFOLIO ######################################################
############################################################################################################
############################################################################################################

## Portfolio on the efficient frontier with the highest sharpe ratio
tangency.portfolio<-function(er,cov.mat,risk.free,shorts=TRUE){
  
# compute tangency portfolio
#
# inputs:
# er				   N x 1 vector of expected returns
# cov.mat		   N x N return covariance matrix
# risk.free		 scalar, risk-free rate
# shorts          logical, allow shorts is TRUE
#
# output is portfolio object with the following elements
# call			  captures function call
# er				  portfolio expected return
# sd				  portfolio standard deviation
# weights		 N x 1 vector of portfolio weights
call <- match.call()

#
# check for valid inputs
#
asset.names <- names(er)
if(risk.free < 0)
  stop("Risk-free rate must be positive")
er <- as.vector(er)
cov.mat <- as.matrix(cov.mat)
N <- length(er)
if(N != nrow(cov.mat))
  stop("invalid inputs")
if(any(diag(chol(cov.mat)) <= 0))
  stop("Covariance matrix not positive definite")
# remark: could use generalized inverse if cov.mat is positive semi-definite

#
# compute global minimum variance portfolio
#
gmin.port <- globalMin.portfolio(er, cov.mat, shorts=shorts)
if(gmin.port$er < risk.free)
  stop("Risk-free rate greater than avg return on global minimum variance portfolio")

# 
# compute tangency portfolio
#
if(shorts==TRUE){
  cov.mat.inv <- solve(cov.mat)
  w.t <- cov.mat.inv %*% (er - risk.free) # tangency portfolio
  w.t <- as.vector(w.t/sum(w.t))	# normalize weights
} else if(shorts==FALSE){
  Dmat <- 2*cov.mat
  dvec <- rep.int(0, N)
  er.excess <- er - risk.free
  Amat <- cbind(er.excess, diag(1,N))
  bvec <- c(1, rep(0,N))
  result <- solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
  w.t <- round(result$solution/sum(result$solution), 6)
} else {
  stop("Shorts needs to be logical. For no-shorts, shorts=FALSE.")
}

names(w.t) <- asset.names
er.t <- crossprod(w.t,er)
sd.t <- sqrt(t(w.t) %*% cov.mat %*% w.t)
tan.port <- list("call" = call,
                 "er" = as.vector(er.t),
                 "sd" = as.vector(sd.t),
                 "weights" = w.t)
class(tan.port) <- "portfolio"
tan.port
}
################################################################################################
################################################################################################
################################################################################################

tangency_portfolio <-tangency.portfolio(mu_hat_annual,cov_mat_annual,risk.free=.03)

# Print out tangency portfolio
tangency_portfolio

# Summary of tangency_portfolio with annual risk free rate of 3%
summary(tangency_portfolio,risk.free=.03)

# Portfolio weights Boeing and Microsoft
barplot(tangency_portfolio$weights,main="Tangency Portfolio Weights",ylab="Weight")


# Plot previous exercises
plot(sigma_portfolio, mu_portfolio, type = "b", pch = 16, ylim = c(0, max(mu_portfolio)), xlim = c(0, max(sigma_portfolio)), xlab = expression(sigma[p]), ylab = expression(mu[p]), col = c(rep("green", 18), rep("red", 13)))
text(x = sigma_boeing, y = mu_boeing, labels = "Boeing", pos = 4)
text(x = sigma_msft, y = mu_msft, labels = "MSFT", pos = 4)

# Plot the position of the global minimum variance portfolio
text(x = global_min_var_portfolio$sd, y = global_min_var_portfolio$er, labels = "Global min", pos = 2)
points(x = global_min_var_portfolio$sd, y =global_min_var_portfolio$er,col=1,pch=17)

# Plot the position of the Tangency Portfolio

text(x=tangency_portfolio$sd,y=tangency_portfolio$er,labels="Tangency Port",pos=2)
points(x=tangency_portfolio$sd,y=tangency_portfolio$er,col=1,pch=17)


##############################################
### TANGENCY T-BILL RATE

# Annual risk-free rate of 3% per year for the T-bill
t_bill_rate <- 0.03

# Set of tangency portfolio weights
tangency_weights <- seq(from = 0, to = 2, by = 0.1)

# Portfolio parameters
mu_portfolio_tangency_bill <-t_bill_rate+tangency_weights*(tangency_portfolio$er-t_bill_rate)
sigma_portfolio_tangency_bill <-tangency_weights*tangency_portfolio$sd

# Plot previous exercises
plot(sigma_portfolio, mu_portfolio, type = "b", pch = 16,
     ylim = c(0, max(mu_portfolio)), xlim = c(0, max(sigma_portfolio)),
     xlab = expression(sigma[p]), ylab = expression(mu[p]),
     col = c(rep("green", 18), rep("red", 13)))
text(x = sigma_boeing, y = mu_boeing, labels = "Boeing", pos = 4)
text(x = sigma_msft, y = mu_msft, labels = "MSFT", pos = 4)

# Plot portfolio combinations of tangency portfolio and T-bills
text(x = tangency_portfolio$sd, y = tangency_portfolio$er, labels = "Tangency", pos = 2)
points(sigma_portfolio_tangency_bill,mu_portfolio_tangency_bill,type="b",col="blue",pch=16)



# Define the portfolio ratio's
tangency_weight <- 0.3
t_bill_weight <- 1 - tangency_weight

# Define the portfolio parameters
mu_portfolio_efficient <- t_bill_rate + tangency_weight * (tangency_portfolio$er - t_bill_rate)
sd_portfolio_efficient <- tangency_weight * tangency_portfolio$sd

# Plot previous exercises
plot(sigma_portfolio, mu_portfolio, type = "b", pch = 16, ylim = c(0, max(mu_portfolio)), xlim = c(0, max(sigma_portfolio)), xlab = expression(sigma[p]), ylab = expression(mu[p]), col = c(rep("green", 18), rep("red", 13)))
text(x = sigma_boeing, y = mu_boeing, labels = "Boeing", pos = 4)
text(x = sigma_msft, y = mu_msft, labels = "MSFT", pos = 4)
text(x = tangency_portfolio$sd, y = tangency_portfolio$er, labels = "Tangency", pos = 2)
points(sigma_portfolio_tangency_bill, mu_portfolio_tangency_bill, type = "b", col = "blue", pch = 16)

# Plot Efficient Portfolio with 30% Tangency
points(sd_portfolio_efficient, mu_portfolio_efficient, type = "b", col = "orange", pch = 16, cex = 2)

# Add text
text(x = sd_portfolio_efficient, y = mu_portfolio_efficient, labels = "Efficient Portfolio with 30% Tangency", pos = 4, cex = 0.75)

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
######### find the efficient portfolio (combination of 
######### T-bills and tangency portfolio) that has the same risk (SD) as Boeing.
# Define the tangency portfolio ratio
tangency_weight  <- sigma_boeing / tangency_portfolio$sd

# Calculate the weight of the tangency portfolio in the portfolio
mu_portfolio_efficient <- t_bill_rate + tangency_weight * (tangency_portfolio$er - t_bill_rate)
sd_portfolio_efficient <- tangency_weight * tangency_portfolio$sd

# Plot previous exercises
plot(sigma_portfolio, mu_portfolio, bg = "NA", type = "b", pch = 16, ylim = c(0, max(mu_portfolio)), xlim = c(0, max(sigma_portfolio)), xlab = expression(sigma[p]), ylab = expression(mu[p]), col = c(rep("green", 18), rep("red", 13)))
text(x = sigma_boeing, y = mu_boeing, labels = "Boeing", pos = 4)
text(x = sigma_msft, y = mu_msft, labels = "MSFT", pos = 4)
text(x = tangency_portfolio$sd, y = tangency_portfolio$er, labels = "Tangency", pos = 2)
points(sigma_portfolio_tangency_bill, mu_portfolio_tangency_bill, type = "b", col = "blue", pch = 16)

# Plot Efficient Portfolio with the same risk as Boeing
points(sd_portfolio_efficient, mu_portfolio_efficient, type = "p", col = "orange", pch = 16, cex = 2)
text(x = sd_portfolio_efficient, y = mu_portfolio_efficient, labels = "Efficient Portfolio with same risk as Boeing", pos = 2, cex = 0.75)
dev.off()
############################################################################################################
############################################################################################################