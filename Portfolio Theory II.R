############################################################################################
############################################################################################
############################################################################################
#######################################   PORTFOLIO THEORY II   ############################
############################################################################################
############################################################################################
library(zoo)
library(PerformanceAnalytics)
library(quadprog)   ####### To solve quadratic programming problems, in this case we can also use
## the globalMin.portfolio, but is has to be noted also the more complicated way


rm(list=ls())
getwd()
setwd("C:/Escuela/Actuaría 9° Semestre/Datacamp_Coursera")
dir()
load("lab9.RData")
head(returns_df)

# Timeplots of stocks on seperate graphs
my.panel <- function(...) {
  lines(...)
  abline(h = 0)
}
plot(returns_df, lwd = 2, panel = my.panel, col = "blue")

# Timeplots of stocks on same graph
plot(returns_df, plot.type = "single", main = "Returns", col = 1:4, lwd = 2)
abline(h = 0)
legend(x = "bottomleft", legend = colnames(returns_df), col = 1:4, lwd = 2)


# Parameters CER model
mu_hat_month <- apply(returns_df, 2, mean)
mu_hat_month
sigma2_month <-apply(returns_df,2,var)

sigma2_month
sigma_month <-apply(returns_df,2,sd)

sigma_month
cov_mat_month <- var(returns_df)
cov_mat_month
cor_mat_month <-cor(returns_df)

cor_mat_month

# Pairwise scatterplots
pairs(coredata(returns_df),col="blue",pch=16)

# Calculate the global minimum variance portfolio, short sales allowed
global_min_var_portfolio <- globalMin.portfolio(mu_hat_month, cov_mat_month, shorts = TRUE)
global_min_var_portfolio

# Plot the portfolio weights of our four stocks
plot(global_min_var_portfolio)

# set restriction matrices
D_matrix <- 2 * cov_mat_month
D_matrix
d_vector <- rep(0, 4)
d_vector
A_matrix <- cbind(rep(1, 4), diag(4))
A_matrix
b_vector <- c(1, rep(0, 4))
b_vector

# use solve.QP to minimize portfolio variance
quad_prog <- solve.QP(Dmat=D_matrix,dvec=d_vector,Amat=A_matrix,bvec=b_vector,meq=1) ## meq=1 because of the equality constrait
## of the weights, recall that the sum of all the weights must be equal to one
quad_prog
## quad_prog$solution gives us the new weights for the intended portfolio (no short sales allowed)
global_min_var_portfolio <-globalMin.portfolio(er=mu_hat_month,cov.mat=cov_mat_month,shorts=FALSE)
## same thing but using the function globalMin.portfolio, it's solved equally as a quadratic programming problem
## but with all the complexity hidden

# Print out global_min_var_portfolio
global_min_var_portfolio
## Note that the weights of the portfolio are the same between each method


#####################
## Now we want a portfolio with a target return, in this case the highest
# highest average return
mu_target <- max(mu_hat_month)


## efficient.portfolio returns the portfolio with minimum variance subject to a target return
## works similar as globalMin.portfolio
# short sales allowed
efficient_porfolio_short <- efficient.portfolio(mu_hat_month, cov_mat_month, mu_target, shorts = TRUE)
efficient_porfolio_short
plot(efficient_porfolio_short)

# no short sales allowed
efficient_porfolio_no_short <- efficient.portfolio(mu_hat_month, cov_mat_month, mu_target, shorts = FALSE)
efficient_porfolio_no_short
plot(efficient_porfolio_no_short)

##########################
## Also we can write all efficient portfolios as a convex combination of efficient portfolios
## Z = A*M + (1-A)*N  where M & N are efficient portfolios and A is between -1 and 1

## Efficient frontier of risky assets
efficient_frontier <- efficient.frontier(mu_hat_month, cov_mat_month, alpha.min = -1, alpha.max = 1)

# Generate summary
summary(efficient_frontier)

# Plot
plot(efficient_frontier, plot.assets = TRUE, col = "blue", lwd = 2)

# risk free rate (monthly)
t_bill_rate <- 0.005

# Tangency portfolio short sales allowed
tangency_portfolio_short <- tangency.portfolio(mu_hat_month, cov_mat_month, risk.free = t_bill_rate, shorts = TRUE)
# Summary
summary(tangency_portfolio_short)
# Plot
plot(tangency_portfolio_short)

# Tangency portfolio short sales not allowed
tangency_portfolio_no_short <- tangency.portfolio(mu_hat_month, cov_mat_month, risk.free = t_bill_rate, shorts = FALSE)
# Summary
summary(tangency_portfolio_no_short)
# Plot
plot(tangency_portfolio_no_short)