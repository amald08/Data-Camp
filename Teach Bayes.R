## Teach Bayes
library(TeachBayes)
# Define new spinner: regions
regions<-c(2,2,4)

# Simulation 1000 spins: spins
spins<-spinner_data(regions,1000)

# Graph the spin data using bar_plot()
bar_plot(spins)

# Construct frequency table of spins
table(spins)

# Find fraction of spins equal to 2
mean(spins==2)

# Find mean spin value
mean(spins)

# Create the vector of models: Model
Model <- c("Spinner A", "Spinner B")

# Define the vector of prior probabilities: Prior
Prior <- c(.5,.5)

# Define the vector of likelihoods: Likelihood
Likelihood <- c(1/2 ,1/6)

# Make a data frame with variables Model, Prior, Likelihood: bayes_df
bayes_df<-data.frame(Model,Prior,Likelihood)

# Compute the posterior probabilities
bayesian_crank(bayes_df)
# Display the vector of models: Model
Model <- c("Spinner A", "Spinner B")

# Define the vector of prior probabilities: Prior
Prior <- c(.75,.25)

# Define the vector of likelihoods: Likelihood
Likelihood <- c(1/2,1/6)

# Make a data frame with variables Model, Prior, Likelihood: bayes_df
bayes_df<-data.frame(Model,Prior,Likelihood)

# Compute the posterior probabilities
bayesian_crank(bayes_df)