#RevBayes Episodic Diversification model
#Anh Mai
#26 June 2026

setwd("~/maia1/260617_RevBayes_episodic_div_rates")

#install.packages("RevGadgets")
install.packages("logr")
library(RevGadgets)
library(ggplot2)

speciation_time_file <- "output/primates_EBD_lambda_times.log"
speciation_rate_file <- "output/primates_EBD_lambda_rates.log"
extinction_time_file <- "output/primates_EBD_mu_times.log"
extinction_rate_file <- "output/primates_EBD_mu_rates.log"

# read in and process rates
rates <- processDivRates(speciation_time_log = speciation_time_file,
                         speciation_rate_log = speciation_rate_file,
                         extinction_time_log = extinction_time_file,
                         extinction_rate_log = extinction_rate_file,
                         burnin = 0.25,
                         summary = "median")


mu_times <- read.delim("~/maia1/260617_RevBayes_episodic_div_rates/output/primates_EBD_extinction_times.log")
burn_in <- round(nrow(mu_times) * 0.25)
mu_times <- mu_times[burn_in:nrow(mu_times), ]
mu_times <- mu_times |> 
              group_by(interval)
              summarize()