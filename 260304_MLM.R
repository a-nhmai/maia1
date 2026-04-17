#260304 Lab: JPL tutorial on MultiLevel Models


##### Libraries #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' 
library(cmdstanr) # R Interface to 'CmdStan' 
library(posterior) # Tools for Working with Posterior Distributions 
library(bayesplot) # Plotting for Bayesian Models 
library(brms) # Bayesian Regression Models using 'Stan' 
library(ape) # Analyses of Phylogenetics and Evolution 

bayesplot::color_scheme_set("brightblue")
theme_set(bayesplot::theme_default(base_family = "sans"))

##### Data ##### 

### Trait data and predictors
dt <- read_csv("~/mai_a/260304_MLM/e120_long_Jan-2026.csv")

glimpse(dt)

dt %>% 
  view()

dt1 <- dt |>
  filter(biomass > 0) |>
  group_by(NumSp, Year, species, tipLabel) |>
  reframe(bio_mean = mean(biomass, na.rm = TRUE),
          sd = sd(biomass, na.rm = TRUE),
          bio_mean_log = log(mean(biomass, na.rm = TRUE)))

glimpse(dt1)
head(dt1)


#running MLM 
model1 <- brm(bio_mean_log ~ Year, 
              data = dt1, 
              cores = 4,
              iter = 2000,
              warmup = 2000/5, #20% for burnout
              chains = 4,
              backend = "cmdstanr"#engine: stan or *high level processing of stan* 
) 

summary(model1)

conditional_effects(model1)

plot(conditional_effects(model1), points = TRUE)

hypothesis(model1, "Year < 0")

bayes_R2(model1)
loo_R2(model1)

pp_check(model1, ndraws = 100)


##more complicated model
model2 <- brm(bio_mean_log ~ Year + (1|species), 
              data = dt1, 
              cores = 4,
              iter = 2000,
              warmup = 2000/5, #20% for burnout
              chains = 4,
              backend = "cmdstanr"#engine: stan or *high level processing of stan* 
) 

summary(model2)

model3 <- brm(bio_mean_log ~ Year + (1 + Year |species), 
              data = dt1, 
              cores = 4,
              iter = 2000,
              warmup = 2000/5, #20% for burnout
              chains = 4,
              backend = "cmdstanr"#engine: stan or *high level processing of stan* 
) 

summary(model3)

coef(model3)


phy <- read.nexus("~/2026_Spring/2026_lab/MLM/CDR_e120_planted.nex") #var-covar matrix

A <- ape::vcv.phylo(phy = phy) #transform into var-covar matrix

model4 <- brm(formula = bio_mean_log ~ Year + 
                (1 + Year | species) + #random int and slope
                (1 | gr(tipLabel, cov = A)), 
             data = dt1, 
             data2 = list(A = A),
             cores = 10,
             iter = 5000,
             warmup = 5000/5, 
             chains = 4,
             backend = "cmdstanr"
) 

summary(model4)
