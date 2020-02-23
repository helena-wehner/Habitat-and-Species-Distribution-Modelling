##########################
### SSF Example Paper/AMT Package ####
### file:///C:/Users/Lenovo/Desktop/Master/Buffalo_SSF/SSF/SSF_in_R/Fitting%20Step-Selection%20Functions%20with%20amt.html
### Fitting Step Selection Functions with package: amt
##########################
# movement data: red deer (northern Germany)
# environmental data: one covariate - forest cover map

# Getting the data ready

library(lubridate)
library(raster)
install.packages("amt")
library(amt)

data("deer")
deer
plot(deer)

# in order to continue we need a regular sampling rate
# checking the current sampling rate
summarize_sampling_rate(deer)
# the median sampling rate is 6 h, what we aimed for

# next we have to get the environmental covariates
# a forest layer is included in this package (as regular raster layer)
data("sh_forest")
sh_forest
plot(sh_forest)

### Prepare Data for SSF

# first: change from a point presentation to a step presentation
ssf1 <- steps_by_burst(deer)
# note: written as pipeline
ssf1 <- deer %>% steps_by_burst()
ssf1

plot(ssf1)

# generate random steps
ssf1 <- ssf1 %>% random_steps(n = 15)

#extract the covariates at the end point of each step
ssf1 <- ssf1 %>% extract_covariates(sh_forest)

# forest layer is coded as: 1=forest and 2!=forest (we create a factor with appropriate levels)
# also calculate the log of the step length and the cosine of the turn angle (we may use later for integrated step selection)
ssf1 <- ssf1 %>% 
  mutate(forest = factor(sh.forest, levels = 1:2, labels = c("forest", "non-forest")), 
         cos_ta = cos(ta_), 
         log_sl = log(sl_)) 

### Fitting SSF
# Now all pieces are there to fit a SSF. We will use fit_clogit, which is a wrapper around survival::clogit.

m0 <- ssf1 %>% fit_clogit(case_ ~ forest + strata(step_id_))
m1 <- ssf1 %>% fit_clogit(case_ ~ forest + forest:cos_ta + forest:log_sl + log_sl * cos_ta + strata(step_id_))
m2 <- ssf1 %>% fit_clogit(case_ ~ forest + forest:cos_ta + forest:log_sl + log_sl + cos_ta + strata(step_id_))

summary(m0)
summary(m1)
summary(m2)

### A Note to Piping
# all steps described above, could easily be wrapped into on piped workflow

m1 <- deer %>% 
  steps_by_burst() %>% random_steps(n = 15) %>% 
  extract_covariates(sh_forest) %>% 
  mutate(forest = factor(sh.forest, levels = 1:2, labels = c("forest", "non-forest")), 
         cos_ta = cos(ta_), 
         log_sl = log(sl_)) %>% 
  fit_clogit(case_ ~ forest + forest:cos_ta + forest:sl_ + sl_ * cos_ta + strata(step_id_))

summary(m1)
plot(m1$more)

### Session
devtools::session_info()

sessionInfo()
