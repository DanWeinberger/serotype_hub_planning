library(tidyverse) 
library(INLA)
s1 <- read.csv('https://data.cdc.gov/api/views/qvzb-qs6p/rows.csv?accessType=DOWNLOAD')
 saveRDS(s1,'./Data/ABCs_st_1998_2023.rds') 

 start.forecast = 2022
 test.st = '19F'
 
s1 <- readRDS('./Data/ABCs_st_1998_2023.rds') %>%
  rename(agec = "Age.Group..years.",
         year=Year,
         st=IPD.Serotype,
         N_IPD = Frequency.Count) %>%
  mutate( st= if_else(st=='16','16F', st)) %>%
  group_by(st,agec,Site, year) %>%
  summarize(N_IPD=sum(N_IPD)) %>%
  ungroup() %>%
  tidyr::complete(agec,year,st,Site,fill=list(N_IPD=0)) %>%
  mutate(yearN = year - min(year) + 1,
         yearN2 = yearN,
         agegrp = as.factor(agec),
         agec = as.numeric(as.factor(agec)),
         agec2 = agec,
         N_IPD_fit = if_else(year>=end.training, NA_integer_, N_IPD)
         ) %>%
  group_by(agec, Site,year) %>%
  mutate(offset1 = sum(N_IPD)) %>%
  ungroup()



form2 <- as.formula("N_IPD_fit ~ 1 + agegrp + 
                  f(yearN2, model='rw2',  hyper =   list(
                              prec = list(
                                prior = 'pc.prec',
                                param = c(1, 0.01)   
                              )
                            ),   
                  scale.model=TRUE, 
                  constr=TRUE, 
                  replicate=agec2) 
                  ")

subset <- s1 %>% filter(st==test.st & Site =='All_Sites')

mod.offset = subset$offset1

mod1 <- inla(form2, data = subset,  family = "nbinomial",E=mod.offset,
             control.compute = list(dic = FALSE, 
                                    waic = FALSE, 
                                    config = T,
                                    return.marginals=F
             ),
             # save predicted values on response scale
             control.predictor = list(compute=TRUE, link=1),
             control.inla = list(strategy='adaptive', # adaptive gaussian
                                 cmin=0),
             control.fixed = list(mean.intercept=0, 
                                  prec.intercept=0.04, # precision 1
                                  mean=0, 
                                  prec=1), # weakly regularising on fixed effects (sd of 1)
             inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
             num.threads=8
)    

n_samples <- 5000

samples <- inla.posterior.sample(n = n_samples, result = mod1)

# Generate samples from the posterior
generate_counts <- function(s) {
  
  # 1. Extract the linear predictor (eta)
  # INLA stores the linear predictor for the observations with the prefix "Predictor"
  # We use grep to find the rows corresponding to the linear predictor
  idx <- grep("^Predictor", rownames(s$latent))
  eta <- s$latent[idx, 1]
  
  # 2. Transform to the mean scale (lambda)
  # lambda = exp(eta)
  # Note: If you have an offset that was NOT included in the formula 
  # (e.g., passed via E=...), you must multiply by it here: lambda <- exp(eta) * data$E
  lambda <- exp(eta)*mod.offset
  
  # 3. Simulate counts (Posterior Predictive Distribution)
  # We use rpois to account for observation uncertainty
  y_pred <- rpois(n = length(lambda), lambda = lambda)
  
  return(y_pred)
}

# Apply the function to all 1000 samples
# This results in a matrix where rows are observations and columns are samples
predicted_counts_matrix <- sapply(samples, generate_counts)

preds_summary <- data.frame(
  obs_index = 1:nrow(predicted_counts_matrix),
  
  # Calculate the MEAN prediction across all samples for each observation
  mean_pred = apply(predicted_counts_matrix, 1, mean),
  
  # Calculate the 2.5th percentile (Lower CI bound)
  lower_ci = apply(predicted_counts_matrix, 1, quantile, probs = 0.025),
  
  # Calculate the 97.5th percentile (Upper CI bound)
  upper_ci = apply(predicted_counts_matrix, 1, quantile, probs = 0.975)
) 

subset2 <- cbind.data.frame(subset, preds_summary) %>%
      mutate(lower_ci = if_else(year<start.forecast, mean_pred, lower_ci),
             upper_ci = if_else(year<start.forecast, mean_pred, upper_ci),
             )
ggplot(subset2)+
  geom_line(aes(x=year, y=mean_pred))+
  geom_ribbon(aes(x=year, ymin=lower_ci, ymax=upper_ci), alpha=0.2)+
  geom_point(aes(x=year, y=N_IPD))+
             facet_wrap(~agec)+
  theme_classic()

