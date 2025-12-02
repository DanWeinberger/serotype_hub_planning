library(dplyr)
library(tidyr)
library(readr)
library(forcats)
set.seed(2025)

SEROTYPES <- c(
  "1","2","3","4","5","6A","6B","6C","6D","6E","6F","6G","6H","6I",
  "7A","7B","7C","7F","7Fv","7P","7V","7(23)","7D","8","8A","8B","8C","8D",
  "9A","9L","9N","9V","9E","10A","10B","10C","10F","10D","10E","10G",
  "11A","11B","11C","11D","11E","11F","11G","11H","11I","12A","12B","12F",
  "12Fv","44","46","13","14","15A","15B","15C","15D","15F","16A","16F",
  "17A","17F","17B","18A","18B","18C","18F","19A","19F","19B","19C","20",
  "20A","20B","20C","20D","21","22A","22F","23A","23B","23F","23V","24A",
  "24B","24F","25A","25F","27","28A","28F","29","31")

n <- 3000
years <- 2025
months <- 1:12

common_us <- c("6A","6B","19A","19F","23A","23B","35B","35F","11A","15B","15C","21","3")
num_common <- length(common_us)
remaining <- setdiff(SEROTYPES, common_us)

p_common <- rep(0.75/num_common, num_common)
p_remaining <- rep(0.25/length(remaining), length(remaining))
serotype_probs <- c(setNames(p_common, common_us), setNames(p_remaining, remaining))
serotype_probs <- serotype_probs[SEROTYPES]

sim <- tibble(
  id = 1:n,
  age_group = sample(c('<2','2-5','5-9','50-64','65+'), n, replace = TRUE,
                     prob = c(0.25,0.25,0.2,0.15,0.15)),
  race = sample(c('Black','White','Hispanic','Other'), n, replace = TRUE),
  vaccine_history = sample(c('None','PCV13','PCV15','PCV20'), n, replace = TRUE,
                           prob = c(0.4,0.2,0.2,0.2)),
  viral_test = sample(c('Positive','Negative'), n, replace = TRUE, prob = c(0.2,0.8)),
  month = sample(months, n, replace = TRUE),
  year = years,
  serotype = sample(SEROTYPES, n, replace = TRUE, prob = serotype_probs)
)

prob <- c('<2'=0.30,'2-5'=0.20,'5-9'=0.12,'50-64'=0.10,'65+'=0.08)

sim <- sim %>% mutate(colonized = rbinom(n(), 1, prob[age_group]))

write_csv(sim, "simulated_colonization.csv")

ipd <- sim %>%
  mutate(ipd = rbinom(n(), 1, colonized * 0.02))

write_csv(ipd, "simulated_ipd.csv")