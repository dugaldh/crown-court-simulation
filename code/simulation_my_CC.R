library(simmer)
library(simmer.plot)
set.seed(5678)

# Set variables -----------------------------------------------------------

# Time
time_trial_CC <- function() rexp(n = 1, rate = log(2) / ((11.9 / 2.6) / (365)))
# (2.6 * 365 * log(2)) / 11.9 # average time for a trial in years, adjusting for 5 day weeks
time_sentencing_CC <- function() rexp(n = 1, rate = log(2) / ((1.2 / 2.6) / (365))) # average time for a sentencing in years, adjusting for 5 day weeks
time_trial_MC <- function() rexp(n = 1, rate = 2 * log(2) / ((11.9 / 2.6) / (365)))
time_sentencing_MC <- function() rexp(n = 1, rate = 5 * log(2) / ((1.2 / 2.6) / (365)))

time_prison_sentence <- function() rexp(n = 1, rate = log(2) / (20.9 / 12)) # 6% of cases are immediate custody, average custodial sentence in years https://data.justice.gov.uk/cjs-statistics/cjs-sentence-types

# Probabilities
probability_plead_guilty <- 0.65 # probability a defendant pleads guilty, according to https://www.gov.uk/government/statistics/criminal-court-statistics-quarterly-october-to-december-2023/criminal-court-statistics-quarterly-october-to-december-2023, but CPS (https://www.cps.gov.uk/publication/cps-data-summary-quarter-1-2023-2024) suggests 0.71
probability_found_guilty <- 0.3 # probability a defendant is found guilty (having pleaded not guilty)

probability_CC_not_MC <- 0.05 # probability that a trial goes to crown court, rather than go to a magistrate court - in lieu of knowledge about the crime, looks to agree with https://www.gov.uk/government/statistics/criminal-justice-system-statistics-quarterly-march-2024

# Resources
resource_capacity_CC <- round(106500 / 365) # https://www.bbc.co.uk/news/articles/c5yr03p24y5o
resource_capacity_MC <- round(resource_capacity_CC * 1300000 / 70000)
resource_capacity_Prisons <- 87900

resource_queue_CC <- 65000 # https://www.instituteforgovernment.org.uk/publication/performance-tracker-2023/criminal-courts
resource_queue_MC <- 350000 # https://www.instituteforgovernment.org.uk/publication/performance-tracker-2023/criminal-courts
resource_queue_Prisons <- 0 

# Generators
# generator_arrivals_CC <- function() rexp(1,70000 / 365)
# generator_arrivals_MC <- function() rexp(1,20000 / 365)
generator_arrivals_all <- function() rexp(1, 1300000)

# Trajectories

sentencing_CC <- trajectory() %>% 
  # Take time for the sentencing
  # log_("GUILTY") %>% 
  seize("crown_court", 1) %>% 
  timeout(task = time_sentencing_CC) %>%
  release("crown_court", 1) %>% 
  seize("prison", 1) %>%
  timeout(task = time_prison_sentence) %>%
  release("prison", 1)

sentencing_MC <- trajectory() %>% 
  # Take time for the sentencing
  # log_("GUILTY") %>% 
  seize("magistrate_court", 1) %>% 
  timeout(task = time_sentencing_MC) %>%
  release("magistrate_court", 1) %>% 
  seize("prison", 1) %>%
  timeout(task = time_prison_sentence) %>%
  release("prison", 1)

case_CC <- trajectory() %>% 
  # Begin the process
  # log_(function() {paste("start_time:", now(env))}) %>% 
  # Decide whether pleading guilty/not guilty
  branch(
    option = function() {ifelse(sample(1:1000,1)/1000 < probability_plead_guilty, 1, 2)},
    continue = c(FALSE, TRUE),
    c(sentencing_CC, trajectory())
  ) %>% 
  # Continue with trial, after not guilty plea
  # log_("Entering not guilty plea") %>% 
  seize("crown_court", 1) %>% 
  timeout(task = time_trial_CC()) %>% 
  release("crown_court", 1) %>% 
  # Decide whether found guilty/not guilty
  branch(
    option = function() {ifelse(sample(1:1000,1)/1000 < probability_found_guilty, 1, 2)},
    continue = c(FALSE, TRUE),
    # c(sentencing_CC, trajectory() %>% log_("NOT GUILTY"))
    c(sentencing_CC, trajectory())
  )
  # Finish as found not guilty

case_MC <- trajectory() %>%
  # Begin the process
  # log_(function() {paste("start_time:", now(env))}) %>%
  # Decide whether pleading guilty/not guilty
  branch(
    option = function() {ifelse(sample(1:1000,1)/1000 <= probability_plead_guilty, 1, 2)},
    continue = c(FALSE, TRUE),
    c(sentencing_MC, trajectory())
  ) %>%
  # Continue with trial, after not guilty plea
  # log_("Entering not guilty plea") %>%
  seize("magistrate_court", 1) %>%
  timeout(task = time_trial_MC()) %>%
  release("magistrate_court", 1) %>%
  # Decide whether found guilty/not guilty and if guilty, refer to CC
  branch(
    option = function() {ifelse(sample(1:1000,1)/1000 <= probability_found_guilty, 1, 2)},
    continue = c(FALSE, TRUE),
    c(sentencing_MC, trajectory())
  )

case <- trajectory() %>% 
  # log_("CPS brings charges") %>% 
  # Decide on whether Magistrates or Criminal court, can complicate later by type of case as generator
  branch(
    option = function() {ifelse(sample(1:1000,1)/1000 <= probability_CC_not_MC, 1, 2)},
    continue = c(FALSE, FALSE),
    c(case_CC, case_MC)
  )

# Build model -------------------------------------------------------------

env <- simmer("CrownCourtsSim") %>% 
  # Resources
  add_resource(
    name = "crown_court",
    capacity = resource_capacity_CC,
    # queue_size = resource_queue_CC
  ) %>% 
  add_resource(
    name = "magistrate_court",
    capacity = resource_capacity_MC,
    # queue_size = resource_queue_MC
  ) %>%
  add_resource(
    name = "prison",
    capacity = resource_capacity_Prisons,
    # queue_size = resource_queue_Prisons
  ) %>%
  # Generators
  add_generator(
    name_prefix = "cases",
    trajectory = case,
    distribution = generator_arrivals_all
  ) 
  # add_generator(
  #   name_prefix = "crown_court_individual",
  #   trajectory = case_CC,
  #   distribution = generator_arrivals_CC
  # ) %>% 
  # add_generator(
  #   name_prefix = "magistrate_court_individual",
  #   trajectory = mc_individual,
  #   distribution = CC_arrivals
  # )
  
env %>% 
  # Run the model
  run(until = 1) %>% 
  print()

resources <- get_mon_resources(env)
arrivals <- get_mon_arrivals(env)

plot(resources, metric = "utilization")
plot(arrivals, metric = "waiting_time")
