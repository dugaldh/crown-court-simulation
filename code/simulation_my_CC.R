library(simmer)
library(simmer.plot)
set.seed(1234)

# Set variables -----------------------------------------------------------

# Time
time_trial <- 11.9 / 2.6
time_sentencing <- 1.2 / 2.6
time_prison_sentence <- function() rnorm(1, )

# Probabilities
probability_plead_guilty <- 0.1
probability_found_guilty <- 0.3

probability_CC_not_MC <- 0.05

# Resources
resource_capacity_CC <- round(102600/365)
resource_capacity_MC <- round(resource_capacity_CC * 10)
resource_capacity_Prisons <- 50000

# resource_queue_CC <- 65000
# resource_queue_MC <- resource_queue_CC * 5
# resource_queue_Prisons <- 45000

# Generators
generator_arrivals_CC <- function() rexp(1,70000/365)
generator_arrivals_MC <- function() rexp(1,20000/365)
generator_arrivals_all <- function() rexp(1,70000/365 + 20000/365)

# Trajectories

sentencing_CC <- trajectory() %>% 
  # Take time for the sentencing
  log_("GUILTY") %>% 
  seize("crown_court", 1) %>% 
  timeout(task = time_sentencing) %>% 
  release("crown_court", 1) %>% 
  # Increase prison population by 1
  log_("Increasing prison population size by 1") %>% 
  # set_capacity(
  #   resource = "prison",
  #   value = 1,
  #   mod = "+"
  # )
  # set_queue_size(
  #   resource = "prison",
  #   value = 1,
  #   mod = "+"
  # )
  seize("prison", 1) %>% 
  timeout(task = time_prison_sentence) %>% 
  release("prison", 1)

case_CC <- trajectory() %>% 
  # Begin the process
  # log_(function() {paste("start_time:", now(env))}) %>% 
  # Decide whether pleading guilty/not guilty
  branch(
    option = function() {ifelse(runif(1) < probability_plead_guilty, 1, 2)},
    continue = c(FALSE, TRUE),
    c(sentencing_CC, trajectory())
  ) %>% 
  # Continue with trial, after not guilty plea
  # log_("Entering not guilty plea") %>% 
  seize("crown_court", 1) %>% 
  timeout(task = time_trial) %>% 
  release("crown_court", 1) %>% 
  # Decide whether found guilty/not guilty
  branch(
    option = function() {ifelse(runif(1) < probability_found_guilty, 1, 2)},
    continue = c(FALSE, TRUE),
    c(sentencing_CC, trajectory() %>% log_("NOT GUILTY"))
  )
# Finish as found not guilty

case_MC <- trajectory() %>%
  # Begin the process
  # log_(function() {paste("start_time:", now(env))}) %>%
  # Decide whether pleading guilty/not guilty
  branch(
    option = function() {ifelse(runif(1) < probability_plead_guilty, 1, 2)},
    continue = c(FALSE, TRUE),
    c(sentencing_CC, trajectory())
  ) %>%
  # Continue with trial, after not guilty plea
  log_("Entering not guilty plea") %>%
  seize("magistrate_court", 1) %>%
  timeout(task = time_trial) %>%
  release("magistrate_court", 1) %>%
  # Decide whether found guilty/not guilty and if guilty, refer to CC
  branch(
    option = function() {ifelse(runif(1) < probability_found_guilty, 1, 2)},
    continue = c(FALSE, TRUE),
    c(sentencing_CC, trajectory())
  )

case <- trajectory() %>% 
  log_("CPS brings charges") %>% 
  # Decide on whether Magistrates or Criminal court, can complicate later by type of case as generator
  branch(
    option = function() {ifelse(runif(1) < probability_CC_not_MC, 1, 2)},
    continue = c(FALSE, FALSE),
    c(case_CC, case_MC)
  )

# Build model -------------------------------------------------------------

env <- simmer("CrownCourtsSim") %>% 
  # Resources
  add_resource(name = "crown_court",
               capacity = resource_capacity_CC,
               # queue_size = resource_queue_CC
  ) %>% 
  add_resource(name = "magistrate_court",
               capacity = resource_capacity_MC,
               # queue_size = resource_queue_MC
  ) %>%
  add_resource(name = "prison",
               capacity = resource_capacity_Prisons,
               # queue_size = resource_queue_Prisons
  ) %>%
  # Generators
  add_generator(name_prefix = "cases",
                trajectory = case,
                distribution = generator_arrivals_all
  )
# add_generator(name_prefix = "crown_court_individual",
#               trajectory = case_CC,
#               distribution = generator_arrivals_CC
# )
# add_generator(name_prefix = "magistrate_court_individual",
#               trajectory = mc_individual,
#               distribution = CC_arrivals
#               )

env %>% 
  # Run the model
  run(until = 100) %>% 
  print()

resources <- get_mon_resources(env)
arrivals <- get_mon_arrivals(env)

plot(resources, metric = "utilization")
plot(arrivals, metric = "waiting_time")
