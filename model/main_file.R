rm(list=ls())

pkg = list("simmer",
           "ggplot2",
           "reshape2",
           "plyr", #need to load this before "dplyr"
           "tidyr",
           "dplyr",
           "msm",
           "data.table",
           "deSolve")
invisible(lapply(pkg, require, character.only = TRUE))

###
###assign attributes

initialize_patient <- function(traj, inputs)
{
        traj %>%
                seize("time_in_model") %>%
                set_attribute("aAgeInitial", function() inputs$vAge) %>%
                set_attribute("aAge", function(attrs) attrs[['aAgeInitial']]) %>%
                set_attribute("aGender", function() inputs$vGender) %>%
                set_attribute("aGene", function() sample(1:2,1,prob=c(inputs$vGene,1-inputs$vGene)))#1 - targeted, 2 - not
}


####
## Inputs
source("./inputs.R")

####
## Secular Death
source('./event_secular_death.R')
# source('./events_simple.R')


####
## Cleanup 
cleanup_on_termination <- function(traj)
{
        traj %>% 
                release("time_in_model")
}

terminate_simulation <- function(traj, inputs)
{
        traj %>%
                branch(
                        function() 1, 
                        continue=FALSE,
                        trajectory() %>% cleanup_on_termination()
                )
}



###
#fill in event_registry
event_registry <- list(
        list(name          = "Secular Death",
             attr          = "aSecularDeathTime",
             time_to_event = days_till_death,
             func          = secular_death,
             reactive      = FALSE)
)

#### Counters
counters <- c(
        "time_in_model", 
        "secular_death"
)

source('./event_main_loop_simple.R')


##########
# Start the clock!
exec.simulation <- function(inputs)
{
        set.seed(12345)
        env  <<- simmer("Simple")
        traj <- simulation(env, inputs)
        env %>% create_counters(counters)
        
        env %>%
                add_generator("patient", traj, at(rep(0, inputs$vN)), mon=2) %>%
                run(365*inputs$vHorizon+1) %>% # Simulate just past horizon
                wrap()
        
        get_mon_arrivals(env, per_resource = T)
}


