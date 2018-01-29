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
                set_attribute("aAge", function() get_attribute(env,'aAgeInitial')) %>%
                set_attribute("aGender", function() inputs$vGender) %>%
                set_attribute("aGene", function() sample(1:6,1,prob=inputs$vGene)) %>% 
                #1-high/pathogenic,2-high/unknown,3-high/nonpath,4-no finding,5-low penetrance,6-moderate penetrance
                set_attribute("aTest", 2) %>% #1-tested, 2-not
                branch(function() get_attribute(env,"aGene"),
                       continue=c(rep(TRUE,6)), 
                       #1-not, 2-modify behavior:bene, 3-modify behavior:harm
                       trajectory("") %>% set_attribute("aMod",function() sample(1:2,1,prob=c(inputs$mod1,1-inputs$mod1))) %>%
                               branch(function() get_attribute(env,"aMod"),continue=c(TRUE,TRUE),
                                      trajectory("modify") %>% 
                                              set_attribute("aMod",function() sample(2:3,1,prob=c(inputs$behav1,1-inputs$behav1))),
                                      trajectory("not") %>% timeout(0)
                               ),
                       trajectory("") %>% set_attribute("aMod",function() sample(1:2,1,prob=c(inputs$mod2,1-inputs$mod2))) %>%
                               branch(function() get_attribute(env,"aMod"),continue=c(TRUE,TRUE),
                                      trajectory("modify") %>% 
                                              set_attribute("aMod",function() sample(2:3,1,prob=c(inputs$behav2,1-inputs$behav2))),
                                      trajectory("not") %>% timeout(0)
                               ),
                       trajectory("") %>% set_attribute("aMod",function() sample(1:2,1,prob=c(inputs$mod3,1-inputs$mod3))) %>%
                               branch(function() get_attribute(env,"aMod"),continue=c(TRUE,TRUE),
                                      trajectory("modify") %>% 
                                              set_attribute("aMod",function() sample(2:3,1,prob=c(inputs$behav3,1-inputs$behav3))),
                                      trajectory("not") %>% timeout(0)
                               ),
                       trajectory("") %>% set_attribute("aMod",1),
                       trajectory("") %>% set_attribute("aMod",1), 
                       trajectory("") %>% set_attribute("aMod",1) 
                        
                )
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

terminate_simulation <- function(traj)
{
        traj %>%
                branch(
                        function() 1, 
                        continue=FALSE,
                        trajectory() %>% cleanup_on_termination()
                )
}

####
## Test
test_event <- function(traj,inputs) {
        traj %>% set_attribute("aTest",1) %>% mark("test")
}


####
## Adverse events
time_to_AE = function(inputs) 
{
        #baseline risk
        gene=get_attribute(env,"aGene")
        rates=inputs[paste0("mod",gene)]
        
        #relative risk
        behav=get_attribute(env,"aMod")
        if(behav==2) {rr=0.8}
        else if(behav==3) {rr=1.3}
        else {rr=1}
        
        #time frame
        days  = 365
        
        #convert rate
        rates2 = (- (log ( 1 - rates)*rr) / days)
        t2e = rpexp(1, rate=c(rates2,epsilon), t=c(0,4*365))
        return(t2e)
}

AE_event <- function(traj) {
        traj %>% mark("AE") 
}


###
#fill in event_registry
event_registry <- list(
        list(name          = "Secular Death",
             attr          = "aSecularDeathTime",
             time_to_event = days_till_death,
             func          = secular_death,
             reactive      = FALSE)
        # list(name          = "Test",
        #      attr          = "aTestTime",
        #      time_to_event = 1,
        #      func          = test_event,
        #      reactive      = FALSE)
        # list(name          = "Adverse Event",
        #      attr          = "aAETime",
        #      time_to_event = time_to_AE,
        #      func          = AE_event,
        #      reactive      = FALSE)
)

#### Counters
counters <- c(
        "time_in_model", 
        "secular_death",
        "test",
        "AE"
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


