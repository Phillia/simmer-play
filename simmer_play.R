#library(simmer)
#library(dplyr)

env <- simmer("play", verbose = TRUE)

inputs <- list(
        strategy="Ask" #Ask vs None
)

initialize_customer <- function(traj)
{
        traj %>%
                set_attribute("aTaste",function() sample(1:2,1,prob=c(0.5,0.5))) %>%
                set_attribute("fuid", function() sample(1:3,1,prob=c(0.2,0.5,0.3)))
}

get_choco <- function(traj)
{        
        traj %>%
                seize("chocolate") %>%
                branch(function(attrs) attrs[["aTaste"]],
                       continue=rep(TRUE,2),
                       trajectory("dark") %>% seize("dark") %>% timeout(5) %>% release("dark"),
                       trajectory("white") %>% seize("white") %>% timeout(10) %>% release("white") %>%
                               send(signals = function(attrs) paste0(attrs[["fuid"]]),delay=3)
                ) %>% 
                release("chocolate")
}

ask <- function(traj,inputs) {
        if(inputs$strategy=="None") {
                traj %>% set_attribute("aTaste",1) 
        } else {
                traj 
        }
                
}

customer <-   trajectory("customer") %>%
        initialize_customer()    %>%
        ask(inputs) %>%
        branch( # Used branch, to prevent rollback from looking inside event loop function
                function() 1,
                continue=TRUE,
                trajectory("main_loop") %>% get_choco()
        ) %>% 
        rollback(amount=1, times=100) 

counters <- c("chocolate","white","dark")
create_counters <- function(env, counters)
{
        sapply(counters, FUN=function(counter)
        {
                env <- add_resource(env, counter, Inf, 0)
        })
        
        env
}


env %>%
        create_counters(counters) %>%
        add_generator("customer",customer,at(rep(0, 5)), mon=2) %>%
        run(15) %>% wrap() 

get_mon_arrivals(env, per_resource = T)


# arrange(get_mon_attributes(env),name,key,time)

