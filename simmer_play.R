# library(simmer)
# library(dplyr)
# library(simmer.plot)

rm(list=ls())

#set family status as global object, update and regularly evaluate
#eg: white chocolate lover tiggers family signal, flip everyone to dark, get chocolates 


env <- simmer("play", verbose = TRUE)

#family global object: update to 2 and trigger group action
signal <- data.frame(fuid=c(1,2,3),test=c(1,1,1))

#assign taste preference and family id
initialize_customer <- function(traj)
{
        traj %>%
                set_attribute("aTaste",function() sample(1:2,1,prob=c(0.5,0.5))) %>%
                set_attribute("fuid", function() sample(1:3,1,prob=c(0.2,0.5,0.3))) 
}

#return taste attribute
f1 <- function() {
        get_attribute(env,"aTaste")
}

#return family id attribute
f2 <- function() {
        get_attribute(env,"fuid")
}

#main event
get_choco <- function(traj) {        
        traj %>%
                branch(f1,
                       continue=rep(TRUE,2),
                       trajectory("dark") %>% seize("dark") %>% timeout(5) %>% release("dark"),
                       trajectory("white") %>% seize("white") %>% timeout(10) %>% release("white")
                )
}


#trigger: if white, update signal
trigger <- function(traj) {
        traj %>%
        branch(f1,
               continue=rep(TRUE,2),
               trajectory("not") %>% timeout(0),
               trajectory("yes") %>% timeout(function() {
                       id <- f2()
                       signal$test[signal$fuid==id] <<- 2; 0
                       })
               )
}

#flip: if signal, set to black
flip <- function(traj) {
        traj %>% branch(
                function() {id <- f2()
                        signal$test[signal$fuid==id]},
                continue=rep(TRUE,2),
                        trajectory("") %>% timeout(0),
                        trajectory("") %>% set_attribute("aTaste",1) %>% seize("flip") %>% release("flip")
        )

}

#wrap actions to rollback
toloop <- function(traj) {
        traj %>% flip() %>% get_choco() %>% timeout(5)
}

#high-level traj
customer <-   trajectory("customer") %>%
        initialize_customer()  %>%
        trigger() %>%
        branch( # Used branch, to prevent rollback from looking inside event loop function
                function() 1,
                continue=TRUE,
                trajectory("main_loop") %>% toloop()
        ) %>% 
        rollback(amount=1, times=100) # Process up to 100 events per person

#register counters
counters <- c("white","dark","flip")
create_counters <- function(env, counters)
{
        sapply(counters, FUN=function(counter)
        {
                env <- add_resource(env, counter, Inf, 0)
        })
        
        env
}

#execution
env %>%
        create_counters(counters) %>%
        add_generator("customer",customer,at(1:10), mon=2) %>%
        run(50) 

get_mon_arrivals(env, per_resource = T)
arrange(get_mon_attributes(env),name,key,time)

plot(customer)