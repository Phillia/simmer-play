# library(simmer)
rm(list=ls())

env <- simmer("signal", verbose = TRUE)

#1.assign parent/child, family id, whether to test (only matters for parents)
initialize_customer <- function(traj)
{
        traj %>%
                set_attribute("fuid", function() sample(1:3,1,prob=c(0.2,0.5,0.3))) %>%
                set_attribute("parent", function() sample(1:2,1,prob=c(0.6,0.4))) %>% #1-parent;2-child
                set_attribute("totest",function() sample(1:2,1,prob=c(0.5,0.5))) %>% #1-test;2-not; only matter for parents
                log_(function() paste0("fuid:",f1(),"; parent:",f2(),"; totest:",f3()))
        
}

f1 <- function() {
        get_attribute(env,"fuid")
}

f2 <- function() {
        get_attribute(env,"parent")
}

f3 <- function() {
        get_attribute(env,"totest")
}

#2.trigger: split traj for parent and child
#When a parent gets tested, the simulation broadcasts the family id and triggers testing children with that family id.
trigger <- function(traj) {
        traj %>%
                branch(f2,
                       continue=c(T,T),
                       trajectory("parent") %>% parent(),
                       trajectory("child") %>% child()
                       )
}
        

#traj parent: delay 10 and signal fuid if test
parent <- function(traj) {
        traj %>%
                timeout(10) %>%
                branch(
                        f3,
                        continue=c(T,T),
                        trajectory("test") %>% 
                                send(function() paste(f1())) %>% 
                                seize("test") %>% timeout(2) %>% release("test") ,
                        trajectory("not") %>% timeout(0)
                )

}

#traj child: wait signal to test
child <- function(traj) {
        traj %>%
                trap(function() paste(f1())) %>%
                wait() %>% timeout(5) %>% seize("test") %>% timeout(2) %>% release("test")
}


#high-level traj
patient <-   trajectory("patient") %>%
        initialize_customer()  %>%
        trigger() 

env %>% add_resource("test", Inf,0) %>%
        add_generator("patient", patient, at(rep(0,10))) %>%
        run()


get_mon_arrivals(env, per_resource = T)


