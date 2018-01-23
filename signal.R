# library(simmer)

rm(list=ls())

env <- simmer("play", verbose = TRUE)


f1 <- function() {
        get_attribute(env,"fuid")
}

parent <- trajectory() %>%
        set_attribute("fuid", function() sample(1:3,1,prob=c(0.2,0.5,0.3))) %>%
        timeout(10) %>%
        log_(function() paste(f1())) %>%
        send(function() paste(f1()))

child <- trajectory() %>%
        set_attribute("fuid", function() sample(1:3,1,prob=c(0.2,0.5,0.3))) %>%
        log_(function() paste(f1())) %>%
        trap(function() paste(f1())) %>%
        wait() %>%
        log_(function() paste(f1()))

env %>% add_generator("child", child, at(rep(0,5))) %>%
        add_generator("parent", parent, at(rep(0,3))) %>%      
        run()


# get_mon_arrivals(env, per_resource = T)
