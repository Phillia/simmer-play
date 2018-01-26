setwd("~/Desktop/simmer-play/simple/model")
rm(list=ls())
source("./main_file.R")
source("./costs_simple.R")

## Look at summary statistics
#can modify here
inputs$vHorizon <- 10
inputs$vN <- 100
inputs$vAge <- 40

results <- exec.simulation(inputs)
at <- arrange(get_mon_attributes(env),name,key,time)

DT <- data.table(results)
summary <- DT[, .N, by = resource]
summary 


cost_qaly <- cost.qaly(results,inputs) 
cost_qaly1 <- cost.qaly.i(results,inputs)
