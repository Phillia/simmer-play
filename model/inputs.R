###initial inputs
epsilon <- 0.000000000001
inputs <- list(
        
        vHorizon = 1,
        vN = 100,
        
        vAge = 40,
        vGender = 1,
        vGene = c(0.1,0.2,0.3,0.2,0.1,0.1),
        mod1=0.5,
        mod2=0.3,
        mod3=0.7,
        behav1=0.5,
        behav2=0.5,
        behav3=0.5,
        
        disutilities = list(
                secular_death = 1
        ),
        
        durations = list(
        ),
        
        type = list(
                secular_death = 0
        ),
        
        costs = list(
        )
)
