setwd()

library(Hmsc)

load(file = "DL_unfitted_models_PoolU4_2025.Rdata")

nChains = 4
samples = 250
thin = 1000

n = 1

    m = models[[n]]
    m = sampleMcmc(m, samples = samples, thin=thin,
                   adaptNf=rep(ceiling(0.4*samples*thin),m$nr),
                   transient = ceiling(0.5*samples*thin),
                   nChains = nChains, nParallel = nChains)
    models[[n]] = m

save(models, modelnames, file="DL_PoolU4_models_thin_1000_samples_250_chains_4.Rdata")
