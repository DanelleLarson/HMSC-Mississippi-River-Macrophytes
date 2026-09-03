setwd()

library(Hmsc)

#Note: cross-validations run with thin 100 as these are very slow.

load(file = "DL_PoolU4_models_thin_100_samples_250_chains_4.Rdata")

nChains = 4
samples = 250
thin = 100

MF = list()
MFCV = list()
WAIC = list()

for(n in 1:1){
  m = models[[n]]
  preds = computePredictedValues(m)
  MF[[n]] = evaluateModelFit(hM=m, predY=preds)
  partition = createPartition(m, nfolds = 5)
  preds = computePredictedValues(m, partition=partition, nParallel = nChains)
  MFCV[[n]] = evaluateModelFit(hM=m, predY=preds)
  WAIC[[n]] = computeWAIC(m)       
}

save(MF, MFCV, WAIC, modelnames, file = "DL_PoolU4_MF2_models_thin_100_samples_250_chains_4.Rdata")
