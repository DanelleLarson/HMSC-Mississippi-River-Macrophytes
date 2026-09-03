setwd() 

library(Hmsc)

load(file="all_HMSC_Data_2026.R") #S,X,Y,Tr, Tax, P (based on Tax)

# Four Mississippi river pools have been sampled repeatedly in 681 unique aquatic areas annually
# for macrophytes during 22 years. Species presence-absence.

# Here pool U4 is selected for modelling:

# All 2640 samples from pool U4 are retained for modelling.
selrows = which(S$POOL == "U4")
Y = Y[selrows,]
S = droplevels(S[selrows,])

order_yearplot = order(S$YEAR, S$PLOT)
S = S[order_yearplot,]
X = X[order_yearplot,]
Y = Y[order_yearplot,]

names(X)

# Reduce to 9 variables for HMSC modelling:
XData = X[,names(X) %in% c("DEPTH", "VELOCITY", "CONNECTIVITY", "EXCEEDANCE", "logN", "logP", "TSS", "CHLOROPHYLL", "TEMPERATURE")]

cortable = round(cor(XData),2)
diag(cortable) = NA
max(cortable, na.rm = T)
#max. correlation between any pair of variables = 0.6

# Calculate annual means and residuals per covariate.
XData_annual = data.frame(matrix(NA, nrow = 0, ncol = 18))
names(XData_annual) = c(paste0(names(XData), "_mean"), paste0(names(XData), "_resid"))

for(n in 1:length(unique(S$YEAR)))
{
  sel_XData = XData[which(S$YEAR==unique(S$YEAR)[n]),]
  means = colMeans(sel_XData)
  XDatameans = sel_XData
  names(XDatameans) = paste0(names(sel_XData), "_mean")
  XDatares = sel_XData
  names(XDatares) = paste0(names(sel_XData), "_resid")
  for(i in 1:nrow(XDatameans))
  {
    XDatameans[i,] = means
    XDatares[i,] = sel_XData[i,]-means
  }
  XData_annual = rbind(XData_annual, cbind(XDatameans, XDatares))
}

#Define Hmsc XFormula based on these:
XFormula = ~DEPTH_mean + DEPTH_resid + VELOCITY_mean + VELOCITY_resid + CONNECTIVITY_mean + CONNECTIVITY_resid + EXCEEDANCE_mean + EXCEEDANCE_resid + logN_mean + logN_resid + logP_mean + logP_resid + TSS_mean + TSS_resid + CHLOROPHYLL_mean + CHLOROPHYLL_resid + TEMPERATURE_mean + TEMPERATURE_resid


# Check Y matrix for absent (0) or ubiquitous species (1).
range(colMeans(Y>0))
sort(colSums(Y>0))

# Exclude rare species, defined as those with < 20 occurrences.
rarespecies = which(colSums(Y>0)<20)
length(rarespecies)
# = 32 out of the original 48 taxa are rare by this definition. 
# Excluding these leaves 16 species in the dataset.
Y = Y[,-rarespecies]

hist(colMeans(Y>0),main="Species prevalence")

Tr = data.frame(Tr)
Tr = droplevels(Tr[-rarespecies,])
summary(Tr)

#For HMSC, retain just two traits due to limited data (1-2 species) in some categories of the others:
# LIFE_FORM and POLLINATION

TrFormula = ~LIFE_FORM + POLLINATION

head(S)
# Unique BARCODE, POOL, DATE, YEAR, STRATUM, EASTING, NORTHING AND RIVER_MILE

sum(duplicated(S[,c("EASTING","NORTHING")]))
# = 0. Coordinates are unique per sample.

studyDesign = data.frame(plot = as.factor(S$PLOT), stratum = as.factor(S$STRATUM), year = as.factor(S$YEAR))
studyDesign$stratum_year = as.factor(paste(studyDesign$stratum, studyDesign$year, sep = "_"))

#Define plot, stratum and year level random effects.
rL.plot = HmscRandomLevel(units = levels(studyDesign$plot))

rL.stratum = HmscRandomLevel(units = levels(studyDesign$stratum))

yr = data.frame(year=as.numeric(levels(studyDesign$year)))
rownames(yr) = levels(studyDesign$year)
rL.year = HmscRandomLevel(sData = yr)

Ypa = Y

#Define entire Hmsc model structure:
m1 = Hmsc(Y=Ypa, XData = XData_annual,  XFormula = XFormula,
          TrData = Tr, TrFormula = TrFormula,
          phyloTree = P,
          distr="probit",
          studyDesign=studyDesign,
          ranLevels={list("plot" = rL.plot, "stratum" = rL.stratum, "year" = rL.year)})

models = list(m1)
modelnames = c("pres_abs_PoolU4")

save(models,modelnames,file = "DL_unfitted_models_PoolU4_2025.Rdata")
