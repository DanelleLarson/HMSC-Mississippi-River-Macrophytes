#HMSC results for marginal mean effects of the environment on species richness
#Danelle updated in December 2025 (dmlarson@usgs.gov)

#Setup---------
#session info
sessionInfo()


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

#load libraries
library(Hmsc)
library(abind)


#load HMSC fitted models
setwd("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Analyses\\HMSC final fit models Dec 2025\\")
fitted_models = list.files(path = ".", pattern = "DL_Pool", recursive = T)



# SUPPLEMENTAL FIGURE 4----------
#re-oligotrophy covariates only
covariates <- c("logN_mean", "logP_mean", "TSS_mean", "CHLOROPHYLL_mean")

# Data order = fitted_models order
pool_names_loaded <- c("Pool 13", "Pool 8", "Lower Pool 4", "Upper Pool 4")

# Legend presentation order (e.g., upstream -> downstream)
desired_order <- c("Upper Pool 4", "Lower Pool 4", "Pool 8", "Pool 13")

# Named colors by pool (so lines + legend always match)
colours <- c(
  "Upper Pool 4" = "#D6453E",  # brick red
  "Lower Pool 4" = "#2A00A2",  # dark blue
  "Pool 8"       = "#6FD1B3",  # teal
  "Pool 13"      = "#F9C74F"   # gold
)

# Fixed y-limits across all panels
fixed_ylim <- c(0, 4)

# Panel letters (A–D)
panel_letters <- c("(A)", "(B)", "(C)", "(D)")

# Toggle smoothing (TRUE for smooth spline, FALSE for raw line)
use_smoothing <- TRUE

# compute curves for a covariate 
get_curves_for_covariate <- function(covariate, fitted_models, pool_order) {
  qpredS_per_pool <- vector("list", length(pool_order))
  gradientvals_per_pool <- vector("list", length(pool_order))
  
  for (n in seq_along(pool_order)) {
    e <- new.env()
    load(fitted_models[n], envir = e)
    m <- e$models[[1]]  # adjust if needed based on your saved object
    
    Gradient <- constructGradient(m, focalVariable = covariate, non.focalVariables = 1)
    predY <- predict(m, Gradient = Gradient, expected = TRUE)
    predS <- abind(lapply(predY, rowSums), along = 2)
    
    # Median-only curve
    qpredS_per_pool[[n]]       <- apply(predS, 1, median, na.rm = TRUE)
    gradientvals_per_pool[[n]] <- Gradient$XDataNew[, 1]
  }
  list(qpredS_per_pool = qpredS_per_pool,
       gradientvals_per_pool = gradientvals_per_pool)
}

#  4-panel figure (each panel = one covariate, labeled A–D) 
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # 2x2 panels

for (i in seq_along(covariates)) {
  covariate <- covariates[i]
  
  curves <- get_curves_for_covariate(covariate, fitted_models, pool_names_loaded)
  qpredS_per_pool       <- curves$qpredS_per_pool
  gradientvals_per_pool <- curves$gradientvals_per_pool
  
  # x-limits per panel from data (use global if you prefer)
  x_all <- unlist(gradientvals_per_pool)
  xlim_vals <- range(x_all, na.rm = TRUE)
  
  # y-limits fixed across panels
  ylim_vals <- fixed_ylim
  
  # Start panel (no main title; covariate name removed from title)
  p<- plot(NA, NA,
           xlab = covariate,                     # keep as X-axis label; change to "" if you want no label
           ylab = "Predicted species richness",
           xlim = xlim_vals,
           ylim = ylim_vals)
  
  # Draw lines in ORIGINAL data order; color selected by pool label
  for (n in seq_along(pool_names_loaded)) {
    lbl <- pool_names_loaded[n]
    x <- gradientvals_per_pool[[n]]
    y <- qpredS_per_pool[[n]]
    
    # Optional smoothing
    if (use_smoothing) {
      ok <- is.finite(x) & is.finite(y)
      x2 <- x[ok]; y2 <- y[ok]
      if (length(unique(x2)) >= 4) {
        ss <- try(smooth.spline(x2, y2, spar = 0.6), silent = TRUE)
        if (!inherits(ss, "try-error")) {
          lines(ss$x, ss$y, col = colours[[lbl]], lwd = 6)
        } else {
          lines(x, y, col = colours[[lbl]], lwd = 6)
        }
      } else {
        lines(x, y, col = colours[[lbl]], lwd = 6)
      }
    } else {
      lines(x, y, col = colours[[lbl]], lwd = 6)
    }
  }
  
  # Panel letter (A/B/C/D) at top-left; covariate title removed
  mtext(panel_letters[i], side = 3, adj = 0, line = 0.2, cex = 1.1, font = 2)
  
  # Legend only in the first panel
  if (i == 1) {
    legend("topright",
           legend = desired_order,
           fill   = colours[desired_order],
           pt.cex = 2,
           cex    = 1.1,
           bty    = "n")
  }
}





#SUPPLEMENTAL FIG 4 PRINTED-----------

# Open a TIFF device BEFORE plotting
tiff("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Figures HMSC\\SuppFig4.tiff",
     width = 8, height = 6, units = "in", res = 600, compression = "lzw")

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # 2x2 panels

for (i in seq_along(covariates)) {
  covariate <- covariates[i]
  
  curves <- get_curves_for_covariate(covariate, fitted_models, pool_names_loaded)
  qpredS_per_pool       <- curves$qpredS_per_pool
  gradientvals_per_pool <- curves$gradientvals_per_pool
  
  x_all <- unlist(gradientvals_per_pool)
  xlim_vals <- range(x_all, na.rm = TRUE)
  ylim_vals <- fixed_ylim
  
  plot(NA, NA,
       xlab = covariate,
       ylab = "Predicted species richness",
       xlim = xlim_vals,
       ylim = ylim_vals)
  
  for (n in seq_along(pool_names_loaded)) {
    lbl <- pool_names_loaded[n]
    x <- gradientvals_per_pool[[n]]
    y <- qpredS_per_pool[[n]]
    
    if (use_smoothing) {
      ok <- is.finite(x) & is.finite(y)
      x2 <- x[ok]; y2 <- y[ok]
      if (length(unique(x2)) >= 4) {
        ss <- try(smooth.spline(x2, y2, spar = 0.6), silent = TRUE)
        if (!inherits(ss, "try-error")) {
          lines(ss$x, ss$y, col = colours[[lbl]], lwd = 6)
        } else {
          lines(x, y, col = colours[[lbl]], lwd = 6)
        }
      } else {
        lines(x, y, col = colours[[lbl]], lwd = 6)
      }
    } else {
      lines(x, y, col = colours[[lbl]], lwd = 6)
    }
  }
  
  mtext(panel_letters[i], side = 3, adj = 0, line = 0.2, cex = 1.1, font = 2)
  
  if (i == 1) {
    legend("topright",
           legend = desired_order,
           fill   = colours[desired_order],
           pt.cex = 2,
           cex    = 1.1,
           bty    = "n")
  }
}

dev.off()  # Close the TIFF device




# Extras all marginal means----------
#includes covariates that did not have 90%+ posterior probability
#depth and exceedance had >90% probability; velocity, temp, and connectivity did not

covariates <- c("DEPTH_mean", "CONNECTIVITY_mean", "VELOCITY_mean", "EXCEEDANCE_mean", "logN_mean", "logP_mean", "TSS_mean", "CHLOROPHYLL_mean", "TEMPERATURE_mean")

# Data order = fitted_models order
pool_names_loaded <- c("Pool 13", "Pool 8", "Lower Pool 4", "Upper Pool 4")

# Legend presentation order (e.g., upstream -> downstream)
desired_order <- c("Upper Pool 4", "Lower Pool 4", "Pool 8", "Pool 13")

# Named colors by pool (so lines + legend always match)
colours <- c(
  "Upper Pool 4" = "#D6453E",  # brick red
  "Lower Pool 4" = "#2A00A2",  # dark blue
  "Pool 8"       = "#6FD1B3",  # teal
  "Pool 13"      = "#F9C74F"   # gold
)

# Fixed y-limits across all panels
fixed_ylim <- c(0, 4)

# Panel letters (A–I)
panel_letters <- c("(A)", "(B)", "(C)", "(D)", "(E)", "(F)", "(G)", "(H)", "(I)")

# Toggle smoothing (TRUE for smooth spline, FALSE for raw line)
use_smoothing <- TRUE

# compute curves for a covariate 
get_curves_for_covariate <- function(covariate, fitted_models, pool_order) {
  qpredS_per_pool <- vector("list", length(pool_order))
  gradientvals_per_pool <- vector("list", length(pool_order))
  
  for (n in seq_along(pool_order)) {
    e <- new.env()
    load(fitted_models[n], envir = e)
    m <- e$models[[1]]  # adjust if needed based on your saved object
    
    Gradient <- constructGradient(m, focalVariable = covariate, non.focalVariables = 1)
    predY <- predict(m, Gradient = Gradient, expected = TRUE)
    predS <- abind(lapply(predY, rowSums), along = 2)
    
    # Median-only curve
    qpredS_per_pool[[n]]       <- apply(predS, 1, median, na.rm = TRUE)
    gradientvals_per_pool[[n]] <- Gradient$XDataNew[, 1]
  }
  list(qpredS_per_pool = qpredS_per_pool,
       gradientvals_per_pool = gradientvals_per_pool)
}

#  6-panel figure (each panel = one covariate, labeled A–I) 
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))  # 3x3 panels

for (i in seq_along(covariates)) {
  covariate <- covariates[i]
  
  curves <- get_curves_for_covariate(covariate, fitted_models, pool_names_loaded)
  qpredS_per_pool       <- curves$qpredS_per_pool
  gradientvals_per_pool <- curves$gradientvals_per_pool
  
  # x-limits per panel from data (use global if you prefer)
  x_all <- unlist(gradientvals_per_pool)
  xlim_vals <- range(x_all, na.rm = TRUE)
  
  # y-limits fixed across panels
  ylim_vals <- fixed_ylim
  
  # Start panel (no main title; covariate name removed from title)
  plot(NA, NA,
       xlab = covariate,                     # keep as X-axis label; change to "" if you want no label
       ylab = "Predicted species richness",
       xlim = xlim_vals,
       ylim = ylim_vals)
  
  # Draw lines in ORIGINAL data order; color selected by pool label
  for (n in seq_along(pool_names_loaded)) {
    lbl <- pool_names_loaded[n]
    x <- gradientvals_per_pool[[n]]
    y <- qpredS_per_pool[[n]]
    
    # Optional smoothing
    if (use_smoothing) {
      ok <- is.finite(x) & is.finite(y)
      x2 <- x[ok]; y2 <- y[ok]
      if (length(unique(x2)) >= 4) {
        ss <- try(smooth.spline(x2, y2, spar = 0.6), silent = TRUE)
        if (!inherits(ss, "try-error")) {
          lines(ss$x, ss$y, col = colours[[lbl]], lwd = 6)
        } else {
          lines(x, y, col = colours[[lbl]], lwd = 6)
        }
      } else {
        lines(x, y, col = colours[[lbl]], lwd = 6)
      }
    } else {
      lines(x, y, col = colours[[lbl]], lwd = 6)
    }
  }
  
  # Panel letter (A/B/C/D) at top-left; covariate title removed
  mtext(panel_letters[i], side = 3, adj = 0, line = 0.2, cex = 1.1, font = 2)
  
  # Legend only in the first panel
  if (i == 1) {
    legend("topright",
           legend = desired_order,
           fill   = colours[desired_order],
           pt.cex = 2,
           cex    = 1.1,
           bty    = "n")
  }
}

###END