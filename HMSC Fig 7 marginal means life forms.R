#HMSC results for marginal mean effects of the environment on life forms
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





#traits attempt 1-------------
#base map from HMSC

#chl a temporal
#Create empty list objects to collect predictions per pool to plot.
qpredS_per_pool = list()
gradientvals_per_pool = list()

covariate = "CHLOROPHYLL_mean"

pool_names = vector()

for(n in 1:4)
{
  load(fitted_models[n])
  m = models[[1]]
  
  pool_names[n] = gsub("pres_abs_", "", modelnames)
  
  covariates = all.vars(m$XFormula)
  
  # I have used argument non.focalVariables = 1 so that we get marginal predictions for the 
  # focal X covariate, keeping all other variables at their mean in the dataset.
  
  Gradient = constructGradient(m, focalVariable=covariate, non.focalVariables = 1)
  predY = predict(m, Gradient=Gradient, expected = TRUE) 
  predS = abind(lapply(predY, rowSums), along=2)
  
  qpredS_per_pool[[n]] = apply(predS, c(1), quantile, probs = c(0.025, 0.5, 0.975), na.rm=TRUE)
  gradientvals_per_pool[[n]] = Gradient$XDataNew[,1]
}


par(mfrow= c(1, 1))
for(n in 1:4)
{
  qpredS = qpredS_per_pool[[n]] 
  focalgradient = gradientvals_per_pool[[n]] 
  
  lo = qpredS[1, ]
  hi = qpredS[3, ]
  me = qpredS[2, ]
  
  colour = colours[n]
  cicol = adjustcolor(colour, alpha.f = 0.2)
  
  
  {
    plotGradient(m, Gradient, pred=predY, measure="T", index = 3, showData =T,cicol = rgb(0.16,0.67, 0.88, alpha = 0.8), pointsize = 0)
    #plot(focalgradient, me, xlab = covariate, ylab = "Predicted species richness", ylim = c(0,3), type = "l", col = colour, lwd = 2)
    polygon(c(focalgradient, rev(focalgradient)), c(lo, rev(hi)), col = cicol, border = FALSE)
    legend("topright", pool_names, fill = colours, cex = 0.8, bty = "n") 
  }
}

#attempt 2 using copilot--------

###############################################################################
# Setup: paths, models, covariates, pools, colors, plot options
###############################################################################

# --- Load HMSC fitted models (adjust path if needed) ---
setwd("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Analyses\\HMSC final fit models Dec 2025\\")
fitted_models <- list.files(path = ".", pattern = "DL_Pool", recursive = TRUE)

# --- Covariates (panels plotted in this order) ---
covariates <- c("logN_mean", "logP_mean", "TSS_mean", "CHLOROPHYLL_mean")

# --- Pools: data order corresponds to fitted_models order ---
pool_names_loaded <- c("Pool 13", "Pool 8", "Lower Pool 4", "Upper Pool 4")
stopifnot(length(fitted_models) == length(pool_names_loaded))  # guard

# --- Legend order (e.g., upstream -> downstream) ---
desired_order <- c("Upper Pool 4", "Lower Pool 4", "Pool 8", "Pool 13")

# --- Colors by pool (named vector: line colors & legend colors) ---
colours <- c(
  "Upper Pool 4" = "#D6453E",  # brick red
  "Lower Pool 4" = "#2A00A2",  # dark blue
  "Pool 8"       = "#6FD1B3",  # teal
  "Pool 13"      = "#F9C74F"   # gold
)

# --- Panel letters and plotting options ---
panel_letters <- c("(A)", "(B)", "(C)", "(D)")
use_smoothing <- TRUE                  # toggle smoothing spline
fixed_ylim    <- c(0, 1)               # proportions in [0,1]
group_lty     <- c(submersed = 1, floating = 2)  # solid vs dashed

# --- Trait and group definitions (case-insensitive matching) ---
trait_name         <- "LIFE_FORM"
group_levels_sub   <- c("submersed", "submerged")
group_levels_float <- c("floating", "floating-leaved")

###############################################################################
# Helpers: studyDesign, dfPiNew builder, safe XDataNew builder
###############################################################################

get_study_design <- function(m) {
  sd <- m$studyDesign
  if (is.null(sd)) {
    sd <- try(m$D$studyDesign, silent = TRUE)
    if (inherits(sd, "try-error")) sd <- NULL
  }
  sd
}

# Build dfPiNew with the SAME column names and factor levels as studyDesign
build_dfPiNew <- function(m, n_points) {
  sd <- get_study_design(m)
  if (is.null(sd)) return(NULL)  # no random levels -> dfPiNew not needed
  
  required_cols <- colnames(sd)
  if (is.null(required_cols) || length(required_cols) == 0) {
    required_cols <- names(m$ranLevels)
  }
  if (is.null(required_cols) || length(required_cols) == 0) {
    stop("Model has random effects but studyDesign column names could not be determined.")
  }
  
  # Template row: factor/character -> most frequent level; numeric -> mean
  template <- lapply(required_cols, function(nm) {
    col <- sd[[nm]]
    if (is.factor(col)) {
      lev <- levels(col); lev[which.max(tabulate(as.integer(col)))]
    } else if (is.character(col)) {
      fac <- factor(col); lev <- levels(fac); lev[which.max(tabulate(as.integer(fac)))]
    } else if (is.numeric(col)) {
      mean(col, na.rm = TRUE)
    } else {
      idx <- which(!is.na(col))[1]; if (is.na(idx)) NA else col[idx]
    }
  })
  names(template) <- required_cols
  
  dfPiNew <- as.data.frame(template, stringsAsFactors = FALSE)
  dfPiNew <- dfPiNew[rep(1, n_points), , drop = FALSE]
  
  # Preserve factor levels exactly
  for (nm in required_cols) {
    col <- sd[[nm]]
    if (is.factor(col)) {
      dfPiNew[[nm]] <- factor(dfPiNew[[nm]], levels = levels(col))
    } else if (is.character(col)) {
      dfPiNew[[nm]] <- as.character(dfPiNew[[nm]])
    } else if (is.numeric(col)) {
      dfPiNew[[nm]] <- as.numeric(dfPiNew[[nm]])
    }
  }
  
  # Ensure column order matches studyDesign (some builds are strict)
  dfPiNew <- dfPiNew[, required_cols, drop = FALSE]
  
  dfPiNew
}

# Build XDataNew across the observed range of the focal covariate; non-focals set to mean or mode
build_XDataNew <- function(m, focalVariable, n_points) {
  X <- m$XData
  if (is.null(X) || !(focalVariable %in% colnames(X))) {
    stop("Covariate '", focalVariable, "' not found in m$XData. Available: ",
         paste(colnames(X), collapse = ", "))
  }
  if (!is.numeric(X[[focalVariable]])) {
    stop("Focal variable '", focalVariable, "' must be numeric to build a continuous gradient.")
  }
  
  # Reference row: numeric -> mean; factor/char -> most frequent level
  ref_vals <- lapply(colnames(X), function(nm) {
    col <- X[[nm]]
    if (is.numeric(col)) {
      mean(col, na.rm = TRUE)
    } else if (is.factor(col)) {
      lev <- levels(col); lev[which.max(tabulate(as.integer(col)))]
    } else if (is.character(col)) {
      fac <- factor(col); lev <- levels(fac); lev[which.max(tabulate(as.integer(fac)))]
    } else {
      col[which(!is.na(col))[1]]
    }
  })
  names(ref_vals) <- colnames(X)
  
  ref_df <- as.data.frame(ref_vals, stringsAsFactors = FALSE)
  for (nm in names(ref_df)) {
    if (is.factor(X[[nm]]))      ref_df[[nm]] <- factor(ref_df[[nm]], levels = levels(X[[nm]]))
    else if (is.numeric(X[[nm]])) ref_df[[nm]] <- as.numeric(ref_df[[nm]])
    else if (is.character(X[[nm]])) ref_df[[nm]] <- as.character(ref_df[[nm]])
  }
  
  x_min <- suppressWarnings(min(X[[focalVariable]], na.rm = TRUE))
  x_max <- suppressWarnings(max(X[[focalVariable]], na.rm = TRUE))
  if (!is.finite(x_min) || !is.finite(x_max) || x_min == x_max) {
    stop("Cannot construct gradient for '", focalVariable, "': invalid/constant range in XData.")
  }
  grid <- seq(x_min, x_max, length.out = n_points)
  
  XDataNew <- ref_df[rep(1, n_points), , drop = FALSE]
  XDataNew[[focalVariable]] <- grid
  
  XDataNew
}

###############################################################################
# Trait helpers: LIFE_FORM → 0/1 indicator per group (exclude missing)
###############################################################################

get_group_indicator_from_TrData <- function(
    m, trait_name, group_levels, match_mode = c("contains", "exact"), verbose = FALSE
) {
  match_mode <- match.arg(match_mode)
  
  if (is.null(m$TrData) || !is.data.frame(m$TrData)) {
    stop("m$TrData is missing or not a data.frame.")
  }
  sp <- colnames(m$Y)
  if (is.null(sp)) stop("m$Y has no column names; cannot align species to traits.")
  if (!(trait_name %in% names(m$TrData))) {
    stop("Trait column '", trait_name, "' not found in m$TrData. Available: ",
         paste(names(m$TrData), collapse = ", "))
  }
  
  tr <- m$TrData[[trait_name]]
  if (is.factor(tr)) tr <- as.character(tr)
  tr <- trimws(tr)
  
  if (!is.null(rownames(m$TrData)) && all(sp %in% rownames(m$TrData))) {
    tr_aligned <- tr[sp]  # align by rownames (your case)
  } else {
    stop("Species IDs in m$Y do not match rownames(m$TrData).")
  }
  
  tr_low   <- tolower(tr_aligned)
  groups_l <- tolower(group_levels)
  
  in_group <- if (match_mode == "exact") {
    tr_low %in% groups_l
  } else {
    pattern <- paste(groups_l, collapse = "|")
    ifelse(!is.na(tr_low), grepl(pattern, tr_low), FALSE)
  }
  
  is_missing <- is.na(tr_low) | tr_low == ""
  indicator  <- ifelse(is_missing, NA_real_, ifelse(in_group, 1.0, 0.0))
  
  if (verbose && any(is_missing)) {
    message("Missing ", trait_name, " for species (excluded): ",
            paste(sp[is_missing], collapse = ", "))
  }
  indicator
}

compute_predicted_proportion_masked <- function(predY_sample, indicator) {
  mask <- !is.na(indicator)
  if (!any(mask)) return(rep(NA_real_, nrow(predY_sample)))  # empty denominator
  P   <- predY_sample[, mask, drop = FALSE]
  ind <- indicator[mask]
  numer <- as.numeric(P %*% matrix(ind, ncol = 1))  # sum_j P_ij * ind_j
  denom <- rowSums(P)
  denom <- pmax(denom, .Machine$double.eps)
  numer / denom
}

###############################################################################
# Curves: dual-group proportions per covariate (submersed & floating)
###############################################################################

get_dual_group_proportion_curves_for_covariate <- function(
    covariate, fitted_models, pool_order, trait_name = "LIFE_FORM",
    group_levels_sub   = c("submersed", "submerged"),
    group_levels_float = c("floating", "floating-leaved"),
    match_mode = c("contains", "exact"), verbose = FALSE,
    n_points = 50
) {
  match_mode <- match.arg(match_mode)
  qprop_sub_per_pool    <- vector("list", length(pool_order))
  qprop_float_per_pool  <- vector("list", length(pool_order))
  gradientvals_per_pool <- vector("list", length(pool_order))
  
  for (n in seq_along(pool_order)) {
    e <- new.env()
    load(fitted_models[n], envir = e)
    m <- e$models[[1]]
    
    # Build XDataNew and dfPiNew (matching studyDesign)
    XDataNew <- build_XDataNew(m, focalVariable = covariate, n_points = n_points)
    dfPiNew  <- build_dfPiNew(m, n_points = n_points)  # may be NULL if no random levels
    
    # Try direct predict call; fallback to Gradient if needed
    predY <- try(
      predict(m, XData = XDataNew, dfPiNew = dfPiNew, expected = TRUE),
      silent = TRUE
    )
    if (inherits(predY, "try-error")) {
      Gradient <- list(XDataNew = XDataNew, dfPiNew = dfPiNew)
      predY <- predict(m, Gradient = Gradient, expected = TRUE)
    }
    
    # Indicators (0/1/NA) aligned via rownames
    ind_sub <- get_group_indicator_from_TrData(
      m, trait_name = trait_name, group_levels = group_levels_sub,
      match_mode = match_mode, verbose = verbose
    )
    ind_float <- get_group_indicator_from_TrData(
      m, trait_name = trait_name, group_levels = group_levels_float,
      match_mode = match_mode, verbose = verbose
    )
    
    # Site-level proportions across posterior samples
    prop_sub_matrix   <- sapply(predY, function(P) compute_predicted_proportion_masked(P, ind_sub))
    prop_float_matrix <- sapply(predY, function(P) compute_predicted_proportion_masked(P, ind_float))
    
    # Posterior median per gradient row
    qprop_sub_per_pool[[n]]    <- apply(prop_sub_matrix,   1, median, na.rm = TRUE)
    qprop_float_per_pool[[n]]  <- apply(prop_float_matrix, 1, median, na.rm = TRUE)
    
    # Gradient x-values (the focal covariate)
    gradientvals_per_pool[[n]] <- XDataNew[[covariate]]
  }
  
  list(
    qprop_sub_per_pool    = qprop_sub_per_pool,
    qprop_float_per_pool  = qprop_float_per_pool,
    gradientvals_per_pool = gradientvals_per_pool
  )
}

###############################################################################
# Plotting: 4 panels, dual overlays (submersed = solid; floating = dashed)
###############################################################################
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

for (i in seq_along(covariates)) {
  covariate <- covariates[i]
  
  # Add tryCatch so one problematic pool doesn't stop the whole figure
  curves <- tryCatch({
    get_dual_group_proportion_curves_for_covariate(
      covariate          = covariate,
      fitted_models      = fitted_models,
      pool_order         = pool_names_loaded,
      trait_name         = trait_name,
      group_levels_sub   = group_levels_sub,
      group_levels_float = group_levels_float,
      match_mode         = "contains",
      verbose            = FALSE,
      n_points           = 50
    )
  }, error = function(e) {
    message("Curve building failed for covariate ", covariate, ": ", conditionMessage(e))
    NULL
  })
  
  # Panel setup
  plot(NA, NA, xlab = covariate,
       ylab = paste0("Predicted proportion (", trait_name, " ∈ {",
                     paste(unique(c(group_levels_sub, group_levels_float)), collapse = ", "), "})"),
       xlim = if (!is.null(curves)) range(unlist(curves$gradientvals_per_pool), na.rm = TRUE) else c(0,1),
       ylim = fixed_ylim)
  
  drew_any <- FALSE
  if (!is.null(curves)) {
    q_sub   <- curves$qprop_sub_per_pool
    q_float <- curves$qprop_float_per_pool
    x_list  <- curves$gradientvals_per_pool
    
    # Helper: draw line only when finite points exist; optionally spline
    draw_line <- function(x, y, col, lwd, lty, spar = 0.6) {
      ok <- is.finite(x) & is.finite(y)
      if (sum(ok) >= 2) {
        if (use_smoothing && length(unique(x[ok])) >= 4 && sum(ok) >= 4) {
          ss <- try(smooth.spline(x[ok], y[ok], spar = spar), silent = TRUE)
          if (!inherits(ss, "try-error")) {
            lines(ss$x, ss$y, col = col, lwd = lwd, lty = lty)
            return(TRUE)
          }
        }
        lines(x[ok], y[ok], col = col, lwd = lwd, lty = lty)
        return(TRUE)
      }
      FALSE
    }
    
    for (n in seq_along(pool_names_loaded)) {
      lbl <- pool_names_loaded[n]
      x <- x_list[[n]]
      y_sub   <- q_sub[[n]]
      y_float <- q_float[[n]]
      
      drew_any <- draw_line(x, y_sub,   colours[[lbl]], 6, group_lty["submersed"]) || drew_any
      drew_any <- draw_line(x, y_float, colours[[lbl]], 6, group_lty["floating"])  || drew_any
    }
  }
  
  # Panel letter
  mtext(panel_letters[i], side = 3, adj = 0, line = 0.2, cex = 1.1, font = 2)
  
  # Legends (only in first panel)
  if (i == 1) {
    legend("topright",
           legend = desired_order,
           fill   = colours[desired_order],
           pt.cex = 2,
           cex    = 1.1,
           bty    = "n")
    legend("bottomright",
           legend = c("submersed", "floating"),
           lty    = c(group_lty["submersed"], group_lty["floating"]),
           lwd    = 6,
           col    = rep("grey20", 2),
           cex    = 1.0,
           bty    = "n")
  }
  
  if (!drew_any) {
    mtext("No data (all NA/empty)", side = 3, adj = 1, line = -1, cex = 0.8, col = "grey40")
  }
} 





# chatgpt attempt----------


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

trait_lty <- c(
  "submersed" = 1,  # solid
  "floating"  = 2   # dashed
)
  
# Fixed y-limits across all panels
fixed_ylim <- c(0, 1)  # proportions

# Panel letters (A–D)
panel_letters <- c("(A)", "(B)", "(C)", "(D)")

# Toggle smoothing (TRUE for smooth spline, FALSE for raw line)
use_smoothing <- TRUE

# compute curves for a covariate 
get_trait_prop_curves <- function(covariate, fitted_models, pool_order,
                                  trait_name = "LIFE_FORM",
                                  trait_levels = c("submersed", "floating")) {
  
  qprop_per_pool <- vector("list", length(pool_order))
  gradientvals_per_pool <- vector("list", length(pool_order))
  
  for (n in seq_along(pool_order)) {
    e <- new.env()
    load(fitted_models[n], envir = e)
    m <- e$models[[1]]
    
    trait_vec <- m$trData[[trait_name]]
    
    Gradient <- constructGradient(
      m,
      focalVariable = covariate,
      non.focalVariables = 1
    )
    
    predY <- predict(m, Gradient = Gradient, expected = TRUE)
    # predY is a LIST of matrices: gradient × species
    
    # Total community prediction (for proportions)
    pred_total <- do.call(cbind, lapply(predY, rowSums))
    
    # Trait-specific sums
    trait_sums <- lapply(trait_levels, function(tr) {
      spp_idx <- which(trait_vec == tr)
      do.call(cbind, lapply(predY, function(mat) {
        rowSums(mat[, spp_idx, drop = FALSE])
      }))
    })
    
    # Convert to proportions
    trait_props <- lapply(trait_sums, function(ts) ts / pred_total)
    
    # Posterior medians (graphics-friendly numeric vectors)
    qprop_per_pool[[n]] <- lapply(trait_props, function(tp) {
      apply(tp, 1, median, na.rm = TRUE)
    })
    
    gradientvals_per_pool[[n]] <- Gradient$XDataNew[, 1]
  }
  
  list(
    qprop_per_pool = qprop_per_pool,
    gradientvals_per_pool = gradientvals_per_pool
  )
}
      
      
    

#  4-panel figure (each panel = one covariate, labeled A–D) 
fixed_ylim <- c(0, 1)

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

for (i in seq_along(covariates)) {
  
  curves <- get_trait_prop_curves(
    covariates[i],
    fitted_models,
    pool_names_loaded
  )
  
  qprop_per_pool       <- curves$qprop_per_pool
  gradientvals_per_pool <- curves$gradientvals_per_pool
  
  x_all <- unlist(gradientvals_per_pool)
  plot(NA, NA,
       xlab = covariates[i],
       ylab = "Predicted proportion of LIFE_FORM",
       xlim = range(x_all, na.rm = TRUE),
       ylim = fixed_ylim)
  
  for (n in seq_along(pool_names_loaded)) {
    lbl <- pool_names_loaded[n]
    x   <- gradientvals_per_pool[[n]]
    
    for (tr in names(trait_lty)) {
      y <- qprop_per_pool[[n]][[tr]]
      
      ok <- is.finite(x) & is.finite(y)
      if (use_smoothing && sum(ok) >= 4) {
        ss <- smooth.spline(x[ok], y[ok], spar = 0.6)
        lines(ss$x, ss$y,
              col = colours[lbl],
              lwd = 4,
              lty = trait_lty[tr])
      } else {
        lines(x[ok], y[ok],
              col = colours[lbl],
              lwd = 4,
              lty = trait_lty[tr])
      }
    }
  }
  
  mtext(panel_letters[i], side = 3, adj = 0, line = 0.2,
        cex = 1.1, font = 2)
  
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
  
  if (i == 1) {
    legend("topleft",
           legend = names(trait_lty),
           lty    = trait_lty,
           lwd    = 4,
           col    = "black",
           title  = "Life form",
           bty    = "n")
  }



