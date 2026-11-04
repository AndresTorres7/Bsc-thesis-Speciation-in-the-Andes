## =============================================================================
## STEP 3: POTENTIAL DISTRIBUTION MODELS (MaxEnt via 'kuenm')
## =============================================================================
## Potential distribution models are calibrated using Maxent through the R
## package 'kuenm'. For each species, candidate models are built from five
## feature-class combinations (L, LQ, LQP, LQPT, LQPTH) crossed with
## regularization multipliers from 0.1 to 1 (by 0.1) and 2 to 5 (by 1).
## Candidate models are then evaluated on statistical significance, omission
## rate, and AICc. The M built earlier (a buffer around each species'
## occurrence records) is used as both the calibration and projection
## extent, so no separate G (transfer) area is needed.
##
## This follows the standard kuenm_start()-style structure, in four stages:
##   STAGE 1 -- kuenm_cal:   candidate model calibration
##   STAGE 2 -- kuenm_ceval: candidate model evaluation or selection
##   STAGE 3 -- kuenm_mod:   final model creation
##   STAGE 4 -- binarization of each final model using the minimum training
##              presence logistic threshold, with the resulting rasters
##              saved as one .rda per genus (a named list of binary
##              RasterLayers, one per species), matching the input format
##              used by the geographic speciation mode analyses of Skeels
##              and Cardillo (2019).
##
## Between Stage 2 and Stage 3, kuenm_ceval writes a "selected_models.txt"
## file (in each species' Calibration_results folder) listing the
## significant, low-omission candidate models within 2 AICc units of the
## best one. Choosing a single final model among those ties by simplest
## parametrization is done by hand, by editing that file before running
## Stage 3 -- this step is intentionally not automated here.
## =============================================================================


## -----------------------------------------------------------------------
## 0. SETUP
## -----------------------------------------------------------------------

library(kuenm)
library(raster)
library(sf)

# --- EDIT THESE FOR YOUR OWN MACHINE --------------------------------------
basepath    <- "path/to/your/project"
occ_dir     <- file.path(basepath, "Occurrence_data")  # per-genus/per-species folders
maxent_path <- "path/to/folder/containing/maxent.jar"
rda_dir     <- file.path(basepath, "rdaS")             # one .rda per genus, Stage 4 output
# ---------------------------------------------------------------------------

# Run/skip switches for each stage, so Stage 3 can be run separately after
# the manual review of "selected_models.txt" described above
run_stage1 <- TRUE
run_stage2 <- TRUE
run_stage3 <- TRUE
run_stage4 <- TRUE

genus_list <- list.dirs(occ_dir, recursive = FALSE, full.names = FALSE)


## -----------------------------------------------------------------------
## STAGE 1 -- CANDIDATE MODEL CALIBRATION (kuenm_cal)
## -----------------------------------------------------------------------
## Five feature-class combinations (L, LQ, LQP, LQPT, LQPTH; kuenm's "basic"
## preset) crossed with regularization multipliers 0.1-1 (by 0.1) and 2-5
## (by 1).

if (run_stage1) {

  reg_mult <- c(seq(0.1, 1, 0.1), seq(2, 5, 1))
  f_clas   <- "basic"   # = "l", "lq", "lqp", "lqpt", "lqpth"

  batch_cal <- "Candidate_models"
  out_dir   <- "Candidate_Models"

  for (genus_name in genus_list) {

    genus_dir <- file.path(occ_dir, genus_name)
    spplist   <- list.dirs(genus_dir, recursive = FALSE, full.names = FALSE)

    for (sp in spplist) {

      species_dir <- file.path(genus_dir, sp)

      occ_joint <- list.files(species_dir, pattern = "_join\\.csv$")
      occ_tra   <- list.files(species_dir, pattern = "_train\\.csv$")
      M_var_dir <- "M"

      if (length(occ_joint) == 0 || length(occ_tra) == 0 || !dir.exists(file.path(species_dir, M_var_dir))) {
        cat("Skipping", genus_name, "-", sp, ": missing occurrence data or M\n")
        next
      }

      old_wd <- getwd()
      setwd(species_dir)

      kuenm_cal(occ.joint = occ_joint, occ.tra = occ_tra, M.var.dir = M_var_dir,
                batch = batch_cal, out.dir = out_dir, reg.mult = reg_mult, f.clas = f_clas,
                args = NULL, maxent.path = maxent_path, wait = FALSE, run = TRUE)

      setwd(old_wd)
      cat("Candidate models calibrated for", genus_name, "-", sp, "\n")
    }
  }
}


## -----------------------------------------------------------------------
## STAGE 2 -- CANDIDATE MODEL EVALUATION / SELECTION (kuenm_ceval)
## -----------------------------------------------------------------------
## Evaluates every candidate model on statistical significance (partial
## ROC), omission rate (5% threshold), and AICc. selection = "AICc" flags
## all significant, low-omission models within 2 AICc units of the lowest
## AICc model -- ties among those are resolved by hand afterward (see the
## note at the top of this script).

if (run_stage2) {

  batch_cal <- "Candidate_models"
  out_dir   <- "Candidate_Models"
  out_eval  <- "Calibration_results"

  threshold    <- 5     # omission-rate threshold (%)
  rand_percent <- 50    # bootstrap resampling percentage for partial ROC
  iterations   <- 500
  kept         <- TRUE
  selection    <- "AICc"

  for (genus_name in genus_list) {

    genus_dir <- file.path(occ_dir, genus_name)
    spplist   <- list.dirs(genus_dir, recursive = FALSE, full.names = FALSE)

    for (sp in spplist) {

      species_dir <- file.path(genus_dir, sp)

      occ_joint <- list.files(species_dir, pattern = "_join\\.csv$")
      occ_tra   <- list.files(species_dir, pattern = "_train\\.csv$")
      occ_test  <- list.files(species_dir, pattern = "_test\\.csv$")

      if (length(occ_joint) == 0 || length(occ_tra) == 0 || length(occ_test) == 0 ||
          !dir.exists(file.path(species_dir, out_dir))) {
        cat("Skipping", genus_name, "-", sp, ": missing occurrence data or candidate models\n")
        next
      }

      old_wd <- getwd()
      setwd(species_dir)

      kuenm_ceval(path = out_dir, occ.joint = occ_joint, occ.tra = occ_tra, occ.test = occ_test,
                  batch = batch_cal, out.eval = out_eval, threshold = threshold,
                  rand.percent = rand_percent, iterations = iterations, kept = kept,
                  selection = selection)

      setwd(old_wd)
      cat("Candidate models evaluated for", genus_name, "-", sp, "\n")
    }
  }

  cat("\nStage 2 complete. Before running Stage 3, review each species'\n",
      "Calibration_results/selected_models.txt and, where more than one\n",
      "model is listed (ties within 2 AICc units), keep only the model with\n",
      "the simplest parametrization.\n")
}


## -----------------------------------------------------------------------
## STAGE 3 -- FINAL MODEL CREATION (kuenm_mod)
## -----------------------------------------------------------------------
## Builds the final model(s) using the parametrization(s) left in each
## species' "selected_models.txt" after the manual review above. M is used
## as both the calibration and projection extent, so project = FALSE (no
## separate G-area transfer).

if (run_stage3) {

  out_eval  <- "Calibration_results"
  batch_fin <- "Final_models"
  mod_dir   <- "Final_Models"

  rep_n      <- 10   # number of replicates -- not specified in the text; adjust as needed
  rep_type   <- "Bootstrap"
  jackknife  <- FALSE
  out_format <- "logistic"
  project    <- FALSE   # M is also the projection area -- no separate G needed

  for (genus_name in genus_list) {

    genus_dir <- file.path(occ_dir, genus_name)
    spplist   <- list.dirs(genus_dir, recursive = FALSE, full.names = FALSE)

    for (sp in spplist) {

      species_dir <- file.path(genus_dir, sp)

      occ_joint <- list.files(species_dir, pattern = "_join\\.csv$")
      M_var_dir <- "M"
      selected_file <- file.path(species_dir, out_eval, "selected_models.txt")

      if (length(occ_joint) == 0 || !file.exists(selected_file)) {
        cat("Skipping", genus_name, "-", sp, ": no selected_models.txt yet",
            "(complete the manual review described in Stage 2 first)\n")
        next
      }

      old_wd <- getwd()
      setwd(species_dir)

      kuenm_mod(occ.joint = occ_joint, M.var.dir = M_var_dir, out.eval = out_eval,
                batch = batch_fin, rep.n = rep_n, rep.type = rep_type, jackknife = jackknife,
                out.dir = mod_dir, out.format = out_format, project = project,
                maxent.path = maxent_path, args = NULL, wait = FALSE, run = TRUE)

      setwd(old_wd)
      cat("Final model created for", genus_name, "-", sp, "\n")
    }
  }
}


## -----------------------------------------------------------------------
## STAGE 4 -- BINARIZE FINAL MODELS AND SAVE ONE .rda PER GENUS
## -----------------------------------------------------------------------
## For each species, the continuous suitability raster ("_avg.asc", the
## across-replicate average) is thresholded at the minimum training
## presence logistic value from Maxent's own maxentResults.csv, giving a
## binary presence/absence raster. Each species' binary raster is then
## aggregated (6x6 blocks, keeping a cell if any of the 6x6 source cells
## was suitable), extended to a common genus-wide extent (a buffer around
## every occurrence record in the genus, so all species in a genus share
## the same alignment), and collected into a single named list -- one
## element per species -- saved as "<genus>_rasters.rda". This matches the
## input format expected by the geographic speciation mode analyses of
## Skeels and Cardillo (2019).

if (run_stage4) {

  dir.create(rda_dir, showWarnings = FALSE, recursive = TRUE)

  for (genus_name in genus_list) {

    genus_dir <- file.path(occ_dir, genus_name)
    spplist   <- list.dirs(genus_dir, recursive = FALSE, full.names = FALSE)

    ## ---- genus-wide extent, from every species' occurrence records ------
    df <- data.frame()
    for (sp in spplist) {
      join_file <- list.files(file.path(genus_dir, sp), pattern = "_join\\.csv$", full.names = TRUE)
      if (length(join_file) == 0) next
      df <- rbind(df, read.csv(join_file[1]))
    }
    if (nrow(df) == 0) {
      cat("Skipping genus", genus_name, ": no occurrence data found\n")
      next
    }

    sf_pts <- st_as_sf(df, coords = c(2, 3), crs = 4326)
    poly   <- st_buffer(sf_pts, dist = 2)
    pla    <- raster::extent(poly)

    ## ---- binarize each species' final model ------------------------------
    k <- list()
    kept_names <- character(0)

    for (sp in spplist) {

      fm_dir <- file.path(genus_dir, sp, "Final_Models")
      if (!dir.exists(fm_dir)) {
        cat("Skipping", genus_name, "-", sp, ": no Final_Models\n")
        next
      }

      models <- list.dirs(fm_dir, recursive = FALSE, full.names = FALSE)
      if (length(models) == 0) {
        cat("Skipping", genus_name, "-", sp, ": Final_Models is empty\n")
        next
      }
      if (length(models) > 1) {
        warning("Genus '", genus_name, "', species '", sp, "': more than one model folder ",
                "found in Final_Models (", paste(models, collapse = ", "), "). Using '",
                models[1], "' -- confirm only one final model was selected for this species.")
      }
      model_dir <- file.path(fm_dir, models[1])

      avg_file <- list.files(model_dir, pattern = "_avg\\.asc$", full.names = TRUE)
      if (length(avg_file) == 0) {
        cat("Skipping", genus_name, "-", sp, ": no _avg.asc found\n")
        next
      }
      model <- raster(avg_file[1])

      maxent  <- read.csv(file.path(model_dir, "maxentResults.csv"))
      mtp_col <- grep("Minimum.training.presence.*Logistic.threshold", colnames(maxent), ignore.case = TRUE)
      if (length(mtp_col) == 0) {
        cat("Skipping", genus_name, "-", sp, ": couldn't find the minimum training presence",
            "logistic threshold column in maxentResults.csv\n")
        next
      }
      maxo <- maxent[nrow(maxent), mtp_col[1]]  # last row = the across-replicate summary row

      bina <- calc(model, fun = function(x) ifelse(x >= maxo, 1, NA))
      bina <- raster::aggregate(bina, fact = 6, fun = max, na.rm = TRUE)
      bina <- raster::extend(bina, pla, NA)
      crs(bina) <- "EPSG:4326"

      writeRaster(bina, file.path(fm_dir, "modelofinal.tif"), overwrite = TRUE)

      modelbinar <- readAll(raster(file.path(fm_dir, "modelofinal.tif")))
      k[[length(k) + 1]] <- modelbinar
      kept_names <- c(kept_names, sp)

      cat("Binarized model for", genus_name, "-", sp, "\n")
    }

    if (length(k) == 0) {
      cat("No binarized models to save for genus", genus_name, "\n")
      next
    }

    names(k) <- kept_names
    save(k, file = file.path(rda_dir, paste0(genus_name, "_rasters.rda")))
    cat("Saved", genus_name, "_rasters.rda with", length(k), "species\n")
  }
}
