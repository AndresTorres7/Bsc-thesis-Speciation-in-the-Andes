## =============================================================================
## STEP 2 (CONCAVE POLYGONS): SPATIALLY CONSTRAINED RANGE APPROXIMATION
## =============================================================================
## For each species, a concave polygon is generated from its cleaned
## occurrence records using the 'concaveman' package, with the concavity
## parameter set to 3. This gives a spatially constrained approximation of
## a species' distribution based on known occurrences, reducing
## overestimation relative to a convex hull. Each polygon is then
## rasterized (0.05 decimal degrees per cell -- roughly 5 km north-south
## everywhere, narrowing east-west with distance from the equator, the same
## way the earlier M buffer's degree-based distance behaves) to match the
## spatial format required for the range-overlap and phylogenetic-structure
## analyses.
##
## The resulting binary raster ("polygonModel.tif") is written into each
## species' Final_Models folder, alongside the MaxEnt-based binary model
## from the potential-distribution-modeling script, and all species in a
## genus are collected into a single "<genus>_polirasters.rda" file -- an
## alternative spatial representation of the same species set, for the
## comparative analyses following Skeels and Cardillo (2019).
## =============================================================================


## -----------------------------------------------------------------------
## 0. SETUP
## -----------------------------------------------------------------------

library(sf)
library(concaveman)
library(raster)

# --- EDIT THESE FOR YOUR OWN MACHINE --------------------------------------
basepath <- "path/to/your/project"
occ_dir  <- file.path(basepath, "Occurrence_data")   # per-genus/per-species folders
rda_dir  <- file.path(basepath, "polirdaS")          # one .rda per genus, this script's output
# ---------------------------------------------------------------------------

cell_res <- 0.05  # decimal degrees per raster cell (see header note on real-world distance)

dir.create(rda_dir, showWarnings = FALSE, recursive = TRUE)

genus_list <- list.dirs(occ_dir, recursive = FALSE, full.names = FALSE)


## -----------------------------------------------------------------------
## 1. BUILD CONCAVE POLYGONS AND RASTERIZE THEM, GENUS BY GENUS
## -----------------------------------------------------------------------

for (genus_name in genus_list) {

  genus_dir <- file.path(occ_dir, genus_name)
  # list.dirs (not dir()) -- a genus folder also contains "sppList.csv",
  # which dir() would wrongly treat as a species
  spplist <- list.dirs(genus_dir, recursive = FALSE, full.names = FALSE)

  ## ---- read every species' occurrence points once, reused below ---------
  occ_by_species <- list()
  df <- data.frame()
  for (sp in spplist) {
    join_file <- list.files(file.path(genus_dir, sp), pattern = "_join\\.csv$", full.names = TRUE)
    if (length(join_file) == 0) next
    sp_df <- read.csv(join_file[1])
    occ_by_species[[sp]] <- sp_df
    df <- rbind(df, sp_df)
  }
  if (nrow(df) == 0) {
    cat("Skipping genus", genus_name, ": no occurrence data found\n")
    next
  }

  ## ---- genus-wide extent, so every species' raster shares the same ------
  ## ---- alignment (same approach as the MaxEnt-binarization stage) -------
  pts_all    <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
  genus_poly <- concaveman(pts_all, concavity = 3)
  pla        <- raster::extent(genus_poly)

  ## ---- build and rasterize each species' own concave polygon ------------
  k <- list()
  kept_names <- character(0)

  for (sp in spplist) {

    if (is.null(occ_by_species[[sp]])) {
      cat("Skipping", genus_name, "-", sp, ": no occurrence data found\n")
      next
    }

    r_mask <- tryCatch({

      pts     <- st_as_sf(occ_by_species[[sp]], coords = c("longitude", "latitude"), crs = 4326)
      sp_poly <- concaveman(pts, concavity = 3)

      r   <- raster(raster::extent(sp_poly), res = cell_res, crs = "+proj=longlat +datum=WGS84")
      out <- rasterize(as_Spatial(sp_poly), r, field = 1)
      out <- raster::extend(out, pla, NA)

      # if the polygon is small enough to fall entirely between cell
      # centers, force at least one pixel to 1 so the species isn't lost
      # at this resolution
      if (all(is.na(values(out)))) {
        centroid <- st_centroid(st_geometry(sp_poly))
        cell <- raster::cellFromXY(out, st_coordinates(centroid))
        if (!is.na(cell)) out[cell] <- 1
      }
      out

    }, error = function(e) {
      cat("Skipping", genus_name, "-", sp, ": concaveman/rasterize failed (", conditionMessage(e), ")\n")
      NULL
    })

    if (is.null(r_mask) || all(is.na(values(r_mask)))) {
      cat("Skipping", genus_name, "-", sp, ": no usable polygon at this resolution",
          "(likely too few or too tightly clustered occurrence points)\n")
      next
    }

    fm_dir <- file.path(genus_dir, sp, "Final_Models")
    dir.create(fm_dir, showWarnings = FALSE, recursive = TRUE)
    writeRaster(r_mask, file.path(fm_dir, "polygonModel.tif"), overwrite = TRUE)

    modelbinar <- readAll(raster(file.path(fm_dir, "polygonModel.tif")))
    k[[length(k) + 1]] <- modelbinar
    kept_names <- c(kept_names, sp)

    cat("Concave polygon rasterized for", genus_name, "-", sp, "\n")
  }

  if (length(k) == 0) {
    cat("No polygon rasters to save for genus", genus_name, "\n")
    next
  }

  names(k) <- kept_names
  save(k, file = file.path(rda_dir, paste0(genus_name, "_polirasters.rda")))
  cat("Saved", genus_name, "_polirasters.rda with", length(k), "species\n")
}
