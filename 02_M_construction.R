## =============================================================================
## STEP 2 (M CONSTRUCTION): ACCESSIBLE AREA (M) FOR EACH SPECIES
## =============================================================================
##
## For every species, this script:
##   1. Reads its "..._join.csv" occurrence file (columns: species,
##      longitude, latitude).
##   2. Buffers those points with gBuffer(width = 2).
##   3. Masks and crops the 3 environmental PCA rasters to that buffer.
##   4. Writes the 3 resulting rasters as ASCII grids -- the M used as the
##      calibration/projection area for the MaxEnt models run with 'kuenm'.
## =============================================================================


## -----------------------------------------------------------------------
## 0. SETUP
## -----------------------------------------------------------------------

library(rgeos)
library(raster)

# --- EDIT THESE FOR YOUR OWN MACHINE --------------------------------------
basepath <- "path/to/your/project"
occ_dir  <- file.path(basepath, "Occurrence_data")     
pca_dir  <- file.path(basepath, "Worldclim/Origin")    # folder containing PC1.tif, PC2.tif, PC3.tif
# ---------------------------------------------------------------------------

buffer_width <- 2 
utm_crs <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs"

## ---- Load the 3 environmental PCA layers --------------------------------
map1 <- raster(file.path(pca_dir, "PC1.tif"))
map2 <- raster(file.path(pca_dir, "PC2.tif"))
map3 <- raster(file.path(pca_dir, "PC3.tif"))
crs(map1) <- utm_crs
crs(map2) <- utm_crs
crs(map3) <- utm_crs
maps <- c(map1, map2, map3)


## -----------------------------------------------------------------------
## 1. BUILD M FOR EVERY SPECIES IN EVERY CANDIDATE GENUS
## -----------------------------------------------------------------------


genus_list <- list.dirs(occ_dir, recursive = FALSE, full.names = FALSE)

for (genus_name in genus_list) {

  genus_dir <- file.path(occ_dir, genus_name)
  spplist <- list.dirs(genus_dir, recursive = FALSE, full.names = FALSE)

  for (sp in spplist) {

    species_dir <- file.path(genus_dir, sp)
    out_dir     <- file.path(species_dir, "M", "Set_1")

    # --- resume-safe: skip species already processed -----------------------
    if (file.exists(file.path(out_dir, "PC3.asc"))) next

    # --- need cleaned occurrence data for this species ----------------------
    join_file <- list.files(species_dir, pattern = "_join\\.csv$", full.names = TRUE)
    if (length(join_file) == 0) {
      print(paste(sp, "NoData", sep = "_"))
      next
    }

    Occurencedata <- read.csv(join_file[1])
    cordi  <- Occurencedata[, c(2, 3)]   # longitude, latitude columns
    spData <- SpatialPointsDataFrame(cordi, Occurencedata)
    crs(spData) <- utm_crs

    ## ---- buffer ------------------------
    buferr <- gBuffer(spData, width = buffer_width)

    ## ---- mask and crop the 3 PCA layers to the buffer ---------------------
    mascas  <- lapply(X = maps, FUN = mask, mask = buferr)
    maskis  <- lapply(mascas, crop, y = buferr)

    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    writeRaster(maskis[[1]], file.path(out_dir, "PC1.asc"), format = "ascii", datatype = "INT4S", overwrite = TRUE)
    writeRaster(maskis[[2]], file.path(out_dir, "PC2.asc"), format = "ascii", datatype = "INT4S", overwrite = TRUE)
    writeRaster(maskis[[3]], file.path(out_dir, "PC3.asc"), format = "ascii", datatype = "INT4S", overwrite = TRUE)

    cat("M built for", genus_name, "-", sp, "\n")
  }
}
