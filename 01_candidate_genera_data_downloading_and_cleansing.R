## =============================================================================
## CANDIDATE ANDEAN GENERA: SELECTION, GBIF DOWNLOAD/CLEANING, AND FILTERING
## =============================================================================
##
##   STAGE 1 -- candidate genera with > 12 species
##              
##   STAGE 2 -- remove non-target higher taxa (e.g. fishes)
##             
##   STAGE 3 -- download, clean, thin, and 50/50 split occurrence records
##              
##              --> THE MAIN OUTPUT OF THIS SCRIPT: for every retained
##                  species, a "_join.csv", "_train.csv" and "_test.csv",
##                  ready to be used as Occurrences input for MaxEnt models
##                  run through the 'kuenm' package.
##   STAGE 4 -- Andean-centredness / sampling-fraction filter
##              
##
## Manual/external step this script assumes has already been done:
##   The raw GBIF download itself. Query GBIF.org with occurrenceStatus =
##   "PRESENT", hasCoordinate = TRUE, continent = "South America", and a
##   geometry filter using the GMBA Andes polygon (https://www.gmba.unibe.ch/).
##   Save the Darwin Core export (tab-separated) as "rawspecieslist.csv".
## =============================================================================


## -----------------------------------------------------------------------
## 0. SETUP
## -----------------------------------------------------------------------

library(rgbif)
library(readxl)
library(writexl)
library(dplyr)
# 'ellipsenm' is GitHub-only: remotes::install_github("marlonecobos/ellipsenm")
library(ellipsenm)
library(ape)

# Set to TRUE to parallelise the GBIF query loops with doParallel/foreach, as
# the scripts were run on the lab server (makeCluster(20, type="FORK") /
# makeCluster(57)). FORK clusters only work on Linux/Mac.
use_parallel <- FALSE
if (use_parallel) {
  library(doParallel)
  n_cores <- 20
  myCluster <- makeCluster(n_cores, type = "FORK")
  registerDoParallel(myCluster)
}

# --- EDIT THIS FOR YOUR OWN MACHINE --------------------------------------
basepath <- "path/to/your/project"
# normalizePath() makes 'basepath' absolute so that later calls to
# file.path(basepath, ...) still resolve correctly after the setwd() below
# changes the working directory (a relative basepath would otherwise get
# silently re-interpreted relative to itself).
basepath <- normalizePath(basepath, mustWork = FALSE)
setwd(basepath)
occ_dir  <- file.path(basepath, "Occurrence_data")
trees_dir <- file.path(basepath, "trees")  # folder of manually downloaded TimeTree Newick files
dir.create(occ_dir, showWarnings = FALSE, recursive = TRUE)
# ---------------------------------------------------------------------------

# Polygon covering continental South America (used to clip
# GBIF queries and discard obvious oceanic outliers)

poli <-"POLYGON((-77.01855 10.14697,-84.14209 16.27075,-87.8335 16.552,  
                  -86.25586 22.54394,-95.25586 19.73144,-96.5918 27.39551,-87.87304 29.64551,-79.65527 25.12353,-80.70117 31.05176,-59.18555 46.87207,
                  -52.36523 46.73144,-51.67969 51.08203,-62.92969 62.05077,-75.30467 75.26953,-59.27343 69.36328,-47.46094 55.86328,-39.58593 62.89453,
                  -23.83594 67.39453,-8.64841 83.98828,-58.99219 84.83203,-93.58592 83.42578,-120.86719 79.76953,-131.44043 70.40478,-150.74121 71.14307,
                  -153.20215 58.979,-142.65527 59.40088,-130.7373 52.36963,-128.13574 44.67041,-121.03418 34.229,-111.43652 22.52197,-107.63965 23.04932,
                  -106.19824 22.17041,-106.19824 20.69385,-104.40527 18.05713,-94.73731 14.75244,-88.02246 12.57275,-86.61621 10.07666,-82.39746 6.45557,
                  -78.81152 7.26416,-78.1084 3.67822,-80.6748 1.42822,-82.0459 -5.35693,-76.28027 -15.16553,-70.97168 -19.80615,-72.30762 -28.31397,
                  -76.24512 -45.15381,-76.13965 -53.69678,-68.44043 -56.61475,-64.6084 -55.17334,-67.63184 -51.13037,-60.74121 -41.91943,-39.8584 -23.28662,
                  -38.03027 -13.75928,-34.72558 -10.17334,-34.26855 -5.42725,-49.10449 3.25635,-60.31933 11.09619,-71.67481 13.27588,-77.01855 10.14697))"

## -----------------------------------------------------------------------
## HELPER FUNCTIONS
## -----------------------------------------------------------------------

# Accepted species (or subspecies) belonging to a genus, deduplicated by
# speciesKey. Used to build the species list to download for each genus
# (Stages 1 and 3).
get_species_list <- function(genus_key, ranks = c("SPECIES", "SUBSPECIES")) {
  z <- name_lookup(higherTaxonKey = genus_key, limit = 10000, hl = TRUE)
  z <- z[["data"]][, c("species", "rank", "taxonomicStatus", "synonym", "speciesKey")]
  z <- z[z$synonym == FALSE, -4]
  z <- z[z$taxonomicStatus == "ACCEPTED", -3]
  z <- z[z$rank %in% ranks, -2]
  z <- na.omit(z)
  z <- z %>% distinct(speciesKey, .keep_all = TRUE)
  z
}

# Total number of ACCEPTED, SPECIES-rank taxa in GBIF for a genus, counting
# only species with more than 2 georeferenced occurrences -- this is the
# denominator used for the "sampling fraction" criterion in Stage 4.
count_total_gbif_species <- function(genus_key) {
  sp <- name_lookup(higherTaxonKey = genus_key, limit = 10000, hl = TRUE, status = "ACCEPTED")
  sp <- sp[["data"]][, c("species", "rank", "taxonomicStatus", "synonym", "speciesKey")]
  sp <- sp[sp$synonym == FALSE, ]
  sp <- sp[sp$taxonomicStatus == "ACCEPTED", ]
  sp <- sp[sp$rank == "SPECIES", ]
  sp <- na.omit(sp)
  sp <- sp %>% distinct(speciesKey, .keep_all = TRUE)
  sp$numOcc <- vapply(sp$speciesKey, function(k) occ_count(taxonKey = k, georeferenced = TRUE), numeric(1))
  sum(sp$numOcc > 2)
}

# Clean a raw GBIF occurrence table for one species: keep the useful columns,
# drop NAs, restrict to reliable basisOfRecord types, drop (0,0) coordinates, and drop exact
# duplicates.
clean_species_occurrences <- function(raw_occ, species_name, min_year = NULL) {
  keep_cols <- c("scientificName", "decimalLongitude", "decimalLatitude", "basisOfRecord")
  if (!is.null(min_year)) keep_cols <- c(keep_cols, "year")
  keep_cols <- intersect(keep_cols, colnames(raw_occ))

  occ <- raw_occ[, keep_cols]
  occ <- na.omit(occ)

  basesofrecord <- c("HUMAN_OBSERVATION", "LITERATURE", "LIVING_SPECIMEN", "MACHINE_OBSERVATION",
                      "OBSERVATION", "PRESERVED_SPECIMEN", "OCCURRENCE")
  occ <- occ[occ$basisOfRecord %in% basesofrecord, ]
  occ$basisOfRecord <- NULL

  if (!is.null(min_year) && "year" %in% colnames(occ)) {
    occ <- occ[occ$year >= min_year, ]
    occ$year <- NULL
  }

  occ <- occ[occ$decimalLongitude != 0 & occ$decimalLatitude != 0, ]
  occ <- occ[!duplicated(paste(occ$decimalLongitude, occ$decimalLatitude)), ]

  names(occ) <- c("species", "longitude", "latitude")
  occ$species <- gsub(" ", "_", species_name)
  occ
}

# write.csv() crashes on list-typed columns (occasionally returned by
# occ_data()); lastfilter.R worked around this with apply(a, 2, as.character)
# -- kept here as a small helper so it isn't repeated at every call site.
safe_write_csv <- function(df, path) {
  write.csv(apply(df, 2, as.character), path, row.names = FALSE)
}


# Find and read the Newick tree for a genus (files named e.g.
# "Genus (Genus)_arbol" or "Genus (Genus-Family)_arbol", no extension
# required -- matching is done by substring/regex, exactly as in the
# 'trees' folder downloaded from timetree.org). Returns NULL if no tree is
# found for the genus.

get_genus_tree <- function(genus_name, trees_dir) {
  tree_files <- list.files(trees_dir, pattern = "_arbol")
  tree_genus_names <- gsub("_arbol", "", tree_files)
  tree_genus_names <- trimws(gsub(r"{\s*\([^\)]+\)}", "", tree_genus_names))
  match_file <- tree_files[tree_genus_names == genus_name]
  if (length(match_file) == 0) return(NULL)
  read.tree(file.path(trees_dir, match_file[1]))
}



## =============================================================================
## STAGE 1 -- CANDIDATE GENERA WITH > 12 SPECIES
## =============================================================================
## "Records were retained only when they corresponded to accepted species-level
##  taxa belonging to animals, plants, or fungi... We retained genera with at
##  least 12 species."

# Raw Darwin Core export from the manual GBIF query (see header comment)
rawdata <- read.csv(file.path(basepath, "rawspecieslist.csv"), sep = "\t")

# NOTE: we keep "species", "speciesKey" and "class" here so that this same 
# filtered table can be reused later, in Stage 2 (class-based exclusion) and 
# Stage 4 (Andean-species matching), instead of
# re-reading and re-filtering rawspecieslist.csv a second and third time.

columns <- rawdata[, c("species", "speciesKey", "taxonomicStatus", "taxonRank",
                        "kingdom", "genus", "genusKey", "class", "order", "family")]

# Depuration of kingdoms
threetaxa <- columns[columns$kingdom %in% c("Animalia", "Fungi", "Plantae"), ]

# Depuration of taxon rank (species/subspecies only; "GENUS"-rank records with
# no associated species are excluded)
d <- threetaxa[threetaxa$taxonRank %in% c("SPECIES", "SUBSPECIES"), ]
d <- na.omit(d)

# Species-level table of everything present in the Andes/S. America GBIF
# query, deduplicated by speciesKey. Reused as-is in Stage 4 

andes_species_master <- d %>% distinct(speciesKey, .keep_all = TRUE)

# Genus-level candidate list
y <- d[, c("genus", "genusKey", "order", "family")] %>% distinct(genusKey, .keep_all = TRUE)

## ---- Pragmatic pre-filter by total genus-level occurrence count ----------
## (reduces the number of genera
## that then need a full per-species query in the loop below)
y$numOccGenus <- 0
if (use_parallel) {
  y2 <- foreach(o = 1:nrow(y)) %dopar% {
    occ_count(taxonKey = y$genusKey[o], georeferenced = TRUE)
  }
  y$numOccGenus <- t(as.data.frame(y2))
} else {
  for (o in 1:nrow(y)) {
    y$numOccGenus[o] <- occ_count(taxonKey = y$genusKey[o], georeferenced = TRUE)
  }
}
y <- y[y$numOccGenus >= 60, ]

## ---- Per-genus species count (>= 12 species with >= 1 occurrence) --------
m <- data.frame(y)
m$numSpp <- NA_integer_

if (use_parallel) {
  m2 <- foreach(i = 1:nrow(y)) %dopar% {
    z <- get_species_list(y$genusKey[i])
    z$numPerSp <- vapply(z$speciesKey, function(k) occ_count(taxonKey = k, georeferenced = TRUE), numeric(1))
    z <- z[z$numPerSp > 0, ]
    length(z$species)
  }
  m$numSpp <- t(as.data.frame(m2))
} else {
  for (i in 1:nrow(y)) {
    z <- get_species_list(y$genusKey[i])
    z$numPerSp <- vapply(z$speciesKey, function(k) occ_count(taxonKey = k, georeferenced = TRUE), numeric(1))
    z <- z[z$numPerSp > 0, ]
    m$numSpp[i] <- length(z$species)
  }
}

write_xlsx(m, file.path(basepath, "Clados_candidatos.xlsx"))

# Candidate genera with more than 12 species -- output of Stage 1
candidate_genera <- m[m$numSpp >= 12, ]
write_xlsx(candidate_genera, file.path(basepath, "Clados_candidatos_con_mas_de_doce_spp.xlsx"))
cat(nrow(candidate_genera), "candidate genera retained with >= 12 species.\n")


## =============================================================================
## STAGE 2 -- REMOVE NON-TARGET HIGHER TAXA (e.g. FISHES)
##            (from Removingfishes.R)
## =============================================================================
## The GBIF/Andes-polygon query can pick up non-target groups (e.g. fishes,
## brittle stars) despite the terrestrial focus of the study. These are
## removed here, at the class level, before any further (costly) GBIF
## querying happens in Stage 3.

# genus -> class lookup, taken from the table already built in Stage 1
# (no need to re-query GBIF for this)
genus_class <- andes_species_master %>% distinct(genusKey, .keep_all = TRUE) %>%
  select(genusKey, class)

candidate_genera <- merge(candidate_genera, genus_class, by = "genusKey")

# EDIT this list for the candidate genera 
out <- c("Ophiuroidea", "Actinopterygii")

candidate_genera <- candidate_genera[!candidate_genera$class %in% out, ]
write.csv(candidate_genera, file.path(basepath, "fishremoved.csv"), row.names = FALSE)
cat(nrow(candidate_genera), "genera remain after removing non-target classes:", paste(out, collapse = ", "), "\n")


## =============================================================================
## STAGE 3 -- DOWNLOAD, CLEAN, THIN AND SPLIT OCCURRENCE RECORDS
##
## =============================================================================
## "Species occurrence records for each selected genus were downloaded from
##  GBIF using 'rgbif'. ... duplicate records, records with coordinates equal
##  to zero, and records whose taxonomic status was not 'accepted' [were
##  removed]... we applied a spatial thinning procedure that retained
##  occurrence points separated by a minimum distance of 1 km using the
##  thin_data function from the 'ellipsenm' package."
##
## MAIN OUTPUT: for every species that is retained, a "_join.csv",
## "_train.csv" and "_test.csv" file -- these three are what should be handed
## to 'kuenm' as the Occurrences input for MaxEnt model calibration.

MIN_YEAR <- NULL  

clados <- candidate_genera
clados$numSpp2      <- rep(NA_integer_, nrow(clados))  # species retained after cleaning (>=5 occ.)
clados$numOccGenus2 <- rep(NA_integer_, nrow(clados))  # total cleaned occurrences per genus

for (v in 1:nrow(clados)) {

  genus_name   <- clados$genus[v]
  genus_folder <- file.path(occ_dir, genus_name)
  dir.create(genus_folder, showWarnings = FALSE)

  tablaGenus <- get_species_list(clados$genusKey[v])
  tablaGenus$NumperSp <- vapply(tablaGenus$speciesKey,
                                 function(k) occ_count(taxonKey = k, georeferenced = TRUE), numeric(1))
  tablaGenus <- tablaGenus[tablaGenus$NumperSp > 0, ]

  for (k in 1:nrow(tablaGenus)) {

    species_name   <- tablaGenus$species[k]
    species_folder <- file.path(genus_folder, species_name)
    dir.create(species_folder, showWarnings = FALSE)

    datos <- occ_data(taxonKey = tablaGenus$speciesKey[k], hasCoordinate = TRUE,
                       geometry = poli, limit = 100000)
    a <- datos[["data"]]

    if (is.null(a)) {
      unlink(species_folder, recursive = TRUE)
      tablaGenus$numOcc[k] <- 0
      next
    }

    # raw data saved with an abbreviated file name, e.g. "L_pardalis_Raw_data.csv"
    raw_name <- paste0(substr(species_name, 1, 1), "_",
                        gsub(".* ", "", species_name), "_Raw_data.csv")
    safe_write_csv(a, file.path(species_folder, raw_name))

    occ <- clean_species_occurrences(a, species_name, min_year = MIN_YEAR)
    safe_write_csv(occ, file.path(species_folder, paste0(gsub(" ", "_", species_name), "_clean.csv")))

    if (nrow(occ) > 4) {

      ## ---- Spatial thinning: keep points >= 1 km apart --------------------
      # thin_data() writes its own log into the working directory, so we
      # briefly cd into the species folder for this call only.
      old_wd <- getwd()
      setwd(species_folder)
      occt <- thin_data(occ, "longitude", "latitude", thin_distance = 1, save = TRUE,
                         name = "Occ_Thinning_1km")
      setwd(old_wd)

      safe_write_csv(occt, file.path(species_folder, paste0(gsub(" ", "_", species_name), "_thin.csv")))
      tablaGenus$numOcc[k] <- nrow(occt)

      ## ---- 50/50 train/test split -----------------------------------------
      all <- unique(occt)
   
      j <- sample(nrow(all), round(nrow(all) / 2))
      test  <- all[j, ]
      train <- all[-j, ]

      write.csv(all,   file.path(species_folder, paste0(gsub(" ", "_", species_name), "_join.csv")),  row.names = FALSE)
      write.csv(train, file.path(species_folder, paste0(gsub(" ", "_", species_name), "_train.csv")), row.names = FALSE)
      write.csv(test,  file.path(species_folder, paste0(gsub(" ", "_", species_name), "_test.csv")),  row.names = FALSE)

    } else {
      # too few clean records to thin/split,
      # but still record how many occurrences the species ended up with
      tablaGenus$numOcc[k] <- nrow(occ)
    }
  }

  ## ---- Second-round filter within the genus: >= 5 occurrences/species ----
  tablaGenus <- tablaGenus[tablaGenus$numOcc >= 5, ]

  clados$numSpp2[v]      <- nrow(tablaGenus)
  clados$numOccGenus2[v] <- sum(tablaGenus$numOcc)

  ## ----  genus-removal step: drop genera that no longer -----
  ## ---- have >= 12 species once poorly-sampled species are excluded  ------
  if (nrow(tablaGenus) >= 12) {
    write.csv(tablaGenus, file.path(genus_folder, "sppList.csv"), row.names = FALSE)
  } else {
    unlink(genus_folder, recursive = TRUE)
  }

  cat("Finished genus", v, "of", nrow(clados), ":", genus_name, "-", clados$numSpp2[v], "species retained\n")
}

genera_after_cleaning <- clados[clados$numSpp2 >= 12, ]
write.csv(genera_after_cleaning, file.path(basepath, "segundoFiltroMasde12spp.csv"), row.names = FALSE)
cat(nrow(genera_after_cleaning), "genera retained after occurrence cleaning.\n")
if (nrow(genera_after_cleaning) == 0) {
  stop("No genera passed the post-cleaning >=12-species filter -- nothing to carry forward to Stage 4.")
}


## =============================================================================
## STAGE 4 -- ANDEAN-CENTREDNESS / SAMPLING-FRACTION FILTER
##           
## =============================================================================
##  we excluded genera that included fewer than 11 Andean species with at
##  least five occurrence records each... genera for which Andean species
##  represented less than 75% of species included in the available
##  phylogeny... genera with sampling fraction lower than 25%, total number
##  of species was calculated for each genera based on accepted species with
##  more than 2 georeferenced occurrences in the GBIF.
##

final_genera <- genera_after_cleaning
final_genera$numspinmydata         <- NA_integer_  # species you have ANY data for (diagnostic only)
final_genera$sppinmydatayAndes     <- NA_integer_  # ...of those, how many are confirmed Andean (criterion 1)
final_genera$nSpp_phylogeny        <- NA_integer_  # number of tips (species) in the genus's phylogeny
final_genera$nSpp_phylogeny_Andean <- NA_integer_  # ...of those tips, how many are confirmed Andean
final_genera$TotalAceptedSppinGbif <- NA_integer_  # total accepted GBIF species (>2 occ.)

for (i in 1:nrow(final_genera)) {
  
  genus_name   <- final_genera$genus[i]
  genus_folder <- file.path(occ_dir, genus_name)
  
  # species you actually retained data for, read straight from disk (this is
  # the real, on-the-ground result of Stage 3, not re-derived from memory)
  # kept as a diagnostic column, no longer used for the 75% criterion below
  sp_folders <- list.dirs(genus_folder, recursive = FALSE, full.names = FALSE)
  final_genera$numspinmydata[i] <- length(sp_folders)
  
  andes_sp_this_genus <- andes_species_master$species[andes_species_master$genus == genus_name]
  final_genera$sppinmydatayAndes[i] <- sum(sp_folders %in% andes_sp_this_genus)
  
  # --- phylogeny-based Andean representation -----------------------------
  tr <- get_genus_tree(genus_name, trees_dir)
  if (is.null(tr)) {
    warning("No TimeTree phylogeny found for genus '", genus_name, "' -- excluding it (phylogenetic availability is required).")
    final_genera$nSpp_phylogeny[i]        <- NA_integer_
    final_genera$nSpp_phylogeny_Andean[i] <- NA_integer_
  } else {
    tip_species <- gsub("_", " ", tr$tip.label)  # Newick tips are usually "Genus_species"
    final_genera$nSpp_phylogeny[i]        <- length(tip_species)
    final_genera$nSpp_phylogeny_Andean[i] <- sum(tip_species %in% andes_sp_this_genus)
  }
  
  # total accepted species for the genus in GBIF, for the sampling-fraction denominator
  final_genera$TotalAceptedSppinGbif[i] <- count_total_gbif_species(final_genera$genusKey[i])
  
  cat("Andes-filter check", i, "of", nrow(final_genera), ":", genus_name, "\n")
}

final_genera$PorcentajeAndesspPresentsp <- with(final_genera, nSpp_phylogeny_Andean / nSpp_phylogeny * 100)
final_genera$samplingFraction           <- with(final_genera, sppinmydatayAndes / TotalAceptedSppinGbif * 100)

final_genera <- final_genera[
  !is.na(final_genera$nSpp_phylogeny)      &   # drop genera with no matching tree
    final_genera$sppinmydatayAndes           > 11 &
    final_genera$PorcentajeAndesspPresentsp >= 75 &
    final_genera$samplingFraction           >= 25,
]

# Genera that failed Stage 4 are removed from Occurrence_data too, so the
# folder tree on disk matches the final candidate list exactly
genera_removed <- setdiff(genera_after_cleaning$genus, final_genera$genus)
if (length(genera_removed) > 0) {
  unlink(file.path(occ_dir, genera_removed), recursive = TRUE)
}

write_xlsx(final_genera, file.path(basepath, "FinalGenusList.xlsx"))
cat(nrow(final_genera), "Andean-centred candidate genera retained after all filters.\n",
    "Final list written to FinalGenusList.xlsx\n")

if (use_parallel) stopCluster(myCluster)
