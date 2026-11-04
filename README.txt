README
Supporting Information -- R scripts
=====================================================================

This folder contains the R scripts used to select candidate Andean
genera, download and clean their occurrence data, build calibration
areas, run potential distribution models, and generate concave-hull
range polygons, as described in the Methods section.

The scripts are numbered in the order they are meant to be run. Each
one reads the outputs of the previous step from a shared folder
structure (Occurrence_data/<genus>/<species>/...).

---------------------------------------------------------------------
01_candidate_genera_data_downloading_and_cleansing.R
---------------------------------------------------------------------
Selects the Andean-centred candidate genera and downloads and cleans
their GBIF occurrence records. Run in four stages:
  Stage 1 - keeps candidate genera with more than 12 species.
  Stage 2 - removes non-target higher taxa (e.g. fishes) that were
            unintentionally captured by the GBIF query.
  Stage 3 - downloads, cleans, spatially thins, and splits each
            species' occurrence records into calibration/evaluation
            subsets (the "_join.csv", "_train.csv" and "_test.csv"
            files used by every later script).
  Stage 4 - applies the Andean-centredness and sampling-fraction
            filters, using each genus's TimeTree phylogeny to assess
            what proportion of its species are Andean.

Requires as manual/external input: a raw GBIF occurrence download
(queried with the GMBA Andes polygon) and the genus-level TimeTree
phylogenies (Newick files, downloaded from timetree.org).

---------------------------------------------------------------------
02_M_construction.R
---------------------------------------------------------------------
Builds each species' accessible area (M): a buffer around its cleaned
occurrence records, used as the calibration and projection extent for
the potential distribution models. Reads the occurrence data produced
by script 01 and the environmental PCA rasters, and writes the M for
each species as three ASCII grids (one per principal component).

---------------------------------------------------------------------
03_potential_distribution_models.R
---------------------------------------------------------------------
Calibrates potential distribution models with MaxEnt through the R
package 'kuenm'. Run in four stages:
  Stage 1 - candidate model calibration.
  Stage 2 - candidate model evaluation (statistical significance,
            omission rate, and AICc). 
  Stage 3 - final model creation.
  Stage 4 - binarizes each final model with the minimum training
            presence logistic threshold, and saves the resulting
            rasters as one "<genus>_rasters.rda" per genus.

Requires script 01 (occurrence data) and script 02 (M) to have been
run first, and a working Maxent + Java installation.

---------------------------------------------------------------------
04_concave_polygons.R
---------------------------------------------------------------------
Generates a concave polygon for each species from its cleaned
occurrence records and rasterizes it to match the resolution 
used elsewhere. Saves the resulting rasters as one
 "<genus>_polirasters.rda" per genus.

Requires script 01 (occurrence data) to have been run first.

---------------------------------------------------------------------
---------------------------------------------------------------------
The final step -- inferring the predominant geographic mode of
speciation from the outputs above -- is not included in this
repository. That analysis was developed by Skeels and Cardillo (2019),
who have made their own script publicly available in their paper; the
"_rasters.rda" and "_polirasters.rda" files produced by scripts 03 and
04 are formatted to be used as input to it.
