# BirdRetreats
# Are birds retreating from lower to upper elevations due to climate change?
# I use eBird and GBIF data to prove so. 
# This code uses five parameters:
# spp_list is a dataframe with a single column listing the bird species of interest.
# occurrences is the path to the folder where occurrences are stored. This data can be donwloaded from eBird or GBIF repositories.
# low_lim and up_lim are lower and upper elevation thresholds beyond which we want to analyze data.
# year is the separation timeline to analyse data. 
# For more information see the published paper.

Notes: This codes uses eBird and GBIF data. The user can select either or both. The file extention to the occurrences is in .csv and each file path is written as _pres.csv for eBird and _gbif_P.csv for GBIF. This can be easily adjusted.
