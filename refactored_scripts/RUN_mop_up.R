setwd("~/Dropbox/Nigeria 661 Baseline Data Cleaning/")

#functions
source("scripts/mop_up/mop_up_functions.R")
#FACILITY LISTS
  #importing/merging
  source("scripts/mop_up/mop_up_FL_importmerge.R")
  #cleaning/ID generation
  source("scripts/mop_up/mop_up_FL_cleaningIDgeneration.R")
  #writingout/aggregation
  source("scripts/mop_up/mop_up_FL_writingAggregation.R")

#BASELINE databaseline data (importing/merging/cleaning/analysis)
  source("scripts/mop_up/mop_up_Baseline.R")

#OSSAP_updates
  source("scripts/mop_up/ossap_updates.R")
