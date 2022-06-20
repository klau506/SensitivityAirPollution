library(data.table)
library(ggplot2)
library(tidyverse)
library(plyr)
library(broom)
library(ggpubr)
library(ggsci)
library(desiderata)
library(grid)

version = '_av' #choose between '_av' or '_bypx
file = paste("conc_fasstr",version,sep='_')
load(file.path('Data',paste(file,"RData",sep='.')))
datConc = data.table(datConc)
source('C_functions.R')
###########################################################################################
#                               DISTRIB & CUMULATIVE PLOT                                 #
###########################################################################################

# plot and save distrib+cum with all pollutants per region
doC_FIGURE_cum_distrib(datConc)

###########################################################################################
#                                       P-VALUE PLOT                                      #
###########################################################################################

# plot and save p-value with all pollutants and all regions
doC_ks_test_plot(datConc)
