library(data.table)
library(ggplot2)
library(tidyverse)
library(plyr)
library(broom)
library(ggpubr)
library(ggsci)
library(Matching)
library(desiderata)
library(ggnewscale)
library(FedData)
library(strex)
library(binom)

file = "mort_imp_fun"
load(file.path('Data',paste(file,"RData",sep='.')))
datMort = data.table(datMort)
source('zzz.R')
#################################################################################
#                            DISTRIB & CUMULATIVE GRAPHS                        #
#################################################################################

source('M_distrib_cum.R')
doM_FIGURE_distrib_cum(datMort)

################################################################################
#                                    KS TEST                                   #
################################################################################

source('M_ks_test.R')
doM_FIGURE_ks_test(datMort)

#################################################################################
#                              CI and ZCF SCENSITIVITY                          #
#################################################################################

source('M_sensitivity.R')

# CI
doM_FIGURE_CI_sens_by_region_pollutant(datMort)
doM_FIGURE_CI_sens_by_region(datMort)

# ZCF
doM_FIGURE_ZCF_sens_by_region_pollutant(datMort)
doM_FIGURE_ZCF_sens_by_region(datMort)

# ZCF and CI
doM_FIGURE_ZCF_CI_sens_by_region_pollutant(datMort)
doM_FIGURE_ZCF_CI_sens_by_region(datMort)

###############################################################################
#                               NUMBER OF DEATHS                              #
###############################################################################

source('M_num_deaths.R')
doM_FIGURE_num_deaths(datMort[Regions != 'R10WORLD'])
source('M_avoided_deaths.R')
doM_FIGURE_av_deaths_table(datMort[Regions != 'R10WORLD'])
doM_FIGURE_av_deaths_map(datMort[Regions != 'R10WORLD'])
doM_FIGURE_av_deaths_map_by_year(datMort[Regions != 'R10WORLD'])
doM_FIGURE_av_deaths_map_by_climate_policy_year(datMort[Regions != 'R10WORLD'])

###############################################################################
#                                BAD TAILS PROB                               #
###############################################################################

source('M_badtails.R')
tmp = doM_compute_bad_tails_prob(datMort,0.9)
doM_plot_bad_tails_prob(tmp,0.9)

###############################################################################
#                         IAMs vs Imapact Functions                           #
###############################################################################

source('IamsVsImpfun.R')
doM_FIGURE_IAMS_Impfun(datMort)
