# MORTALITY data preprocess: create columns from the info extracted from the
# scen_model_complete name.
# Data: mortality computed with impact functions and FASST concentrations
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(strex)
library(gridExtra)
library(desiderata)
library(FedData)

#########################################################################################
#                                       IMPORT FILES                                    #
#########################################################################################

folder = 'Mort_Data'

importfun = function(file){
  load(file.path('Data/',folder,file))
  temp = str_extract_all(file,"(?<=mmortot_newimpfun_).+(?=.RData)")
  MMorttot$model_scen = as.character(temp[1])
  datMort = rbind(datMort,MMorttot)
  rm(MMorttot)
  return(datMort)
}

list_files = as.list(list.files(path = paste('Data',folder,sep='/'),
                                pattern = "mmortot_newimpfun_", recursive = TRUE))
load(file.path('Data/',folder,list_files[1]))
temp = str_extract_all(list_files[[1]],"(?<=mmortot_newimpfun_).+(?=.RData)")
datMort = MMorttot
datMort$model_scen = as.character(temp[1])
for (f in list_files[2:length(list_files)]) {
  print(f)
  datMort = importfun(f)
}
rm(list_files)
rm(MMorttot)

#########################################################################################
#                                     DATA PRE-PROCESS                                  #
#########################################################################################

datMort = data.table(datMort)

datMort = datMort |>
  pivot_longer(!c('Regions','model_scen','n','t'), names_to = "impact_function")
datMort = subset(datMort, grepl('CTOT', impact_function) )
datMort$impact_function = str_before_first(str_after_first(datMort$impact_function, 'CTOT_'),'_SC')
datMort = data.table(datMort)

# extract scenario
temp = str_after_nth(str_before_first(datMort$model_scen,'-'),'_', 2)
temp = temp |> replace_na('REF')
datMort$scen = as.character(temp)
datMort$scen <- ifelse(endsWith(datMort$scen, "f"), 'EoC',
                       ifelse(endsWith(datMort$scen, "0") & !endsWith(datMort$scen, "2100"), "NZ",'REF'))

# extract model
datMort$model = str_after_first(datMort$model_scen, '-')

# extract carbon budget
temp = as.numeric(gsub("([0-9]+).*$", "\\1", temp))
temp = temp |> replace_na(5000)
datMort$carbon_budget = temp

# group carbon budgets
datMort <- datMort %>%
  mutate(cb_group = case_when(
    datMort$carbon_budget < 1000 ~ "<1000",
    datMort$carbon_budget %in% (1000:2000) ~ "[1000,2000]",
    datMort$carbon_budget %in% (2500:5000) ~ ">2000"
  ))
datMort$cb_group <- factor(datMort$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))

# add column for the accounted pollutant: pm25, o3, or both
datMort$pollutant = ifelse(str_detect(datMort$impact_function, 'PM25'),'PM25','O3')

# rename years
datMort$t = ifelse(datMort$t == 1, 2020,
                 ifelse(datMort$t == 2, 2030,
                        ifelse(datMort$t == 3, 2040, 2050)))

# extract levels and groups
datMort$global_level = str_after_nth(datMort$impact_function,'_',2)
datMort$ci_level = str_before_first(datMort$global_level,'_')
datMort$z_level = ifelse(str_detect(str_after_first(datMort$global_level,'_'),'_g'),
                         str_before_first(str_after_first(datMort$global_level,'_'),'_g'),
                          ifelse(str_after_first(datMort$global_level,'_') %in% c('gOUT','gWITH'), 'zUNI',
                            str_after_first(datMort$global_level,'_')))

datMort$group = ifelse(str_detect(datMort$global_level,'_g'),
                       str_after_first(datMort$global_level,'_g'),'UNI')

# simplify impact_functions' name
datMort$impact_function_old = datMort$impact_function
datMort$impact_function = str_before_nth(datMort$impact_function,'_',2)
datMort$impact_function_group = paste(datMort$impact_function,datMort$group,sep='_')

datMort = data.table(datMort)
datMort$n = NULL
datMort_world = datMort[, list(value=sum(value,na.rm=TRUE)),
                    by=c('t','model_scen','impact_function','scen',
                         'model','carbon_budget','cb_group','pollutant',
                         'global_level','ci_level','z_level','group',
                         'impact_function_old','impact_function_group')]
datMort_world$Regions = 'R10WORLD'

# add world emissions as a new region
datMort2 = rbind(datMort,datMort_world)
datMort = datMort2
save(datMort,file=file.path('Data','mort_imp_fun.RData'))
