# rename impact functions and order them by year of publication
do_rename_imp_fun = function(dat,poll_names,short_names) {
  dat = data.table(dat)

  if (poll_names & !short_names) {
    dat[impact_function_group == "PM25MORT_OSTRO2004_UNI", imp_fun_label := 'PM25 OSTRO2004']
    dat[impact_function_group == "PM25MORT_KREWSKI2009_UNI", imp_fun_label := 'PM25 KREWSKI2009']
    dat[impact_function_group == "PM25MORT_BURNETT2014_UNI", imp_fun_label := 'PM25 BURNETT2014']
    dat[impact_function_group == "PM25MORT_GBD2016_LO", imp_fun_label := 'PM25 GBD2016 LOW']
    dat[impact_function_group == "PM25MORT_GBD2016_MED", imp_fun_label := 'PM25 GBD2016 MEDIUM']
    dat[impact_function_group == "PM25MORT_GBD2016_HI", imp_fun_label := 'PM25 GBD2016 HIGH']
    dat[impact_function_group == "PM25MORT_BRUNETT2018_WITH", imp_fun_label := 'PM25 BRUNETT2018 WITH']
    dat[impact_function_group == "PM25MORT_BRUNETT2018_OUT", imp_fun_label := 'PM25 BRUNETT2018 WITHOUT']
    dat[impact_function_group == "O3MORT_GBD2015_UNI", imp_fun_label := 'O3 GBD2015']
    dat[impact_function_group == "O3MORT_JERRET2009_UNI", imp_fun_label := 'O3 JERRET2009']
    dat[, impact_function_group := factor(imp_fun_label, levels = c('PM25 OSTRO2004',
                                                              'PM25 KREWSKI2009',
                                                              'PM25 BURNETT2014',
                                                              'PM25 GBD2016 LOW',
                                                              'PM25 GBD2016 MEDIUM',
                                                              'PM25 GBD2016 HIGH',
                                                              'PM25 BRUNETT2018 WITH',
                                                              'PM25 BRUNETT2018 WITHOUT',
                                                              'O3 JERRET2009',
                                                              'O3 GBD2015'))]
  } else if (!poll_names & !short_names) {
    dat[impact_function_group == "PM25MORT_OSTRO2004_UNI", imp_fun_label := 'OSTRO2004']
    dat[impact_function_group == "PM25MORT_KREWSKI2009_UNI", imp_fun_label := 'KREWSKI2009']
    dat[impact_function_group == "PM25MORT_BURNETT2014_UNI", imp_fun_label := 'BURNETT2014']
    dat[impact_function_group == "PM25MORT_GBD2016_LO", imp_fun_label := 'GBD2016 LOW']
    dat[impact_function_group == "PM25MORT_GBD2016_MED", imp_fun_label := 'GBD2016 MEDIUM']
    dat[impact_function_group == "PM25MORT_GBD2016_HI", imp_fun_label := 'GBD2016 HIGH']
    dat[impact_function_group == "PM25MORT_BRUNETT2018_WITH", imp_fun_label := 'BRUNETT2018 WITH']
    dat[impact_function_group == "PM25MORT_BRUNETT2018_OUT", imp_fun_label := 'BRUNETT2018 WITHOUT']
    dat[impact_function_group == "O3MORT_GBD2015_UNI", imp_fun_label := 'GBD2015']
    dat[impact_function_group == "O3MORT_JERRET2009_UNI", imp_fun_label := 'JERRET2009']
    dat[, impact_function_group := factor(imp_fun_label, levels = c('OSTRO2004',
                                                              'KREWSKI2009',
                                                              'BURNETT2014',
                                                              'GBD2016 LOW',
                                                              'GBD2016 MEDIUM',
                                                              'GBD2016 HIGH',
                                                              'BRUNETT2018 WITH',
                                                              'BRUNETT2018 WITHOUT',
                                                              'JERRET2009',
                                                              'GBD2015'))]
  } else if (!poll_names & short_names) {
    dat[impact_function_group == "PM25MORT_OSTRO2004_UNI", imp_fun_label := 'OSTRO2004']
    dat[impact_function_group == "PM25MORT_KREWSKI2009_UNI", imp_fun_label := 'KREWSKI2009']
    dat[impact_function_group == "PM25MORT_BURNETT2014_UNI", imp_fun_label := 'BURNETT2014']
    dat[impact_function_group == "PM25MORT_GBD2016_LO", imp_fun_label := 'GBD2016 LOW']
    dat[impact_function_group == "PM25MORT_GBD2016_MED", imp_fun_label := 'GBD2016 MED']
    dat[impact_function_group == "PM25MORT_GBD2016_HI", imp_fun_label := 'GBD2016 HIGH']
    dat[impact_function_group == "PM25MORT_BRUNETT2018_WITH", imp_fun_label := 'BRUNETT18 W']
    dat[impact_function_group == "PM25MORT_BRUNETT2018_OUT", imp_fun_label := 'BRUNETT18 O']
    dat[impact_function_group == "O3MORT_GBD2015_UNI", imp_fun_label := 'GBD2015']
    dat[impact_function_group == "O3MORT_JERRET2009_UNI", imp_fun_label := 'JERRET2009']
    dat[, impact_function_group := factor(imp_fun_label, levels = c('OSTRO2004',
                                                                    'KREWSKI2009',
                                                                    'BURNETT2014',
                                                                    'GBD2016 LOW',
                                                                    'GBD2016 MED',
                                                                    'GBD2016 HIGH',
                                                                    'BRUNETT18 W',
                                                                    'BRUNETT18 O',
                                                                    'JERRET2009',
                                                                    'GBD2015'))]
  } else {
    dat[impact_function_group == "PM25MORT_OSTRO2004_UNI", imp_fun_label := 'PM25 OSTRO2004']
    dat[impact_function_group == "PM25MORT_KREWSKI2009_UNI", imp_fun_label := 'PM25 KREWSKI2009']
    dat[impact_function_group == "PM25MORT_BURNETT2014_UNI", imp_fun_label := 'PM25 BURNETT2014']
    dat[impact_function_group == "PM25MORT_GBD2016_LO", imp_fun_label := 'PM25 GBD2016 LOW']
    dat[impact_function_group == "PM25MORT_GBD2016_MED", imp_fun_label := 'PM25 GBD2016 MED']
    dat[impact_function_group == "PM25MORT_GBD2016_HI", imp_fun_label := 'PM25 GBD2016 HIGH']
    dat[impact_function_group == "PM25MORT_BRUNETT2018_WITH", imp_fun_label := 'PM25 BRUNETT18 W']
    dat[impact_function_group == "PM25MORT_BRUNETT2018_OUT", imp_fun_label := 'PM25 BRUNETT18 O']
    dat[impact_function_group == "O3MORT_GBD2015_UNI", imp_fun_label := 'O3 GBD2015']
    dat[impact_function_group == "O3MORT_JERRET2009_UNI", imp_fun_label := 'O3 JERRET2009']
    dat[, impact_function_group := factor(imp_fun_label, levels = c('PM25 OSTRO2004',
                                                                    'PM25 KREWSKI2009',
                                                                    'PM25 BURNETT2014',
                                                                    'PM25 GBD2016 LOW',
                                                                    'PM25 GBD2016 MED',
                                                                    'PM25 GBD2016 HIGH',
                                                                    'PM25 BRUNETT18 W',
                                                                    'PM25 BRUNETT18 O',
                                                                    'O3 JERRET2009',
                                                                    'O3 GBD2015'))]
  }
  return(dat)
}

# rename models and order them alphabetically
do_rename_models = function(dat) {
  dat = data.table(dat)

  dat[model == "AIM_CGE_V2_2", model_label := 'AIM CGE']
  dat[model == "IMAGE_3_0", model_label := 'IMAGE']
  dat[model == "MESSAGEix-GLOBIOM_1_1", model_label := 'MESSAGEix-GLOBIOM']
  dat[model == "POLES-JRC_ENGAGE", model_label := 'POLES-JRC']
  dat[model == "REMIND-MAgPIE_2_1-4_2", model_label := 'REMIND-MAgPIE']
  dat[model == "WITCH_5_0", model_label := 'WITCH']
  dat[, model := factor(model_label, levels = c('AIM CGE',
                                                  'IMAGE',
                                                  'MESSAGEix-GLOBIOM',
                                                  'POLES-JRC',
                                                  'REMIND-MAgPIE',
                                                  'WITCH'))]

  return(dat)
}

# palettes
regionspal = c('#FFFF00','#FF4500','#8A2BE2','#1E90FF','#32CD32','#FFA500','#FF1493',
               '#00FA9A','#EE82EE','#FF7F50','#7b857e')
shortpal = c('EoC'='#EF2424',
             'NZ'='#2465EF')
longpal = c('EoC.ciHI'='#A50909',
            'NZ.ciHI'='#0934A5',
            'EoC.ciMED'='#EF2424',
            'NZ.ciMED'='#2465EF',
            'EoC.ciLO'='#F66E6E',
            'NZ.ciLO'='#6E9BF6')
longlabs = c('Eoc, 95% CI','NZ, 95% CI',
             'EoC, 50% CI','NZ, 50% CI',
             'EoC, 5% CI','NZ, 5% CI')
