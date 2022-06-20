
############################################################################################
#                                     KOLMOGOROV-SMIRNOV                                   #
############################################################################################

doM_ks_testt = function(datIni) {
  datIni = datIni |> filter(scen != 'REF' & t %in% c(2030,2050))

  # ks test to check if the EoC and NZ scenarios are so different or not
  res_test = NULL
  # LOOP YEAR
  for (y in unique(datIni$t)) {
    #LOOP REGION
    for (reg in unique(datIni$Regions)) {
      # LOOP CARBON BUDGET
      for(cbg in unique(datIni$cb_group)) {
        # LOOP IMPACT FUNCTION & GROUP
        for(impfun in unique(datIni$impact_function_group)) {

          dat = datIni |> filter(t == y & Regions == reg & cb_group == cbg
                                 & z_level %in% c('zUNI','zMED') & ci_level == 'ciMED'
                                 & impact_function_group == impfun)

          res = setDT(glance(ks.test(subset(dat,scen == "EoC")$value,
                                     subset(dat,scen == "NZ")$value,
                                     alternative = 'two.sided')))
          res[,dataNZ := length(subset(dat,scen == "NZ")$value)]
          res[,dataEoC := length(subset(dat,scen == "EoC")$value)]
          res[,impact_function_group := impfun]
          res[,pollutant := unique(dat$pollutant)]
          res[,cb_group := cbg]
          res[,Regions := reg]
          res[,t := y]
          res_test = rbindlist(list(res_test,res))
        }
      }
    }
  }

  return(res_test)
}

doM_ks_test_preprocess = function(datIni) {
  ks_M_test = doM_ks_testt(datIni)
  ks_M_test = ks_M_test[Regions != 'R10WORLD']

  # order the p.value per regions-pollutant-cbgroup-t of the p.value
  ks_all = ks_M_test
  ks_all = ks_all[order(ks_all$p.value),]
  ks_all$cb_group <- factor(ks_all$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  ks_all <- do_rename_imp_fun(ks_all,T,F)
  ks_all = data.table(ks_all)

  return(ks_all)
}

doM_ks_comp_plot = function(ks_all) {
  pl_all = ggplot(data = ks_all, aes(x = cb_group, y = p.value, group = impact_function_group,
                                     color = Regions)) +
    geom_point( aes(shape = impact_function_group), position=position_dodge(width = .5), alpha = 0.9) +
    geom_hline(yintercept = 0.05, color = 'red', linetype = 'dashed', alpha = 0.9) +
    facet_grid(. ~ t) +
    theme(panel.background = element_rect(fill = 'grey99'), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE) +
    theme(legend.position = "bottom") +
    scale_color_manual(values = regionspal,
                       name = 'Regions') +
    scale_shape_manual(values = c(0,1,2,4,3,6,7,8,15,16),
                       name = 'Impact functions') +
    theme_pubr() +
    labs(title='Scenario dependency', x = 'Carbon budget', y = "p-value")

  if(!dir.exists(file.path('Results/Mort/KSComp'))){
    dir.create(file.path('Results/Mort/KSComp'))
  }
  h = ifelse(length(unique(ks_all$pollutant)) == 2 | unique(ks_all$pollutant) == 'PM25', 500, 400)[1]
  pl_name = ifelse(length(unique(ks_all$pollutant)) == 2, 'ks_test_comparisson_all',
                   ifelse(unique(ks_all$pollutant) == 'PM25', 'ks_test_comparisson_PM25', 'ks_test_comparisson_O3'))
  name_file_all = paste("Results/Mort/KSComp",paste(pl_name,'png',sep='.'),sep='/')
  ggsave(file=name_file_all, width = h, height = 180, units = "mm",plot = pl_all, limitsize = FALSE)
}

# by pollutant
doM_FIGURE_ks_test_by_pollutant = function(datMort) {
  ks_test_dat = doM_ks_test_preprocess(datMort)

  for (poll in unique(ks_test_dat$pollutant)) {
    doM_ks_comp_plot(ks_test_dat[pollutant == poll])
  }
}

# PM25 and O3 togheter
doM_FIGURE_ks_test = function(datMort) {
  ks_test_dat = doM_ks_test_preprocess(datMort)
  doM_ks_comp_plot(ks_test_dat)
}


