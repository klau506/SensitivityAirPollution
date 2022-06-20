source('E_C_functions.R')

###########################################################################################
#                               DISTRIB & CUMULATIVE PLOT                                 #
###########################################################################################

E_cum_distrib_plot_single_poll = function(datt,reg,poll,save) {
  pl_c = do_cumm_plot(datt,reg,poll,T,T,expression(paste('Emissions [kg/year]')),T)
  pl_d = do_distrib_plot(datt,reg,poll,T,F,expression(paste('Emissions [kg/year]')),T)

  fig = ggarrange(pl_d + font("title", size = 8),
                  pl_c + font("title", size = 8),
                  labels = ifelse(save, c(poll,''), ''),
                  ncol = 2, nrow = 1,
                  common.legend = TRUE, legend=ifelse(save | poll == 'VOC',"bottom","none"),
                  widths = c(1,1.15))

  if(save) {
    fig = annotate_figure(fig,
                          top = text_grob(paste0("Emissions probability distribution and cumulative frequency of ",reg), color = "black", face = "bold", size = 14))

    if(!dir.exists(file.path('Results/Emiss/CumAndDistrib'))){
      dir.create(file.path('Results/Emiss/CumAndDistrib'))
    }

    pl_title = paste('cum_freq_AND_distrib',paste(reg,poll,sep='_'),sep='_')
    name_file = paste("Results/Emiss/CumAndDistrib",paste(pl_title,'png',sep='.'),sep='/')
    ggsave(file=name_file, width = 200, height = 100, units = "mm",plot = fig, limitsize = FALSE)
  }
  else {
    return(fig)
  }
}

doE_FIGURE_cum_distrib = function(dat) {
  for(r in unique(dat$region)) {
    print(r)
    dat = dat[year %in% c(2030,2050) & scenario != 'REF']

    pl_nox = E_cum_distrib_plot_single_poll(dat,r,'NOx',F)
    pl_so2 = E_cum_distrib_plot_single_poll(dat,r,'SO2',F)
    pl_bcoc = E_cum_distrib_plot_single_poll(dat,r,'BC+OC',F)
    pl_voc = E_cum_distrib_plot_single_poll(dat,r,'VOC',F)

    fig = ggarrange(pl_nox + rremove("ylab") + font("title", size = 8),
                    pl_bcoc + rremove("ylab") + font("title", size = 8),
                    pl_so2 + rremove("ylab") + font("title", size = 8),
                    pl_voc + rremove("ylab") + rremove("xlab") + font("title", size = 8),
                    labels = c("NOx", 'BC+OC', "SO2", "VOC"),
                    hjust = c(-0.5,-0.25,-0.5,-0.5),
                    ncol = 1, nrow = 4,
                    common.legend = TRUE, legend="bottom")

    fig = annotate_figure(fig,
                          top = text_grob(paste0("Emissions probability distribution and cumulative frequency of ",r), color = "black", face = "bold", size = 14))

    if(!dir.exists(file.path('Results/Emiss/CumAndDistrib'))){
      dir.create(file.path('Results/Emiss/CumAndDistrib'))
    }
    pl_title = paste('cum_freq_AND_distrib_alltogehter',r,sep='_')
    name_file = paste("Results/Emiss/CumAndDistrib",paste(pl_title,'png',sep='.'),sep='/')
    ggsave(file=name_file, width = 200, height = 300, units = "mm",plot = fig, limitsize = FALSE)
  }
}

###########################################################################################
#                                       P-VALUE PLOT                                      #
###########################################################################################

doE_ks_test_plot = function(dat) {
  dat = dat[year %in% c(2030,2050) & scenario != 'REF' ]
  ks_E_test = ks_testt(dat)
  ks_E_test = ks_E_test[region != 'R10WORLD']
  ks_E_test$cb_group <- factor(ks_E_test$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  pl_E = ggplot(data = ks_E_test, aes(x = cb_group, y = p.value, group = pollutant,
                                      color = region)) +
    geom_point( aes(shape = pollutant), position=position_dodge(width = .5), alpha = 0.8) +
    facet_grid(. ~ year) +
    theme_light() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text = element_text(size = 14),
          legend.position = "bottom") +
    geom_hline(yintercept = 0.05, color = 'red', linetype = 'dashed', alpha = 0.5) +
    scale_color_manual(values = regionspal,
                       name = 'Regions') +
    scale_shape_manual(values = c(15,16,17,18),
                       name = 'Pollutant') +
    labs(title='Scenario dependence regarding emissions', x = 'Carbon budget', y = "p-value")

  if(!dir.exists(file.path('Results/Emiss/KS_test'))){
    dir.create(file.path('Results/Emiss/KS_test'))
  }

  name_file_E = paste("Results/Emiss/KS_test",paste('ks_test','png',sep='.'),sep='/')
  ggsave(file=name_file_E, width = 300, height = 180, units = "mm",plot = pl_E, limitsize = FALSE)
}
