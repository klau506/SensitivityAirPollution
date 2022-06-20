
do_iams_vs_impfun_pre = function(datIni, reg, poll) {
  dat_to_plot = datIni |> filter(t %in% c(2030,2050) & Regions == reg
                                 & pollutant == poll & scen != 'REF')

  dat_medi_iams = dat_to_plot[, .(medi_iams = quantile(value, 0.5)), #median
                       by=c('scen','cb_group','t','model')]
  dat_medi_impfun = dat_to_plot[, .(medi_impfun = quantile(value, 0.5)), #median
                       by=c('scen','cb_group','t','impact_function_group')]
  dat = merge(dat_to_plot, dat_medi_iams, by=c('scen','cb_group','t','model'))
  datall = merge(dat, dat_medi_impfun, by=c('scen','cb_group','t','impact_function_group'))

  datall = data.table(datall)
  datall = do_rename_imp_fun(datall,poll_names = F,short_names = T)
  datall = do_rename_models(datall)
  datall$cb_group <- factor(datall$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))

  return(datall)
}

do_iams_plot = function(datIni, regi, pollu, save) {
  dat_to_plot = do_iams_vs_impfun_pre(datIni, regi, pollu)
  pl_title = paste(pollu,regi,sep='_')
  tt = ''

  pl_iams <- ggplot(dat_to_plot) +
    geom_density(aes(x = value, group = scen,
                     color = scen,
                     fill = scen), alpha = 0.5) +
    geom_vline(aes(color = scen, xintercept = medi_iams),
               linetype="dashed", size = 1) +

    facet_grid(model ~ t, scales='free')+
    theme_light() +
    theme(panel.background = element_rect(fill = NA), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE, legend.position = "bottom", strip.text.y = element_text(size = 7),
          plot.background = element_rect(fill = 'white')) +
    rotate_y_facet_text(angle = 0, align = 0.5, valign = 0.5)+
    scale_fill_manual(values = shortpal,
                      name = 'Scenario')+
    scale_color_manual(values = shortpal,
                       name = 'Scenario')+
    labs(title=tt, x = 'Deaths [in thousands]', y = "Probability density")
  pl_iams
  if(save) {
    if(!dir.exists(file.path('Results/Mort/IAMsVSImpFun'))){
      dir.create(file.path('Results/Mort/IAMsVSImpFun'))
    }
    h = as.integer(length(unique(dat_to_plot$impact_function_group)))*37.5
    name_file_iams = paste("Results/Mort/IAMsVSImpFun",paste(paste('iams',pl_title,sep='_'),'png',sep='.'),sep='/')
    ggsave(file=name_file_iams, width = 300, height = h, units = "mm",plot = pl_iams, limitsize = FALSE)
  }
  else {
    return(pl_iams)
  }
}

do_impfun_plot = function(datIni, regi, pollu,save) {
    dat_to_plot = do_iams_vs_impfun_pre(datIni, regi, pollu)
    pl_title = paste(pollu,regi,sep='_')
    tt = ''

  pl_impfun <- ggplot(dat_to_plot) +
    geom_density(aes(x = value, group = scen,
                     color = scen,
                     fill = scen), alpha = 0.5) +
    geom_vline(aes(color = scen, xintercept = medi_impfun),
               linetype="dashed", size = 1) +

    facet_grid(impact_function_group ~ t, scales='free')+
    theme_light() +
    theme(panel.background = element_rect(fill = NA), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE, legend.position = "bottom", strip.text.y = element_text(size = 7),
          plot.background = element_rect(fill = 'white')) +
    rotate_y_facet_text(angle = 0, align = 0.5, valign = 0.5)+
    scale_fill_manual(values = shortpal,
                      name = 'Scenario')+
    scale_color_manual(values = shortpal,
                       name = 'Scenario')+
    labs(title=tt, x = 'Deaths [in thousands]', y = "Probability density")

  if(save) {
    if(!dir.exists(file.path('Results/Mort/IAMsVSImpFun'))){
      dir.create(file.path('Results/Mort/IAMsVSImpFun'))
    }
    h = as.integer(length(unique(dat_to_plot$impact_function_group)))*37.5
    name_file_impfun = paste("Results/Mort/IAMsVSImpFun",paste(paste('impfun',pl_title,sep='_'),'png',sep='.'),sep='/')
    ggsave(file=name_file_impfun, width = 300, height = h, units = "mm",plot = pl_impfun, limitsize = FALSE)
  }
  else {
    return(pl_impfun)
  }
}

do_iams_vs_impfun_fig_all_cb = function(datIni, reg) {
  datIni = datIni[scen != 'REF']
  pl_pm25_iam = do_iams_plot(datIni,reg,'PM25',F)
  pl_o3_iam = do_iams_plot(datIni,reg,'O3',F)
  pl_pm25_impfun = do_impfun_plot(datIni,reg,'PM25',F)
  pl_o3_impfun = do_impfun_plot(datIni,reg,'O3',F)

  fig_pm25 = ggarrange(pl_pm25_iam + font("title", size = 8),
                       pl_pm25_impfun + font("title", size = 8),
                       labels = c('IAMs','Impact functions'),
                       ncol = 2, nrow = 1,
                       common.legend = TRUE, legend="bottom",
                       widths = c(1,1))

  if(!dir.exists(file.path('Results/Mort/IAMsVSImpFun'))){
    dir.create(file.path('Results/Mort/IAMsVSImpFun'))
  }
  pl_title = paste('iams_vs_impfun',reg,sep='_')
  name_file_pm25 = paste("Results/Mort/IAMsVSImpFun/",paste(paste(pl_title,'PM25',sep='_'),'png',sep='.'),sep='/')
  ggsave(file=name_file_pm25, width = 400, height = 500, units = "mm",plot = fig_pm25, limitsize = FALSE)

  pl_blank = ggplot()+
    theme(panel.background = element_rect(fill = NA), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE, legend.position = "bottom")
  fig_o3_impfun = ggarrange(pl_o3_impfun + font("title", size = 8),
                            pl_blank,
                            common.legend = TRUE, legend="none",
                            ncol = 1, nrow = 2,
                            heights = c(1,2))

  fig_o3 = ggarrange(pl_o3_iam + font("title", size = 8),
                     fig_o3_impfun,
                       labels = c('O3       IAMs','Impact functions'),
                       ncol = 2, nrow = 1,
                       common.legend = TRUE, legend="bottom")

  if(!dir.exists(file.path('Results/Mort/IAMsVSImpFun'))){
    dir.create(file.path('Results/Mort/IAMsVSImpFun'))
  }
  pl_title = paste('iams_vs_impfun',reg,sep='_')
  name_file_o3 = paste("Results/Mort/IAMsVSImpFun/",paste(paste(pl_title,'O3',sep='_'),'png',sep='.'),sep='/')
  ggsave(file=name_file_o3, width = 400, height = 500, units = "mm",plot = fig_o3, limitsize = FALSE)
}

do_iams_vs_impfun_fig_one_cb = function(datIni, reg) {
  # restrict to low carbon budgets since in the others no difference is expected
  datIni = datIni[scen != 'REF' & cb_group == '<1000']
  pl_pm25_iam = do_iams_plot(datIni,reg,'PM25',F)
  pl_o3_iam = do_iams_plot(datIni,reg,'O3',F)
  pl_pm25_impfun = do_impfun_plot(datIni,reg,'PM25',F)
  pl_o3_impfun = do_impfun_plot(datIni,reg,'O3',F)

  fig_pm25 = ggarrange(pl_pm25_iam + font("title", size = 8),
                       pl_pm25_impfun + font("title", size = 8),
                       labels = c('IAMs','Impact functions'),
                       # labels = c('PM25) IAMs','Impact functions'),
                       # hjust = c(-0.20,-0.5),
                       ncol = 2, nrow = 1,
                       common.legend = TRUE, legend="none",
                       widths = c(1,1))
  fig_pm25 = annotate_figure(fig_pm25,
                           left = text_grob(paste0("PM25"), rot = 90, color = "black",
                                            face = "bold", size = 14))

  pl_blank = ggplot()+
    theme(panel.background = element_rect(fill = NA), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE, legend.position = "bottom", plot.background = element_rect(fill = 'white'))
  fig_o3_impfun = ggarrange(pl_o3_impfun + font("title", size = 8),
                            pl_blank,
                            common.legend = TRUE, legend="none",
                            ncol = 1, nrow = 2,
                            heights = c(1,1.25))

  fig_o3 = ggarrange(pl_o3_iam + font("title", size = 8),
                     fig_o3_impfun,
                     labels = c('IAMs','Impact functions'),
                     # labels = c('O3)   IAMs','Impact functions'),
                     # hjust = c(-0.30,-0.5),
                     ncol = 2, nrow = 1,
                     common.legend = TRUE, legend="bottom")
  fig_o3 = annotate_figure(fig_o3,
                             left = text_grob(paste0("O3"), rot = 90, color = "black",
                                              face = "bold", size = 14))

  fig = ggarrange(fig_pm25 + font("title", size = 8),
                  fig_o3 + font("title", size = 8),
                     ncol = 1, nrow = 2,
                     heights = c(1,1.1),
                     common.legend = TRUE, legend="bottom")

  fig = annotate_figure(fig,
                        top = text_grob(paste0("",reg), color = "black",
                                        face = "bold", size = 14))

  if(!dir.exists(file.path('Results/Mort/IAMsVSImpFun'))){
    dir.create(file.path('Results/Mort/IAMsVSImpFun'))
  }
  pl_title = paste('iams_vs_impfun',reg,sep='_')
  name_file = paste("Results/Mort/IAMsVSImpFun/",paste(pl_title,'png',sep='.'),sep='/')
  ggsave(file=name_file, width = 400, height = 265, units = "mm",plot = fig, limitsize = FALSE)
}

# do figure by region
doM_FIGURE_IAMS_Impfun = function(dat) {
  for (r in unique(dat$Regions)) {
    print(r)
    do_iams_vs_impfun_fig_one_cb(dat,r)
  }
}

