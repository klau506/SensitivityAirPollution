
################################################################################
#                             CI LEVEL SENSITIVITY                             #
################################################################################

doM_ci_sens_plot_by_reg = function(datIni,reg,poll,xx,yy,save) {

  pl_title = paste(poll,reg,sep='_')
  tt = ''

  dat <- datIni[t %in% c(2030,2050) & Regions == reg & pollutant == poll
                & z_level %in% c('zUNI','zMED'),
                .(cmed = quantile(value, 0.5),
                  c05  = quantile(value, 0.05),
                  c95  = quantile(value, 0.95),
                  mmax  = max(value),
                  mmin  = min(value),
                  mmean  = mean(value)),
                by = c('t','Regions','impact_function_group','cb_group','scen','ci_level')]

  dat[ci_level == "ciLO", ci_label := "5th"]
  dat[ci_level == "ciMED", ci_label := "median"]
  dat[ci_level == "ciHI", ci_label := "95th"]
  dat[, ci_level := factor(ci_label, levels = c("5th",
                                                "median",
                                                "95th"))]
  dat = do_rename_imp_fun(dat,poll_names = F,short_names = F)
  dat$impact_function_group = fct_rev(dat$impact_function_group)
  dat$cb_group <- factor(dat$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))

  pl <- ggplot(dat) +
    geom_errorbar(aes(xmin = c05,
                      xmax = c95,
                      y = impact_function_group,
                      group = interaction(scen,ci_level),
                      color = ci_level),
                  size = 1,
                  width = 0.33,
                  position = position_dodge(width = 0.4)) +
    geom_point(aes(x = cmed,
                   y = impact_function_group,
                   group = interaction(scen,ci_level),
                   shape = scen),
               position = position_dodge(width = 0.4),
               size = 2.5) +
    geom_point(aes(x = cmed,
                   y = impact_function_group,
                   group = interaction(scen,ci_level),
                   color = ci_level,
                   shape = scen),
               position = position_dodge(width = 0.4),
               size = 1.5) +
    facet_grid(cb_group ~ t, scales='free')+
    theme_light() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.y.left = element_text(size = 12),
          axis.text.x.bottom = element_text(size = 8)) +
    scale_x_continuous(labels = function(cmed) format(cmed, scientific = TRUE))+
    guides(x =  guide_axis(n.dodge = 2)) +
    scale_shape_manual(values = c(16,17),
                       name = 'Scenario')+
    scale_color_brewer(palette = "Set1",
                       name = 'Impact function\npercentile') +
    labs(title=tt, x = 'Deaths [in thousands]', y = "Impact functions")

  if(!xx) {
    pl = pl + theme(strip.text.x = element_blank())
  }
  if(!yy) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  if(poll == 'PM25'){
    pl = pl + guides(x =  guide_axis(n.dodge = 2))
  }

  if(save) {
    if(!dir.exists(file.path('Results/Mort/CI/ByPoll'))){
      dir.create(file.path('Results/Mort/CI/ByPoll'))
    }
    h = as.integer(length(unique(dat$impact_function_group)))*37.5
    name_file_M = paste("Results/Mort/CI/ByPoll",paste(paste('Medians',pl_title,sep='_'),'png',sep='.'),sep='/')
    ggsave(file=name_file_M, width = 400, height = h, units = "mm",plot = pl, limitsize = FALSE)
  }
  else {
    return(pl)
  }
}


# by region and poll
doM_FIGURE_CI_sens_by_region_pollutant = function(dat) {
  for (reg in unique(dat$Regions)) {
    for (poll in unique(dat$pollutant)) {
      # restric to NZ and EoC
      dat = dat |> filter(scen != 'REF')

      doM_ci_sens_plot_by_reg(dat,reg,poll,T,T,T)
    }
  }
}

# by region
doM_FIGURE_CI_sens_by_region = function(dat) {
  for (r in unique(dat$Regions)) {
    # restric to NZ and EoC
    dat = dat |> filter(scen != 'REF')

    print(r)
    pl_pm25 = doM_ci_sens_plot_by_reg(dat,r,'PM25',T,T,F)
    pl_o3 = doM_ci_sens_plot_by_reg(dat,r,'O3',T,T,F)

    fig = ggarrange(pl_pm25 + rremove("ylab") + font("title", size = 8),
                    pl_o3 + rremove("ylab") + font("title", size = 8),
                    labels = c("PM25", 'O3'),
                    ncol = 1, nrow = 2,
                    heights = c(1,0.3),
                    common.legend = TRUE, legend="right")

    fig = annotate_figure(fig,
                          top = text_grob(paste0('Impact funcitons\' parameter value sensitivty analysis of ',r), color = "black", face = "bold", size = 14))

    if(!dir.exists(file.path('Results/Mort/CI/ByRegion'))){
      dir.create(file.path('Results/Mort/CI/ByRegion'))
    }
    name_file_M = paste("Results/Mort/CI/ByRegion",paste(paste('Medians',r,sep='_'),'png',sep='.'),sep='/')
    ggsave(file=name_file_M, width = 220, height = 325, units = "mm",plot = fig, limitsize = FALSE)
  }
}

################################################################################
#                                  ZCF SENSITIVITY                             #
################################################################################

doM_zcf_sens_plot_by_reg = function(datIni,reg,poll,xx,yy,save) {

  pl_title = paste(poll,reg,sep='_')
  tt = ''

  dat <- datIni[t %in% c(2030,2050) & Regions == reg & pollutant == poll
                & ci_level %in% c('ciMED','ciUNI') & z_level != 'zUNI',
                .(cmed = quantile(value, 0.5),
                  c05  = quantile(value, 0.05),
                  c95  = quantile(value, 0.95),
                  mmax  = max(value),
                  mmin  = min(value),
                  mmean  = mean(value)),
                by = c('t','Regions','impact_function_group','cb_group',
                       'scen','z_level')]

  dat[z_level == "zLO", z_label := '5th']
  dat[z_level == "zMED", z_label := 'mean']
  dat[z_level == "zHI", z_label := '95th']
  dat[, z_level := factor(z_label, levels = c('5th','mean','95th'))]

  dat = do_rename_imp_fun(dat,poll_names = F,short_names = F)
  dat$impact_function_group = fct_rev(dat$impact_function_group)
  dat$cb_group <- factor(dat$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))

  pl <- ggplot(dat) +
    geom_errorbar(aes(xmin = c05,
                      xmax = c95,
                      y = impact_function_group,
                      group = interaction(scen,z_level),
                      color = z_level),
                  size = 1,
                  width = 0.33,
                  position = position_dodge(width = 0.5)) +
    geom_point(aes(x = cmed,
                   y = impact_function_group,
                   group = interaction(scen,z_level),
                   shape = scen),
               position = position_dodge(width = 0.5),
               size = 2.5) +
    geom_point(aes(x = cmed,
                   y = impact_function_group,
                   group = interaction(scen,z_level),
                   color = z_level,
                   shape = scen),
               position = position_dodge(width = 0.5),
               size = 1.5) +
    facet_grid(cb_group ~ t, scales='free')+
    theme_light() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.y.left = element_text(size = 12),
          axis.text.x.bottom = element_text(size = 8)) +
    scale_x_continuous(labels = function(cmed) format(cmed, scientific = TRUE))+
    guides(x = guide_axis(n.dodge = 2)) +
    scale_shape_manual(values = c(16,17),
                       name = 'Scenario')+
    scale_color_brewer(palette = "Set2",
                       name = 'TMREL\npercentile') +
    labs(title=tt, x = 'Deaths [in thousands]', y = "Impact functions")

  if(!xx) {
    pl = pl + theme(strip.text.x = element_blank())
  }
  if(!yy) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  if(poll == 'PM25'){
    pl = pl + guides(x =  guide_axis(n.dodge = 2))
  }


  if(save) {
    if(!dir.exists(file.path('Results/Mort/ZCF/ByPoll'))){
      dir.create(file.path('Results/Mort/ZCF/ByPoll'))
    }
    h = as.integer(length(unique(dat$impact_function_group)))*37.5
    name_file_M = paste("Results/Mort/ZCF/ByPoll",paste(paste('Medians',pl_title,sep='_'),'png',sep='.'),sep='/')
    ggsave(file=name_file_M, width = 250, height = h, units = "mm",plot = pl, limitsize = FALSE)
  }
  else {
    return(pl)
  }
}

# by region and poll
doM_FIGURE_ZCF_sens_by_region_pollutant = function(dat) {
  for (reg in unique(dat$Regions)) {
    for (poll in unique(dat$pollutant)) {
      # restric to NZ and EoC
      dat = dat |> filter(scen != 'REF')

      doM_zcf_sens_plot_by_reg(dat,reg,poll,T,T,T)
    }
  }
}

# by region
doM_FIGURE_ZCF_sens_by_region = function(dat) {
  for (r in unique(dat$Regions)) {
    # restric to NZ and EoC
    dat = dat |> filter(scen != 'REF')

    print(r)
    pl_pm25 = doM_zcf_sens_plot_by_reg(dat,r,'PM25',T,T,F)
    pl_o3 = doM_zcf_sens_plot_by_reg(dat,r,'O3',T,T,F)

    fig = ggarrange(pl_pm25 + rremove("ylab") + font("title", size = 8),
                    pl_o3 + rremove("ylab") + font("title", size = 8),
                    labels = c("PM25", 'O3'),
                    ncol = 1, nrow = 2,
                    heights = c(1,0.3),
                    common.legend = TRUE, legend="right")
    fig = annotate_figure(fig,
                          top = text_grob(paste0('Impact funcitons\' counterfactual value sensitivty analysis of ',r), color = "black", face = "bold", size = 14))

    if(!dir.exists(file.path('Results/Mort/ZCF/ByRegion'))){
      dir.create(file.path('Results/Mort/ZCF/ByRegion'))
    }
    name_file_M = paste("Results/Mort/ZCF/ByRegion",paste(paste('Medians',r,sep='_'),'png',sep='.'),sep='/')
    ggsave(file=name_file_M, width = 220, height = 325, units = "mm",plot = fig, limitsize = FALSE)
  }
}

################################################################################
#                               ZCF & CI SENSITIVITY                           #
################################################################################

doM_zcf_ci_sens_plot_by_reg = function(datIni,reg,poll,xx,yy,save) {

  pl_title = paste(poll,reg,sep='_')
  tt = ''

  dat <- datIni[t %in% c(2030,2050) & Regions == reg & pollutant == poll
                & z_level != 'zUNI',
                .(cmed = quantile(value, 0.5),
                  c05  = quantile(value, 0.05),
                  c95  = quantile(value, 0.95),
                  mmax  = max(value),
                  mmin  = min(value),
                  mmean  = mean(value)),
                by = c('t','Regions','impact_function_group','cb_group',
                       'scen','ci_level','z_level')]

  dat[ci_level == "ciLO", ci_label := "5th"]
  dat[ci_level == "ciMED", ci_label := "median"]
  dat[ci_level == "ciHI", ci_label := "95th"]
  dat[, ci_level := factor(ci_label, levels = c("5th",
                                                "median",
                                                "95th"))]
  dat[z_level == "zLO", z_label := '5th']
  dat[z_level == "zMED", z_label := 'mean']
  dat[z_level == "zHI", z_label := '95th']
  dat[, z_level := factor(z_label, levels = c('5th','mean','95th'))]

  dat = do_rename_imp_fun(dat,poll_names = F,short_names = F)
  dat$impact_function_group = fct_rev(dat$impact_function_group)
  dat$cb_group <- factor(dat$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))

  pl <- ggplot(dat) +
    geom_errorbar(aes(xmin = c05,
                      xmax = c95,
                      y = impact_function_group,
                      group = interaction(scen,ci_level,z_level),
                      color = ci_level,
                      linetype = z_level),
                  size = 1,
                  width = 0.33,
                  position = position_dodge(width = 0.5)) +
    geom_point(aes(x = cmed,
                   y = impact_function_group,
                   group = interaction(scen,ci_level,z_level),
                   shape = scen),
               position = position_dodge(width = 0.5),
               size = 2.5) +
    geom_point(aes(x = cmed,
                   y = impact_function_group,
                   group = interaction(scen,ci_level,z_level),
                   color = ci_level,
                   shape = scen),
               position = position_dodge(width = 0.5),
               size = 1.5) +
    facet_grid(cb_group ~ t, scales='free')+
    theme_light() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.y.left = element_text(size = 12),
          axis.text.x.bottom = element_text(size = 8)) +
    scale_x_continuous(labels = function(cmed) format(cmed, scientific = TRUE))+
    scale_shape_manual(values = c(16,17),
                       name = 'Scenario')+
    scale_color_brewer(palette = "Set1",
                       name = 'Impact function\npercentile') +
    scale_linetype_manual(values = c('dotted','solid','44'),
                          name = 'TMREL\npercentile') +
    labs(title=tt, x = 'Deaths [in thousands]', y = "Impact functions")

  if(!xx) {
    pl = pl + theme(strip.text.x = element_blank())
  }
  if(!yy) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  if(poll == 'PM25'){
    pl = pl + guides(x =  guide_axis(n.dodge = 2))
  }

  if(save) {
    if(!dir.exists(file.path('Results/Mort/ZCF_CI/ByPoll'))){
      dir.create(file.path('Results/Mort/ZCF_CI/ByPoll'))
    }
    h = as.integer(length(unique(dat$impact_function_group)))*37.5
    name_file_M = paste("Results/Mort/ZCF_CI/ByPoll",paste(paste('Medians',pl_title,sep='_'),'png',sep='.'),sep='/')
    ggsave(file=name_file_M, width = 300, height = h, units = "mm",plot = pl, limitsize = FALSE)
  }
  else {
    return(pl)
  }
}

# by region and poll
doM_FIGURE_ZCF_CI_sens_by_regio_pollutant = function(dat) {
  for (reg in unique(dat$Regions)) {
    for (poll in unique(dat$pollutant)) {
      # restric to NZ and EoC
      dat = dat |> filter(scen != 'REF')

      doM_zcf_ci_sens_plot_by_reg(dat,reg,poll,T,T,T)
    }
  }
}

# by region
doM_FIGURE_ZCF_CI_sens_by_region = function(dat) {
  for (r in unique(dat$Regions)) {
    # restric to NZ and EoC
    dat = dat |> filter(scen != 'REF')

    print(r)
    pl_pm25 = doM_zcf_ci_sens_plot_by_reg(dat,r,'PM25',T,T,F)
    pl_o3 = doM_zcf_ci_sens_plot_by_reg(dat,r,'O3',T,T,F)

    fig = ggarrange(pl_pm25 + rremove("ylab") + font("title", size = 8),
                    pl_o3 + rremove("ylab") + font("title", size = 8),
                    labels = c("PM25", 'O3'),
                    ncol = 1, nrow = 2,
                    heights = c(1,0.3),
                    common.legend = TRUE, legend="right")
    fig = annotate_figure(fig,
                          top = text_grob(paste0('Impact funcitons\' parameter and counterfactual values sensitivty analysis of ',r), color = "black", face = "bold", size = 14))

    if(!dir.exists(file.path('Results/Mort/ZCF_CI/ByRegion'))){
      dir.create(file.path('Results/Mort/ZCF_CI/ByRegion'))
    }
    name_file_M = paste("Results/Mort/Medians/ZCF_CI/ByRegion",paste(paste('Medians',r,sep='_'),'png',sep='.'),sep='/')
    ggsave(file=name_file_M, width = 220, height = 375, units = "mm",plot = fig, limitsize = FALSE)
  }
}

