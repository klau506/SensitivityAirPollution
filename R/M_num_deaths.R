###############################################################################
#                               NUMBER OF DEATHS                              #
###############################################################################
doM_FIGURE_num_deaths = function(datIni) {
  datIni$zz = ifelse(datIni$z_level == 'zLO', 'zL',
                     ifelse(datIni$z_level == 'zHI', 'zH', 'zM'))
  dd = datIni |> filter(t %in% c(2030,2050))
  dd = data.table(dd)

  # consider the average deaths by cb, reg, year, climate policy, poll --> the impact function does not play a role any more
  dd_mean = aggregate(dd$value, by = list(cb_group=dd$cb_group, Regions=dd$Regions,
                                          t=dd$t, scen=dd$scen, pollutant=dd$pollutant,
                                          ci_level=dd$ci_level, zz=dd$zz), FUN=median)
  dd_mean = data.frame(dd_mean)

  # consider the overall deaths: O3 + PM25 deaths
  dd_sum = aggregate(dd_mean$x, list(cb_group=dd_mean$cb_group, Regions=dd_mean$Regions,
                                     t=dd_mean$t, scen=dd_mean$scen, ci_level=dd_mean$ci_level,
                                     zz=dd_mean$zz), FUN=sum)
  dd_sum = data.frame(dd_sum)
  names(dd_sum)[names(dd_sum) == 'x'] <- 'val_sum'
  dd_all = merge(dd, dd_sum, by=c('cb_group', 'Regions', 't', 'scen', 'ci_level','zz'))

  # compute CI
  dd_all <- dd_all[,
                   .(vmed = quantile(val_sum, 0.5),
                     v05  = quantile(val_sum, 0.05),
                     v95  = quantile(val_sum, 0.95),
                     vmax  = max(val_sum),
                     vmin  = min(val_sum),
                     vmean  = mean(val_sum)),
                   by = c('cb_group', 'Regions', 't', 'scen')]

  arrange(dd_all, Regions)
  dd_all = dd_all[order(dd_all$Regions), ]
  dd_all$Regions = fct_rev(dd_all$Regions)

  do_num_deaths_plot = function(dat,sc,yr,ylab) {
    tt = paste(sc, 'scenario')

    pl = ggplot(data = dat[t == yr & scen == sc], aes(x = cb_group, y = Regions)) +
      geom_tile(aes(fill = vmed)) +
      geom_text(aes(label = paste(round(vmed, 1),'\n','(',round(v05, 1),',',round(v95, 1),')')), size = 3.2) +
      scale_fill_gradient("Deaths \n [in thousands]", low = "#e6d329", high = "#d6290b") +
      theme_bw() +
      labs(x = "\n Carbon budget", y = "Region",
           title = tt) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_text(size = 10),
            axis.title.y = element_text(size = 10),
            legend.title = element_text(size = 10)) +
      theme_pubr(legend = 'right') +
      labs_pubr(base_size = 10)

    if(!ylab) {
      pl = pl + scale_y_discrete(labels = NULL, breaks = NULL)
    }

    return(pl)
  }

  # figure by year
  for(year in c(2030,2050)) {
    pl_nz = do_num_deaths_plot(dd_all,'NZ',year,T)
    pl_eoc = do_num_deaths_plot(dd_all,'EoC',year,F)
    pl_ref = do_num_deaths_plot(dd_all,'REF',year,F)

    maxHeight = grid::unit.pmax(pl_nz$heights, pl_eoc$heights, pl_ref$heights)
    pl_nz$heights <- as.list(maxHeight)
    pl_eoc$heights <- as.list(maxHeight)
    pl_ref$heights <- as.list(pl_eoc$heights)

    fig = ggarrange(pl_nz + rremove('xlab') + rremove('ylab'),
                    pl_eoc + rremove('xlab') + rremove('ylab'),
                    pl_ref + rremove('xlab') + rremove('ylab'),
                    common.legend = TRUE, legend="right",
                    ncol = 3, nrow = 1,
                    widths = c(1,0.75,0.35),
                    heights = c(1,1,0.5))
    fig = annotate_figure(fig,
                          top = text_grob(paste0("Premature deaths in ",year), color = "black", face = "bold", size = 14),
                          left = text_grob("Regions", color = "black", size = 10, rot = 90),
                          bottom = text_grob("Carbon budget [GtCO2]", color = "black", size = 10))

    if(!dir.exists(file.path('Results/Mort/NumDeaths'))){
      dir.create(file.path('Results/Mort/NumDeaths'))
    }
    pl_name = ifelse(length(unique(datIni$Regions)) == 1, 'num_deaths_world', 'num_deaths_table')
    name_file_numD = paste("Results/Mort/NumDeaths",paste(paste(pl_name,year,sep='_'),'png',sep='.'),sep='/')
    ggsave(file=name_file_numD, width = 350, height = 150, units = "mm",plot = fig, limitsize = FALSE)
  }

  # whole figure
  pl_nz_2030 = do_num_deaths_plot(dd_all,'NZ',2030,T)
  pl_eoc_2030 = do_num_deaths_plot(dd_all,'EoC',2030,F)
  pl_ref_2030 = do_num_deaths_plot(dd_all,'REF',2030,F)

  pl_nz_2050 = do_num_deaths_plot(dd_all,'NZ',2050,T)
  pl_eoc_2050 = do_num_deaths_plot(dd_all,'EoC',2050,F)
  pl_ref_2050 = do_num_deaths_plot(dd_all,'REF',2050,F)

  fig = ggarrange(pl_nz_2030 + rremove('xlab') + rremove('ylab'),
                  pl_eoc_2030 + rremove('xlab') + rremove('ylab'),
                  pl_ref_2030 + rremove('xlab') + rremove('ylab'),

                  pl_nz_2050 + rremove('xlab') + rremove('ylab'),
                  pl_eoc_2050 + rremove('xlab') + rremove('ylab'),
                  pl_ref_2050 + rremove('xlab') + rremove('ylab'),

                  common.legend = TRUE, legend="right",
                  labels = c('2030','','','2050'),
                  ncol = 3, nrow = 2,
                  widths = c(1,0.75,0.35))

  fig = annotate_figure(fig,
                        top = text_grob(paste0("Premature deaths"), color = "black", face = "bold", size = 14),
                        left = text_grob("Regions", color = "black", size = 10, rot = 90),
                        bottom = text_grob("Carbon budget [GtCO2]", color = "black", size = 10))

  if(!dir.exists(file.path('Results/Mort/NumDeaths'))){
    dir.create(file.path('Results/Mort/NumDeaths'))
  }
  pl_name = ifelse(length(unique(datIni$Regions)) == 1, 'num_deaths_world', 'num_deaths_table')
  name_file_numD = paste("Results/Mort/NumDeaths",paste(pl_name,'png',sep='.'),sep='/')
  ggsave(file=name_file_numD, width = 350, height = 300, units = "mm",plot = fig, limitsize = FALSE)
}
