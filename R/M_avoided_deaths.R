data_path = '/home/klaudia/Documents/UPC/MAMME/TFM/FASST_External_Disk/FASST/fasstr_data'
load(file.path(data_path,'WITCH',"mask3618.RData"))
require(countrycode)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
###############################################################################
#                               NUMBER OF DEATHS                              #
###############################################################################
doM_av_deaths_map_preprocess = function(datIni) {
  datIni$zz = ifelse(datIni$z_level == 'zLO', 'zL',
                     ifelse(datIni$z_level == 'zHI', 'zH', 'zM'))
  dd = datIni |> filter(t %in% c(2030,2050))
  dd = data.table(dd)

  # consider the average deaths by cb, reg, year, climate policy, poll --> the impact function does not play a role any more
  dd_med = aggregate(dd$value, by = list(cb_group=dd$cb_group, Regions=dd$Regions,
                                          t=dd$t, scen=dd$scen, pollutant=dd$pollutant,
                                          ci_level=dd$ci_level, zz=dd$zz), FUN=median)
  dd_med = data.frame(dd_med)

  # consider the overall deaths: O3 + PM25 deaths
  dd_sum = aggregate(dd_med$x, list(cb_group=dd_med$cb_group, Regions=dd_med$Regions,
                                     t=dd_med$t, scen=dd_med$scen, ci_level=dd_med$ci_level,
                                     zz=dd_med$zz), FUN=sum)
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

  # copy the ref value for percentile for each region-year pair
  dd_all = dd_all %>%
    pivot_wider(names_from = scen, values_from = c(vmed,v05,v95))
  dd_all = data.table(dd_all)
  dd_all_full = data.frame()
  for (reg in unique(dd_all$Regions)) {
    for (yr in unique(dd_all$t)) {
      dd_all_tmp <- dd_all[Regions == reg & t == yr,
                             .(v05_REF_full = unique(na.omit(dd_all[Regions == reg & t == yr & cb_group == '>2000']$v05_REF)),
                               vmed_REF_full = unique(na.omit(dd_all[Regions == reg & t == yr & cb_group == '>2000']$vmed_REF)),
                               v95_REF_full = unique(na.omit(dd_all[Regions == reg & t == yr & cb_group == '>2000']$v95_REF))),
                             by = c('t','Regions')]

      dd_all_full = rbind(dd_all_full,dd_all_tmp)
    }
  }
  dd_all_full2 = merge(dd_all, dd_all_full, by = c('t','Regions'))

  return(dd_all_full2)
}

#cp = climate policy (NZ or EoC)
doM_av_deaths_map_select_climate_policy = function(dat,cp,map) {

  if (cp == 'NZ') {
    dd_sel = data.frame(t = dat$t,
                        Regions = dat$Regions,
                        cb_group = dat$cb_group,
                        v05 = dat$v05_NZ,
                        vmed = dat$vmed_NZ,
                        v95 = dat$v95_NZ,
                        v05_REF_full = dat$v05_REF_full,
                        vmed_REF_full = dat$vmed_REF_full,
                        v95_REF_full = dat$v95_REF_full)
  } else if (cp == 'EoC') {
    dd_sel = data.frame(t = dat$t,
                        Regions = dat$Regions,
                        cb_group = dat$cb_group,
                        v05 = dat$v05_EoC,
                        vmed = dat$vmed_EoC,
                        v95 = dat$v95_EoC,
                        v05_REF_full = dat$v05_REF_full,
                        vmed_REF_full = dat$vmed_REF_full,
                        v95_REF_full = dat$v95_REF_full)
  } else {
    dd_sel = data.frame(t = dat$t,
                        Regions = dat$Regions,
                        cb_group = dat$cb_group,
                        v05 = dat$v05_REF,
                        vmed = dat$vmed_REF,
                        v95 = dat$v95_REF,
                        v05_REF_full = dat$v05_REF_full,
                        vmed_REF_full = dat$vmed_REF_full,
                        v95_REF_full = dat$v95_REF_full)
  }
  dd_sel = na.omit(dd_sel)

  dd_sel$v05_comp    = dd_sel$v05_REF_full - dd_sel$v05
  dd_sel$vmed_comp   = dd_sel$vmed_REF_full - dd_sel$vmed
  dd_sel$v95_comp    = dd_sel$v95_REF_full - dd_sel$v95

  if(map) {
    dd = dd_sel %>%
      pivot_longer(c("v05_comp","vmed_comp","v95_comp"), names_to = "per_comp", values_to = "val_comp")
    dat = data.table(dd)
    dat[per_comp == "v05_comp", per_label := '5th']
    dat[per_comp == "vmed_comp", per_label := 'median']
    dat[per_comp == "v95_comp", per_label := '95th']
    dat[, per_comp := factor(per_label, levels = c('5th','median','95th'))]
    dat = as_tibble(dat)
  } else {
    dat = dd_sel
  }
  return(dat)
}

doM_dat_world_merge_single_plot = function(dat,cp,yr,save) {
  # obtain regions
  wreg <- read_csv(file.path(data_path,'WITCH','mapwitch10.csv'),show_col_types = FALSE)
  # transforms ISO codes into country names
  wreg <- wreg |>
    mutate(region = countrycode(ISO, origin = 'iso3c', destination = 'country.name'))
  # Correcting some mismatches
  wreg['ISO'=='ANT']$region = 'Netherlands Antilles'


  world <- ne_countries(scale = "small", returnclass = "sf")
  world <- subset(world,!adm0_a3 %in% c("ATA","FJI"))

  # merge the regional definition with the world map
  world <- merge(world,wreg, by.x = "adm0_a3", by.y = "ISO")
  # merge with the emission data.frame
  r10_nmap <- merge(dat, nmap, by.x="Regions", by.y="n")
  world0 <- merge(world, r10_nmap, by.x = 'n', by.y='Regions', allow.cartesian=TRUE)

  # Remove small islands
  target_crs <- '+proj=eqearth +wktext'
  world1 <- st_transform(world0, crs = target_crs)

  tt = ifelse(save, paste(cp,'climate policy,',yr), paste(cp, 'climate policy'))
  pl <- ggplot(data = world1 |> filter(t == yr)) +
    geom_sf(aes(fill = val_comp)) +
    coord_sf(datum = target_crs,
             expand = FALSE,
             clip = "off") +
    facet_grid(cb_group ~ per_comp) +
    scale_fill_gradient("Avoided deaths   \n[in thousands]", low = "#e6d329", high = "#d6290b") +
    labs(title = tt) +
    theme_void() +
    theme(legend.position = c(0.5,-0.1),
          legend.key.width = unit(2, 'cm'),
          legend.key.height = unit(0.7, 'cm'),
          legend.direction = "horizontal",
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          strip.text.x = element_text(vjust = 1, size = 14),
          strip.text.y = element_text(size = 14),
          plot.title = element_text(hjust = 0.5)) +
    rotate_y_facet_text(angle = 270, align = 0.5, valign = 0.5)


  if(save) {
    if(!dir.exists(file.path('Results/Mort/AvDeaths'))){
      dir.create(file.path('Results/Mort/AvDeaths'))
    }

    pl = pl + theme(plot.title = element_text(hjust = 0.5, vjust = 1.5,size = 16))
    name_file = paste("Results/Mort/AvDeaths",paste(paste('av_deaths',cp,yr,sep='_'),'png',sep='.'),sep='/')
    ggsave(file=name_file, width = 350, height = 200, units = "mm",plot = pl, limitsize = FALSE)
  } else {
    return(pl)
  }
}

doM_FIGURE_av_deaths_map_by_climate_policy_year = function(dat0) {
  dat0 = dat0[t %in% c(2030,2050)]
  dat1 = doM_av_deaths_map_preprocess(dat0)
  #plot and save single maps
  for (sc in unique(dat0$scen)) {
    for (yr in unique(dat0$t)) {
      dat2 = doM_av_deaths_map_select_climate_policy(dat1,sc,T)
      doM_dat_world_merge_single_plot(dat2,sc,yr,T)
    }
  }
}

doM_FIGURE_av_deaths_map_by_year = function(dat0) {
  dat0 = dat0[t %in% c(2030,2050)]
  dat1 = doM_av_deaths_map_preprocess(dat0)
  # figure by year
  for (yr in unique(dat0$t)) {
    print(yr)

    dat2 = doM_av_deaths_map_select_climate_policy(dat1,'NZ',T)
    pl_NZ = doM_dat_world_merge_single_plot(dat2,'NZ',yr,F)
    dat2 = doM_av_deaths_map_select_climate_policy(dat1,'EoC',T)
    pl_EoC = doM_dat_world_merge_single_plot(dat2,'EoC',yr,F)

    fig = ggarrange(pl_NZ + font("title", size = 0),
                    pl_EoC + font("title", size = 0),

                    common.legend = TRUE, legend="bottom",
                    labels = c('NZ','EoC'),
                    font.label = list(size = 16),
                    ncol = 2, nrow = 1,
                    widths = c(1,1))

    fig = annotate_figure(fig,
                          top = text_grob(paste0("Avoided deaths in ", yr), color = "black", face = "bold", size = 20))

    if(!dir.exists(file.path('Results/Mort/AvDeaths'))){
      dir.create(file.path('Results/Mort/AvDeaths'))
    }

    name_file_numD = paste("Results/Mort/AvDeaths",paste(paste0('av_deaths_',yr,'_map'),'png',sep='.'),sep='/')
    ggsave(file=name_file_numD, width = 500, height = 150, units = "mm",plot = fig, limitsize = FALSE)
  }
}

doM_FIGURE_av_deaths_map = function(dat0) {
  dat0 = dat0[t %in% c(2030,2050)]
  dat1 = doM_av_deaths_map_preprocess(dat0)

  # unique figure
  dat2 = doM_av_deaths_map_select_climate_policy(dat1,'NZ',T)
  pl_NZ_2030 = doM_dat_world_merge_single_plot(dat2,'NZ',2030,F)
  pl_NZ_2050 = doM_dat_world_merge_single_plot(dat2,'NZ',2050,F)
  dat2 = doM_av_deaths_map_select_climate_policy(dat1,'EoC',T)
  pl_EoC_2030 = doM_dat_world_merge_single_plot(dat2,'EoC',2030,F)
  pl_EoC_2050 = doM_dat_world_merge_single_plot(dat2,'EoC',2050,F)

  fig = ggarrange(pl_NZ_2030 + font("title", size = 14, color = "black", face = "bold"),
                  pl_EoC_2030 + font("title", size = 14, color = "black", face = "bold"),

                  pl_NZ_2050 + font("title", size = 14, color = "black", face = "bold"),
                  pl_EoC_2050 + font("title", size = 14, color = "black", face = "bold"),

                  common.legend = TRUE, legend="bottom",
                  labels = c('2030','','2050'),
                  ncol = 2, nrow = 2,
                  widths = c(1,1))

  fig = annotate_figure(fig,
                        top = text_grob(paste0("Avoided deaths"), color = "black", face = "bold", size = 22))

  if(!dir.exists(file.path('Results/Mort/AvDeaths'))){
    dir.create(file.path('Results/Mort/AvDeaths'))
  }
  name_file_whole = paste("Results/Mort/AvDeaths",paste('av_deaths_whole_map','png',sep='.'),sep='/')
  ggsave(file=name_file_whole, width = 500, height = 300, units = "mm",plot = fig, limitsize = FALSE)
}

doM_FIGURE_av_deaths_table = function(dat0) {
  dat0 = dat0[t %in% c(2030,2050)]
  dat1 = doM_av_deaths_map_preprocess(dat0)

  # whole figure
  dat2_NZ = doM_av_deaths_map_select_climate_policy(dat1,'NZ',map=F)
  dat2_NZ = data.table(dat2_NZ)
  dat2_NZ = dat2_NZ[order(dat2_NZ$Regions), ]
  dat2_NZ$Regions = fct_rev(dat2_NZ$Regions)
  dat2_NZ = data.table(dat2_NZ)
  pl_nz_2030 = do_av_deaths_table(dat2_NZ,'NZ',2030,T)
  pl_nz_2050 = do_av_deaths_table(dat2_NZ,'NZ',2050,T)

  dat2_EoC = doM_av_deaths_map_select_climate_policy(dat1,'EoC',map=F)
  dat2_EoC = data.table(dat2_EoC)
  dat2_EoC = dat2_EoC[order(dat2_EoC$Regions), ]
  dat2_EoC$Regions = fct_rev(dat2_EoC$Regions)
  dat2_EoC = data.table(dat2_EoC)
  pl_eoc_2030 = do_av_deaths_table(dat2_EoC,'EoC',2030,F)
  pl_eoc_2050 = do_av_deaths_table(dat2_EoC,'EoC',2050,F)

  fig = ggarrange(pl_nz_2030 + rremove('xlab') + rremove('ylab'),
                  pl_eoc_2030 + rremove('xlab') + rremove('ylab'),

                  pl_nz_2050 + rremove('xlab') + rremove('ylab'),
                  pl_eoc_2050 + rremove('xlab') + rremove('ylab'),

                  common.legend = TRUE, legend="right",
                  labels = c('2030','','2050'),
                  ncol = 2, nrow = 2,
                  widths = c(1,0.83))

  fig = annotate_figure(fig,
                        top = text_grob(paste0("Avoided deaths"), color = "black", face = "bold", size = 14),
                        left = text_grob("Regions", color = "black", size = 10, rot = 90),
                        bottom = text_grob("Carbon budget [GtCO2]", color = "black", size = 10))

  if(!dir.exists(file.path('Results/Mort/AvDeaths'))){
    dir.create(file.path('Results/Mort/AvDeaths'))
  }
  name_file = paste("Results/Mort/AvDeaths",paste('av_deaths_table','png',sep='.'),sep='/')
  ggsave(file=name_file, width = 350, height = 300, units = "mm",plot = fig, limitsize = FALSE)

}

do_av_deaths_table = function(dat,cp,yr,ylab) {
  tt = paste(cp, 'climate policy')

  pl = ggplot(data = dat[t == yr], aes(x = cb_group, y = Regions)) +
    geom_tile(data = dat[t == yr & !is.na(dat$vmed_comp)], aes(fill = vmed_comp)) +
    geom_text(aes(label = paste(round(vmed_comp, 1),'\n','(',round(v05_comp, 1),',',round(v95_comp, 1),')')), size = 3.2) +
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

