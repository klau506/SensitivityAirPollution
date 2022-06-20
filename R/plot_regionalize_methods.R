# plot engage db with world map
library(ggplot2)
library(data.table)
library(dplyr)
library(viridis)
library(tidyverse)
library(countrycode)
library(rnaturalearth)
library(sf)

load(paste('data',"mmortot_newimpfun_EN_INDCi2030_800f-WITCH_5_0.RData",sep='/'))
load(paste('data',"concfield_EN_INDCi2030_500f-WITCH_5_0.RData",sep='/'))
dat_eng_proxy = fread(paste('data',"dat_engage.csv",sep='/'))
load(paste('data',"mask3618.RData"))
source('zzz.R')
#############################################################################

plot_emi = function(datIni) {
  # decide what to plot
  dat = datIni |> filter(year == 2030 &
                                  model == 'WITCH 5.0' &
                                  scen_complete_name == 'EN_INDCi2030_800f' &
                                  pollutant %in% c('BC') & region != 'WORLD')

  # obtain regions
  wreg <- read_csv(file.path('data','mapwitch10.csv'),show_col_types = FALSE)
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
  r10_nmap <- merge(dat, nmap, by.x="REGION", by.y="n")
  world0 <- merge(world, r10_nmap, by.x = 'adm0_a3', by.y='ISOCODE', allow.cartesian=TRUE)

  # Remove small islands
  target_crs <- '+proj=eqearth +wktext'
  world1 <- st_transform(world0, crs = target_crs)

  #regionalized
  p_eng <- ggplot(data = world1) +
    geom_sf(aes(fill = value_ogl)) +
    coord_sf(datum = target_crs,
             expand = FALSE,
             clip = "off") +
    theme_void() +
    scale_fill_viridis_c(option = "plasma", name = "Emissions [kg/year]") +
    theme(legend.position = c(0.5,-0.1),
          legend.key.size = unit(0.3, 'cm'),
          legend.direction = "horizontal",
          legend.text = element_text(size = 5),
          legend.title = element_text(size = 7))

  if(!dir.exists(file.path('Results/Methods'))){
    dir.create(file.path('Results/Methods'))
  }
  ggsave(width = 1362, heigh = 1115, unit = 'px', file=file.path('Results/Methods','plot_engage.png'), plot = p_eng)
}

plot_regions = function(datIni) {
  # decide what to plot
  dat = datIni |> filter(year == 2030 &
                           model == 'WITCH 5.0' &
                           scen_complete_name == 'EN_INDCi2030_800f' &
                           pollutant %in% c('BC'))

  # obtain regions
  wreg <- read_csv(file.path('data','mapwitch10.csv'),show_col_types = FALSE)
  # transforms ISO codes into country names
  wreg <- wreg |>
    mutate(region = countrycode(ISO, origin = 'iso3c', destination = 'country.name'))
  # Correcting some mismatches
  wreg['ISO'=='ANT']$region = 'Netherlands Antilles'



  world <- ne_countries(scale = "small", returnclass = "sf")
  world <- subset(world,!adm0_a3 %in% c("ATA","FJI"))
  # merge the WITCH regional definition with the world map
  # world <- merge(world,dat, by.x = "adm0_a3", by.y = "ISOCODE")

  # merge the regional definition with the world map
  world <- merge(world,wreg, by.x = "adm0_a3", by.y = "ISO")
  # merge with the emission data.frame
  r10_nmap <- merge(dat, nmap, by.x="REGION", by.y="n")
  world0 <- merge(world, r10_nmap, by.x = 'adm0_a3', by.y='ISOCODE', allow.cartesian=TRUE)

  # Remove small islands
  target_crs <- '+proj=eqearth +wktext'
  world1 <- st_transform(world0, crs = target_crs)

  p_reg <- ggplot(data = world1) +
    geom_sf(aes(fill = n)) +
    coord_sf(datum = target_crs,
             expand = FALSE,
             clip = "off") +
    theme_void() +
    scale_fill_manual(values = regionspal, name = "Regions") +
    theme(legend.position = c(0.5,-0.2),
          legend.key.size = unit(0.3, 'cm'),
          legend.direction = "horizontal",
          legend.text = element_text(size = 5),
          legend.title = element_text(size = 7))

  if(!dir.exists(file.path('Results/Methods'))){
    dir.create(file.path('Results/Methods'))
  }
  ggsave(width = 1362, heigh = 1115, unit = 'px', file=file.path('Results/Methods','regions.png'), plot = p_reg)
}


plot_conc = function(datIni) {
  #select all the PM25 components
  c_pm = datIni %>%
    pivot_wider(names_from = aaqe, values_from = tot)
  c_pm = data.table(c_pm)
  c_pm$TOT_PM25 = c_pm$DNH4 + c_pm$DNO3_A + c_pm$DSO4 + c_pm$DBC +   #bc_enh
    c_pm$DPOM +                                        #pom_enh
    c_pm$dupm25 + c_pm$sspm25 +                        #natural pm
    c_pm$pmsec +                                       #secondary pm
    c_pm$h2o_35                                        #athmosphere impact
  conc_susbset_pm25 = data.frame(c_pm$nb, c_pm$t, c_pm$TOT_PM25)
  names(conc_susbset_pm25) <- c("nb","year","tot")
  conc_susbset_pm25$aaqe = 'PM25'
  conc_susbset_pm25 = data.table(conc_susbset_pm25)

  # select what to plot
  yr = 2030
  qemi <- subset(conc_susbset_pm25,year == yr)

  # apply the mask
  r10conc=data.table()
  for(rr in nmap$nn){
    print(rr)
    maks=apply(SMALL_MASK, 2, rev)
    maks[maks!=rr]=0
    maks[maks==rr]=1
    mask_applied=qemi$tot*maks
    r10conct=data.table(value=mean(mask_applied[mask_applied>0]),n=nmap[nn==rr])
    r10conc = rbind(r10conc,r10conct)
  }
  names(r10conc) <- c('value','nn','REGION')


  # obtain regions
  wreg <- read_csv(file.path('data','mapwitch10.csv'),show_col_types = FALSE)
  # transforms ISO codes into country names
  wreg <- wreg |>
    mutate(region = countrycode(ISO, origin = 'iso3c', destination = 'country.name'))
  # Correcting some mismatches
  wreg['ISO'=='ANT']$region = 'Netherlands Antilles'


  # plot
  world <- ne_countries(scale = "small", returnclass = "sf")
  world <- subset(world,!adm0_a3 %in% c("ATA","FJI"))
  # merge the WITCH regional definition with the world map
  world <- merge(world,wreg, by.x = "adm0_a3", by.y = "ISO")
  # merge with the emission data.frame
  r10conc_nmap <- merge(r10conc, nmap, by="nn")
  # r10conc_nmap <- merge(r10conc, nmap, by.x = "REGION", by.y="n")  # does the same as the line above. Maybe more simple :)
  alldatr10 <- data.frame(r10conc_nmap$nn, r10conc_nmap$value, r10conc_nmap$REGION)
  names(alldatr10) <- c('n','value','REGION')
  world0 <- merge(alldatr10, world, by.x = 'REGION', by.y = "n", allow.cartesian=TRUE)
  # Use a better projection 'equal projection'
  target_crs <- '+proj=eqearth +wktext'
  world1 <- st_as_sf(world0)
  world1 <- st_transform(world1, crs = target_crs)

  p_conc <- ggplot(data = world1) +
    geom_sf(aes(fill = value)) +
    coord_sf(datum = target_crs,
             expand = FALSE,
             clip = "off") +
    theme_void() +
    scale_fill_viridis_c(option = "plasma",name = expression(paste("Concentrations [",mu,g,m^-3,']'))) +
    theme(legend.position = c(0.5,-0.1),
          legend.key.size = unit(0.3, 'cm'),
          legend.direction = "horizontal",
          legend.text = element_text(size = 5),
          legend.title = element_text(size = 7))

  if(!dir.exists(file.path('Results/Methods'))){
    dir.create(file.path('Results/Methods'))
  }
  ggsave(width = 1362, heigh = 1115, unit = 'px', file=file.path('Results/Methods','plot_conc.png'), plot = p_conc)
}

plot_mort = function(datIni) {
  qemi <- MMorttot  |> filter(t == 1)
  impfun_name <- 'CTOT_PM25MORT_BURNETT2014_ciMED_zMED_SC'
  # obtain mortality values
  r10mort = data.frame(get(impfun_name,qemi), qemi$Regions)
  names(r10mort) <- c('value','REGION')

  # obtain regions
  wreg <- read_csv(file.path('data','mapwitch10.csv'),show_col_types = FALSE)
  # transforms ISO codes into country names
  wreg <- wreg |>
    mutate(region = countrycode(ISO, origin = 'iso3c', destination = 'country.name'))
  # Correcting some mismatches
  wreg['ISO'=='ANT']$region = 'Netherlands Antilles'

  # plot
  world <- ne_countries(scale = "small", returnclass = "sf")
  world <- subset(world,!adm0_a3 %in% c("ATA","FJI"))
  # merge the WITCH regional definition with the world map
  world <- merge(world,wreg, by.x = "adm0_a3", by.y = "ISO")
  # merge with the emission data.frame
  r10mort_nmap <- merge(r10mort, nmap, by.x="REGION", by.y="n")
  world0 <- merge(r10mort_nmap, world, by.x = 'REGION', by.y = "n", allow.cartesian=TRUE)
  # Use a better projection 'equal projection'
  target_crs <- '+proj=eqearth +wktext'
  world1 <- st_as_sf(world0)
  world1 <- st_transform(world1, crs = target_crs)

  pl_mort <- ggplot(data = world1) +
    geom_sf(aes(fill = value)) +
    coord_sf(datum = target_crs,
             expand = FALSE,
             clip = "off") +
    theme_void() +
    scale_fill_viridis_c(option = "plasma",name = 'Deaths') +
    theme(legend.position = c(0.5,-0.1),
          legend.key.size = unit(0.3, 'cm'),
          legend.direction = "horizontal",
          legend.text = element_text(size = 5),
          legend.title = element_text(size = 7))

  if(!dir.exists(file.path('Results/Methods'))){
    dir.create(file.path('Results/Methods'))
  }
  ggsave(width = 1362, heigh = 1115, unit = 'px', file=file.path('Results/Methods','plot_mort.png'), plot = pl_mort)
}
##############################################################
# Run functions
plot_regions(dat_eng_proxy)
plot_emi(dat_eng_proxy)
plot_conc(concfield)
plot_mort(MMorttot)
