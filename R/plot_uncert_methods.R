version = 'new_cb_3'
file = paste("emi_engage",version,sep='_')
load(file.path('Data',paste(file,"RData",sep='.')))
datEmi = data.table(datEmi)

version = 'v1'
file = paste("conc_fasstr",version,sep='_')
load(file.path('Data',paste(file,"RData",sep='.')))
datConc = data.table(datConc)

version = 'v23'
file = paste("mort_imp_fun",version,sep='_')
load(file.path('Data',paste(file,"RData",sep='.')))

source('zzz.R')
############################################################################

do_distrib_plot = function(datIni,reg,poll,xxbb,E) {
  dat_to_plot = datIni |> filter(year == 2030 & region == reg
                                 & pollutant == poll & scenario != 'REF'
                                 & cb_group == '<1000')

  dat_to_plot = data.table(dat_to_plot)
  datall_medi <- dat_to_plot[, .(medi = quantile(value, 0.5)),
                             by=c('year','region','pollutant','cb_group','scenario')]
  pl_title = paste(reg,poll,sep = '_')
  tt = ''

  pl <- ggplot(dat_to_plot) +
    geom_density(aes(x = value, group = interaction(scenario,pollutant,year,cb_group),
                     color = scenario, fill = scenario), alpha = 0.5) +
    geom_vline(aes(color = scenario, xintercept = medi),
               data = datall_medi, linetype="dashed", size = 1) +
    theme(panel.background = element_rect(fill = NA), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE) +
    theme(legend.position = 'bottom',
          # legend.position = c(0.5,-0.1),
          legend.key.size = unit(0.3, 'cm'),
          legend.direction = "horizontal",
          legend.text = element_text(size = 5),
          legend.title = element_text(size = 7)) +
    scale_color_manual(values = shortpal,
                       name = 'Scenario')+
    scale_fill_manual(values = shortpal,
                      name = 'Scenario')+
    guides(x = ifelse(E, guide_axis(n.dodge = 1), guide_axis(n.dodge = 2))) +
    labs(title=tt, x = xxbb, y = "Probability density")
  pl
  pl_name = ifelse(E, 'plot_uncert_E.png', 'plot_uncert_C.png')
  ggsave(width = 1362, heigh = 1115, unit = 'px', file=file.path(file_methods,'Images/',pl_name), plot = pl)
}

doM_distrib_plot = function(datIni,reg,poll,xxbb) {
  dat_to_plot = datIni |> filter(t == 2030 & Regions == reg & pollutant == poll
                                 & z_level %in% c('zUNI','zMED') & cb_group == '<1000'
                                 & impact_function_group == 'PM25MORT_BURNETT2014_UNI'
                                 & scen != 'REF')

  dat_kl = dat_to_plot[, .(medi = quantile(value, 0.5)), #median
                       by=c('scen','cb_group','pollutant','t',
                            'impact_function_group','ci_level')]
  datall = merge(dat_kl, dat_to_plot, by=c('scen','cb_group','pollutant','t',
                                           'impact_function_group','ci_level'))

  datall = data.table(datall)
  datall = do_rename_imp_fun(datall,poll_names = F,F)
  datall$cb_group <- factor(datall$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))

  pl_title = paste(poll,reg,sep='_')
  tt = ''

  pl_u_mort <- ggplot(datall) +
    geom_density(aes(x = value, group = interaction(scen,ci_level),
                     color = interaction(scen,ci_level),
                     fill = interaction(scen,ci_level)), alpha = 0.5) +
    geom_vline(data = datall[ci_level == 'ciMED'], aes(color = interaction(scen,ci_level), xintercept = medi),
               linetype="dashed", size = 1) +

    theme(panel.background = element_rect(fill = NA), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE) +
    theme(legend.position = 'bottom',
          # legend.position = c(0.5,-0.1),
          legend.key.size = unit(0.3, 'cm'),
          legend.direction = "horizontal",
          legend.text = element_text(size = 5),
          legend.title = element_text(size = 7)) +
    scale_fill_manual(values = longpal,
                      name = 'Scenario and 95% CI',
                      labels = longlabs)+
    scale_color_manual(values = longpal,
                       name = 'Scenario and 95% CI',
                       labels = longlabs)+
    theme(strip.text.y = element_text(size = 7)) +
    labs(title=tt, x = xxbb, y = "Probability density")
  pl_u_mort
  ggsave(width = 1362, heigh = 1115, unit = 'px', file=file.path(file_methods,'Images/','plot_uncert_M.png'), plot = pl_u_mort)
}



###############################################################################
do_distrib_plot(datEmi,'R10INDIA+','BC+OC','Emissions [kg/year]',T)
do_distrib_plot(datConc,'R10INDIA+','PM25',expression(paste('Concentrations [',mu,g,m^-3,']')),F)
doM_distrib_plot(datMort,'R10INDIA+','PM25','Deaths')
