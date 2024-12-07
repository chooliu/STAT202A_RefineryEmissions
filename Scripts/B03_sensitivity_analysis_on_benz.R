# iterate days pad on daily model ---------------------------------------------

sens_dayspad_benzene_daily <- function(dayspad_before, dayspad_after) {

# parameter: days pad from origin
  
  target_integers_benzene <-
    dates_benzene %>% as.Date %>% as.integer()
  
  target_integers_benzene <-
    sapply(target_integers_benzene, function(x) {
      seq(x - dayspad_before, x + dayspad_after)}) %>% as.numeric
  
  
  # # pull out interpolated 2D to look at effects
  # set.seed(1234)
  # dat_benzene %>%
  #   filter(Date %in% as.integer(target_integers_benzene)) %>%
  #   .$Date %>% unique %>% sample(., size = 4) %>%
  #   map(make_interpolation_plot_benzene) %>%
  #   map(~ .x + delete_axes) %>%
  #   plot_grid(plotlist = .)
  
  
  
  # setup modeling parameters ----------------------------------------------------
  # join with other external datasets (exceedance, weather)
  
  dat_benzene_daily <-
    dat_benzene_daily %>%
    mutate(EmissionReported = Date %in%
             as.Date(target_integers_benzene, origin = lubridate::origin) %>%
             if_else(., "yes", "no") %>% as.factor) %>%
    mutate(DateInt = as.integer(Date),
           DateIntNorm = DateInt - offset_integer_benzenefence)
  table(dat_benzene_daily$EmissionReported) 
  
  dat_benzene_daily <-
    dat_benzene_daily %>%
    select(-intersect(names(.), names(weather)[-1])) %>%
    left_join(., weather, by = "Date") %>%
    mutate(AnyPercip = if_else(Percip > 0 | Snow > 0, "yes", "no"))
  

model_benzene_weather <-
  gam(PolluteConc ~ TempMax + Percip + Snow +
        s(DateIntNorm, bs = 'bs', k = 25) + EmissionReported,
      data = dat_benzene_daily, family = Gamma(link = 'log'))

return(list(num_events = table(dat_benzene_daily$EmissionReported),
       regtable = summary_table_from_gam(model_benzene_weather, display = F)))

}



sensres_benzene <-
  tibble(dayspad_before = 0:13,
       dayspad_after = 0:13) %>%
pmap(sens_dayspad_benzene_daily)



sensres_benzene %>%
  map_dfr( ~ .x$regtable %>% filter(coef == "EmissionReportedyes")) %>%
  bind_cols(num_events = 
              sensres_benzene %>%
              map_int(~ .x$num_events["yes"])) %>%
  bind_cols(  tibble(DaysPadded = as.factor( 0:13 )) , .) %>%
  ggplot(data = ., aes(DaysPadded, fc)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = fc_low, ymax = fc_high), width = 0.25) +
  geom_hline(yintercept = 1, lty = 3) +
  geom_text(aes(label = num_events), nudge_x = 0.3, nudge_y = 0.05) +
  theme_few() +
  scale_x_discrete("+/- Days Padding\nFrom Emission Report") +
  ylab("Effect Size, Documented Emission\n(Fold Change, 95% CI)")


sensres_benzene %>%
  map_dfr( ~ .x$regtable %>% filter(coef == "Percip")) %>%
  bind_cols(num_events = 
              sensres_benzene %>%
              map_int(~ .x$num_events["yes"])) %>%
  bind_cols(  tibble(DaysPadded = as.factor( 0:10 )) , .) %>%
  ggplot(data = ., aes(DaysPadded, fc)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = fc_low, ymax = fc_high), width = 0.25) +
  geom_hline(yintercept = 1, lty = 3) +
  geom_text(aes(label = num_events), nudge_x = 0.25) +
  theme_few() +
  scale_x_discrete("+/- Days Padding\nFrom Emission Report") +
  ylab("Effect Size, Reported Emission\n(Fold Change, 95% CI)")







# iterate days pad on random fx model ------------------------------------------

sens_dayspad_benzene_re <- function(dayspad_before, dayspad_after) {
  
  # parameter: days pad from origin
  
  target_integers_benzene <-
    dates_benzene %>% as.Date %>% as.integer()
  
  target_integers_benzene <-
    sapply(target_integers_benzene, function(x) {
      seq(x - dayspad_before, x + dayspad_after)}) %>% as.numeric
  
  
  dat_benzene <-
    dat_benzene %>%
    select(-intersect(names(.), names(weather)[-1])) %>%
    left_join(., weather, by = "Date") %>%
    mutate(AnyPercip = if_else(Percip > 0 | Snow > 0, "yes", "no")) %>%
    mutate(EmissionReported = Date %in%
             as.Date(target_integers_benzene, origin = lubridate::origin) %>%
             if_else(., "yes", "no") %>% as.factor)

  model_benzene_random_weatherdetailed <-
    gam(PolluteConc ~ TempMax + Percip + Snow +
          s(DateIntNorm, bs = 'bs', k = 100) +
          s(MonitorID, bs = 're') + s(EmissionReported, MonitorID, bs = 're') +
          EmissionReported,
        data = dat_benzene, family = Gamma(link = 'log'))

  return(list(num_events = table(dat_benzene$EmissionReported),
              regtable = summary_table_from_gam(model_benzene_random_weatherdetailed, display = F)))
  
}


sensres_benzene <-
  tibble(dayspad_before = 0:13,
         dayspad_after = 0:13) %>%
  pmap(sens_dayspad_benzene_re)



sensres_benzene %>%
  map_dfr( ~ .x$regtable %>% filter(coef == "EmissionReportedyes")) %>%
  bind_cols(num_events = 
              sensres_benzene %>%
              map_int(~ .x$num_events["yes"])) %>%
  bind_cols(  tibble(DaysPadded = as.factor( 0:13 )) , .) %>%
  ggplot(data = ., aes(DaysPadded, fc)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = fc_low, ymax = fc_high), width = 0.25) +
  geom_hline(yintercept = 1, lty = 3) +
  geom_text(aes(label = num_events), nudge_x = 0.33) +
  theme_few() +
  scale_x_discrete("+/- Days Padding\nFrom Emission Report") +
  ylab("Effect Size, Reported Emission\n(Fold Change, 95% CI)")

