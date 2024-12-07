# B02_benzene_modeling.R =======================================================

# pad dates of emission reports due to sparsity of events? ---------------------
# parameter: days pad from origin

dayspad_before <- 2
dayspad_after <- 2

target_integers_benzene <-
  dates_benzene %>% as.Date %>% as.integer()

target_integers_benzene <-
  sapply(target_integers_benzene, function(x) {
    seq(x - dayspad_before, x + dayspad_after)}) %>% as.numeric

# pull out interpolated 2D to look at effects
set.seed(1234)
dat_benzene %>%
  filter(Date %in% as.integer(target_integers_benzene)) %>%
  arrange(-PolluteConc) %>%
  .$Date %>% unique %>% # sample(., size = 4) %>%
  map(make_interpolation_plot_benzene) %>%
  map(~ .x + delete_axes) %>%
  plot_grid(plotlist = .)




# setup modeling parameters ----------------------------------------------------
# join with other external datasets (exceedance, weather)


dat_benzene <-
  dat_benzene %>%
  select(-intersect(names(.), names(weather)[-1])) %>%
  left_join(., weather, by = "Date") %>%
  mutate(AnyPercip = if_else(Percip > 0 | Snow > 0, "yes", "no")) %>%
  mutate(EmissionReported = Date %in%
           as.Date(target_integers_benzene, origin = lubridate::origin) %>%
           if_else(., "yes", "no") %>% as.factor)

# aggregate by day (ignore monitor effect)
dat_benzene_daily <-
  dat_benzene %>%
  group_by(Date) %>%
  summarize(PolluteConc = quantile(PolluteConc, 0.90),
            DateInt = first(DateInt),
            DateIntNorm = first(DateIntNorm),
            EmissionReported = first(EmissionReported),
            AnyPercip = first(AnyPercip),
            Percip = first(Percip),
            Snow = first(Snow),
            TempMax = first(TempMax))

dim(dat_benzene)
dim(dat_benzene_daily)


# number of events?
unique(dat_benzene_daily %>% filter(EmissionReported == 'yes') %>% .$Date) %>% length
dat_benzene_daily %>% .[complete.cases(.), ] %>% .$Date %>% unique %>% length
table(dat_benzene_daily$EmissionReported)

# run GAMs, day-aggreg ---------------------------------------------------------

# emission report only, no weather

wrap_gam <- function(formulain, datin) {
  model <- gam(formulain, data = datin, family = Gamma(link = 'log'))
  return(list(
    model = model,
    summary = summary(model),
    gamcheck = function() { par(mfrow = c(2, 2)); gam.check(model) },
    ar = plot_autocorr(model),
    summarytable = summary_table_from_gam(model),
    summarytable_print = summary_table_from_gam(model, display = T)
  ))
  
}

# baseline
# ~ time + emission
mod_benzene_baseline <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = 'bs', k = 30) + EmissionReported,
         dat_benzene_daily)

# weathersimple
# ~ time + emission + temp + anypercip
mod_benzene_weathersimple <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = 'bs', k = 30) + EmissionReported +
             TempMax + AnyPercip,
           dat_benzene_daily)

# weatherdetailed
# ~ time + emission + temp + percip + snow
mod_benzene_weatherdetailed <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = 'bs', k = 30) + EmissionReported +
             TempMax + Percip + Snow,
           dat_benzene_daily)

# weatherdetailed
# ~ time + emission + temp + percip + snow + emission*percip
mod_benzene_weatherinteract <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = 'bs', k = 30) + EmissionReported +
             TempMax + Percip + Snow + EmissionReported*Percip,
           dat_benzene_daily)



# run GAMs, random effect on monitor -------------------------------------------

# baseline
# ~ time + emission + (1|monitor)
modre_benzene_baseline <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = 'bs', k = 75) + EmissionReported +
             s(MonitorID, bs = 're'),
           dat_benzene)

# weathersimple
# ~ time + emission + temp + anypercip + (1|monitor)
modre_benzene_weathersimple <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = 'bs', k = 75) + EmissionReported +
             TempMax + AnyPercip + s(MonitorID, bs = 're'),
           dat_benzene)

# weatherdetailed
# ~ time + emission + temp + percip + snow + (1|monitor)
modre_benzene_weatherdetailed <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = 'bs', k = 75) + EmissionReported +
             TempMax + Percip + Snow + s(MonitorID, bs = 're'),
           dat_benzene)

# weatherdetailed
# ~ time + emission + temp + percip + snow + emission*percip + (1|monitor)
modre_benzene_weatherinteract <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = 'bs', k = 75) + EmissionReported +
             TempMax + Percip + Snow + EmissionReported*Percip +
             s(MonitorID, bs = 're'),
           dat_benzene)

# weatherdetailed
# ~ time + emission + temp + percip + snow + emission*percip + (1|monitor)
modre_benzene_weatherslopeman <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = 'bs', k = 75) + EmissionReported +
             TempMax + Percip + Snow +
             s(MonitorID, bs = 're') +
             s(MonitorID, DateIntNorm, bs = 're'),
           dat_benzene)


# compare models overall -------------------------------------------------------

# mod_benzene_weatherdetailed
ls(pattern = "mod_benzene*") %>%
  paste0(., '$model') %>%
  paste0(collapse = ", ") %>%
  paste0("BIC(", ., ")") %>%
  parse(text = .) %>% eval %>%
  as_tibble(rownames = "modelname") %>%
  arrange(BIC)

# modre_benzene_weatherdetailed
ls(pattern = "modre_benzene*") %>%
  paste0(., '$model') %>%
  paste0(collapse = ", ") %>%
  paste0("BIC(", ., ")") %>%
  parse(text = .) %>% eval %>%
  as_tibble(rownames = "modelname") %>%
  arrange(BIC)

mod_benzene <- mod_benzene_weatherdetailed$model
modre_benzene <- modre_benzene_weatherdetailed$model

ls(pattern = "modre_benzene*") %>% sapply(., rm)
ls(pattern = "mod_benzene*") %>% sapply(., rm)



# plotting final estimates, day-aggreg -----------------------------------------

mod_benzene_weatherdetailed$summarytable_print

plot_predictions(mod_benzene_weatherdetailed$model,
                 condition = c('DateIntNorm', 'Percip'),
                 type = 'response') +
  theme_few()

plot_predictions(mod_benzene_weatherdetailed$model,
                 condition = c('DateIntNorm', 'EmissionReported'),
                 type = 'response') + 
  theme_few()

plot_predictions(model_benzene_random,
                 condition = c('DateIntNorm', 'TempMax'), type = 'response') +
  theme_few()


target_xindices <-
  dat_benzene_daily$Date %>%
  length %>%
  seq(1, ., length.out = 4) %>% round(.)

plot_predictions(mod_benzene_weatherdetailed$model,
                 condition = c('DateIntNorm'), type = 'response') +
  theme_few() +
  geom_point(data = dat_benzene_daily,
             aes(DateIntNorm, PolluteConc,
                 fill = EmissionReported, shape = EmissionReported, size = EmissionReported),
             inherit.aes = F) +
  geom_point(size = 0.25, alpha = 1) +
  geom_vline(xintercept = paste0(2021:2024, "-01-01") %>% as.Date() %>% as.integer() %>% `-`(offset_integer_benzenefence),
             lty = 1, alpha = 0.25) +
  geom_rug(data = tibble(emission = dates_benzene %>% as.integer() %>% `-`(offset_integer_benzenefence)),
           aes(x = emission),
           inherit.aes = F) +
  scale_x_continuous("Date",
                     breaks = dat_benzene_daily$DateIntNorm[target_xindices],
                     labels = dat_benzene_daily$Date[target_xindices] %>% format.Date("%Y-%b")) +
  ylab(label_benzene) +
  scale_color_manual(name = "Reportable\nEvent", values = palette_color_emissionreport) +
  scale_fill_manual(name = "Reportable\nEvent", values = palette_color_emissionreport) +
  scale_shape_manual(name = "Reportable\nEvent", values = palette_shape_emissionreport) +
  scale_size_manual(name = "Reportable\nEvent", values = c(no = 1, yes = 3))




# is it rare to get above 4?
wrap_gausskern(dat_benzene_daily$PolluteConc,
               grid_benz) %>% cumsum %>% .[grid_benz == 4] %>%
  `/`(sum(wrap_gausskern(dat_benzene_daily$PolluteConc,
                      grid_benz)))



# plotting final estimates, random effect for monitor id -----------------------


plot_predictions(model_benzene_random,
                 condition = c('DateIntNorm', 'MonitorID', 'MonitorID'), type = 'response') +
  theme_few() +
  geom_point(data = dat_benzene,
             aes(DateIntNorm, PolluteConc,
                 fill = EmissionReported, shape = EmissionReported, size = EmissionReported),
             inherit.aes = F) +
  geom_vline(xintercept = dates_benzene %>% as.integer() %>% `-`(., offset_integer_benzenefence),
             lty = 3, alpha = 0.5) +
  scale_x_continuous("Data Availability Period",
                     breaks = dat_benzene_daily$DateIntNorm[target_xindices],
                     labels = dat_benzene_daily$Date[target_xindices] %>% format.Date("%Y-%b")) +
  ylab(label_benzene) +
  scale_color_manual(name = "Emission\nReported", values = palette_color_emissionreport) +
  scale_fill_manual(name = "Emission\nReported", values = palette_color_emissionreport) +
  scale_shape_manual(name = "Emission\nReported", values = palette_shape_emissionreport) +
  scale_size_manual(name = "Emission\nReported", values = c(no = 1, yes = 3))
  