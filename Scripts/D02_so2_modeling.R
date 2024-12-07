# D02_so2_modeling.R =======================================================

# pad dates of emission reports due to sparsity of events? ---------------------
# parameter: days pad from origin

dayspad_before <- 0
dayspad_after <- 0

target_integers_so2 <-
  dates_so2 %>% as.Date %>% as.integer()

target_integers_so2 <-
  sapply(target_integers_so2, function(x) {
    seq(x - dayspad_before, x + dayspad_after)}) %>% as.numeric

# pull out interpolated 2D to look at effects

set.seed(999)
dat_so2_long %>%
  arrange(-PolluteConc) %>%
  filter(Date %in% unique(dates_so2)) %>%
  .$Date %>% head(10) %>% unique %>% head(6) %>%
  map(make_interpolation_plot_so2) %>%
  map(~ .x + scale_fill_viridis_c(label_so2_short, limits = c(0, 14), option = "C", breaks = pretty_breaks(4)) +
        geom_shadowtext(data = longlat_suncor, 
                        aes(Longitude, Latitude), label = "X", inherit.aes = F, size = 4) +
        theme(plot.margin = unit(c(0.75, 0.5, 0.5, 0.5), "cm")) +
        delete_axes) %>%
  plot_grid(plotlist = .) 



# setup modeling parameters ----------------------------------------------------
# join with other external datasets (exceedance, weather)

dat_so2_long <-
  dat_so2_long %>%
  select(-intersect(names(.), names(weather)[-1])) %>%
  left_join(., weather, by = "Date") %>%
  mutate(AnyPercip = if_else(Percip > 0 | Snow > 0, "yes", "no")) %>%
  mutate(EmissionReported = Date %in%
           as.Date(target_integers_so2, origin = lubridate::origin) %>%
           if_else(., "yes", "no") %>% as.factor)

# aggregate by day (ignore monitor effect)
dat_so2_daily <-
  dat_so2_long %>%
  group_by(Date) %>%
  summarize(PolluteConc = quantile(PolluteConc, 0.90, na.rm = T),
            DateInt = first(DateInt),
            DateIntNorm = first(DateIntNorm),
            EmissionReported = first(EmissionReported),
            AnyPercip = first(AnyPercip),
            Percip = first(Percip),
            Snow = first(Snow),
            TempMax = first(TempMax))

dim(dat_so2)
dim(dat_so2_daily)


# gamma > 0
epsilon <- 1e-6
dat_so2_daily$PolluteConc[dat_so2_daily$PolluteConc == 0] <- epsilon

# number of events?
unique(dat_so2_long %>% filter(EmissionReported == 'yes') %>% .$Date) %>% length
dat_so2_long %>% .[complete.cases(.), ] %>% .$Date %>% unique %>% length
table(dat_so2_long$EmissionReported)


# run GAMs, day-aggreg ---------------------------------------------------------

# emission report only, no weather

wrap_gam <- function(formulain, datin) {
  model <- gam(formulain, data = datin, family = Gamma(link = 'log'))
  return(list(
    model = model,
    summary = summary(model),
    gamcheck = function() { par(mfrow = c(2, 2)); gam.check(model) },
    ar = plot_autocorr(model),
    summarytable = summary_table_from_gam(model, digs = 2),
    summarytable_print = summary_table_from_gam(model, display = T, digs = 2)
  ))
  
}


# baseline
# ~ time + emission
mod_so2_baseline <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = 'bs', k = 200) + EmissionReported,
         dat_so2_daily)

# weathersimple
# ~ time + emission + temp + anypercip
mod_so2_weathersimple <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = c('cs', 'bs'), k = 200) + EmissionReported +
             TempMax + AnyPercip,
           dat_so2_daily)

# weatherdetailed
# ~ time + emission + temp + percip + snow
mod_so2_weatherdetailed <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = c('cs', 'bs'), k = 200) + EmissionReported +
             TempMax + Percip + Snow,
           dat_so2_daily)

# weatherdetailed
# ~ time + emission + temp + percip + snow + emission*percip
mod_so2_weatherinteract <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = 'bs', k = 200) + EmissionReported +
             TempMax + Percip + Snow + EmissionReported*Percip,
           dat_so2_daily)



# run GAMs, random effect on monitor -------------------------------------------

dat_so2_long$MonitorID <- as.factor(dat_so2_long$MonitorID)
dat_so2_long$PolluteConc[dat_so2_long$PolluteConc == 0] <- epsilon

# weatherdetailed
# ~ time + emission + temp + percip + snow + (1|monitor)
modre_so2_weatherdetailed <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = c('bs', 'cs'), k = 400) + EmissionReported +
             TempMax + Percip + Snow + s(MonitorID, bs = 're'),
           dat_so2_long)

# example of more complicated model attempt
# that does not converge
# test_ar <- gamm(PolluteConc ~ s(DateIntNorm, bs = 'bs', k = 10) + EmissionReported +
#                   TempMax + Percip + Snow,
#                   data = dat_so2_daily,
#                   family = Gamma("log"),
#                   correlation = corARMA(form = ~ 1, p = 2),
#                   method = "REML", na.action = na.exclude)



# plotting final estimates, day-aggreg -----------------------------------------

# final table
mod_so2_weatherdetailed$summarytable_print

# poor residual fits
plot_resids_vs_time(mod_so2_weatherdetailed$model)

# poor ACF
mod_h2s_weatherdetailed$ar

# plot real data and predictions
plot_predictions(mod_so2_weatherdetailed$model,
                 condition = list('DateIntNorm'),
                 type = 'response') +
  theme_few(base_size = 14) +
  geom_point(data = dat_so2_long, aes(DateIntNorm, PolluteConc), color = 'blue',
             alpha = 0.5, shape = '.', inherit.aes = F) +
  geom_vline(xintercept = paste0(2021:2024, "-01-01") %>% as.Date() %>% as.integer() %>% `-`(offset_integer_so2),
             lty = 2, alpha = 0.5) +
  scale_color_manual("Max Temp\n(°F)",values =  c("#53a5b8", "black", "#bf340d")) +
  scale_fill_manual("Max Temp\n(°F)", values = c("#53a5b8", "black", "#bf340d")) +
  scale_x_continuous("Data Availability Period",
                     breaks = dat_so2_daily$DateIntNorm[target_xindices],
                     labels = dat_so2_daily$Date[target_xindices] %>% format.Date("%Y-%b")) +
  ylab(label_so2) +
  geom_rug(data = tibble(dates_so2 = as.integer(dates_so2[year(dates_so2) > 2020]) - offset_integer_so2),
           aes(x = dates_so2),
           inherit.aes = F, alpha = 0.5)

# show temperature effect
plot_predictions(mod_so2_weatherdetailed$model,
                 condition = list('DateIntNorm', TempMax = c(20, 50, 75)),
                 type = 'response') +
  theme_few(base_size = 14) +
  geom_vline(xintercept = paste0(2021:2024, "-01-01") %>% as.Date() %>% as.integer() %>% `-`(offset_integer_so2),
             lty = 2, alpha = 0.5) +
  scale_color_manual("Max Temp\n(°F)",values =  c("#53a5b8", "black", "#bf340d")) +
  scale_fill_manual("Max Temp\n(°F)", values = c("#53a5b8", "black", "#bf340d")) +
  scale_x_continuous("Data Availability Period",
                     breaks = dat_so2_daily$DateIntNorm[target_xindices],
                     labels = dat_so2_daily$Date[target_xindices] %>% format.Date("%Y-%b")) +
  ylab(label_so2) +
  geom_rug(data = tibble(dates_so2 = as.integer(dates_so2[year(dates_so2) > 2020]) - offset_integer_so2),
           aes(x = dates_so2),
           inherit.aes = F, alpha = 0.5)

# emission effect
plot_predictions(mod_so2_weatherdetailed$model,
                 condition = c('DateIntNorm', 'EmissionReported'),
                 type = 'response') + 
  theme_few() +
  scale_color_manual(values = palette_color_emissionreport) +
  scale_x_continuous("Data Availability Period",
                     breaks = dat_so2_daily$DateIntNorm[target_xindices],
                     labels = dat_so2_daily$Date[target_xindices] %>% format.Date("%Y-%b")) +
  ylab(label_so2) +
  geom_rug(data = tibble(dates_so2 = as.integer(dates_so2[year(dates_so2) > 2020]) - offset_integer_so2),
           aes(x = dates_so2),
           inherit.aes = F, alpha = 0.5)


# snow effect
plot_predictions(mod_so2_weatherdetailed$model,
                 condition = c('DateIntNorm', 'Snow'),
                 type = 'response') + 
  theme_few() +
  scale_color_manual(values = palette_color_emissionreport) +
  scale_x_continuous("Data Availability Period",
                     breaks = dat_so2_daily$DateIntNorm[target_xindices],
                     labels = dat_so2_daily$Date[target_xindices] %>% format.Date("%Y-%b")) +
  ylab(label_so2) +
  geom_rug(data = tibble(dates_so2 = as.integer(dates_so2[year(dates_so2) > 2020]) - offset_integer_so2),
           aes(x = dates_so2),
           inherit.aes = F, alpha = 0.5)
