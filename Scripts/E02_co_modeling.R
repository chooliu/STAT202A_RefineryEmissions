# E02_co_modeling.R =======================================================

# pad dates of emission reports due to sparsity of events? ---------------------
# parameter: days pad from origin

dayspad_before <- 0
dayspad_after <- 0

target_integers_co <-
  dates_co %>% as.Date %>% as.integer()

target_integers_co <-
  sapply(target_integers_co, function(x) {
    seq(x - dayspad_before, x + dayspad_after)}) %>% as.numeric

# pull out interpolated 2D to look at effects

set.seed(999)
dat_co_long %>%
  arrange(-PolluteConc) %>%
  filter(Date %in% unique(dates_co)) %>%
  .$Date %>% unique %>% head(6) %>%
  map(make_interpolation_plot_co) %>%
  map(~ .x + scale_fill_viridis_c(label_co_short, limits = c(0, 1.5),
                                  option = "B", breaks = pretty_breaks(4)) +
        geom_shadowtext(data = longlat_suncor, 
                        aes(Longitude, Latitude), label = "X", inherit.aes = F, size = 4) +
        theme(plot.margin = unit(c(0.75, 0.5, 0.5, 0.5), "cm")) +
        delete_axes) %>%
  plot_grid(plotlist = .) 



# setup modeling parameters ----------------------------------------------------
# join with other external datasets (exceedance, weather)

dat_co_long <-
  dat_co_long %>%
  select(-intersect(names(.), names(weather)[-1])) %>%
  left_join(., weather, by = "Date") %>%
  mutate(AnyPercip = if_else(Percip > 0 | Snow > 0, "yes", "no")) %>%
  mutate(EmissionReported = Date %in%
           as.Date(target_integers_co, origin = lubridate::origin) %>%
           if_else(., "yes", "no") %>% as.factor)

# aggregate by day (ignore monitor effect)
dat_co_daily <-
  dat_co_long %>%
  group_by(Date) %>%
  summarize(PolluteConc = quantile(PolluteConc, 0.90, na.rm = T),
            DateInt = first(DateInt),
            DateIntNorm = first(DateIntNorm),
            EmissionReported = first(EmissionReported),
            AnyPercip = first(AnyPercip),
            Percip = first(Percip),
            Snow = first(Snow),
            TempMax = first(TempMax))

dim(dat_co)
dim(dat_co_daily)

# gamma > 0
epsilon <- 1e-6
dat_co_daily$PolluteConc[dat_co_daily$PolluteConc == 0] <- epsilon

# number of events?
unique(dat_co_long %>% filter(EmissionReported == 'yes') %>% .$Date) %>% length
dat_co_long %>% .[complete.cases(.), ] %>% .$Date %>% unique %>% length
table(dat_co_long$EmissionReported)



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
mod_co_baseline <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = 'bs', k = 100) + EmissionReported,
         dat_co_daily)

# weathersimple
# ~ time + emission + temp + anypercip
mod_co_weathersimple <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = c('cs', 'bs'), k = 100) + EmissionReported +
             TempMax + AnyPercip,
           dat_co_daily)

# weatherdetailed
# ~ time + emission + temp + percip + snow
mod_co_weatherdetailed <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = c('cs', 'bs'), k = 100) + EmissionReported +
             TempMax + Percip + Snow,
           dat_co_daily)

# weatherdetailed
# ~ time + emission + temp + percip + snow + emission*percip
mod_co_weatherinteract <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = 'bs', k = 100) + EmissionReported +
             TempMax + Percip + Snow + EmissionReported*Percip,
           dat_co_daily)



# run GAMs, random effect on monitor -------------------------------------------

dat_co_long$MonitorID <- as.factor(dat_co_long$MonitorID)
dat_co_long$PolluteConc[dat_co_long$PolluteConc == 0] <- epsilon

# weatherdetailed
# ~ time + emission + temp + percip + snow + (1|monitor)
modre_co_weatherdetailed <-
  wrap_gam(PolluteConc ~ s(DateIntNorm, bs = c('bs', 'cs'), k = 200) + EmissionReported +
             TempMax + Percip + Snow + s(MonitorID, bs = 're'),
           dat_co_long)

# example of more complicated model attempt
# that does not converge
# test_ar <- gamm(PolluteConc ~ s(DateIntNorm, bs = 'bs', k = 10) + EmissionReported +
#                   TempMax + Percip + Snow,
#                   data = dat_co_daily,
#                   family = Gamma("log"),
#                   correlation = corARMA(form = ~ 1, p = 2),
#                   method = "REML", na.action = na.exclude)



# plotting final estimates, day-aggreg -----------------------------------------

# final table
mod_co_weatherdetailed$summarytable_print

# plot real data and predictions
plot_predictions(mod_co_weatherdetailed$model,
                 condition = list('DateIntNorm'),
                 type = 'response') +
  theme_few(base_size = 14) +
  geom_point(data = dat_co_long, aes(DateIntNorm, PolluteConc), color = 'blue',
             alpha = 0.5, shape = '.', inherit.aes = F) +
  geom_vline(xintercept = paste0(2021:2024, "-01-01") %>% as.Date() %>% as.integer() %>% `-`(offset_integer_co),
             lty = 2, alpha = 0.5) +
  scale_color_manual("Max Temp\n(°F)",values =  c("#53a5b8", "black", "#bf340d")) +
  scale_fill_manual("Max Temp\n(°F)", values = c("#53a5b8", "black", "#bf340d")) +
  scale_x_continuous("Data Availability Period",
                     breaks = dat_co_daily$DateIntNorm[target_xindices],
                     labels = dat_co_daily$Date[target_xindices] %>% format.Date("%Y-%b")) +
  ylab(label_co) +
  geom_rug(data = tibble(dates_co = as.integer(dates_co[year(dates_co) > 2020]) - offset_integer_co),
           aes(x = dates_co),
           inherit.aes = F, alpha = 0.5)

# show temperature effect
plot_predictions(mod_co_weatherdetailed$model,
                 condition = list('DateIntNorm', TempMax = c(20, 50, 75)),
                 type = 'response') +
  theme_few(base_size = 14) +
  geom_vline(xintercept = paste0(2021:2024, "-01-01") %>% as.Date() %>% as.integer() %>% `-`(offset_integer_co),
             lty = 2, alpha = 0.5) +
  scale_color_manual("Max Temp\n(°F)",values =  c("#53a5b8", "black", "#bf340d")) +
  scale_fill_manual("Max Temp\n(°F)", values = c("#53a5b8", "black", "#bf340d")) +
  scale_x_continuous("Data Availability Period",
                     breaks = dat_co_daily$DateIntNorm[target_xindices],
                     labels = dat_co_daily$Date[target_xindices] %>% format.Date("%Y-%b")) +
  ylab(label_co) +
  geom_rug(data = tibble(dates_co = as.integer(dates_co[year(dates_co) > 2020]) - offset_integer_co),
           aes(x = dates_co),
           inherit.aes = F, alpha = 0.5)

# emission effect
plot_predictions(mod_co_weatherdetailed$model,
                 condition = c('DateIntNorm', 'EmissionReported'),
                 type = 'response') + 
  theme_few() +
  scale_color_manual(values = palette_color_emissionreport) +
  scale_x_continuous("Data Availability Period",
                     breaks = dat_co_daily$DateIntNorm[target_xindices],
                     labels = dat_co_daily$Date[target_xindices] %>% format.Date("%Y-%b")) +
  ylab(label_co) +
  geom_rug(data = tibble(dates_co = as.integer(dates_co[year(dates_co) > 2020]) - offset_integer_co),
           aes(x = dates_co),
           inherit.aes = F, alpha = 0.5)

target_xindices <-
  dat_co_daily$Date %>%
  length %>%
  `-`(., 50) %>%
  seq(1, ., length.out = 4) %>% round(.)

  plot_predictions(mod_co_weatherdetailed$model,
                   condition = c('DateIntNorm', 'EmissionReported'), type = 'response') +
  theme_few() +
  geom_point(data = dat_co_daily,
             aes(DateIntNorm, PolluteConc,
                 fill = EmissionReported, shape = EmissionReported, size = EmissionReported),
             inherit.aes = F) +
  geom_rug(data = tibble(dates_co = as.integer(dates_co[year(dates_co) > 2020]) - offset_integer_co),
           aes(x = dates_co),
           inherit.aes = F, alpha = 0.5) +
  scale_x_continuous("Data Availability Period",
                     breaks = dat_co_daily$DateIntNorm[target_xindices],
                     labels = dat_co_daily$Date[target_xindices] %>% format.Date("%Y-%b")) +
  ylab(label_co) +
  scale_color_manual(name = "Emission\nReported", values = palette_color_emissionreport) +
  scale_fill_manual(name = "Emission\nReported", values = palette_color_emissionreport) +
  scale_shape_manual(name = "Emission\nReported", values = palette_shape_emissionreport) +
  scale_size_manual(name = "Emission\nReported", values = c(no = 1, yes = 3))


# EDA = for co, show temperature effect =======================================

dat_co_long$TmpQuartile <- dat_co_long$TempMax %>% cut(breaks = 4)

dat_co_long %>%
  .[complete.cases(.), ] %>%
  ggplot(.,
         aes(Date, PolluteConc, color = TmpQuartile)) +
  geom_point(size = 0.5, alpha = 0.5) +
  # geom_smooth(se = F, span = 1, method = 'loess', aes(color = MonitorID)) +
  geom_vline(xintercept = paste0(2021:2024, "-01-01") %>% as.Date(),
             lty = 2, alpha = 0.5) +
  geom_rug(data = tibble(dates_co = dates_co %>% .[dates_co[year(dates_co) > 2020]]),
           aes(x = dates_co),
           inherit.aes = F, alpha = 0.5) +
  theme_few(base_size = 14) +
  scale_x_date(breaks = paste0(2021:2024, "-01-01") %>% as.Date(),
               labels = (2021:2024)) +
  ylab(label_co) +
  scale_color_brewer("Temperature\nQuantile (°F)", palette = "YlOrRd") +
  guides(color = guide_legend(override.aes = list(size = 3) ) )

