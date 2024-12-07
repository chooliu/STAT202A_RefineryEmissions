# B01_benzene_dat_eda.R ========================================================

# load data

label_benzene <- "Benzene Conc. (ug/m3)"
label_benzene_short <- "Benzene\n(ug/m3)"

# range of analysis (change this based on temporal overlap between
# exceedance reports of monitor data, temperature, etc)
startdate_benzene <- as.Date("2020-01-01")
enddate_benzene <- as.Date("2024-11-01")

# https://awsedap.epa.gov/public/extensions/Fenceline_Monitoring/Fenceline_Monitoring.html?sheet=MonitoringDashboard&REGISTRY_ID=110032913024
# biweekly at 16 monitors
# change dates to integer for modeling

dat_benzene <-
  read_excel("Data/Benzene_EPA/e4642d56-6ed1-490f-87ce-28cb29048a84.xlsx") %>%
  mutate(MonitorID = gsub("FL", "", `Monitor ID`) %>% factor(levels = 1:16),
         Date = date(`Period Start Date`),
         PolluteConc = as.numeric(`Benzene Concentration`)) %>%
  filter(Date > startdate_benzene &
           Date < enddate_benzene) %>%
  mutate(DateInt = as.integer(Date))

dat_benzene$`Monitor ID` %>% unique # FL1, ..., FL16


# 1837 x 30
dim(dat_benzene)
dat_benzene$Date %>% unique %>% length # 115
range(dat_benzene$Date)

# to convert date --> numeric
# start at 0
offset_integer_benzenefence <- min(dat_benzene$DateInt)

dat_benzene <-
  dat_benzene %>%
  mutate(DateIntNorm =
           DateInt - offset_integer_benzenefence)

dat_benzene_daily <-
  dat_benzene %>%
  group_by(Date) %>%
  summarize(PeriodStartDateInt = first(DateInt),
            PeriodStartDateIntNorm = first(DateIntNorm),
            PolluteConc =
              quantile(PolluteConc, prob = 0.90, na.rm = T))



# EDA: correlations btwn monitors ==============================================

dat_benzene %>%
  filter(year(dat_benzene$Date) == 2022) %>%
  pivot_wider(id_cols = Date,
              values_from = PolluteConc,
              names_from = MonitorID) %>%
  select(-Date) %>%
  mutate_all(as.numeric) %>%
  cor(method = 'spearman', use = 'complete') %>%
  corrplot(tl.col = 'black', method = 'square')

dat_benzene %>%
  filter(year(dat_benzene$Date) == 2023) %>%
  pivot_wider(id_cols = Date,
              values_from = PolluteConc,
              names_from = MonitorID) %>%
  select(-Date) %>%
  mutate_all(as.numeric) %>%
  cor(method = 'spearman', use = 'complete') %>%
  corrplot(tl.col = 'black', method = 'square')

dat_benzene %>%
  filter(year(dat_benzene$Date) == 2024) %>%
  pivot_wider(id_cols = Date,
              values_from = PolluteConc,
              names_from = MonitorID) %>%
  select(-Date) %>%
  mutate_all(as.numeric) %>%
  cor(method = 'spearman', use = 'complete') %>%
  corrplot(tl.col = 'black', method = 'square')



# EDA: interpolated density at time t ==========================================

longlat_benzene_df <-
  dat_benzene %>% 
  filter(!duplicated(MonitorID)) %>%
  transmute(Longitude = `Monitor Longitude`, Latitude = `Monitor Latitude`) %>%
  mutate_all(as.numeric)

longlat_benzene_mat <-
  longlat_benzene_df %>% 
  as.matrix

make_grid <- function(x) {
  seq(to = x[1] - 0.001, from = x[2] + 0.001, length.out = 100) }

my_grid <-
  expand_grid(
    Longitude = longlat_benzene_mat[ , 1] %>% range %>% make_grid,
    Latitude = longlat_benzene_mat[ , 2] %>% range %>% make_grid
  )

# distance for plots btwn 5 and 11, in km
spDistsN1(pts = longlat_benzene_mat,
          pt = c(longlat_benzene_mat[5, 1], longlat_benzene_mat[5, 2]), longlat = T) %>%
  .[11] # 1.11 km

# 1 to 9
spDistsN1(pts = longlat_benzene_mat,
          pt = c(longlat_benzene_mat[1, 1], longlat_benzene_mat[1, 2]), longlat = T) %>%
  .[9] # 1.11 km

make_interpolation_plot_benzene <-
  function(target_date) {
    
    test_concs <-
      dat_benzene %>%
      filter(Date == target_date) %>% .$PolluteConc
    
    filter_na_conc <- 
      is.na(test_concs)
    
    interpolated_values <-
      my_grid %>%
      pmap_dbl(
        function(Longitude, Latitude) {
          wrap_idw(query_long = Longitude,
                   query_lat = Latitude,
                   pollutant = test_concs[!filter_na_conc],
                   ref_coords = longlat_benzene_mat[!filter_na_conc, ])})
    
    df_grid_plot <- 
      my_grid %>% bind_cols(value = interpolated_values) %>%
      # mutate(value = if_else(value > 5, 5, value)) %>%
      ggplot(data = ., aes(Longitude, Latitude, fill = value)) +
      geom_tile() +
      geom_shadowtext(data = longlat_benzene_df %>% bind_cols(ID = 1:nrow(.)),
                aes(Longitude, Latitude, label = ID), inherit.aes = F, size = 4) +
      geom_shadowtext(data = longlat_suncor, 
                      aes(Longitude, Latitude), label = "X", inherit.aes = F, size = 4) +
      theme_few() +
      scale_fill_viridis_c(label_benzene_short, limits = c(0, 5)) +
      scale_x_continuous("Longitude", expand = c(0, 0)) +
      scale_y_continuous("Latitude", expand = c(0, 0))
    
    return(df_grid_plot)
    
  }

# random plots per year
set.seed(1243)
dat_benzene$Date %>% .[year(.) == "2022"] %>%
  unique %>% sample(size = 4) %>%
  map(make_interpolation_plot_benzene) %>%
  map(~ .x + theme_void() +
        theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
              legend.position = 'none')) %>%
  plot_grid(plotlist = .)

set.seed(1243)
dat_benzene$Date %>% .[year(.) == "2024"] %>%
  unique %>% sample(size = 4) %>%
  map(make_interpolation_plot_benzene) %>%
  map(~ .x + delete_axes +
        theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
              legend.position = 'none')) %>%
  plot_grid(plotlist = .)



# dates with noted emissions?
# (more of these in B02)
make_interpolation_plot_benzene("2022-12-06")



# EDA = plot patterns over time ================================================

ggplot(dat_benzene,
         aes(Date, PolluteConc)) +
  geom_point(size = 0.25, alpha = 1) +
  geom_vline(xintercept = paste0(2021:2024, "-01-01") %>% as.Date(),
             lty = 1, alpha = 0.25) +
  # geom_smooth(se = F, method = 'loess', aes(color = MonitorID)) +
  geom_rug(data = tibble(emission = dates_benzene %>% as.Date),
           aes(x = emission), inherit.aes = F) +
  theme_few() +
  scale_x_date(breaks = paste0(2021:2024, "-01-01") %>% as.Date(),
               labels = (2021:2024)) +
  ylab(label_benzene) +
  scale_color_manual(values = palette_color_monitors)



# density of concs by monitor id ===============================================

# grid at which to eval density
range(dat_benzene$PolluteConc, na.rm = T) # 0.10 to 6.76
grid_benz <-
  seq(0, 6, length.out = 100)

# calculate densities
compare_densities <-
  dat_benzene %>%
  group_by(MonitorID) %>%
  group_split() %>%
  map_dfc(~ .x$PolluteConc %>%
            .[!is.na(.)] %>%
            wrap_gausskern(x = ., g = grid_benz))

# plot densities
compare_densities %>%
  set_names(., 1:ncol(.)) %>%
  bind_cols(conc = grid_benz, .) %>%
  pivot_longer(cols = 2:ncol(.)) %>%
  mutate(name = factor(name, levels = 1:16)) %>%
  ggplot(data = ., aes(conc, value, color = name)) +
  geom_line() +
  theme_few() +
  scale_x_continuous(label_benzene, expand = c(0, 0), limits = c(0, 6)) +
  scale_y_continuous("Kernel Estimate", expand = c(0, 0)) +
  scale_color_manual("Monitor", values = palette_color_monitors)


# which four monitor densities have the elevated levels?
# 6, 7, 8, 10
compare_densities %>% apply(., 2, which.max) %>%
  grid_benz[.] %>% `>`(., 2) %>% which




