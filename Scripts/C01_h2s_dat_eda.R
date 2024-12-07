# C01_h2s_dat_eda.R ========================================================

# load data
label_h2s <- "H2S (ppb)"
label_h2s_short <- "H2S\n(ppb)"

# range of analysis (change this based on temporal overlap between
# exceedance reports of monitor data, temperature, etc)
startdate_h2s <- as.Date("2021-08-01")
enddate_h2s <- as.Date("2024-11-01")


dat_h2s_2021 <-
  bind_rows(
  read_excel("Data/CCND/CCND_Q3_2021_Final_Data.xlsx", sheet = "H2S"),
  read_excel("Data/CCND/CCND_Q4_2021_Final_Data.xlsx", sheet = "H2S")) %>%
  select(DateTime, contains("CM")) %>%
  select(-CM9) %>% # all missing
  mutate(Date = date(DateTime)) %>%
  select(-DateTime)

dat_h2s_2022 <-
  bind_rows(
    read_excel("Data/CCND/CCND_Q1_2022_Data.xlsx", sheet = "H2S 24-Hour"),
    read_excel("Data/CCND/CCND_Q2_2022_Data.xlsx", sheet = "H2S 24-Hour"),
    read_excel("Data/CCND/CCND_Q3_2022_Data.xlsx", sheet = "H2S 24-hr"),
    read_excel("Data/CCND/CCND_Q4_2022_Data.xlsx", sheet = "H2S 24-hr"),
  ) %>%
  select(Date, contains("CM")) %>%
  mutate(Date = date(Date))

dat_h2s_2023A <-
  bind_rows(
    read_excel("Data/CCND/CCND_Q1_2023_Data.xlsx", sheet = "H2S 24-hr"),
    read_excel("Data/CCND/CCND_Q2_2023_Data.xlsx", sheet = "H2S 24-hr"),
    read_excel("Data/CCND/CCND Q4 2023 Data.xlsx", sheet = "H2S-24hr"),
  ) %>%
  select(Date, contains("CM")) %>%
  mutate(Date = date(Date))

# * 2023 but diff fmt, similar to 2024
dat_h2s_2023B <-
  read_excel("Data/CCND/CCND Q3 2023 Data.xlsx", sheet = "H2S-24hr") %>%
  select(Date, contains("CM")) %>%
  mutate(Date = date(Date))

names(dat_h2s_2023B)[-1] <-
  dat_h2s_2023B %>% names %>% .[-1] %>% str_extract("(CM[0-9]+)")

dat_h2s_2024 <-
  bind_rows(
    read_excel("Data/CCND/CCND Q1 2024 Data.xlsx", sheet = "H2S", skip = 3)
  ) %>%
  select(Date, contains("CM")) %>%
  mutate(Date = date(Date))

names(dat_h2s_2024)[-1] <-
  dat_h2s_2024 %>% names %>% .[-1] %>% str_extract("(CM[0-9]+)")


dat_h2s <-
  bind_rows(
    dat_h2s_2021,
    dat_h2s_2022,
    dat_h2s_2023A,
    dat_h2s_2023B,
    dat_h2s_2024
  ) %>%
  select(Date, contains("CM"))


rm(
  dat_h2s_2021,
  dat_h2s_2022,
  dat_h2s_2023A,
  dat_h2s_2023B,
  dat_h2s_2024
)





# integers for modeling --------------------------------------------------------

# to convert date --> numeric
# start at 0
dat_h2s <- dat_h2s %>% mutate(DateInt = as.integer(Date))
offset_integer_h2s <- min(dat_h2s$DateInt, na.rm = T)

dat_h2s <-
  dat_h2s %>%
  mutate(DateIntNorm =
           DateInt - offset_integer_h2s)

dat_h2s_long <-
  dat_h2s %>%
  pivot_longer(cols = names(dat_h2s) %>% .[grepl("CM", .)],
               names_to = "MonitorID", values_to = "PolluteConc") %>%
  group_by(Date, MonitorID, DateInt, DateIntNorm) %>%
  summarize(PolluteConc = quantile(PolluteConc, probs = 0.90, na.rm = T)) %>%
  mutate(MonitorID = gsub("CM", "", MonitorID) %>% as.numeric) %>%
  arrange(Date, MonitorID)




# density via C ----------------------------------------------------------------

# grid to eval density at
quantile(dat_h2s_long$PolluteConc, na.rm = T)
grid_h2s <-
  seq(0, 5, length.out = 100)

# calculate 
compare_densities <-
  dat_h2s_long %>%
  group_by(MonitorID) %>%
  group_split() %>%
  map_dfc(~ .x$PolluteConc %>%
            .[!is.na(.)] %>%
            wrap_gausskern(x = ., g = grid_h2s))

# plot densities
compare_densities %>%
  set_names(., 1:ncol(.)) %>%
  bind_cols(conc = grid_h2s, .) %>%
  pivot_longer(cols = 2:ncol(.)) %>%
  mutate(MonitorID = factor(name, levels = 1:10)) %>%
  ggplot(data = ., aes(conc, value, color = MonitorID)) +
  geom_line() +
  theme_few() +
  scale_x_continuous(label_h2s, expand = c(0, 0), limits = c(0, 6)) +
  scale_y_continuous("Density Estimate", expand = c(0, 0))


# no clear difference in monitor ids
# which densities have the highest lowest levels?
# 2 = high; 1, 4, 5, 6, 8 = mode at 0
compare_densities %>% apply(., 2, which.max) %>% grid_h2s[.]



# interpolated density at time t -----------------------------------------------

latlon_community_mat <-
  monitor_community_df %>% select(Longitude, Latitude) %>% 
  as.matrix

make_grid <- function(x) {
  seq(to = x[1] - 0.001, from = x[2] + 0.001, length.out = 100) }

my_grid <-
  expand_grid(
    Longitude = latlon_community_mat[ , 1] %>% range %>% make_grid,
    Latitude = latlon_community_mat[ , 2] %>% range %>% make_grid
  )



make_interpolation_plot_h2s <-
  function(target_date) {
    
    test_concs <-
      dat_h2s_long %>%
      filter(Date == target_date) %>% .$PolluteConc
    
    filter_na_conc <- 
      is.na(test_concs)
    
    interpol_values <-
      my_grid %>%
      pmap_dbl(
        function(Longitude, Latitude) {
          wrap_idw(query_long = Longitude,
                   query_lat = Latitude,
                   pollutant = test_concs[!filter_na_conc],
                   ref_coords = latlon_community_mat[!filter_na_conc, ])})
    
    df_grid_plot <- 
      my_grid %>% bind_cols(value = interpol_values) %>%
      ggplot(data = ., aes(Longitude, Latitude, fill = value)) +
      geom_tile() +
      geom_shadowtext(data = monitor_community_df,
                      aes(Longitude, Latitude, label = ID), inherit.aes = F, size = 4) +
      geom_shadowtext(data = longlat_suncor,
                      aes(Longitude, Latitude), label = "X", inherit.aes = F, size = 4) +
      theme_few() +
      scale_x_continuous("Longitude", expand = c(0, 0)) +
      scale_y_continuous("Latitude", expand = c(0, 0))
    
    return(df_grid_plot)
    
  }

set.seed(999)
dat_h2s_long %>%
  arrange(-PolluteConc) %>%
  .$Date %>% unique %>% sample(6) %>%
  map(make_interpolation_plot_h2s) %>%
  map(~ .x + scale_fill_viridis_c(label_h2s_short, option = "C") +
        geom_shadowtext(data = longlat_suncor, 
                        aes(Longitude, Latitude), label = "X", inherit.aes = F, size = 4) +
        theme(plot.margin = unit(c(0.75, 0.5, 0.5, 0.5), "cm")) +
        delete_axes) %>%
  plot_grid(plotlist = .) +
  scale_color_manual(values = palette_color_monitors)



# calculate distances for .ppt
# distance for x-dist btwn 9 and 3
spDistsN1(pts = latlon_community_mat,
          pt = c(latlon_community_mat[3, 1], latlon_community_mat[9, 2]), longlat = T) %>%
  .[9] 

# distance for y-dist btwn 9 and 3
spDistsN1(pts = latlon_community_mat,
          pt = c(latlon_community_mat[9, 1], latlon_community_mat[3, 2]), longlat = T) %>%
  .[9] 





# EDA: correlations btwn monitors ==============================================
# correlations all extremely high but this is because most of the values are zero!

dat_h2s_long %>%
  filter(year(Date) == 2022) %>%
  pivot_wider(id_cols = Date,
              values_from = PolluteConc,
              names_from = MonitorID) %>%
  ungroup %>%
  select(-Date) %>%
  mutate_all(as.numeric) %>%
  cor(method = 'spearman', use = 'complete') %>%
  corrplot(tl.col = 'black', method = 'square')

dat_h2s_long %>%
  filter(year(Date) == 2023) %>%
  pivot_wider(id_cols = Date,
              values_from = PolluteConc,
              names_from = MonitorID) %>%
  ungroup %>%
  select(-Date) %>%
  mutate_all(as.numeric) %>%
  cor(method = 'spearman', use = 'complete') %>%
  corrplot(tl.col = 'black', method = 'square')

dat_h2s_long %>%
  filter(year(Date) == 2024) %>%
  pivot_wider(id_cols = Date,
              values_from = PolluteConc,
              names_from = MonitorID) %>%
  ungroup %>%
  select(-Date) %>%
  mutate_all(as.numeric) %>%
  cor(method = 'spearman', use = 'complete') %>%
  corrplot(tl.col = 'black', method = 'square')



# EDA = plot patterns over time ================================================

dat_h2s_long %>%
  ggplot(.,
         aes(Date, PolluteConc)) +
  geom_point(size = 0.25, alpha = 1) +
  geom_vline(xintercept = paste0(2021:2024, "-01-01") %>% as.Date(),
             lty = 1, alpha = 0.25) +
  geom_rug(data = tibble(emission = dates_h2s %>%
                           .[year(dates_h2s) > 2020] %>% as.Date),
           aes(x = emission),
           inherit.aes = F) +
  scale_x_date(breaks = paste0(2021:2024, "-01-01") %>% as.Date(),
               labels = (2021:2024)) +
  scale_y_continuous(label_h2s) +
  theme_few(base_size = 14) +
  guides(color = guide_legend(override.aes = list(size = 3) ) ) +
  scale_color_manual("Monitor ID", values = palette_color_monitors) +
  scale_color_brewer("Temperature\nQuantile (°F)", palette = "YlOrRd")

