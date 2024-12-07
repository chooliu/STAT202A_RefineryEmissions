# F01_cor_btwn_poll.R ==========================================================

left_join(dat_so2_daily, dat_h2s_daily, by = "Date") %>%
  summarize(cor(PolluteConc.x, PolluteConc.y, use = 'complete'))

left_join(dat_benzene_daily, dat_h2s_daily, by = "Date") %>%
  summarize(cor(PolluteConc.x, PolluteConc.y, use = 'complete'))

left_join(dat_co_daily, dat_h2s_daily, by = "Date") %>%
  summarize(cor(PolluteConc.x, PolluteConc.y, use = 'complete'))

left_join(dat_co_daily, dat_so2_daily, by = "Date") %>%
  summarize(cor(PolluteConc.x, PolluteConc.y, use = 'complete'))
