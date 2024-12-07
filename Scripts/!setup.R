# libraries --------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(glue)

library(pdfsearch)
library(pdftools)

library(sp)
library(marginaleffects)
library(mgcv)

library(scales)
library(corrplot)
library(cowplot)
library(shadowtext)
library(ggthemes)
library(Polychrome)



# plotting ---------------------------------------------------------------------

palette_color_emissionreport <-
  c(yes = "maroon", no = 'navy')
palette_shape_emissionreport <-
  c(yes = 21, no = 19)

palette_color_monitors <-
  dark.colors() %>% as.character()

# guesses from google maps
# and description of three plant locations
longlat_suncor <-
  tribble(~ID, ~Latitude, ~Longitude,
          "W1", 39.80330520873259, -104.94603373517161,
          "W2", 39.80436022676684, -104.9445746135102,
          "E", 39.803214542364906, -104.94366266247182)

delete_axes <-
  theme(axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))



# C functions ------------------------------------------------------------------

# 1d gaussian kernel
# input data (x) and grid (g)

system("R CMD SHLIB Scripts/gausskern.c")
dyn.load("Scripts/gausskern.so")

wrap_gausskern <- function(x, g) {
  x <- x[!is.na(x)] # remove NAs
  gaussout <- .C("gausskern",
                 m = length(g),
                 n = length(x),
                 g = g, x = x,
                 bw = bw.nrd(x), # bandwidth based on scott's rule
                 y = double(length(g)))
  
  gaussout$y
}

# 2d distance based interpolation
# input outcome to interp (pollutant), grid

system("R CMD SHLIB Scripts/idw_interp.c")
dyn.load("Scripts/idw_interp.so")


wrap_idw <- function(query_long, query_lat,
                     pollutant, ref_coords) {
  mydist <- spDistsN1(pts = ref_coords,
                      pt = c(query_long, query_lat), longlat = T)
  
  res_idw <- .C("idw_interpolation",
                num_points = length(pollutant),
                pollutant = pollutant,
                dist = mydist,
                p = 2,
                interpolated = double(1))
  res_idw$interpolated
}



# misc fxns used throughout ----------------------------------------------------
# assumes log link

summary_table_from_gam <-
  function(modelin, display = F, digs = 2) {
  n_fixed_eff <- length(summary(modelin)$p.coeff)
  regresults <- tibble(
    coef = names(modelin$coefficients[1:n_fixed_eff]),
    beta = summary(modelin)$p.coeff,
    fc = exp(beta),
    se = summary(modelin)$se[1:n_fixed_eff], 
    t = summary(modelin)$p.t,
    p = summary(modelin)$p.pv) 
  
  if (display) {
  regresults %>%
    mutate(fc_low = exp(beta - 1.96*se),
           fc_high = exp(beta + 1.96*se)) %>%
    mutate_at(.vars = c("fc", "fc_low", "fc_high"), format, digits = digs) %>%
    mutate_at(.vars = "p", formatC, format = "g", digits = digs) %>%
    transmute(` ` = coef, `Fold Change` = paste0(fc, " (", fc_low, "-", fc_high, ")"),
              `p-value` = p)
  } else {
    regresults %>%
      mutate(fc_low = exp(beta - 1.96*se),
             fc_high = exp(beta + 1.96*se)) %>%
      transmute(coef, fc, fc_low, fc_high, p)
  }
}

# check autocorrelation of residuals 
plot_autocorr <- function(modelin) {
  
  acfobj <-
    acf(residuals(modelin), method = 'deviance')
  pacfobj <-
    pacf(residuals(modelin), method = 'deviance')

  # https://stats.stackexchange.com/questions/211628/how-is-the-confidence-interval-calculated-for-the-acf-function
  ic_alpha <- function(alpha, acf_res){
    return(qnorm((1 + (1 - alpha))/2)/sqrt(acf_res$n.used))
  }
  
  lim_acf <- ic_alpha(0.05, acfobj)
  lim_pacf <-ic_alpha(0.05, pacfobj)

  plot_grid(
    acfobj %>%
    with(., data.frame(lag, acf)) %>%
    ggplot(., mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0), size = 2) +
    xlab("Lag") + ylab("ACF") +
    geom_hline(yintercept = c(-lim_acf, lim_acf), linetype = 2, color = 'blue') +
    theme_few(base_size = 16),
    
    pacfobj %>%
      with(., data.frame(lag, acf)) %>%
      ggplot(., mapping = aes(x = lag, y = acf)) +
      geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0), size = 2) +
      xlab("Lag") + ylab("PACF") +
      geom_hline(yintercept = c(-lim_pacf, lim_pacf), linetype = 2, color = 'blue') +
      theme_few(base_size = 16)
  )
}


plot_resids_vs_time <-
  function(modelin) { 
    ggplot(data = NULL, aes(modelin$model$DateIntNorm,
                            resid(modelin, "deviance"))) +
      geom_point(alpha = 0.8, size = 1) +
      geom_hline(yintercept = 0, lty = 1, color = 'blue') +
      theme_few(base_size = 16) +
      xlab("Time (days)") + ylab("Deviance Residual") }





# scripts in order -------------------------------------------------------------

# general data loading
source("Scripts/A01_weatherstations.R")
source("Scripts/A02_exceedancereports.R")
source("Scripts/A03_community_monitoring_sites.R")

# pollutant specific: benzene
source("Scripts/B01_benzene_dat_eda.R")
source("Scripts/B02_benzene_modeling.R")
source("Scripts/B03_sensitivity_analysis_on_benz.R")

# pollutant specific: H2S
source("Scripts/C01_h2s_dat_eda.R")
source("Scripts/C02_h2s_modeling.R")

# pollutant specific: SO2
source("Scripts/D01_so2_dat_eda.R")
source("Scripts/D02_so2_modeling.R")

# pollutant specific: CO
source("Scripts/E01_co_dat_eda.R")
source("Scripts/E02_co_modeling.R")

# misc
source("Scripts/F01_cor_btwn_poll.R")

