# A02_exceedance_reports.R =====================================================



# suncor exceedance reports, as aggregated by ----------------------------------
# https://www.c3gov.com/living-in/energy-equity-and-the-environment/suncor-refinery
# for 2020 to 2023, then augmented by .pdf search for 2024



# sheet 1 (2020 - 2023) --------------------------------------------------------
dat_exc1 <-
  read_excel("Data/CommerceCity/exceedancereports-1.8.2024.xlsx", skip = 1,
             col_types = c('list', 'list', rep("text", 11)))

# removes just one record says "FILL IN"
dat_exc1 <-
  dat_exc1 %>%
  filter(!map_chr(dat_exc1$`Start Date & Time`, typeof) == "character") 
dat_exc1$`Start Date & Time` %>%
  map(~ as.Date(.x) %>% as.character) %>% unlist %>% as.Date

dat_exc1$DateFixed <-
  dat_exc1$`Start Date & Time` %>%
  map(~ as.Date(.x) %>% as.character) %>% unlist %>% as.Date



# sheet 2 (2022-2023) -----------------------------------------------------------

dat_exc2 <-
  read_excel("Data/CommerceCity/exceedancereports-1.8.2024.xlsx", skip = 1, sheet = 2,
             col_types = c('list', rep("text", 15)))
dat_exc2$DateFixed <- dat_exc2$Date

# removes NAs (classified as lgl)
dat_exc2 <-
  dat_exc2 %>%
  filter(!map_chr(dat_exc2$DateFixed, typeof) == "logical")

# need manual replacement
filter_dates_exc2_manual <- 
  (dat_exc2$DateFixed %>% map_chr(typeof)) == "character"
dat_exc2$Date[ filter_dates_exc2_manual ]
dat_exc2_replacement <-
  c("12/14/2023", "11/23/2023", NA, "9/01/2023", "7/04/2023",
    "7/04/2023", "7/04/2023", "12/26/2022", "12/25/2022", "12/25/2022",
    "12/24/2022", "12/24/2022", "12/23/2022", "12/23/2022", "06/15/2022",
    "04/02/2022", "04/02/2022", "3/19/2022", "3/19/2022", "3/19/2022",
    "3/19/2022", "3/19/2022", "3/19/2022", "3/19/2022", "3/19/2022",
    "3/19/2022") %>% as.Date(., "%m/%d/%Y")
length(dat_exc2_replacement) # 26
dat_exc2$DateFixed[filter_dates_exc2_manual] <- NA

dat_exc2$DateFixed <-
  dat_exc2$DateFixed %>%
  map_chr(~ as.Date(.x, origin = "1899-12-30") %>% as.character) %>% as.Date

dat_exc2$DateFixed[filter_dates_exc2_manual] <- 
  dat_exc2_replacement %>%
  map_chr(~ as.Date(.x, origin = lubridate::origin) %>% as.character) %>% as.Date



# sheet 3 (2023? one 2019 date) ------------------------------------------------
# the 2019 entries seem to say "no violations"

dat_exc3 <-
  read_excel("Data/CommerceCity/exceedancereports-1.8.2024.xlsx",
             sheet = "NPDES", col_types = c("text", 'list', rep("text", 12)))
dat_exc3$DateFixed <- dat_exc3$`Start Date`

# removes NAs
dat_exc3 <-
  dat_exc3 %>%
  filter(!map_chr(dat_exc3$DateFixed, typeof) == "logical") #

dat_exc3$DateFixed <-
  dat_exc3$`Start Date` %>% map(~ as.Date(.x) %>% as.character) %>% unlist %>% as.Date

dat_exc3$Summary <- dat_exc3$`Compliance Status`



# join the three sheets --------------------------------------------------------

# one entry is a character
dat_exceedance <-
  bind_rows(dat_exc1, dat_exc2, dat_exc3)
rm(dat_exc1, dat_exc2, dat_exc3)

# removes two seemingly empty entries
dat_exceedance <- dat_exceedance %>% filter(DateFixed > 2000)

# top pollutants
dat_exceedance %>%
  group_by(Pollutants) %>%
  tally %>%
  arrange(-n)

# focus on 2020 period and on since data available on all outcomes n covariates
# the 2019 entries seem to say "no violations"
dat_exceedance <-
  dat_exceedance %>%
  filter(DateFixed > "2020-01-01")



# model benzene, H2S, SO2, CO emissions ----------------------------------------

# benzene
# dat_exceedance[filter_exceedance_benzene, ] %>% View
filter_exceedance_benzene <-
  apply(dat_exceedance, 1, function(x) {
  grepl("benzene", x, ignore.case = T) %>% any
}) | tolower(dat_exceedance$Pollutants) == "benzene"

dates_benzene <-
  dat_exceedance %>%
  filter(filter_exceedance_benzene) %>%
  .$DateFixed %>%
  unique
length(dates_benzene)


# so2
# dat_exceedance[filter_exceedance_so2, ] %>% View
filter_exceedance_so2 <-
  apply(dat_exceedance, 1, function(x) {
    grepl("so2", x, ignore.case = T) %>% any
  })| tolower(dat_exceedance$Pollutants) == "so2"

dates_so2 <-
  dat_exceedance %>%
  filter(filter_exceedance_so2) %>%
  .$DateFixed %>%
  unique
length(dates_so2)

# h2s
# dat_exceedance[filter_exceedance_h2s, ] %>% View
filter_exceedance_h2s <-
  apply(dat_exceedance, 1, function(x) {
    grepl("h2s", x, ignore.case = T) %>% any
  }) | tolower(dat_exceedance$Pollutants) == "h2s"

dates_h2s <-
  dat_exceedance %>%
  filter(filter_exceedance_h2s) %>%
  .$DateFixed %>%
  unique
length(dates_h2s)


# co
# dat_exceedance[filter_exceedance_co, ] %>% View
filter_exceedance_co <-
  apply(dat_exceedance, 1, function(x) {
    grepl(" co ", x, ignore.case = T) %>% any
  }) | tolower(dat_exceedance$Pollutants) == "co"

dates_co <-
  dat_exceedance %>%
  filter(filter_exceedance_co) %>%
  .$DateFixed %>%
  unique
length(dates_co)



# add any additional manual curation from pdfs  ================================
# (makes sure updated towards october 2024)

dir_reportable <- "Data/reportable_events/"


# pdf search, benzene
pdfres_benz <-
  keyword_directory(dir_reportable, keyword = "benzene", ignore_case = T)
# pdfres_benz$pdf_name %>% unique %>% glue("open {dir_reportable}/", .) %>% system()

dates_benzene <-
  c(dates_benzene, as.Date("2024-01-31")) %>%
  sort %>% unique


# pdf search, h2s
pdfres_h2s <-
  keyword_directory(dir_reportable, keyword = "h2s", ignore_case = T)
# pdfres_h2s$pdf_name %>% unique %>% map(~ glue("open {dir_reportable}/", .x) %>% system())

dates_h2s <-
  c(dates_h2s,
    as.Date(c("2024-05-21", "2024-05-24", "2024-03-05", "2024-03-08",
              "2024-01-13", "2024-01-14", "2024-01-15", "2024-01-16",
              "2024-01-19", "2024-02-11"))) %>%
  sort %>% unique


# pdf search, so2
pdfres_so2 <-
  keyword_directory(dir_reportable, keyword = "so2", ignore_case = T)
# pdfres_so2$pdf_name %>% unique %>% map(~ glue("open {dir_reportable}/", .x) %>% system())

dates_so2 <-
  c(dates_so2,
    as.Date(
      c("2024-08-29", "2024-06-17", "2024-06-18", "2024-06-22",
        "2024-05-22", "2024-03-24", "2024-04-09", "2024-02-25", "2024-02-26",
        "2024-01-16", "2024-01-17"))) %>%
  sort %>% unique


# pdf search, co
pdfres_co <-
  keyword_directory(dir_reportable, keyword = " co ", ignore_case = T)
# pdfres_co$pdf_name %>% unique %>% map(~ glue("open {dir_reportable}/", .x) %>% system())

dates_co <-
  c(dates_co,
    as.Date("2024-01-16")) %>%
  sort %>% unique

