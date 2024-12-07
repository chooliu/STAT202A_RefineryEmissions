# A03_community_monitoring_sites.R =============================================
# get GPS coords of the non-benzene monitoring sites

dat_h2s_2024 <-
  bind_rows(
    read_excel("Data/CCND/CCND Q1 2024 Data.xlsx", sheet = "H2S", skip = 3)
  ) %>%
  select(Date, contains("CM")) %>%
  mutate(Date = date(Date))

monitor_community_df <-
  tibble(fullname = names(dat_h2s_2024)) %>%
  rowwise() %>%
  mutate(site = str_split(fullname, ":", simplify = T) %>% .[[1]] %>% head(1),
         cm = str_extract(fullname, "CM([0-9]+)"))

monitor_community_df <- monitor_community_df[-1, ]

monitor_community_df <-
  monitor_community_df %>%
  mutate(ID = gsub("CM", "", cm) %>% as.numeric(.)) %>%
  arrange(ID)

txt_ccnd_gps <-
  pdf_text("Data/CCND/Quality_Assurance_Project_Plan_(English).pdf")[6] %>%
  str_split("\n") %>% unlist

monitor_community_df$LongitudeLatitude <-
  txt_ccnd_gps %>% .[grepl(", -", .)] %>% str_extract("[0-9]+.[0-9]+, -[0-9]+.[0-9]+") %>%
  c(., "39.826007, -104.937438") # assume same spot alsup https://www.colorado.gov/airquality/site_description.aspx

monitor_community_df <-
  monitor_community_df %>%
  separate(LongitudeLatitude, into = c("Latitude", "Longitude"), sep = ", ", convert = T)

monitor_community_df

