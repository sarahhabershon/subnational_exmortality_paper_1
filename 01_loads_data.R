# Geo data (exclude the islands)

# Offshore islands like the Azores and Svalbard.
island_filter <- c("FRY", "ES7", "PT2", "PT3", "IS0", "NO0B")

# Download geo data and filter out the unwanted units
geodata <- get_eurostat_geospatial(
  output_class = "sf",
  resolution = "60",
  nuts_level = 3,
  year = 2021) %>%
  rename(code_nuts = NUTS_ID) %>%
  filter(!str_detect(code_nuts, paste(island_filter, collapse="|")))

geodata <- sf::st_make_valid(geodata)
geodata <- geodata[sf::st_is_valid(geodata), ] 

geodata_0 <- get_eurostat_geospatial(
  output_class = "sf",
  resolution = "60",
  nuts_level = 0,
  year = 2021) %>%
  rename(code_nuts = NUTS_ID) 
  # filter(!geo %in% c("UK", "TR", "IE", "IS"))

geodata_0 <- sf::st_make_valid(geodata_0)
geodata_0 <- geodata_0[sf::st_is_valid(geodata_0), ] 


weekly_raw_mort_nuts3 <- recode_nuts(read.csv("data/estat_demo_r_mwk3_t_en.csv"), geo_var = "geo", nuts_year = 2021) %>%
  filter(!str_detect(geo, paste(island_filter, collapse="|")),
         nchar(code_2021) == 5,
         !grepl( "UK", code_2021),
         !grepl("AL", code_2021)) %>% #incomplete data for 2021
  select(code_2021, OBS_VALUE, TIME_PERIOD) %>%
  rename(code_nuts = code_2021,
         raw_mort = OBS_VALUE,
         year_week = TIME_PERIOD) %>%
  mutate(year_week = as.numeric(str_replace_all(year_week, "[^0-9]", "")),
         year = as.numeric(substr(year_week, 1, 4)),
         week = as.numeric(str_sub(year_week, -2, -1))) %>%
  group_by(year, week, code_nuts) %>%
  summarise(raw_mort = sum(raw_mort))


valid_2021 <- unique(nuts_changes$code_2021)

check_validity <- weekly_raw_mort_nuts3 %>%
  mutate(validity_check = code_nuts %in% valid_2021) %>%
  filter(validity_check == FALSE)

invalid_codes <- unique(check_validity$code_nuts)

# weekly mortality by nuts3 pre-pando
nuts3_weekly_baseline_mort <- weekly_raw_mort_nuts3 %>%
  drop_na() %>%
  filter(year %in% c(2015, 2016, 2017, 2018, 2019),
         nchar(code_nuts) == 5) %>%
  group_by(code_nuts, week) %>%
  summarise(expected_mort = (sum(raw_mort))/5,
            count = n())

# ex mortality nuts3
exmort_weekly_nuts3 <- weekly_raw_mort_nuts3 %>%
  # left_join(urban_rural_long) %>%
  filter(year %in% c(2020, 2021),
         week < 53,
         nchar(code_nuts) == 5) %>%
  group_by(year, week, code_nuts) %>%
  summarise(actual_mort = sum(raw_mort)) %>%
  left_join(nuts3_weekly_baseline_mort) %>%
  mutate(exmort = (actual_mort-expected_mort)/expected_mort,
         yearweek = as.numeric(paste0(year, week)),
         date = as.Date(paste0(year, "/", week, "/", 1), format ="%Y/%U/%u")) %>%
  ungroup()

exmort_weekly_nuts3_wide <- exmort_weekly_nuts3 %>%
  select(code_nuts, date, exmort) %>%
  mutate(date = paste(date)) %>%
  pivot_wider(names_from = "date", values_from = exmort) %>%
  drop_na()



# total ex mortality  (i.e. aggregated not time series)
nuts3_baseline_mort <- weekly_raw_mort_nuts3 %>%
  filter(nchar(code_nuts) == 5) %>%
  drop_na() %>%
  filter(year %in% c(2015, 2016, 2017, 2018, 2019),
         week < 53) %>%
  group_by(code_nuts) %>%
  summarise(expected_mort = (sum(raw_mort))/5)


# excess mortality europe
nuts3_total_exmort <- weekly_raw_mort_nuts3 %>%
  filter(nchar(code_nuts) == 5) %>%
  filter(year %in% c(2020, 2021),
         week < 53) %>%
  group_by(code_nuts) %>%
  summarise(europe_mort = sum(raw_mort)) %>%
  left_join(nuts3_baseline_mort) %>%
  mutate(expected_biannual_mort = 2* expected_mort,
         exmort = (europe_mort-expected_biannual_mort)/expected_biannual_mort)
# 
# 
# 
# Exmost nuts0

pre_pando_mort_nuts0 <- weekly_raw_mort_nuts3 %>%
  drop_na() %>%
  mutate(country = substr(code_nuts, 1, 2)) %>%
  filter(year %in% c(2015, 2016, 2017, 2018, 2019)) %>%
  group_by(country, week) %>%
  summarise(expected_mort = (sum(raw_mort))/5)

excess_mort_nuts0 <- weekly_raw_mort_nuts3 %>%
  drop_na() %>%
  mutate(country = substr(code_nuts, 1, 2)) %>%
  filter(year %in% c(2020, 2021),
         week < 53) %>%
  group_by(year, week, country) %>%
  summarise(actual_mort = sum(raw_mort)) %>%
  left_join(pre_pando_mort_nuts0) %>%
  mutate(exmort = (actual_mort-expected_mort)/expected_mort,
         yearweek = as.numeric(paste0(year, week)),
         date = as.Date(paste0(year, "/", week, "/", 1), format ="%Y/%U/%u")) %>%
  ungroup() %>%
  rename(code_nuts = country)


total_pre_pando_mort_nuts0 <- weekly_raw_mort_nuts3 %>%
  drop_na() %>%
  mutate(country = substr(code_nuts, 1, 2)) %>%
  filter(year %in% c(2015, 2016, 2017, 2018, 2019)) %>%
  group_by(country) %>%
  summarise(expected_mort = (sum(raw_mort))/5)

total_excess_mort_nuts0 <- weekly_raw_mort_nuts3 %>%
  drop_na() %>%
  mutate(country = substr(code_nuts, 1, 2)) %>%
  filter(year %in% c(2020, 2021),
         week < 53) %>%
  group_by(country) %>%
  summarise(actual_mort = sum(raw_mort)) %>%
  left_join(total_pre_pando_mort_nuts0) %>%
  mutate(exmort = (actual_mort-expected_mort)/expected_mort) %>%
  ungroup() %>%
  rename(code_nuts = country)

# 
# # 
# 
# 
# #  Oxford stringency
# # 
# # stringency <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-dataset/main/data/OxCGRT_compact_national_v1.csv") %>%
# #   rename(country_code = CountryCode) %>%
# #   left_join(read_csv("data/joining_codes.csv")) %>%
# #   filter(!is.na(code_nuts))
# 
# # write_csv(stringency, "data/stringency.csv")
# 
# stringency <- read_csv("data/stringency.csv")
# stringency_simple <- stringency %>%
#   clean_names() %>%
#   select(code_nuts, date, name, stringency_index_average, economic_support_index, containment_health_index_average) %>%
#   mutate(date = as.Date(paste(date), format = "%Y%m%d"))
# 
# 



pop_in <- read.table("data/estat_demo_r_pjangrp3.tsv", header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
population <- separate(pop_in, freq.sex.unit.age.geo.TIME_PERIOD, into = c("freq", "sex", "unit","age", "geo"), sep = ",") %>%  
  filter(sex == "T", age == "TOTAL", nchar(geo) == 5)  %>%
  select(geo, X2019) %>%
  rename(code_nuts = geo,
         pop19 = X2019) %>%
  mutate(pop19 = as.integer(gsub("[^0-9]", "", pop19))) 