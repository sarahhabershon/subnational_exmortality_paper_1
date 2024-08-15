source("00_loads_packages.R")
who_date <- as.Date("20200311", format = "%Y%m%d")


events <- read_csv("data/SARS-CoV-2 Superspreading Events from Around the World - SARS-CoV-2 Superspreading Events.csv", skip = 2) %>% 
  clean_names() %>%
  select(code, country, city_region, lat, long, setting1, description, total_cases, index_date_day_month) %>%
  mutate(date = as.Date(index_date_day_month, format = "%d/%m/%Y"),
         total_cases = as.numeric(total_cases)) %>%
  filter(country %in% c("Switzerland", "Sweden", "Spain", "Slovenia", "Portugal", "Norway", "Netherlands", "Italy", "Iceland", "Germany", "France", "Finland", "Estonia", "Denmark", "Czechia", "Belgium", "Austria")) %>%
  drop_na()


euro_events <- events %>%
  filter(country %in% c("Switzerland", "Sweden", "Spain", "Slovenia", "Portugal", "Norway", "Netherlands", "Italy", "Iceland", "Germany", "France", "Finland", "Estonia", "Denmark", "Czechia", "Belgium", "Austria"))

ggplot(euro_events, aes(x = date, y = country, size = total_cases)) +
  geom_point() +
  geom_vline(xintercept = as.Date("20200311", format = "%Y%m%d"))

events_sf <- st_as_sf(events, coords = c("long", "lat"), crs = 4326)


ggplot(clustered_mort %>%
         filter(mort_cluster_ids %in% c(2,3)) %>%
         inner_join(geodata)) +
  geom_sf(aes(geometry = geometry, fill = as.factor(mort_cluster_ids))) +
  geom_sf(data = geodata_0,
  aes(geometry = geometry), fill = NA, colour="black") +
  geom_sf(data = events_sf %>% filter(date < as.Date("2020-03-11")), color = "blue") +
  coord_sf(xlim = c(-10,32), ylim = c(36,72)) +
  bbc_style() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        axis.text = element_blank(),
        axis.text.x= element_blank()) +
  labs(title = "Cluster 2")







# stringency <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-dataset/main/data/OxCGRT_compact_national_v1.csv")
national_cases <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv") %>%
  filter(continent == "Europe") %>%
  select(date, location, new_cases_smoothed_per_million, new_tests_smoothed_per_thousand)

national_cases_smooth <- national_cases %>%
  rename(country_name = location) %>%
  mutate(date  = as.Date(date, format = "%Y-%m%d"))

relevant_headers <- grep("^c(?!.*m_f).*m_", names(stringency %>% clean_names), value = TRUE, perl = TRUE)


response_time <- stringency %>%
  clean_names() %>%
  select(country_name, country_code, date, h1_public_information_campaigns, all_of(relevant_headers)) %>%
  left_join(read_csv("data/country_codes.csv")) %>%
  filter(!is.na(code_nuts)) %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
  pivot_longer(!c(country_name, country_code, code_nuts, date), names_to = "intervention", values_to = "value") %>%
  mutate(var = case_when(
              intervention == "c1m_school_closing" ~ "school",
              intervention == "c2m_workplace_closing" ~ "workplace",
              intervention == "c3m_cancel_public_events" ~ "Cancel public events",
              intervention == "c4m_restrictions_on_gatherings" ~ "Restrictions on gatherings",
              intervention == "c5m_close_public_transport" ~ "public_transport",
              intervention == "c6m_stay_at_home_requirements" ~ "stay_at_home",
              intervention == "c7m_restrictions_on_internal_movement" ~ "internal_movement",
              intervention == "h1_public_information_campaigns" ~ "info",
              TRUE ~ "Other"
            )
  )




strindex <- stringency %>%
  clean_names() %>%
  select(country_name, country_code, date, stringency_index_average) %>%
  left_join(read_csv("data/country_codes.csv")) %>%
  filter(!is.na(code_nuts)) %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"))



ggplot(response_time %>%
         filter(date < as.Date("20200801", format = "%Y%m%d"),
                country_name %in% c("Slovenia", "Sweden", "Poland", "Bulgaria")),
       aes(x = date, y = value, group = intervention, colour = intervention)) +
  geom_line() +
  facet_wrap(~country_name, ncol = 1)



confirmed_cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>%
  pivot_longer(!c("Province/State",
                 "Country/Region",
                 "Lat",
                 "Long"), names_to = "date", values_to = "value") %>%
  clean_names() %>%
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  filter(is.na(province_state))

earliest_cases <- confirmed_cases %>%
  filter(country_region %in% unique(response_time$country_name),
         date < as.Date("01/08/2020", format = '%d/%m/%Y'))

first_cases <- earliest_cases %>%
  filter(value > 99) %>%
  group_by(country_region) %>%
  reframe(start_date = min(date),
            value = first(value)) %>%
  mutate(var = "100 cases") %>%
  rename(country_name = country_region)

first_restrictions <- response_time %>%
  filter(value > 0) %>%
  group_by(country_name, var) %>%
  reframe(start_date = min(date),
          value = first(value))

sort_date <- first_cases %>%
  select(country_name, start_date) %>%
  rename(sort_date = start_date)

country_names <- c("Italy",
                   "France",
                   "Slovenia",
                   "Spain",
                   "Finland",
                   "Sweden",
                   "Belgium",
                   "Denmark",
                   "Romania",
                   "Austria",
                   "Greece",
                   "Hungary",
                   "Portugal",
                   "Netherlands",
                   "Bulgaria",
                   "Poland")

ggplot(first_cases %>%
         rbind(first_restrictions) %>%
         filter(var %in% c("Cancel public events", 
                           "Restrictions on gatherings", 
                           "100 cases"),
                country_name %in% country_names) %>%
         left_join(sort_date), aes(x = start_date, y = reorder(country_name, desc(sort_date)), group = country_name, colour = var)) +
  geom_line(colour = "black") +
  geom_point(size = 4) +
  scale_colour_manual(values = c("#2a8e9d", "#e9c46a", "#b3c46a")) +
  scale_x_date(date_labels = "%d %b %y") +
  bbc_style() +
  theme(legend.position = "right",
        legend.text = element_text(size = 8)) +
  geom_vline(xintercept = as.Date("20200311", format = "%Y%m%d")) +
  annotate("text", x = as.Date("2020-03-11"), y = 17, label = "WHO declares pandemic", angle = 90, vjust = 1.2, hjust = 1, size = 3, color = "black")






facet_strindex <- strindex %>%
  filter(country_name %in% c("Poland", "Romania", "Hungary", "Bulgaria", "Greece", "Slovak Republic", "Slovenia")) %>% 
  mutate(stringency_index_average = stringency_index_average*10) %>%
  left_join(national_cases_smooth) %>%
  pivot_longer(c(stringency_index_average, new_cases_smoothed_per_million), names_to = "var", values_to = "value") %>%
  drop_na()

facet_strindex$country_name = factor(facet_strindex$country_name, unique(facet_strindex$country_name))


ggplot(facet_strindex %>%
         filter(date < as.Date("2021-04-01", format = "%Y-%m-%d")), 
       aes(x = date, y = value, group = var, colour = var)) +
  geom_line() +
  facet_wrap(~country_name, ncol = 1)

ggplot(national_cases_smooth %>%
         filter(country_name %in% c("Poland", "Romania", "Hungary", "Bulgaria", "Greece", "Slovak Republic", "Slovenia")) %>%
         filter(date < as.Date("2021-04-01", format = "%Y-%m-%d")), 
       aes(x = date, y = new_cases_smoothed_per_million)) +
  geom_line() +
  facet_wrap(~country_name, ncol = 1)


Netherlands:
"https://bmcpublichealth.biomedcentral.com/articles/10.1186/s12889-020-09612-6"
  
               


table <- strindex %>%
  group_by(country_name) %>%
  arrange(date) %>%
  summarize(stringency = list(stringency_index_average), .groups = "drop") %>%
  left_join(first_cases) %>%s
  gt() %>%
  gt_theme_538() %>% 
  gt_plt_sparkline(stringency,
                   type = "shaded",
                   same_limit = FALSE, 
                   label = FALSE,
                   palette = c("black", "black", "transparent", "#C70039", "lightgrey"))


table



cases <- read_dta("data/covid_master.dta") %>%
  select(date, nuts_id, cases_daily, population) %>%
  rename(code_nuts = nuts_id) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(nchar(code_nuts) == 5) %>%
  right_join(clustered_mort) %>%
  drop_na() %>%
  group_by(mort_cluster_ids, date) %>%
  summarise(total_cases = sum(cases_daily),
            total_pop = sum(population)) %>%
  mutate(rolling_average = rollmean(total_cases, k = 7, fill = NA),
         rolling_pc = rolling_average/total_pop)

cases_nuts3_cumsum <- read_dta("data/covid_master.dta")  %>%
  select(date, country, nuts_id, cases_daily, population) %>%
  drop_na() %>%
  rename(code_nuts = nuts_id) %>%
  left_join(clustered_mort) %>%
  filter(mort_cluster_ids > 3) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  group_by(code_nuts) %>%
  mutate(cumulative_cases = cumsum(cases_daily))

march_snap <- cases_nuts3_cumsum %>%
  filter(date < who_date)


october_snap <- cases_nuts3_cumsum %>%
  filter(date < as.Date("2020-09-31", format = "%Y-%m-%d"))





ggplot(cases %>%
         filter(date < as.Date("20200331", format = "%Y%m%d")), aes(x = date, y = rolling_pc, group = as.factor(mort_cluster_ids), colour = as.factor(mort_cluster_ids))) +
  geom_line() +
  geom_vline(xintercept = as.Date("20200311", format = "%Y%m%d")) +
  labs(title = "Kicking off the first wave",
       subtitle = "Confirmed cases per capita. by cluster")






ggplot(cases %>%
         filter(date >= as.Date("20200701", format = "%Y%m%d") & date <= as.Date("20210331", format = "%Y%m%d")), aes(x = date, y = rolling_pc, group = as.factor(mort_cluster_ids), colour = as.factor(mort_cluster_ids))) +
  geom_line() +
  scale_colour_manual(values = c("#264653",
                               "#2a9d8f",
                               "#e9c46a",
                               "#f4a261",
                               "#e76f51")) +
  labs(title = "Ok now the second winter")





case_fatality <- read_dta("data/covid_master.dta") %>%
  select(date, nuts_id, cases_daily, population) %>%
  rename(code_nuts = nuts_id) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(nchar(code_nuts) == 5) %>%
  mutate(week = lubridate::floor_date(date, unit = "week")) %>%
  group_by(code_nuts, week, population) %>%
  summarise(weekly_cases = sum(cases_daily),
            population = mean(population)) %>%
  left_join(exmort_weekly_nuts3 %>% select(code_nuts, exmort, date) %>%
              filter(date > as.Date("2020-02-23", format = "%Y-%m-%d")) %>%
              mutate(date = date-1))


  

  

