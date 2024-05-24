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
         filter(mort_cluster_ids == 2) %>%
         left_join(geodata)) +
  geom_sf(aes(geometry = geometry), fill = "#2a9d8f") +
  geom_sf(data = geodata_0,
          aes(geometry = geometry), fill = NA, colour="black") +
  geom_sf(data = events_sf %>% filter(date < as.Date("2020-03-11")), color = "red") +
  coord_sf(xlim = c(-10,32), ylim = c(36,72)) +
  bbc_style() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        axis.text = element_blank(),
        axis.text.x= element_blank()) +
  labs(title = "Cluster 2")







# stringency <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-dataset/main/data/OxCGRT_compact_national_v1.csv")

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

ggplot(first_cases %>%
         rbind(first_restrictions) %>%
         filter(var %in% c("Cancel public events", "Restrictions on gatherings", "100 cases"),
                country_name %in% c("Italy",
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
                                                                 "Poland")) %>%
         left_join(sort_date), aes(x = start_date, y = reorder(country_name, desc(sort_date)), group = country_name, colour = var)) +
  geom_line(colour = "black") +
  geom_point(size = 4, alpha = 0.5) +
  scale_colour_manual(values = c("#2a8e9d", "#e9c46a", "#b3c46a")) +
  bbc_style() +
  theme(legend.position = "right",
        legend.text = element_text(size = 8)) +
  labs(title = "Response timeline") +
  geom_vline(xintercept = as.Date("20200311", format = "%Y%m%d"))

facet_strindex <- strindex %>%
  left_join(first_cases) %>%
  filter(date < as.Date("2020-04-01", format = "%Y-%m-%d"),
         country_name %in% c("Italy", "Spain", "France", "Poland", "Romania", "Hungary", "Netherlands")) %>%
  arrange(start_date)

facet_strindex$country_name = factor(facet_strindex$country_name, unique(facet_strindex$country_name))


ggplot(facet_strindex, 
       aes(x = date, y = stringency_index_average)) +
  geom_line() +
  geom_vline(aes(xintercept = start_date)) +
  facet_wrap(~country_name, ncol = 1)


Netherlands:
"https://bmcpublichealth.biomedcentral.com/articles/10.1186/s12889-020-09612-6"
  
               


table <- strindex %>%
  group_by(country_name) %>%
  arrange(date) %>%
  summarize(stringency = list(stringency_index_average), .groups = "drop") %>%
  left_join(first_cases) %>%
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


ggplot(cases %>%
         filter(date < as.Date("20200331", format = "%Y%m%d")), aes(x = date, y = rolling_pc, group = as.factor(mort_cluster_ids), colour = as.factor(mort_cluster_ids))) +
  geom_line() +
  geom_vline(xintercept = as.Date("20200311", format = "%Y%m%d")) +
  labs(title = "Kicking off the first wave",
       subtitle = "Confirmed cases per capita. by cluster")


ggplot(cases %>%
         filter(date >= as.Date("20200701", format = "%Y%m%d") & date <= as.Date("20210131", format = "%Y%m%d")), aes(x = date, y = rolling_pc, group = as.factor(mort_cluster_ids), colour = as.factor(mort_cluster_ids))) +
  geom_line() +
  labs(title = "Ok now the second winter")
