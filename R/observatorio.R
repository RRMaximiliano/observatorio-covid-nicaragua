# Packages ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(socviz)
library(ggrepel)
library(paletteer)
library(hrbrthemes)
library(extrafont)

# Get current date
date <- Sys.Date()

# Path --------------------------------------------------------------------
project <- file.path("D:/Documents/GitHub/observatorio-covid-nicaragua")
data    <- file.path(project, "data")
figures <- file.path(project, "figs")

# Read data ---------------------------------------------------------------
nicaragua <- read_csv(file.path(data, "observatorio_cases.csv"))

nicaragua %>% 
    filter(cases > 100) %>% 
    mutate(
        date = lubridate::ymd(date),
        days_elapsed = date - min(date)
    ) %>%
    ggplot(mapping = aes(x = days_elapsed, y = cases)) + 
    geom_line(size = 0.8) + 
    guides(color = FALSE) + 
    scale_color_manual(values = prismatic::clr_darken(paletteer_d("ggsci::category20_d3"), 0.2)) +
    scale_y_continuous(labels = scales::comma_format(accuracy = 1), 
                       breaks = 2^seq(4, 12),
                       trans = "log2") + 
    theme_ipsum_rc() 

# ECDC Data ---------------------------------------------------------------
## Download today's excel file, saving it to data/ and reading it in
get_ecdc_data <- function(url = "https://www.ecdc.europa.eu/sites/default/files/documents/",
                          fname = "COVID-19-geographic-distribution-worldwide-", 
                          date = lubridate::today(), 
                          ext = "xlsx", 
                          dest = "data") {
    
    target <-  paste0(url, fname, date, ".", ext)
    message("target: ", target)
    
    destination <- fs::path(here::here("data"), paste0(fname, date), ext = ext)
    message("saving to: ", destination)
    
    tf <- tempfile(fileext = ext)
    curl::curl_download(target, tf)
    fs::file_copy(tf, destination)
    
    switch(ext, 
           xls = janitor::clean_names(readxl::read_xls(tf)),
           xlsx = janitor::clean_names(readxl::read_xlsx(tf))
    )
}                          

covid_raw <- get_ecdc_data(url = "https://www.ecdc.europa.eu/sites/default/files/documents/",
                           fname = "COVID-19-geographic-disbtribution-worldwide-",
                           ext = "xlsx")
covid <- covid_raw %>%
    mutate(date = lubridate::ymd(date_rep),
           iso2 = geo_id) %>% 
    filter(cases != -9)

cov_curve <- covid %>%
    select(date, countries_and_territories, iso2, cases, deaths) %>%
    filter(countries_and_territories %in% c("Nicaragua", "Costa_Rica", "El_Salvador", "Guatemala", "Honduras", "Panama", "Belize")) %>% 
    drop_na(iso2) %>%
    group_by(iso2) %>%
    arrange(date) %>%
    mutate(cu_cases = cumsum(cases), 
           cu_deaths = cumsum(deaths)) %>% 
    rename(countries = countries_and_territories) %>% 
    select(date, countries, iso2, cu_cases, cu_deaths) %>% 
    filter(countries != "Nicaragua")

# Homegnizar variables ----------------------------------------------------
nic_cov_curve <- nicaragua %>% 
    rename(cu_cases = cases,
           cu_deaths = deaths) %>% 
    mutate(countries = "Nicaragua", iso2 = "NI") %>% 
    select(date, countries, iso2, cu_cases, cu_deaths)

full_cov_curve <- cov_curve %>% 
    bind_rows(nic_cov_curve) %>% 
    arrange(date, countries)

write_csv(full_cov_curve, path = file.path(data, "full_cov_curve.csv"))

# Plot --------------------------------------------------------------------

# Cases -------------------------------------------------------------------
full_cov_curve %>% 
    filter(cu_cases > 10) %>% 
    mutate(
        countries = case_when(countries == "El_Salvador" ~ "El Salvador",
                              countries == "Costa_Rica" ~ "Costa Rica",
                              TRUE ~ countries),
        days_elapsed = date - min(date),
        end_label = ifelse(date == max(date), countries, NA),
        end_label = case_when(date == ymd("2020-06-17") & countries == "Nicaragua" ~ "Nicaragua",
                              TRUE ~ end_label)
    ) %>%
    ggplot(mapping = aes(x = days_elapsed, y = cu_cases, 
                         color = countries, label = end_label, 
                         group = countries)) + 
    geom_line(size = 0.8) + 
    geom_text_repel(nudge_x = 1.1,
                    nudge_y = 0.1, 
                    family = "Roboto Condensed", 
                    segment.color = NA) + 
    guides(color = FALSE) + 
    scale_color_manual(values = prismatic::clr_darken(paletteer_d("ggsci::category20_d3"), 0.2)) +
    scale_y_log10(labels = scales::comma_format(accuracy = 1)) + 
    # scale_y_continuous(labels = scales::comma_format(accuracy = 1),
    #                    breaks = 2^seq(4, 12),
    #                    trans = "log2") +
    labs(x = "Days since 10th reported confirmed case", 
         y = "Cumulative number of cases (log scale)", 
         title = "Cumulative Cases from COVID-19, Central America",
         subtitle = "Data as of Sunday 21, 2020. Data for Nicaragua as of Wednesday, 17, 2020.",
         caption = "Data: European Centre for Disease Prevention and Control.\nData for Nicaragua: Observatorio Ciudadano Covid-19.\nPlot: @rrmaximiliano") + 
    theme_ipsum_rc() +
    theme(
        plot.caption = element_text(hjust = 0)
    )

# Save Plot
ggsave(paste0(figures, "/cases-", date, ".pdf"), 
       device = cairo_pdf, scale = 0.8,
       height = 8, width = 10)

ggsave(paste0(figures, "/cases-", date, ".png"), 
       dpi = 750, scale = 0.8,
       height = 8, width = 10)

# Deaths ------------------------------------------------------------------
full_cov_curve %>% 
    filter(cu_deaths > 10) %>% 
    mutate(
        countries = case_when(countries == "El_Salvador" ~ "El Salvador",
                              countries == "Costa_Rica" ~ "Costa Rica",
                              TRUE ~ countries),
        days_elapsed = date - min(date),
        end_label = ifelse(date == max(date), countries, NA),
        end_label = case_when(date == ymd("2020-06-17") & countries == "Nicaragua" ~ "Nicaragua",
                              TRUE ~ end_label)
    ) %>%
    ggplot(mapping = aes(x = days_elapsed, y = cu_deaths, 
                         color = countries, label = end_label, 
                         group = countries)) + 
    geom_line(size = 0.8) + 
    geom_text_repel(nudge_x = 1.1,
                    nudge_y = 0.1, 
                    family = "Roboto Condensed", 
                    segment.color = NA) + 
    guides(color = FALSE) + 
    scale_color_manual(values = prismatic::clr_darken(paletteer_d("ggsci::category20_d3"), 0.2)) +
    scale_y_log10(labels = scales::comma_format(accuracy = 1)) + 
    # scale_y_continuous(labels = scales::comma_format(accuracy = 1),
    #                    breaks = 2^seq(4, 12),
    #                    trans = "log2") +
    labs(x = "Days since 10th reported confirmed death", 
         y = "Cumulative Number of Cases (log scale)", 
         title = "Cumulative Deaths from COVID-19, Central America",
         subtitle = "Data as of Sunday 21, 2020. Data for Nicaragua as of Wednesday, 17, 2020.",
         caption = "Data: European Centre for Disease Prevention and Control.\nData for Nicaragua: Observatorio Ciudadano Covid-19.\nPlot: @rrmaximiliano") + 
    theme_ipsum_rc() +
    theme(
        plot.caption = element_text(hjust = 0)
    )

# Save Plot
ggsave(paste0(figures, "/deaths-", date, ".pdf"), 
       device = cairo_pdf, scale = 0.8,
       height = 8, width = 10)

ggsave(paste0(figures, "/deaths-", date, ".png"), 
       dpi = 750, scale = 0.8,
       height = 8, width = 10)

