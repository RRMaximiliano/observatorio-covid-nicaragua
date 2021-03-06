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
nicaragua <- read_csv(file.path(data, "observatorio_nicaragua.csv"))


nicaragua %>% 
    filter(cases > 100) %>% 
    mutate(
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

# Limpiar datos -----------------------------------------------------------

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
    mutate(
        countries = case_when(countries == "Nicaragua" ~ "Nicaragua",
                              TRUE ~ as.character(countries))
    ) %>% 
    filter(countries != "Nicaragua")

# Homogenizar variables ----------------------------------------------------
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
date_nic <- full_cov_curve %>% 
    filter(countries == "Nicaragua") %>% 
    summarise(date = max(date)) %>% 
    select(date)

date_nic <- date_nic$date[1]

# Cases -------------------------------------------------------------------
full_cov_curve %>% 
    filter(cu_cases > 10) %>% 
    mutate(
        countries = case_when(countries == "El_Salvador" ~ "El Salvador",
                              countries == "Costa_Rica" ~ "Costa Rica",
                              TRUE ~ countries),
        days_elapsed = date - min(date),
        end_label = ifelse(date == max(date), countries, NA),
        end_label = case_when(date == max(date) & countries == "Nicaragua" ~ "Nicaragua",
                              TRUE ~ end_label)
    ) %>%
    ggplot(mapping = aes(x = days_elapsed, y = cu_cases, 
                         color = countries, label = end_label, 
                         group = countries)) + 
    geom_line(size = 0.8) + 
    geom_text_repel(nudge_x = 2.1,
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
         subtitle = paste("Data as of", format(max(full_cov_curve$date), "%A, %B %e, %Y")), 
         caption = paste("Data: European Centre for Disease Prevention and Control.\nData for Nicaragua: Observatorio Ciudadano Covid-19. Data as of", format(max(date_nic), "%A, %B %e, %Y"), "\nPlot: @rrmaximiliano")
    ) + 
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
        end_label = case_when(date == max(date) & countries == "Nicaragua" ~ "Nicaragua",
                              TRUE ~ end_label)
    ) %>%
    ggplot(mapping = aes(x = days_elapsed, y = cu_deaths, 
                         color = countries, label = end_label, 
                         group = countries)) + 
    geom_line(size = 0.8) + 
    geom_text_repel(nudge_x = 1.1,
                    nudge_y = -0.07, 
                    family = "Roboto Condensed", 
                    segment.color = NA) + 
    guides(color = FALSE) + 
    scale_color_manual(values = prismatic::clr_darken(paletteer_d("ggsci::category20_d3"), 0.2)) +
    scale_y_log10(labels = scales::comma_format(accuracy = 1)) + 
    labs(x = "Days since 10th reported confirmed death", 
         y = "Cumulative Number of Cases (log scale)", 
         title = "Cumulative Deaths from COVID-19, Central America",
         subtitle = paste("Data as of", format(max(full_cov_curve$date), "%A, %B %e, %Y")), 
         caption = paste("Data: European Centre for Disease Prevention and Control.\nData for Nicaragua: Observatorio Ciudadano Covid-19. Data as of", format(max(date_nic), "%A, %B %e, %Y"), "\nPlot: @rrmaximiliano")
    ) +  
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


# DEPA --------------------------------------------------------------------
# Read data ---------------------------------------------------------------
departamentos <- read_csv(file.path(data, "observatorio_nicaragua_departamentos.csv"))

departamentos %>% 
    group_by(departamento) %>% 
    filter(cases > 100) %>%
    mutate(
        days_elapsed = date - min(date),
        end_label = ifelse(date == max(date), departamento, NA)
    ) %>%
    ggplot(mapping = aes(x = days_elapsed, 
                         y = cases, 
                         label = end_label,
                         color = departamento, 
                         group = departamento)) + 
    geom_line(size = 0.8) +
    geom_text_repel(nudge_x = 2,
                    nudge_y = 0.07,
                    family = "Roboto Condensed", 
                    segment.color = NA) + 
    guides(color = FALSE) + 
    scale_color_manual(values = prismatic::clr_darken(paletteer_d("ggsci::category20_d3"), 0.2)) +
    scale_y_log10(labels = scales::comma_format(accuracy = 1)) + 
    labs(x = "Days since 100th reported suspected case", 
         y = "Cumulative Number of Suspected Cases (log 10 scale)", 
         title = "Cumulative Suspected Cases of COVID-19 in Nicaragua",
         subtitle = paste("Data as of", format(max(departamentos$date), "%A, %B %e, %Y")), 
         caption = "Data: Observatorio Ciudadano Covid-19\nPlot: @rrmaximiliano") + 
    theme_ipsum_rc() +
    theme(
        plot.caption = element_text(hjust = 0)
    )

# Save Plot
ggsave(paste0(figures, "/cases-departamentos", date, ".pdf"), 
       device = cairo_pdf, scale = 0.8,
       height = 8, width = 12)

ggsave(paste0(figures, "/cases-departamentos", date, ".png"), 
       dpi = 750, scale = 0.8,
       height = 8, width = 12)

# Highlight ---------------------------------------------------------------

library(gghighlight)

plot_departamento <- departamentos %>% 
    group_by(departamento) %>% 
    filter(cases > 10) %>%
    mutate(
        days_elapsed = date - min(date)
    )

plot_departamento %>% 
    ggplot(aes(days_elapsed, cases, color = departamento)) +
    geom_path(color = "#327da8",
              size = 0.85, 
              lineend = "round") +
    gghighlight() +
    geom_point(data = plot_departamento %>% filter(date == max(date)), 
               size = 1.1, 
               shape = 21, 
               color = "#327da8",
               fill = "#327da8"
    ) + 
    facet_wrap(~ departamento, nrow = 3, scales = "free_x", labeller = label_wrap_gen(width = 20)) +
    scale_y_log10(labels = scales::label_number_si()) + 
    labs(x = "Days since 10th reported suspected case", 
         y = "Cumulative Number of Suspected Cases (log 10 scale)", 
         title = "Cumulative Suspected Cases of COVID-19 in Nicaragua",
         subtitle = paste("Data as of", format(max(departamentos$date), "%A, %B %e, %Y")), 
         caption = "Data: Observatorio Ciudadano Covid-19\nPlot: @rrmaximiliano") + 
    theme_ipsum_rc() +
    theme(
        strip.text = element_text(colour = "#327da8", face = "bold"),
        plot.caption = element_text(hjust = 0),
        # axis.text.x = element_text(size = rel(1)),
        # axis.text.y = element_text(size = rel(1)),
        # axis.title.x = element_text(size = rel(1)),
        # axis.title.y = element_text(size = rel(1)),
        # legend.text = element_text(size = rel(1))
    )

# Save Plot
ggsave(paste0(figures, "/cases-departamentos-todos-", date, ".pdf"), 
       device = cairo_pdf, scale = 0.8,
       height = 10, width = 14)

ggsave(paste0(figures, "/cases-departamentos-todos-", date, ".png"), 
       dpi = 750, scale = 0.8,
       height = 10, width = 14)


plot_departamento <- departamentos %>% 
    filter(departamento %in% c("Matagalpa", "Le�n", "Granada", "Masaya", "Chinandega", "Estel�")) %>%
    group_by(departamento) %>% 
    filter(cases > 10) %>%
    mutate(
        days_elapsed = date - min(date)
    )

plot_departamento %>% 
    ggplot(aes(days_elapsed, cases, color = departamento)) +
    geom_path(color = "#327da8",
              size = 0.85, 
              lineend = "round") +
    gghighlight() +
    geom_point(data = plot_departamento %>% filter(date == max(date)), 
               size = 1.1, 
               shape = 21, 
               color = "#327da8",
               fill = "#327da8"
    ) + 
    facet_wrap(~ departamento, nrow = 3, scales = "free_x", labeller = label_wrap_gen(width = 20)) +
    scale_y_log10(labels = scales::label_number_si()) + 
    labs(x = "Days since 10th reported suspected case", 
         y = "Cumulative Number of Suspected Cases (log 10 scale)", 
         title = "Cumulative Suspected Cases of COVID-19 in Nicaragua",
         subtitle = paste("Data as of", format(max(departamentos$date), "%A, %B %e, %Y")), 
         caption = "Data: Observatorio Ciudadano Covid-19\nPlot: @rrmaximiliano") + 
    theme_ipsum_rc() +
    theme(
        strip.text = element_text(colour = "#327da8", face = "bold"),
        plot.caption = element_text(hjust = 0),
        # axis.text.x = element_text(size = rel(1)),
        # axis.text.y = element_text(size = rel(1)),
        # axis.title.x = element_text(size = rel(1)),
        # axis.title.y = element_text(size = rel(1)),
        # legend.text = element_text(size = rel(1))
    )

# Save Plot
ggsave(paste0(figures, "/cases-departamentos-todos-", date, ".pdf"), 
       device = cairo_pdf, scale = 0.8,
       height = 10, width = 14)

ggsave(paste0(figures, "/cases-departamentos-todos-", date, ".png"), 
       dpi = 750, scale = 0.8,
       height = 10, width = 14)


# Col Graphs --------------------------------------------------------------
library(zoo)

departamentos %>%
    group_by(departamento) %>% 
    mutate(cases_per = cases - lag(cases, default = cases[1]),
           rollmean = rollmean(cases_per, 7, na.pad = T)) %>% 
    ggplot(aes(date)) +
    geom_bar(aes(y = cases_per), stat = "identity", width = 0.5, color = "grey") + 
    geom_line(aes(y = rollmean), color = "#327da8", size = 1) + 
    facet_wrap(~departamento, scales = "free_y") +
    labs(x = "Date", 
         y = "Suspected Cases per day", 
         title = "Suspected Cases of COVID-19 per day in Nicaragua",
         subtitle = paste("Data as of", format(max(departamentos$date), "%A, %B %e, %Y")), 
         caption = "Notes: Line represents 7-day moving average.\nData: Observatorio Ciudadano Covid-19\nPlot: @rrmaximiliano") + 
    theme_ipsum_rc() +
    theme(
        strip.text = element_text(colour = "#327da8", face = "bold"),
        plot.caption = element_text(hjust = 0),
        # axis.text.x = element_text(size = rel(1)),
        # axis.text.y = element_text(size = rel(1)),
        # axis.title.x = element_text(size = rel(1)),
        # axis.title.y = element_text(size = rel(1)),
        # legend.text = element_text(size = rel(1))
    )

# Save Plot
ggsave(paste0(figures, "/cases_per_day-departamentos-todos-", date, ".pdf"), 
       device = cairo_pdf, scale = 0.8,
       height = 10, width = 14)

ggsave(paste0(figures, "/cases_per_day-departamentos-todos-", date, ".png"), 
       dpi = 750, scale = 0.8,
       height = 10, width = 14)

nicaragua %>% 
mutate(cases_per = cases - lag(cases, default = cases[1]),
       rollmean = rollmean(cases_per, 7, fill = NA)) %>% View()
