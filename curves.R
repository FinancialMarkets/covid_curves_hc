library(dplyr)
library(readr)
library(highcharter)
library(htmlwidgets)

options(browser = "/usr/bin/firefox")

### cases <- read_csv("../../google_charts_data/line_chart_african_countries/line_chart_cases.csv")
### deaths <- read_csv("../../google_charts_data/line_chart_african_countries/line_chart_deaths.csv")

data <- read_csv("../../african_latest_data.csv")

data$dateRep <- as.Date(data$dateRep, format="%d/%m/%Y")
data$popData2018 <- data$popData2018 / 1000000
data$cases_per_million <- data$cumulative_cases / data$popData2018
data$deaths_per_million <- data$cumulative_deaths / data$popData2018

data_graphic <- data[, c("dateRep", "countriesAndTerritories", "cases_per_million", "cumulative_cases", "cases", "deaths_per_million", "cumulative_deaths", "deaths")]

## remove france

data_graphic <- subset(data_graphic, countriesAndTerritories != "France")

## add moving average

## 2 sided => backwards and forwards, 1 only backward
ma <- function(x, n = 3){ifelse(x > 3, stats::filter(x, rep(1 / n, n), sides = 1), 0)}

## use the moving average to determine when we start the curve (when moving average is > 3 like in the El Pais graphics).

data_graphic <- data_graphic %>% 
    group_by(countriesAndTerritories) %>%
    arrange(dateRep) %>%
    mutate(mov_avg_new = ma(cases)) %>%
    mutate(mov_avg_cum_cases = ma(cumulative_cases))

## group and remove rows prior to 3 cases, but leave if there are fewer than 3 after the initial date------

## get_min_date <- function(){min(x[x$cases >= 3]$dateRep)}

test <- data_graphic %>%
    group_by(countriesAndTerritories) %>%
    arrange(dateRep) %>%
    ## find date when ma >= 3, and remove all prior to this date------
    mutate(cases_3 = cases >= 3)

test2 <- test %>%
    group_by(countriesAndTerritories) %>%
    arrange(dateRep) %>%
    filter(cases_3 == TRUE) %>%
    mutate(first_date = min(dateRep))

## map from country to first date----
test2 <- unique(test2[, names(test2) %in% c("countriesAndTerritories", "first_date")])

## first date is good, but need to apply this date to data_graphic (not test2 because this gets rid of obs less then 3 post min date).

data_graphic_with_min <- merge(data_graphic, test2, by = "countriesAndTerritories")

data_graphic_with_min <- subset(data_graphic_with_min, dateRep >= first_date)

### Last, need to create days since MA cases > 3-----
data_graphic_with_min$const <- 1
data_graphic_with_min <- data_graphic_with_min %>%
    group_by(countriesAndTerritories) %>%
    arrange(dateRep) %>%
    mutate(days_since_ma_3 = cumsum(const))

## cases graphic
cases_graphic <- hchart(data_graphic_with_min, "line", hcaes(x = days_since_ma_3, y = cumulative_cases, group = countriesAndTerritories)) %>%
    hc_boost(enabled=FALSE) %>%
    hc_plotOptions(
        series = list(
            events = list(
                mouseOver = JS("function() { if(this.options.color !== '#0066CC') {this.update({color: '#0066CC'})} }"),
                mouseOut = JS("function() { if(this.options.color === '#0066CC') {this.update({color: '#ddd'})} }")
            ),
            states = list(
                hover = list(
                    enabled = TRUE,
                    lineWidth = 10
                )
            )
        )) %>%
    hc_tooltip(enabled = FALSE) %>%
    hc_xAxis(title = list(text = "Days Since an Average of 3 Cases")) %>%
    hc_yAxis(title = list(text = "Number of Cases"), type = "logarithmic") %>%
    hc_title(
        text = "The Number of Total Cases Over Time",
        useHTML = TRUE) %>% 
    hc_colors("#dbdbdb")

cases_graphic

saveWidget(cases_graphic, "cases_graphic.html")


## deaths graphic
deaths_graphic <- hchart(data_graphic_with_min, "line", hcaes(x = days_since_ma_3, y = cumulative_deaths, group = countriesAndTerritories)) %>%
    hc_boost(enabled=FALSE) %>%
    hc_plotOptions(
        series = list(
            events = list(
                mouseOver = JS("function() { if(this.options.color !== '#0066CC') {this.update({color: '#0066CC'})} }"),
                mouseOut = JS("function() { if(this.options.color === '#0066CC') {this.update({color: '#ddd'})} }")
            ),
            states = list(
                hover = list(
                    enabled = TRUE,
                    lineWidth = 10
                )
            )
        )) %>%
    hc_tooltip(enabled = FALSE) %>%
    hc_xAxis(title = list(text = "Days Since an Average of 3 Cases")) %>%
    hc_yAxis(title = list(text = "Number of Deaths"), type = "logarithmic") %>%
    hc_title(
        text = "The Number of Total Deaths Over Time",
        useHTML = TRUE) %>% 
    hc_colors("#dbdbdb")

deaths_graphic

saveWidget(deaths_graphic, "deaths_graphic.html")


## cases per million graphic
cases_per_million_graphic <- hchart(data_graphic_with_min, "line", hcaes(x = days_since_ma_3, y = cases_per_million, group = countriesAndTerritories)) %>%
    hc_boost(enabled=FALSE) %>%
    hc_plotOptions(
        series = list(
            events = list(
                mouseOver = JS("function() { if(this.options.color !== '#0066CC') {this.update({color: '#0066CC'})} }"),
                mouseOut = JS("function() { if(this.options.color === '#0066CC') {this.update({color: '#ddd'})} }")
            ),
            states = list(
                hover = list(
                    enabled = TRUE,
                    lineWidth = 10
                )
            )
        )) %>%
    hc_tooltip(enabled = FALSE) %>%
    hc_xAxis(title = list(text = "Days Since an Average of 3 Cases")) %>%
    hc_yAxis(title = list(text = "Number of Cases_Per_Million"), type = "logarithmic") %>%
    hc_title(
        text = "The Number of Total Cases Per Million Over Time",
        useHTML = TRUE) %>% 
    hc_colors("#dbdbdb")

cases_per_million_graphic

saveWidget(cases_per_million_graphic, "cases_per_million_graphic.html")


## deaths per million graphic
deaths_per_million_graphic <- hchart(data_graphic_with_min, "line", hcaes(x = days_since_ma_3, y = deaths_per_million, group = countriesAndTerritories)) %>%
    hc_boost(enabled=FALSE) %>%
    hc_plotOptions(
        series = list(
            events = list(
                mouseOver = JS("function() { if(this.options.color !== '#0066CC') {this.update({color: '#0066CC'})} }"),
                mouseOut = JS("function() { if(this.options.color === '#0066CC') {this.update({color: '#ddd'})} }")
            ),
            states = list(
                hover = list(
                    enabled = TRUE,
                    lineWidth = 10
                    #color = '#0066CC'
                )
            )
        )) %>%
    hc_tooltip(enabled = FALSE) %>%
    hc_xAxis(title = list(text = "Days Since an Average of 3 Cases")) %>%
    hc_yAxis(title = list(text = "Number of Deaths_Per_Million"), type = "logarithmic") %>%
    hc_title(
        text = "The Number of Total Deaths Per Million Over Time",
        useHTML = TRUE) %>% 
    hc_colors("#dbdbdb")

deaths_per_million_graphic

saveWidget(deaths_per_million_graphic, "deaths_per_million_graphic.html")
