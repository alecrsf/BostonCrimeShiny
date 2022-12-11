#============== Setup ==================
library(shiny)
library(modules)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(htmltools)

library(highcharter)
options(highcharter.theme = hc_theme_hcrt(tooltip = list(valueDecimals = 2)))
library(apexcharter)
library(stringr)

library(countup)
library(glue)

library(reactable)
library(sparkline)

library(leaflet)

library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)
library(tidyr)
library(purrr, warn.conflicts = FALSE)

'%!in%' <- function(x,y)!('%in%'(x,y))

#============== Theme ==================
app_title = "Boston Crime"
primary = "#3D88D2"
secondary = "#1D4D7D"
background = "#191919"

text_color <- "#484848"
text_color_light <- "#767676"
text_color_lighter <- "#d7dce1"
bg_color <- "white"
gray <- "#eee"

htmltools::tags$link(href = "https://fonts.googleapis.com/css?family=IBM+Plex+Sans", rel = "stylesheet")

#============== Data ==================
load("data/BostonCrime.RData")

# Read geojson as SF for maps --------
mapdata <- sf::st_read("https://raw.githubusercontent.com/blackmad/neighborhoods/master/boston.geojson") |>
	select(name, geometry) |> 
	rename(neighborhood = "name")

# Crimes of all years --------
crimes <- BostonCrime |> 
	group_by(neighborhood) |>
	count(crime_type) |>
	pivot_wider(names_from = crime_type,
							values_from = n) 
crimes$Crimes <- rowSums(crimes[, 2:9])

# Average crimes between these years --------
crimes_avg <- BostonCrime |> 
	group_by(neighborhood, year) |>
	count(crime_type) |> 
	filter(crime_type == "violence") |> 
	pivot_wider(names_from = crime_type,
							values_from = n) |> 
	group_by(neighborhood) |> 
	summarise(avg_violence = mean(violence, na.rm = TRUE)) 
	

# Data for elevated cloropleth --------
mapdata <- mapdata |> 
	left_join(crimes |> select(1,11), 
						by = "neighborhood") 


#============== Modules ==================
source("modules/drilldown.R")
source("modules/map.R")
source("modules/overview.R")

#============== Utils ==================
source("utils/input.R")
source("utils/metrics_card.R")
source("utils/get_percent.R")
