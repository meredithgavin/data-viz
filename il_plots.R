library(tidyverse)
library(rvest)
library(styler)
library(janitor)
library(scales)
library(sf)
library(spData)
library(rnaturalearth)
library(readr)
library(readxl)
library(usmap)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(stringr)
library(leaflet)
library(shiny)
library(htmlwidgets)
library(RColorBrewer)


# BACKGROUND WORK

# load data

data_path <-
  "/Users/meredithgavin/Desktop/final-project-meredith-haley-joshua/tidy_data"


tech_health_merged <- read_csv(file.path(data_path, "tech_health_merged.csv"))

# clean data as needed

tech_health_pivoted <- tech_health_merged %>%
  pivot_longer(cols = `All Other`:`Wood/Wood Waste Biomass`,
               values_to = "air_quality") %>%
  filter(!grepl("000", state)) %>%
  group_by(state, county, interval_year, fips) %>%
  summarise(avg_air_quality = mean(air_quality)) %>%
  ungroup()

# prepare fips codes (county markers) for census data

data("fips_codes")

# clean fips to make the codes 4-5 digits

fips_codes <- fips_codes %>%
  mutate(fips = paste(state_code, county_code, sep = "")) %>%
  mutate(fips = str_remove(fips, "^0+")) %>%
  mutate(county = str_remove(county, "County")) %>%
  mutate(county = str_trim(county)) %>%
  mutate(fips = as.character(fips))


# select census variables to pull for the county shapefile

census_vars <- c("P001001", "CD111")

# census variable codes: https://api.census.gov/data/2010/dec/sf1/variables.html

# load the census us counties shapefile, keeping fips codes 
# (set keep_geo_vars = TRUE)

us_counties <- get_decennial(
  geography = "county",
  variables = census_vars,
  year = 2010,
  geometry = TRUE,
  shift_geo = FALSE,
  keep_geo_vars = TRUE
)

# add pre-configured fips codes to census data


us_counties_fips <- us_counties %>%
  left_join(fips_codes,
            by = c("STATE" = "state_code", "COUNTY" = "county_code")) %>%
  mutate(fips = as.character(fips))

# make data mappable by transforming geometry to correct type and
# pivot to include census data in individual columns

st_transform(us_counties_fips, 4326) 

# make fips codes compatible by changing type

tech_health_pivoted$fips <- as.character(tech_health_pivoted$fips)


# merge counties shapefile with data

us_counties_tech_health <- us_counties_fips %>%
  right_join(tech_health_pivoted, by = "fips") %>%
  st_transform(4326)

# BUILDING INTERACTIVE LEAFLET MAP

# plot data

# Illinois because we're in Chicago - so why not?

# first - a static map to get an idea of the data 

il_air_quality <- us_counties_tech_health %>%
  filter(state_name == "Illinois") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = avg_air_quality)) +
  labs(title = "Average Air Quality by County",
       subtitle = "IL, 2010",
       fill = "Air Quality Measure")

il_air_quality

ggsave("il_air_quality.png", plot = il_air_quality)



# create interactive plot using leaflet

# set up the color palettes for the leaflet map
air_quality_palette <- colorNumeric("Purples", NULL, n = 5)

air_label_palette <-
  colorBin(palette = "Purples", 4, domain = us_counties_tech_health$avg_air_quality)

air_quality_labels <-
  sprintf(
    "<strong>%s</strong><br/>%s Average Air Quality 2010 / mi<sup>2</sup>",
    us_counties_tech_health$NAME.y,
    us_counties_tech_health$avg_air_quality
  ) %>%
  lapply(htmltools::HTML)
#resource: https://rstudio.github.io/leaflet/choropleths.html

# create the interactive map 

il_air_quality_leaflet <- us_counties_tech_health %>%
  filter(interval_year == '2010',
         state_name == "Illinois") %>%
  leaflet(us_counties_tech_health,
          height = 1000,
          width = 1000) %>%
  addPolygons(
    data = us_counties_tech_health,
    stroke = TRUE,
    weight = 1,
    color = "gray",
    fillColor = ~ air_quality_palette(us_counties_tech_health$avg_air_quality),
    fillOpacity = 0.8,
    smoothFactor = 0.5,
    label = air_quality_labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    ),
    highlightOptions = highlightOptions(
      weight = 3,
      color = "red",
      bringToFront = TRUE
    )
  ) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addLegend(position = "bottomleft",
            values = ~ avg_air_quality,
            pal = air_quality_palette,
            title = "2010 Average Air Quality by County - IL")

# check it out
il_air_quality_leaflet

# save the plot as an HTML widget
saveWidget(il_air_quality_leaflet, file = "il_air_quality_leaflet.html")


# SHINY
# now let's make a shiny map for IL average air quality in 2010

# filter data in advance

il_air_quality_shiny <- us_counties_tech_health %>%
  filter(interval_year == "2010",
         state_name == "Illinois")

ui_il <-
  fluidPage(
    tags$style(type = "text/css", "html, body, .leaflet {width:100%; height:100%}"),
    
    titlePanel("Illinois Average Air Quality by County - 2010"),
    # define output
    leafletOutput("map")
  )

server_il <-
  function(input, output)
    ({
      output$map <- renderLeaflet({
        
        # set up the color palettes (same as previous)
        air_quality_palette <- colorNumeric("Purples", NULL,
                                            na.color = "gray")
        
        air_label_palette <-
          colorBin(palette = "Purples", 4, domain = il_air_quality_shiny$avg_air_quality)
        
        air_quality_labels <-
          sprintf(
            "<strong>%s</strong><br/>%s - Average Air Quality 2010 </sup>",
            il_air_quality_shiny$NAME.y,
            il_air_quality_shiny$avg_air_quality
          ) %>%
          lapply(htmltools::HTML)
        #resource: https://rstudio.github.io/leaflet/choropleths.html
        
        # map data
        leaflet(il_air_quality_shiny) %>%
          addPolygons(
            data = il_air_quality_shiny,
            stroke = TRUE,
            weight = 1,
            color = "gray",
            fillColor = ~ air_quality_palette(il_air_quality_shiny$avg_air_quality),
            fillOpacity = 0.8,
            smoothFactor = 0.5,
            label = air_quality_labels,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            ),
            highlightOptions = highlightOptions(
              weight = 3,
              color = "red",
              bringToFront = TRUE
            )
          ) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addLegend(position = "bottomleft",
                    values = il_air_quality_shiny$avg_air_quality,
                    #resource: https://community.rstudio.com/t/no-applicable-method-for-metadata-applied-to-an-object-of-class-null-with-leaflet-map/47720
                    pal = air_quality_palette,
                    title = "2010 Average Air Quality by County - IL")
        
      })
    })

# run the shiny App
shinyApp(ui_il, server_il)