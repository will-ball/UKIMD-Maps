setwd("C:/Users/PGR3/Desktop/WB/Data/New folder/UKIMD Map")

install.packages("sf",
                 "tidyverse",
                 "classInt",
                 "viridis")

library(sf)
library(tidyverse)
library(classInt)
library(viridis)

## Load in LSOA Data

sf_LSOA <- st_read(
  "https://raw.githubusercontent.com/gausie/LSOA-2011-GeoJSON/master/lsoa.geojson",
                 quiet = TRUE)
glimpse(sf_LSOA) # 34753 areas = 32844 (Eng) + 1909 (Wal)

st_geometry(sf_LSOA)

## Plot Boundaries

plot(st_geometry(sf_LSOA))

## For selecting smaller areas within boundaries, a lookup table is required
#  From lookup file, collects corresponding LSOA2011 codes from local authority name

lookup <- read_csv("https://opendata.arcgis.com/datasets/8c05b84af48f4d25a2be35f1d984b883_0.csv") %>%
filter(LAD18NM %in% c("Suffolk Coastal")) %>%
pull(LSOA11CD)

## Filter the boundaries for area of interest by LSOA codes identified in lookup above

sf_suffco <- sf_LSOA %>%
filter(LSOA11CD %in% lookup)

plot(st_geometry(sf_suffco))

## Rename code and name
sf_EngWal <- sf_LSOA %>%
  select(area_code = LSOA11CD, area_name = LSOA11NM)

sf_suffco <- sf_suffco %>%
  select(area_code = LSOA11CD, area_name = LSOA11NM)

## Download IMD file & check variable names of interest

imd <- read_tsv("https://data.bris.ac.uk/datasets/1ef3q32gybk001v77c1ifmty7x/uk_imd_scores_data.txt")

# Here you can add a new variable if not looking simply at quintiles

## Create joined df based on area codes required (make sure variable types the same)

sf_suffco_imd <- left_join(sf_suffco, imd, by = "area_code")
glimpse(sf_suffco_imd)

sf_EngWal_imd <- left_join(sf_EngWal, imd, by = "area_code")

## Plot Cloropleth Map

ggplot() +                                                                          # initialise a ggplot object
  geom_sf(data = sf_suffco_imd,                                                      # add a simple features (sf) object
          aes(fill = sf_suffco_imd$uk_imd_england_quintile),                            # specify what variable to fill on
          alpha = 0.8,                                                              # add transparency to the fill
          colour = 'white',                                                         # make polygon boundaries white
          size = 0.2) +                                                             # adjust width of polygon boundaries
  scale_fill_brewer(palette = "",                                               # choose a http://colorbrewer2.org/ palette
                    name = "UK IMD Quintiles") +                                    # add legend title
  labs(x = NULL, y = NULL,                                                          # drop axis titles
       title = "Deprivation Quintiles for Suffolk Coastal by LSOA",                # add title
       subtitle = "Sources: Abel et al (2016) BMJ and Office for National Statistics under Open
       Government License v3.0",                                                    # add subtitle
       caption = "Plot by @WillBall12 made by code adapted from @traffordDataLab. Contains OS data © Crown copyright and database right (2020)") +  # add caption
  theme(panel.background = element_blank(),                                       # remove background gridlines
        line = element_blank(),                                                    # remove axis lines
        axis.text = element_blank(),                                               # remove tick labels
        axis.title = element_blank()) +                                              # remove axis labels
  coord_sf(datum = NA)                                                              # remove gridlines

