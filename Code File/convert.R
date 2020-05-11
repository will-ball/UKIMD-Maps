## ESRI files to geojson

# Here is the basic flow.
# 1. Obtain Shapefile — Geospatial (Map) Data
# 2. Import Shapefile to R
# 3. Manipulate Attributes Data in Spatial Data Frame
# 4. Convert to GeoJSON
# 5. Manipulate Geometry Data in GeoJSON — Simplify and Clip / Erase
# 6. Export as GeoJSON file

# Install and Load Packages
install.packages("rgdal")
install.packages("spdplyr")
install.packages("geojsonio")
install.packages("rmapshaper")
install.packages("tidyverse")

library(rgdal)
library(spdplyr)
library(geojsonio)
library(rmapshaper)
library(tidyverse)

# Load in .shp file
dz2001 <- readOGR(dsn = "/Users/PGR3/Desktop/WB/Data/New folder/UKIMD Map/Shapefiles",
layer = "SG_DataZone_Bdry_2001", verbose = FALSE)

head(dz2001)

# Convert to JSON

dz2001_json <- geojson_json(dz2001)

# Simplify

dz2001_json_simplified <- ms_simplify(dz2001_json)

# Export

geojson_write(dz2001_json_simplified, file = "/Users/PGR3/Desktop/WB/Data/New folder/UKIMD Map/dz2001.geojson")

# Load in

sf_DZ <- st_read(dz2001_json_simplified, quiet = TRUE)
glimpse(sf_DZ)

st_geometry(sf_DZ)
plot(st_geometry(sf_DZ))

# Create lookup for dz2001 codes Health Board and Council Areas

scotcodes <- read_csv("https://www2.gov.scot/Resource/Doc/933/0112765.txt") %>%
  rename(area_code = ZONECODE)
lacodes <- read_csv("lacodescount.csv") # Local Council Names for codes
hbcodes <- read_csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/2db6cf68-2890-4bf8-9fae-92ca63c9b07f/download/hb2006_codes_and_labels_06042020.csv") %>%
  select(HB_CODE = HB, hb_name = HBName) # Health Board names for codes

scotcodes <- left_join(scotcodes, lacodes, by = "LA_CODE")
scotcodes <- left_join(scotcodes, hbcodes, by = "HB_CODE")  # Lookup from DZ codes to HB and LC by name

write_delim(scotcodes, path = "2001dzhblc.csv", delim = ",")

# Read in UKIMD

imd <- read_tsv("https://data.bris.ac.uk/datasets/1ef3q32gybk001v77c1ifmty7x/uk_imd_scores_data.txt")

# Select DZ codes within a Council area

lookup <- scotcodes %>%  
 filter(LA_NAME %in% c("Aberdeenshire")) %>%
 pull(area_code)

# Create object for chosen area

sf_abdns <- sf_DZ %>%
  filter(DZ_CODE %in% lookup)

plot(st_geometry(sf_abdns))

# Prepare for linkage to IMD

sf_abdns <- sf_abdns %>%
  rename(area_code = DZ_CODE)

sf_abdns_imd <- left_join(sf_abdns, imd, by = "area_code")
glimpse(sf_abdns_imd)

# Plot

ggplot() +                                                                          # initialise a ggplot object
  geom_sf(data = sf_abdns_imd,                                                      # add a simple features (sf) object
          aes(fill = uk_imd_england_quintile),                            # specify what variable to fill on
          alpha = 0.9,                                                              # add transparency to the fill
          colour = 'white',                                                         # make polygon boundaries white
          size = 0.05) +                                                             # adjust width of polygon boundaries
  scale_fill_brewer(palette = "YlOrRd",                                               # choose a http://colorbrewer2.org/ palette
                    name = "UK IMD Quintiles")# +                                    # add legend title
  labs(x = NULL, y = NULL,                                                          # drop axis titles
       title = "Deprivation Quintiles for Aberdeenshire by Data Zone (2001)",                # add title
       subtitle = "Sources: Abel et al (2016) BMJ and Office for National Statistics under Open
       Government License v3.0",                                                    # add subtitle
       caption = "Plot by @WillBall12 made by code adapted from @traffordDataLab.
       Contains OS data © Crown copyright and database right (2020)") +  # add caption
  theme(panel.background = element_blank(),                                       # remove background gridlines
        line = element_blank(),                                                    # remove axis lines
        axis.text = element_blank(),                                               # remove tick labels
        axis.title = element_blank()) +                                              # remove axis labels
  coord_sf(datum = NA)                                                              # remove gridlines