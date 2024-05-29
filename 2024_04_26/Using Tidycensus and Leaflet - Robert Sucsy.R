#GOALS: first half will show you how to pull census data with tidyverse. 
#Next, we'll go into basic spatial data manipulation.

#PACKAGES

#General use
library(tidyverse)
#census api interface
library(tidycensus)
#spatial data manipulation
library(sf)
#a nice visualization interface for ggplot
library(ggthemes)
#basemaps
library(ggspatial)


#CENSUS DATA MANIPULATION

#Load in your census API key
#census_api_key('', install = TRUE)
#NOTE - it seems that you're allowed limited use of the census API without a key

#loads ACS-5 variables
acs5_vars<-load_variables(2022,"acs5")
view(acs5_vars)

#Pulls census info


varlist<-c(total_population = "B01003_001", 
           male_under_5_population = "B01001_003",
           female_under_5_population = "B01001_027", 
           over_65_population = "B09021_022", 
           under_18_population = "B09018_001",
           nonenglish_speakers_at_home = "B99162_003", 
           high_school_graduate_over_age_25 = "B15003_017", 
           white = "B02001_002", 
           black = "B02001_003", 
           hispanic = "B03002_012",
           population_below_poverty_line = "B17021_002",
           median_household_income = "B19013_001",
           american_indian = "B02010_001")

#Pulls census info
ri_wide<-get_acs(geography = "tract",
                 variables = varlist,
                 state = "RI",
                 year = 2022,
                 geometry = TRUE,
                 output = "wide")

#basic plot
ggplot(ri_wide) + geom_sf() 

#Lets make a choropleth of population 
ggplot(ri_wide) + geom_sf(aes(fill = total_populationE)) + 
  scale_fill_viridis_c(option = "magma", direction = -1) +
  ggtitle("total Pop.")

#Just population is not great, lets calculate area & look at population density
ri_wide$area<-st_area(ri_wide)
ri_wide$popdens<-as.numeric(ri_wide$total_populationE/ri_wide$area)
plot1<-ggplot(ri_wide) + geom_sf(aes(fill = popdens)) + 
  scale_fill_viridis_c(option = "magma", direction = -1) +
  ggtitle("Pop. Dens.")
plot1

#Last, lets add on a basemap. We put it first so that it projects as the furthest down layer
plot1<-ggplot(ri_wide) + annotation_map_tile() + geom_sf(aes(fill = popdens)) + 
  scale_fill_viridis_c(option = "magma", direction = -1) +
  ggtitle("Pop. Dens.")
plot1

#Now, lets pull in some other data. Lets look at ESRI, a common source you'll probably encounter
#ESRI_URLs
#"https://www.rigis.org/datasets/79ccc9b901684a958ac7134199f82b9f_0/explore"
API_link<-("https://services2.arcgis.com/S8zZg9pg23JUEexQ/arcgis/rest/services/Zip_Code_Tabulation_Areas/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
RI_ZCTAs<-st_read(API_link)

#"https://www.arcgis.com/home/item.html?id=01ae5e15fa154ae6b0f92adfa48075be"
dunkin_url<-"https://services1.arcgis.com/oASeSX1dVztKCgUc/ArcGIS/rest/services/Dunkin'_Donuts/FeatureServer/0"
#brownfields_url<-"https://risegis.ri.gov/hosting/rest/services/RIDEM/Regulated_Facilties/MapServer/23"

#Doesn't work? - needs a query definition
st_read(dunkin_url)
dunkin_fixed<-paste(dunkin_url,"/query?outFields=*&where=1%3D1&f=geojson", sep = "")

#Now it works! Lets combine the point data with the choropleth map
dunkin_sf<-st_read(dunkin_fixed)
plot1 + geom_sf(data = dunkin_sf)

#OH NO! We lost RI, lets subset $ try again
dunkin_sf_subset<-dunkin_sf[dunkin_sf$State == "RI",]
plot1 + geom_sf(data = dunkin_sf_subset)

#points are overlapping - lets convert this to a density map to see it better
egads<-st_join(ri_wide,dunkin_sf_subset)
#We need to set the coordinate reference systems to be the same, then we intersect, group_by, and summarize
st_crs(dunkin_sf_subset)
ri_wide<-st_transform(ri_wide,crs = st_crs(dunkin_sf_subset))


joined_sf<-st_join(ri_wide,dunkin_sf_subset)
joined_and_summarized_sf<-joined_sf %>% group_by(GEOID) %>% summarise(count=n()) 

ggplot(joined_and_summarized_sf) + annotation_map_tile() + geom_sf(aes(fill = count)) + 
  scale_fill_viridis_c(option = "magma", direction = -1) +
  ggtitle("Dunk. count")


#leaflet - I don't use it that much but if you want interactive plots it's the way to go
library(leaflet)
mymap <-  (ri_wide) %>% leaflet() %>%
  addTiles() %>% 
  addPolygons(weight = 1,
              popup = paste0(ri_wide$total_populationE, " people live in ",ri_wide$NAME) )

mymap





