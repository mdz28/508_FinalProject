

###DONT FORGET CELINE's COLOR PALLETTE! 


setwd("C:\\Users\\Marlana\\Desktop\\Fall Semester\\MUSA 508\\FinalProject")

library(RSocrata)
library(dplyr)
library(tidyverse)
library(sf)
library(tidycensus)
library(ggplot2)
library(mapview)
library(tmap)
library(viridis)
library(spatstat)
library(raster)
library(spdep)
library(FNN)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(tidycensus)
library(readxl)
library(osmdata)
library(tigris)
library(plotly)
library(ggmap)


root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")
 
crossValidate1 <- function(dataset, id, dependentVariable, indVariables) {
  
  allPredictions <- data.frame()
  cvID_list <- unique(dataset$cvID)
  
  for (i in cvID_list) {
    
    thisFold <- i
    cat("This hold out fold is", thisFold, "\n")
    
    fold.train <- filter(dataset, cvID != thisFold) %>% as.data.frame() %>% 
      dplyr::select(cvID, geometry, indVariables, dependentVariable)
    fold.test  <- filter(dataset, cvID == thisFold) %>% as.data.frame() %>% 
      dplyr::select(cvID, geometry, indVariables, dependentVariable)
    
    regression <-
      glm(countOverdose2017 ~ ., family = "poisson", 
          data = fold.train %>% 
            dplyr::select(-geometry, -cvID))
    
    thisPrediction <- 
      mutate(fold.test, Prediction = predict(regression, fold.test, type = "response"))
    
    allPredictions <-
      rbind(allPredictions, thisPrediction)
    
  }
  return(st_sf(allPredictions))
}


crossValidate2 <- function(dataset, id, dependentVariable, indVariables) {
  
  allPredictions <- data.frame()
  districts_list <- unique(dataset$districts)
  
  for (i in districts_list) {
    
    thisFold <- i
    cat("This hold out fold is", thisFold, "\n")
    
    fold.train <- filter(dataset, districts != thisFold) %>% as.data.frame() %>% 
      dplyr::select(districts, geometry, indVariables, dependentVariable)
    fold.test  <- filter(dataset, districts == thisFold) %>% as.data.frame() %>% 
      dplyr::select(districts, geometry, indVariables, dependentVariable)
    
    regression <-
      glm(countOverdose2017 ~ ., family = "poisson", 
          data = fold.train %>% 
            dplyr::select(-geometry, -districts))
    
    thisPrediction <- 
      mutate(fold.test, Prediction = predict(regression, fold.test, type = "response"))
    
    allPredictions <-
      rbind(allPredictions, thisPrediction)
    
  }
  return(st_sf(allPredictions))
}


### ---- Geographic Data ----

## Mesa Boundaries 
census_api_key("e0f5bf7187090237f6ebb3e0289be4cc832b2112", overwrite = TRUE)
v18 <- load_variables(2018, "acs5", cache = TRUE)

MesaCityBoundary <- st_read("City Boundary.geojson") %>%
  st_transform('EPSG:26912')

MesaZipCodes <- st_read("Arizona Postal Code Boundaries.geojson")

MesaZipCodes <- MesaZipCodes %>% 
  st_transform('EPSG:26912')

CouncilDistricts <- st_read("Council District.geojson") %>% 
  st_transform('EPSG:26912')

MesaTracts <- 
  get_acs(geography = "tract", variables = c("B01003_001E", # Total population
                                             "B01001_011E", # Male 25-29
                                             "B01001_012E", # Male 30-34
                                             "B01001_013E", # Male 35-39
                                             "B01001_014E"), # Male 40-44
          year=2018, state=04, county=013, geometry=T) %>% 
  st_transform('EPSG:26912')

MesaTracts <- 
  MesaTracts %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(TotalPop = B01003_001, 
         Male25_29 = B01001_011, 
         Male30_34 = B01001_012,
         Male35_39 = B01001_013, 
         Male40_44 = B01001_014) 

MesaTracts <- MesaTracts %>%
  mutate(male25_44 = (Male25_29 + Male30_34 + Male35_39 + Male40_44),
         pctMale25_44 = ((male25_44)/(TotalPop) * 100))


## CDC Places Data --> is insurance here???? 

CDC.Places2017 <- read.socrata("https://chronicdata.cdc.gov/resource/kucs-wizg.json") %>%
  dplyr::select(placename, placefips, tractfips, access2_crudeprev, mhlth_crudeprev, sleep_crudeprev, teethlost_crudeprev) %>% 
  dplyr::filter(placefips == "0446000")

## Joing CDC and tracts data 

MesaTracts <- MesaTracts %>%
  left_join(CDC.Places2017, by = c("GEOID" = "tractfips"))

DemographicCentroids <- st_centroid(MesaTracts)

## Lack insurance

Insurance <- DemographicCentroids %>%
  dplyr::select(access2_crudeprev, geometry) %>%
  dplyr::filter(as.numeric(access2_crudeprev) > 15.0)

Insurance <- Insurance[!(is.na(Insurance$access2_crudeprev)), ]

Insurance.sf <- Insurance %>%
  dplyr::select(geometry) %>%
  dplyr::mutate(legend = "Lack_Insurance")

mapview(MesaCityBoundary) + mapview(Insurance.sf)

## High pct of teeth lacking 

TeethLost <- DemographicCentroids %>%
  dplyr::select(teethlost_crudeprev, geometry) %>%
  dplyr::filter(as.numeric(teethlost_crudeprev) > 10.0)

TeethLost <- TeethLost[!(is.na(TeethLost$teethlost_crudeprev)), ]

TeethLost.sf <- TeethLost %>%
  dplyr::select(geometry) %>%
  dplyr::mutate(legend = "Teeth_Lost")

mapview(MesaCityBoundary) + mapview(TeethLost.sf)

## Poor mental health

MentalHealth <- DemographicCentroids %>%
  dplyr::select(mhlth_crudeprev, geometry) %>%
  dplyr::filter(as.numeric(mhlth_crudeprev) > 11.0)

MentalHealth <- MentalHealth[!(is.na(MentalHealth$mhlth_crudeprev)), ]

MentalHealth.sf <- MentalHealth %>%
  dplyr::select(geometry) %>%
  dplyr::mutate(legend = "Mental_Health")

mapview(MesaCityBoundary) + mapview(MentalHealth.sf)


## Make Fishnet 

fishnet <- MesaCityBoundary %>%
  st_make_grid(cellsize = 500) %>% # try 152.4 bc this is in meters  
  st_intersection(MesaCityBoundary) %>%
  # st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(id = row_number()) %>% 
  st_transform(4326) %>%
  st_transform("EPSG:26912")

# st_crs(fishnet) <- 26912


# fishnet1 <- 
#   st_make_grid(MesaCityBoundary,
#                cellsize = 1320, 
#                square = TRUE) %>%
#   .[MesaCityBoundary] %>%           
#   st_sf() %>%
#   mutate(uniqueID = rownames(.))

### ---- Dependent Variable ----

## Overdose Locations 

MesaOverdoses <- read.socrata("https://data.mesaaz.gov/resource/qufy-tzv6.json")

# ggplot(data = MesaOverdoses, aes(x = time_of_day)) +
#   geom_bar()

MesaOverdoses <- MesaOverdoses[!(is.na(MesaOverdoses$latitude)), ]

MesaOverdoses <- st_as_sf(x = MesaOverdoses,                         
            coords = c("longitude", "latitude"),
            crs = "EPSG:4326", agr = "constant", remove = FALSE)

MesaOverdoses <- MesaOverdoses %>% 
  st_transform('EPSG:26912')

MesaOverdoses2017 <- MesaOverdoses %>%
  dplyr::filter(year == "2017")




### ---- Predictors ----



# CDC.Places2018 <- st_read("500 Cities_ Census Tract-level Data (GIS Friendly Format), 2018 release.geojson") %>%
#   dplyr::filter(placefips == "446000")
# 
# CDC.Places2021 <- st_read("PLACES_ Census Tract Data (GIS Friendly Format), 2021 release.geojson")%>%
#   dplyr::filter(statedesc == "Arizona" & countyname == "Maricopa")


## Unsheltered People 

unshelteredPeople <- read.socrata("https://data.mesaaz.gov/resource/jagk-fkkw.json") %>%
  dplyr::filter(city == "Mesa")

unshelteredPeople2017 <- read.socrata("https://data.mesaaz.gov/resource/jagk-fkkw.json") %>%
  dplyr::filter(city == "Mesa" & reporting_year == "2017")

unshelteredPeople2017.sf <- st_as_sf(x = unshelteredPeople2017,                         
                          coords = c("longitude", "latitude"),
                          crs = 4326) %>% 
  st_transform('EPSG:26912') %>%
  dplyr::select(geometry) %>%
  mutate(legend = "Unsheltered_People")




unshelteredPeople2018 <- read.socrata("https://data.mesaaz.gov/resource/jagk-fkkw.json") %>%
  dplyr::filter(city == "Mesa" & reporting_year == "2018")

unshelteredPeople2018 <- unshelteredPeople2018[!(is.na(unshelteredPeople2018$latitude)), ]

unshelteredPeople2018.sf <- st_as_sf(x = unshelteredPeople2018,                         
                                     coords = c("longitude", "latitude"),
                                     crs = 4326) %>% 
  st_transform('EPSG:26912')


## Code Violations

codeViolations <- read.socrata("https://data.mesaaz.gov/resource/ears-rpf9.json") %>%
  dplyr::filter(ordinance_in_violation == "8-6-3(Z)" | ordinance_in_violation == "8-6-3(A)" |
                  ordinance_in_violation == "8-6-3(B)" | ordinance_in_violation == "8-6-3(C)" |
                  ordinance_in_violation == "8-6-3(D)")

codeViolations.sf <- st_as_sf(x = codeViolations,                         
                                     coords = c("longitude", "latitude"),
                                     crs = 4326)

codeViolations2017.sf <- codeViolations.sf %>%
  dplyr::filter(year_opened == "2017") %>% 
  st_transform('EPSG:26912')%>% 
  dplyr::select(geometry) %>%
  mutate(legend = "Code_Violations")

codeViolations2018.sf <- codeViolations.sf %>%
  dplyr::filter(year_opened == "2018") %>% 
  st_transform('EPSG:26912')


## Street Light Fixtures 

streetLights <- st_read("Streetlight Fixtures.geojson") %>% 
  dplyr::filter(fixture_usage == "Pedestrian" | fixture_usage == "Novelty") %>% 
  st_transform('EPSG:26912') %>%
  dplyr::select(geometry) %>%
  mutate(legend = "Street_Lights")

## Crime Incidents
## FILTER FOR DRUG CRIMES 

policeIncidents <- read.socrata("https://data.mesaaz.gov/resource/39rt-2rfj.json")

policeIncidents <- policeIncidents[!(is.na(policeIncidents$latitude)), ]

policeIncidents2017 <- policeIncidents %>%
  dplyr::filter(report_year == "2017")

policeIncidents2017 <- policeIncidents2017 %>%
  dplyr::filter(national_incident_based_crime_reporting_description == "DRUG EQUIPMENT VIOLATION" | 
                  national_incident_based_crime_reporting_description == "DRUG/NARCOTIC VIOLATION" &
                  crime_type != "MARIJUANA-POSSESS-USE") 


policeInxidents2017.sf <- st_as_sf(x = policeIncidents2017,                         
                                     coords = c("longitude", "latitude"),
                                     crs = 4326) %>% 
                          st_transform('EPSG:26912') %>%
                          dplyr::select(geometry) %>%
                          mutate(legend = "Police_Incidents")

# policeIncidents2018 <- policeIncidents %>%
#   dplyr::filter(report_year == "2018")
# 
# policeInxidents2018.sf <- st_as_sf(x = policeIncidents2018,                         
#                                    coords = c("longitude", "latitude"),
#                                    crs = 4326) %>% 
#   st_transform('EPSG:26912')

## Properties / Vacant Properties 

cityProperty <- read.socrata("https://data.mesaaz.gov/resource/xms2-ya86.json")

cityProperty <- cityProperty[!(is.na(cityProperty$latitude)), ]

cityProperty.sf <- st_as_sf(x = cityProperty,                         
                                   coords = c("longitude", "latitude"),
                                   crs = 4326) %>% 
                    st_transform('EPSG:26912')%>% 
  dplyr::filter(property_use == "Vacant" | property_use == "Vacant (ADOT Remnant)") %>%
  dplyr::select(geometry) %>%
  mutate(legend = "Vacant_Properties")

## Subsidized Housing 

subsidizedHousing = read_excel("NHPD Properties Only Mesa.xlsx")
subsidizedHousing = janitor::clean_names(subsidizedHousing)


subsidizedHousing <- subsidizedHousing[!(is.na(subsidizedHousing$latitude)), ]

subsidizedHousing.sf <- st_as_sf(x = subsidizedHousing,                         
                                 coords = c("longitude", "latitude"),
                                 crs = 4326) %>% 
                        st_transform('EPSG:26912') %>%
                        dplyr::select(geometry) %>%
                        mutate(legend = "Subsidized_Housing")
mapview(MesaCityBoundary) + mapview(subsidizedHousing.sf) + mapview(MesaOverdoses2017, color = "red")

## OSM Data

q0 <- opq(bbox = c(-111.88,33.41,-111.57,33.40)) 

# 33.410898294665536, -111.88814214730442
# 
# 33.40631279270059, -111.57983832899043

# clinics

clinic <- add_osm_feature(opq = q0, key = 'amenity', value = "clinic") %>%
  osmdata_sf(.)

clinic.sf <- st_geometry(clinic$osm_points) %>%
  st_transform("EPSG:26912") %>%
  st_sf() %>%
  cbind(., clinic$osm_points$amenity) %>%
  rename(NAME = clinic.osm_points.amenity)%>%
  dplyr::select(geometry) %>%
  mutate(legend = "Clinics")

mapview(MesaCityBoundary) + mapview(clinic.sf)

# bars

bars <- add_osm_feature(opq = q0, key = 'amenity', value = "bar") %>%
  osmdata_sf(.)

bars.sf <- st_geometry(bars$osm_points) %>%
  st_transform("EPSG:26912") %>%
  st_sf() %>%
  cbind(., bars$osm_points$amenity) %>%
  rename(NAME = bars.osm_points.amenity) %>%
  dplyr::select(geometry) %>%
  mutate(legend = "Bars")

mapview(MesaCityBoundary) + mapview(bars.sf)

# benches

bench <- add_osm_feature(opq = q0, key = 'amenity', value = "bench") %>%
  osmdata_sf(.)

bench.sf <- st_geometry(bench$osm_points) %>%
  st_transform("EPSG:26912") %>%
  st_sf() %>%
  cbind(., bench$osm_points$amenity) %>%
  rename(NAME = bench.osm_points.amenity) 

mapview(MesaCityBoundary) + mapview(bench.sf)


# Parks

park <- add_osm_feature(opq = q0, key = 'leisure', value = "park") %>%
  osmdata_sf(.)

park.sf <- st_geometry(park$osm_polygons) %>%
  st_transform("EPSG:26912") %>%
  st_sf() %>%
  cbind(., park$osm_polygons$name) %>%
  rename(NAME = park.osm_polygons.name)

park.sf <- st_centroid(park.sf) %>%
  dplyr::select(geometry) %>%
  mutate(legend = "Parks")

mapview(MesaCityBoundary) + mapview(park.sf)

### ---- Joining 2017 overdoses to fishnet  

overdoses2017_net <- MesaOverdoses2017 %>% 
  mutate(countOverdose2017 = 1) %>% 
  dplyr::select(countOverdose2017) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countOverdose2017 = replace_na(countOverdose2017, 0),
         id = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE))

ggplot() +
  geom_sf(data = overdoses2017_net, aes(fill=countOverdose2017), colour=NA) +
  scale_fill_viridis_c(option = "C") +
  labs(title= "2017 Overdoses by Fishnet") +
  mapTheme()

#mutate(id = row_number()) %>% 
  

# Alternative fishnet join just for future reference 
# overdoses2017_net$temp_count <- lengths(st_intersects(fishnet, MesaOverdoses2017))

mapview(fishnet) + mapview(MesaOverdoses2017)
mapview(overdoses2017_net, zcol = "countOverdose2017")


### --- 2017 Risk Factors by Fishnet 

# Get help with CDC data 


vars_net <- 
  rbind(codeViolations2017.sf, unshelteredPeople2017.sf, streetLights, 
        policeInxidents2017.sf, MentalHealth.sf, Insurance.sf) %>%
  st_join(., fishnet, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(id, legend) %>%
  summarize(count = n()) %>%
  full_join(fishnet) %>%
  spread(legend, count, fill=0) %>%
  st_sf() %>%
  dplyr::select(-`<NA>`) %>%
  na.omit() %>%
  ungroup()

vars_net.long <- 
  gather(vars_net, Variable, value, -geometry, -id)

vars <- unique(vars_net.long$Variable)
mapList <- list()

for(i in vars){
  mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(vars_net.long, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis_c(option = "C") +
    labs(title=i) +
    mapTheme()}

do.call(grid.arrange,c(mapList, ncol=3, top="Risk Factors by Fishnet"))


## -- Engineering Nearest Neighbor

# Error:  Error: Problem with `mutate()` column `Code_Violations.nn`.
#`Code_Violations.nn = nn_function(st_c(st_coid(vars_net)), st_c(Code_Violations), 3)`.
# x error in evaluating the argument 'x' in selecting a method for function 'as.matrix': 
# no applicable method for 'st_coordinates' applied to an object of class "c('double', 'numeric')"

##RESOLVED - issue I was using column names instead of df names 

# Also get help understanding the purpose of this besides smoothing

st_c <- st_coordinates
st_coid <- st_centroid

vars_net <-
  vars_net %>%
  dplyr::mutate(
    Code_Violations.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(codeViolations2017.sf),3),
    Police_Incidents.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(policeInxidents2017.sf),3),
    Street_Lights.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(streetLights),3),
    Unsheltered_People.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(unshelteredPeople2017.sf),3),
   MentalHealth.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(MentalHealth.sf),3),
    Insurance.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(Insurance.sf),3))



vars_net.long.nn <- 
  dplyr::select(vars_net, ends_with(".nn")) %>%
  gather(Variable, value, -geometry)

vars <- unique(vars_net.long.nn$Variable)

mapList <- list()
for(i in vars){
  mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(vars_net.long.nn, Variable == i), 
            aes(fill=value),
            colour=NA) +
    scale_fill_viridis_c(option = "C") +
    labs(title=i) +
    mapTheme()}

do.call(grid.arrange,c(mapList, ncol = 3, top = "Nearest Neighbor Risk Factors by Fishnet"))



## --- Final Net

# Doing the operations below because was initially getting the following error
#Error: Can't join on `x$id` x `y$id` because of incompatible types.
#`x$id` is of type <character>>.
#`y$id` is of type <integer>>.
# RESOLVED with as.character 

fishnet$id <- as.character(fishnet$id)
vars_net$id <- as.character(vars_net$id)

final_net <-
  left_join(overdoses2017_net, st_drop_geometry(vars_net), by="id") 

final_net <-
  st_centroid(final_net) %>%
  st_join(dplyr::select(CouncilDistricts, districts)) %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(final_net, geometry, id)) %>%
  st_sf() %>%
  na.omit()

## Having trouble finding boundaries other than census tracts for Mesa
## Should we skip that part of the code? How to use medical service areas? https://www.mesaaz.gov/Home/ShowDocument?id=21281

final_net.nb <- poly2nb(as_Spatial(final_net), queen=TRUE)
final_net.weights <- nb2listw(final_net.nb, style="W", zero.policy=TRUE)

##EVERYTHING HERE AND BELOW NEEDS TO BE RE-RUN ONCE ERROR IS RESOLVED
## Getting following error for code below
## Error in h(simpleError(msg, call)) : 
## error in evaluating the argument 'x' in selecting a method for function 'as.data.frame': 
## lengths of 'breaks' and 'labels' differ
# RESOLVED: needed to add zero.policy = TRUE after final_net.weights and change P_Value column name

final_net.localMorans <- 
  cbind(
    as.data.frame(localmoran(final_net$countOverdose2017, final_net.weights, zero.policy = TRUE)),
    as.data.frame(final_net)) %>% 
  st_sf() %>%
  dplyr::select(Overdose_Count = countOverdose2017, 
                Local_Morans_I = Ii, 
                P_Value = `Pr(z != E(Ii))`) %>%
  mutate(Significant_Hotspots = ifelse(P_Value <= 0.05, 1, 0)) %>%
  gather(Variable, Value, -geometry)

vars <- unique(final_net.localMorans$Variable)
varList <- list()

for(i in vars){
  varList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(final_net.localMorans, Variable == i), 
            aes(fill = Value), colour=NA) +
    scale_fill_viridis_c(option = "C") +
    labs(title=i) +
    mapTheme() + theme(legend.position="bottom")}

do.call(grid.arrange,c(varList, ncol = 4, top = "Local Morans I statistics, Overdoses"))


# 
# ggplot() +
#   geom_sf(data = MesaCityBoundary) +
#   geom_sf(data = filter(final_net.localMorans, Variable == "Significant_Hotspots"), 
#           aes(fill = Value), colour =NA) +
#   scale_fill_gradient(low = "#f5ec61", high= "2c70bb") +
#   labs(title= "") +
#   mapTheme() + 
#   theme(legend.position="bottom")
# 
# MesaPalette <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")
# 
# ggplot() +
#   geom_sf(data = MesaCityBoundary, fill = NA, colour="grey40") +
#   geom_sf(data = filter(final_net.localMorans, Variable == "Significant_Hotspots"), 
#           aes(fill = Value), colour =NA)+
#   scale_colour_manual(values = MesaPalette) +
#   labs(title="") +
#   mapTheme()+ 
#   theme(legend.position="bottom")



final_net <-
  final_net %>% 
  mutate(overdose.isSig = 
           ifelse(localmoran(final_net$countOverdose2017, 
                             final_net.weights, zero.policy = TRUE)[,5] <= 0.0000001, 1, 0)) %>%
  mutate(overdose.isSig.dist = 
           nn_function(st_coordinates(st_centroid(final_net)),
                       st_coordinates(st_centroid(
                         filter(final_net, overdose.isSig == 1))), 1))


### --- Correlation Plots 

## Make table of variables and correlation

correlation.long <-
  st_drop_geometry(final_net) %>%
  dplyr::select(-id, -cvID, -districts) %>%
  gather(Variable, Value, -countOverdose2017)

correlation.cor <-
  correlation.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, countOverdose2017, use = "complete.obs"))

ggplot(correlation.long, aes(Value, countOverdose2017)) +
  geom_point(size = 0.1) +
  geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~Variable, ncol = 2, scales = "free") +
  labs(title = "Overdose count as a Function of risk factors") +
  plotTheme()

# # Code Violation Correlation
# 
# cor_plot <- correlation.long %>%
#   dplyr::filter(Variable == "Code_Violations") %>%
#   ggplot(correlation.long, mapping=aes(x= Value, y= countOverdose2017)) +
#   geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
#             x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
#   geom_smooth(method = "lm", se = FALSE, colour = "#B32034") +
#   geom_point(colour = "#38A19A")  +
#   labs(title = "Overdose Count as a Function of Unsafe/Unsantitary Living Conditions", 
#        x = "Living Condition Complaint Count", y = "OVerdose Count") +
#   plotTheme()
# 
# cor_plot <- ggplotly(cor_plot)
# 
# # Drug Crime Correlation
# 
# cor_plot2 <- correlation.long %>%
#   dplyr::filter(Variable == "Police_Incidents") %>%
#   ggplot(correlation.long, mapping=aes(x= Value, y= countOverdose2017)) +
#   geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
#             x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
#   geom_smooth(method = "lm", se = FALSE, colour = "#B32034") +
#   geom_point(colour = "#38A19A")  +
#   labs(title = "Overdose Count as a Function of Drug Crimes", 
#        x = "Drug Crime Count", y = "OVerdose Count") +
#   plotTheme()
# 
# cor_plot2 <- ggplotly(cor_plot2)
# 
# # Street Lights Correlation
# 
# cor_plot3 <- correlation.long %>%
#   dplyr::filter(Variable == "Street_Lights") %>%
#   ggplot(correlation.long, mapping=aes(x= Value, y= countOverdose2017)) +
#   geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
#             x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
#   geom_smooth(method = "lm", se = FALSE, colour = "#B32034") +
#   geom_point(colour = "#38A19A")  +
#   labs(title = "Overdose Count as a Function of Street Lights", 
#        x = "Street Lights Count", y = "OVerdose Count") +
#   plotTheme()
# 
# cor_plot3 <- ggplotly(cor_plot3)
# 
# # Subsidized Housing Correlation
# 
# cor_plot4 <- correlation.long %>%
#   dplyr::filter(Variable == "Subsidized_Housing") %>%
#   ggplot(correlation.long, mapping=aes(x= Value, y= countOverdose2017)) +
#   geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
#             x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
#   geom_smooth(method = "lm", se = FALSE, colour = "#B32034") +
#   geom_point(colour = "#38A19A")  +
#   labs(title = "Overdose Count as a Function of Subsidized Housing", 
#        x = "Subsidized Housing Count", y = "OVerdose Count") +
#   plotTheme()
# 
# cor_plot4 <- ggplotly(cor_plot4)
# 
# # Unsheltered People Correlation
# 
# cor_plot5 <- correlation.long %>%
#   dplyr::filter(Variable == "Unsheltered_People") %>%
#   ggplot(correlation.long, mapping=aes(x= Value, y= countOverdose2017)) +
#   geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
#             x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
#   geom_smooth(method = "lm", se = FALSE, colour = "#B32034") +
#   geom_point(colour = "#38A19A")  +
#   labs(title = "Overdose Count as a Function of Unsheltered People", 
#        x = "Unsheltered People Count", y = "OVerdose Count") +
#   plotTheme()
# 
# cor_plot5 <- ggplotly(cor_plot5)
# 
# # Vacant Properties Correlation
# 
# cor_plot6 <- correlation.long %>%
#   dplyr::filter(Variable == "Vacant_Properties") %>%
#   ggplot(correlation.long, mapping=aes(x= Value, y= countOverdose2017)) +
#   geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
#             x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
#   geom_smooth(method = "lm", se = FALSE, colour = "#B32034") +
#   geom_point(colour = "#38A19A")  +
#   labs(title = "Overdose Count as a Function of Vacant Properties", 
#        x = "Vacant Properties Count", y = "OVerdose Count") +
#   plotTheme()
# 
# cor_plot6 <- ggplotly(cor_plot6)
# 
# 
# subplot(cor_plot, cor_plot2, cor_plot3, cor_plot4, cor_plot5, cor_plot5, cor_plot6)


### --- Cross Validation  

reg.vars <- c("Code_Violations.nn", "Police_Incidents.nn", "Street_Lights.nn", 
              "Insurance.nn", "MentalHealth.nn", "Unsheltered_People.nn")

reg.ss.vars <- c("Code_Violations.nn", "Police_Incidents.nn", "Street_Lights.nn", 
                 "Insurance.nn", "MentalHealth.nn", "Unsheltered_People.nn", 
                 "overdose.isSig", "overdose.isSig.dist")

reg.cv <- crossValidate1(
  dataset = final_net,
  id = "cvID",
  dependentVariable = "countOverdose2017",
  indVariables = reg.vars) %>%
  dplyr::select(cvID = cvID, countOverdose2017, Prediction, geometry)

reg.ss.cv <- crossValidate1(
  dataset = final_net,
  id = "cvID",
  dependentVariable = "countOverdose2017",
  indVariables = reg.ss.vars) %>%
  dplyr::select(cvID = cvID, countOverdose2017, Prediction, geometry)

reg.spatialCV <- crossValidate2(
  dataset = final_net,
  id = "districts",
  dependentVariable = "countOverdose2017",
  indVariables = reg.vars) %>%
  dplyr::select(cvID = districts, countOverdose2017, Prediction, geometry)

reg.ss.spatialCV <- crossValidate2(
  dataset = final_net,
  id = "districts",
  dependentVariable = "countOverdose2017",
  indVariables = reg.ss.vars) %>%
  dplyr::select(cvID = districts, countOverdose2017, Prediction, geometry)


## Accuracy & Generalizability 

reg.summary <- 
  rbind(
    mutate(reg.cv,           Error = Prediction - countOverdose2017,
           Regression = "Random k-fold CV: Just Risk Factors"),
    mutate(reg.ss.cv,        Error = Prediction - countOverdose2017,
           Regression = "Random k-fold CV: Spatial Process"),
     mutate(reg.spatialCV,    Error = Prediction - countOverdose2017,
           Regression = "Spatial LOGO-CV: Just Risk Factors"),
    mutate(reg.ss.spatialCV, Error = Prediction - countOverdose2017,
           Regression = "Spatial LOGO-CV: Spatial Process")) %>%
  st_sf() 


error_by_reg_and_fold <- 
  reg.summary %>%
  group_by(Regression, cvID) %>% 
  summarize(Mean_Error = mean(Prediction - countOverdose2017, na.rm = T),
            MAE = mean(abs(Mean_Error), na.rm = T),
            SD_MAE = mean(abs(Mean_Error), na.rm = T)) %>%
  ungroup()

error_by_reg_and_fold %>%
  ggplot(aes(MAE)) + 
  geom_histogram(bins = 30, colour="black", fill = "#FDE725FF") +
  facet_wrap(~Regression) +  
  geom_vline(xintercept = 0) + scale_x_continuous(breaks = seq(0, 8, by = 1)) + 
  labs(title="Distribution of MAE", subtitle = "k-fold cross validation vs. LOGO-CV",
       x="Mean Absolute Error", y="Count") +
  plotTheme()


st_drop_geometry(error_by_reg_and_fold) %>%
  group_by(Regression) %>% 
  summarize(Mean_MAE = round(mean(MAE), 2),
            SD_MAE = round(sd(MAE), 2)) %>%
  kable() %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(2, color = "black", background = "#FDE725FF") %>%
  row_spec(4, color = "black", background = "#FDE725FF") 


error_by_reg_and_fold %>%
  filter(str_detect(Regression, "LOGO")) %>%
  ggplot() +
  geom_sf(aes(fill = MAE)) +
  facet_wrap(~Regression) +
  scale_fill_viridis() +
  labs(title = "Overdose errors by LOGO-CV Regression") +
  mapTheme() + theme(legend.position="bottom")


neighborhood.weights <-
  filter(error_by_reg_and_fold, Regression == "Spatial LOGO-CV: Spatial Process") %>%
  group_by(cvID) %>%
  poly2nb(as_Spatial(.), queen=TRUE) %>%
  nb2listw(., style="W", zero.policy=TRUE)

filter(error_by_reg_and_fold, str_detect(Regression, "LOGO"))  %>% 
  st_drop_geometry() %>%
  group_by(Regression) %>%
  summarize(Morans_I = moran.mc(abs(Mean_Error), neighborhood.weights, 
                                nsim = 99, zero.policy = TRUE, 
                                na.action=na.omit)[[1]],
            p_value = moran.mc(abs(Mean_Error), neighborhood.weights, 
                               nsim = 99, zero.policy = TRUE, 
                               na.action=na.omit)[[3]])

## Comparing to Kernel Density 

overdose_ppp <- as.ppp(st_coordinates(MesaOverdoses2017), W = st_bbox(final_net))
overdose_KD <- density.ppp(overdose_ppp, 1000)

as.data.frame(overdose_KD) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(final_net)) %>%
  aggregate(., final_net, mean) %>%
  ggplot() +
  geom_sf(aes(fill=value)) +
  geom_sf(data = sample_n(MesaOverdoses2017, 1500, replace=TRUE), size = .5) +
  scale_fill_viridis(name = "Density") +
  labs(title = "Kernel density of 2017 overdoses") +
  mapTheme()

MesaOverdoses2018 <- MesaOverdoses %>%
  dplyr::filter(year == "2018") %>%
  .[fishnet,]


overdose_KDE_sf <- as.data.frame(overdose_KD) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(final_net)) %>%
  aggregate(., final_net, mean) %>%
  mutate(label = "Kernel Density",
         Risk_Category = ntile(value, 100),
         Risk_Category = case_when(
           Risk_Category >= 90 ~ "90% to 100%",
           Risk_Category >= 70 & Risk_Category <= 89 ~ "70% to 89%",
           Risk_Category >= 50 & Risk_Category <= 69 ~ "50% to 69%",
           Risk_Category >= 30 & Risk_Category <= 49 ~ "30% to 49%",
           Risk_Category >= 1 & Risk_Category <= 29 ~ "1% to 29%")) %>%
  cbind(
    aggregate(
      dplyr::select(MesaOverdoses2018) %>% mutate(overdoseCount2018 = 1), ., sum) %>%
      mutate(overdoseCount2018 = replace_na(overdoseCount2018, 0))) %>%
  dplyr::select(label, Risk_Category, overdoseCount2018)


overdose_risk_sf <-
  filter(reg.summary, Regression == "Spatial LOGO-CV: Spatial Process") %>%
  mutate(label = "Risk Predictions",
         Risk_Category = ntile(Prediction, 100),
         Risk_Category = case_when(
           Risk_Category >= 90 ~ "90% to 100%",
           Risk_Category >= 70 & Risk_Category <= 89 ~ "70% to 89%",
           Risk_Category >= 50 & Risk_Category <= 69 ~ "50% to 69%",
           Risk_Category >= 30 & Risk_Category <= 49 ~ "30% to 49%",
           Risk_Category >= 1 & Risk_Category <= 29 ~ "1% to 29%")) %>%
  cbind(
    aggregate(
      dplyr::select(MesaOverdoses2018) %>% mutate(overdoseCount2018 = 1), ., sum) %>%
      mutate(overdoseCount2018 = replace_na(overdoseCount2018, 0))) %>%
  dplyr::select(label,Risk_Category, overdoseCount2018)


rbind(overdose_KDE_sf, overdose_risk_sf) %>%
  na.omit() %>%
  gather(Variable, Value, -label, -Risk_Category, -geometry) %>%
  ggplot() +
  geom_sf(aes(fill = Risk_Category), colour = NA) +
  geom_sf(data = sample_n(MesaOverdoses2018, 3000, replace=TRUE), size = .5, colour = "black") +
  facet_wrap(~label, ) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Comparison of Overdose Kernel Density and Risk Predictions",
       subtitle="2018 overdoses compared to 2017 risk predictions") +
  mapTheme()



rbind(overdose_KDE_sf, overdose_risk_sf) %>%
  st_set_geometry(NULL) %>% na.omit() %>%
  gather(Variable, Value, -label, -Risk_Category) %>%
  group_by(label, Risk_Category) %>%
  summarize(MesaOverdoses2018 = sum(Value)) %>%
  ungroup() %>%
  group_by(label) %>%
  mutate(Rate_of_test_set_crimes = MesaOverdoses2018 / sum(MesaOverdoses2018)) %>%
  ggplot(aes(Risk_Category,Rate_of_test_set_crimes)) +
  geom_bar(aes(fill=label), position="dodge", stat="identity") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Risk prediction vs. Kernel density, 2018 overdoses") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


## Site Suitability 


sample_site <- data.frame(lat= c(-111.83920989923757), lon= c(33.41080887865899)) %>%
  st_as_sf(coords = c("lat", "lon"), crs=4326) %>%
  st_transform("EPSG:26912")

sample_buffer1 <- st_buffer(sample_site, 500)
sample_buffer2 <- st_buffer(sample_site, 1000)
sample_buffer3 <- st_buffer(sample_site, 1500)

risk_centroid1 <- overdose_risk_sf %>% 
  st_centroid() %>%
  dplyr::select(overdoseCount2018) %>%
  aggregate(., sample_buffer1, sum)

risk_centroid2 <- overdose_risk_sf %>% 
  st_centroid() %>%
  dplyr::select(overdoseCount2018) %>%
  aggregate(., sample_buffer2, sum)

risk_centroid3 <- overdose_risk_sf %>% 
  st_centroid() %>%
  dplyr::select(overdoseCount2018) %>%
  aggregate(., sample_buffer3, sum)

register_google(key = "AIzaSyAcot6-KJnKCwIQHJmFO5YDI3TYdXh2Y8I")

mesa_basemap <- get_map(location=c(lon = -111.88, lat = 33.41), zoom=11, maptype = 'terrain-background', source = 'stamen')


# ggplot() +
  
  
# ggmap(mesa_basemap) +
#   geom_sf(data = risk_centroid3, (aes(fill=NA, colour = "red"))) +
#   geom_sf(data = risk_centroid2, (aes(fill=NA, colour = "red"))) +
#   geom_sf(data = risk_centroid1, (aes(fill=NA, colour = "red"))) +
#   coord_sf(crs = st_crs(4326)) %>%

  
  tm_shape(risk_centroid3)+
    tm_fill(col = NA, alpha = 0) +
    tm_polygons("overdoseCount2018", col = NA, border.col = "red") +
    tm_fill(col = NA) +
  tm_shape(risk_centroid2)+
    tm_fill(col = NA, alpha = 0) +
    tm_polygons("overdoseCount2018", col = NA, border.col = "red") +
    tm_fill(col = NA) +
  tm_shape(risk_centroid1)+
    tm_fill(col = NA, alpha = 0) +
    tm_polygons("overdoseCount2018", col = NA, border.col = "red") +
    tm_fill(col = NA)
    
tmap_mode("view")
tmap_last()

## change the location of this point and think about buffer size
## remember to interpret as predicted for 2018 

read.csv("CBA.csv") %>%
  kable() %>%
  kable_styling()
