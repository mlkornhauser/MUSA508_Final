#########################
# MUSA 508 FINAL PROJECT
# Airbnb in Amsterdam
# Ejiro & Maddy
#########################

###############
# LOAD PACKAGES
###############
library(tidyverse)
library(sf)
library(sp)
library(ggcorrplot)
library(viridis)
library(caret)
library(stargazer)
library(kableExtra)
library(FNN)
library(osmdata)
library(spdep)
library(grid)
library(gridExtra)

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <- as.matrix(measureFrom)
  measureTo_Matrix <- as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    dplyr::summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

palette5 <- c('#f765b8','#f98dc9','#d7fffe','#a8f6f8', '#27fdf5')

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

crossValidate <- function(dataset, id, dependentVariable, indVariables) {
  
  allPredictions <- data.frame()
  cvID_list <- unique(dataset[[id]])
  
  for (i in cvID_list) {
    
    thisFold <- i
    cat("This hold out fold is", thisFold, "\n")
    
    fold.train <- filter(dataset, dataset[[id]] != thisFold) %>% as.data.frame() %>% 
      dplyr::select(id, geometry, indVariables, dependentVariable)
    fold.test  <- filter(dataset, dataset[[id]] == thisFold) %>% as.data.frame() %>% 
      dplyr::select(id, geometry, indVariables, dependentVariable)
    
    regression <-
      glm(count_airbnb ~ ., family = "poisson", 
          data = fold.train %>% 
            dplyr::select(-geometry, -id))
    
    thisPrediction <- 
      mutate(fold.test, Prediction = predict(regression, fold.test, type = "response"))
    
    allPredictions <-
      rbind(allPredictions, thisPrediction)
    
  }
  return(st_sf(allPredictions))
}


################
# DATA WRANGLING
################

#Load & project
nhoods <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=GEBIED_BUURTCOMBINATIES&THEMA=gebiedsindeling", quiet = TRUE)
nhoods.sf <-
  nhoods %>%
  st_as_sf() %>%
  st_transform('EPSG:28992')
apartments <- read.csv('./data/AmsterdamApts.csv')
airbnb <- read.csv('./data/listings_details.csv') 
airbnb.sf <- 
  airbnb %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  st_transform('EPSG:28992')
postcodes4 <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=PC4_BUURTEN&THEMA=postcode", quiet = TRUE)
postcodes4.sf <-
  postcodes4 %>%
  st_as_sf(coords = "geometry", crs = 4326, agr = "constant") %>%
  st_transform('EPSG:28992')
amsBoundary <-
  st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=GEBIEDEN25&THEMA=gebiedsindeling")

postcodes6 <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=PC6_VLAKKEN_BAG&THEMA=postcode", quiet = TRUE)
postcodes6.sf <-
  postcodes6 %>%
  st_as_sf(coords = "geometry", crs = 4326, agr = "constant") %>%
  st_transform('EPSG:28992') %>%
  rename(postcode = Postcode6)

apartments$postcode <- gsub(" ", "", apartments$postcode, fixed = TRUE)

apartmentsPC6 <- left_join(apartments, postcodes6.sf, by = "postcode") 

apartmentsPC6 <- apartmentsPC6 %>%
  dplyr::select('house_price', 'bedrooms', 'surface', 'geometry') %>%
  st_as_sf() %>%
  st_centroid() 

ggplot() +
  geom_sf(data = nhoods.sf) +
  geom_sf(data = apartmentsPC6)


?left_join

postcodes6.sf


#Other data ideas - red light district, old historic district (tighten the reigns of airbnb)
#Other architectural attractions.
#Other tourist attractions.
#Shopping districts.
#Beaches & pools
#Walkability
#take the log of the price

#---Open Data Amsterdam
bikestands <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=FIETSTAXI_STANDPLAATSEN&THEMA=fietstaxi", quiet = TRUE) %>%
  st_transform('EPSG:28992')
bikestands.sf <- st_join(bikestands, nhoods.sf, join = st_intersects, left = FALSE)

# culturezones <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=CULTUURHISTORIE_VERKENNINGEN&THEMA=cultuurhistorie", quiet = TRUE) %>%
#   st_transform('EPSG:28992')
# culture.sf <- st_join(culturezones, nhoods.sf, join = st_intersects, left = FALSE)

oldtrees <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=MONUMENTALE_BOMEN&THEMA=monumentaal_groen") %>%
  st_transform('EPSG:28992') %>%
  filter(Toelichting == "Leeftijd > 70 jaar")
oldtrees.sf <- st_join(oldtrees, nhoods.sf, join = st_intersects, left = FALSE)

parks <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=PARKPLANTSOENGROEN&THEMA=stadsparken", quiet = TRUE) %>%
  st_transform('EPSG:28992') %>%
  filter(Stadspark == 'J')
parks.sf <- parks %>% st_as_sf()

trammetro <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=TRAMMETRO_PUNTEN_2020&THEMA=trammetro", quiet = TRUE) %>%
  st_transform('EPSG:28992') %>%
  st_as_sf()
trammetro.sf <- st_join(trammetro, nhoods.sf, join = st_intersects, left = FALSE) 

# walkability <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=WALKABILITY&THEMA=walkability", quiet = TRUE) %>%
#   st_transform('EPSG:28992')

wallart <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=WANDKUNST&THEMA=wandkunst", quiet = TRUE) %>%
  st_transform('EPSG:28992')
wallart.sf <- st_join(wallart, nhoods.sf, join = st_intersects, left = FALSE)

#---Open Street Map
#Setting the bounding box
xmin = st_bbox(amsBoundary)[[1]]
ymin = st_bbox(amsBoundary)[[2]]
xmax = st_bbox(amsBoundary)[[3]]  
ymax = st_bbox(amsBoundary)[[4]]

bars <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>%
  add_osm_feature(key = 'amenity', value = c("bar", "pub")) %>%
  osmdata_sf()
bars <- bars$osm_points %>% .[amsBoundary,]
bars.sf <- bars %>%
  dplyr::select(geometry) %>%
  na.omit() %>%
  st_transform('EPSG:28992') %>%
  distinct() %>%
  st_centroid()

# hotels <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>%
#   add_osm_feature(key = "tourism", value = c("hotel")) %>%
#   osmdata_sf()
# hotels <- hotels$osm_points %>% .[amsBoundary,]
# hotels.sf <- hotels %>%
#   dplyr::select(geometry) %>%
#   na.omit() %>%
#   st_transform('EPSG:28992') %>%
#   distinct() %>%
#   st_centroid()

museums <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>%
  add_osm_feature(key = "tourism", value = c("museum")) %>%
  osmdata_sf()
museums <- museums$osm_polygons %>% .[amsBoundary,]
museums.sf <- museums %>%
  dplyr::select(geometry) %>%
  na.omit() %>%
  st_transform('EPSG:28992') %>%
  distinct() %>%
  st_centroid()

museums <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>%
  add_osm_feature(key = "tourism", value = c("museum")) %>%
  osmdata_sf()
museums <- museums$osm_polygons %>% .[amsBoundary,]
museums.sf <- museums %>%
  dplyr::select(geometry) %>%
  na.omit() %>%
  st_transform('EPSG:28992') %>%
  distinct() %>%
  st_centroid()

restaurants <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>%
  add_osm_feature(key = 'amenity', value = c("restaurant", "cafe")) %>%
  osmdata_sf()
restaurants <- restaurants$osm_points %>% .[amsBoundary,]
restaurants.sf <- restaurants %>%
  dplyr::select(geometry) %>%
  na.omit() %>%
  st_transform('EPSG:28992') %>%
  distinct() %>%
  st_centroid()

ggplot() + geom_sf(data = nhoods.sf) +
  geom_sf(data = hotels.sf, colour = "purple") 


#####################
# FEATURE ENGINEERING
#####################
#Dummy variables
airbnb.sf$price_no <- as.numeric(gsub('[$,]', '', airbnb.sf$price))
airbnb.sf$log_price <- log(airbnb.sf$price_no)
airbnb.sf$accommodates_no <- as.numeric(airbnb.sf$accommodates)
airbnb.sf$host_response_rate <- as.numeric(gsub('[%]', '', airbnb.sf$host_response_rate))
airbnb.sf$cleaning_fee_no <- as.numeric(gsub('[$]', '', airbnb.sf$cleaning_fee))

top10hoods <- 
  airbnb %>%
  group_by(neighbourhood) %>%
  summarise(mean_price = mean(price_no, na.rm = TRUE)) %>%
  arrange(-mean_price) %>% 
  top_n(., 10) %>%
  dplyr::select(neighbourhood) %>%
  pull(neighbourhood)


airbnb.sf <- 
  airbnb.sf %>%
  mutate(TV = as.numeric(grepl('TV', airbnb.sf$amenities)),
         Wifi = as.numeric(grepl('Wifi', airbnb.sf$amenities)),
         Families = as.numeric(grepl('Family/kid friendly', airbnb.sf$amenities)),
         Garden = as.numeric(grepl('Garden or backyard', airbnb.sf$amenities)),
         Patio = as.numeric(grepl('Patio or balcony', airbnb.sf$amenities)),
         Washer = as.numeric(grepl('Washer', airbnb.sf$amenities)),
         Workspace = as.numeric(grepl('"Laptop friendly workspace', airbnb.sf$amenities)),
         SmokeDetect = as.numeric(grepl('Smoke detector', airbnb.sf$amenities)),
         CO2 = as.numeric(grepl('Carbon monoxide detector', airbnb.sf$amenities)),
         FireEx = as.numeric(grepl('Fire extinguisher', airbnb.sf$amenities)),
         Alldaycheckin = as.numeric(grepl('24-hour check-in', airbnb.sf$amenities)),
         AnnualRev = price_no * availability_365,
         MonthlyRev = price_no * availability_30,
         Buiksloterham = ifelse(neighbourhood == "Buiksloterham", 1,0),
         TopTen = ifelse(neighbourhood %in% top10hoods, 1, 0))

##Nearest neighbor variables
airbnb.sf <- 
  airbnb.sf %>%
  mutate(bars_nn4 = nn_function(st_coordinates
                                (st_centroid(airbnb.sf)),
                                st_coordinates(st_centroid(bars.sf)), 4),
         bikestands_nn4 = nn_function(st_coordinates
                                (st_centroid(airbnb.sf)), 
                                st_coordinates(st_centroid(trammetro.sf)), 4),
         museums_nn4 = nn_function(st_coordinates
                                      (st_centroid(airbnb.sf)), 
                                      st_coordinates(st_centroid(museums.sf)), 4),
         restaurants_nn4 = nn_function(st_coordinates
                                      (st_centroid(airbnb.sf)), 
                                      st_coordinates(st_centroid(restaurants.sf)), 4),
         parks_nn4 = nn_function(st_coordinates
                                       (st_centroid(airbnb.sf)), 
                                       st_coordinates(st_centroid(parks.sf)), 4),
         tram_nn1 = nn_function(st_coordinates
                                (st_centroid(airbnb.sf)), 
                                st_coordinates(st_centroid(trammetro.sf)), 1),
         tram_nn2 = nn_function(st_coordinates
                                (st_centroid(airbnb.sf)), 
                                st_coordinates(st_centroid(trammetro.sf)), 2),
         tram_nn4 = nn_function(st_coordinates
                                (st_centroid(airbnb.sf)), 
                                st_coordinates(st_centroid(trammetro.sf)), 4),
         wallart_nn4 = nn_function(st_coordinates
                                   (st_centroid(airbnb.sf)), 
                                   st_coordinates(st_centroid(wallart.sf)), 4),
         oldtrees_nn4 = nn_function(st_coordinates
                                    (st_centroid(airbnb.sf)), 
                                    st_coordinates(st_centroid(oldtrees.sf)), 4))

#Spatial lag
coords <- st_coordinates(airbnb.sf) 
neighborList <- knn2nb(knearneigh(coords, 5))
spatialWeights <- nb2listw(neighborList, style="W")
airbnb.sf$lagPrice <- lag.listw(spatialWeights, airbnb.sf$price_no)

#Removing outliers
airbnb.sf <- subset(airbnb.sf, bedrooms < 6) 
airbnb.sf <- subset(airbnb.sf, bathrooms < 10) 
airbnb.sf <- subset(airbnb.sf, price_no < 1000 & price_no > 0)
airbnb.sf <- subset(airbnb.sf, calculated_host_listings_count < 20)

###---APARTMENT DATASET
postcodes4.sf$Postcode4 <- as.numeric(postcodes4.sf$Postcode4)
apartment_PC <- 
  apartments %>%
  group_by(postcode1) %>%
  summarise(mean_price = mean(house_price, na.rm = TRUE),
            median_price = median(house_price, na.rm = TRUE)) %>%
  rename(., Postcode4 = postcode1)
apartment_PC.sf <- left_join(apartment_avg, postcodes4.sf, by = 'Postcode4') %>%
  st_as_sf()

ggplot() +
  geom_sf(data = apartment_PC.sf, aes(fill=mean_price))

ggplot() +
  geom_sf(data = apartment_PC.sf, aes(fill=median_price))

######################
# EXPLORATORY ANALYSIS
######################
ggplot() +
  geom_sf(data = nhoods.sf, fill = "grey80") +
  geom_sf(data = airbnb.sf, aes(colour=q5(price_no)))

ggplot() +
  geom_sf(data = nhoods.sf, fill = "grey80") +
  geom_sf(data = airbnb.sf, aes(colour=bedrooms))

############
# Corrplots
############
#----Grid
colnames(airbnb.sf)
numericVars <-
  select_if(airbnb.sf, is.numeric) %>%
  st_drop_geometry() %>%
  dplyr::select(price_no,
                lagPrice,
                #bars_nn4,
                #restaurants_nn4,
                #museums_nn4,
                accommodates_no,
                availability_30,
                availability_365,
                bedrooms, 
                bathrooms, 
                cleaning_fee_no, 
                number_of_reviews, 
                bikestands_nn4,
                square_feet, 
                #tram_nn4,
                #wallart_nn4,
                #oldtrees_nn4,
                #parks_nn4, 
                review_scores_rating,
                review_scores_accuracy,
                review_scores_cleanliness, 
                review_scores_checkin,
                review_scores_communication,
                review_scores_location,
                review_scores_value) %>%
  na.omit()

ggcorrplot(
  round(cor(numericVars), 1),
  p.mat = cor_pmat(numericVars),
  colors = c("#f765b8", "white", "#27fdf5"),
  type="lower",
  insig = "blank") +
  labs(title = "Correlation across numeric variables",
       subtitle = "Figure X.X")

#---Scatterplots
correlation.long <-
  st_drop_geometry(airbnb.sf) %>%
  dplyr::select(price_no,
                review_scores_rating,
                review_scores_accuracy,
                review_scores_cleanliness, 
                review_scores_checkin,
                review_scores_communication,
                review_scores_location,
                review_scores_value) %>%
  gather(Variable, Value, -price_no)

correlation.cor <-
  correlation.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, price_no, use = "complete.obs"))

ggplot(correlation.long, aes(Value, price_no)) +
  geom_point(size = 0.1) +
  geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Price as a Function of Variables") +
  plotTheme()

#---AIRBNB COUNT

#Number of airbnbs by neighborhood
airbnb.sf %>%
  dplyr::select(name) %>%
  mutate(count = 1) %>%
  dplyr::select(count) %>%
  aggregate(., nhoods.sf, sum) %>%
  ggplot() +
  geom_sf(aes(fill=count)) 

airbnb.sf %>%
  dplyr::select(beds) %>%
  #mutate(count = 1) %>%
  #dplyr::select(count) %>%
  aggregate(., nhoods.sf, sum, na.rm = TRUE) %>%
  ggplot() +
  geom_sf(aes(fill=beds)) 

#Count of airbnbs by postal code
airbnb.sf %>%
  dplyr::select(name) %>%
  mutate(count = 1) %>%
  dplyr::select(count) %>%
  aggregate(., postcodes4.sf, sum) %>%
  ggplot() +
  geom_sf(aes(fill=count)) 

#---AIRBNB PRICE

#Average price by neighborhood 
airbnb.sf %>%
  dplyr::select(price_no) %>%
  aggregate(., nhoods.sf, mean) %>%
  ggplot() +
  geom_sf(aes(fill=price_no)) 

#Average price by postal code
airbnb.sf %>%
  dplyr::select(price_no) %>%
  aggregate(., postcodes4.sf, mean) %>%
  ggplot() +
  geom_sf(aes(fill=price_no)) 

#---AIRBNB AVAILABILITY

#Average monthly availability by neighborhood
airbnb.sf %>%
  dplyr::select(availability_30) %>%
  aggregate(., nhoods.sf, mean) %>%
  ggplot() +
  geom_sf(aes(fill=availability_30)) 

#Average annual availability by neighborhood
airbnb.sf %>%
  dplyr::select(availability_365) %>%
  aggregate(., nhoods.sf, mean) %>%
  ggplot() +
  geom_sf(aes(fill=availability_365)) 

#Annual monthly availability by neighborhood
airbnb.sf %>%
  dplyr::select(availability_30) %>%
  aggregate(., postcodes4.sf, mean) %>%
  ggplot() +
  geom_sf(aes(fill=availability_30)) 

#Annual average availability by postal code
airbnb.sf %>%
  dplyr::select(availability_365) %>%
  aggregate(., postcodes4.sf, mean) %>%
  ggplot() +
  geom_sf(aes(fill=availability_365)) 


#---AIRBNB SIZE
airbnb.sf %>%
  dplyr::select(bedrooms) %>%
  aggregate(., postcodes4.sf, mean) %>%
  ggplot() +
  geom_sf(aes(fill=bedrooms)) 

airbnb.sf %>%
  dplyr::select(bedrooms) %>%
  aggregate(., nhoods.sf, mean) %>%
  ggplot() +
  geom_sf(aes(fill=bedrooms))

airbnb.sf %>%
  dplyr::select(bathrooms) %>%
  aggregate(., postcodes4.sf, mean) %>%
  ggplot() +
  geom_sf(aes(fill=bathrooms)) 

airbnb.sf %>%
  dplyr::select(bathrooms) %>%
  aggregate(., nhoods.sf, mean) %>%
  ggplot() +
  geom_sf(aes(fill=bathrooms))

airbnb.sf %>%
  dplyr::select(accommodates_no) %>%
  aggregate(., postcodes4.sf, median) %>%
  ggplot() +
  geom_sf(aes(fill=accommodates_no)) 

airbnb.sf %>%
  dplyr::select(accommodates_no) %>%
  aggregate(., nhoods.sf, median) %>%
  ggplot() +
  geom_sf(aes(fill=accommodates_no))

# airbnb.sf %>%
#   dplyr::select(square_feet) %>%
#   aggregate(., postcodes4.sf, mean) %>%
#   ggplot() +
#   geom_sf(aes(fill=square_feet)) 
# 
# airbnb.sf %>%
#   dplyr::select(square_feet) %>%
#   aggregate(., nhoods.sf, median) %>%
#   ggplot() +
#   geom_sf(aes(fill=square_feet))

# #Apartments by postal code
# apartments %>%
#   dplyr::select(house_price, postcode1) %>%
#   aggregate(., postcodes4.sf, mean) %>%
#   ggplot() +
#   geom_sf(aes(fill=house_price)) 


############
# Barplots
############

#barplots numeric variables by price
airbnb %>%
  dplyr::select(bedrooms, bathrooms, accommodates_no, calculated_host_listings_count, price_no) %>%
  gather(Variable, Value, -price_no) %>%
  ggplot(aes(Value, price_no)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, scales = "free")+
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#barplots categorical variables by price
airbnb %>%
  dplyr::select(room_type, bed_type, Garden, Patio, Families, TV, Wifi, Alldaycheckin, price_no) %>%
  gather(Variable, Value, -price_no) %>%
  ggplot(aes(Value, price_no)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, scales = "free")+
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

hist(airbnb.sf$bedrooms)
hist(airbnb.sf$bathrooms)
hist(airbnb$availability_30)
hist(airbnb$host_response_rate)
hist(airbnb$calculated_host_listings_count)
hist(airbnb$cleaning_fee_no)

###super host categories 
# airbnb %>%
#   dplyr::select(price_no, host_is_superhost) %>%
#   gather(Variable, Value, -price_no) %>%
#   ggplot(aes(Value, price_no)) + 
#   geom_bar(position = "dodge", stat = "summary", fun = "mean") + 
#   facet_wrap(~Variable, scales = "free") +
#   labs(title = "Mean Prices based on SuperHost Status", y = "Mean_Price") +
#   plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mean Price for each Property Type
airbnb %>%
  dplyr::select(price_no, property_type) %>%
  gather(Variable, Value, -price_no) %>%
  ggplot(aes(Value, price_no)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") + 
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Mean Prices based on Property Type", y = "Mean_Price") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

airbnb %>%
  group_by(property_type) %>%
  summarise(mean_price = mean(price_no, na.rm = TRUE)) %>%
  arrange(-mean_price)

airbnb%>% 
  dplyr::select(price_no, neighbourhood) %>%
  gather(Variable, Value, -price_no) %>% 
  ggplot(aes(Value, price_no)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, ncol = 1, scales = "free") +
  labs(title = "Price as a function of\ncategorical variables", y = "Mean_Price") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

airbnb %>%
  group_by(neighbourhood) %>%
  summarise(mean_price = mean(price_no, na.rm = TRUE)) %>%
  arrange(-mean_price)

airbnb%>% 
  dplyr::select(price_no, bedrooms) %>%
  gather(Variable, Value, -price_no) %>% 
  ggplot(aes(Value, price_no)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, ncol = 1, scales = "free") +
  labs(title = "Price as a function of\ncategorical variables", y = "Mean_Price") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

airbnb %>%
  group_by(bedrooms) %>%
  summarise(mean_price = mean(price_no, na.rm = TRUE),
            mean_accommodates = mean(accommodates),
            count = n()) %>%
  arrange(bedrooms)

airbnb %>%
  group_by(bedrooms) %>%
  summarise(n()) %>%
  arrange(bedrooms)

########################
# FISHNET MODEL (CHICAGO)
########################
fishnet <- 
  st_make_grid(nhoods.sf,
               cellsize = 200, 
               square = TRUE) %>%
  .[nhoods.sf] %>% ## MH added this - something changed in the package?
  st_sf() %>%
  mutate(uniqueID = rownames(.))

ggplot() + geom_sf(data = fishnet)

airbnb_net <- 
  dplyr::select(airbnb.sf) %>% 
  mutate(count_airbnb = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(count_airbnb = replace_na(count_airbnb, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE))

airbnb_net_count <- 
  airbnb.sf %>%
  dplyr::select(accommodates_no, bedrooms, availability_365) %>% 
  mutate(count_airbnb = 1,
         count_accomodates = accommodates_no, #Add in count variables
         count_bedrooms = bedrooms,
         count_avail365 = availability_365) %>%
  aggregate(., fishnet, sum) %>%
  mutate(count_airbnb = replace_na(count_airbnb, 0),
         count_accomodates = replace_na(count_accomodates, 0),
         count_bedrooms = replace_na(count_bedrooms, 0),
         count_avail365 = replace_na(count_avail365, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE)) %>%
  dplyr::select(-accommodates_no, -bedrooms, - availability_365)

airbnb_net_mean <- airbnb.sf %>%
  dplyr::select(price_no) %>%
  mutate(avg_price = price_no) %>%
  aggregate(., fishnet, mean) %>%
  mutate(avg_price = replace_na(avg_price, 0),
         price_no = replace_na(price_no, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE)) %>%
  st_drop_geometry()

airbnb_net <- left_join(airbnb_net_count, airbnb_net_mean, by = "uniqueID")

?left_join

ggplot() +
  geom_sf(data = airbnb_net, aes(fill = count_airbnb), color = NA) +
  scale_fill_viridis() +
  geom_sf(data = nhoods.sf, colour = 'grey80', fill = 'transparent') +
  labs(title = "Count of Airbnbs for the Fishnet") +
  mapTheme()


#What would be the best way to incorporate interior features and characteristics into the fishnet model?
#Does that work for the fishnet?

#Outside Data

#OSM Data
bars_net <- bars.sf %>% mutate(Legend = "Bars")

#Open Data Amsterdam
oldtrees_net <- oldtrees.sf %>% dplyr::select(geometry) %>% mutate(Legend = "Old_Trees")

vars_net <- 
  rbind(bars_net, 
        oldtrees_net) %>%
  st_join(., fishnet, join = st_within) %>%
  st_drop_geometry() %>%
  group_by(uniqueID, Legend) %>%
  summarize(count = n()) %>%
  full_join(fishnet, by = "uniqueID") %>%
  spread(Legend, count, fill = 0) %>%
  st_sf() %>%
  dplyr::select(-`<NA>`) %>%
  na.omit() %>%
  ungroup()

st_c <- st_coordinates
st_coid <- st_centroid

vars_net <- vars_net %>%
  mutate(
    Bars.nn =
      nn_function(st_c(st_coid(varsnet)), st_c(bars.sf),2),
    Oldtrees.nn =
      nn_function(st_c(st_coid(varsnet)), st_c(oldtrees.sf),2))

#Small multiple maps with the fishnet
vars_net.long <- 
  gather(varsnet, Variable, value, -geometry, -uniqueID)

vars <- unique(vars_net.long$Variable)
mapList <- list()

for(i in vars){
  mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(vars_net.long, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis(name="") +
    labs(title=i) +
    mapTheme()}

do.call(grid.arrange,c(mapList, ncol =2, top = "Variables by Fishnet"))

#Local Moran's
final_net <-
  left_join(airbnb_net, st_drop_geometry(vars_net), by="uniqueID") 

final_net <-
  st_centroid(final_net) %>%
  st_join(dplyr::select(nhoods.sf, Buurtcombinatie), by = "uniqueID") %>%
  st_join(dplyr::select(postcodes4.sf, Postcode4), by = "uniqueID") %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(final_net, geometry, uniqueID)) %>%
  st_sf() %>%
  na.omit()

## generates warnings from PROJ issues
## {spdep} to make polygon to neighborhoods... 
final_net.nb <- poly2nb(as_Spatial(final_net), queen=TRUE)
## ... and neighborhoods to list of weights
final_net.weights <- nb2listw(final_net.nb, style="W", zero.policy=TRUE)

final_net.localMorans <- 
  cbind(
    as.data.frame(localmoran(final_net$count_airbnb, final_net.weights)),
    as.data.frame(final_net)) %>% 
  st_sf() %>%
  dplyr::select(Airbnb_Count = count_airbnb, 
                Local_Morans_I = Ii, 
                P_Value = `Pr(z > 0)`) %>%
  mutate(Significant_Hotspots = ifelse(P_Value <= 0.0000001, 1, 0)) %>% #.001,.0001,4,6,8,10,12,14,16
  gather(Variable, Value, -geometry)

vars <- unique(final_net.localMorans$Variable)
varList <- list()

for(i in vars){
  varList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(final_net.localMorans, Variable == i), 
            aes(fill = Value), colour=NA) +
    scale_fill_viridis(name="") +
    labs(title=i) +
    mapTheme() + theme(legend.position="bottom")}

do.call(grid.arrange,c(varList, ncol = 4, top = "Local Morans I statistics, Heroin Crimes"))

final_net <-
  final_net %>% 
  mutate(airbnb.isSig = 
           ifelse(localmoran(final_net$count_airbnb, 
                             final_net.weights)[,5] <= 0.00001, 1, 0)) %>%
  mutate(airbnb.isSig.dist = 
           nn_function(st_coordinates(st_centroid(final_net)),
                       st_coordinates(st_centroid(
                         filter(final_net, airbnb.isSig == 1))), 1)) 

## Regression with the fishnet
colnames(final_net)
reg.vars <- c("Bars", "Old_Trees", "Bars.nn", "Oldtrees.nn", "Buurtcombinatie", "Postcode4",
              "airbnb.isSig", "airbnb.isSig.dist")

#LOGO cross validation
#Having trouble here
reg.cv <- crossValidate(
  dataset = final_net,
  id = "cvID.x",
  dependentVariable = "count_airbnb",
  indVariables = reg.vars) %>%
  dplyr::select(cvID = cvID, count_airbnb, Prediction, geometry)

reg.spatialCV <- crossValidate(
  dataset = final_net,
  id = "name",
  dependentVariable = "countheroin_cases",
  indVariables = reg.vars) %>%
  dplyr::select(cvID = name, countheroin_cases, Prediction, geometry)




###############################
# LINEAR PRICE PREDICTION MODEL (MIAMI)
###############################
#Setting up test and training datasets
inTrain <- createDataPartition( 
  y = paste(airbnb.sf$room_type, airbnb.sf$neighbourhood_cleansed), 
  p = .70, list = FALSE)

airbnb.training <- airbnb.sf[inTrain,] 
airbnb.test <- airbnb.sf[-inTrain,]  

colnames(airbnb.sf)
airbnb.sf$review_scores_rating

#Multivariate regression
reg.vars <- c("log_price",
              "neighbourhood_cleansed", 
              "availability_365", 
              "accommodates_no", 
              "bedrooms", 
              "bathrooms",
              "minimum_nights",
              "number_of_reviews",
              "review_scores_rating",
              #"review_scores_accuracy",
              "review_scores_cleanliness", 
              #"review_scores_checkin",
              #"review_scores_communication",
              "review_scores_location",
              "review_scores_value",
              #"square_feet",
              "room_type",
              #"bed_type",
              #"Families",
              "TopTen", 
              "TV", 
              #"tram_nn4",
              "museums_nn4",
              #"bikestands_nn4",
              #"restaurants_nn4",
              "bars_nn4",
              #"oldtrees_nn4",
              #"wallart_nn4",
              "Wifi",
              "lagPrice",
              "parks_nn4"
              #"Alldaycheckin"
              )

reg1 <- lm(log_price ~ ., data = st_drop_geometry(airbnb.training) %>% 
             dplyr::select(all_of(reg.vars)))

stargazer(
  reg1,
  type = "text",
  title = "Linear Model Summary Table",
  dep.var.caption = " ",
  dep.var.labels = "Model Features")

#Cross validation tests
reg1_predict <- predict(reg1, newdata = airbnb.test)

rmse.train <- caret::MAE(predict(reg1), airbnb.training$log_price)
rmse.test <- caret::MAE(reg1_predict, airbnb.test$log_price)

preds.train <- data.frame(pred   = exp(predict(reg1)),
                          actual = airbnb.training$price_no,
                          source = "training data")
preds.test  <- data.frame(pred   = exp(reg1_predict),
                          actual = airbnb.test$price_no,
                          source = "testing data")

preds <- rbind(preds.train, preds.test)

ggplot(preds, aes(x = pred, y = actual, color = source)) +
  geom_point() +
  geom_smooth(method = "lm", color = "green") +
  geom_abline(color = "orange") +
  coord_equal() +
  theme_bw() +
  facet_wrap(~source, ncol = 2) +
  labs(title = "Comparing predictions to actual values",
       x = "Predicted Value",
       y = "Actual Value",
       subtitle = "Figure X.X") +
  theme(
    legend.position = "none"
  )

#Evaluate errors
airbnb.test <-
  airbnb.test %>%
  mutate(Regression = "Baseline Regression",
         SalePrice.Predict = predict(reg1, airbnb.test),
         SalePrice.Error = exp(SalePrice.Predict) - price_no,
         SalePrice.AbsError = abs(exp(SalePrice.Predict) - price_no),
         SalePrice.APE = (abs(exp(SalePrice.Predict) - price_no)) / exp(SalePrice.Predict))

ErrorTable <- 
  airbnb.test %>% 
  dplyr::summarize(Regression = "Baseline Regression",
                   MAE = mean(SalePrice.AbsError, na.rm = T), 
                   MAPE = mean(SalePrice.AbsError, na.rm = T) / mean(price_no, na.rm = T)) 

ErrorTable %>% 
  st_drop_geometry %>%
  group_by(Regression) %>%
  arrange(desc(MAE)) %>% 
  kable(caption = "MAE and MAPE for Test Set Data") %>% kable_styling()

#Generalizable 
fitControl <- trainControl(method = "cv", 
                           number = 100,
                           savePredictions = TRUE)

set.seed(717)
reg1.cv <- 
  train(price_no ~ ., data = st_drop_geometry(airbnb.sf) %>% 
          dplyr::select(reg.vars), 
        method = "lm", 
        trControl = fitControl, 
        na.action = na.pass)

reg1.cv 

#Standard Deviation and Histogram of MAE
reg1.cv.resample <- reg1.cv$resample

sd(reg1.cv.resample$MAPE)

ggplot(reg1.cv.resample, aes(x=MAE)) + geom_histogram(color = "grey40", fill = "#27fdf5", bins = 50) + 
  labs(title="Histogram of Mean Average Error Across 100 Folds",
       subtitle = "Figure X.X") +
  plotTheme()

