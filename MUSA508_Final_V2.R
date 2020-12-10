############################
# MUSA 508 FINAL PROJECT V2
# Airbnb in Amsterdam
# Ejiro & Maddy
############################

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


############
# LOAD DATA
############
#Load & project boundary, airbnb data
nhoods <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=GEBIED_BUURTCOMBINATIES&THEMA=gebiedsindeling", quiet = TRUE)
nhoods.sf <- nhoods %>%
  st_as_sf() %>%
  st_transform('EPSG:28992')
ostcodes4 <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=PC4_BUURTEN&THEMA=postcode", quiet = TRUE)
postcodes4.sf <- postcodes4 %>%
  st_as_sf(coords = "geometry", crs = 4326, agr = "constant") %>%
  st_transform('EPSG:28992')
amsBoundary <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=GEBIEDEN25&THEMA=gebiedsindeling")
postcodes6 <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=PC6_VLAKKEN_BAG&THEMA=postcode", quiet = TRUE)
postcodes6.sf <- postcodes6 %>%
  st_as_sf(coords = "geometry", crs = 4326, agr = "constant") %>%
  st_transform('EPSG:28992') %>%
  rename(postcode = Postcode6) 

airbnb <- read.csv('./data/listings_details.csv') 
airbnb.sf <- airbnb %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  st_transform('EPSG:28992') %>%
  dplyr::select("price", "bedrooms", "geometry") %>%
  mutate(Type = "Short-Term") %>%
  na.omit()
airbnb.sf$price <- as.numeric(gsub('[$,]', '', airbnb.sf$price)) #removing $ signs from price

apartments <- read.csv('./data/AmsterdamApts.csv')
apartments$postcode <- gsub(" ", "", apartments$postcode, fixed = TRUE)
apartmentsPC6 <- left_join(apartments, postcodes6.sf, by = "postcode") 
apartmentsPC6 <- apartmentsPC6 %>%
  dplyr::select('house_price', 'bedrooms', 'surface', 'geometry') %>%
  st_as_sf() %>%
  st_centroid() %>%
  mutate(price = house_price*1.21) %>% #convert variables to USD, feet
  dplyr::select('price', 'bedrooms', 'geometry') %>%
  mutate(Type = "Long-Term") %>% 
  st_as_sf() %>%
  na.omit()

ggplot() + geom_sf(data = airbnb.sf)

#square_feet = surface* 10.7639
#Combining the short-term and long-term rental datasets
df <- rbind(airbnb.sf, apartmentsPC6)
df <- st_join(df, nhoods.sf) %>%
  rename(neighborhood = Buurtcombinatie) %>%
  dplyr::select(-Opp_m2, ) %>%
  na.omit() %>%
  st_as_sf()

df <- subset(df, bedrooms < 4)
df <- subset(df, price > 0)
df <- subset(df, price < 1000)

########################
# NEIGHBORHOOD VARIABLES
########################

#---Open Data Amsterdam
bikestands <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=FIETSTAXI_STANDPLAATSEN&THEMA=fietstaxi", quiet = TRUE) %>%
  st_transform('EPSG:28992')
bikestands.sf <- st_join(bikestands, nhoods.sf, join = st_intersects, left = FALSE)

conservation <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=BESCHERMDESTADSGEZICHTEN&THEMA=cultuurhistorie", quiet = TRUE) %>%
  st_transform('EPSG:28992') 

Ams_Binnen <- conservation %>%
  filter(Naam_gebied =="Amsterdam-Binnen de Singelgracht") 
Ams_Binnen.sf <- st_join(Ams_Binnen, nhoods.sf, join = st_intersects, left = FALSE)

OudZuid <- conservation %>%
  filter(Naam_gebied =="Oud Zuid") 
OudZuid.sf <- st_join(OudZuid, nhoods.sf, join = st_intersects, left = FALSE)

PlanZuid <- conservation %>%
  filter(Naam_gebied =="Plan Zuid") 
PlanZuid.sf <- st_join(PlanZuid, nhoods.sf, join = st_intersects, left = FALSE)

devnhoods <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=ONTWIKKELBUURTEN&THEMA=ontwikkelbuurten", quiet = TRUE) %>%
  st_transform('EPSG:28992')
devnhoods.sf <- st_join(devnhoods, nhoods.sf, join = st_intersects, left = FALSE)

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

wallart <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=WANDKUNST&THEMA=wandkunst", quiet = TRUE) %>%
  st_transform('EPSG:28992')
wallart.sf <- st_join(wallart, nhoods.sf, join = st_intersects, left = FALSE)

#---Open Street Map
#Setting the bounding box
xmin = st_bbox(amsBoundary)[[1]]
ymin = st_bbox(amsBoundary)[[2]]
xmax = st_bbox(amsBoundary)[[3]]  
ymax = st_bbox(amsBoundary)[[4]]

#Querying the API
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

universities <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>%
  add_osm_feature(key = 'amenity', value = c("university")) %>%
  osmdata_sf()
universities <- universities$osm_polygons %>% .[amsBoundary,]
universities.sf <- universities %>%
  dplyr::select(geometry) %>%
  na.omit() %>%
  st_transform('EPSG:28992') %>%
  distinct() %>%
  st_centroid()

ggplot() + geom_sf(data = amsBoundary) + geom_sf(data = universities.sf)

#####################
# FEATURE ENGINEERING
#####################

#Dummy variables
top10hoods <- 
  df %>%
  filter(Type == "Short-Term") %>%
  group_by(neighborhood) %>%
  summarise(mean_price = mean(price, na.rm = TRUE)) %>%
  arrange(-mean_price) %>% 
  dplyr::select(neighborhood) %>%
  pull(neighborhood)
top10hoods <- top10hoods[1:10]

top20hoods <- 
  df %>%
  filter(Type == "Short-Term") %>%
  group_by(neighborhood) %>%
  summarise(mean_price = mean(price, na.rm = TRUE)) %>%
  arrange(-mean_price) %>% 
  dplyr::select(neighborhood) %>%
  pull(neighborhood)
top20hoods <- top20hoods[1:20]

airbnb$accommodates_no <- as.numeric(as.character(airbnb$accommodates))

accommodates <-
  airbnb %>% 
  filter(bedrooms >0) %>%
  dplyr::select(bedrooms, accommodates) %>%
  group_by(bedrooms) %>%
  summarise(mean_accomodates = mean(accommodates))

df <- 
  df %>%
  mutate(TopTen = ifelse(neighborhood %in% top10hoods, 1, 0),
         TopTwenty = ifelse(neighborhood %in% top20hoods, 1, 0),
         OverThreeBeds = ifelse(bedrooms >=3, 1, 0),
         OverFiveBeds = ifelse(bedrooms >=5, 1, 0))

#Nearest neighbor variable
df <- 
  df %>%
  mutate(bars_nn = nn_function(st_coordinates
                                (st_centroid(df)),
                                st_coordinates(st_centroid(bars.sf)), 1),
         bikestands_nn = nn_function(st_coordinates
                                      (st_centroid(df)), 
                                      st_coordinates(st_centroid(trammetro.sf)), 1),
         museums_nn = nn_function(st_coordinates
                                   (st_centroid(df)), 
                                   st_coordinates(st_centroid(museums.sf)), 1),
         restaurants_nn = nn_function(st_coordinates
                                       (st_centroid(df)), 
                                       st_coordinates(st_centroid(restaurants.sf)), 1),
         parks_nn = nn_function(st_coordinates
                                 (st_centroid(df)), 
                                 st_coordinates(st_centroid(parks.sf)), 1),
         tram_nn = nn_function(st_coordinates
                                (st_centroid(df)), 
                                st_coordinates(st_centroid(trammetro.sf)), 1),
         universities_nn = nn_function(st_coordinates
                                     (st_centroid(df)), 
                                     st_coordinates(st_centroid(universities.sf)), 1),
         wallart_nn = nn_function(st_coordinates
                                   (st_centroid(df)), 
                                   st_coordinates(st_centroid(wallart.sf)), 1),
         oldtrees_nn = nn_function(st_coordinates
                                    (st_centroid(df)), 
                                    st_coordinates(st_centroid(oldtrees.sf)), 1),
         AmsBinnen_nn = nn_function(st_coordinates
                                   (st_centroid(df)), 
                                   st_coordinates(st_centroid(Ams_Binnen.sf)), 1),
         OudZuid_nn = nn_function(st_coordinates
                                    (st_centroid(df)), 
                                    st_coordinates(st_centroid(OudZuid.sf)), 1),
         PlanZuid_nn = nn_function(st_coordinates
                               (st_centroid(df)), 
                               st_coordinates(st_centroid(PlanZuid.sf)), 1),
         devnhoods_nn = nn_function(st_coordinates
                                    (st_centroid(df)), 
                                    st_coordinates(st_centroid(devnhoods.sf)), 1))

#Spatial lag
coords <- st_coordinates(df) 
neighborList5 <- knn2nb(knearneigh(coords, 5))
spatialWeights5 <- nb2listw(neighborList5, style="W")
df$lagPrice5 <- lag.listw(spatialWeights5, df$price)

coords <- st_coordinates(df) 
neighborList10 <- knn2nb(knearneigh(coords, 10))
spatialWeights10 <- nb2listw(neighborList10, style="W")
df$lagPrice10 <- lag.listw(spatialWeights10, df$price)

#Variable Manipulations
df$logprice <- log(df$price)
df$museums2 <- df$museums_nn^2
df$museums3 <- df$museums_nn^3
df$logmuseum <- log(df$museums_nn)
df$parks2 <- df$parks_nn^2
df$logparks <- log(df$parks_nn)
df$logrestaurants <- log(df$restaurants_nn)
df$logbars <- log(df$bars_nn)
df$logwallart <- log(df$wallart_nn)
df$logbikestand <- log(df$bikestands_nn)
df$logtram <- log(df$tram_nn)
df$loguniversities <- log(df$universities_nn)
df$logoldtrees <- log(df$oldtrees_nn)
df$logAmsBinnen <- log(df$AmsBinnen_nn)
df$logOudZuid <- log(df$OudZuid_nn)
df$logPlanZuid <- log(df$PlanZuid_nn)
df$logdevnhoods <- log(df$devnhoods_nn)

#######################
# EXPLORATORY ANALYSIS
#######################
numericVars <-
  select_if(df, is.numeric) %>%
  st_drop_geometry() %>%
  dplyr::select(price,
                bars_nn,
                bikestands_nn,
                museums_nn,
                logparks,
                restaurants_nn,
                parks_nn,
                logmuseum,
                tram_nn,
                universities_nn,
                wallart_nn,
                oldtrees_nn,
                AmsBinnen_nn,
                OudZuid_nn,
                PlanZuid_nn, 
                devnhoods_nn) %>%
  na.omit()

ggcorrplot(
  round(cor(numericVars), 1),
  p.mat = cor_pmat(numericVars),
  colors = c("#f765b8", "white", "#27fdf5"),
  type="lower",
  insig = "blank") +
  labs(title = "Correlation across numeric variables",
       subtitle = "Figure X.X")

boxplot(df$bedrooms)

#---Scatterplots
correlation.long <-
  st_drop_geometry(df) %>%
  filter(bedrooms < 6) %>%
  dplyr::select(price,
                bedrooms,
                lagPrice5,
                lagPrice10,
                bars_nn,
                logbars,
                restaurants_nn,
                logrestaurants,
                bikestands_nn,
                logbikestand,
                museums_nn,
                logmuseum,
                parks_nn,
                parks2,
                logparks,
                tram_nn,
                logtram,
                universities_nn,
                loguniversities,
                oldtrees_nn,
                logoldtrees,
                AmsBinnen_nn,
                logAmsBinnen,
                OudZuid_nn,
                logOudZuid,
                PlanZuid_nn,
                logPlanZuid,
                devnhoods_nn,
                logdevnhoods
                ) %>%
  gather(Variable, Value, -price) 
  
correlation.cor <-
  correlation.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, price, use = "complete.obs"))

ggplot(correlation.long, aes(Value, price)) +
  geom_point(size = 0.1) +
  geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(title = "Amenities") +
  plotTheme()

#---Maps
airbnb_price <- airbnb.sf %>%
  dplyr::select(price) %>%
  aggregate(., nhoods.sf, median) %>%
  ggplot() +
  geom_sf(aes(fill=price)) + 
  scale_fill_viridis() +
  mapTheme()

apt_price <- apartmentsPC6 %>%
  dplyr::select(price) %>%
  aggregate(., nhoods.sf, mean) %>%
  ggplot() +
  geom_sf(aes(fill=price)) + 
  scale_fill_viridis() +
  mapTheme()

grid.arrange(airbnb_price, apt_price, 
             ncol = 2, 
             top = textGrob("Average Price of Airbnb vs. Apartment by Neighborhood", 
                            gp=gpar(fontsize=20)))

airbnb.sf %>%
  mutate(count = 1) %>%
  dplyr::select(count) %>%
  aggregate(., nhoods.sf, sum) %>%
  ggplot() +
  geom_sf(aes(fill=count)) + 
  scale_fill_viridis() +
  labs(title = "2018 Count of Airbnb by Neighborhood") +
  mapTheme()

apartmentsPC6 %>%
  dplyr::select(price, geometry) %>%
  ggplot() +
  geom_sf(data = nhoods.sf, fill = "grey80") +
  geom_sf(colour = "red", size = .7) +
  mapTheme() +
  labs(title = "Long-Term Rentals to Predict Short-Term Price")
  

###############################
# LINEAR PRICE PREDICTION MODEL 
###############################
df_reg <- subset(df, Type == "Short-Term")
#df_reg <- subset(df, price > 0)
#df_reg$logprice <- log(df_reg$price)

#Setting up test and training datasets
inTrain <- createDataPartition( 
  y = paste(df_reg$neighborhood), 
  p = .70, list = FALSE)

df.training <- df_reg[inTrain,] 
df.test <- df_reg[-inTrain,]  

colnames(df)

#Multivariate regression
reg.vars <- c("price",
              "bedrooms",
              "lagPrice5",
              "lagPrice10",
              "TopTen",
              "TopTwenty",
              "OverThreeBeds",
              "OverFiveBeds",
              #"bars_nn",
              "logbars",
              #"restaurants_nn",
              "logrestaurants",
              "bikestands_nn",
              #"logbikestand",
              #"museums_nn",
              "logmuseum",
              #"parks_nn",
              "logparks",
              "tram_nn",
              #'logtram',
              "universities_nn",
              #"loguniversities",
              "oldtrees_nn",
              #"logoldtrees",
              #"AmsBinnen_nn",
              "logAmsBinnen",
              #"OudZuid_nn",
              "logOudZuid",
              "PlanZuid_nn",
              #"logPlanZuid",
              #"devnhoods_nn",
              "logdevnhoods"
              )

reg1 <- lm(price ~ ., data = st_drop_geometry(df.training) %>% 
             dplyr::select(all_of(reg.vars)))

stargazer(
  reg1,
  type = "text",
  title = "Linear Model Summary Table",
  dep.var.caption = " ",
  dep.var.labels = "Model Features")

#Cross validation tests
reg1_predict <- predict(reg1, newdata = df.test)

rmse.train <- caret::MAE(predict(reg1), df.training$price)
rmse.test <- caret::MAE(reg1_predict, df.test$price)

preds.train <- data.frame(pred   = predict(reg1),
                          actual = df.training$price,
                          source = "training data")
preds.test  <- data.frame(pred   = reg1_predict,
                          actual = df.test$price,
                          source = "testing data")

preds <- rbind(preds.train, preds.test)

ggplot(preds, aes(x = pred, y = actual, color = source)) +
  geom_point() +
  geom_smooth(method = "lm", color = "green") +
  geom_abline(color = "orange") +
  #coord_equal() +
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
df.test <-
  df.test %>%
  mutate(Regression = "Baseline Regression",
         SalePrice.Predict = predict(reg1, df.test),
         SalePrice.Error = SalePrice.Predict - price,
         SalePrice.AbsError = abs(SalePrice.Predict - price),
         SalePrice.APE = (abs(SalePrice.Predict - price)) / SalePrice.Predict)

ErrorTable <- 
  df.test %>% 
  dplyr::summarize(Regression = "Baseline Regression",
                   MAE = mean(SalePrice.AbsError, na.rm = T), 
                   MAPE = mean(SalePrice.AbsError, na.rm = T) / mean(price, na.rm = T)) 

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
  train(price ~ ., data = st_drop_geometry(df_reg) %>% 
          dplyr::select(reg.vars), 
        method = "lm", 
        trControl = fitControl, 
        na.action = na.pass)

reg1.cv

#Standard Deviation and Histogram of MAE
reg1.cv.resample <- reg1.cv$resample

sd(reg1.cv.resample$MAE)

ggplot(reg1.cv.resample, aes(x=MAE)) + geom_histogram(color = "grey40", fill = "#232761ff", bins = 50) + 
  labs(title="Histogram of Mean Average Error Across 100 Folds") +
  plotTheme()

######################################
# Compare predictions to actual prices
######################################

df$predict <- predict(reg1, newdata = df)
df$monthlyprice <- df$price*30
df$monthlypred <- df$predict*30
df$absdiff <- abs(df$monthlyprice - df$monthlypred)
df$pctdiff <- df$diff / df$monthlyprice

longterm <- df %>% filter(Type == "Long-term")

longterm %>%
  ggplot() +
  geom_sf(data = nhoods) +
  geom_sf(data = longterm, aes(colour = pctdiff)) +
  scale_color_viridis() +
  mapTheme()

df %>%
  filter(bedrooms == 2 & Type == "Long-term") %>%
  ggplot() +
  geom_sf(data = nhoods) +
  geom_sf(data = ., aes(colour = pctdiff)) +
  scale_color_viridis() +
  mapTheme()

ggplot() +
  geom_sf(data = nhoods) +
  geom_sf(data = df, aes(colour = diff, alpha = .5)) +
  scale_color_viridis() +

df %>%
  dplyr::select(pctdiff) %>%
  aggregate(., nhoods.sf, mean) %>%
  ggplot() +
  geom_sf(aes(fill = pctdiff)) +
  labs(title = "Average Percent Difference Between \nLong-Term & Short-Term Revenue") +
  scale_fill_viridis() +
  mapTheme()

df %>%
  dplyr::select(diff) %>%
  aggregate(., nhoods.sf, mean) %>%
  ggplot() +
  geom_sf(aes(fill = diff)) +
  labs(title = "Average Absolute Difference Between \nLong-Term & Short-Term Revenue") +
  scale_fill_viridis() +
  mapTheme()




#-------------------------------------------------------------

###############################
# MODEL w/ LOG PRICE
###############################
dflog_reg <- subset(df, Type == "Short-Term")

#Setting up test and training datasets
inTrain <- createDataPartition( 
  y = paste(dflog_reg$neighborhood), 
  p = .70, list = FALSE)

dflog.training <- dflog_reg[inTrain,] 
dflog.test <- dflog_reg[-inTrain,]  

#Multivariate regression
reg.vars.log <- c("logprice",
              "bedrooms",
              "lagPrice5",
              "lagPrice10",
              "TopTen",
              "TopTwenty",
              "OverThreeBeds",
              "OverFiveBeds",
              #"bars_nn",
              "logbars",
              #"restaurants_nn",
              "logrestaurants",
              "bikestands_nn",
              #"logbikestand",
              #"museums_nn",
              "logmuseum",
              #"parks_nn",
              "logparks",
              "tram_nn",
              #'logtram',
              "universities_nn",
              #"loguniversities",
              "oldtrees_nn",
              #"logoldtrees",
              #"AmsBinnen_nn",
              "logAmsBinnen",
              #"OudZuid_nn",
              "logOudZuid",
              "PlanZuid_nn",
              #"logPlanZuid",
              #"devnhoods_nn",
              "logdevnhoods"
)

reglog1 <- lm(logprice ~ ., data = st_drop_geometry(dflog.training) %>% 
             dplyr::select(all_of(reg.vars.log)))

stargazer(
  reglog1,
  type = "text",
  title = "Linear Model Summary Table",
  dep.var.caption = " ",
  dep.var.labels = "Model Features")

#Cross validation tests
reglog_predict <- exp(predict(reglog1, newdata = dflog.test))

rmse.train <- caret::MAE(exp(predict(reglog1)), dflog.training$logprice)
rmse.test <- caret::MAE(reglog_predict, dflog.test$logprice)

preds.train <- data.frame(pred   = exp(predict(reglog1)),
                          actual = df.training$price,
                          source = "training data")
preds.test  <- data.frame(pred   = reglog_predict,
                          actual = df.test$price,
                          source = "testing data")

preds <- rbind(preds.train, preds.test)

#Evaluate errors
dflog.test <-
  dflog.test %>%
  mutate(Regression = "Log Regression",
         SalePrice.Predict = exp(predict(reglog1, dflog.test)),
         SalePrice.Error = SalePrice.Predict - price,
         SalePrice.AbsError = abs(SalePrice.Predict - price),
         SalePrice.APE = (abs(SalePrice.Predict - price)) / SalePrice.Predict)

ErrorTableLog <- 
  dflog.test %>% 
  dplyr::summarize(Regression = "Log Regression",
                   MAE = mean(SalePrice.AbsError, na.rm = T), 
                   MAPE = mean(SalePrice.AbsError, na.rm = T) / mean(price, na.rm = T)) 

ErrorTableLog %>% 
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
  train(price ~ ., data = st_drop_geometry(df_reg) %>% 
          dplyr::select(reg.vars), 
        method = "lm", 
        trControl = fitControl, 
        na.action = na.pass)

reg1.cv

#Standard Deviation and Histogram of MAE
reg1.cv.resample <- reg1.cv$resample

sd(reg1.cv.resample$MAE)

ggplot(reg1.cv.resample, aes(x=MAE)) + geom_histogram(color = "grey40", fill = "#232761ff", bins = 50) + 
  labs(title="Histogram of Mean Average Error Across 100 Folds") +
  plotTheme()












#################
# 1 BEDROOM MODEL
#################
df_1Bed <- subset(df, Type == "Short-Term" & bedrooms == 1)
df_2Bed <- subset(df, Type == "Short-Term" & bedrooms == 2)
df_3Bed <- subset(df, Type == "Short-Term" & bedrooms == 3)
#df_reg <- subset(df, price > 0)
#df_reg$logprice <- log(df_reg$price)

#Setting up test and training datasets
inTrain <- createDataPartition( 
  y = paste(df_1Bed$neighborhood), 
  p = .70, list = FALSE)

df1.training <- df_1Bed[inTrain,] 
df1.test <- df_1Bed[-inTrain,]  

colnames(df)

#Multivariate regression
reg.vars <- c("price",
              #"bedrooms",
              "lagPrice5",
              "lagPrice10",
              "TopTen",
              "TopTwenty",
              #"bars_nn",
              "logbars",
              #"restaurants_nn",
              "logrestaurants",
              "bikestands_nn",
              #"logbikestand",
              #"museums_nn",
              "logmuseum",
              #"parks_nn",
              "logparks",
              "tram_nn",
              #'logtram',
              "universities_nn",
              #"loguniversities",
              "oldtrees_nn",
              #"logoldtrees",
              #"AmsBinnen_nn",
              "logAmsBinnen",
              #"OudZuid_nn",
              "logOudZuid",
              "PlanZuid_nn",
              #"logPlanZuid",
              #"devnhoods_nn",
              "logdevnhoods"
)

reg1Bed <- lm(price ~ ., data = st_drop_geometry(df1.training) %>% 
             dplyr::select(all_of(reg.vars)))

stargazer(
  reg1Bed,
  type = "text",
  title = "Linear Model Summary Table",
  dep.var.caption = " ",
  dep.var.labels = "Model Features")

#Cross validation tests
reg1Bed_predict <- predict(reg1Bed, newdata = df1.test)

rmse.train <- caret::MAE(predict(reg1Bed), df1.training$price)
rmse.test <- caret::MAE(reg1Bed_predict, df1.test$price)

preds.train <- data.frame(pred   = predict(reg1Bed),
                          actual = df1.training$price,
                          source = "training data")
preds.test  <- data.frame(pred   = reg1Bed_predict,
                          actual = df1.test$price,
                          source = "testing data")

preds <- rbind(preds.train, preds.test)

df1.test <-
  df1.test %>%
  mutate(Regression = "1-Bed Regression",
         SalePrice.Predict = predict(reg1Bed, df1.test),
         SalePrice.Error = SalePrice.Predict - price,
         SalePrice.AbsError = abs(SalePrice.Predict - price),
         SalePrice.APE = (abs(SalePrice.Predict - price)) / SalePrice.Predict)

ErrorTable1 <- 
  df1.test %>% 
  dplyr::summarize(Regression = "1-Bed Regression",
                   MAE = mean(SalePrice.AbsError, na.rm = T), 
                   MAPE = mean(SalePrice.AbsError, na.rm = T) / mean(price, na.rm = T)) 

ErrorTable1 %>% 
  st_drop_geometry %>%
  group_by(Regression) %>%
  arrange(desc(MAE)) %>% 
  kable(caption = "MAE and MAPE for Test Set Data") %>% kable_styling()


###2Bed
#Setting up test and training datasets
inTrain <- createDataPartition( 
  y = paste(df_2Bed$neighborhood), 
  p = .70, list = FALSE)

df2.training <- df_2Bed[inTrain,] 
df2.test <- df_2Bed[-inTrain,]  

colnames(df)

#Multivariate regression
reg.vars <- c("price",
              #"bedrooms",
              "lagPrice5",
              "lagPrice10",
              "TopTen",
              "TopTwenty",
              #"bars_nn",
              "logbars",
              #"restaurants_nn",
              "logrestaurants",
              "bikestands_nn",
              #"logbikestand",
              #"museums_nn",
              "logmuseum",
              #"parks_nn",
              "logparks",
              "tram_nn",
              #'logtram',
              "universities_nn",
              #"loguniversities",
              "oldtrees_nn",
              #"logoldtrees",
              #"AmsBinnen_nn",
              "logAmsBinnen",
              #"OudZuid_nn",
              "logOudZuid",
              "PlanZuid_nn",
              #"logPlanZuid",
              #"devnhoods_nn",
              "logdevnhoods"
)

reg2Bed <- lm(price ~ ., data = st_drop_geometry(df2.training) %>% 
                dplyr::select(all_of(reg.vars)))

stargazer(
  reg2Bed,
  type = "text",
  title = "Linear Model Summary Table",
  dep.var.caption = " ",
  dep.var.labels = "Model Features")

#Cross validation tests
reg2Bed_predict <- predict(reg1Bed, newdata = df2.test)

rmse.train <- caret::MAE(predict(reg2Bed), df2.training$price)
rmse.test <- caret::MAE(reg2Bed_predict, df2.test$price)

preds.train <- data.frame(pred   = predict(reg2Bed),
                          actual = df2.training$price,
                          source = "training data")
preds.test  <- data.frame(pred   = reg2Bed_predict,
                          actual = df2.test$price,
                          source = "testing data")

preds <- rbind(preds.train, preds.test)

df2.test <-
  df2.test %>%
  mutate(Regression = "2-Bed Regression",
         SalePrice.Predict = predict(reg2Bed, df2.test),
         SalePrice.Error = SalePrice.Predict - price,
         SalePrice.AbsError = abs(SalePrice.Predict - price),
         SalePrice.APE = (abs(SalePrice.Predict - price)) / SalePrice.Predict)

ErrorTable2 <- 
  df2.test %>% 
  dplyr::summarize(Regression = "2-Bed Regression",
                   MAE = mean(SalePrice.AbsError, na.rm = T), 
                   MAPE = mean(SalePrice.AbsError, na.rm = T) / mean(price, na.rm = T)) 

ErrorTable2 %>% 
  st_drop_geometry %>%
  group_by(Regression) %>%
  arrange(desc(MAE)) %>% 
  kable(caption = "MAE and MAPE for Test Set Data") %>% kable_styling()

###ed
#Setting up test and training datasets
inTrain <- createDataPartition( 
  y = paste(df_3Bed$neighborhood), 
  p = .70, list = FALSE)

df3.training <- df_3Bed[inTrain,] 
df3.test <- df_3Bed[-inTrain,]  

colnames(df)

#Multivariate regression
reg.vars <- c("price",
              #"bedrooms",
              "lagPrice5",
              "lagPrice10",
              "TopTen",
              "TopTwenty",
              #"bars_nn",
              "logbars",
              #"restaurants_nn",
              "logrestaurants",
              "bikestands_nn",
              #"logbikestand",
              #"museums_nn",
              "logmuseum",
              #"parks_nn",
              "logparks",
              "tram_nn",
              #'logtram',
              "universities_nn",
              #"loguniversities",
              "oldtrees_nn",
              #"logoldtrees",
              #"AmsBinnen_nn",
              "logAmsBinnen",
              #"OudZuid_nn",
              "logOudZuid",
              "PlanZuid_nn",
              #"logPlanZuid",
              #"devnhoods_nn",
              "logdevnhoods"
)

reg3Bed <- lm(price ~ ., data = st_drop_geometry(df.training) %>% 
                dplyr::select(all_of(reg.vars)))

stargazer(
  reg3Bed,
  type = "text",
  title = "Linear Model Summary Table",
  dep.var.caption = " ",
  dep.var.labels = "Model Features")

#Cross validation tests
reg3Bed_predict <- predict(reg3Bed, newdata = df.test)

rmse.train <- caret::MAE(predict(reg3Bed), df3.training$price)
rmse.test <- caret::MAE(reg3Bed_predict, df3.test$price)

preds.train <- data.frame(pred   = predict(reg3Bed),
                          actual = df3.training$price,
                          source = "training data")
preds.test  <- data.frame(pred   = reg3Bed_predict,
                          actual = df3.test$price,
                          source = "testing data")

preds <- rbind(preds.train, preds.test)

df3.test <-
  df3.test %>%
  mutate(Regression = "3-Bed Regression",
         SalePrice.Predict = predict(reg3Bed, df3.test),
         SalePrice.Error = SalePrice.Predict - price,
         SalePrice.AbsError = abs(SalePrice.Predict - price),
         SalePrice.APE = (abs(SalePrice.Predict - price)) / SalePrice.Predict)

ErrorTable3 <- 
  df3.test %>% 
  dplyr::summarize(Regression = "3-Bed Regression",
                   MAE = mean(SalePrice.AbsError, na.rm = T), 
                   MAPE = mean(SalePrice.AbsError, na.rm = T) / mean(price, na.rm = T)) 

ErrorTable3 %>% 
  st_drop_geometry %>%
  group_by(Regression) %>%
  arrange(desc(MAE)) %>% 
  kable(caption = "MAE and MAPE for Test Set Data") %>% kable_styling()

rbind(ErrorTable, ErrorTableLog, ErrorTable1, ErrorTable2, ErrorTable3) %>%
  st_drop_geometry() %>%
  kable(caption = "MAE and MAPE for Test Set Data") %>%
  kable_styling()

stargazer(reg1, reglog1, reg1Bed, reg2Bed, reg3Bed, 
          type = "text")

?stargazer
