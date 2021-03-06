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

################
# DATA WRANGLING
################ 

#Load & project
nhoods <- st_read('C:/Users/eojen/Documents/GitHub/Final Assign/neighbourhoods.geojson', quiet = TRUE)
nhoods.sf <-
  nhoods %>%
  st_as_sf() %>%
  st_transform('EPSG:28992')
apartments <- read.csv('C:/Users/eojen/Documents/GitHub/Final Assign/AmsterdamApts.csv')
airbnb <- read.csv('C:/Users/eojen/Documents/GitHub/Final Assign/listings_details.csv') 
airbnb.sf <- 
  airbnb %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  st_transform('EPSG:28992')
postcodes4 <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=PC4_BUURTEN&THEMA=postcode", quiet = TRUE)
postcodes4.sf <-
  postcodes4 %>%
  st_as_sf(coords = "geometry", crs = 4326, agr = "constant") %>%
  st_transform('EPSG:28992')
postcodes6 <- st_read("https://maps.amsterdam.nl/open_geodata/geojson.php?KAARTLAAG=PC6_VLAKKEN_BAG&THEMA=postcode", quiet = TRUE)
postcodes6.sf <-
  postcodes6 %>%
  st_as_sf(coords = "geometry", crs = 4326, agr = "constant") %>%
  st_transform('EPSG:28992')

#Spatial join

######################
# EXPLORATORY ANALYSIS
######################
#cleaning data
airbnb$price_no <- as.numeric(gsub('[$,]', '', airbnb$price))
airbnb$accommodates_no <- as.numeric(airbnb$accommodates)
airbnb$host_response_rate <- as.numeric(gsub('[%]', '', airbnb$host_response_rate))
airbnb$cleaning_fee_no <- as.numeric(gsub('[$]', '', airbnb$cleaning_fee))

#Dummy variables
airbnb <- 
  airbnb %>%
  mutate(TV = as.numeric(grepl('TV', airbnb$amenities)),
         Wifi = as.numeric(grepl('Wifi', airbnb$amenities)),
         Families = as.numeric(grepl('Family/kid friendly', airbnb$amenities)),
         Garden = as.numeric(grepl('Garden or backyard', airbnb$amenities)),
         Patio = as.numeric(grepl('Patio or balcony', airbnb$amenities)),
         Washer = as.numeric(grepl('Washer', airbnb$amenities)),
         Workspace = as.numeric(grepl('"Laptop friendly workspace', airbnb$amenities)),
         SmokeDetect = as.numeric(grepl('Smoke detector', airbnb$amenities)),
         CO2 = as.numeric(grepl('Carbon monoxide detector', airbnb$amenities)),
         FireEx = as.numeric(grepl('Fire extinguisher', airbnb$amenities)),
         Lighthouse = as.numeric(grepl('Lighthouse', airbnb$property_type)),
  )



as.numeric(grepl('TV', airbnb$amenities))
head(airbnb$amenities)


#Removing outliers
airbnb <- subset(airbnb, bedrooms < 6) 
airbnb <- subset(airbnb, bathrooms < 10) 
airbnb <- subset(airbnb, price_no < 1000)
airbnb <- subset(airbnb, calculated_host_listings_count < 20)
boxplot(airbnb$price_no)

airbnb.sf <- 
  airbnb %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  st_transform('EPSG:28992') 

#Airbnb
View(airbnb.sf)

ggplot() +
  geom_sf(data = nhoods.sf, fill = "grey80") +
  geom_sf(data = airbnb.sf, aes(colour=(price_no)))

ggplot() +
  geom_sf(data = nhoods.sf, fill = "grey80") +
  geom_sf(data = airbnb.sf, aes(colour=bedrooms))

geom_sf(data = nhoods.sf, fill = "grey80") +
  geom_sf(data = airbnb.sf, aes(colour=baths_no))

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
  dplyr::select(room_type, bed_type, Garden, Patio, Families, price_no) %>%
  gather(Variable, Value, -price_no) %>%
  ggplot(aes(Value, price_no)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, scales = "free")+
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))


colnames(airbnb)
airbnb$amenities

#barplot of Neighborhood

airbnb%>% 
  dplyr::select(price,neighbourhood) %>%
  gather(Variable, Value, -price) %>% 
  ggplot(aes(Value, price)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, ncol = 1, scales = "free") +
  labs(title = "Price as a function of\ncategorical variables", y = "Mean_Price") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Need to create dummy variables for the amenities

hist(airbnb.sf$bedrooms)
hist(airbnb.sf$bathrooms)
hist(airbnb$availability_30)
hist(airbnb$host_response_rate)
hist(airbnb$calculated_host_listings_count)
hist(airbnb$cleaning_fee_no)


barplot(height = "neighbourhood", data = airbnb.sf)

?barplot

colnames(airbnb.sf)
geom_point(data = airports, aes(x = lon, y = lat, size = alt),
           fill = "grey", color = "black", alpha = .2)

ggplot(data = nhoods, fill = "grey60") +
  geom_sf(data = airbnb.sf, 
          aes(colour=as.numeric(price), alpha = .2))

head(airbnb.sf)
ggplot() +
  geom_sf(data = nhoods.sf, fill = "grey60") +
  geom_sf(data = sales, aes(colour = q5(SalePrice)), 
          show.legend = "point", size = .75) +
  scale_colour_manual(values = palette5,
                      labels = qBr(sales, "SalePrice"),
                      name = "Home Sale Price\n(Quintile Breaks)") +
  labs(title="Sales Price, Miami",
       subtitle = "Figure 2.1") +
  mapTheme()

plot(airbnb.sf$geometry)
ggplot() +
  geom_sf(data = postcodes4.sf) +
  geom_sf(data = airbnb.sf)

####availablities
airbnb %>%
  summarize(averagetime = mean(as.numeric(availability_30))) %>%
  ggplot(aes(averagetime, fill = averagetime)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Mean priors by race", y = "Mean Priors", x = "Race") +
  scale_fill_manual(values = palette5, name = "Recidivism") +
  plotTheme() + theme(legend.position = "none") 


hist(airbnb$availability_30)
mean(airbnb$availability_30)
hist(airbnb$availability_60)
mean(airbnb$availability_60)
hist(airbnb$availability_90)
mean(airbnb$availability_90)
hist(airbnb$availability_365)
mean(airbnb$availability_365)

#square feet dustribution 
hist(airbnb$square_feet)

#map of prices 
ggplot() +
  geom_sf(data = airbnb.sf, aes(fill = q5(price), color = q5(price))) +
  scale_fill_manual(values = c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641"),
                    name = "Home Sale Price\n(Quintile Breaks)",
                    labels = qBr(airbnb.sf, "price")) +
  scale_color_manual(values = c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641"),
                     name = "Home Sale Price\n(Quintile Breaks)",
                     labels = qBr(airbnb.sf, "SalePrice")) +
  labs(title = "Home Sale Price", subtitle = "Miami, FL") +
  mapTheme() + theme(plot.title = element_text(size=22))

#corr matrix
library(ggcorrplot)

corr_matrix2 <- airbnb %>%
  dplyr::select(price,
                reviews_per_month, 
                cleaning_fee,
                minimum_nights,
                guests_included, 
                maximum_nights, 
                number_of_reviews,
               review_scores_rating,
                reviews_per_month,
                availability_30,
               availability_60,
               availability_90,
               availability_365,
                extra_people,
                accommodates,
                bathrooms,
               bedrooms,
                beds,
               host_listings_count,
                square_feet) %>%
  na.omit()

cor(corr_matrix2 , use="pairwise.complete.obs")







ggcorrplot(
  round(cor(corr_matrix2), 1), 
  p.mat = cor_pmat(corr_matrix2),
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables") 

###super host categories 
airbnb %>%
  dplyr::select(price,host_is_superhost) %>%
  gather(Variable, value, -host_is_superhost) %>%
  na.omit()%>% ###did nothing 
  ggplot(aes(host_is_superhost, value, fill=host_is_superhost)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") + 
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Mean Prices based on SuperHost Status", y = "Mean_Price") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mean Price for each Property Type
airbnb %>%
  dplyr::select(price,property_type) %>%
  gather(Variable, value, -property_type) %>%
  na.omit()%>% ###did nothing 
  ggplot(aes(property_type, value)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") + 
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Mean Prices based on Property Type", y = "Mean_Price") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))


airbnb%>% 
  dplyr::select(price,neighbourhood) %>%
  gather(Variable, Value, -price) %>% 
  ggplot(aes(Value, price)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, ncol = 1, scales = "free") +
  labs(title = "Price as a function of\ncategorical variables", y = "Mean_Price") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

####plot to see where lighthouses are

airbnb.sf <- 
  airbnb.sf %>%
  mutate(Lighthouse = as.numeric(grepl('Lighthouse', airbnb.sf$property_type)),)

ggplot() + 
  geom_sf(data = nhoods.sf, fill = "grey80") +
  geom_sf(data = airbnb.sf, aes(colour=Lighthouse)) #only one wow 



