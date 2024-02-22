
# Clear workspace
rm(list=ls())
dev.off()

# Load packages
library(CoordinateCleaner)
library(data.table)
library(tidyverse)
library(stringr)
library(dplyr)

# Read in rates data
rates <- read.csv("Supplementary_Data_4_family_rates_raw.csv", h=T)
# Remove families with too small a sample size (<5)
rates <- subset(rates, n > 5)
# Create list of families
families_list <- rates[,"family"]
families_list
rates$family <- as.factor(rates$family)
levels(rates$family)

# Read in and filter species data to match families in list
species <- read.csv("Supplementary_Data_2_Anura_species_means_sd_se_n.csv", h=T)
species <- species %>% filter(family %in% families_list)
species$family <- as.factor(species$family)
levels(species$family)

# Read in latitude data
gbif_anura <- read.csv("Anura.csv", sep = '\t',h=T)

# Filter latitude data to match families in list
gbif_anura <- gbif_anura %>% filter(family %in% families_list)
gbif_anura$family <- as.factor(gbif_anura$family)
levels(gbif_anura$family)
# List families in gbif_anura and compare to those in rates and species
lat_families <- gbif_anura[,"family"]
identical(lat_families, families_list)
families <- intersect(lat_families, families_list)

# A visual inspection showed the family "Strabomantidae" was missing from gbif_anura
# Deleting "Strabomantidae" from species and rates dfs
# Remove case sensitivity by converting the 'family' column to lowercase
species$family <- tolower(species$family)
rates$family <- tolower(rates$family)
# Remove whitespace in family columns
species$family <- trimws(species$family)
rates$family <- trimws(rates$family)
# Filter  rows where family is "Strabomantidae"
species <- species %>%
  filter(family != "strabomantidae")
rates <- rates %>%
  filter(family != "strabomantidae")
# Capitalize the first letter of each word in the 'family' column to match gbif_anura
species$family <- str_to_title(species$family)
rates$family <- str_to_title(rates$family)
# Double check there are 39 levels in each family column
species$family <- as.factor(species$family)
levels(species$family)
rates$family <- as.factor(rates$family)
levels(rates$family)
levels(gbif_anura$family)

# Set coordinates as numeric
gbif_anura$decimalLatitude <- as.numeric(gbif_anura$decimalLatitude)
gbif_anura$decimalLongitude <- as.numeric(gbif_anura$decimalLongitude)
# Remove NAs from coordinates
gbif_anura <- gbif_anura %>% drop_na(decimalLatitude) %>% drop_na(decimalLongitude)
sum(is.na(gbif_anura$decimalLatitude))
sum(is.na(gbif_anura$decimalLongitude))

# Filter gbif_anura to retain only observations from past 20 years
gbif_anura <- gbif_anura %>% filter(year >= 2004)

# Clean coordinates
# Remove data from open ocean
gbif_anura <- cc_sea(
  gbif_anura,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  ref = NULL,
  scale = 110,
  value = "clean",
  speedup = TRUE,
  verbose = TRUE,
  buffer = NULL
)
# Remove data from country capitals
gbif_anura <- cc_cap(
  gbif_anura,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  species = "species",
  buffer = 10000,
  geod = TRUE,
  ref = NULL,
  verify = FALSE,
  value = "clean",
  verbose = TRUE
)
# Remove duplicated data
gbif_anura <- cc_dupl(
  gbif_anura,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  species = "species",
  additions = NULL,
  value = "clean",
  verbose = TRUE
)
# Remove data from country centroids
gbif_anura <- cc_cen(
  gbif_anura,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  species = "species",
  buffer = 1000,
  geod = TRUE,
  test = "both",
  ref = NULL,
  verify = FALSE,
  value = "clean",
  verbose = TRUE
)
# Remove data with 0,0 coordinates
gbif_anura <- cc_zero(
  gbif_anura,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  buffer = 0.5,
  value = "clean",
  verbose = TRUE
)
# Remove data from scientific institutions
gbif_anura <- cc_inst(
  gbif_anura,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  species = "species",
  buffer = 100,
  geod = FALSE,
  ref = NULL,
  verify = FALSE,
  verify_mltpl = 10,
  value = "clean",
  verbose = TRUE
)
# Remove data from on the equator
gbif_anura <- cc_equ(
  gbif_anura,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  test = "absolute",
  value = "clean",
  verbose = TRUE
)
# Remove geographic outliers
gbif_anura <- cc_outl(
  gbif_anura,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  species = "species",
  method = "quantile",
  mltpl = 5,
  tdi = 1000,
  value = "clean",
  sampling_thresh = 0,
  verbose = TRUE,
  min_occs = 7,
  thinning = FALSE,
  thinning_res = 0.5
)

# Saving cleaned dataframe to save time if necessary
write.csv(gbif_anura, "CleanAnura.csv", row.names=FALSE)
gbif_anura <- read.csv("CleanAnura.csv", sep = '\t',h=T)

########
# Merge data from all datasets to create a final version for analysis


# Create new dataframe and calculate average latitude for each family's range
anura_lat <- gbif_anura %>%
  group_by(species, family) %>%
  summarise(decimalLatitude = mean(decimalLatitude, na.rm = FALSE),
            species_count = n())
names(anura_lat)[names(anura_lat) == "mean"] <- "mean_lat"
# Remove missing values
anura_lat$species[anura_lat$species==""] <- NA
anura_lat <- na.omit(anura_lat)
# Change format of species name to align with species df
anura_lat <- anura_lat %>%
  mutate(species = str_replace(species, " ", "_"))
# Remove rows where species count <5
anura_lat <- subset(anura_lat, species_count > 5)


# Create new dataframe
df <- data.frame(species = species$species,
                 family = species$family,
                 microhabitat = species$microhabitat)

# Make sure both dataframes have the same species
splist <- as.vector(df$species)
splat <- as.vector(anura_lat$species)
species_list <- intersect(splist,splat)
anura_lat <- anura_lat %>% filter(species %in% species_list)
df <- df%>% filter(species %in% species_list)
nrow(anura_lat)
nrow(df)


# Merge df with latitude data
df1 <- merge(df,anura_lat,by=c("species","family"), all.x = TRUE)
# Rename some columns
names(df1)[names(df1) == "decimalLatitude"] <- "mean_lat"
names(df1)[names(df1) == "family"] <- "Families"

# Create new rates data frame with only family and bd_rates
rates1 <- data.frame(family = rates$family,
                     bd_rates = rates$bd_rates)
# Merge rates and df1 then apply the bd_rates to df1
merged_df <- merge(df1, rates1, by = "family", all.x = TRUE)
# Fit bd_rate for each species in df1
for (i in 1:nrow(df1)) {
  index <- which(merged_df$species == df1$species[i] & merged_df$family == df1$family[i])
  df1$bd_rates[i] <- merged_df$bd_rates[index]
}

# Get rid of NAs in microhabitat
sum(is.na(df1$microhabitat))
df1 <- df1 %>% drop_na(microhabitat)
nrow(df1)



#######


## Data exploration

# Check for outliers and distribution
hist(df1$bd_rates)
plot(y=1:nrow(df1),x=df1$bd_rates)

# Look for relationships between bd_rates and covariates
# predicting latitude as explanatory variable
M1 <- loess(bd_rates~mean_lat, df1)
P1 <- predict(M1,df1)
plot(x=df1$mean_lat,y=df1$bd_rates)
lines(df1$mean_lat,P1,lwd=5)
cor.test(df1$bd_rates,df1$mean_lat) # cor = 0.162, p-value = 0.0003912
# Variation in microhabitat with bd_rate
boxplot(df1$bd_rates~df1$microhabitat)


#############
## Fitting the model

library(MCMCglmm)

# Run MCMCglmm
as.factor(df1$microhabitat)
model1<-MCMCglmm(bd_rates~mean_lat+microhabitat,
                    family="gaussian", nitt = 100000, burnin =10000,
                 data=df1,verbose=FALSE)

summary(model1) 
# DIC = -2754.099
# Units = 0.000169
plot(model1)

mean(df1$bd_rates) # = 0.04373582
var(df1$bd_rates) # = 0.0002022329


######
## Validating the model

# Check for autocorrelation
autocorr(model1$Sol)
autocorr(model1$VCV)

#########

# Plotting latitude ~ bd_rates
# Predict distribution from the model
lat_pred <- predict(model1, newdata = df1, interval = "prediction")
# Prepare predicted data and link to original data
lat_pred <- as.data.frame(lat_pred)
lat_pred$mean_lat <- df1$mean_lat
lat_pred$bd_rates <- df1$bd_rates

# Plot predictions
ggplot(lat_pred)+
  geom_point(aes(x=mean_lat, y=bd_rates, color = "Observed"), alpha=0.5)+
  geom_smooth(aes(x=mean_lat, y=fit, color = "Predicted"), width= 1.5)+
  xlab("Mean latitude")+
  ylab("Birth-death rate")+
  theme_classic()+
  scale_color_manual(name = "Birth-death rate",  
                     values = c(Predicted = "hotpink", Observed = "black"), 
                     labels = c("Observed", "Predicted")) 
  


# Plotting microhabitat ~ bd_rates
# Predict values from model
hab_pred <- predict(model1, newdata = df1, interval = "prediction")
# Prepare data frame
hab_pred <- as.data.frame(hab_pred)
hab_pred$microhabitat <- df1$microhabitat
# Rename habitats for plot
hab_pred$microhabitat[hab_pred$microhabitat == "arboreal"] <- "Arboreal"
hab_pred$microhabitat[hab_pred$microhabitat == "aquatic-burrowing"] <- "Aquatic/burrowing"
hab_pred$microhabitat[hab_pred$microhabitat == "arb-burrowing"] <- "Arboreal/burrowing"
hab_pred$microhabitat[hab_pred$microhabitat == "aquatic"] <- "Aquatic"
hab_pred$microhabitat[hab_pred$microhabitat == "burrowing"] <- "Burrowing"
hab_pred$microhabitat[hab_pred$microhabitat == "burrowing-semi.arboreal"] <- "Burrowing/semi-arboreal"
hab_pred$microhabitat[hab_pred$microhabitat == "semi.aquatic"] <- "Semi-aquatic"
hab_pred$microhabitat[hab_pred$microhabitat == "semi.arboreal"] <- "Semi-arboreal"
hab_pred$microhabitat[hab_pred$microhabitat == "semi.burrowing"] <- "Semi-burrowing"
hab_pred$microhabitat[hab_pred$microhabitat == "terrestrial"] <- "Terrestrial"
hab_pred$microhabitat[hab_pred$microhabitat == "torrential"] <- "Torrential"


library(hrbrthemes)
# Plot microhabitat~bd_rates with significant microhabitats highlighted
hab_pred  %>% 
  
  # Add a column called Significant and state microhabitats to highlight
  mutate(Significant=ifelse(microhabitat=="Arboreal" | microhabitat=="Semi-aquatic"|
                             microhabitat=="Semi-arboreal"| microhabitat=="Semi-burrowing" |
                               microhabitat =="Terrestrial" | microhabitat=="Torrential", "Yes","No")) %>%
  
  # Build the boxplot
  ggplot( aes(x=microhabitat, y=fit, fill=Significant, colour=Significant, alpha=Significant)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("grey", "hotpink")) +
  scale_color_manual(values = c("black", "hotpink4"))+
  scale_alpha_manual(values=c(0.5,0.7)) +
  theme_classic() +
  xlab("Microhabitat")+
  ylab("Birth-death rate")

