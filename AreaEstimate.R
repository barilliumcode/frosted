# Smith, W. (2026). Change in L. perennis area over time. GitHub. https://github.com/barilliumcode/frosted/blob/c0bb76942e0cf59ed92c1313aea7bbcdeaf588ce/AreaEstimate.R

# Adapted from Kimball-Rhines, C. (2026). Transect Point-Intercept Population Estimate. GitHub. 
# https://github.com/coopermkr/lperennisGBS/blob/d9bb4616f2f687b9945c2e0dca04a4eedac4a256/scripts/transectEstimate.R

# Load libraries
install.packages("tidyverse")
install.packages("performance")
install.packages("broom")
library(tidyverse)
library(performance)
library(broom)

transects <- read_csv("combinedTransectArea.csv") |>
  # Combine 2010 and 2011 which together make a complete set of transects
  mutate(YEAR = replace(YEAR, YEAR %in% c(2010, 2011), 2010.5),
         YEAR = replace(YEAR, YEAR %in% c(2017, 2018), 2017.5))

# Which sites were surveyed in which years?
transects |> group_by(YEAR) |> reframe(sites = unique(SITE)) |>
  ggplot(mapping = aes(x = as.factor(YEAR), y = sites)) +
  geom_tile()+
  theme_classic()

meta <- read_csv("transectMeta.csv") |> 
  mutate(totalLength = TRANSECT_LENGTH * 100 * N_TRANSECTS,
         siteArea = BASELINE_LENGTH * 100 * TRANSECT_LENGTH * 100) |>
  select(SITE, N_TRANSECTS, TRANSECT_LENGTH, BASELINE_LENGTH, totalLength, siteArea)

# Filter to relevant info
transectSizeFilt <- transects |>
  filter(SPP %in% c("LUPE", "LP", "lp")) |>
  mutate(TRANSECT = str_split_i(INTERCEPT, pattern = "-", 1), # Pull transect from intercept column
         Area = as.numeric(Area)) |> # Fix data type of intercept
  filter(!is.na(Area),
         Area > 0) |> # Filter out missing intercept data
  select(SITE, YEAR, TRANSECT, Area) # Only keeping columns relevant to this calculation

# calculate approx. area of each lupine patch in new column (assuming oval shape)
# area of ellipses = length x width x pi (calculated in excel - too many errors in R)

df<-transectSizeFilt |>
  group_by(YEAR, SITE)|>
  summarize(sumArea = sum(Area))

# Graph area per year (discrete) 
# Shows median area with range of areas from CZs
options(scipen = 999) #fixes y axis values
df |>
  ggplot(mapping = aes(x = as.factor(YEAR), y = sumArea)) +
  geom_boxplot()+
  theme_classic() +
  labs(x = "Year", y = "Total Area of Sundial Lupine (cm^2)")

# Change in Population Size
#' The problem with this is that not all sites were surveyed every year. If sites
#' were skipped because they were developed/because plants do not grow there anymore, 
#' then the years are comparable. But if they were skipped by mistake, then they are not.

# Graph area per year (continuous)
# Shows all points
df |>
  ggplot(mapping = aes(x = YEAR, y = sumArea)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_classic() +
  labs(x = "Year", y = "Total Area of Sundial Lupine (cm^2)")+
  ylim(0,800000)

# Assuming skipped sites had zero plants and 2014 was a fluke:

# Sum the total plant area from each CZ
yearTotal <- df |> group_by(YEAR) |>
  summarize(TOTAL = sum(sumArea))
# Graph total plant area over time
yearTotal |>
  ggplot(mapping = aes(x = YEAR, y = TOTAL)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_classic() +
  labs(x = "Year", y = "Total Area of Sundial Lupine (cm^2)")

# Create model shown in the graph
estLM <- yearTotal |>
  filter(YEAR != 2014) %>%
  lm(formula = N ~ YEAR, data = .)

# Graphing the plant area of each CZ over time
#------------------------------------------------------ 
options(scipen=999)
## Site 1
CZ1Adf<-subset(df, SITE=="CZ1A")
CZ1Adf |>
  ggplot(mapping = aes(x = YEAR, y = sumArea)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_classic(base_size = 16) +
  labs(x = "Year", y = "Plant Area")+
  ylim(0,2500000)

##Site 2
CZ2Adf<-subset(df, SITE=="CZ2A")
CZ2Adf |>
  ggplot(mapping = aes(x = YEAR, y = sumArea)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_classic(base_size = 16) +
  labs(x = "Year", y = "Plant Area")+
  ylim(0,2500000)

##Site 3
CZ2Bdf<-subset(df, SITE=="CZ2B")
CZ2Bdf |>
  ggplot(mapping = aes(x = YEAR, y = sumArea)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_classic(base_size = 16) +
  labs(x = "Year", y = "Plant Area")+
  ylim(0,2500000)

##Site 4
CZ3Adf<-subset(df, SITE=="CZ3A")
CZ3Adf |>
  ggplot(mapping = aes(x = YEAR, y = sumArea)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_classic(base_size = 16) +
  labs(x = "Year", y = "Plant Area")+
  ylim(0,2500000)

##Site 5
CZ4Adf<-subset(df, SITE=="CZ4A")
CZ4Adf |>
  ggplot(mapping = aes(x = YEAR, y = sumArea)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_classic(base_size = 16) +
  labs(x = "Year", y = "Plant Area")+
  ylim(0,2500000)

##Site 6
CZ4Ddf<-subset(df, SITE=="CZ4D")
CZ4Ddf |>
  ggplot(mapping = aes(x = YEAR, y = sumArea)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_classic(base_size = 16) +
  labs(x = "Year", y = "Plant Area")+
  ylim(0,2500000)

##Site 7
CZ4Edf<-subset(df, SITE=="CZ4E")
CZ4Edf |>
  ggplot(mapping = aes(x = YEAR, y = sumArea)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_classic(base_size = 16) +
  labs(x = "Year", y = "Plant Area")+
  ylim(0,2500000)

##Site 8
CZ4Fdf<-subset(df, SITE=="CZ4F")
CZ4Fdf |>
  ggplot(mapping = aes(x = YEAR, y = sumArea)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_classic(base_size = 16) +
  labs(x = "Year", y = "Plant Area")+
  ylim(0,2500000)

