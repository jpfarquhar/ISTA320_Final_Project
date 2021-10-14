library(tidyverse)
library(ggplot2)
library(tidyr)
library(usmap)
library(readxl)

world_volcanos <- read_excel("GVP_Volcano_List_Holocene.xls")
glimpse(world_volcanos)
world_volcanos$`Volcano Number` <- as.factor(world_volcanos$`Volcano Number`)

# The above data was aquired from: Global Volcanism Program, 2013. Volcanoes of the World, v. 4.10.2. Venzke, E (ed.). Smithsonian Institution. Downloaded 13 Oct 2021. https://doi.org/10.5479/si.GVP.VOTW4-2013
# and was downloaded as an excel file from the internet. I did a small amount of adjustments within excel before importing the dataset and then worked entirely within RStudio from there.
# The dataset is a list of all of the volcanoes on Earth in the Holocene period each with their own row, with 13 columns of descriptive variables such as Volcano Name, Region, Country, Primary Rock Type, Tectonic Setting, Etc.
# This allowed for the creation of a world map showing the distribution of these volcanoes as well as their tectonic settings, providing a visualization of how and where tectonic movement has created these volcanoes thru time.
# This map also shows how distant regions can actually be very similar, with matching levels and types of volcanic activity creating similar landscapes. 
# I happen to be red/green color deficient ("colorblind") so I used some color codes from the internet that I could see well enough up against each other to differentiate, although with 11 colors needed for my pallette it was a bit tough to find enough.
# I was then able to find the countries with the top 10 most volcanoes from this map and provide a visualization of these quantities against one another, showing how concentrated volcanic activity can sometimes be. 
# The Mt. St. Helens data was aquired from this same website and was also downloaded as an excel file and imported into RStudio.
# I created two line plots showing how the Volcanic Explosivity Index (VEI), a numeric scale that measures the relative explosivity of historic eruptions, of Mt. St. Helens has changed over time.
# The first plot shows eruptions that occurred in BCE years, and how these powerful eruptions were still forming and changing the landscape of our country.
# The second plot shows eruptions all the way up until the most recent one in 2004, and is a visualization of the variability this volcano exhibits. Some years the eruptions are enormous and destructive while other years they are relatively small and not too harmful.
# This is a good way to explain how future eruptions are unpredictable and can be much worse than the three most recent events. 
# Thanks

color_blindPalette <- c("#000000", "#990000", "#000066", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#FFFFFF")
  
tectSet_counts <- NA
  

world_coordinates <- map_data("world")
glimpse(world_coordinates)
worldmap_volc_tectSet <- ggplot() +
  ggtitle("World Map of Holocene Vocanos and Tectonic Settings") +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgreen", size = 0.12
  ) +
  geom_point(
    data = world_volcanos,
    aes(Longitude, Latitude, color = `Tectonic Setting`, size = .1
        ),
    alpha = 0.7
  ) +
  scale_colour_manual(values=color_blindPalette)

worldmap_volc_tectSet

top_countries <- world_volcanos %>%
  group_by(Country) %>%
  summarize(total_obs = n()) %>%
  top_n(10)

glimpse(top_countries)


volcanos_by_country_barplot <- ggplot(top_countries, aes(Country, total_obs, ylab = "Number of Volcanos")) +
  geom_col() +
  labs(title = "Top 11 Countries by Number of Volcanos", x = "Country", y = "Number of Volcanos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = total_obs), vjust = 1.5, colour = "white")
volcanos_by_country_barplot


Mt_St_Helens_Eruptions <- read_excel("Mt_St_Helens_Eruptions.xlsx")

BCE_Mt_St_Helens_Eruptions <- Mt_St_Helens_Eruptions %>%
  filter(Era == "BCE")
BCE_Mt_St_Helens_Eruptions 
CE_Mt_St_Helens_Eruptions <- Mt_St_Helens_Eruptions %>%
  filter(Era == "CE", `Start Date` >= 500)
CE_Mt_St_Helens_Eruptions 

BCE_Mt_St_Helens_lineplot <- ggplot(BCE_Mt_St_Helens_Eruptions, aes(x = `Start Date`, y = VEI)) +
  geom_point() + 
  geom_text(check_overlap = TRUE, aes(label = `Start Date` * -1, hjust = 0.50, vjust = -0.5)) + 
  geom_line() +
  labs(title = "VEI of Mt. St. Helens Eruptions Through Time (BCE)", x = "Year BCE)", y = "VEI")
BCE_Mt_St_Helens_lineplot

CE_Mt_St_Helens_lineplot <- ggplot(CE_Mt_St_Helens_Eruptions, aes(x = `Start Date`, y = VEI)) + 
  geom_point() + 
  geom_text(check_overlap = TRUE, aes(label = `Start Date` * -1, hjust = 0.50, vjust = -0.5)) +
  geom_line() +
  labs(title = "VEI of Mt. St. Helens Eruptions Through Time (CE)", x = "Year CE", y = "VEI")
CE_Mt_St_Helens_lineplot
