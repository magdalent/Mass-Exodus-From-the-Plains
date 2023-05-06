#Mass Exodus From the Plains
#Data Sources: US Census Beuru, https://www2.census.gov/library/publications/decennial/1940/population-volume-1/33973538v1ch08.pdf
#https://kslib.info/431/County-Data
#https://edd.newmexico.gov/documents/total-population-for-new-mexico-and-counties-1900-to-2010/

library(sf)
library(ggplot2)
library(colorspace)
library(ggspatial)
library(magrittr)
library(dplyr)
library(colorspace)
library(cowplot)
library(ggrepel)
library(extrafont)
library(scales)
library(classInt)
library(extrafont)
font_import()
fonts()
fonttable()
#Read in data
setwd('/Users/magdalenthot/Desktop/Mass Exodus From the Plains')
us.state.boundry <- st_read('cb_2018_us_state_20m/cb_2018_us_state_20m.shp')
us.county.boundry <- st_read('cb_2018_us_state_20m/county.shp')
#open csv file -----------------------------------------------------------------
data <- read.csv('fin-data/data.csv')
# Not all states were effected by the dust bowl so filter to  Kansas, Colorado,  Oklahoma, Texas and New Mexico
plains.state.boundry <- us.state.boundry %>%
    filter(NAME ==  'Kansas'| NAME ==  'Colorado' | NAME ==  'Oklahoma'| NAME ==  'New Mexico' | NAME ==  'Texas')

#Because there is no 'state' atribute in the county data, use the plains.state.boundry to clip the county boundry to states
plains.crop <- st_intersection(plains.state.boundry, us.county.boundry)
plains.data <- merge(plains.crop, data, by = 'NAME.1')

#Black Border Around States
border <- plains.data %>% 
  group_by(NAME.x) %>% 
  summarise() #Makes sure the black bordder only surronds the state boundy

#Make the breaks for the 3 maps
breaks <- classIntervals(c(min(plains.data$pop1920), plains.data$pop1920), n = 6, style = "jenks")
plains.data.1920 <- mutate(plains.data, Population = cut(pop1920, breaks$brks,dig.lab=10)) 

breaks2 <- classIntervals(c(min(plains.data$pop1930)  - .00001, plains.data$pop1930), n = 6, style = "jenks")
plains.data.1930 <- mutate(plains.data, Population3 = cut(pop1930, breaks2$brks,dig.lab=10)) 

breaks3 <- classIntervals(c(min(plains.data$pop1940)  - .00001, plains.data$pop1940), n = 6, style = "jenks")
plains.data.1940 <- mutate(plains.data, Population4= cut(pop1940, breaks3$brks,dig.lab=10)) 


#Create a function to avoid repeating in the 3 maps
map.styles <- function(){
  list(
    theme_void(),
    theme(plot.background = element_rect(fill = 'white', color= 'white')),
    geom_sf(data = border, fill = NA, size = 41, color = "black"),
    labs(caption = "Data Sources: United States Census Bureau, State Library of Kansas, New Mexico Economic Devlopment Department"),
    theme(plot.title = element_text(face = "bold", family = "Verdana", size = 18)),
    labs(subtitle = "These maps explore whether or not there was a mass exodus from the great plains during the Dustbowl \n (1930 – 1936) or if people simply migrated to different areas in the plains. The areas most affected by \n the dustbowl were the western third of Kansas, southeastern Colorado, the Oklahoma Panhandle,  \n the northern two-thirds of the Texas Panhandle, and northeastern New Mexico."),
    theme(plot.caption = element_text(hjust = 0)), 
    theme(plot.subtitle =  element_text(size = 10)),
    theme(text = element_text(family = "Verdana")),
    annotation_scale(
      location = "bl",
      width_hint = 0.2,
      pad_x = unit(1, "cm"),
      pad_y = unit(2, "cm")
    ),
    theme(legend.title = element_text(face = "bold"))
    )
  }

# Plot the 3 years
map.1920 <-  ggplot(plains.data.1920) +
      geom_sf(color = "white", aes(fill = Population)) +
      labs(title = 'Mass Exodus From the Plains: Population in 1920')+
      scale_fill_brewer(palette = "Reds", na.value="gray", 
                        labels = c('37 - 10,886', '10,886 - 24,288', 
                                  '24,288 - 44,423', '44,423 - 82,921',
                                  '82,921 - 152,800', '152,800 - 256,491', 
                                  'No Information')) +
labs(fill = "Population by County, 1920")

map.1920.final <- map.1920 + map.styles()

ggsave("map_1920.png", map.1920.final, height = 7, width = 8, dpi = 320)

map.1930 <- ggplot(plains.data.1930) +
  geom_sf(color = "white", aes(fill = Population3)) +
  scale_fill_brewer(palette = "Reds", na.value="gray",
                    labels = c('195 -  16,216', '16,216 - 39,497', '39,497 - 85,200',
                                '85,200 - 141,211', '141,211 - 221,738',
                               '221,738 - 359,328', 'No Information'))+
ggtitle('Mass Exodus From the Plains: Population in 1930') +
labs(fill = "Population, 1930")

final.map.1930 <- map.1930 + map.styles()


ggsave("map_1930.png", final.map.1930, height = 7, width = 9, dpi = 320)



map.1940 <- ggplot(plains.data.1940) +
  geom_sf(color = "white", aes(fill = Population4)) +
  scale_fill_brewer(palette = "Reds", na.value="gray",
                    labels = c('285 - 14,826', '14,826 - 36,158', '36,158 - 83,202',
                               '83,202 - 193,363', '193,363 - 338,176', '338,176 - 528,961',
                               'No Information')) +
  ggtitle('Mass Exodus From the Plains: Population in 1940') +
  labs(fill = "Population, 1940")


final.map.1940 <- map.1940 + map.styles()
ggsave("map_1940.png", final.map.1940, height = 7, width = 9, dpi = 320)


 
