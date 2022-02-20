# Attach packages
library(raster)
library(jpeg)
library(ggpubr)
library(sf)
library(rgdal)
library(shiny)
library(tidyverse)
library(shinythemes)
library(dplyr)
library(here)
library(readxl)
library(janitor)
library(sf)
library(bslib)
library(thematic)


# read in data set that is in .xlsx format
butterflies <-  readxl::read_xlsx(here("data", "WMTC-Data-1997-2020_1.12.2021.xlsx")) %>% 
  clean_names() %>% 
  rename('1997' = 'x1997', '1998' = 'x1998','1999' = 'x1999','2000' = 'x2000','2001' = 'x2001','2002' = 'x2002','2003' = 'x2003','2004' = 'x2004','2005' = 'x2005','2006' = 'x2006','2007' = 'x2007','2008' = 'x2008','2009' = 'x2009','2010' = 'x2010','2011' = 'x2011','2012' = 'x2012','2013' = 'x2013','2014' = 'x2014','2015' = 'x2015','2016' = 'x2016','2017' = 'x2017','2018' = 'x2018','2019' = 'x2019','2020' = 'x2020') %>% 
  pivot_longer(cols = c('1997':'2020'),
               names_to = 'year',
               values_to = 'count') %>% 
  filter(county != "Total reported:",
         county != "No. Counts:",
         county != "Average:",
         county != "Standard Error:",
         county != "Citation:",
         county != "Baja California") %>% 
  filter(!is.na(site_id)) %>% 
  arrange(county) %>% 
  group_by(county,year) %>% 
  mutate(total = sum(count, na.rm = TRUE))

# total count
butterflies2 <- butterflies %>% 
  na.omit(count) %>% 
  group_by(year) %>% 
  mutate(total_year = sum(count)) 


# other data set
monarch <- read_csv(here("data/wmmm_20210122_204446_922952/wmmm_20210122_204446_922952.csv")) %>%
  clean_names() %>% 
  select(state, record_year, latitude, longitude, date_identified, county, state, datum, genus, species_sub_species) %>% 
  filter(genus == 'Danaus') %>%
  filter(record_year == '2020') %>% 
  drop_na(c(latitude, longitude)) %>% 
  arrange(record_year) %>% 
  drop_na(state) %>%
  group_by(county) %>% 
  mutate(count = n())


# convert to sf object
monarch_sf <- st_as_sf(monarch, coords = c("longitude", "latitude"),  crs='WGS84')


# read california counties
california <- read_sf(here("ca_counties"), layer = "CA_Counties_TIGER2016") %>% 
  clean_names() %>% 
  select(name)


#read in geospatial data for tab 5 (map)
milkweeds <- read.csv(here("data", "wmmm_20210122_204446_922952", "wmmm_20210122_204446_922952.csv")) %>% 
  clean_names()


#pull in state shape file and change projection to WGS84
us_states <- read_sf(here("data", "state_boundaries", "tl_2020_us_state.shp"))
us_states <- st_transform(us_states, 4326)


#data frame for widget 3 (Teague) Does not need a data frame


#data frame for widget 4 (Teague)


# select species, state, latitude, longitude, and datum then filter out monarch butterflies and genus-level identifications, datum must be WGS84

milkweed_combined <- milkweeds %>% 
  unite (genusspecies, genus, species_sub_species, sep = " ") %>% 
  dplyr::select (genusspecies, latitude, longitude, datum) %>% 
  filter (genusspecies != ("Asclepias spp."),
          genusspecies != ("Asclepias"),
          datum == "WGS84")



# find top 8 most populous milkweed for tab 3 and 4
milkweedtop8 <- milkweed_combined %>% 
  group_by(genusspecies) %>% 
  count() %>% 
  filter( n > 100) 


# select species top 8 species from milkweed_combined; make column name column for them
milkweedtop8_gis <- milkweed_combined %>% 
  filter(genusspecies == "Asclepias speciosa"| 
           genusspecies == "Asclepias fascicularis"|
           genusspecies == 'Asclepias asperula'|
           genusspecies == 'Asclepias curassavica'|
           genusspecies == 'Asclepias subverticillata'|
           genusspecies == 'Asclepias eriocarpa'|
           genusspecies == 'Asclepias cordifolia'| 
           genusspecies == 'Asclepias californica' |
           genusspecies == "Danaus plexippus") %>% 
  mutate(common_name = genusspecies)

#switch scientific names to common names in the common names column
milkweedtop8_gis$common_name[milkweedtop8_gis$common_name == "Asclepias speciosa"] <- "Showy milkweed"
milkweedtop8_gis$common_name[milkweedtop8_gis$common_name == "Asclepias fascicularis"] <- "Narrow leaf milkweed"
milkweedtop8_gis$common_name[milkweedtop8_gis$common_name == "Asclepias asperula"] <- "Spider milkweed"
milkweedtop8_gis$common_name[milkweedtop8_gis$common_name == "Asclepias curassavica"] <- "Bloodflower"
milkweedtop8_gis$common_name[milkweedtop8_gis$common_name == "Asclepias subverticillata"] <- "Horsetail milkweed"
milkweedtop8_gis$common_name[milkweedtop8_gis$common_name == "Asclepias eriocarpa"] <- "Woolypod milkweed"
milkweedtop8_gis$common_name[milkweedtop8_gis$common_name == "Asclepias cordifolia"] <- "Heartleaf milkweed"
milkweedtop8_gis$common_name[milkweedtop8_gis$common_name == "Asclepias californica"] <- "California milkweed"
milkweedtop8_gis$common_name[milkweedtop8_gis$common_name == "Danaus plexippus"] <- "Monarch butterfly"

# convert data frame into sf
milkweed_sf <- sf::st_as_sf(milkweedtop8_gis, 
                            coords = c(x = "longitude", y = "latitude"),
                            crs = 4326) %>% 
  dplyr::select(common_name)


#select data for states
us_selected <- us_states %>% 
  dplyr::select(REGION, NAME) %>% 
  filter(REGION == 4,
         NAME != "Alaska",
         NAME != "Hawaii") 


# custom theme
monarch_theme <- bs_theme(
  bg = "#F9F9F9", 
  fg = "#191919", 
  primary = "#FDB912",
  secondary = "#FDB912",
  base_font = font_google("Karla"),
  heading_font = font_google("Coustard")
)


# Create the user interface:
ui <- fluidPage(
  theme = monarch_theme,
  titlePanel("Western Monarch Butterfly Conservation"),
  navbarPage("ESM 244 Term Project",
          
             
             # Tab 1: introduction
             tabPanel("About this app",
                      mainPanel("This app vizualises the decline of Western Monarch butterfly populations along the California coast and demonstrates the spatial ranges of native plants which butterflies need for food and habitat. The goal of this app is to inform people who want to help conserve Monarch butterflies what plants should be grown in their area to help the species.",            
             br(),
             br(),
             "The following graph shows the decline in counts for the butterfly. The data supporting the graph of popultion size is from the Western Monarch Thanksgiving Count dataset. This is an annual count taking place at various sites across California. This graph is aggregated across all monitoring site in and around California to present the total popualtion.",            
             br(),
                                  plotOutput("total_count_plot"),
             br(),
             br(),
             "According to the Xerces Society for Invertebrate Conservation, there were once millions of monarchs that migrated along the Western coast of the United States and Mexico. It’s estimated there were up to 4.5 million butterflies migrating in the 1980s, but today there’s only 200,000 to 300,000 butterflies (Western Monarch Call to Action, 2020). In 2018 and 2019, volunteers of the annual Western Monarch Thanksgiving Count (WMTC) counted less than 30,000 individuals; in 2020 this dwindled down to less than 2,000 (Western Monarch Call to Action, 2020). Today we experience a California that has a fraction of the Monarch butterflies that used to migrate. Someday soon we may even see the Western Monarch’s migration in California stop completely.",
             br(),
             br(),
             "The sudden decline of the population is still being researched and understood, but there are some recommendations provided by the Xerces Society. The purpose of this app is to present this organization’s information and visualize data about the butterflies to encourage conservation and inspire Californians with ways we can help.",
             br(),
             br(),
             "One major way Califonians can help is by planting native species which Monarch’s need (Western Monarch Call to Action, 2020). Milkweeds are important for Monarchs, as they serve as the “host plant for caterpillars” (State of the monarch butterfly overwintering sites). For this reason, the second half of this app provides information about milkweeds and where they have been found to grow. Even with this information, however, it’s important to check with local experts what species are best suited to your area.",
             br(),
             br(),
             "Data was also collected from the Western Monarch and Milkweed Occurance Database (1900-Present), which compiled observations of monarch butterflies and milkweed across the western United States (AZ, CA, CO, ID, MT, NM, NV, OR, UT, WA, WY). This information is presenting in the spatial graph in tab 5")),


             # Tab 2
             tabPanel("Threats to Western Monarch Butterflies",
                      sidebarLayout(
                        sidebarPanel("Learn more about the threats that Western Monarchs face.",
                                     selectInput(
                                        inputId = "monarch_checkbox",
                                        label = "Select an option:", 
                                        choices = c("Pesticides","Loss of habitat","Threats to preserves"),
                                      )
                        ),
                        mainPanel("Threats to the species",
                                  textOutput("intro_reactive"),
                                  uiOutput("intro")
                        ) ) ),
             

              # Tab 3
              tabPanel("Monarch Butterfly Counts Over Time",
                      sidebarLayout(
                        sidebarPanel("See how population counts change over time for California counties.",
                          selectInput(inputId = "pick_county", 
                                                  label = "Select county", 
                                                  choices = unique(butterflies$county)
                                      )
                        ),
                        mainPanel("Monarch Butterfly Counts over time",
                                   plotOutput("butterfly_count_plot"))
                      )),


            #tab 4 (Teague)
            tabPanel("Learn About Milkweed Species",
                     sidebarLayout(
                       sidebarPanel( "Select Species:",
                         radioButtons(
                         inputId = "plant_radiobutton",
                         label = "Select a Milkweed Species", 
                         choices = c("Showy milkweed", "Narrow leaf milkweed", "Spider milkweed", "Bloodflower", "Horsetail milkweed", "Woolypod milkweed", "Heartleaf milkweed", "California milkweed"), 
                         inline = TRUE
                       )
                     ),
                     mainPanel(" ",
                               textOutput("text_reactive"),
                               br(),
                               br(),
                               uiOutput("image"),
                               textOutput("source_reactive"),
                               br(),
                               br(),
                               "*Do not plant milkweed within 5-10 miles (8-16km) from coastal areas where it did not historically occur. It is recommended to research specific native milkweeds and contact local experts in your area before planting."
                     ) ) ),
                     
            
            #tab 5 (milkweed/monarch map)
            tabPanel("Milkweed Species and Monarch Distribution in Western States",
                      sidebarLayout(
                        sidebarPanel("Select Species:",
                                     checkboxGroupInput(inputId = "pick_species",
                                                        label = "",
                                                        choices = unique(milkweedtop8_gis$common_name)
                                     )
                                    ),
                        mainPanel("Map of Monarch Butterflies and Milkweed Species Distribution",
                                  br(),
                                  br(),
                                  plotOutput("milkweed_plot")
                                     )
                      )
             ),
            
            #tab 6 (Citations)
            tabPanel("Citations",
                     mainPanel("Armon, CC BY-SA 3.0 <http://creativecommons.org/licenses/by-sa/3.0/>, via Wikimedia Commons",
                               br(),
                               br(),
                               "Brocken Inaglory, CC BY-SA 3.0 <https://creativecommons.org/licenses/by-sa/3.0>, via Wikimedia Commons",
                               br(),
                               br(),
                               "Calflora. (2018). Asclepias speciosa Calflora. Retrieved February 17, 2021, from https://www.calflora.org/cgi-bin/species_query.cgi?where-calrecnum=751",
                               br(),
                               br(),
                               "Calflora. (2018). Asclepias fascicularis Calflora. Retrieved February 17, 2021, from https://www.calflora.org/cgi-bin/species_query.cgi?where-taxon=Asclepias+fascicularis",
                               br(),
                               br(),
                               "Calflora. (2007). Asclepias curassavica Calflora. Retrieved February 17, 2021, from https://www.calflora.org/cgi-bin/species_query.cgi?where-calrecnum=744",
                               br(),
                               br(),
                               "Calflora. (2018). Asclepias asperula Calflora. Retrieved February 17, 2021, from https://www.calflora.org/cgi-bin/species_query.cgi?where-calrecnum=9703",
                               br(),
                               br(),
                               "Calflora. (2020). Asclepias eriocarpa Calflora. Retrieved February 17, 2021, from https://www.calflora.org/cgi-bin/species_query.cgi?where-calrecnum=745",
                               br(),
                               br(),
                               "Calflora. (2016). Asclepias cordifolia Calflora. Retrieved February 17, 2021, from https://www.calflora.org/cgi-bin/species_query.cgi?where-calrecnum=741",
                               br(),
                               br(),
                               "Calflora. (2013). Asclepias californica Calflora. Retrieved February 17, 2021, from https://www.calflora.org/cgi-bin/species_query.cgi?where-calrecnum=739",
                               br(),
                               br(),
                               "'California Pollinator Plants: Native Milkweeds (Asclepias spp.). (2011). The Xerces Society for Invertebrate Conservation. United States Department of Agriculture. Natural Resources Conservation Service. Monarch Joint Venture.",
                               br(),
                               br(),
                               "<https://creativecommons.org/licenses/by-sa/4.0>, via Wikimedia Common",
                               br(),
                               br(),
                               "KATHERINE WAGNER-REISS, CC BY-SA 4.0 <https://creativecommons.org/licenses/by-sa/4.0>, via Wikimedia Commons.",
                               br(),
                               br(),
                               "Pelton, E., Jepsen, S., Schultz, C., Fallon, C., & Black, S. H. (2016). State of the Monarch Butterfly Overwintering Sites in California. www.xerces.org",
                               br(),
                               br(),
                               "Western Monarch and Milkweed Occurrence Database. 2017. Data accessed from the Western Monarch Milkweed Mapper, a project by the Xerces Society, U.S. Fish and Wildlife Service, Idaho Department of Fish and Game, and Washington Department of Fish and Wildlife. Available: www.monarchmilkweedmapper.org. Accessed: [Februrary 2, 2021].",
                               br(),
                               br(),
                               "'Western Monarch Call to Action.' (updated 2021). The Xerces Society for Invertebrate Conservation.",
                               br(),
                               br(),
                               "Western New Mexico University Department of Natural Sciences. (2009). Vascular plants of the gila wilderness. Retrieved March 08, 2021, from https://www.wnmu.edu/academic/nspages/gilaflora/asclepias_subverticillata.html",
                               br(),
                               br(),
                               "Xerces Society Western Monarch Thanksgiving Count. 2021. Western Monarch Thanksgiving Count Data, 1997-2020. Availa"
                               )) 
  )
  )


# Create the server function:

# Server input
server <- function(input, output) {

  
# tab 1: introduction 
  output$total_count_plot <- renderPlot({
    ggplot(data = butterflies2, aes(x = year, y = total_year)) +
      geom_point(color = "darkorange1", size = 3) +
      labs(y = "total counts") +
      theme(legend.position = 'none', axis.text.x = element_text(angle = 45))
    
  })
  
  
# tab 2 (Threats to Western Monarch Butterflies)
output$intro_reactive <- reactive({case_when(
  input$monarch_checkbox == "Pesticides" ~ "Protecting monarchs from pesticides was noted as one of the five key steps to helping the Western Monarch migration in California by the Xerces Society. Pesticides are harmful to Monarch butterflies; they include both herbicide and insecticide used on farms and at homes (Western Monarch Call to Action, 2020). Photo Credit: Albarubescens, CC BY-SA 4.0 <https://creativecommons.org/licenses/by-sa/4.0>, via Wikimedia Commons. Ladybugs can be an organic alternative to pest control.",
  input$monarch_checkbox == "Loss of habitat" ~ "Habitat loss is a major contributor to extinction for any species. Californians are urged to plant nectar plants and milkweeds to provide habitat for butterflies on their migration routes (Western Monarch Call to Action, 2020). Individuals can also urge local authorities to plant these species in public areas. Photo Credit: Armon, CC BY-SA 3.0 <http://creativecommons.org/licenses/by-sa/3.0/>, via Wikimedia Commons",
  input$monarch_checkbox == "Threats to preserves" ~ "Overwintering sites are located all along the California coast, and they are where adult Monarch butterflies aggregate in the late fall until the beginning of spring (State of the Monarch Butterfly Overwintering Sites). They are threatened by development and improper management. Photo Credit: Brocken Inaglory, CC BY-SA 3.0 <https://creativecommons.org/licenses/by-sa/3.0>, via Wikimedia Commons")
  
})

output$intro <- renderUI({
  if(input$monarch_checkbox == "Pesticides"){img(src = "images/ladybug.jpg", height="50%", width="50%", align="left")}
  else if (input$monarch_checkbox == "Loss of habitat"){img(src= "images/chrysalis.jpg", height="50%", width="50%", align="left")}
  else if (input$monarch_checkbox == "Threats to preserves"){img(src= "images/overwintering.jpg", height="50%", width="50%", align="left")}
})
  

# tab 3 (Monarch Butterfly Counts Over Time in California)
  site_counts <- reactive({
    butterflies %>% 
      filter(county == input$pick_county)
  })
  output$butterfly_count_plot <- renderPlot({
    ggplot(data = site_counts(), aes(x = year, y = total)) +
      geom_col(fill = "darkorange1") +
      theme(legend.position = 'none', axis.text.x = element_text(angle = 45))
  })
  
  # tab 4 (learn more about milkweed species)
  output$text_reactive <- reactive({case_when(
    input$plant_radiobutton == "Showy milkweed" ~ "Showy milkweeds (Asclepias speciosa) are the most common native milkweed species in the western United States and thus, are generally recommended to be grown in this region. They can be found in wetlands, yellow pine forests, and mixed evergreen forests. Their bloom period is June through July. (Calflora, 2018)",
    input$plant_radiobutton == "Narrow leaf milkweed" ~ "Narrow leave milkweed (Asclepias fascicularis) are California natives that can be found in a variety of ecosystems, such as red fir forests, lodgepole forests, chaparral, and wetlands. They are exceptional larval host plants for Monarch butterflies and is recommended to be grown in California (Calscape, 2009).",
    input$plant_radiobutton == "Spider milkweed" ~ "Spider milkweed (Asclepias fasciculalris) are California natives that have a blooming period of May through September. They serve as plant hosts for larval Monarch Butterflies and greatly benefit native bees as well.",
    input$plant_radiobutton == "Bloodflower" ~ "Although bloodflowers (Asclepias curassavica), also known as the Mexican Butterfly Weed, provide food to Monarch Butterflies, they are invasive in Northern America and have been shown to cause increased parasite load onto monarch butterflies. They are also poisonous to humans so growing these plants in the western United States is not recommended.  (Califlora, 2007)",
    input$plant_radiobutton == "Horsetail milkweed" ~ "Horsetail milkweeds (Asclepias subverticillata) are endemic to Northern America and are known to be toxic to cattle, goats, horses, and sheep when ingested. It is not recommended to grow them within the reach of livestock. (Western New Mexico University Department of Natural Sciences (2009)",
    input$plant_radiobutton == "Woolypod milkweed" ~ "Woolypod milkweeds (Asclepias eriocarpa) are native to western North America and have a blooming period of March through July. They can be found in the slopes of ecosystems, such as yellow pine forests, mixed evergreen forests, and chaparral (Califlora, 2020)",
    input$plant_radiobutton == "Heartleaf milkweed" ~ "Heartleaf milkweeds (Asclepias cordifolia) are native to western North America and have a blooming period of June through August. They can be found in ecosystems, such as yellow pine forests, red fir forests, chaparral and grasslands. (Califlora, 2016)", 
    input$plant_radiobutton == "California milkweed" ~ "As its name suggests, the California milkweed (Asclepias californica) is a California native plant. It has a blooming period of April through July and can be found in the slopes of yellow pine forests, chaparral, and pinyon-juniper woolands. (Califlora, 2013)")
  })
  output$source_reactive <- reactive({case_when(
    input$plant_radiobutton == "Showy milkweed" ~ "(calflora.org, 2018)",
    input$plant_radiobutton == "Narrow leaf milkweed" ~ "(calflora.org, 2018)",
    input$plant_radiobutton == "Spider milkweed" ~ "(calflora.org, 2018)",
    input$plant_radiobutton == "Bloodflower" ~ "(calflora.org, 2007)",
    input$plant_radiobutton == "Horsetail milkweed" ~ "(Western New Mexico University Department of Natural Sciences, 2009)",
    input$plant_radiobutton == "Woolypod milkweed" ~ "(calflora.org, 2020)",
    input$plant_radiobutton == "Heartleaf milkweed" ~ "(calflora.org, 2016)", 
    input$plant_radiobutton == "California milkweed" ~ "(calflora.org, 2013)")
  })
  output$image <- renderUI({
    if(input$plant_radiobutton == "Showy milkweed"){img (src = "images/a_spec.jpg", height="50%", width="50%")}
    else if (input$plant_radiobutton == "Narrow leaf milkweed"){img(src= "images/a_fasc.jpg", height="50%", width="50%")}
    else if (input$plant_radiobutton == "Spider milkweed"){img(src= "images/a_asp.jpeg", height="50%", width="50%")}
    else if (input$plant_radiobutton == "Bloodflower"){img(src= "images/a_cura.jpeg", height="50%", width="50%")}
    else if (input$plant_radiobutton == "Horsetail milkweed"){img(src= "images/a_subv.jpg", height="50%", width="50%")}
    else if (input$plant_radiobutton == "Woolypod milkweed"){img(src= "images/a_erio.jpg", height="50%", width="50%")}
    else if (input$plant_radiobutton == "Heartleaf milkweed"){img(src= "images/a_cord.jpg", height="50%", width="50%")}
    else if (input$plant_radiobutton == "California milkweed"){img(src= "images/a_cali.jpeg", height="50%", width="50%")}
  })
  

  #tab 5 (milkweed + monarch map)
  #get input from ui
  milkweed_reactive <- reactive({
    milkweed_sf %>% 
      filter(common_name %in% input$pick_species)
  })
  #graph
  output$milkweed_plot <- renderPlot(
    ggplot() + 
      geom_sf(data = us_selected) +
      coord_sf(xlim = c(-126, -102), ylim = c(25,50), expand = FALSE) +
      geom_sf(data = milkweed_reactive(), aes(color = common_name)) + 
      labs(color = "Species")
  )
}

#tab 6 (citations)


# call thematic_shiny()
# thematic_shiny()
thematic_off()

# Combine them into an app:
shinyApp(ui = ui, server = server)