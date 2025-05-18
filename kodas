library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(shiny)
library(leaflet)
library(dplyr)
library(readxl)
library(tidyr)
library(scales)
library(readr)
library(ggplot2)

merged_expenditure_bvp<-read.csv("merged_expenditure_bvp.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")
baltics <- world[world$name %in% c("Lithuania", "Latvia", "Estonia"), ]

ui<- fluidPage( #kuriamas responsyvus UI išdėstymas
  titlePanel("BVP dalis Baltijos šalyse 2012-2021m."), 
  sidebarLayout(sidebarPanel(sliderInput("selected_year","Pasirinkite metus:", #pasirenkame isdestyma, nusistatome, suteikiame pavadinima inputui.
                                          min=min(merged_expenditure_bvp$year), #rezis_1
                                          max=max(merged_expenditure_bvp$year), #rezis_2
                                          value=min(merged_expenditure_bvp$year), #kur pradet?
                                          step=1, #
                                          sep="")),
                              mainPanel(leafletOutput("baltic_map",height=600))) #ismatavimas
) 
funkcija <- function(input, output) { 
  pal <- colorNumeric(
    "OrRd",
    domain = range(merged_expenditure_bvp$proc_bvp_dalis, na.rm = TRUE), #nustatome pagal reiksmes musu
    na.color = "#7393B3"
  ) 

  output$baltic_map <- renderLeaflet({ #generuojamas outputas,corespondina su user inputu
    year_data <- merged_expenditure_bvp %>%
      filter(year == input$selected_year) #inpute saugamas slankiklio info

    baltic_data <- left_join(baltics, year_data, by = c("name" = "country")) #salis sujungiame su duomenimis BVP

    leaflet(data = baltic_data) %>% #pagrazinimai 
      addProviderTiles("Stadia.AlidadeSmoothDark") %>%
      addPolygons(
        fillColor = ~pal(proc_bvp_dalis),
        weight = 1,
        color = "white",
        fillOpacity = 0.7,
        label = ~paste(
          name, ": ",
          ifelse(is.na(proc_bvp_dalis), "NA", paste0(round(proc_bvp_dalis, 2), "%"))
        )
      ) %>%
      addLegend( 
        "bottomright",
        pal = pal,
        values = ~proc_bvp_dalis,  
        title = "BVP dalis",
        opacity = 1,
        labFormat = labelFormat(suffix = "%"),
        na.label = "NA"
      )
  })
}

shinyApp(ui,funkcija) #paleidimas


