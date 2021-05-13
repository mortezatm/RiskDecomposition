library(SOAR)
library(leaflet)
library(DT)
library(ggpubr)
library(dplyr)

Attach()

mapDflt <- leaflet() %>%
        setView(lng = -123.92, lat = 45.993, zoom = 14) %>%
        addPolygons(data  = bldgsPoly,
                    layerId = bldgsPoly$name,
                    label = as.character(bldgsPoly$name),
                    weight = 1,
                    smoothFactor = 0.5,
                    color = "E84A27",
                    opacity = 1.0,
                    fillColor = ~factpal(AreaOut),
                    fillOpacity = 1.0,
                    popup = paste0("<h4>", "ID", "</h4>", bldgsPoly$name),
                    popupOptions = popupOptions(maxWidth ="auto", closeOnClick = TRUE),
                    highlightOptions = highlightOptions(color = "black",
                                                        weight = 2,
                                                        bringToFront = TRUE)
        ) %>%
        addProviderTiles(providers$Stamen.TonerBackground)