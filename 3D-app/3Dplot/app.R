library(shiny)
library(plotly)
# setwd("3D-app/3Dplot")

ui <-  fluidPage(
    titlePanel("Movement at three scales"),
    sidebarLayout(
        sidebarPanel(em("Straus et al., 2023, Macroecological constraints on species’ ‘movement profiles’: body mass does not explain it all, for submission to Global Ecology and Biogeography")
        ),
        mainPanel(
            p("Figure 3. log10-transformed movement types on 3-axes for 321 species. 
              Migration values log10+1 transformed to account for non-migratory vertebrates. 
              Use the dropdown menu to select the grouping factor. 
              Dispersal is on the x-axis, migration on the y-axis, and foraging on the z-axis.")
        )
    ),
    selectInput("var",
                label = "Grouping",
                choices = c("Media", "Class", "Trophic Guild"),
                selected = "Class"),
    # column(3,
    #        radioButtons("radio", h3("Radio buttons"),
    #                     choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3"=3), selected = 1)),
    plotlyOutput("plot"))


server <- function(input, output){
    output$plot <- renderPlotly({
        traits <-  read.csv("Traits_final.csv")
        traits$media_simplified[which(traits$media_simplified=="")] <- "combos"

    # pal <- c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF")
    data <- switch(input$var,
                    "Media" = traits$media_simplified,
                    "Class" = traits$class,
                    "Trophic Guild" = traits$diet_broadest_cat)
    
    color <- switch(input$var,
                    "Media" = c("#45681E", "#A3981C", "#CDCA82", "#85B6CE"),
                    "Class" = c(Amphibia = "#00496f", Aves = "#0f85a0", Chondrichthyes = "#edd746", 
                                Mammalia = "#ed8b00", Reptilia = "#dd4124"),
                    "Trophic Guild" = c(Carnivore = "#4a3a3b", Herbivore = "#984136", 
                                        Invertivore = "#c26a7a", Omnivore = "#ecc0a1"))
    
    plot_ly(traits, x = ~log10(dispersal_km), y = ~log10(Migration_km + 1), 
            z = ~log10(hr.radius), 
            color = ~data, 
            colors = ~color, 
            text = ~scientific_name.x,
            width = 900, height = 600) %>% 
        # layout(autosize = F, width = 500, height = 500) %>% 
        add_markers(marker = list(size = 6)) %>% 
        layout(scene = list(xaxis = list(title = 'Dispersal (km)', range = c(-2,4)),
                            yaxis = list(title = 'Migration (km + 1)', range = c(0,4)),
                            zaxis = list(title = 'Foraging radius (km)', range = c(-3,3)),
                            camera = list(eye = list(x=1, y=2, z =2))))
                            # aspectratio = list(x = 1, y = 1, z = 1))
    
    
    })
}

shinyApp(ui, server) # republish from pop up task box; save script before deploying
# rsconnect::deployApp()
