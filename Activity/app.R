ui <- fluidPage(

  # App title ----
  titlePanel("Mammal Distribution by Forset Type"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      selectInput("species", h3("Choose your Species"),
                  choices = c("White-tailed Deer", "Chipmunk", "Coyote", "Fisher", "Raccoon", "Red Squirrel", "Other Small Mammal", "Gray Squirrel", "Black Bear", "Red Fox", "Porcupine", "Bobcat", "Opossum", "Weasel", "Striped Skunk", "Flying Squirrel", "Snowshoe Hare", "River Otter", "Mink"), selected = "White-tailed Deer"),

    ),
    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      leafletOutput("activity")

    )
  )
)


#define server logic ----
server<-function(input, output) {

  output$activity<-renderPlot({

    data<-switch(input$species,
                 "White-tailed Deer" = activity_24h$DEERWHITETAILED,
                 "Chipmunk" = activity_24h$CHIPMUNK,
                 "Coyote" = activity_24h$COYOTE,
                 "Fisher" = activity_24h$FISHER,
                 "Raccoon" = activity_24h$RACCOON,
                 "Red Squirrel" = activity_24h$SQUIRRELRED,
                 "Gray Squirrel" = activity_24h$SQUIRRELGRAY,
                 "Black Bear" = activity_24h$BLACKBEAR,
                 "Red Fox" = activity_24h$FOXRED,
                 "Porcupine" = activity_24h$PORCUPINE,
                 "Bobcat" = activity_24h$BOBCAT,
                 "Weasel" = activity_24h$WEASEL,
                 "Striped Skunk" = activity_24h$SKUNKSTRIPED,
                 "Flying Squirrel" = activity_24h$SQUIRRELFLYING,
                 "Snowshoe Hare" = activity_24h$SNOWSHOEHARE,
                 "River Otter" = activity_24h$RIVEROTTER,
                 "Mink" = activity_24h$MINK,
                 "Other Small Mammal" = activity_24h$OTHERSMALLMAMMAL,
                 "Opossum" = activity_24h$OPOSSUM)

    clock<-c(0:23)
    clock24.plot(data, clock, show.grid = T, lwd = 2, line.col = "blue", cex.lab = 0.5)

  })

}

# Run the app ----
shinyApp(ui = ui, server = server)
