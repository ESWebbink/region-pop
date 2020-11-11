# GDP per capita by state? maybe?
# 2020-Oct-05

library(shiny)
library(heatmaply)
library(shinyHeatmaply)
library(plotly)
library(readr)

# Define UI for application that draws a plot
ui <- fluidPage(

    # App title
    titlePanel("GDP / capita ?"),

    # Sidebar with an input 
    sidebarLayout(
        sidebarPanel(
            selectInput("states",
                        "State (this part...doesn't quite work):",
                        choices = state.name,
                        selected = NULL,
                        multiple = TRUE),
        
            tags$p(),
            tags$h3("Data"),
            tags$p("- U.S. state populations from ",
                   tags$a(href = "https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html",
                          "census.gov")),
            tags$p("- U.S. state GDP from ",
                   tags$a(href = "https://apps.bea.gov/regional/downloadzip.cfm",
                          "bea.gov")),
            tags$br(),
            tags$p(tags$a(href = "https://github.com/magpiedin/region-pop",
                          "code for this 'gdp-pop' app is here"))
            ),

        # Show a plot of the generated distribution
        mainPanel(
            
            tags$h2("Heatmap"),
            tags$p("GDP (in current dollars) per capita per state"),
            tags$p(tags$em("takes a moment to load")),
            tags$br(),
            plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    gdp <- read_csv(url("https://raw.githubusercontent.com/ESWebbink/region-pop/main/data/SAGDP1__ALL_AREAS_1997_2019.csv"))
    gdp_states <- gdp[gdp$GeoName %in% state.name & grepl("Current-dollar", gdp$Description),]
    gdp_prep <- gdp_states[,c("GeoName", 2010:2019)]
    
    gdp_pop <- (gdp_prep[2:NCOL(gdp_prep)]/pop_prep[2:NCOL(pop_prep)])*1000000
    gdp_pop_2 <- as.data.frame(as.matrix(t(gdp_pop)))
    colnames(gdp_pop_2) <- gdp_prep[[1]]
    
      
    output$distPlot <- renderPlotly({  # renderPlot({
        
        heatmaply(as.matrix(gdp_pop_2))

    })
}


# Run the application 
shinyApp(ui = ui, server = server)
