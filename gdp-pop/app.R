# GDP per capita by state? maybe?
# 2020-Oct-05

library(shiny)
library(heatmaply)
library(shinyHeatmaply)
library(plotly)
library(readr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("GDP / capita ?"),

    # Sidebar with a slider input for number of bins 
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
    
    gdp <- read_csv(url("https://raw.githubusercontent.com/magpiedin/region-pop/main/data/SAGDP1__ALL_AREAS_1997_2019.csv"))
    gdp_states <- gdp[gdp$GeoName %in% state.name & grepl("Current-dollar", gdp$Description),]
    gdp_prep <- gdp_states[,c("GeoName", 2010:2019)]
    
    pop <- read_csv(url("https://raw.githubusercontent.com/magpiedin/region-pop/main/data/state-census-2010-2019.csv"))
    pop_states <- pop[pop$GeographicArea %in% state.name,]
    pop_prep <- pop_states[,c("GeographicArea", 2010:2019)]
    
    gdp_pop <- (gdp_prep[2:NCOL(gdp_prep)]/pop_prep[2:NCOL(pop_prep)])*1000000
    gdp_pop_2 <- as.data.frame(as.matrix(t(gdp_pop)))
    colnames(gdp_pop_2) <- gdp_prep[[1]]
    # gdp_pop_prep <- cbind(gdp_prep[,1], gdp_pop)
    
    output$distPlot <- renderPlotly({  # renderPlot({
        
        
        # x <- gdp_pop_prep  # faithful[, 2]
        # 
        # x_picked <- x[, input$states]
        
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        heatmaply(as.matrix(gdp_pop_2))
        
        # data(mtcars)
        # launch_heatmaply(mtcars)
        
        # d3heatmap(schemas4, # [1:30,],
        #           Rowv = F, Colv = F,
        #           color = brewer.pal (3, "Blues" ) # heat.colors(3)
        # )
        
        # p <- ggplot(x_picked, aes(x = 'Year', y = 'State')) +
        #   #  geom_line(y = input$states)
        #      geom_tile()  + # stat = "identity")  # y = ?)
        #     scale_x_time(name = "Year")
        # 
        # ggplotly(p)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
