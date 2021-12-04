if (!require("ggplot2", quietly = TRUE))
  install.packages("ggplot2")

if (!require("shiny", quietly = TRUE))
  install.packages("shiny")

if (!require("RColorBrewer", quietly = TRUE))
  install.packages("RColorBrewer")

#  The data
library(ggplot2)
library(shiny)
library(RColorBrewer)

dataset <- read.csv('trgn.demo.csv',header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, row.names = 1)
headerNames=colnames(dataset)

ui <- fluidPage(
  pageWithSidebar(
    headerPanel("Data Explorer"),
    sidebarPanel(
      selectInput('x', 'X', c("None"=FALSE,headerNames),headerNames[2]),
      selectInput('y', 'Y', c("None"=FALSE,headerNames),headerNames[3]),
      selectInput('c', 'Colour', c("None"=FALSE,headerNames),headerNames[3]),
      selectInput('size', 'Size', c("None"=FALSE,headerNames),headerNames[4]),
      selectInput('facet_row', 'Facet Row', c(None='.', headerNames)), #Modify the code to only allow choices that are factors to the facet
      selectInput('facet_col', 'Facet Column', c(None='.', headerNames)),
      checkboxInput('geom_point', 'geom_point',TRUE),
      checkboxInput('geom_dotplot', 'geom_dotplot'),
      checkboxInput('geom_bar', 'geom_bar'),
      checkboxInput('geom_violin', 'geom_violin'),
      checkboxInput('geom_histogram', 'geom_histogram'),
      checkboxInput('geom_bin2d', 'geom_bin2d'),
      checkboxInput('geom_density_2d', 'geom_density_2d')
    ),
    mainPanel(
      plotOutput('plot')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot <- renderPlot({
    p <- ggplot(dataset, aes_string(x=input$x, fill=input$c))
    if (input$geom_point)
      p <- p + geom_point(aes_string(x=input$x,y=input$y, color=input$c, size=input$size)) 
    else
      p<-p+geom_point(aes(y=0, size=input$size)) 
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .'){
      p <- p + facet_grid(facets)
    }
    
    if (input$geom_bar)
      p <- p + geom_bar() 
    # geom_dotplot doesn't requires an y input
    if (input$geom_dotplot)
      p <- p + geom_dotplot()
    if (input$geom_histogram)
      p <- p + geom_histogram()
    if (input$geom_bin2d)
      p <- p + geom_bin2d(aes_string(x=input$x,y=input$y, color=input$c), bins=10) 
    if (input$geom_violin)
      p <- p + geom_violin(aes_string(x=input$x,y=input$y, color=input$c)) 
    if (input$geom_density_2d)
      p <- p + geom_density_2d(aes_string(x=input$x,y=input$y, color=input$c)) 
    print(p)
    
  }, height=700)
  
}

# Run the application 
shinyApp(ui = ui, server = server)