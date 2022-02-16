library(shiny)
require(shinyWidgets)
library(plotly)

source('shinyProbDistFunc.R')

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        ".MathJax {
              font-size: 5em !important;
              color: black;
            }
            #alphaDisplayNormID {
              text-align: center;
              font-size: 2em;
              font-style: italic;
              font-weight: bold;
              color: red;
            }
            #event {
              text-align: center;
              font-size: 1em;
              font-weight: bold;
              color: red;
            }
            #event1 {
              text-align: center;
              font-size: 1em;
              font-weight: bold;
              color: red;
            }
            #area {
              text-align: center;
              font-size: 2em;
              font-style: italic;
              font-weight: bold;
              color: blue;
            }"
        )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      width=3,
      # controls for distribution info
      fluidRow(
        column(
          6,
          numericInput(
            'ncpChiID',
            'Non-Centrality',
            value=0,
            min=-Inf,
            max=Inf,
            step=1)
        ),
        column(
          6,
          p()
        )
      ),
      sliderTextInput(
        'nuChiID',
        'Degrees of Freedom',
        choices=c(0:100,Inf),
        selected=10),
      # action button
      fluidRow(
        column(
          4,
          actionButton(
            'plotButtonChiID',
            'Plot'
          ),
          actionButton(
            'alphaButtonChiID',
            'Get \u03B1'
          )
        ),
        column(
          6,
          numericInput(
            'alphaChiID',
            '\u03B1 Value',
            value=NULL,
            min=0.001,
            max=0.999,
            step=0.001
          ),
          align='center'
        )
      ),
      htmlOutput('alphaDisplayChiID'),
      fluidRow(
        column(
          6,
          htmlOutput("event")
        ),
        column(
          6,
          htmlOutput("event1")
        )
      ),
      # htmlOutput('area'),
      p()
    ),
    mainPanel(
      # Fancy title label
      uiOutput('labChiID'),
      # plot output
      plotlyOutput('plotChiID'),
      # fluidRow(
      #   column(
      #     6,
      #     htmlOutput("event")
      #   ),
      #   column(
      #     6,
      #     htmlOutput("event1")
      #   )
      # ),
      verbatimTextOutput('debug')
      # htmlOutput('area')
    )
  )
)

x <- seq(-10, 10)
y <- rnorm(length(x))


server <- function(input, output, session) {
  
  observeEvent(input$plotButtonChiID,{
    # fire the plot function
    p <- chiDist(input$nuChiID, input$ncpChiID)
    # create reactive values for calculating the area
    # each reactive value ties to a line's position
    q <- reactiveVal(pchisq(1,input$nuChiID, input$ncpChiID))
    q1 <- reactiveVal(pchisq(3,input$nuChiID, input$ncpChiID))
    # display the area left/right ignoring the other line
    # xpt <- p$data$x[which.min(abs(p$data$x - 1))]
    # output$event <- renderText((p$data$text[which(p$data$x == xpt)]))
    # xpt <- p$data$x[which.min(abs(p$data$x - -1))]
    # output$event1 <- renderText((p$data$text[which(p$data$x == xpt)]))
    # display the area between the lines
    # output$area <- renderText(paste("Area between the lines:", round(q()-q1(), 4)))
    output$plotChiID <- renderPlotly({
      # format options for the axis
      a <- list(
        autotick = FALSE,
        ticks = "outside",
        tick0 = 0,
        dtick = 0.25,
        ticklen = 3,
        tickwidth = 2,
        tickangle = 45,
        tickcolor = toRGB("blue")
      )
      # create the lines with starting locations
      lines <-
        list(      
          list(
            type = "line",
            line = list(color = "black", dash = "dash"),
            x0 = 1,
            x1 = 1,
            y0 = 0,
            y1 = 0.5,
            yref = "y",
            layer='below'),
          list(
            type = "line",
            line = list(color = "black", dash = "dot"),
            x0 = 3,
            x1 = 3,
            y0 = 0,
            y1 = 0.5,
            yref = "y",
            layer='below'),
          list(
            type = "line",
            line = list(color = "blue", dash = "solid"),
            x0 = 2,
            x1 = 2,
            y0 = 0,
            y1 = 0.5,
            yref = "y",
            layer='below')
        )
      # render the ggplot object as plotly
      ggplotly(p, 
               # source='trajectory',
               tooltip='text',
               #height=600,
               height=400) %>%
        style(hoverlabel = label) %>%
        layout(font = font,
               xaxis = a) -> p
      # add the lines (not possible to add directly into layout for some reason)
      p$x$layout$shapes <- lines
      # make the plot editable so we can move the lines
      p %>% 
        config(editable=TRUE) 
      
    })
    # now add an observe to update the text ourside of renderPlotly on event
    # relayout fires when the plot is moved
    observeEvent(event_data("plotly_relayout"),{
      # get the event data, tells what moved
      d <- event_data("plotly_relayout")
      # if the right line moved
      if(!is.null(d[['shapes[0].x0']])){
        # get the new X location
        xint <- d[["shapes[0].x0"]]
        # get the closest Y location from the plot object's data
        xpt <- p$data$x[which.min(abs(p$data$x - xint))]
        # set the reactive value to get the new area
        q(pchisq(xint, input$nuChiID, input$ncpChiID))
        # output the area left/right of the line ignoring the other line
        output$event <- renderText((p$data$text[which(p$data$x == xpt)]))
        # get the area between the lines
        output$area <- renderText(paste("Area between the lines:", round(q()-q1(), 4)))
      }
      # if the left line moved
      if(!is.null(d[['shapes[1].x0']])){
        # get the new X location
        xint <- d[["shapes[1].x0"]]
        # get the closest Y location from the plot object's data
        xpt <- p$data$x[which.min(abs(p$data$x - xint))]
        # set the reactive value to get the new area
        q1(pchisq(xint, input$nuChiID, input$ncpChiID))
        # output the area left/right of the line ignoring the other line
        output$event1 <- renderText((p$data$text[which(p$data$x == xpt)]))
        # get the area between the lines
        output$area <- renderText(paste("Area between the lines:", round(q()-q1(), 4)))
      }
    })
  })
  observeEvent(input$alphaButtonChiID,{
    output$alphaDisplayChiID <- renderText({
      getCriticalChi(input$alphaChiID, input$nuChiID, input$ncpChiID)
      #c(input$alphaNormID, input$muNormID, input$sigNormID)
    })
  })
  
}

shinyApp(ui, server)


