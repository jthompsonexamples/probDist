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
      # sliders for degrees of freedom
      sliderTextInput(
        'nu1FID',
        'Numerator Degrees of Freedom',
        choices=c(0:100,Inf),
        selected=30),
      sliderTextInput(
        'nu2FID',
        'Denominator Degrees of Freedom',
        choices=c(0:100,Inf),
        selected=10),
      # action button
      fluidRow(
        column(
          4,
          actionButton(
            'plotButtonFID',
            'Plot'
          ),
          actionButton(
            'alphaButtonFID',
            'Get \u03B1'
          )
        ),
        column(
          6,
          numericInput(
            'alphaFID',
            '\u03B1 Value',
            value=NULL,
            min=0.001,
            max=0.999,
            step=0.001
          ),
          align='center'
        )
      ),
      htmlOutput('alphaDisplayFID'),
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
      uiOutput('labFID'),
      # plot output
      plotlyOutput('plotFID'),
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


server <- function(input, output, session) {
  
  observeEvent(input$plotButtonFID,{
    # fire the plot function
    p <- fDist(input$nu1FID, input$nu2FID)
    # create reactive values for calculating the area
    # each reactive value ties to a line's position
    q <- reactiveVal(pf(1,input$nu1FID, input$nu2FID))
    q1 <- reactiveVal(pf(3,input$nu1FID, input$nu2FID))
    # display the area left/right ignoring the other line
    # xpt <- p$data$x[which.min(abs(p$data$x - 1))]
    # output$event <- renderText((p$data$text[which(p$data$x == xpt)]))
    # xpt <- p$data$x[which.min(abs(p$data$x - -1))]
    # output$event1 <- renderText((p$data$text[which(p$data$x == xpt)]))
    # display the area between the lines
    # output$area <- renderText(paste("Area between the lines:", round(q()-q1(), 4)))
    output$plotFID <- renderPlotly({
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
            y1 = 1,
            yref = "y",
            layer='below'),
          list(
            type = "line",
            line = list(color = "black", dash = "dot"),
            x0 = 3,
            x1 = 3,
            y0 = 0,
            y1 = 1,
            yref = "y",
            layer='below'),
          list(
            type = "line",
            line = list(color = "blue", dash = "solid"),
            x0 = 2,
            x1 = 2,
            y0 = 0,
            y1 = 1,
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
        q(pf(xint, input$nu1FID, input$nu2FID))
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
        q1(pf(xint, input$nu1FID, input$nu2FID))
        # output the area left/right of the line ignoring the other line
        output$event1 <- renderText((p$data$text[which(p$data$x == xpt)]))
        # get the area between the lines
        output$area <- renderText(paste("Area between the lines:", round(q()-q1(), 4)))
      }
    })
  })
  observeEvent(input$alphaButtonFID,{
    output$alphaDisplayFID <- renderText({
      getCriticalF(input$alphaFID, input$nu1FID, input$nu2FID)
      #c(input$alphaNormID, input$muNormID, input$sigNormID)
    })
  })
  
}

shinyApp(ui, server)


