require(shiny)
require(shinyWidgets)
require(plotly)
require(tidyverse, quietly = TRUE)

#setwd("C:/Users/bexar_000/Desktop/Stats/shinyDeploy")
source("shinyProbDistFunc.R")

ui <- 
  navbarPage(
    'Distributions',
    tabPanel('F',
      tags$head(
        tags$style(
          HTML(
            ".MathJax {
              font-size: 5em !important;
              color: black;
            }
            #alphaDisplayFID {
              text-align: center;
              font-size: 2em;
              font-style: italic;
              font-weight: bold;
              color: red;
            }"
          )
        )
      ),
      sidebarLayout(
        sidebarPanel(
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
          htmlOutput('alphaDisplayFID')
        ),
        mainPanel(
          # Fancy title label
          uiOutput('labFID'),
          # plot output
          plotlyOutput('plotFID')
        )
      )
    ),
    tabPanel(
      'Normal',
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
            }"
          )
        )
      ),
      sidebarLayout(
        sidebarPanel(          
          # sliders for degrees of freedom
          fluidRow(
            column(
              6,
              numericInput(
                'muNormID',
                '\u03BC',
                value=0,
                min=-Inf,
                max=Inf,
                step=1)
              ),
            column(
              6,
              numericInput(
                'sigmaNormID',
                '\u03C3',
                value=1,
                min=0,
                max=Inf,
                step=1)
              )
          ),
          # action button
          fluidRow(
            column(
              4,
              actionButton(
                'plotButtonNormID',
                'Plot'
              ),
              actionButton(
                'alphaButtonNormID',
                'Get \u03B1'
              )
            ),
            column(
              6,
              numericInput(
                'alphaNormID',
                '\u03B1 Value',
                value=NULL,
                min=0.001,
                max=0.999,
                step=0.001
              ),
              align='center'
            )
          ),
          htmlOutput('alphaDisplayNormID')
        ),
        mainPanel(
          # Fancy title label
          uiOutput('labNormID'),
          # plot output
          plotlyOutput('plotNormID')
        )
      )
    ),
    tabPanel(
      'Student\'s T',
      tags$head(
        tags$style(
          HTML(
            ".MathJax {
              font-size: 5em !important;
              color: black;
            }
            #alphaDisplayTID {
              text-align: center;
              font-size: 2em;
              font-style: italic;
              font-weight: bold;
              color: red;
            }"
          )
        )
      ),
      sidebarLayout(
        sidebarPanel(          
          # controls for distribution info
          fluidRow(
            column(
              6,
              numericInput(
                'muTID',
                '\u03BC',
                value=0,
                min=-Inf,
                max=Inf,
                step=1)
            ),
            column(
              6,
              p(),
              checkboxInput(
                'normBoxTID',
                'Draw Normal',
                value=FALSE)
            )
          ),
          sliderTextInput(
            'nuTID',
            'Degrees of Freedom',
            choices=c(0:100,Inf),
            selected=10),
          # action button
          fluidRow(
            column(
              4,
              actionButton(
                'plotButtonTID',
                'Plot'
              ),
              actionButton(
                'alphaButtonTID',
                'Get \u03B1'
              )
            ),
            column(
              6,
              numericInput(
                'alphaTID',
                '\u03B1 Value',
                value=NULL,
                min=0.001,
                max=0.999,
                step=0.001
              ),
              align='center'
            )
          ),
          htmlOutput('alphaDisplayTID')
        ),
        mainPanel(
          # Fancy title label
          uiOutput('labTID'),
          # plot output
          plotlyOutput('plotTID')
        )
      )
    ),
    tabPanel(
      HTML(paste0('\u03A7', tags$sup('2'))),
      # withMathJax(
      #   helpText(paste0("\\(T\\sim t_{",input$nuTID,
      #                   "},\\mu={",input$muTID,"}\\)"))
      # ),
      tags$head(
        tags$style(
          HTML(
            ".MathJax {
              font-size: 5em !important;
              color: black;
            }
            #alphaDisplayChiID {
              text-align: center;
              font-size: 2em;
              font-style: italic;
              font-weight: bold;
              color: red;
            }"
          )
        )
      ),
      sidebarLayout(
        sidebarPanel(          
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
          htmlOutput('alphaDisplayChiID')
        ),
        mainPanel(
          # Fancy title label
          uiOutput('labChiID'),
          # plot output
          plotlyOutput('plotChiID')
        )
      )
    ),
    tabPanel(
      'Exp',
      # withMathJax(
      #   helpText(paste0("\\(T\\sim t_{",input$nuTID,
      #                   "},\\mu={",input$muTID,"}\\)"))
      # ),
      tags$head(
        tags$style(
          HTML(
            ".MathJax {
              font-size: 5em !important;
              color: black;
            }
            #alphaDisplayChiID {
              text-align: center;
              font-size: 2em;
              font-style: italic;
              font-weight: bold;
              color: red;
            }"
          )
        )
      ),
      sidebarLayout(
        sidebarPanel(          
          # controls for distribution info
          fluidRow(
          #   column(
          #     6,
          #     numericInput(
          #       'ncpChiID',
          #       'Non-Centrality',
          #       value=0,
          #       min=-Inf,
          #       max=Inf,
          #       step=1)
          #   ),
          #   column(
          #     6,
          #     p()
          #   )
          ),
          sliderInput(
            'lambdaExpID',
            HTML('\u03BB'),
            max=25,
            min=0,
            step=0.1,
            value=2),
          # action button
          fluidRow(
            column(
              4,
              actionButton(
                'plotButtonExpID',
                'Plot'
              ),
              actionButton(
                'alphaButtonExpID',
                'Get \u03B1'
              )
            ),
            column(
              6,
              numericInput(
                'alphaExpID',
                '\u03B1 Value',
                value=NULL,
                min=0.001,
                max=0.999,
                step=0.001
              ),
              align='center'
            )
          ),
          htmlOutput('alphaDisplayExpID')
        ),
        mainPanel(
          # Fancy title label
          uiOutput('labExpID'),
          # plot output
          plotlyOutput('plotExpID')
        )
      )
    ),
    tabPanel(
      'Binom',
      tags$head(
        tags$style(
          HTML(
            ".MathJax {
              font-size: 5em !important;
              color: black;
            }
            #alphaDisplayChiID {
              text-align: center;
              font-size: 2em;
              font-style: italic;
              font-weight: bold;
              color: red;
            }"
          )
        )
      ),
      sidebarLayout(
        sidebarPanel(          
          # controls for distribution info
          fluidRow(
            column(
              6,
              numericInput(
                'nBinomID',
                'Trials',
                value=5,
                min=0,
                max=Inf,
                step=1)
            ),
            column(
              6,
              p(),
              checkboxInput(
                'normBoxBinomID',
                'Normal Approx',
                value=FALSE)
            )
          ),
          sliderInput(
            'pBinomID',
            'p Success',
            max=1,
            min=0,
            step=0.005,
            value=0.5),          
          # action button
          fluidRow(
            column(
              4,
              actionButton(
                'plotButtonBinomID',
                'Plot'
              )
            )
          )
        ),
        mainPanel(
          # Fancy title label
          uiOutput('labBinomID'),
          # plot output
          plotlyOutput('plotBinomID')
        )
      )
    )
  )

server <- function(input, output){
  # F Dist
  observeEvent(input$plotButtonFID,{
    output$plotFID <- renderPlotly(ggplotly(fDist(input$nu1FID, input$nu2FID), tooltip='text',
                                            height=800,
                                            width=1200) %>%
                                    style(hoverlabel = label) %>%
                                    layout(font = font)
                                  )
    output$labFID <- renderUI({
      withMathJax(
        helpText(paste0("\\(F_{",
                        input$nu1FID,"},_{",input$nu2FID,"}\\)"))
      )
    })
  })
  observeEvent(input$alphaButtonFID,{
    output$alphaDisplayFID <- renderText({
      getCriticalF(input$alphaFID, input$nu1FID, input$nu2FID)
    })
  })
  # Norm Dist
  observeEvent(input$plotButtonNormID,{
    output$plotNormID <- renderPlotly(ggplotly(normDist(input$muNormID, input$sigmaNormID), tooltip='text',
                                               height=800,
                                               width=1200,
                                               execOnResize = FALSE) %>%
                                        style(hoverlabel = label) %>%
                                        layout(font = font)
    )
    output$labNormID <- renderUI({
      withMathJax(
        helpText(paste0("\\(N(",
                        input$muNormID,",",input$sigmaNormID,")\\)"))
      )
    })
  })
  observeEvent(input$alphaButtonNormID,{
    output$alphaDisplayNormID <- renderText({
      getCriticalNorm(input$alphaNormID, input$muNormID, input$sigmaNormID)
      #c(input$alphaNormID, input$muNormID, input$sigNormID)
    })
  })
  # T Dist
  observeEvent(input$plotButtonTID,{
    output$plotTID <- renderPlotly(ggplotly(tDist(input$muTID, input$nuTID, input$normBoxTID), tooltip='text',
                                            height=800,
                                            width=1200) %>%
                                        style(hoverlabel = label) %>%
                                        layout(font = font)
    )
    output$labTID <- renderUI({
      withMathJax(
        helpText(paste0("\\(T\\sim t_{",input$nuTID,
                        "},\\mu={",input$muTID,"}\\)"))
      )
    })
  })
  observeEvent(input$alphaButtonTID,{
    output$alphaDisplayTID <- renderText({
      getCriticalT(input$alphaTID, input$muTID, input$nuTID)
    })
  })
  # Chi2 Dist
  observeEvent(input$plotButtonChiID,{
    output$plotChiID <- renderPlotly(ggplotly(chiDist(input$nuChiID, input$ncpChiID), tooltip='text',
                                              height=800,
                                              width=1200) %>%
                                     style(hoverlabel = label) %>%
                                     layout(font = font)
    )
    output$labChiID <- renderUI({
      withMathJax(
        helpText(paste0("\\(\\chi^2_{",input$nuChiID,
                        "}\\)"))
      )
    })
  })
  observeEvent(input$alphaButtonChiID,{
    output$alphaDisplayChiID <- renderText({
      getCriticalChi(input$alphaChiID, input$nuChiID, input$ncpChiID)
    })
  })
  # Exp Dist
  observeEvent(input$plotButtonExpID,{
    output$plotExpID <- renderPlotly(ggplotly(expDist(input$lambdaExpID), tooltip='text',
                                              height=800,
                                              width=1200) %>%
                                       style(hoverlabel = label) %>%
                                       layout(font = font)
    )
    output$labChiID <- renderUI({
      withMathJax(
        helpText(paste0("\\(Exp(",input$lambdaExpID,
                        "}\\)"))
      )
    })
  })
  observeEvent(input$alphaButtonExpID,{
    output$alphaDisplayExpID <- renderText({
      getCriticalChi(input$alphaExpID, input$labdaExpID)
    })
  })
  # Binom Dist
  observeEvent(input$plotButtonBinomID,{
    output$plotBinomID <- renderPlotly(ggplotly(binomDist(input$nBinomID, input$pBinomID, input$normBoxBinomID), tooltip='text',
                                                height=800,
                                                width=1200) %>%
                                       style(hoverlabel = label) %>%
                                       layout(font = font)
    )
    output$labBinomID <- renderUI({
      withMathJax(
        helpText(paste0("\\(B(",input$nBinomID,',',
                        input$pBinomID,")\\)"))
      )
    })
  })
  observeEvent(input$alphaButtonBinomID,{
    output$alphaDisplayBinomID <- renderText({
      #getCriticalBinom()
    })
  })
}

shinyApp(ui, server)
