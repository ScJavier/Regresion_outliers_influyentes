
library(shiny)
library(shinyRGL)
library(rgl)

ui <- navbarPage("Observaciones atípicas e influyentes", fluid = T,
                 tabPanel("Regresión simple",
                          fluidRow(
                            column(3,
                              wellPanel(
                                numericInput('n_simple', 'Tamaño de muestra', value = 30, step = 10, min = 10, max = 1000),
                                numericInput('x_simple', 'Variable explicativa (X)', value = 50, step = 1),
                                numericInput('y_simple', 'Variable respuesta (Y)', value = 275, step = 5)
                              )
                            ),
                            column(8, offset = 1, 
                              plotOutput('dispersion_simple')
                            )
                          ),
                          br(),
                          fluidRow(
                            column(4, offset = 4,
                              h4('Leverage'),
                              verbatimTextOutput('leverage_simple'),
                              h4('DFBETAS'),
                              verbatimTextOutput('dfbetas_simple'),
                              h4('DFFITS'),
                              verbatimTextOutput('dffits_simple'),
                              h4('COVRATIO'),
                              verbatimTextOutput('covratio_simple')
                            )
                          )
                 ),
                 
                 tabPanel("Regresión múltiple",
                          fluidRow(
                            column(3,
                                   wellPanel(
                                     numericInput('n_multiple', 'Tamaño de muestra', value = 30, step = 10, min = 10, max = 1000),
                                     numericInput('x1_multiple', 'Variable explicativa 1 (X1)', value = 30, step = 5),
                                     numericInput('x2_multiple', 'Variable explicativa 2 (X2)', value = 50, step = 10),
                                     numericInput('y_multiple', 'Variable respuesta (Y)', value = 420, step = 25)
                                )
                            ),
                            column(8,
                                   rglwidgetOutput("prueba", width = '900px', height = '600px')
                            )
                          ),
                          br(),
                          fluidRow(
                            column(4, offset = 4,
                                   h4('Leverage'),
                                   verbatimTextOutput('leverage_multiple'),
                                   h4('DFBETAS'),
                                   h4('DFFITS'),
                                   h4('COVRATIO')
                            )
                          )
                 )
)
