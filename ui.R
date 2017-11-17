library(shiny)

ui <- navbarPage("Observaciones atípicas e influyentes", fluid = T, theme = 'bootstrap.css',
                 tabPanel("Regresión simple",
                          fluidRow(
                            column(4,
                              wellPanel(
                                numericInput('n_simple', 'Tamaño de muestra', value = 30, step = 10, min = 10, max = 1000),
                                numericInput('x_simple', 'Variable explicativa (X)', value = 50, step = 1),
                                numericInput('y_simple', 'Variable respuesta (Y)', value = 275, step = 5)
                              )
                            ),
                            column(8,
                              plotOutput('grafica_simple')
                            )
                          ),
                          br(),
                          fluidRow(
                            column(8, offset = 4,
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
                 
                 tabPanel("Cobertura",
                          fluidRow(
                            column(4,
                                wellPanel(
                                  numericInput('prob', 'Probabilidad', value = 0.5, step = 0.01, min = 0.01, max = 0.99),
                                  numericInput('n2', 'Tamaño de muestra', value = 30, step = 1, min = 2),
                                  numericInput('a2', 'Confianza', value = 0.95, step = 0.005, min = 0.5, max = 0.995),
                                  numericInput('m', 'Número de muestras', value = 250, step = 50, min = 50),
                                  actionButton('nsample', 'Nueva muestra')
                                )
                            ),
                            column(8,
                                   h3('Cobertura'),
                                   textOutput('cobertura', container = h4)
                            )
                          )
                 )
)

