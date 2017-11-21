library(shiny)

ui <- navbarPage("Observaciones atípicas e influyentes", fluid = T,
                 tabPanel("Regresión simple",
                          fluidRow(
                            column(2,
                              wellPanel(
                                h3('Parámetros'),
                                numericInput('n_simple', 'n', value = 30, step = 10, min = 10, max = 1000),
                                numericInput('x_simple', 'X', value = 50, step = 1),
                                numericInput('y_simple', 'Y', value = 275, step = 5)
                              )
                            ),
                            column(6, plotOutput('dispersion_simple')),
                            column(4,
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
                            column(2,
                                   wellPanel(
                                     h3('Parámetros'),
                                     numericInput('n_multiple', 'n', value = 30, step = 10, min = 10, max = 1000),
                                     numericInput('x1_multiple', 'X1', value = 30, step = 5),
                                     numericInput('x2_multiple', 'X2', value = 50, step = 10),
                                     numericInput('y_multiple', 'Y', value = 420, step = 25)
                                )
                            ),
                            column(6, plotOutput('dispersion_multiple')),
                            column(4,
                                   h4('Leverage'),
                                   verbatimTextOutput('leverage_multiple'),
                                   h4('DFBETAS'),
                                   verbatimTextOutput('dfbetas_multiple'),
                                   h4('DFFITS'),
                                   verbatimTextOutput('dffits_multiple'),
                                   h4('COVRATIO'),
                                   verbatimTextOutput('covratio_multiple')
                            )
                          ),
                          br(),
                          fluidRow(
                            column(4, offset = 2,
                                   sliderInput('theta', 'Azimut', value = 30, step = 1, min = 0, max = 360,
                                               animate = animationOptions(interval = 100, loop = TRUE))),
                            column(4, sliderInput('phi', 'Colatitud', value = 20, step = 20, min = -40, max = 40))
                          )
                 )
)
