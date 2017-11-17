
server <- function(input, output) {
  
  set.seed(1010)
  datos_simple <- data.frame(x = rnorm(999, 50, 15))
  datos_simple$y <- 25 + 5*datos_simple$x + rnorm(999, 0, 50)
    
  actualiza_datos_simple <- reactive({
    auxiliar <- rbind(c(input$x_simple, input$y_simple), datos_simple)
    auxiliar[1:input$n_simple,]
  })
  
  output$grafica_simple <- renderPlot({
    datos <- actualiza_datos_simple()
    
    rango_x <- max(datos$x[-1]) - min(datos$x[-1])
    rango_y <- max(datos$y[-1]) - min(datos$y[-1])
    
    limites_x <- range(datos$x[-1]) + c(-1, 1) * 0.15 * rango_x 
    limites_y <- range(datos$y[-1]) + c(-1, 1) * 0.15 * rango_y
    
    par(mar = c(4.1, 4.1, 1, 1))
    plot(datos[-1,], pch = 16, col = 'darkcyan',
         xlim = limites_x, ylim = limites_y,
         xlab = 'Variable explicativa', ylab = 'Variable respuesta')
    points(datos[1,], pch = 16, col = 'red3')
    abline(lm(y ~ x, datos), col = 'red3', lwd = 3)
    abline(lm(y ~ x, datos[-1,]), col = 'darkcyan', lwd = 3)
  })  


  
  output$leverage_simple <- renderPrint({
    datos <- actualiza_datos_simple()
    leverage <- hatvalues(lm(y ~ x, datos))[1]
    punto_corte <- 2/input$n_simple
    
    auxiliar <- c(leverage, punto_corte)
    names(auxiliar) <- c('Leverage(i)', 'Punto de Corte')
    round(auxiliar, 5)
  })
  
  output$dfbetas_simple <- renderPrint({
    datos <- actualiza_datos_simple()
    punto_corte <- 2/sqrt(input$n_simple)
    auxiliar <- c(dfbetas(lm(y ~ x, datos))[1,], punto_corte)
    names(auxiliar) <- c('DFBETAS(i,0)', 'DFBETAS(i,1)', 'Punto de corte')
    round(auxiliar, 5)
  })
  
  output$dffits_simple <- renderPrint({
    datos <- actualiza_datos_simple()
    punto_corte <- 2/sqrt(input$n_simple)
    auxiliar <- c(dffits(lm(y ~ x, datos))[1], punto_corte)
    names(auxiliar) <- c('DFFITS(i)', 'Punto de corte')
    round(auxiliar, 5)
  })
  
  
  output$covratio_simple <- renderPrint({
    datos <- actualiza_datos_simple()
    puntos_corte <- 1 + c(-1, 1)*3/input$n_simple
    auxiliar <- c(covratio(lm(y ~ x, datos))[1], puntos_corte)
    names(auxiliar) <- c('COVRATIO(i)', 'PC inferior', 'PC superior')
    round(auxiliar, 5)
  })
  
  
  output$cobertura <- reactive({
    
    input$nsample
    
    li <- c()
    ls <- c()
    for (i in 1:input$m)
    {
      muestra <- rbinom(input$n2, 1, input$prob)
      prop <- mean(muestra)
      li[i] <- prop - qnorm(0.5+input$a2/2)*sqrt(prop*(1-prop)/input$n2)
      ls[i] <- prop + qnorm(0.5+input$a2/2)*sqrt(prop*(1-prop)/input$n2)
    }
    aux <- (li <= input$prob) & (ls >= input$prob)
    res <- round(100*(sum(aux)/input$m), 1) 
    paste0(res, '%')
  })
  
  
}
