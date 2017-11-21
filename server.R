library(shiny)

server <- function(input, output) {
  
  set.seed(1010)
  
  # Datos para regresión simple
  datos_simple <- data.frame(x = rnorm(999, 50, 15))
  datos_simple$y <- 25 + 5*datos_simple$x + rnorm(999, 0, 50)
  
  # Datos para regresión múltiple
  datos_multiple <- data.frame(x1 = rnorm(999, 30, 10), x2 = rnorm(999, 50, 15))
  datos_multiple$y <- 200 + 3.1*datos_multiple$x1 + 2.5*datos_multiple$x2 + rnorm(999, 0, 25)
  
  # Función para actualizar el conjunto de datos a usar en la regresión simple
  actualiza_datos_simple <- reactive({
    auxiliar <- rbind(c(input$x_simple, input$y_simple), datos_simple)
    auxiliar[1:input$n_simple,]
  })
  
  # Función para actualizar el conjunto de datos a usar en la regresión múltiple
  actualiza_datos_multiple <- reactive({
    auxiliar <- rbind(c(input$x1_multiple, input$x2_multiple, input$y_multiple),
                      datos_multiple)
    auxiliar[1:input$n_multiple,]
  })
  
  # Gráfica de dispersión de la regresión simple
  output$dispersion_simple <- renderPlot({
    datos <- actualiza_datos_simple()
    
    rango_x <- max(datos$x[-1]) - min(datos$x[-1])
    rango_y <- max(datos$y[-1]) - min(datos$y[-1])
    
    limites_x <- range(datos$x[-1]) + c(-1, 1) * 0.15 * rango_x 
    limites_y <- range(datos$y[-1]) + c(-1, 1) * 0.15 * rango_y
    
    par(mar = c(4.1, 4.1, 1, 1))
    plot(datos[-1,], pch = 16, col = 'darkcyan',
         xlim = limites_x, ylim = limites_y,
         xlab = 'Variable explicativa (X)', ylab = 'Variable respuesta (Y)', cex = 2)
    points(datos[1,], pch = 16, col = 'red3', cex = 2)
    abline(lm(y ~ x, datos), col = 'red3', lwd = 3)
    abline(lm(y ~ x, datos[-1,]), col = 'darkcyan', lwd = 3)
  })  
  
  
  output$outliers_simple <- renderPlot({
    datos <- actualiza_datos_simple()
    
    modelo <- lm(y ~ x, datos)
    ajustados <- fitted(modelo)
    
    library(MASS)
    residuos <- studres(modelo)
    
    par(mar = c(4.1, 4.1, 1, 1))
    plot(ajustados, residuos, pch = 16, col = c('red3', rep('darkcyan', input$n_simple-1)),
         xlab = 'Valores ajustados', ylab = 'Residuos studentizados', cex = 2)
    abline(h = c(-3, 3), col = 'red3', lwd = 3)
  })

  # Estadísticos para observaciones influyentes de la regresión simple
  ## Leverage
  output$leverage_simple <- renderPrint({
    datos <- actualiza_datos_simple()
    leverage <- hatvalues(lm(y ~ x, datos))[1]
    auxiliar <- c(leverage)
    names(auxiliar) <- c('Leverage(i)')
    round(auxiliar, 5)
  })
  
  ## DFBETAS
  output$dfbetas_simple <- renderPrint({
    datos <- actualiza_datos_simple()
    auxiliar <- c(dfbetas(lm(y ~ x, datos))[1,])
    names(auxiliar) <- c('DFBETAS(i,0)', 'DFBETAS(i,1)')
    round(auxiliar, 5)
  })
  
  ## DFFITS
  output$dffits_simple <- renderPrint({
    datos <- actualiza_datos_simple()
    auxiliar <- c(dffits(lm(y ~ x, datos))[1])
    names(auxiliar) <- c('DFFITS(i)')
    round(auxiliar, 5)
  })
  
  ## COVRATIO
  output$covratio_simple <- renderPrint({
    datos <- actualiza_datos_simple()
    auxiliar <- c(covratio(lm(y ~ x, datos))[1])
    names(auxiliar) <- c('COVRATIO(i)')
    round(auxiliar, 5)
  })
  
  output$dispersion_multiple <- renderPlot({
    datos <- actualiza_datos_multiple()
    
    modelo_completo <- lm(y ~ x1 + x2, data = datos)
    modelo_incompleto <- lm(y ~ x1 + x2, data = datos[-1,])
    
    x <- seq(min(datos$x1), max(datos$x1), length.out = 25)
    y <- seq(min(datos$x2), max(datos$x2), length.out = 25)
    
    z1 <- outer(x, y, function(x, y) predict(modelo_completo, data.frame(x1 = x, x2 = y)))
    z2 <- outer(x, y, function(x, y) predict(modelo_incompleto, data.frame(x1 = x, x2 = y)))
    
    plano_completo <- persp(x, y, z1, theta = input$theta, phi = input$phi, col = "red3",
                            shade = 0.5, ticktype = "detailed", ltheta = 30, lphi = 15)
    par(new = TRUE)
    plano_incompleto <- persp(x, y, z2, theta = input$theta, phi = input$phi, col = "darkcyan",
                              shade = NA, ticktype = "simple", ltheta = 30, lphi = 15, axes = F)
    puntos_completo <- trans3d(datos[,1], datos[,2], datos[,3], plano_completo)
    points(puntos_completo, col = c('red3', rep('darkcyan', input$n_multiple-1)),
           pch = 16, cex = 2)
  })
  
  
  # Estadísticos para observaciones atípicas de la regresión multiple
  ## Leverage
  output$leverage_multiple <- renderPrint({
    datos <- actualiza_datos_multiple()
    leverage <- hatvalues(lm(y ~ x1 + x2, datos))[1]
    auxiliar <- c(leverage)
    names(auxiliar) <- c('Leverage(i)')
    round(auxiliar, 5)
  })
  
  ## DFBETAS
  output$dfbetas_multiple <- renderPrint({
    datos <- actualiza_datos_multiple()
    auxiliar <- c(dfbetas(lm(y ~ x1 + x2, datos))[1,])
    names(auxiliar) <- c('DFBETAS(i,0)', 'DFBETAS(i,1)', 'DFBETAS(i,3)')
    round(auxiliar, 5)
  })
  
  ## DFFITS
  output$dffits_multiple <- renderPrint({
    datos <- actualiza_datos_multiple()
    auxiliar <- c(dffits(lm(y ~ x1 + x2, datos))[1])
    names(auxiliar) <- c('DFFITS(i)')
    round(auxiliar, 5)
  })
  
  ## COVRATIO
  output$covratio_multiple <- renderPrint({
    datos <- actualiza_datos_multiple()
    auxiliar <- c(covratio(lm(y ~ x1 + x2, datos))[1])
    names(auxiliar) <- c('COVRATIO(i)')
    round(auxiliar, 5)
  })
}
