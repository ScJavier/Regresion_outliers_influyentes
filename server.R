options(rgl.useNULL=T)

library(shiny)
library(shinyRGL)
library(rgl)

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
         xlab = 'Variable explicativa (X)', ylab = 'Variable respuesta (Y)')
    points(datos[1,], pch = 16, col = 'red3')
    abline(lm(y ~ x, datos), col = 'red3', lwd = 3)
    abline(lm(y ~ x, datos[-1,]), col = 'darkcyan', lwd = 3)
  })  

  # Estadísticos para observaciones atípicas de la regresión simple
  ## Leverage
  output$leverage_simple <- renderPrint({
    datos <- actualiza_datos_simple()
    leverage <- hatvalues(lm(y ~ x, datos))[1]
    punto_corte <- 2/input$n_simple
    
    auxiliar <- c(leverage, punto_corte)
    names(auxiliar) <- c('Leverage(i)', 'Punto de Corte')
    round(auxiliar, 5)
  })
  
  ## DFBETAS
  output$dfbetas_simple <- renderPrint({
    datos <- actualiza_datos_simple()
    punto_corte <- 2/sqrt(input$n_simple)
    auxiliar <- c(dfbetas(lm(y ~ x, datos))[1,], punto_corte)
    names(auxiliar) <- c('DFBETAS(i,0)', 'DFBETAS(i,1)', 'Punto de corte')
    round(auxiliar, 5)
  })
  
  ## DFFITS
  output$dffits_simple <- renderPrint({
    datos <- actualiza_datos_simple()
    punto_corte <- 2/sqrt(input$n_simple)
    auxiliar <- c(dffits(lm(y ~ x, datos))[1], punto_corte)
    names(auxiliar) <- c('DFFITS(i)', 'Punto de corte')
    round(auxiliar, 5)
  })
  
  ## COVRATIO
  output$covratio_simple <- renderPrint({
    datos <- actualiza_datos_simple()
    puntos_corte <- 1 + c(-1, 1)*3/input$n_simple
    auxiliar <- c(covratio(lm(y ~ x, datos))[1], puntos_corte)
    names(auxiliar) <- c('COVRATIO(i)', 'PC inferior', 'PC superior')
    round(auxiliar, 5)
  })
  
  # Gráfica de dispersión de la regresión múltiple
  output$prueba <- renderRglwidget({
    datos <- actualiza_datos_multiple()
    
    plot3d(x = datos[,1], y = datos[,2], z = datos[,3],
           col = c('red3', rep('darkcyan', 999)),
           size = 1, type = 's', box = F,
           xlab = 'X1', ylab = 'X2', zlab = 'Y')

    modelo_completo <- lm(y ~ x1 + x2, data = datos)
    modelo_incompleto <- lm(y ~ x1 + x2, data = datos[-1,])
    
    predichos_completo <- expand.grid(x1 = seq(min(datos$x1), max(datos$x1), length.out = 100),
                                      x2 = seq(min(datos$x2), max(datos$x2), length.out = 100))
    predichos_completo$y <- predict(modelo_completo, predichos_completo)
    
    predichos_incompleto <- expand.grid(x1 = seq(min(datos$x1), max(datos$x1), length.out = 100),
                                        x2 = seq(min(datos$x2), max(datos$x2), length.out = 100))
    predichos_incompleto$y <- predict(modelo_incompleto, predichos_incompleto)
    
    surface3d(unique(predichos_completo[,1]),
              unique(predichos_completo[,2]),
              predichos_completo[,3], col = 'red3', alpha = 0.5)
    
    surface3d(unique(predichos_incompleto[,1]),
              unique(predichos_incompleto[,2]),
              predichos_incompleto[,3], col = 'darkcyan', alpha = 0.5)
    rglwidget()
  })
  
  
  # Estadísticos para observaciones atípicas de la regresión multiple
  ## Leverage
  output$leverage_multiple <- renderPrint({
    datos <- actualiza_datos_multiple()
    leverage <- hatvalues(lm(y ~ x1 + x2, datos))[1]
    punto_corte <- 4/input$n_multiple
    
    auxiliar <- c(leverage, punto_corte)
    names(auxiliar) <- c('Leverage(i)', 'Punto de Corte')
    round(auxiliar, 5)
  })
  
  ## DFBETAS
  output$dfbetas_multiple <- renderPrint({
    datos <- actualiza_datos_multiple()
    punto_corte <- 2/sqrt(input$n_multiple)
    auxiliar <- c(dfbetas(lm(y ~ x1 + x2, datos))[1,], punto_corte)
    names(auxiliar) <- c('DFBETAS(i,0)', 'DFBETAS(i,1)', 'DFBETAS(i,3)', 'Punto de corte')
    round(auxiliar, 5)
  })
  
  ## DFFITS
  output$dffits_multiple <- renderPrint({
    datos <- actualiza_datos_multiple()
    punto_corte <- 2/sqrt(2/input$n_multiple)
    auxiliar <- c(dffits(lm(y ~ x1 + x2, datos))[1], punto_corte)
    names(auxiliar) <- c('DFFITS(i)', 'Punto de corte')
    round(auxiliar, 5)
  })
  
  ## COVRATIO
  output$covratio_multiple <- renderPrint({
    datos <- actualiza_datos_multiple()
    puntos_corte <- 1 + c(-1, 1)*6/input$n_multiple
    auxiliar <- c(covratio(lm(y ~ x1 + x2, datos))[1], puntos_corte)
    names(auxiliar) <- c('COVRATIO(i)', 'PC inferior', 'PC superior')
    round(auxiliar, 5)
  })
}
