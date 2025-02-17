library(shiny)

# Función para calcular el IMC
calcular_imc <- function(peso, talla) {
  return(peso / (talla^2))
}

# Función para calcular el peso esperado basado en el IMC previo
calcular_peso_esperado <- function(imc_previo, peso_pregestacional, semanas) {
  if (imc_previo < 18.5) {
    return(peso_pregestacional + (0.322 * semanas))
  } else if (imc_previo >= 18.5 & imc_previo < 25) {
    return(peso_pregestacional + (0.267 * semanas))
  } else if (imc_previo >= 25 & imc_previo < 30) {
    return(peso_pregestacional + (0.237 * semanas))
  } else {
    return(peso_pregestacional + (0.183 * semanas))
  }
}

# Función para calcular el REE no embarazo
calcular_ree_no_embarazo <- function(peso_pregestacional, edad, naf) {
  if (edad >= 18 && edad <= 30) {
    ger <- (14.818 * peso_pregestacional) + 486.6
  } else if (edad > 30 && edad <= 50) {
    ger <- (8.126 * peso_pregestacional) + 845.6
  } else {
    ger <- (65.3 * peso_pregestacional) - (0.454 * (peso_pregestacional^2)) + 263.4
  }
  
  if (edad >= 18) {
    return(ger * naf)
  } else {
    deposito_energia <- calcular_deposito_energia(edad)
    return(ger + deposito_energia)
  }
}

# Función para calcular el depósito de energía según la edad
calcular_deposito_energia <- function(edad) {
  if (edad >= 12 && edad < 13) {
    return(26)
  } else if (edad >= 13 && edad < 14) {
    return(24)
  } else if (edad >= 14 && edad < 15) {
    return(19)
  } else if (edad >= 15 && edad < 16) {
    return(12)
  } else if (edad >= 16 && edad < 17) {
    return(5)
  } else if (edad >= 17 && edad < 18) {
    return(0)
  } else {
    return(NA)
  }
}

# Función para calcular el costo energético adicional del embarazo
calcular_costo_energetico <- function(trimestre_inicio, trimestre_gestacion) {
  if (trimestre_inicio == 1) {
    if (trimestre_gestacion == 1) {
      return(85)
    } else if (trimestre_gestacion == 2) {
      return(285)
    } else {
      return(475)
    }
  } else {
    if (trimestre_gestacion == 2) {
      return(360)
    } else {
      return(475)
    }
  }
}

# Función para calcular el GET (Gasto Energético Total)
calcular_get <- function(peso_pregestacional, edad, naf) {
  ree_no_embarazo <- calcular_ree_no_embarazo(peso_pregestacional, edad, naf)
  
  if (edad < 18) {
    deposito_energia <- calcular_deposito_energia(edad)
    get_adolescente <- ree_no_embarazo + deposito_energia
    return(list(
      ree_no_embarazo = ree_no_embarazo,
      get_adolescente = get_adolescente
    ))
  } else {
    return(list(
      ree_no_embarazo = ree_no_embarazo
    ))
  }
}

# Función para calcular el REE del embarazo
calcular_ree_embarazo <- function(get_adolescente, trimestre_inicio, trimestre_gestacion) {
  costo_energetico <- calcular_costo_energetico(trimestre_inicio, trimestre_gestacion)
  ree_embarazo <- get_adolescente + costo_energetico
  return(list(
    ree_embarazo = ree_embarazo,
    costo_energetico = costo_energetico
  ))
}

# UI de la aplicación
ui <- fluidPage(
  titlePanel("Cálculo de REE, Peso Esperado e IMC en Embarazadas"),
  sidebarLayout(
    sidebarPanel(
      numericInput("edad", "Edad de la paciente (años):", value = 25, min = 12, max = 50),
      numericInput("peso_pregestacional", "Peso pregestacional (kg):", value = 60, min = 30, max = 150),
      numericInput("talla", "Talla (m):", value = 1.65, min = 1.4, max = 2.0, step = 0.01),
      numericInput("semanas", "Semana de gestación:", value = 20, min = 1, max = 42),
      selectInput("trimestre_inicio", "Trimestre de inicio de atención:", 
                  choices = c("Primero" = 1, "Segundo" = 2)),
      selectInput("trimestre_gestacion", "Trimestre de gestación actual:", 
                  choices = c("Primero" = 1, "Segundo" = 2, "Tercero" = 3)),
      numericInput("naf", "Nivel de actividad física (NAF):", value = 1.55, min = 1.40, max = 2.40, step = 0.01),
      helpText("Referencias del Nivel de Actividad Física (NAF):",
               "1.40 - 1.69: Sedentario",
               "1.70 - 1.99: Activo",
               "2.00 - 2.40: Vigoroso"),
      actionButton("calcular", "Calcular")
    ),
    mainPanel(
      verbatimTextOutput("resultados")
    )
  )
)

# Servidor de la aplicación
server <- function(input, output) {
  observeEvent(input$calcular, {
    imc_previo <- calcular_imc(input$peso_pregestacional, input$talla)
    peso_esperado <- calcular_peso_esperado(imc_previo, input$peso_pregestacional, input$semanas)
    resultados_get <- calcular_get(input$peso_pregestacional, input$edad, input$naf)
    
    if (input$edad < 18) {
      output$resultados <- renderText({
        paste("IMC previo: ", round(imc_previo, 2),
              "\nPeso esperado: ", round(peso_esperado, 2), "kg",
              "\nSemana de gestación: ", round(input$semanas, 0),
              "\nTrimestre de gestación: ", input$trimestre_gestacion,
              "\nREE no embarazo: ", round(resultados_get$ree_no_embarazo, 2), "kcal/día",
              "\nGET del adolescente: ", round(resultados_get$get_adolescente, 2), "kcal/día")
      })
    } else {
      resultados_ree_embarazo <- calcular_ree_embarazo(resultados_get$ree_no_embarazo, input$trimestre_inicio, input$trimestre_gestacion)
      
      output$resultados <- renderText({
        paste("IMC previo: ", round(imc_previo, 2),
              "\nPeso esperado: ", round(peso_esperado, 2), "kg",
              "\nSemana de gestación: ", round(input$semanas, 0),
              "\nTrimestre de gestación: ", input$trimestre_gestacion,
              "\nREE no embarazo: ", round(resultados_get$ree_no_embarazo, 2), "kcal/día",
              "\nCosto energético del embarazo: ", round(resultados_ree_embarazo$costo_energetico, 2), "kcal/día",
              "\nREE del embarazo: ", round(resultados_ree_embarazo$ree_embarazo, 2), "kcal/día")
      })
    }
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)