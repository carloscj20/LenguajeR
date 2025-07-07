# Instala si no los tienes:
# install.packages(c("shiny", "readxl", "ggplot2", "DT", "dplyr", "tidyr", "openxlsx"))

library(shiny)
library(readxl)
library(ggplot2)
library(DT)
library(dplyr)
library(tidyr)
library(openxlsx)

ui <- fluidPage(
  titlePanel("Análisis de Aguas Subterráneas - Región de Tarapacá"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo", "Sube archivo Excel", accept = ".xlsx"),
      downloadButton("descargar", "Guardar Resultados")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Frecuencia por Localización", DTOutput("tabla_frecuencia")),
        tabPanel("Estadísticas por Localización", DTOutput("tabla_estadisticas")),
        tabPanel("Gráfico de Frecuencia", plotOutput("grafico_barras")),
        tabPanel("Tendencia Anual", plotOutput("grafico_linea"))
      )
    )
  )
)

server <- function(input, output) {
  
  datos <- reactive({
    req(input$archivo)
    df <- read_excel(input$archivo$datapath)
    
    # Normaliza nombres de columnas
    names(df) <- tolower(gsub(" ", "_", iconv(names(df), from = "UTF-8", to = "ASCII//TRANSLIT")))
    
    df
  })
  
  # Tabla de frecuencia
  output$tabla_frecuencia <- renderDT({
    df <- datos()
    freq <- df %>%
      count(localizacion, name = "frecuencia") %>%
      arrange(desc(frecuencia))
    datatable(freq)
  })
  
  # Estadísticas descriptivas
  output$tabla_estadisticas <- renderDT({
    df <- datos()
    stats <- df %>%
      group_by(localizacion) %>%
      summarise(
        media = mean(`profundidad_(m)`, na.rm = TRUE),
        mediana = median(`profundidad_(m)`, na.rm = TRUE),
        moda = as.numeric(names(sort(table(`profundidad_(m)`), decreasing = TRUE))[1]),
        desviacion_std = sd(`profundidad_(m)`, na.rm = TRUE),
        varianza = var(`profundidad_(m)`, na.rm = TRUE)
      )
    datatable(stats)
  })
  
  # Gráfico de barras
  output$grafico_barras <- renderPlot({
    df <- datos()
    freq <- df %>%
      count(localizacion)
    
    ggplot(freq, aes(x = reorder(localizacion, -n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = n), vjust = -0.5) +
      labs(x = "Localización", y = "Frecuencia", title = "Frecuencia por Localización") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Gráfico de tendencia anual
  output$grafico_linea <- renderPlot({
    df <- datos()
    req("ano_de_monitoreo" %in% names(df))
    
    tendencia <- df %>%
      group_by(ano_de_monitoreo, localizacion) %>%
      summarise(profundidad_prom = mean(`profundidad_(m)`, na.rm = TRUE)) %>%
      ungroup()
    
    ggplot(tendencia, aes(x = ano_de_monitoreo, y = profundidad_prom, color = localizacion)) +
      geom_line() +
      geom_point() +
      labs(title = "Tendencia de Profundidad Promedio por Año",
           x = "Año de Monitoreo", y = "Profundidad (m)") +
      theme_minimal()
  })
  
  # Botón de descarga
  output$descargar <- downloadHandler(
    filename = function() {
      paste0("resultado_aguas_subterraneas_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- datos()
      
      freq <- df %>% count(localizacion)
      stats <- df %>%
        group_by(localizacion) %>%
        summarise(
          media = mean(`profundidad_(m)`, na.rm = TRUE),
          mediana = median(`profundidad_(m)`, na.rm = TRUE),
          moda = as.numeric(names(sort(table(`profundidad_(m)`), decreasing = TRUE))[1]),
          desviacion_std = sd(`profundidad_(m)`, na.rm = TRUE),
          varianza = var(`profundidad_(m)`, na.rm = TRUE)
        )
      
      wb <- createWorkbook()
      addWorksheet(wb, "Frecuencia")
      addWorksheet(wb, "Estadisticas")
      writeData(wb, "Frecuencia", freq)
      writeData(wb, "Estadisticas", stats)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)
