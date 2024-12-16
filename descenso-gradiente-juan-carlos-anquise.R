# Carga las bibliotecas necesarias
library(shiny)
library(ggplot2)
library(DT) # Para tablas interactivas
library(plotly) # Para gráficos 3D

# UI de la aplicación
ui <- fluidPage(
  titlePanel(
    div(
      img(src = "https://www.r-project.org/logo/Rlogo.png", height = "50px"), # URL de imagen
      "Visualización del Descenso de Gradiente - Juan Carlos Anquise Vargas"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      h3("Configuración del Descenso de Gradiente"),
      textInput("custom_function", 
                "Ingresa la función objetivo (en términos de x):", 
                value = "x^2"),
      numericInput("learning_rate", 
                   "Tasa de aprendizaje (α):", 
                   value = 0.1, 
                   min = 0.001, 
                   max = 1, 
                   step = 0.001),
      numericInput("tolerance", 
                   "Tolerancia (ε):", 
                   value = 1e-6, 
                   min = 1e-10, 
                   max = 1e-2, 
                   step = 1e-7),
      numericInput("max_iterations", 
                   "Máximo de iteraciones:", 
                   value = 1000, 
                   min = 1, 
                   max = 10000, 
                   step = 1),
      numericInput("start_point", 
                   "Punto inicial (x₀):", 
                   value = 5, 
                   step = 0.1),
      selectInput("gradient_type", 
                  "Tipo de Descenso de Gradiente:", 
                  choices = c("Batch" = "batch", 
                              "Estocástico (SGD)" = "sgd", 
                              "Mini-batch" = "mini_batch")),
      numericInput("mini_batch_size",
                   "Tamaño del Mini-batch (para Mini-batch):",
                   value = 5,
                   min = 1),
      actionButton("start", "Iniciar Descenso de Gradiente"),
      hr(),
      h4("Instrucciones"),
      p("1. Ingresa una función objetivo personalizada."),
      p("2. Ajusta los hiperparámetros como la tasa de aprendizaje, tolerancia, el máximo de iteraciones y el tipo de descenso."),
      p("3. Ingresa un punto inicial (x₀) y haz clic en 'Iniciar Descenso de Gradiente'."),
      p("4. Observa cómo el algoritmo se aproxima al mínimo y revisa la tabla de iteraciones."),
      p("Nota: Asegúrate de usar 'x' como variable en la función y de que la función sea matemáticamente correcta.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Resultados",
                 h4("Tabla de Iteraciones"),
                 DT::dataTableOutput("results_table"),
                 verbatimTextOutput("result")
        ),
        tabPanel("Visualización",
                 h4("Descenso de Gradiente"),
                 plotOutput("gradientPlot"),
                 verbatimTextOutput("min_max_text"),
                 plotOutput("learningCurvePlot")
        ),
        tabPanel("3D Visualización",
                 h4("Función en 3D"),
                 plotlyOutput("plot3d")
        )
      )
    )
  )
)

# Define el servidor (Server)
server <- function(input, output, session) {
  # Reactivo para almacenar los resultados del descenso de gradiente
  gradient_descent <- reactiveVal(list(x_vals = NULL, f_vals = NULL, losses = NULL, gradients = NULL, min_point = NULL, max_point = NULL))
  
  # Observador para iniciar el descenso de gradiente al presionar el botón
  observeEvent(input$start, {
    tryCatch({
      # Evalúa la función ingresada por el usuario
      f <- function(x) eval(parse(text = input$custom_function))
      
      # Gradiente aproximado usando diferencias finitas
      grad <- function(x) {
        h <- 1e-4
        (f(x + h) - f(x)) / h
      }
      
      learning_rate <- input$learning_rate
      tolerance <- input$tolerance
      max_iterations <- input$max_iterations
      x <- input$start_point
      gradient_type <- input$gradient_type
      mini_batch_size <- input$mini_batch_size
      
      set.seed(123)
      data <- runif(1000, -100, 100)
      
      x_vals <- numeric(max_iterations)
      f_vals <- numeric(max_iterations)
      losses <- numeric(max_iterations)
      gradients <- numeric(max_iterations)
      
      for (i in 1:max_iterations) {
        x_vals[i] <- x
        f_vals[i] <- f(x)
        gradients[i] <- grad(x)
        
        if (gradient_type == "batch") {
          grad_value <- grad(x)
        } else if (gradient_type == "sgd") {
          sample_point <- sample(data, 1)
          grad_value <- grad(sample_point)
        } else if (gradient_type == "mini_batch") {
          mini_batch <- sample(data, mini_batch_size)
          grad_value <- mean(sapply(mini_batch, grad))
        }
        
        x <- x - learning_rate * grad_value
        losses[i] <- f(x)
        
        if (i > 1 && abs(x - x_vals[i-1]) < tolerance) {
          x_vals <- x_vals[1:i]
          f_vals <- f_vals[1:i]
          losses <- losses[1:i]
          gradients <- gradients[1:i]
          break
        }
      }
      
      min_index <- which.min(f_vals)
      max_index <- which.max(f_vals)
      
      gradient_descent(list(
        x_vals = x_vals, 
        f_vals = f_vals, 
        losses = losses, 
        gradients = gradients,
        min_point = list(x = x_vals[min_index], y = f_vals[min_index]),
        max_point = list(x = x_vals[max_index], y = f_vals[max_index])
      ))
    }, error = function(e) {
      showNotification(paste("Error en la función ingresada:", e$message), type = "error")
    })
  })
  # Generar el gráfico del descenso
  output$gradientPlot <- renderPlot({
    res <- gradient_descent()
    if (is.null(res$x_vals)) return(NULL)
    
    x_vals <- res$x_vals
    f_vals <- res$f_vals
    
    x_range <- seq(min(x_vals) - 10, max(x_vals) + 10, length.out = 1000)
    f <- eval(parse(text = paste0("function(x) {", input$custom_function, "}")))
    y_range <- f(x_range)
    
    ggplot() +
      geom_line(aes(x = x_range, y = y_range), color = "blue", size = 1) +
      geom_point(aes(x = x_vals, y = f_vals), color = "red", size = 2) +
      labs(title = "Descenso de Gradiente",
           x = "x",
           y = "f(x)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$min_max_text <- renderText({
    res <- gradient_descent()
    if (is.null(res$min_point) || is.null(res$max_point)) {
      return("Calculando mínimos y máximos...")
    }
    paste("Mínimo entre puntos rojos: x =", round(res$min_point$x, 4), ", f(x) =", round(res$min_point$y, 4),
          "\nMáximo entre puntos rojos: x =", round(res$max_point$x, 4), ", f(x) =", round(res$max_point$y, 4))
  })
  
  output$learningCurvePlot <- renderPlot({
    res <- gradient_descent()
    if (is.null(res$losses)) return(NULL)
    
    losses <- res$losses
    
    ggplot(data.frame(iteration = 1:length(losses), loss = losses), aes(x = iteration, y = log(loss+1e-10))) + 
      geom_line(color = "green", size = 1) +
      labs(title = "Curva de Aprendizaje (log escala)",
           x = "Iteración",
           y = "Logaritmo del valor de la función") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Gráfico 3D
  output$plot3d <- renderPlotly({
    f <- function(x, y) eval(parse(text = gsub("x", "sqrt(x^2 + y^2)", input$custom_function)))
    x <- seq(-10, 10, length.out = 100)
    y <- seq(-10, 10, length.out = 100)
    z <- outer(x, y, f)
    
    plot_ly(x = ~x, y = ~y, z = ~z, type = "surface") %>%
      layout(scene = list(
        xaxis = list(title = "x"),
        yaxis = list(title = "y"),
        zaxis = list(title = "f(x, y)")
      ))
  })
  
  # Tabla de resultados de iteraciones
  output$results_table <- DT::renderDataTable({
    res <- gradient_descent()
    if (is.null(res$x_vals)) return(DT::datatable(data.frame()))
    
    df <- data.frame(
      Iteración = 1:length(res$x_vals),
      x = round(res$x_vals, 4),
      "f(x)" = round(res$f_vals, 4),
      Gradiente = round(res$gradients, 4),
      Pérdida = round(res$losses, 4)
    )
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Texto de resultado final
  output$result <- renderText({
    res <- gradient_descent()
    if (is.null(res$x_vals)) return("No hay resultados aún.")
    
    last_x <- res$x_vals[length(res$x_vals)]
    last_f <- res$f_vals[length(res$f_vals)]
    paste("Resultado final: x =", round(last_x, 4), ", f(x) =", round(last_f, 4))
  })
  
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)