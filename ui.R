##################### VERSION FEBRERO ################################


library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(tippy)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title="Problemas de genética", titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(
      HTML(paste0(
        "<br>",
        "<center>",
        tags$div(img(src = "logouib.png", width = 100, height = 100, align = "position:center")),
        "</center>",
        "<p style = 'text-align: center;'><small><a >Universitat de les Illes Balears</a></small></p>",
        "<br>"
      )),
      menuItem("Estructura de los ácidos nucléicos", tabName = "tema1", icon = icon("1")),
      menuItem("Mendelismo", tabName = "tema2", icon = icon("2")),
      menuItem("Mapeo y ligamento", tabName = "tema3", icon = icon("3")),
      menuItem("Genética de poblaciones", tabName = "tema4", icon = icon("4"))
    )
  ),
  dashboardBody(
    tabItems(
      #Tab 1
      tabItem(tabName = "tema1", 
              h2("Estructura de los ácidos nucléicos", align = "left"),
              h6("Créditos:", tippy::tippy(text = "Nerea Pareja Mas",
                                           tooltip = "Departamento de Ciencias Matemáticas e Informática. Área de Ciencias de la Computación e Inteligencia Artificial.",
                                           arrow = TRUE, placement = "auto"), ","
                 , tippy::tippy(text = "Arnau Mir Torres",
                                tooltip = "Departamento de Ciencias Matemáticas e Informática. Área de Ciencias de la Computación e Inteligencia Artificial.",
                                arrow = TRUE, placement = "auto"), ","
                 , tippy::tippy(text = "Carlos Juan Clar",
                                tooltip = "Departamento de Biología. Área de Genética.",
                                arrow = TRUE, placement = "auto"), ","
                 , tippy::tippy(text = "José A. Jurado-Rivera",
                                tooltip = "Departamento de Biología. Área de Genética.",
                                arrow = TRUE, placement = "auto")),
              
              fluidRow(
                
                box(sliderInput("pb", "Pares de bases", value = 10^6, min = 0.1*10^5, max = 10^7),
                    sliderInput("por", "Porcentaje de citosinas", value = 25, min = 0, max = 50)),
                
                tabBox(
                  title = "Ejercicio 1", id = "tabset1",
                  
                  tabPanel("Preguntas",
                           p(style="text-align: justify;","El material hereditario de una bacteria es una molécula de ADN de doble hélice circular que presenta
                 el número de pares de bases y el porcentaje de citosinas marcado."),
                           numericInput("num1",p(style="text-align: justify;","a) ¿Cuál es la longitud en micras del ADN de la bacteria?", "(utilizar exactamente tres decimales para todos los cálculos)"), value = 0, min = -Inf, max = Inf),
                           htmlOutput("text1"),
                           
                           numericInput("num2", p(style="text-align: justify;","b) ¿Cuántas moléculas de adenina habrá en 1 mm del ADN de la bacteria?"), value = 0, min = -Inf, max = Inf),
                           htmlOutput("text2")
                           
                  ),
                  
                  tabPanel("Soluciones",
                           p("Pulsa este botón para conocer la solución de la pregunta a):"),
                           actionButton("submit1", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000", align = "left"),
                           uiOutput("text3"),
                           p("Pulsa este botón para conocer la solución de la pregunta b):"),
                           actionButton("submit2", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text4"),
                           actionButton("reset", "Borrar", icon("broom"), align = "right")
                  ),
                )
              ),
              fluidRow(
                box(sliderInput("cociente", "Valor del cociente", value = 0.5, min = 0, max = 1)),
                
                tabBox(
                  title = "Ejercicio 2", id = "tabset2",
                  tabPanel("Preguntas",
                           p(style="text-align: justify;","El cociente (A+G)/(T+C) en una sola cadena de un ADN es el marcado."),
                           numericInput("num3", p(style="text-align: justify;","a) ¿Cuál será este valor en la cadena complementaria?", "(utilizar exactamente tres decimales para todos los cálculos)"), value = 0, min = 0, max = 1),
                           htmlOutput("text5"),
                           
                           numericInput("num4", p(style="text-align: justify;","b) ¿Cuál será este valor en la molécula en su forma de doble cadena?"), value = 0, min = 0, max = 1),
                           htmlOutput("text7"),
                           
                           numericInput("num5", p(style="text-align: justify;","c) Siendo el cociente (G+C)/(A+T) el marcado en una cadena, ¿cuál es el cociente de la cadena complementaria?"), value = 0, min = 0, max = 1),
                           htmlOutput("text9"),
                           
                           numericInput("num6", p(style="text-align: justify;","d) ¿Y en la molécula en su forma doble cadena?"), value = 0, min = 0, max = 1),
                           htmlOutput("text11"),
                  ),
                  
                  tabPanel("Soluciones",
                           p("Pulsa este botón para conocer la solución de la pregunta a):"),
                           actionButton("submit3", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text6"),
                           p("Pulsa este botón para conocer la solución de la pregunta b):"),
                           actionButton("submit4", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text8"),
                           p("Pulsa este botón para conocer la solución de la pregunta c):"),
                           actionButton("submit5", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text10"),
                           p("Pulsa este botón para conocer la solución de la pregunta d):"),
                           actionButton("submit6", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text12"),
                           actionButton("reset2", "Borrar", icon("broom"), align = "right")
                  )
                ) #end tabBox
              ), #end fluidrow
              
              fluidRow(
                box(sliderInput("gr", "Gramos", value = 10^-4, min = 10^-5 , max = 10^-3)),
                
                tabBox(
                  title = "Ejercicio 3", id = "tabset3",
                  tabPanel("Pregunta",
                           numericInput("num7", p(style="text-align: justify;","Determina la longitud en centímetros de una molécula de ADN cuyo peso es los gramos marcados."), value = 0, min = -Inf, max = Inf),
                           htmlOutput("text13")
                        ),
                  
                  tabPanel("Solución",
                           p("Pulsa este botón para conocer la solución:"),
                           actionButton("submit7", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text14"),
                           actionButton("reset3", "Borrar", icon("broom"), align = "right")
                  )
                ) #end tabBox
                
              ), #end fluidrow
              
              ),
      #Tab 2
      tabItem(tabName = "tema2", 
              h2("Mendelismo", align = "left"),
              h6("Créditos:", tippy::tippy(text = "Nerea Pareja Mas",
                                           tooltip = "Departamento de Ciencias Matemáticas e Informática. Área de Ciencias de la Computación e Inteligencia Artificial.",
                                           arrow = TRUE, placement = "auto"), ","
                 , tippy::tippy(text = "Arnau Mir Torres",
                                tooltip = "Departamento de Ciencias Matemáticas e Informática. Área de Ciencias de la Computación e Inteligencia Artificial.",
                                arrow = TRUE, placement = "auto"), ","
                 , tippy::tippy(text = "Carlos Juan Clar",
                                tooltip = "Departamento de Biología. Área de Genética.",
                                arrow = TRUE, placement = "auto"), ","
                 , tippy::tippy(text = "José A. Jurado-Rivera",
                                tooltip = "Departamento de Biología. Área de Genética.",
                                arrow = TRUE, placement = "auto")),
              
              fluidRow(
                
                box(selectInput("hijos", p(style="text-align: justify;","¿Cuántos hijos tiene la familia?"), c(3,4,5,6,7), multiple = FALSE)),
                
                tabBox(
                  title = "Ejercicio 1", id = "tabset4",
                  
                  tabPanel("Preguntas",
                           p(style="text-align: justify;","Supón que el color azul de los ojos en una especie animal está determinado por un gen recesivo frente a su alelo
                     para color pardo, y que el pelo rubio está determinado por un gen recesivo frente a su alelo para color oscuro. Ambos
                     genes segregan independientemente. En familias en las que tanto el padre como la madre son heterocigotos para estos genes, 
                     calcular la probabilidad de que: (utilizar exactamente tres decimales para todos los cálculos)"),
                           htmlOutput("enunciado"),
                           
                           numericInput("num8","", value = 0, min = -Inf, max = Inf),
                           htmlOutput("text15"),
                           
                           numericInput("num9", p(style="text-align: justify;","b) Al menos uno de los hijos tenga los ojos azules."), value = 0, min = -Inf, max = Inf),
                           htmlOutput("text17"),
                           
                           numericInput("num10", p(style="text-align: justify;","c) Al menos uno de los hijos sea de pelo rubio y ojos azules."), value = 0, min = -Inf, max = Inf),
                           htmlOutput("text19"),
                           
                           numericInput("num11", p(style="text-align: justify;","d) Un hijo sea de pelo oscuro y ojos azules; otro de pelo oscuro y ojos pardos; y el resto de pelo rubio y ojos pardos."), value = 0, min = -Inf, max = Inf),
                           htmlOutput("text21"),
                  ),
                  
                  tabPanel("Soluciones",
                           p("Pulsa este botón para conocer la solución de la pregunta a):"),
                           actionButton("submit8", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text16"),
                           
                           p("Pulsa este botón para conocer la solución de la pregunta b):"),
                           actionButton("submit9", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text18"),
                           
                           p("Pulsa este botón para conocer la solución de la pregunta c):"),
                           actionButton("submit10", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text20"),
                           
                           p("Pulsa este botón para conocer la solución de la pregunta d):"),
                           actionButton("submit11", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text22"),
                           actionButton("reset4", "Borrar", icon("broom"), align = "right")
                  ), #end tabPanel
                ) #end tabBox
              ), #end fluidRow
              
              fluidRow(
                
                box(p("Pulsa el botón para generar el enunciado del ejercicio 2."),
                    actionButton("submit12", label = "Generar enunciado", icon("play-circle"), 
                                 style="color: white; background-color: #555555; border-color: #555555"),
                    htmlOutput("text23")),
                
                tabBox(
                  title = "Ejercicio 2", id = "tabset5",
                  
                  tabPanel("Pregunta",
                           
                           checkboxGroupInput("hipotesis", "a) Emitir una hipótesis del modo de herencia del caracter albino (Selecciona solo una respuesta) ", choices = c("Herencia autosómica dominante (albino > normal)", "Herencia autosómica recesiva (normal > albino)", "Herencia ligada al sexo dominante (albino > normal)", "Herencia ligada al sexo recesiva (normal > albino)"), selected = NULL),
                           htmlOutput("text24a"),
                
                           numericInput("num12","b) ¿Qué valor de χ2 has obtenido? (utilizar exactamente tres decimales para todos los cálculos)", value = 0, min = -Inf, max = Inf),
                           htmlOutput("text24"),
                  ),
                  
                  tabPanel("Solución",
                           
                           p("Pulsa este botón para conocer la solución de la pregunta a):"),
                           actionButton("submit13a", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text25a"),
                          
                           p("Pulsa este botón para conocer la solución de la pregunta b):"),
                           actionButton("submit13", label = "Me rindo", icon("play-circle"), 
                                      style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           
                           uiOutput("text25"),
                           actionButton("reset5", "Borrar", icon("broom"), align = "right")
                  ), #end tabPanel
                ) #end tabBox
              ), #end fluidRow
              
              fluidRow(
                
                box(p("Pulsa el botón para generar el enunciado del ejercicio 3."),
                    actionButton("submit14", label = "Generar enunciado", icon("play-circle"), 
                                 style="color: white; background-color: #555555; border-color: #555555"),
                    htmlOutput("text26")),
                
                tabBox(
                  title = "Ejercicio 3", id = "tabset6",
                  
                  tabPanel("Preguntas",
                           
                           checkboxGroupInput("recesivo", "a) ¿Cuál de dichos caracteres fenotípicos es más probable que esté causado por un homocigoto recesivo? (Selecciona solo una respuesta) ", choices = c("Caracter rojo", "Caracter negro"), selected = NULL),
                           htmlOutput("text27"),
                           
                           numericInput("num13","b) Segun tu hipótesis, ¿cuántos individuos de cada clase habrías esperado?", value = 0, min = -Inf, max = Inf),
                           htmlOutput("text29"),
                           
                           numericInput("num14","c) Prueba la hipótesis por el método de la χ2. ¿Qué valor has obtenido? (Usa tres decimales)", value = 1, min = -Inf, max = Inf),
                           radioButtons(inputId="conclusion", label="Señala la conclusión que creas correcta.", c("Existen evidencias estadísticas significativas para rechazar la hipótesis", "No existen evidencias estadísticas significativas para rechazar la hipótesis"),selected = character(0)),
                           htmlOutput("text31"),
                  ),
                  
                  tabPanel("Soluciones",
                           p("Pulsa este botón para conocer la solución de la pregunta a):"),
                           actionButton("submit15", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text28"),
                           
                           p("Pulsa este botón para conocer la solución de la pregunta b):"),
                           actionButton("submit16", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text30"),
                           
                           p("Pulsa este botón para conocer la solución de la pregunta c):"),
                           actionButton("submit17", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text32"),
                           actionButton("reset6", "Borrar", icon("broom"), align = "right")
                  ), #end tabPanel
                ) #end tabBox
              ), #end fluidRow
      ),
      
      #Tab 3 
      tabItem(tabName = "tema3",
              h2("Mapeo y ligamiento", align = "left"),
              h6("Créditos:", tippy::tippy(text = "Nerea Pareja Mas",
                                           tooltip = "Departamento de Ciencias Matemáticas e Informática. Área de Ciencias de la Computación e Inteligencia Artificial.",
                                           arrow = TRUE, placement = "auto"), ","
                 , tippy::tippy(text = "Arnau Mir Torres",
                                tooltip = "Departamento de Ciencias Matemáticas e Informática. Área de Ciencias de la Computación e Inteligencia Artificial.",
                                arrow = TRUE, placement = "auto"), ","
                 , tippy::tippy(text = "Carlos Juan Clar",
                                tooltip = "Departamento de Biología. Área de Genética.",
                                arrow = TRUE, placement = "auto"), ","
                 , tippy::tippy(text = "José A. Jurado-Rivera",
                                tooltip = "Departamento de Biología. Área de Genética.",
                                arrow = TRUE, placement = "auto")),
              
              fluidRow(
                
                box(p(style="text-align: justify;","En una especie de coleóptero se han caracterizado tres genes ligados, cada uno de ellos con alelos recesivos que producen los fenotipos:
            1) élitros deformados (alelo h), 2) patas con longitud mayor (alelo a) y 3) color rojizo (alelo g). Los correspondientes alelos silvestres 
            determinan élitros y patas normales (alelos h+ y a+, respectivamente), y color negro (g+). En un cruce de una hembra heterocigótica de tipo silvestre
            para los tres caracteres con un macho, también de tipo silvestre, se obtuvo una descendencia fenotípicamente en la que todas las hembras fueron 
            de tipo silvestre y los machos como sigue:"),
                    p("Pulsa el botón para generar los datos del problema."),
                    actionButton("datos", label = "Generar datos", icon("play-circle"), 
                                 style="color: white; background-color: #555555; border-color: #555555"),
                    tableOutput("table1")),
                
                tabBox(
                  title = "Ejercicio 1", id = "tabset7",
                  
                  tabPanel("Preguntas",
                           
                           checkboxGroupInput("centro", "a) ¿Qué gen tiene una situación central? (Selecciona solo una respuesta) ", choices = c("Gen a", "Gen g", "Gen h"), selected = NULL),
                           htmlOutput("text33"),
                           
                           helpText("Para los siguientes apartados utilizar exactamente tres decimales para dar las soluciones."),
                           
                           numericInput("num15","b) Determina la distancia entre los loci h y a. ", value = 0, min = -Inf, max = Inf),
                           htmlOutput("text35"),
                           
                           numericInput("num16","c) Determina la distancia entre los loci h y g. ", value = 0, min = -Inf, max = Inf),
                           htmlOutput("text37"),
                           
                           numericInput("num17","d) ¿Cuál es el valor del coeficiente de interferencia?", value = 1, min = -Inf, max = Inf),
                           htmlOutput("text39")
                  ),
                  
                  tabPanel("Soluciones",
                           p("Pulsa este botón para conocer la solución de la pregunta a):"),
                           actionButton("submit18", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text34"),
                           tableOutput("table2"),
                           
                           p("Pulsa este botón para conocer la solución de la pregunta b):"),
                           actionButton("submit19", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text36"),
                           tableOutput("table3"),
                           
                           p("Pulsa este botón para conocer la solución de la pregunta c):"),
                           actionButton("submit20", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text38"),
                           
                           p("Pulsa este botón para conocer la solución de la pregunta d):"),
                           actionButton("submit21", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text40"),
                           
                           actionButton("reset7", "Borrar", icon("broom"), align = "right")
                  ), #end tabPanel
                ) #end tabBox
              ), #end fluidRow
              
              fluidRow(
                
                box(p(style="text-align: justify;","Se realizó un cruce de Neurospora entre una cepa que portaba los alelos silvestres ad+ y T+, y otra cepa
                      que portaba los alelos mutantes ad y T. Se aislaron 100 tétradas, y tras su visualización en el microscopio se pudieron clasificar en las siguientes
                      siete clases:"),
                    imageOutput("neurospora")
                    ),
                
                tabBox(
                  title = "Ejercicio 2", id = "tabset8",
                  
                  tabPanel("Preguntas",
                           
                           checkboxGroupInput("ligados", "a) ¿Están ligados los genes ad y T? (Selecciona solo una respuesta) ", choices = c("Si", "No"), selected = NULL),
                           htmlOutput("text50"),
                                                  
                           checkboxGroupInput("brazo","b) Si lo están, ¿se encuentran en el mismo brazo cromosómico o en distinto? (Selecciona solo una respuesta) ", choices = c("Se encuentran en el mismo brazo.", "Se encuentran en brazos distintos." ), selected = NULL),
                           htmlOutput("text51"),
                           
                           textInput("num19","c) Calcula la distancia de cada uno de los dos genes al centrómero.", value = c("ad T")),
                           helpText("Indicación: escribir las distancias del gen ad y del gen T al centrómero, en este orden, separadas por un espacio en blanco, redondeadas a 3 cifras decimales y separando los decimales por un punto."),
                           htmlOutput("text52"),
                           
                  ),
                  
                  tabPanel("Soluciones",
                           p("Pulsa este botón para conocer la solución de la pregunta a):"),
                           actionButton("submit27", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text53"),
                           
                           p("Pulsa este botón para conocer la solución de la pregunta b):"),
                           actionButton("submit28", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text54"),
                           
                           p("Pulsa este botón para conocer la solución de la pregunta c):"),
                           actionButton("submit29", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text55"),
                           
                           actionButton("reset10", "Borrar", icon("broom"), align = "right")
                  ), #end tabPanel
                ) #end tabBox
              ), #end fluidRow
              
              
      ),
      
      #Tab 4
      tabItem(tabName = "tema4",
              h2("Genética de poblaciones", align = "left"),
              h6("Créditos:", tippy::tippy(text = "Nerea Pareja Mas",
                                           tooltip = "Departamento de Ciencias Matemáticas e Informática. Área de Ciencias de la Computación e Inteligencia Artificial.",
                                           arrow = TRUE, placement = "auto"), ","
                 , tippy::tippy(text = "Arnau Mir Torres",
                                tooltip = "Departamento de Ciencias Matemáticas e Informática. Área de Ciencias de la Computación e Inteligencia Artificial.",
                                arrow = TRUE, placement = "auto"), ","
                 , tippy::tippy(text = "Carlos Juan Clar",
                                tooltip = "Departamento de Biología. Área de Genética.",
                                arrow = TRUE, placement = "auto"), ","
                 , tippy::tippy(text = "José A. Jurado-Rivera",
                                tooltip = "Departamento de Biología. Área de Genética.",
                                arrow = TRUE, placement = "auto")),
              
              fluidRow(
                
                box(p(style="text-align: justify;","Se hizo un cribado genético para el locus autosómico de la hemoglobina β de una población de recién nacidos en Musona, Tanzania.
                     Este locus cuenta con dos alelos, A y S. Los resultados fueron:"),
                    p("Pulsa el botón para generar los datos del problema."),
                    actionButton("datos1", label = "Generar datos", icon("play-circle"), 
                                 style="color: white; background-color: #555555; border-color: #555555"),
                    tableOutput("table4")),
                
                tabBox(
                  title = "Ejercicio 1", id = "tabset9",
                  
                  tabPanel("Preguntas",
                           
                           textInput("frecuencias", "a) Calcula las frecuencias genotípicas y las frecuencias alélicas para A y S en esta población.", value = "f(AA) f(AS) f(SS) p q"),
                           helpText("Indicación: escribir las frecuencias en orden f(AA), f(AS), f(SS), p, q, separadas por un espacio en blanco, redondeadas a 3 cifras decimales y separando los decimales por un punto."),
                           htmlOutput("text41"),
                           
                           textInput("frecuencias_hw","b) ¿Cuáles deberían ser las frecuencias genotípicas en caso de estar la población en equilibrio Hardy-Weinberg? ", value = "f(AA) f(AS) f(SS)"),
                           helpText("Indicación: escribir las frecuencias en orden f(AA), f(AS), f(SS) separadas por un espacio en blanco, redondeadas a 3 cifras decimales y separando los decimales por un punto."),
                           htmlOutput("text43"),
                           
                           numericInput("equilibrio", "c) Comprueba estadísticamente si la población se encuentra en situación de equilibrio Hardy-Weinberg.Introduce el valor de chi-cuadrado redondeado a tres decimales.", value = 0),
                           radioButtons(inputId="conclusion2", label="Señala la conclusión que creas correcta.", c("Existen evidencias estadísticas significativas para rechazar la hipótesis", "No existen evidencias estadísticas significativas para rechazar la hipótesis"),selected = character(0)),
                           htmlOutput("text45")
                           
                  ),
                  
                  tabPanel("Soluciones",
                           p("Pulsa este botón para conocer la solución de la pregunta a):"),
                           actionButton("submit22", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text42"),
                           
                           p("Pulsa este botón para conocer la solución de la pregunta b):"),
                           actionButton("submit23", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text44"),
                           
                           p("Pulsa este botón para conocer la solución de la pregunta c):"),
                           actionButton("submit24", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text46"),
                           
                           actionButton("reset8", "Borrar", icon("broom"), align = "right")
                  ), #end tabPanel
                ) #end tabBox
              ), #end fluidRow
              
              fluidRow(
                
                box(p("Pulsa el botón para generar el enunciado del ejercicio 2."),
                    actionButton("submit25", label = "Generar enunciado", icon("play-circle"), 
                                 style="color: white; background-color: #555555; border-color: #555555"),
                    htmlOutput("text47")),
                
                tabBox(
                  title = "Ejercicio 2", id = "tabset10",
                  
                  tabPanel("Pregunta",
                           
                           textInput("num18","Calcula las frecuencias de los alelos A, B y 0 suponiendo que la población está en equilibrio para este locus.", value = "f(A)  f(B)  f(0)"),
                           helpText("Indicación: escribir las frecuencias en orden f(A), f(B), f(0) separadas por un espacio en blanco, redondeadas a 3 cifras decimales y separando los decimales por un punto."),
                           htmlOutput("text48"),
                           
                  ),
                  
                  tabPanel("Solución",
                           p("Pulsa este botón para conocer la solución:"),
                           actionButton("submit26", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text49"),
                           tableOutput("table5"),
                           
                           actionButton("reset9", "Borrar", icon("broom"), align = "right")
                  ), #end tabPanel
                ) #end tabBox
              ), #end fluidRow
              
              fluidRow(
                
                box(p("Pulsa el botón para generar el enunciado del ejercicio 3."),
                    actionButton("submit32", label = "Generar enunciado", icon("play-circle"), 
                                 style="color: white; background-color: #555555; border-color: #555555"),
                    htmlOutput("text60")),
                
                tabBox(
                  title = "Ejercicio 3", id = "tabset11",
                  
                  tabPanel("Pregunta",
                           numericInput("num20", "¿Cuál será la frecuencia del alelo A?", value = 0, max = Inf, min = -Inf),
                           helpText("Indicación: escribir la frecuencia redondeada a 5 cifras decimales."),
                           htmlOutput("text56")
                  ), #end tabPanel
                  
                  tabPanel("Solución",
                           p("Pulsa este botón para conocer la solución:"),
                           actionButton("submit30", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text57"),
                           plotOutput("plot"),
                           
                           actionButton("reset11", "Borrar", icon("broom"), align = "right")
                  ), #end tabPanel
                ) #end tabBox
              ), #end fluidRow
              
              fluidRow(
                
                box(p("Pulsa el botón para generar el enunciado del ejercicio 4."),
                    actionButton("submit33", label = "Generar enunciado", icon("play-circle"), 
                                 style="color: white; background-color: #555555; border-color: #555555"),
                    htmlOutput("text61")),
                
                tabBox(
                  title = "Ejercicio 4", id = "tabset12",
                  
                  tabPanel("Pregunta",
                           textInput("frec_mut", "¿Cuál será la frecuencia de cada uno de los genotipos para este locus cuando la población alcance el equilibrio?", value = "f(AA) f(Aa) f(aa)"),
                           helpText("Indicación: escribir las frecuencias en orden f(AA), f(Aa), f(aa) separadas por un espacio en blanco, redondeadas a 5 cifras decimales y separando los decimales por un punto."),
                           htmlOutput("text58"),
                  ), #end tabPanel
                  
                  tabPanel("Solución",
                           p("Pulsa este botón para conocer la solución:"),
                           actionButton("submit31", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text59"),
                           
                           actionButton("reset12", "Borrar", icon("broom"), align = "right")
                  ), #end tabPanel
                ) #end tabBox
              ), #end fluidRow
              
              fluidRow(
                
                box(p("Pulsa el botón para generar el enunciado del ejercicio 5."),
                    actionButton("submit34", label = "Generar enunciado", icon("play-circle"), 
                                 style="color: white; background-color: #555555; border-color: #555555"),
                    htmlOutput("text62")),
                
                tabBox(
                  title = "Ejercicio 5", id = "tabset13",
                  
                  tabPanel("Preguntas",
                           numericInput("num21", "a) ¿Cuál será la frecuencia del alelo en cuestión una generación después?", value = 0, min = -Inf, max = Inf),
                           helpText("Indicación: escribir la respuesta redondeada a 3 cifras decimales."),
                           htmlOutput("text63"),
                           
                           numericInput("num22", "b) ¿Cuál será después de cinco generaciones?", value = 0, min = -Inf, max = Inf),
                           helpText("Indicación: escribir la respuesta redondeada a 3 cifras decimales."),
                           htmlOutput("text64"),
                           
                  ), #end tabPanel
                  
                  tabPanel("Soluciones",
                           p("Pulsa este botón para conocer la solución del apartado a):"),
                           actionButton("submit35", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text65"),
                           plotOutput("plot1"),
                           
                           p("Pulsa este botón para conocer la solución del apartado b):"),
                           actionButton("submit36", label = "Me rindo", icon("play-circle"), 
                                        style="color: #fff; background-color: #FF0000; border-color: #FF0000"),
                           uiOutput("text66"),
                           plotOutput("plot2"),
                           
                           actionButton("reset13", "Borrar", icon("broom"), align = "right")
                  ), #end tabPanel
                ) #end tabBox
              ), #end fluidRow
      )
  
    )
  )
)



#shinyApp(ui = ui, server = server)
