library(shiny)

server <- function(input, output,session) {
  
  # Comprobar si los paquetes necesarios están instalados y cargarlos o instalarlos
  output$checkResult <- renderText({
    requiredPackage <- c("shiny","shinydashboard","shinydashboardPlus","tippy", "ggplot2")
    
    if (!requireNamespace(requiredPackage, quietly = TRUE)) {
      install.packages(requiredPackage)
    } 
  })
  
  # Logo UIB
  
  output$logouib <- renderImage({
    return(list(
      src = "www/logouib.png",
      contentType = "image/png",
      width = 100,
      height = 100,
      align = "center",
      alt = "logo_uib"
    ))
  }, deleteFile = FALSE)
  
  #Tema 1
  #Ejercicio 1
  
  output$text1 <- renderText({
    if (input$num1 == 0) {""}
    else if (input$num1 == round(input$pb*3.4*(10^{-4}),3)) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  output$text2 <- renderText({
    ad <- round(((100-2*input$por)/2)/100,3)
    nt <- round(10^7*(1/3.4)*2,3)
    if (input$num2 == 0) {""}
    else if (input$num2 == round(nt*ad,3)) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive3 = reactiveValues(text = "")
  output$text3 <- renderUI({
    withMathJax(helpText(text_reactive3$text))
  })
  
  observeEvent(input$submit1,{
    sol1 <- round(input$pb*3.4*(10^{-4}),3)
    text_reactive3$text <-  sprintf("La solución a la primera pregunta se calcula multiplicando 
                                      el número de pares de bases de la molécula por $$3.4 \\times 10^4$$
                                      micras de separación que hay entre cada par de bases, 
                                      en este caso la respuesta correcta sería %s micras.",sol1)
  })
  
  
  text_reactive4 = reactiveValues(text = "")
  output$text4 <- renderUI({
    withMathJax(helpText(text_reactive4$text))
  })
  
  observeEvent(input$submit2,{
    ad <- round(((100-2*input$por)/2)/100,3)
    nt <- round(10^7*(1/3.4)*2,3)
    sol2 <- round(nt*ad,3)
    text_reactive4$text <-  sprintf("La solución a la segunda pregunta se calcula deduciendo en primer lugar, dado que se trata de
                                      una doble hélice, el porcentaje de adenina en la molécula a partir del dato de citosinas del enunciado
                                      mediante la expresión $$\\frac{100-2C}{2}$$ (leyes de Chargaff). En nuestro caso el enunciado 
                                      especifica un %s por ciento de citosina, por tanto habrá un %s por ciento de adenina. En segundo lugar
                                      calculamos el número de nucleótidos que hay en un milímetro de ADN de doble cadena
                                      mediante la expresión $$\\frac{10^7Å}{1mm} \\times \\frac{1pb}{3.4Å} \\times \\frac{2nt}{1pb}$$ que en nuestro caso serían
                                      %s nucleoótidos. Finalmente bastaría multiplicar el número de nucleótidos por la fracción
                                      de adeninas que hemos calculado previamente, de manera que obtenemos %s moléculas de adenina.",input$por,ad*100,nt,sol2)
  })
  
  
  observeEvent(input$reset, {
    text_reactive3$text <- NULL
    text_reactive4$text <- NULL
  })  
  
  #Ejercicio 2
  
  output$text5 <- renderText({
    if (input$num3 == 0) {""}
    else if (input$num3 == round(1/input$cociente,3)) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive5 = reactiveValues(text = "")
  output$text6 <- renderUI({
    HTML(text_reactive5$text)
    isolate(text_reactive5$text)
  })
  
  observeEvent(input$submit3,{
    sol3 <- round(1/input$cociente,3)
    text_reactive5$text <-  sprintf("Sabemos que el complementario de A es T y el de G es C,
                                    por lo tanto, nos queda el cociente (T+C)/(A+G). En este caso
                                    la respuesta correcta sería %s.",sol3)
  })
  
  output$text7 <- renderText({
    if (input$num4 == 0) {""}
    else if (input$num4 == 1) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive6 = reactiveValues(text = "")
  output$text8 <- renderUI({
    HTML(text_reactive6$text)
    isolate(text_reactive6$text)
  })
  
  observeEvent(input$submit4,{
    sol4 <- 1
    text_reactive6$text <-  sprintf("Siguiendo las leyes de Chargaff, la respuesta correcta sería %s.",sol4)
  })
  
  output$text9 <- renderText({
    if (input$num5 == 0) {""}
    else if (input$num5 == input$cociente) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive7 = reactiveValues(text = "")
  output$text10 <- renderUI({
    HTML(text_reactive7$text)
    isolate(text_reactive7$text)
  })
  
  observeEvent(input$submit5,{
    sol5 <- input$cociente
    text_reactive7$text <-  sprintf("Como en el primer caso, el complementario de A es T y el de
                                    G es C, por lo tanto nos queda el cociente (C+G)/(T+A) que es
                                    el mismo que el del enunciado original. Entonces, 
                                    la respuesta correcta sería %s.",sol5)
  })
  
  output$text11 <- renderText({
    if (input$num6 == 0) {""}
    else if (input$num6 == input$cociente) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive8 = reactiveValues(text = "")
  output$text12 <- renderUI({
    HTML(text_reactive8$text)
    isolate(text_reactive8$text)
  })
  
  observeEvent(input$submit6,{
    sol6 <- input$cociente
    text_reactive8$text <-  sprintf("Siguiendo la pregunta anterior, el cociente se mantiene constante.
                                    Por lo tanto, la respuesta correcta sería %s.",sol6)
  })
  
  observeEvent(input$reset2, {
    text_reactive5$text <- NULL
    text_reactive6$text <- NULL
    text_reactive7$text <- NULL
    text_reactive8$text <- NULL
  })  
  
  #Ejercicio 3
  
  output$text13 <- renderText({
    if (input$num7 == 0) {""}
    else if (input$num7 == round(input$gr*(1/(1.66*10^{-24}))*(1/700)*3.4*10^{-8},3)) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive9 = reactiveValues(text = "")
  output$text14 <- renderUI({
    withMathJax(helpText(text_reactive9$text))
  })
  
  observeEvent(input$submit7,{
    sol7 <- round(input$gr*(1/(1.66*10^{-24}))*(1/700)*3.4*10^{-8},3)
    text_reactive9$text <-  sprintf("Usando factores de conversión de la siguiente forma
                                    $$%s gr \\times \\frac{1Da}{1.66 \\times 10^{-24}gr} \\times \\frac{1pb}{700Da} \\times \\frac{3.4Å}{1pb} \\times \\frac{10^{-8}cm}{1Å}$$
                                    obtenemos la respuesta correcta %s cm.",input$gr,sol7)
  })
  
  observeEvent(input$reset3, {
    text_reactive9$text <- NULL
  }) 
  
  #Tema 2
  #Ejercicio 1
  
  
  output$enunciado <- renderText({
    pardos <<- sample(1:(as.numeric(input$hijos)-1),1,replace=TRUE)
    enunciado <- sprintf("a) Que %s tengan los ojos pardos y el resto de hijos azules.", pardos)
    HTML("<b>",enunciado)
  })
  
  
  output$text15 <- renderText({
    if (input$num8 == 0) {""}
    else if (input$num8 == round(choose(as.numeric(input$hijos),pardos)*(3/4)^(pardos)*(1/4)^(as.numeric(input$hijos)-pardos),3)) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive11 = reactiveValues(text = "")
  output$text16 <- renderUI({
    withMathJax(helpText(text_reactive11$text))
  })
  
  observeEvent(input$submit8,{
    sol8 <- round(choose(as.numeric(input$hijos),pardos)*(3/4)^(pardos)*(1/4)^(as.numeric(input$hijos)-pardos),3)
    text_reactive11$text <-  sprintf("Gracias al enunciado sabemos que P(pardo)>p(azul) y O(oscuro)>o(rubio) y que tenemos
                                     el cruce PpOo x PpOo. Entonces, la probabilidad de descendientes con ojos pardos es de 3/4, 
                                     mientras que a probabilidad de descendientes con ojos azules es de 1/4. Por lo tanto, la respuesta es
                                     $$\\binom{%s}{%s} \\times \\Big(\\frac{3}{4}\\Big)^{%s} \\times \\Big(\\frac{1}{4}\\Big)^{(%s-%s)} = %s$$",as.numeric(input$hijos),pardos,pardos,as.numeric(input$hijos),pardos,sol8)
  })
  
  
  output$text17 <- renderText({
    if (input$num9 == 0) {""}
    else if (input$num9 == round(1-(3/4)^(as.numeric(input$hijos)),3)) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive12 = reactiveValues(text = "")
  output$text18 <- renderUI({
    withMathJax(helpText(text_reactive12$text))
  })
  
  observeEvent(input$submit9,{
    sol9 <- round(1-(3/4)^(as.numeric(input$hijos)),3)
    text_reactive12$text <-  sprintf("La probabilidad que se pide es igual a 1 - (probabilidad de todos con ojos pardos). Por lo tanto, la respuesta es
                                     $$1 - \\Big(\\frac{3}{4}\\Big)^{%s} = %s$$",as.numeric(input$hijos),sol9)
  })
  
  output$text19 <- renderText({
    if (input$num10 == 0) {""}
    else if (input$num10 == round(1-(1-(1/4)^2)^(as.numeric(input$hijos)),3)) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive13 = reactiveValues(text = "")
  output$text20 <- renderUI({
    withMathJax(helpText(text_reactive13$text))
  })
  
  observeEvent(input$submit10,{
    op <- (1/4)^2
    nop <- 1-op
    sol10 <- round(1-(1-(1/4)^2)^(as.numeric(input$hijos)),3)
    text_reactive13$text <-  sprintf("Primero tenemos que la probabilidad de que un descendiente sea rubio con ojos azules es
                                     $$\\frac{1}{4} \\times \\frac{1}{4} = %s$$
                                     Luego, la probabilidad de que un descendiente no sea rubio y con ojos azules es
                                     $$1-%s=%s$$
                                     Por lo tanto, la probabilidad que se pide es igual a 1-(probabilidad de que todos los descendientes
                                     sean diferentes a rubio con ojos azules), es decir
                                     $$1-(%s)^{%s}=%s$$",op,op,nop,nop,as.numeric(input$hijos),sol10)
  })
  
  output$text21 <- renderText({
    if (input$num11 == 0) {""}
    else if (input$num11 == round((factorial(as.numeric(input$hijos)))/(factorial(1)*factorial(1)*factorial(as.numeric(input$hijos)-2))*(3/16)*(9/16)*(3/16)^{as.numeric(input$hijos)-2},3)) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive14 = reactiveValues(text = "")
  output$text22 <- renderUI({
    withMathJax(helpText(text_reactive14$text))
  })
  
  observeEvent(input$submit11,{
    Op <- 3/16
    OP <- 9/16
    oP <- 3/16
    fact <- (factorial(as.numeric(input$hijos)))/(factorial(1)*factorial(1)*factorial(as.numeric(input$hijos)-2))
    sol11 <- round(fact*Op*OP*oP^{as.numeric(input$hijos)-2},3)
    text_reactive14$text <-  sprintf("Tenemos que calcular primero diferentes probabilidades. La probabilidad de que un descendiente tenga
                                     el pelo oscuro y ojos azules es
                                     $$\\frac{3}{4} \\times \\frac{1}{4} = %s$$
                                     La probabilidad de que uno de los descendientes sea de pelo oscuro y ojos pardos es
                                     $$\\frac{3}{4} \\times \\frac{3}{4} = %s$$
                                     La probabilidad de que uno de los descendientes sea de pelo rubio y ojos pardos es
                                     $$\\frac{3}{4} \\times \\frac{1}{4} = %s$$
                                     Entonces, la solución es
                                     $$\\frac{%s!}{1!1!%s!} \\times %s \\times %s \\times %s^{%s-2} = %s$$", Op,OP,oP,as.numeric(input$hijos),(as.numeric(input$hijos)-2),Op,OP,oP,as.numeric(input$hijos),sol11)
  })
  
  observeEvent(input$reset4, {
    text_reactive11$text <- NULL
    text_reactive12$text <- NULL
    text_reactive13$text <- NULL
    text_reactive14$text <- NULL
  }) 
  
  #Ejercicio 2
  
  text_reactive15 = reactiveValues(text = "")
  output$text23 <- renderUI({
    withMathJax(helpText(text_reactive15$text))
  })
  
  especies <<- c("ratones","gatos","conejos", "hamsters", "hurones","tejones","cobayas")
  animal <<- sample(especies,1)
  
  observeEvent(input$submit12,{
    normales <<- sample(100:1000,1)
    albinos <<- ceiling(normales/sample(seq(from=2.7, to=3.3, by=.01),1))
    total <<- normales + albinos
    chisq <<- ((normales-(total*0.75))^2/((0.75*total))) + ((albinos-(0.25*total))^2/(0.25*total))
    text_reactive15$text <-  sprintf("Al cruzar %s de color normal con otros albinos, todos los %s de la F1 fueron normales, y en la F2 producto
                                     de cruzar individuos de la F1, %s normales y %s albinos. Emitir una hipótesis del modo de herencia de este carácter
                                     y comprobar el ajuste con la proporción teórica mediante la prueba χ2.", animal, animal, normales, albinos)
    
  })
  
  output$text24a <- renderText({
    if (is.null(input$hipotesis)) {""}
    else if (length(input$hipotesis) >= 2) {HTML(paste0("<span style='color:red'>","Selecciona solo una respuesta","</span>"))}
    else if (input$hipotesis == "Herencia autosómica recesiva (normal > albino)") {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive16a = reactiveValues(text = "")
  output$text25a <- renderUI({
    withMathJax(helpText(text_reactive16a$text))
  })
  
  observeEvent(input$submit13a,{
    text_reactive16a$text <-  sprintf("Se trata de herencia autosómica recesiva. Pongamos que el color viene dado por el gen normal (A) > albino (a). Nos dice el enunciado que todos los %s de la F1 fueron normales,
                                      eso quiere decir que al inicio cruzamos %s AA con %s aa, ya que si tuviéramos Aa x aa nos saldrían %s albinos en la F1. De este modo, en la F2 obtenemos aproximadamente una proporción 3:1
                                      que nos indica que el rasgo color normal domina al rasgo color albino.", animal, animal, animal, animal)
  })
  
  output$text24 <- renderText({
    if (input$num12 == 0) {""}
    else if (input$num12 == round(chisq,3)) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive16 = reactiveValues(text = "")
  output$text25 <- renderUI({
    withMathJax(helpText(text_reactive16$text))
  })
  
  observeEvent(input$submit13,{
    normal_esp <- 3/4*total
    albino_esp <- 1/4*total
    sol12 <- round(chisq,3)
    text_reactive16$text <-  sprintf("Queremos comprobar si la diferencia entre los valores experimentales obtenidos y los esperados
                                     bajo la hipótesis es lo suficientemente pequeña para atribuirlo al azar.
                                     Para ello debemos utilizar la prueba χ2 y necestiamos los valores esperados: si normales tenemos %s, el número esperado es $$\\frac{3}{4} \\times %s=%s$$
                                     Si albinos tenemos %s, el número esperado es $$\\frac{1}{4} \\times %s = %s$$.
                                     Con estos valores ya podemos calcular el valor de χ2 de la siguiente forma:
                                     $$\\frac{(%s-%s)^2}{%s} + \\frac{(%s-%s)^2}{%s}=%s$$
                                     De esta forma, con nivel de significación p=0.05 y n-1=2-1=1 grados de libertad, si miramos en la tabla de χ2, vemos que el valor obtenido es menor que el valor teórico (3.84)
                                     y, por lo tanto, no existen evidencias significativas para rechazar la hipótesis de herencia.",normales, total, normal_esp, albinos, total, albino_esp, normales, normal_esp, normal_esp, albinos, albino_esp, albino_esp, sol12 )
  })
  
  observeEvent(input$reset5, {
    text_reactive16a$text <- NULL
    text_reactive16$text <- NULL
  }) 
  
  #Ejercicio 3
  
  text_reactive17 = reactiveValues(text = "", total2 = NULL, chisq2 = NULL)
  output$text26 <- renderUI({
    withMathJax(helpText(text_reactive17$text))
  })
  
  observeEvent(input$submit14,{
    rojos <<- sample(10:100,1)
    negros <<- ceiling(rojos/sample(seq(from=0.7, to=1.3, by=.01),1))
    total2 <<- rojos + negros
    chisq2 <<- ((rojos-(total2*0.5))^2/((0.5*total2))) + ((negros-(0.5*total2))^2/(0.5*total2))
    
    text_reactive17$text <-  sprintf("Un caballo negro de antepasados desconocidos fue apareado con cierto número de yeguas de color
                                     rojo de raza pura. Estos apareamientos dieron %s descendientes de color rojo y %s descendientes negros.",rojos, negros)
    
  })
  
  output$text27 <- renderText({
    if (is.null(input$recesivo)) {""}
    else if (length(input$recesivo) == 2) {HTML(paste0("<span style='color:red'>","Selecciona solo una respuesta","</span>"))}
    else if (input$recesivo == "Caracter rojo") {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive18 = reactiveValues(text = "")
  output$text28 <- renderUI({
    withMathJax(helpText(text_reactive18$text))
  })
  
  observeEvent(input$submit15,{
    text_reactive18$text <-  sprintf("Las hembras son de raza pura, así que serán homocigóticas para este carácter. Ahora bien,
                                     si fueran homocigóticas dominantes, toda la descendencia debería ser de color rojo y eso no se cumple. 
                                     Por lo tanto, asumimos que el color de los caballos está determinado por un locus con los alelos A (negro) > a (rojo).
                                     Entonces, el carácter rojo se debe seguramente a la presencia en homocigosis del alelo recesivo a.")
  })
  
  #output$text29 <- renderText({
   # if (input$num13 == 0) {""}
    #else if (input$num13 == round(total2,3)) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    #else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  #})
  
  output$text29 <- renderText({
    # Solo proceder si hay una entrada y no está vacía.
    if (is.null(input$num13) || input$num13 == 0) {
      return() # No hacer nada si no hay entrada.
    }
    
    # Comparar el valor numérico introducido por el usuario con el valor calculado.
    solucion2 <- round(total2/2,3)
    
    if (input$num13 == solucion2) {
      return(HTML(paste0("<span style='color:green'>", "¡La respuesta es correcta!", "</span>")))
    } else {
      return(HTML(paste0("<span style='color:red'>", "La respuesta no es correcta.", "</span>")))
    }
  })
  
  text_reactive19 = reactiveValues(text = "")
  output$text30 <- renderUI({
    withMathJax(helpText(text_reactive19$text))
  })
  
  observeEvent(input$submit16,{
    sol13 <- total2/2
    text_reactive19$text <-  sprintf("Por el apartado anterior, tenemos que los genotipos de los progenitores deben ser 
                                     $$ P: \\; Caballo \\; Negro \\; Aa \\times Yegua \\; Roja \\; aa $$
                                     De esta forma entonces, los genotipos de la F1 deben ser los siguientes:
                                     $$ F1: \\frac{1}{2} Negros \\; Aa + \\frac{1}{2} Rojos \\; aa $$
                                     Por lo tanto, el número de individuos esperado es: $$ Negros \\; Aa: \\frac{%s}{2}=%s \\; y \\; Rojos \\; aa: \\frac{%s}{2}=%s $$",total2,sol13,total2,sol13)
  })
  
  
  output$text31 <- renderText({
    if (input$num14 == 1) {""}
    else if (input$num14 != round(chisq2,3)) {HTML(paste0("<span style='color:red'>","¡La respuesta no es correcta!","</span>"))}
    else if (input$num14 == round(chisq2,3) & is.null(input$conclusion)) {""}
    else if (input$num14 == round(chisq2,3) & input$conclusion == "No existen evidencias estadísticas significativas para rechazar la hipótesis")
    {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else if (input$num14 == round(chisq2,3) & input$conclusion == "Existen evidencias estadísticas significativas para rechazar la hipótesis")
    {HTML(paste0("<span style='color:red'>","¡La respuesta no es correcta!","</span>"))}
  })
  
  text_reactive20 = reactiveValues(text = "")
  output$text32 <- renderUI({
    withMathJax(helpText(text_reactive20$text))
  })
  
  observeEvent(input$submit17,{
    esperados <- round(total2/2,3)
    sol14 <- round(chisq2,3)
    text_reactive20$text <-  sprintf("Sabemos el número de individuos observados y esperados, por lo tanto, podemos realizar la prueba para ver si los datos observados se ajustan a los esperados:
                                     $$\\frac{(%s-%s)^2}{%s} + \\frac{(%s-%s)^2}{%s}=%s$$
                                     De esta forma, con nivel de significación p=0.05 y n-1=2-1=1 grados de libertad, si miramos en la tabla de χ2, vemos que el valor obtenido es menor que el valor teórico (3.84)
                                     y, por lo tanto, no existen evidencias significativas para rechazar la hipótesis de herencia.",negros,esperados,esperados,rojos,esperados,esperados,sol14)
  })
  
  observeEvent(input$reset6, {
    text_reactive18$text <- NULL
    text_reactive19$text <- NULL
    text_reactive20$text <- NULL
  })
  
  ###TEMA 3
  #Ejercicio 1
  
  values3 <- reactiveValues(parentales1 = NULL, parentales2 = NULL, recombinanteshg1 = NULL, recombinanteshg2 = NULL,
                            recombinantesah1 = NULL, recombinantesah2 = NULL, doblerecomb1 = NULL, doblerecomb2 = NULL)
  
  text_reactive33a = reactiveValues(text = "")
  output$text33a <- renderUI({
    withMathJax(helpText(text_reactive33a$text))
  })
  
  observeEvent(input$datos,{
    parentales1 <<- sample(400:500,1)
    parentales2 <<- sample(400:500,1)
    recombinanteshg1 <<- sample(50:70,1)
    recombinanteshg2 <<- sample(50:70,1)
    recombinantesah1 <<- sample(9:16,1)
    recombinantesah2 <<- sample(9:16,1)
    doblerecomb1 <<- sample(1:3,1)
    doblerecomb2 <<- sample(1:3,1)
    total <<- parentales1+parentales2+recombinanteshg1+recombinanteshg2+recombinantesah1+recombinantesah2+doblerecomb1+doblerecomb2
    df1 <- data.frame(c(recombinanteshg2,parentales2,recombinanteshg1,doblerecomb2,doblerecomb1,parentales1,recombinantesah1,recombinantesah2), 
                      c("élitros rojizos, patas largas","élitros rojizos, élitros deformados","élitros deformados", "patas largas, élitros deformados", "élitros rojizos", "patas largas","tipo silvestre","élitros rojizos, patas largas, élitros deformados"))
    colnames(df1) <- c("Frecuencia", "Fenotipo")
    output$table1 <- renderTable(df1)
    
    #Actualizamos
    values3$parentales1 <- parentales1
    values3$parentales2 <- parentales2
    values3$recombinanteshg1 <- recombinanteshg1
    values3$recombinanteshg2 <- recombinanteshg2
    values3$recombinantesah1 <- recombinantesah1
    values3$recombinantesah2 <- recombinantesah2
    values3$doblerecomb1 <- doblerecomb1
    values3$doblerecomb2 <- doblerecomb2
    
  })
  
  output$text33 <- renderText({
    if (is.null(input$centro)) {""}
    else if (length(input$centro) >= 2) {HTML(paste0("<span style='color:red'>","Selecciona solo una respuesta","</span>"))}
    else if (input$centro == "Gen h") {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive20 = reactiveValues(text = "")
  output$text34 <- renderUI({withMathJax(helpText(text_reactive20$text))})
  
  observeEvent(input$submit18,{
    text_reactive20$text <-  sprintf("La tabla del enunciado expresa las frecuencias fenotípicas. Si consideramos las genotípicas deducimos que el gen h está en el centro.")
    df2 <- data.frame(c(recombinanteshg2,parentales2,recombinanteshg1,doblerecomb2,doblerecomb1,parentales1,recombinantesah1,recombinantesah2),
                      c("+ag","h+g","h++","ha+","++g","+a+","+++","hag"))
    colnames(df2) <- c("Frecuencia", "Genotipo")
    output$table2 <- renderTable(df2)
  })
  
  output$text35 <- renderText({
    # Solo proceder si hay una entrada y no está vacía.
    if (is.null(input$num15) || input$num15 == 0) {
      return() # No hacer nada si no hay entrada.
    }
    
    # Comparar el valor numérico introducido por el usuario con el valor calculado.
    solucion <- round(((recombinantesah1+recombinantesah2+doblerecomb1+doblerecomb2)/total)*100,3)
    
    if (input$num15 == solucion) {
      return(HTML(paste0("<span style='color:green'>", "¡La respuesta es correcta!", "</span>")))
    } else {
      return(HTML(paste0("<span style='color:red'>", "La respuesta no es correcta.", "</span>")))
    }
  })
  
  text_reactive21 = reactiveValues(text = "")
  output$text36 <- renderUI({withMathJax(helpText(text_reactive21$text))})
  
  observeEvent(input$submit19,{
    sol15 <- round(((recombinantesah1+recombinantesah2+doblerecomb1+doblerecomb2)/total)*100,3)
    text_reactive21$text <-  sprintf("Ordenando la tabla e identificando la clase de descendientes tenemos que la distancia es:
                                       $$ \\Big(\\frac{%s+%s+%s+%s}{%s}\\Big) \\times 100 = %s \\; cM$$", recombinantesah1, recombinantesah2, doblerecomb1, doblerecomb2, total, sol15) 
    df3 <- data.frame(c(parentales1,parentales2,recombinanteshg1,recombinanteshg2,recombinantesah1,recombinantesah2,doblerecomb1,doblerecomb2),
                      c("+a+","h+g","h++","+ag","+++","hag","++g","ha+"),
                      c("a++","+hg","+h+","a+g","+++","ahg","++g","ah+"),
                      c("parentales", "parentales", "recombinantes entre h y g", "recombinantes entre h y g","recombinantes entre a y h", "recombinantes entre a y h","doble recombinantes", "doble recombinantes"))
    colnames(df3) <- c("Frecuencia", "Genotipo", "Orden real", "Clase")
    output$table3 <- renderTable(df3)
  }) 
  
  output$text37 <- renderText({
    # Solo proceder si hay una entrada y no está vacía.
    if (is.null(input$num16) || input$num16 == 0) {
      return() # No hacer nada si no hay entrada.
    }
    
    # Comparar el valor numérico introducido por el usuario con el valor calculado.
    solucion1 <- round(((recombinanteshg1+recombinanteshg2+doblerecomb1+doblerecomb2)/total)*100,3)
    
    if (input$num16 == solucion1) {
      return(HTML(paste0("<span style='color:green'>", "¡La respuesta es correcta!", "</span>")))
    } else {
      return(HTML(paste0("<span style='color:red'>", "La respuesta no es correcta.", "</span>")))
    }
  })
  
  text_reactive22 = reactiveValues(text = "")
  output$text38 <- renderUI({withMathJax(helpText(text_reactive22$text))})
  
  observeEvent(input$submit20,{
    sol16 <- round(((recombinanteshg1+recombinanteshg2+doblerecomb1+doblerecomb2)/total)*100,3)
    
    dist1 <<- round(((recombinantesah1+recombinantesah2+doblerecomb1+doblerecomb2)/total)*100,3)
    dist2 <<- round(((recombinanteshg1+recombinanteshg2+doblerecomb1+doblerecomb2)/total)*100,3)
    
    DR_obs <<- doblerecomb1+doblerecomb2
    DR_esp <<- round(((dist1/100)*(dist2/100))*total,3)
    CC <<- round(DR_obs/DR_esp,3)
    text_reactive22$text <-  sprintf("Usando la tabla anterior tenemos que la distancia es:
                                       $$ \\Big(\\frac{%s+%s+%s+%s}{%s}\\Big) \\times 100 = %s \\; cM$$", recombinanteshg1, recombinanteshg2, doblerecomb1, doblerecomb2, total, sol16) 
  }) 
  
  
  output$text39 <- renderText({
    # Solo proceder si hay una entrada y no está vacía.
    if (is.null(input$num17) || input$num17 == 1) {
      return() # No hacer nada si no hay entrada.
    }
    
    # Comparar el valor numérico introducido por el usuario con el valor calculado.
    solucion2 <- round(1-CC,3)
    
    if (input$num17 == solucion2) {
      return(HTML(paste0("<span style='color:green'>", "¡La respuesta es correcta!", "</span>")))
    } else {
      return(HTML(paste0("<span style='color:red'>", "La respuesta no es correcta.", "</span>")))
    }
  })
  
  text_reactive23 = reactiveValues(text = "")
  output$text40 <- renderUI({withMathJax(helpText(text_reactive23$text))})
  
  observeEvent(input$submit21,{
    sol17 <- round(1-CC,3)
    DR_obs <<- doblerecomb1+doblerecomb2
    text_reactive23$text <-  sprintf("Para calcular el coeficiente de interferencia necesitamos primero el coeficiente de 
                                       que se obtiene a partir de las dobles recombinaciones observadas y las esperadas.
                                       
                                       Las dobles recombinaciones observadas son: $$%s+%s = %s$$
                                       
                                       Las dobles recombinaciones esperadas son: $$ \\Big(\\frac{%s}{100} \\times \\frac{%s}{100}\\Big) \\times %s = %s $$
                                       De esta forma, $$CC = \\frac{%s}{%s} = %s $$
                                       Por lo tanto, $$ I = 1- CC = 1 - %s = %s $$", doblerecomb1, doblerecomb2, DR_obs, dist1, dist2, total, DR_esp, DR_obs, DR_esp, CC, CC, sol17) 
  }) 
  
  observeEvent(input$reset7, {
    text_reactive20$text <- NULL
    output$table2 <- NULL
    text_reactive21$text <- NULL
    output$table3 <- NULL
    text_reactive22$text <- NULL
    text_reactive23$text <- NULL
  })
  
  #Ejercicio 2
  
  output$neurospora <- renderImage({
    return(list(
      src = "www/neurospora.png",
      contentType = "image/png",
      width = 400,
      height = 200,
      alt = "neurospora"
    ))
  }, deleteFile = FALSE)
  
  output$text50 <- renderText({
    if (is.null(input$ligados)) {""}
    else if (length(input$ligados) == 2) {HTML(paste0("<span style='color:red'>","Selecciona solo una respuesta","</span>"))}
    else if (input$ligados == "Si") {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive29 = reactiveValues(text = "")
  output$text53 <- renderUI({
    withMathJax(helpText(text_reactive29$text))
  })
  
  observeEvent(input$submit27,{
    text_reactive29$text <-  sprintf("Para saber si son genes independientes o si están ligados comparamos el número de descendientes ditipo parental DP (primera columna) 
                                     con el número de ditipo no parental DNP (segunda columna). Si el número de DP es diferente de DNP los genes están ligados. En nuestro caso tenemos
                                     DP = 49 y DNP = 8. Por lo tanto, podemos concluir que los genes ad y T están ligados.")
  })
  
  output$text51 <- renderText({
    if (is.null(input$brazo)) {""}
    else if (length(input$brazo) == 2) {HTML(paste0("<span style='color:red'>","Selecciona solo una respuesta","</span>"))}
    else if (input$brazo == "Se encuentran en el mismo brazo.") {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive30 = reactiveValues(text = "")
  output$text54 <- renderUI({
    withMathJax(helpText(text_reactive30$text))
  })
  
  observeEvent(input$submit28,{
    text_reactive30$text <-  sprintf("Para saber si están en el mismo brazo comparamos los recombinantes en configuración X (quinta columna) con los recombinantes en configuración Y (sexta columna).
                                      De este modo tenemos X=6 e Y=1. Entonces si la frX es igual frY son de diferente brazo, en cambio si la frX es diferente frY están en el mismo brazo.
                                     Por lo tanto, podemos concluir que los genes ad y T están ligados y en el mismo brazo.")
  })
  
  
  gen_ad <<- round(1/2*12/100*100,3)
  gen_T <<- round(1/2*40/100*100,3)
  
  output$text52 <- renderText({
    dist_split <- strsplit(input$num19, " ")
    dist <- unlist(dist_split)
    if (dist[1] == gen_ad & dist[2] == gen_T) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else if (input$num19 == c("ad T")){""}
    else {HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  }) 
  
  text_reactive31 = reactiveValues(text = "")
  output$text55 <- renderUI({
    withMathJax(helpText(text_reactive31$text))
  })
  
  observeEvent(input$submit29,{
    text_reactive31$text <-  sprintf("Tenemos una fórmula para calcular las distancias de los genes al centrómero:
                                     $$\\Big(\\frac{1}{2} \\times \\frac{R}{total}\\Big) \\times 100$$ 
                                     Por lo tanto, solamente tenemos que calcular los recombinantes. 
                                     
                                     Si miramos la imagen, los recombinantes del gen ad son la suma de las columnas 4, 5, 6 y 7; mientras que los recombinantes
                                     del gen T son la suma de las columnas 3, 5, 6 y 7. De forma que obtenemos 12 y 40 recombinantes, respectivamente. Luego por
                                     el enunciado sabemos que tenemos un total de 100 tétradas.
                                     
                                     Así pues, ya tenemos todos los datos para aplicar la fórmula. Empezamos con la distancia del gen ad al centrómero:
                                     $$\\Big(\\frac{1}{2} \\times \\frac{12}{100}\\Big) \\times 100 = %s$$ 
                                     
                                     Ahora la distancia del gen T al centrómero:
                                     $$\\Big(\\frac{1}{2} \\times \\frac{40}{100}\\Big) \\times 100 = %s$$",gen_ad,gen_T)
  })
  
  
  
  observeEvent(input$reset10, {
    text_reactive29$text <- NULL
    text_reactive30$text <- NULL
    text_reactive31$text <- NULL
  })
  
  ###TEMA 4
  #Ejercicio 1
  
  values1 <- reactiveValues(genAA = NULL, genAS = NULL, genSS = NULL,
                            frecA = NULL, frecS = NULL, frecAA = NULL, frecAS = NULL, frecSS = NULL,
                            frecAAhw = NULL, frecAShw = NULL, frecSShw = NULL)
  
  observeEvent(input$datos1,{
    genAA <<- sample(150:200,1)
    genAS <<- sample(50:100,1)
    genSS <<- sample(5:15,1)
    ind_total <<- genAA + genAS + genSS
    df4 <- data.frame(c("AA", "AS", "SS", "TOTAL"), 
                      c(genAA, genAS, genSS, ind_total))
    colnames(df4) <- c("Genotipo", "Frecuencia")
    output$table4 <- renderTable(df4)
    
    # Actualiza los valores en reactiveValues
    values1$genAA <- genAA
    values1$genAS <- genAS
    values1$genSS <- genSS
    
    frecA <<- round((2*genAA+genAS)/(2*ind_total),3)
    frecS <<- round((2*genSS+genAS)/(2*ind_total),3)
    frecAA <<- round(genAA/ind_total,3)
    frecAS <<- round(genAS/ind_total,3)
    frecSS <<- round(genSS/ind_total,3)
    
    # Actualiza los valores en reactiveValues
    values1$frecA <- frecA
    values1$frecS <- frecS
    values1$frecAA <- frecAA
    values1$frecAS <- frecAS
    values1$frecSS <- frecSS
    
    frecAAhw <<- round(frecA^2,3)
    frecAShw <<- round(2*frecA*frecS,3)
    frecSShw <<- round(frecS^2,3)
    
    # Actualiza los valores en reactiveValues
    values1$frecAAhw <- frecAAhw
    values1$frecAShw <- frecAShw
    values1$frecSShw <- frecSShw
    
  })
  
  output$text41 <- renderText({
    # Solo proceder si hay una entrada y no está vacía.
    if (is.null(input$frecuencias) || input$frecuencias == "") {
      return() # No hacer nada si no hay entrada.
    }
    
    freq_split <- strsplit(input$frecuencias, " ")
    freq <- unlist(freq_split)
    
    # Convertir la entrada a numérico y proceder solo si todos los valores son numéricos y exactamente tres.
    freq_num <- suppressWarnings(as.numeric(freq))
    if (length(freq_num) != 3 || any(is.na(freq_num))) {
      return() # No hacer nada si los valores no son tres números válidos.
    }
    
    # Comparar los valores numéricos introducidos por el usuario con los valores calculados.
    frecuencias1 <- c(values$frecAA, values$frecAS, values$frecSS)
    
    if (!is.null(frecuencias1) && all(freq_num == frecuencias1)) {
      return(HTML(paste0("<span style='color:green'>", "¡La respuesta es correcta!", "</span>")))
    } else {
      return(HTML(paste0("<span style='color:red'>", "La respuesta no es correcta.", "</span>")))
    }
  })
  
  
  text_reactive24 = reactiveValues(text = "")
  output$text42 <- renderUI({
    withMathJax(helpText(text_reactive24$text))
  })
  
  observeEvent(input$submit22,{
    
    text_reactive24$text <-  sprintf("Para calcular las frecuencias genotípicas y alélicas simplemente tenemos que aplicar las fórmulas.
                                       
                                       $$f(AA) = \\frac{número \\; de \\; individuos \\; AA}{total \\; de \\; individuos}= \\frac{%s}{%s} = %s$$
                                       
                                       $$f(AS) = \\frac{número \\; de \\; individuos \\; AS}{total \\; de \\; individuos}= \\frac{%s}{%s} = %s$$
                                       
                                       $$f(SS) = \\frac{número \\; de \\; individuos \\; SS}{total \\; de \\; individuos}= \\frac{%s}{%s} = %s$$
                                       
                                       $$p = \\frac{número \\; de \\; copias \\; de \\; A}{número \\; de \\; copias \\; de \\; todos \\; los \\; alelos \\; en \\; el \\; locus}$$
                                       $$= \\frac{2 \\times %s+%s}{2 \\times %s} = %s$$
                                       
                                       $$q = \\frac{número \\; de \\; copias \\; de \\; S}{número \\; de \\; copias \\; de \\; todos \\; los \\; alelos \\; en \\; el \\; locus}$$
                                       $$= \\frac{2 \\times %s+%s}{2 \\times %s} = %s$$
                                       
                                       ", genAA, ind_total, frecAA, genAS, ind_total, frecAS, genSS, ind_total, frecSS, genAA, genAS, ind_total, frecA, genSS, genAS, ind_total, frecS )
  })
  
  output$text43 <- renderText({
    # Solo proceder si hay una entrada y no está vacía.
    if (is.null(input$frecuencias_hw) || input$frecuencias_hw == "") {
      return() # No hacer nada si no hay entrada.
    }
    
    freqhw_split <- strsplit(input$frecuencias_hw, " ")
    freqhw <- unlist(freqhw_split)
    
    # Convertir la entrada a numérico y proceder solo si todos los valores son numéricos y exactamente tres.
    freqhw_num <- suppressWarnings(as.numeric(freqhw))
    if (length(freqhw_num) != 3 || any(is.na(freqhw_num))) {
      return() # No hacer nada si los valores no son tres números válidos.
    }
    
    # Comparar los valores numéricos introducidos por el usuario con los valores calculados.
    frecuencias2 <- c(values$frecAAhw, values$frecAShw, values$frecSShw)
    
    if (!is.null(frecuencias2) && all(freqhw_num == frecuencias2)) {
      return(HTML(paste0("<span style='color:green'>", "¡La respuesta es correcta!", "</span>")))
    } else {
      return(HTML(paste0("<span style='color:red'>", "La respuesta no es correcta.", "</span>")))
    }
  })
  
  
  text_reactive25 = reactiveValues(text = "")
  output$text44 <- renderUI({
    withMathJax(helpText(text_reactive25$text))
  })
  
  observeEvent(input$submit23,{
    
    text_reactive25$text <-  sprintf("De nuevo basta aplicar las fórmulas:
                                       
                                       $$f(AA) \\; Hardy-Weinberg = p^2 = %s^2 = %s$$
                                       
                                       $$f(AS) \\; Hardy-Weinberg = 2 p q = 2 \\times %s \\times %s = %s$$
                                       
                                       $$f(SS) \\; Hardy-Weinberg = q^2 = %s^2 = %s$$
                                       
                                       ", frecA, frecAAhw, frecA, frecS, frecAShw, frecS, frecSShw )
  })
  
  
  output$text45 <- renderText({
    if (input$equilibrio == 0) {""}
    else if (input$equilibrio != round(chisqhw,3)) {HTML(paste0("<span style='color:red'>","¡La respuesta no es correcta!","</span>"))}
    else if (input$equilibrio == round(chisqhw,3) & is.null(input$conclusion2)) {""}
    else if (input$equilibrio == round(chisqhw,3) & round(chisqhw,3) < 3.84 & input$conclusion2 == "No existen evidencias estadísticas significativas para rechazar la hipótesis")
    {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else if (input$equilibrio == round(chisqhw,3) & round(chisqhw,3) >= 3.84 & input$conclusion2 == "Existen evidencias estadísticas significativas para rechazar la hipótesis")
    {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else {HTML(paste0("<span style='color:red'>","¡La respuesta no es correcta!","</span>"))}
  })
  
  text_reactive26 = reactiveValues(text = "")
  output$text46 <- renderUI({withMathJax(helpText(text_reactive26$text))})
  
  observeEvent(input$submit24,{
    frecAA_esphw <<- round(frecAAhw*ind_total,3)
    frecAS_esphw <<- round(frecAShw*ind_total,3)
    frecSS_esphw <<- round(frecSShw*ind_total,3)
    
    chisqhw <<- ((genAA-frecAA_esphw)^2/frecAA_esphw)+((genAS-frecAS_esphw)^2/frecAS_esphw)+((genSS-frecSS_esphw)^2/frecSS_esphw)
    
    solhw <- round(chisqhw,3)
    text_reactive26$text <-  sprintf("Lo primero que debemos calcular son los valores esperados en situación de equilibrio de Hardy-Weinberg.
                                       Para ello, multiplicaremos las frecuencias obtenidas en el apartado anterior por el total de individuos de nuestro problema:
                                       
                                       $$f(AA)_{HW} = %s \\times %s = %s$$
                                       $$f(AS)_{HW} = %s \\times %s = %s$$
                                       $$f(SS)_{HW} = %s \\times %s = %s$$
                                       
                                       Una vez tenemos estos datos, ya podemos realizar el test de chi-cuadrado comparando los valores observados y los esperados:
                                       
                                       $$χ2 = \\frac{(%s-%s)^2}{%s}+\\frac{(%s-%s)^2}{%s}+\\frac{(%s-%s)^2}{%s}$$
                                       $$ = %s$$
                                       
                                       Si el valor obtenido es menor que 3.84 podemos deducir que no existen evidencias significativas para descartar que la poblacion esté en equilibrio Hardy-Weinberg.
                                       En cambio, si el valor es mayor que dicho número podremos decir que existen evidencias para descartar la hipótesis."
                                     
                                     
                                     
                                     , frecAAhw, ind_total, frecAA_esphw, frecAShw, ind_total, frecAS_esphw, frecSShw, ind_total, frecSS_esphw, genAA, frecAA_esphw, frecAA_esphw,
                                     genAS, frecAS_esphw, frecAS_esphw, genSS, frecSS_esphw, frecSS_esphw, solhw)
    
    
  })
  
  observeEvent(input$reset8, {
    text_reactive24$text <- NULL
    text_reactive25$text <- NULL
    text_reactive26$text <- NULL
  })
  
  #Ejercicio 2
  
  values4 <- reactiveValues(p2 = NULL, q2 = NULL, r2 = NULL,
                           poblacion = NULL, gr0 = NULL, grA = NULL, grB = NULL, grAB = NULL,
                           freq_gen0 = NULL, freq_genA = NULL, freq_genB = NULL, freq_genAB = NULL)
  
  text_reactive27 = reactiveValues(text = "")
  output$text47 <- renderUI({
    withMathJax(helpText(text_reactive27$text))
  })
  
  observeEvent(input$submit25,{
    p2 <<- round(runif(1, 0.2, 0.4),3)
    q2 <<- round(runif(1, 0.2, 0.4),3)
    r2 <<- round(1-(p2+q2),3)
    
    poblacion <<- sample(10000:16000,1)
    gr0 <- ceiling((r2^2)*poblacion)
    grA <- ceiling(((p2^2)+(2*p2*r2))*poblacion)
    grB <- ceiling(((q2^2)+(2*q2*r2))*poblacion)
    grAB <- poblacion-(gr0+grA+grB)
    
    freq_gen0 <<- round((ceiling((r2^2)*poblacion))/poblacion,3)
    freq_genA <<- round(ceiling(((p2^2)+(2*p2*r2))*poblacion)/poblacion,3)
    freq_genB <<- round(ceiling(((q2^2)+(2*q2*r2))*poblacion)/poblacion,3)
    freq_genAB <<- round((poblacion-(ceiling((r2^2)*poblacion)
                                     + ceiling(((p2^2)+(2*p2*r2))*poblacion)
                                     + ceiling(((q2^2)+(2*q2*r2))*poblacion)))/poblacion,3)
    
    #Actualizamos
    values4$p2 <- p2
    values4$q2 <- q2
    values4$r2 <- r2
    values4$poblacion <- poblacion
    values4$gr0 <- gr0
    values4$grA <- grA
    values4$grB <- grB
    values4$grAB <- grAB
    values4$freq_gen0 <- freq_gen0
    values4$freq_genA <- freq_genA
    values4$freq_genB <- freq_genB
    values4$freq_genAB <- freq_genAB

    text_reactive27$text <-  sprintf("En una población de %s individuos se ha encontrado %s personas del grupo sanguíneo 0,
                                       %s del grupo A, %s del grupo B y %s del grupo AB.", poblacion, gr0, grA, grB, grAB)
    
  })
  
  #output$text48 <- renderText({
    #freqsplit <- strsplit(input$num18, " ")
    #freq2 <- unlist(freqsplit)
    #if (freq2[1] == freq_genA & freq2[2] == freq_genB & freq2[3] == freq_gen0) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    #else if (input$num18 == c("f(A)  f(B)  f(0)")){""}
    #else {HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  #}) 
  
  output$text48 <- renderText({
    # Solo proceder si hay una entrada y no está vacía.
    if (is.null(input$num18) || input$num18 == "") {
      return() # No hacer nada si no hay entrada.
    }
    
    freq2split <- strsplit(input$num18, " ")
    freq2 <- unlist(freq2split)
    
    # Convertir la entrada a numérico y proceder solo si todos los valores son numéricos y exactamente tres.
    freq2_num <- suppressWarnings(as.numeric(freq2))
    if (length(freq2_num) != 3 || any(is.na(freq2_num))) {
      return() # No hacer nada si los valores no son tres números válidos.
    }
    
    # Comparar los valores numéricos introducidos por el usuario con los valores calculados.
    frecuencias3 <- c(values$frec_genA, values$frec_genB, values$frec_gen0)
    
    if (!is.null(frecuencias3) && all(freq2_num == frecuencias3 )) {
      return(HTML(paste0("<span style='color:green'>", "¡La respuesta es correcta!", "</span>")))
    } else {
      return(HTML(paste0("<span style='color:red'>", "La respuesta no es correcta.", "</span>")))
    }
  })
  
  text_reactive28 = reactiveValues(text = "")
  output$text49 <- renderUI({
    withMathJax(helpText(text_reactive28$text))
  })
  
  
  values5 <- reactiveValues(r3 = NULL, grupoBy0 = NULL, grupoB = NULL, grupoA = NULL)
  
  observeEvent(input$submit26,{
    r3 <<- round(sqrt(freq_gen0),3)
    grupoBy0 <<- round(sqrt(freq_genB + freq_gen0),3)
    grupoB <<- round(grupoBy0-r3,3)
    grupoA <<- round(1-grupoB-r3,3)
    
    values5$r3 <- r3
    values5$grupoBy0 <- grupoBy0
    values5$grupoB <- grupoB
    values5$grupoA <- grupoA
    
    df5 <- data.frame(c("0","A","B","AB"),
                      c(ceiling((r2^2)*poblacion), ceiling(((p2^2)+(2*p2*r2))*poblacion), ceiling(((q2^2)+(2*q2*r2))*poblacion), poblacion-(ceiling((r2^2)*poblacion)
                                                                                                                                            + ceiling(((p2^2)+(2*p2*r2))*poblacion)
                                                                                                                                            + ceiling(((q2^2)+(2*q2*r2))*poblacion))),
                      c(freq_gen0, freq_genA, freq_genB, freq_genAB),
                      c("r^2", "p^2+2pr", "q^2+2qr","2pq"))
    colnames(df5) <- c("Fenotipo", "Individuos", "Frecuencia genotípica", "Frec genot. HW")
    
    
    output$table5 <- renderTable(df5)
    text_reactive28$text <-  sprintf("Consideramos A=p, B=q y 0=r y creamos una tabla donde se relacionan los datos con sus respectivas frecuencias genotípicas
                                       y sus expresiones H-W en el equilibrio.
                                       
                                      A partir de aquí, podemos despejar fácilmente el valor de r: $$r = \\sqrt{r^2} =%s$$
                                      
                                      Ahora podemos despejar B. Combinando los individuos con fenotipo B y 0 obtenemos:
                                       $$Frec(grupoB)+Frec(grupo0)=%s+%s=q^2+2pr+r^2$$
                                       $$ =(q+r)^2$$
                                       por lo tanto, $$(q+r)=\\sqrt{(q+r)^2}=%s$$
                                       
                                       Entonces, si $$(q+r)=%s \\; y \\; r=%s$$ tenemos $$q=%s-%s=%s$$
                                       
                                       Y ya para acabar, $$p=1-q-r=%s$$"
                                     
                                     ,r3,freq_genB,freq_gen0,grupoBy0,grupoBy0,r3,grupoBy0,r3,grupoB,grupoA)
    
    
  })
  
  observeEvent(input$reset9, {
    text_reactive27$text <- NULL
    text_reactive28$text <- NULL
    output$table5 <- NULL
  })
  
  #Ejercicio 3
  
  text_reactive34 = reactiveValues(text = "")
  output$text60 <- renderUI({
    withMathJax(helpText(text_reactive34$text))
  })
  
  observeEvent(input$submit32,{
    tasa_mut <<- round(runif(1, 10^{-5}, 10^{-3}),5)
    generaciones <<- sample(10000:20000,1)
    
    text_reactive34$text <-  sprintf("En una población se da una tasa de mutación A ⭢ a de %s, siendo la tasa de mutación a ⭢ A de un valor cercano a 0
                           (consideramos que es un valor lo suficientemente bajo para no ser considerado en los cálculos) y asumiendo que la frecuencia inicial de A es 1. Consideramos que pasan %s generaciones.", tasa_mut, generaciones)
  })
  
  output$text56 <- renderText({
    if (input$num20 == 0) {""}
    else if (input$num20 == round(1*(1-tasa_mut)^{generaciones},5)) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive32 = reactiveValues(text = "")
  output$text57 <- renderUI({
    withMathJax(helpText(text_reactive32$text))
  })
  
  observeEvent(input$submit30,{
    sol_tasa <- round(1*(1-tasa_mut)^{generaciones},5)
    
    p_0 = 1
    F=function(n){p_0*(1-tasa_mut)^n}
    h=round(F(generaciones),5)
    
    output$plot <- renderPlot({
      ggplot(data.frame(x = c(0,100000)), aes(x)) + stat_function(fun = F) + ylab("p") + xlab("Número de generaciones") + ylim(c(0.0,1.0)) + geom_vline(xintercept=generaciones, linetype="dashed", color = "red") + geom_hline(yintercept = F(generaciones), linetype="dashed", color = "blue") + geom_text(aes(0,h,label = h, vjust = -1),colour="blue")
    })
    
    text_reactive32$text <-  sprintf("Asumiendo una frecuencia inicial $$Frec(A)=p_{0}=1$$.
                                     En la generación %s podemos conocer qué frecuencia tendrá el alelo A aplicando la ecuación:
                                     $$ p_{n}=p_{0} \\times (1-u)^n$$, donde u es la tasa de mutación y n el número de generaciones. 
                                     
                                     Por lo tanto, la solución al problema, viene dada por:
                                     $$p_{%s}=1 \\times (1-%s)^{%s}=%s$$",generaciones,generaciones,tasa_mut,generaciones,sol_tasa)
  })
  
  observeEvent(input$reset11, {
    text_reactive32$text <- NULL
    output$plot <- NULL
  })
  
  #Ejercicio 4
  
  values <- reactiveValues(frecmutAA = NULL, frecmutAa = NULL, frecmutaa = NULL)
  
  text_reactive35 = reactiveValues(text = "")
  output$text61 <- renderUI({
    withMathJax(helpText(text_reactive35$text))
  })
  
  observeEvent(input$submit33,{
    u <<- round(runif(1, 10^{-4}, 1*10^{-2}),5) #tasa mutacion A -> a
    v <<- round(runif(1, 10^{-5}, 10^{-4}),5) #tasa mutacion a -> A
    
    
    p_eq <<- round(v/(u+v),5)
    q_eq <<- round(u/(u+v),5)
    
    frecmutAA <<- round((p_eq)^2,5)
    frecmutAa <<- round(2*(p_eq)*(q_eq),5)
    frecmutaa <<- round((q_eq)^2,5)
    
    # Actualiza los valores en reactiveValues
    values$frecmutAA <- frecmutAA
    values$frecmutAa <- frecmutAa
    values$frecmutaa <- frecmutaa
    
    text_reactive35$text <-  sprintf("En una población de Drosophilia melanogaster la tasa de mutación A ⭢ a es de %s y la de retromutación a ⭢ A es de %s",u,v)
  })
  
  
  output$text58 <- renderText({
 # Solo proceder si hay una entrada y no está vacía.
  if (is.null(input$frec_mut) || input$frec_mut == "") {
    return() # No hacer nada si no hay entrada.
  }

  tasasplit <- strsplit(input$frec_mut, " ")
  mutacion <- unlist(tasasplit)
  
  # Convertir la entrada a numérico y proceder solo si todos los valores son numéricos y exactamente tres.
  mutacion_num <- suppressWarnings(as.numeric(mutacion))
  if (length(mutacion_num) != 3 || any(is.na(mutacion_num))) {
    return() # No hacer nada si los valores no son tres números válidos.
  }
  
  # Comparar los valores numéricos introducidos por el usuario con los valores calculados.
  frecuencias <- c(values$frecmutAA, values$frecmutAa, values$frecmutaa)
  
  if (!is.null(frecuencias) && all(mutacion_num == frecuencias)) {
    return(HTML(paste0("<span style='color:green'>", "¡La respuesta es correcta!", "</span>")))
  } else {
    return(HTML(paste0("<span style='color:red'>", "La respuesta no es correcta.", "</span>")))
  }
})

  
  text_reactive33 = reactiveValues(text = "")
  output$text59 <- renderUI({
    withMathJax(helpText(text_reactive33$text))
  })
  
  observeEvent(input$submit31,{
    
    text_reactive33$text <-  sprintf("Consideramos u=%s y v=%s de forma que podemos calcular las frecuencias p y q en equilibrio mediante las fórmulas:
                                     $$p_{eq} = \\frac{v}{u+v} = %s$$
                                     $$q_{eq} = \\frac{u}{u+v} = %s$$
                                     
                                     Ahora, las frecuencias genotípicas en el equilibrio serán por tanto: 
                                     $$Frec(AA) = p_{eq}^2 = %s$$
                                     $$Frec(Aa) = 2 \\times p_{eq} \\times q_{eq} = %s$$
                                     $$Frec(aa) = q_{eq}^2 = %s$$",u,v,p_eq,q_eq,frecmutAA,frecmutAa,frecmutaa)
  })
  
  observeEvent(input$reset12, {
    text_reactive33$text <- NULL
  })
  
  #Ejercicio 5
  
  text_reactive36 = reactiveValues(text = "")
  output$text62 <- renderUI({
    withMathJax(helpText(text_reactive36$text))
  })
  
  observeEvent(input$submit34,{
    alelopob1 <<- round(runif(1, 10^{-2}, 10^{-1}),5)
    alelopob2 <<- round(runif(1, 10^{-2}, 10^{-1}),5)
    tasa_mig <<- sample(10:90,1)
    
    text_reactive36$text <-  sprintf("En una población animal que mantiene consistente su tamaño a lo largo de las generaciones, la frecuencia de un alelo de un
                                     locus autosómico en un determinado momento es %s. La tasa de migración a esa población desde otra vecina de mayor tamaño, donde la frecuencia 
                                     de dicho alelo es %s, es del %s por ciento.",alelopob1,alelopob2,tasa_mig)
  })
  
  output$text63 <- renderText({
    if (input$num21 == 0) {""}
    else if (input$num21 == round(alelopob1+(tasa_mig/100)*(alelopob2-alelopob1),5)) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive37 = reactiveValues(text = "")
  output$text65 <- renderUI({
    withMathJax(helpText(text_reactive37$text))
  })
  
  observeEvent(input$submit35,{
    sol_mig <- round(alelopob1+(tasa_mig/100)*(alelopob2-alelopob1),5)
    
    q_0 <<- alelopob1
    q_m <<- alelopob2
    m <<- tasa_mig/100
    F=function(n){q_m + (((1-m)^n)*(q_0-q_m))}
    h=round(F(1),5)
    
    output$plot1 <- renderPlot({
      ggplot(data.frame(x = c(0,5)), aes(x)) + stat_function(fun = F) + ylab("q") + xlab("Número de generaciones") + ylim(c(0.001,0.099)) + geom_hline(yintercept = h, linetype="dashed", color = "blue") + geom_vline(xintercept=1, linetype="dashed", color = "red") + geom_text(aes(0,h,label = h, vjust = 2),colour="blue") 
    })
    
    text_reactive37$text <-  sprintf("Consideramos: $$q_0=%s, q_m=%s \\; y \\; m=\\frac{%s}{100}$$ Entonces, después de una generación tenemos:
                                     $$q_1=q_0+m \\times (q_m-q_0)$$
                                     $$q_1=%s+\\frac{%s}{100} \\times (%s-%s) = %s$$",alelopob1,alelopob2,tasa_mig,alelopob1,tasa_mig,alelopob2,alelopob1,sol_mig)
  })
  
  output$text64 <- renderText({
    if (input$num22 == 0) {""}
    else if (input$num22 == round(alelopob2+(1-tasa_mig/100)^{5}*(alelopob1-alelopob2),5)) {HTML(paste0("<span style='color:green'>","¡La respuesta es correcta!","</span>"))}
    else{HTML(paste0("<span style='color:red'>","La respuesta no es correcta","</span>"))}
  })
  
  text_reactive38 = reactiveValues(text = "")
  output$text66 <- renderUI({
    withMathJax(helpText(text_reactive38$text))
  })
  
  observeEvent(input$submit36,{
    sol_mig2 <- round(alelopob2+(1-tasa_mig/100)^{5}*(alelopob1-alelopob2),5)
    
    q_0 <<- alelopob1
    q_m <<- alelopob2
    m <<- tasa_mig/100
    F=function(n){q_m + (((1-m)^n)*(q_0-q_m))}
    h=round(F(5),5)
    
    output$plot2 <- renderPlot({
      ggplot(data.frame(x = c(0,5)), aes(x)) + stat_function(fun = F) + ylab("q") + xlab("Número de generaciones") + ylim(c(0.001,0.099)) + geom_hline(yintercept = h, linetype="dashed", color = "blue") + geom_vline(xintercept=1, linetype="dashed", color = "red") + geom_text(aes(0,h,label = h, vjust = 2),colour="blue") 
    })
    
    text_reactive38$text <-  sprintf("Como antes, consideramos: $$q_0=%s, q_m=%s \\; y \\; m=\\frac{%s}{100}$$ Entonces, después de cinco generaciones tenemos:
                                     $$q_5=q_m+(1-m)^5 \\times (q_0-q_m)$$
                                     $$q_5=%s+(1-\\frac{%s}{100})^5 \\times (%s-%s) = %s$$",alelopob1,alelopob2,tasa_mig,alelopob2,tasa_mig,alelopob1,alelopob2,sol_mig2)
  })
  
  
  
  observeEvent(input$reset13, {
    text_reactive37$text <- NULL
    output$plot1 <- NULL
    text_reactive38$text <- NULL
    output$plot2 <- NULL
  })
  
}
