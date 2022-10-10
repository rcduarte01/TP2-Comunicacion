library(shiny)
library(shinythemes)
library(plotrix)

graficarcapaC <- function(radio,capa, titulo=""){
  par(mar = c(0,0,0,0))
  plot(NULL, xlim=c(0,2*radio), 
       ylim=c(0,2*radio), main=titulo,asp=1,
       xlab="",ylab="",axes=F,
       yaxt="n",xaxt="n",cex.main = 3)
  
  draw.circle(x=radio,y=radio,radius=radio,col="black") 
  
  for(i in 1:(dim(capa)[1])){
    if(capa$estado[i]==1){
      draw.circle(capa$x[i],capa$y[i],radius=capa$r[i],border="black",col="white")
    }
  }
}
CAPA <- read.csv("Datos/CAPA800F5.csv")
load("Datos/simulacion_multi.RData")

graficarParticula<- function(color, particulas){
  for(i in 1: dim(particulas)[1]){
    draw.circle(x=particulas[i,3],y=particulas[i,4],
                r=particulas[i,1]/2,
                col=color)
  }
}

graficarParticulas<- function(tiempo){
  
  if(tiempo==0){
    graficarcapaC(radio=800, CAPA, titulo="")
  }else{
  tt <- tiempo/5
  graficarcapaC(radio=800, CAPA, titulo="")
  colores <- rep(c("red", "yellow", "blue"), c(8,8,8))
  
  for(i in 1:tt){
    temp <- subset(simulacion_multi[[2]][[i]], 
                   simulacion_multi[[2]][[i]][,5]==1)
    graficarParticula(colores[i], temp)
    
  }
  }
}

ui <-fluidPage(
  navbarPage("Simulación de una capa de medio filtrante",
           tabPanel("Gráfico",fluidPage(theme = shinytheme("flatly")),
                    tags$head(
                      tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                    pageWithSidebar(
                      headerPanel(' '),
                      sidebarPanel(sliderInput("bins",
                                               "Tiempo en minutos",
                                               min = 0,
                                               step = 5,
                                               max = 120,
                                               value = 0),
                                   HTML("Para ver las partículas capturadas seleccionar un tiempo de simulación, para distinguir los tiempos se utilizan 3 colores con los siguintes significados:
<p>&nbsp;</p>
🔴 0 a 40 minutos
<p>&nbsp;</p>
🟡 40 a 80 minutos
<p>&nbsp;</p>
🔵 80 a 120 minutos"),
                                   
                                   hr(),
                                   downloadButton(
                                     outputId = "imagen_descarga",
                                     label = "Descargar imagen"
                                   )),

                      mainPanel(
                        column(8, plotOutput("plot",width = 500, height=500))
                      )
                    )),
           tabPanel("Información",p("Esta aplicación presenta un modelo computacional de medios filtrantes
            construido aplicando diagramas de Voronoi-Laguerre para representar materiales fibrosos no tejidos.
             El modelo de filtro se construye a partir de las propiedades medidas del material como la porosidad,
              el diámetro medio de la fibra, el grosor y la distribución del tamaño de los poros.
              Se utiliza el modelo computacional del material para realizar simulaciones de Monte Carlo,
              aplicando mecanismos básicos de filtración.
              Las simulaciones de los procesos de filtración incluyeron las siguientes interacciones: 
              partícula-poro, partícula-fibra y partícula- partícula. Los cálculos de la eficiencia del filtro, 
              para diferentes distribuciones de tamaño de partículas y poros, concuerdan con la teoría general 
              de filtración. El modelo permitió rastrear y ubicar las partículas capturadas en poros o fibras,
              lo que permite visualizar la estructura del medio filtrante con las partículas a traves del tiempo", ".",style = "font-size:20px"),
                    p("La aplicación muestra una capa de 800 micrómetros y un tiempo de simulación de 120 minutos 
                      en el cual podemos observar la cantidad de partículas contaminantes capturadas en este tiempo.",style = "font-size:20px"),
                    hr(), 
                    p("Bibliografía",style = "font-size:25px"),
                    p("Destephen, J. A., & Choi, K. J. (1996). Modelling of filtration processes of fibrous filter media. Separations Technology, 6(1), 55-67.",style = "font-size:15px;color: grey"),
                    p("Duarte, R.C. Simulación estocástica de procesos de filtración utilizando un modelo de medio filtrante basado en 
                      diagramas de Voronoi-Laguerre.",style = "font-size:15px;color: grey")),

           tabPanel("Autor",
                    p(a("Roberto C. Duarte", href="https://github.com/rcduarte01", target="_blank"),style = "font-size:25px"),
                    p("e-mail: robertoduarte0612@gmail.com",style = "font-size:20px"))

)

)

server <- function(input, output) {
  
    output$plot <- renderPlot({
      
      graficarParticulas(input$bins)
    })
    
    output$imagen_descarga <- downloadHandler(
      
      filename = "file.png", 
      content = function(file){
        pdf(file = "Grafico_de_capa.pdf", width = 8.27, height = 11.69)
        graficarParticulas(input$bins)
        dev.off()
      }
      
    )
}

 
shinyApp(ui = ui, server = server)






