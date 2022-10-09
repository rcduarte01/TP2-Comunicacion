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
🔵 80 a 120 minutos")),

                      mainPanel(
                        column(8, plotOutput("plot",width = 500, height=500))
                      )
                    )),
           tabPanel("Información",p("Esta aplicación muestra la captura de partículas contaminantes
            a traves del tiempo sobre una capa simulada de material fibroso no tejido.
                                      La aplicación muestra una capa circular de material de radio 800 micrómetros y
                                     la cantidad de partículas contaminantes capturadas a traves de 
                                     una simulación de 120 minutos", ".",style = "font-size:20px")),

           tabPanel("Autor",
                    p(a("Roberto C. Duarte", href="https://github.com/rcduarte01", target="_blank"),style = "font-size:25px"),
                    p("e-mail: robertoduarte0612@gmail.com",style = "font-size:20px"))

)

)

server <- function(input, output) {
    output$plot <- renderPlot({
      
      graficarParticulas(input$bins)
    })
    
    
}

 
shinyApp(ui = ui, server = server)






