library(shiny)
library(plotrix)

graficarcapaC <- function(radio,capa, titulo=""){
  
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
CAPA <- read.csv("CAPA800F5.csv")
load("simulacion_multi.RData")

graficarParticula<- function(color, particulas){
  for(i in 1: dim(particulas)[1]){
    draw.circle(x=particulas[i,3],y=particulas[i,4],
                r=particulas[i,1]/2,
                col=color)
  }
}

graficarParticulas<- function(tiempo){
  tt <- tiempo/5
  graficarcapaC(radio=800, CAPA, titulo="")
  colores <- rep(c("red", "yellow", "blue"), c(8,8,8))
  
  for(i in 1:tt){
    temp <- subset(simulacion_multi[[2]][[i]], 
                   simulacion_multi[[2]][[i]][,5]==1)
    graficarParticula(colores[i], temp)
    
  }
}


ui <- fluidPage(

    titlePanel("Título"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Tiempo en minutos",
                        min = 0,
                        step = 5,
                        max = 120,
                        value = 5)
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)


#Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
      
      graficarParticulas(input$bins)
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # 
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white',
        #      xlab = 'Waiting time to next eruption (in mins)',
        #      main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
