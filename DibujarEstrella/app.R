library(shiny)
library(plotly)

ui <- fluidPage(
   
   titlePanel("Dibujar y transformar una estrella"),
  
   sidebarLayout(
      sidebarPanel(
         sliderInput("n",
                     "Extremidades de la estrella:",
                     min = 3,
                     max = 100,
                     value = 5)
      ),

      mainPanel(
         plotOutput("dibujarEstrella")
      )
   )
)

server <- function(input, output) {
  calcularEstrella <- function(n, r=1, rIntPct=50, alfa0grados=0, fill="blue") {#Me pasas la distancia de los puntos al centro; qe tanto qeres qe se hunda (en porcentaje de r) y la cantidad de puntos qe qeres qe dibuje
    alfa0 <- alfa0grados*pi/180
    
    x <- c() #Aca voy a ir guardando los valores de x e y
    y <- c()
    
    if (n >= 3) { #Revisa qe la estrella tenga tres o mas puntos
      rint <- r-(rIntPct*r/100) #Calculo con el porcentaje de hundimiento la distancia de los puntos internos al centro
      alfa <- 2*pi/n #Calcula la apertura entre dos extremidades
      
      for (i in 0:(n-1)) { #Puntos externos
        ex <- r*cos(i*alfa+alfa0) #Calcula la posicion de los puntos extremos
        ey <- r*sin(i*alfa+alfa0)
        
        x <- append(x, ex)
        y <- append(y, ey)
        
        #Puntos internos
        ix <- rint*cos((i+1/2)*alfa+alfa0) #Calcula la posicion de los puntos internos
        iy <- rint*sin((i+1/2)*alfa+alfa0)
        
        x <- append(x, ix)
        y <- append(y, iy)
      }
      
    } else {
      print("ERROR: No, no, no. Las estrellas tienen tres o mas puntos")
    }
    dfEst <- data.frame(
      x,
      y,
      color = rep(fill, length(x))
    )
  }
  
  dibujarEstrella <- function(input$n, r=1, rIntPct=50, alfa0=0, fill="blue") {
    dfEst <- calcularEstrella(n, r, rIntPct, alfa0, fill)
    
    dibujo <- ggplot(dfEst) +
      geom_polygon(aes(x= x, y=y, fill = color))
    
    ggplotly(dibujo, width = 600, height = 600) %>%
      animation_opts(1000)
  }
}

# Run the application 
shinyApp(ui = ui, server = server)

