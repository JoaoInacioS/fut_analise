#Grafico do campo:
Campo <- function(timeA,timeB){
  
  # Dimensões do campo de futebol (em metros)
  comprimento <- 120
  largura <- 80
  grande_area_largura <- 40.3
  grande_area_profundidade <- 16.5
  pequena_area_largura <- 18.32
  pequena_area_profundidade <- 5.5
  goleira_largura <- 7.32
  raio_circulo_central <- 9.15
  
  desenhar_retangulo <- function(xmin, xmax, ymin, ymax, color="black") {
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color=color, fill=NA)
  }
  
  ggplot() +
    # Campo (borda externa)
    desenhar_retangulo(0, comprimento, 0, largura) +
    
    # Grande área (lado esquerdo e direito)
    desenhar_retangulo(0, grande_area_profundidade, (largura - grande_area_largura) / 2, (largura + grande_area_largura) / 2) +
    desenhar_retangulo(comprimento - grande_area_profundidade, comprimento, (largura - grande_area_largura) / 2, (largura + grande_area_largura) / 2) +
    
    # Pequena área (lado esquerdo e direito)
    desenhar_retangulo(0, pequena_area_profundidade, (largura - pequena_area_largura) / 2, (largura + pequena_area_largura) / 2) +
    desenhar_retangulo(comprimento - pequena_area_profundidade, comprimento, (largura - pequena_area_largura) / 2, (largura + pequena_area_largura) / 2) +
    
    # Goleira (lado esquerdo e direito)
    desenhar_retangulo(0, 0.3, (largura - goleira_largura) / 2, (largura + goleira_largura) / 2) +
    desenhar_retangulo(comprimento - 0.3, comprimento, (largura - goleira_largura) / 2, (largura + goleira_largura) / 2) +
    
    # Linha central
    geom_segment(aes(x = comprimento / 2, y = 0, xend = comprimento / 2, yend = largura), color = "black") +
    
    # Círculo central
    annotate("path",
             x = comprimento / 2 + raio_circulo_central * cos(seq(0, 2 * pi, length.out = 100)),
             y = largura / 2 + raio_circulo_central * sin(seq(0, 2 * pi, length.out = 100)),
             color = "black") +
    
    # Ajustes de coordenadas e proporções
    coord_fixed(ratio = 1) +
    theme_void() +
    annotation_custom(timeA, xmin = 10, xmax = 20, ymin = 0.8, ymax = 5) + # bandeira argentina
    annotation_custom(timeB, xmin = 100, xmax = 110, ymin = 0.8, ymax = 5) + # bandeira colombia
    geom_segment(aes(x = 25,y = -2.5, xend = 95, yend = -2.5),arrow = arrow(length = unit(0.2, "cm")),size=0.4)
}