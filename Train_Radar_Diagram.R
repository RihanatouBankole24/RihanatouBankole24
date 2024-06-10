#clean the environment
rm(list=ls())

#checking the current directory
getwd()

#setting the working directory
setwd("C:/Users/HP-PC/Desktop/Ruforum_Formation")

#Import data
Qualité_Pain <- data.frame( 
  row.names = c('p1','p2','p3','p4','p5'),
     Gout= c(3,5,7,1,3),
        
     Couleur= c(5,6,5,6,8),

     tendreté = c(9,4,3,1,7),

     croustillance = c(3,2,4,5,6),

     levée = c(5,3,6,1,6),

     élasticité = c(1,10,9,7,2),

     texture = c(3,4,10,5,3),

     apparence= c(9,4,5,8,9),

     senteur = c(5,9,6,10,6)

)

Qualité_Pain


#graphique radar fmsb

#install packages
  
#install.packages("fmsb")


  #data preparation
 
  summary(Qualité_Pain)
  
  
  max_min<- data.frame(
    
    Gout = c(7,0),Couleur = c(8,5),tendreté = c(9,1),
    croustillance = c(6,2), levée = c(6,1),élasticité = c(10,1),
    texture = c(10,3),apparence = c(9,4),senteur = c(10,5)
  )
  
  rownames(max_min) <- c('Max','Min')
  
  #attach the cloud of variables to data
  
  Data <- rbind(max_min,Qualité_Pain)
  
  Data
  
  #Graphique radar de base
  #Representons les donnees pour le Pain P1
  
  #loading packages
  
  library(fmsb)
  library(scales)
 
   P1 <- Data[c("Max", "Min",'p1'),]
  
  radarchart(P1)
  
  
  #rownames(Data[c(3:7),])
  
  
  
  
  create_beautiful_radarchart <- function(Qualité_Pain, color = "#00AFBB", 
                                          vlabels = colnames(Qualité_Pain), vlcex = 0.59,
                                          caxislabels = NULL, title = 'Qualité du Pain', ...){
    radarchart(
      Qualité_Pain, axistype = 1,
      # Personnaliser le polygone
      pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
      # Personnaliser la grille
      cglcol = "grey", cglty = 1, cglwd = 0.8,
      # Personnaliser l'axe
      axislabcol = "grey", 
      # Étiquettes des variables
      vlcex = vlcex, vlabels = vlabels,
      caxislabels = caxislabels, title = title, ...
    )
  }
  
  # Réduire la marge du graphique à l'aide de par()
  op <- par(mar = c(1, 2, 2, 1))
  create_beautiful_radarchart(P1, caxislabels = c(1, 2.5, 4.5, 6.5, 10))
  par(op)
 
  # Réduire la marge du graphique à l'aide de par()
  op <- par(mar = c(1, 2, 2, 2))
  # Créer les graphiques radar
  library(fmsb)
  create_beautiful_radarchart(
    Data, caxislabels = c(0, 2.5, 5, 7.5, 10),
    color = c("#00AFBB", "#E7B800", "#FC4E07", "#6d904f","#A1FF33")
  )
   # Ajouter une légende horizontale
  legend(
    x = "bottom", legend =rownames(Data[c(3:7),]), horiz = TRUE,
    bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07", "#6d904f", "#A1FF33"),
    text.col = "black", cex = 0.8, pt.cex = 1.5
  )
  par(op)
  
  # Définir les couleurs et les titres
  colors <- c("#00AFBB", "#E7B800", "#FC4E07", "#6d904f", "#A1FF33")
  titles <- c("p1", "p2", "p3","p4","p5")
  
  # Réduire la marge du graphique à l'aide de par()
  # Diviser l'écran en 3 parties
  op <- par(mar = c(1, 1, 1, 1))
  par(mfrow = c(2,3))
  
  # Créer le graphique radar
  for(i in 1:5){
    create_beautiful_radarchart(
      data = df[c(1, 2, i+2), ], caxislabels = c(0, 2.5, 5, 7.5, 10),
      color = colors[i], title = titles[i]
    )
  }
  par(op)