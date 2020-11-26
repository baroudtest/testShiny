library(shiny)
#install.packages("shiny")
library(lubridate)
#install.packages("lubridate")
library(circular)
#install.packages("circular")
library(ggplot2)
#install.packages("ggplot2")
library(cowplot)
#install.packages("cowplot")
library(doBy)
#install.packages("doby")
library(sf)
#install.packages("sf")
library(dplyr)
#install.packages("dplyr")
library(ggspatial)
#install.packages("ggspatial")
library(grid)
#install.packages("grid")
library(mapproj)
#install.packages("mapproj")
library(rgdal)
#install.packages("rgdal")
library(gdalUtils)
#install.packages("gdalUtils")
library(ggrepel)
#install.packages("ggrepel")
library(ggspatial)
#install.packages("ggspatial")
library(units)
#install.packages("units")
library(purrr)
#install.packages("purrr")
library(lwgeom)
#install.packages("lwgeom")
library(reshape2)
#install.packages("reshape2")
library(vegan)
#install.packages("vegan")

## Agencement fluipage sur la page, outils d'interactions (uploader un fichier, case à cocher, sliders,...)----
ui <- fluidPage(
## Contrôle sidebar -------------------------------------------
sidebarLayout(
  sidebarPanel(
    
# File inputs Sidebar------------------------------------------------
    fileInput("status",h2("Table IUCN")) # fileIput est l'outil permettant de lire un fichier de son choix à uploader
    ,
    
    h5("Le fichier à uploader ci-dessus est téléchargeable dans l'onglet Analyse & reporting du site FauneFac, il est intitulé statuts.csv"),
    br(),
    h5("(ou à uploader du serveur de la fac : soit fait par l'utilisateur, soit automatique)"),
    br(),
    br(),
    fileInput("file",h2("Table de données")) # fileIput est l'outil permettant de lire un fichier de son choix à uploader
    ,
    fileInput("infocam",h2("Informations de localisation des caméras")),
    fileInput("shp",h3("Ajouter un polygone de délimitation de zones")),
    numericInput("epsg",h3("Sélectionnez l'EPSG souhaité pour la cartographie"),32632),
    
    br(),
    div(textOutput("erreur1"), style = "color:red"),
    div(textOutput("erreur2"), style = "color:red"),
    div(textOutput("erreur3"), style = "color:red"),
    div(textOutput("erreur4"), style = "color:red"),
    div(textOutput("erreur5"), style = "color:red"),
    div(textOutput("erreur6"), style = "color:red"),
    div(textOutput("erreur7"), style = "color:red"),
    div(textOutput("erreur8"), style = "color:red"),
    div(textOutput("erreur9"), style = "color:red"),
    br(),
   
   
# Note utilisateur

 tabsetPanel(
tabPanel("Afficher la Note d'utilisateur",
         fluidRow(
           
  h4(div("Note importante concernant le format du jeu de données d'entrée :", style = "color:red")),
  p("1) Le jeu de donnée doit être au", strong("format de sortie .csv attribué par CameraBase, organisé comme suit"), "et doit comporter des colonnes ayant ces noms exacts, écrits dans cet ordre respectif :"),
  p(div(em("'Species'",
           br(),
           "'Camera'",
           br(),
           "'Site'",
           br(),
           "'Individuals'",
           br(),
           "'Date'",
           br(),
           "'Hour'",
           br(),
           "'zone d'étude'",
           br(),
           "'Image1'"), style = "color:blue")
  ),
  p("- La colonne :", br(), div(em("'inventory_ID'"),style = "color:blue"),br(),"Doit être rajoutée afin de renseigner le numéro d'inventaire pour une comparaison périodique. Il doit être compris entre 1 et n avec n le numéro du dernier inventaire."),
  
  p("2) Les individus identifiés doivent être renseignés dans le champ 'Species' par leur", strong("genre en majuscule"), "et leur", strong("espèce en minuscule"), "exemple :"),
  div(em("'Loxodonta cyclotis'"),style = "color:blue"),
  br(),
  p("- S'ils ne sont pas visibles, ils doivent être renseignés par :"),
  div(em("'no_sp'"), style = "color:blue"),
  br(),
  p("- S'ils ne peuvent être identifiés, ils doivent être renseignés par :"),
  div(em("'indetermined'"), style = "color:blue")
)),
tabPanel("Cacher la Note d'utilisateur",
         fluidRow(
         br()
))
)),
## Ouverture des onglets ----------------------------------------------
  mainPanel(br(),
            "Cette application Shiny est dédiée à l’analyse de données issues d’inventaire 
                        par pièges photographiques. Elle permet par une analyse automatisée de fournir 
                        quelques indicateurs qui caractérisent les inventaires de faune menés, la communauté 
                        et les espèces animale détectées le tout sous forme de tableaux, graphiques et cartes 
                        facilement téléchargeables.",
            br(),
    tabsetPanel(
## Onglet "Caractéristique des communautés" ---------------------------------------------
    tabPanel("Caractéristiques des communautés",
             fluidRow(
               column(width = 12,
                      "Vous trouverez ci-dessous un tableau récapitulatif de la communauté détectée durant votre/vos inventaire(s).",
                      br(),
                      tableOutput("richesse"),
                      downloadButton("downloadCom", "Download"),
                      br(),
                      br()
                      )
               ),
             fluidRow(
               column(width = 7,
                      "La richesse spécifique peut également s’analyser en fonction de l’effort d’échantillonnage réalisé, 
                      ce qui permet d’analyser l’exhaustivité de l’inventaire.",
                      br(),
                      plotOutput("accumul"),
                      downloadButton("downloadAccumul", "Download Graph"),
                      br()),
               column(width = 4,offset=1,
                      h3("Indices :"),
                      br(),
                      h5("Pourcentage de détections nocturnes"),
                      textOutput("indnoc"),
                      br(),
                      h5("Pourcentage de détections humaines"),
                      br(),
                      textOutput("homme"))
               )
             ),
## Onglet "Analyse par espèce" (sélection de l'espèce et du site) ---------------------------------
    tabPanel("Analyse par espèce",
             fluidRow(
               column(width = 12,
                                 "De multiples indices peuvent être générés à partir des données 
                                  obtenues par pièges photographiques. Nous avons décidé de vous 
                                  présenter seulement le taux de détection standardisé par l’effort 
                                  d’inventaire, communément décrit sous le terme de RAI en anglais 
                                  (Relative Abundance index) . D’autres analyses sont possibles et des 
                                  ressources sont mobilisables dans la partie « Pour en savoir plus »",
                                  br(),
                      selectizeInput(inputId = "selectSp",
                                     label = "Sélection de l'espèce",
                                     choices = "",
                                     multiple = TRUE),
                      br(),
                      selectizeInput(inputId = "selectLoc",
                                     label = "Sélection du site",
                                     choices = "",
                                     multiple = TRUE),
                      br(),
                      downloadButton("downloadData", "Download"),
                      br(),
                                  tableOutput("ab_rel"),
                      h3("Cacher/Dérouler le graphique"),
                      checkboxInput("affigraphique", "Afficher", value = TRUE),
                                  plotOutput("graph24h"),
                                  downloadButton("downloadGraph", "Download Graph")
                                
                          
    ))),

## Onglet "Cartes" ----------------------------------------------
    tabPanel("Cartes",
             fluidRow(
               column(width = 12,
                      plotOutput("test_shp"),
                      plotOutput("carte_richesse_spe")
    )))

## Fermeture des onglets ----------------------------------------

  ) #Close tabset panel
  
  ) #Close mainpanel

) #Close sidebarLayout

) #Close fluidPage



## Partie Server ------------------------------------------------ 
# traitement de données, récupération des inputs, préparation des outputs

server <- function(input, output, session) {   
  #Objet "session" rajouté pour le bon fonctionnement de l'observe
 
# Lecture et préparation des données issues des inputs : ---------------------------------
  
  IUCN <- reactive({
    
    req(input$status)
    esp <- read.csv(input$status$datapath,
                header = TRUE,
                sep = ";",
                quote = '"',
                colClasses = "character")
    esp
  })
  
  
   SHP <- reactive({
     shape <- st_read(input$shp$datapath,stringsAsFactors = F)
     shape
   })
      # Trouver une solution : st_read est pas supporté par shiny, il plante et dit que le fichier est corrompu...
  
  coordcam <- reactive({
    
    req(input$infocam)
    infocamera <- read.csv(input$infocam$datapath,
                           header = TRUE,
                           sep = ";",
                           quote = '"',
                           colClasses = "character")
    
    infocamera$utm_x <- as.numeric(infocamera$utm_x)
    infocamera$utm_y <- as.numeric(infocamera$utm_y)
    infocamera1 <- na.omit(infocamera)
    infocamera1
  })
  
  data <- reactive({

    req(input$file)
    df<- read.csv(input$file$datapath,
                  header = TRUE,
                  sep = ";",
                  quote = '"',
                  colClasses = "character")
    df$Individuals <- as.numeric(df$Individuals)
    #exclure no_sp, indetermined
    no_sp <- which(df$Species == "no_sp")
    df <- df[-no_sp, ]
    indet <- which(df$Species == "indetermined")
    df <- df[-indet, ]
    #remplacer cephalophe par cephalophus
    library(doBy)
    df$Species <- recodeVar(df$Species,"Cephalophe spp.","Cephalophus spp.")
    #Regrouper repetitions (30 min)
    datf <- data.frame(1,2,3,4,5,6,7)
    
    k <- colnames(df)
    colnames(datf) <- k 
    l <- nrow(df)
    s <- 1
    r <- 1
    # boucle lisant chaque ligne et la comparant avec la suivante, si plus de 30 min écoulées, écrit la ligne,
    # si moins de 30 min écoulé, observe l'espèce, si elle sont égales, passe la ligne sans écrire la deuxième ligne
    for (i in 1:l) {
      
      if (i < l) {s <- i +1 }
  
      # Encodage des heures des deux lignes respectives
      heurea <- factor(df$Hour[c(i)])
      heureb <- factor(df$Hour[c(s)])
      
      a <- lubridate ::hms(as.character(heurea))
      b <- lubridate ::hms(as.character(heureb))
      
      #calcul de la différence
      g <- as.duration(b-a)
      
      # vérification du critère temps
      if (g > 1800)  {datf[c(r),] <- df[c(i),]
      
      r <- r+1} # avancement à la prochaine ligne du nouveau dataframe
      #vérification du critère espèce 
      else if (df$Species[c(i)] == df$Species[c(s)]) {}
      else {datf[c(r),] <- df[c(i),]
      r <- r+1
      
      }
    }
    datf
  })
  
# Traitement des données de la partie communauté --------------------------------------
# table des informations sur les communautés par site 
tableCom <- reactive({
  effort <- aggregate(Individuals ~ Site, data = data(), sum)
  ncam <- aggregate(Individuals ~ Site+Camera, data = data(), sum)
  ncam <- aggregate(Individuals ~ Site, data = ncam, length)
  tab1 <- merge(ncam,effort,by=c("Site","Site"),all=T)
  rich <- aggregate(Individuals ~ Site+Species, data = data(), sum)
  rich <- aggregate(Individuals ~ Site, data = rich, length)
  tab1 <- merge(tab1,rich,by=c("Site","Site"),all=T)
  datEN <- merge(data(),IUCN(),by="Species",all.x=T)
  datEN <- subset(datEN,datEN$IUCN=="EN"|datEN$IUCN=="CR")
  EN <- aggregate(Individuals ~ Site+Species, data = datEN, sum)
  EN <- aggregate(Individuals ~ Site, data = EN, length)
  tab1 <- merge(tab1,EN,by=c("Site","Site"),all=T)
  names(tab1) <- c("Site","Nombre de caméras","Effort d'inventaire",
                   "Richesse spécifique","Nombre d'espèces menacées")
  n <- length(tab1$Site)
  tab1[n+1,1] <- "TOTAL"
  tab1[n+1,2] <- sum(tab1[-(n+1),2])
  tab1[n+1,3] <- sum(tab1[-(n+1),3])
  richTot <- aggregate(Individuals ~ Species, data = data(), length)
  tab1[n+1,4] <- nrow(richTot)
  ENTot <- aggregate(Individuals ~ Species, data = datEN, length)
  tab1[n+1,5] <- nrow(ENTot)
  tab1
})

output$richesse <- renderTable({
  tableCom()
})

# Gérer le téléchargement de la liste d'info par site
output$downloadCom <- downloadHandler(
  filename = function() {
    paste("Liste Com", ".csv", sep = "")
  },
  content = function(file) {
    write.table(tableCom(), file,quote = TRUE, sep = ";",dec=",",row.names = FALSE, col.names = TRUE)
  } 
)


# Création de la courbe d'accumulation en réactive de façon à pouvoir la télécharger
##PLUSIEURS SITES PAR GRAPHIQUE
accumul <- reactive ({
  
  matriceSite <- aggregate(Individuals ~ Date+Site+Species,data=data(),sum)
  matriceSite$Date <- dmy(matriceSite$Date)
  matriceSite <- matriceSite[order(matriceSite$Date),]
  
  sites <- aggregate(Individuals ~ Site, data = data(), sum)
  sites <- sites$Site
  nSites <- length(sites)
  
  par(mfrow = c(1,1))
  matrices_sep <- list()
  accumSite <- list()
  for (i in 1:nSites) {
    matrices_sep[[i]] <- subset(matriceSite, substr(matriceSite$Site,1,3)==substr(sites[i],1,3))
    matrices_sep[[i]] <- matrices_sep[[i]][,-2]
    matrices_sep[[i]] <- dcast(matrices_sep[[i]],Date~Species,fill=0)
    rownames(matrices_sep[[i]]) <- matrices_sep[[i]][,1]
    matrices_sep[[i]] <- matrices_sep[[i]][,-1]
    accumSite[[i]] <- specaccum(matrices_sep[[i]])
  }
  
  rich <- aggregate(Individuals ~ Site+Species, data = data(), sum)
  rich <- aggregate(Individuals ~ Site, data = rich, length)
  m <- which(rich$Individuals==max(rich$Individuals))
  plot(accumSite[[m]],xlab = "Nb de jours d'inventaire cumulés",ylab = "Nombre d'espèces",
       ci=0,col=2,key.pos=4) #ajouter legende : couleurs selon les sites
  title("Courbes d'accumulation")
  legend(x="bottomright",legend=c(sites),col=2:(nSites+1),lty=1:1,lwd=1:1,cex=0.8)
  for (i in 1:nSites) {
    plot(accumSite[[i]],xlab = "Nb de jours d'inventaire cumulés",ylab = "Nombre d'espèces",
         ci=0,add=T,col=i+1) #ajouter legende : couleurs selon les sites
  }
})

# Encodage du graphique réactif en output de manière à l'afficher
output$accumul <- renderPlot({
  req(input$file)
  accumul()
})

# gérer le téléchargement du graphique d'accumulation 
output$downloadAccumul <- downloadHandler(
  # filename pour définir le nom par défaut du fichier produit, Content pour choisir le graph dans l'image
  filename = function() {paste("accumul", '.png', sep='') }, #ou //input$selectSp si choix avec tot
  content = function(file) {
    
    png(file)
    print(accumul())
    dev.off() 
  }
  
)


# indice d'occurence nocturne
output$indnoc <- renderText({
  req(input$file)
  
  ligne <- nrow(data())
  b <- 0
  #comptage des détection nocturne en observant chaque lingne de Data
  for (i in 1:ligne) {
    heure <- factor(data()$Hour[c(i)])
    a <- lubridate::hms(as.character(heure))
    c <- hour(a)
    if (c[c(1)] > 18) { }
    else  if (c[c(1)] < 6) {b <- b + data()$Individuals[c(i)]}
    
  }
  # nombre de détections nocturne sur nombre de détections totales
  d <- (b / ligne) *100
  d
})

# Indice de présence humaine
output$homme <- renderText({
  req(input$file)
  
  # prend le nombre de lignes totale
  ligne <- nrow(data())
  b <- 0
  # compte le nombre de lignes correspondante à Homo sapiens
  for (i in 1:ligne) {
    if (data()$Species[c(i)] == "Homo sapiens")
    {b <- b + data()$Individuals[c(i)]}
    
  }
  # rapport du nombre de "Homo sapiens" sur le nombre de lignes totales
  d <- (b / ligne) *100
  d
})

observe({
  updateSelectizeInput(
    session,
    inputId = "selectSp",
    choices = c("All", data()$Species),
    selected = "All"
  )  
})

observe({
  updateSelectizeInput(
    session,
    inputId = "selectLoc",
    choices = c("All", data()$Site),
    selected = "All"
  )  
})

# Traitement des données des espèces : ----------------------------------------------
# table des informations par espèces , abondance relative, nombre d'individus détecté
# faudrait-il ajouer détection par mois ? 
tableEsp <- reactive({
  req(input$selectSp, input$selectLoc)
  ###
  #Tentative de caser le ALL
  if(input$selectSp == "All")
    selesp <- as.data.frame(data()$Species)
  else 
    selesp <- as.data.frame(input$selectSp, 
                            row.names = NULL)
  colnames(selesp) <- "Species"
  #Loc
  if(input$selectLoc == "All")
    selloc <- as.data.frame(data()$Site)
  else
    selloc <- as.data.frame(input$selectLoc,
                            row.names = NULL)
  colnames(selloc) <- "Site"
  #bondocde
  nb <- aggregate(Individuals ~ Species+Site, data = data(), sum)
  jours <- aggregate(Individuals ~ Species+Site+Date, data = data(), sum) # ! colonne site ou choix prealable ?
  nj <- aggregate(Date ~ Species+Site, data = jours, length)
  table <- merge(nb,nj,by=c("Species","Site"))
  if(input$selectLoc != "All")
    table <- merge(table, selloc, by = "Site")
  if(input$selectSp != "All")
    table <- merge(table, selesp, by = "Species")
  table$Date <- table$Individuals/table$Date
  tot <- sum(data()$Individuals)
  ab <- (table$Individuals/tot)*100
  table <- cbind(table,ab)
  table <- merge(table,IUCN(),by="Species",all.x=T)
  names(table) <- c("Espèce","Site","Nombre d'individus","Taux de détection (nb par jour)",
                    "Abondance relative (en %)","Statut IUCN")
  table
})

output$ab_rel <- renderTable({
  tableEsp()
})


# Gérer le télchargement de la liste d'info par espèce
output$downloadData <- downloadHandler(
  filename = function() {
    paste("Liste", ".csv", sep = "")
  },
  content = function(file) {
    write.table(tableEsp(), file,quote = TRUE, sep = ";",dec=",",row.names = FALSE, col.names = TRUE)
  } 
)

# Création d'une cartographie des abondances relatives par espèce ------------------------------

# Traitement et préparation des données utiles aux cartes 

EPSG <- reactive({input$epsg}) # On récupère l'epsg de manière réactive.

données_cartes_ab_rel_esp <- reactive({ 
  
  req(data())
  data1 <- data()
  # Création du jeu de données aggrégées sans coordonnées : 
  aboncam <- aggregate(Individuals ~ Species+Camera, data = data1, sum)
  tot <- sum(aboncam$Individuals)
  aboncam$aboncam1 <- (aboncam$Individuals/tot)*100
  aboncam2 <- cbind(nb,aboncam1)
  aboncam2
})
  # Création du jeu de données avec les coordonnées par jointure (objet data.frame)
gps_cartes_richesse_spe <- reactive({
  # On récupère les données :
  req(coordcam())
  coordocam <- coordcam() # On met le fichier d'entrée dans coordocam
  req(données_cartes_ab_rel_esp)
  aboncam3 <- données_cartes_ab_rel_esp()
  
  coordocam$name <- as.character(coordocam$name) # On s'assure que le nom de la camera est bien en character
  coordocam1 = dplyr::select(coordocam,utm_x:utm_y,Camera = name) # On vire ce qui n'est pas utile, on ne garde que le nom de la camera (colonne name renommée Camera) et les coordonnées x et y utm
  
  coordocam1$Camera=as.character(coordocam1$Camera)
  aboncam3$Camera=as.character(aboncam3$Camera) # On transforme les valeurs des champs Camera en character afin de pouvoir effectuer la jointure
  
  aboncoordocam = left_join(aboncam3,coordocam1, by = c("Camera" = "Camera")) #jointure gauche des coordonnées !
  
  # Transformation en d.f de classe sf (d.f spatial), ajoute le champ geometry qui contient le couple de coordonnées, et retire les 2 champs de coordonnées simple:
  req(EPSG())
  epsg <-EPSG()
  aboncoordocam1 <- st_as_sf(aboncoordocam,coords=c("utm_x","utm_y"),crs=epsg)
  aboncoordocam1})
  
# Manip de sélection de l'espèce :

gps_cartes_abon_paresp <- reactive({
  req(gps_cartes_richesse_spe())
  aboncord <- gps_cartes_richesse_spe()
  req(input$selectSp)
  x <- as.character(input$selectSp)
  aboncoordocam2 <- aboncord
  # si "All" est encodé, graphique de toute les epsèces, si le nom d'une espèe est encodé, le prend en compte
  if (input$selectSp != "All")
    aboncoordocam2 <- aboncord[aboncord$Species == x,]
  aboncordocam2 # A modifier, pour le test seulement 
})
  
 # Test carto polygone 

 output$test_shp <- renderPlot ({
   req(EPSG())
   epsg <-EPSG()
   req(SHP())
   shp <- SHP()
   emmprise <- st_bbox(shp)
   xmin <- emmprise[1]
   ymin <- emmprise[2]
   xmax <- emmprise[3]
   ymax <- emmprise[4]
   
   
   # Préparation de coeffs issus de l'emprise des coordonnées afin de produire une marge pour une meilleure visibilité des points et pour placer l'échelle et la flèche nord
   diffx <- abs(abs(xmax)-abs(xmin))
   diffy <- abs(abs(ymax)-abs(ymin))
   diffx
   diffy
   coefx <- as.numeric(diffx/7)
   coefy <- as.numeric(diffy/7)
   coefx
   coefy
   
   poly1 <- ggplot() +
     geom_sf(data=shp) +
     coord_sf(crs = st_crs(epsg),xlim=c(xmin-coefx,xmax+coefx),ylim=c(ymin-coefy,ymax+coefy), expand = FALSE)
   poly1  
   })
 
 # Test carto richesse spé :
 
 output$carte_richesse_spe <- renderPlot ({ 
   # Calcul et affectation des données de l'emprise de la carte :
   req(EPSG())
   epsg <-EPSG()
   req(gps_cartes_richesse_spe())
   richespe <- gps_cartes_richesse_spe()
   emmprise <- st_bbox(richespe)
   xmin <- emmprise[1]
   ymin <- emmprise[2]
   xmax <- emmprise[3]
   ymax <- emmprise[4]
   
   # Préparation de coeffs issus de l'emprise des coordonnées afin de produire une marge pour une meilleure visibilité des points et pour placer l'échelle et la flèche nord
   diffx <- abs(abs(xmax)-abs(xmin))
   diffy <- abs(abs(ymax)-abs(ymin))
   diffx
   diffy
   coefx <- as.numeric(diffx/7)
   coefy <- as.numeric(diffy/7)
   coefx
   coefy
   
   carte1 <- ggplot() +
     geom_sf(mapping=aes(size=aboncam1, color=aboncam1) ,data=richespe) +
     coord_sf(crs = st_crs(epsg),xlim=c(xmin-coefx,xmax+coefx),ylim=c(ymin-coefy,ymax+coefy), datum = sf::st_crs(4326), expand = FALSE) +
     scale_size_continuous(name = "Abondance (en %)", range=c(1,10)) +
     scale_colour_gradientn(name = "Abondance (en %)", colours = terrain.colors(5)) + 
     guides(size=FALSE) +
     labs(title = "Cartographie de l'abondance des espèces ",
          subtitle = "Par caméra et par espèce",
          caption = "Données source : data caméra trap",
          x = "utm_x", y = "utm_y") +
     theme_dark() +
     theme(
       legend.position = c(1.2, 0.5),
       legend.direction = "vertical",
       legend.key.size = unit(0.5, "cm"),
       legend.key.width = unit(0.5,"cm"),
       legend.title = element_text(color = "red", size = 9, face = "bold"),
       legend.text = element_text(color = "red", size = 8),
       plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
       plot.subtitle = element_text(hjust = 0.5),
       axis.title.x = element_text(color = "red", size = 10, face = "bold"),
       axis.title.y = element_text(color = "red", size = 10, face = "bold")) +
     annotation_scale(location = "tr", width_hint = 0.3) +
     annotation_north_arrow(location = "tr", which_north = "true", 
                            pad_x = unit(0.2, "cm"), pad_y = unit(0.4, "cm"),
                            style = north_arrow_fancy_orienteering)
   
   carte1
     })


# Création du graphique d'activité en 24h en réactive de façon à pouvoir le télécharger

  graph24 <- reactive ({
# récupérer l'espèce encodée 
  
  x <- as.character(input$selectSp)
  
# récupérer le dataframe nettoyé
  df <- data()
# si "All" est encodé, graphique de toute les epsèces, si le nome d'une espèe est encodé, le prend en compte
  if (input$selectSp != "All")
  df <- df[df$Species == x,]
  
  # récupérer les heures concernées de l'espèce choisie
  
  heurea <- df$Hour
  a <- lubridate ::hms(as.character(heurea))
  
  hour_of_event <- hour(a)
  
  eventdata <- data.frame(datetime = df$Date, eventhour = hour_of_event)
  
  # désignation de la période nocturne (entre 6h et 18h)
  eventdata$Day <- eventdata$eventhour %in% seq(6, 18)
  
  # création du graphique, mise en couleur du "jour", nombre de 0 à 24h
  frete<- ggplot(eventdata, aes(x = eventhour, fill = Day)) + geom_histogram(breaks = seq(0, 
                                                                                              24)) + coord_polar(start = 0) + theme_minimal() + 
    scale_fill_brewer() + ylab("Nombre de détection") + ggtitle(paste(input$selectSp,"Répartition des détection par heure", sep=" ")) + 
    scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 
                                                                                24))
  
  frete
})
  
# Encodage du graphique réactif en output de manière à l'afficher
output$graph24h <- renderPlot({
  req(input$affigraphique == TRUE)
req(input$file)
  graph24()
})

# réception de l'spèce choisie en réactif

chosenSp <- reactive ({
  
  Input$selectSp
})

# gérer le téléchargement du graphique circulaire
output$downloadGraph <- downloadHandler(
  # filename pour définir le nom par défaut du fichier produit, Content pour choisir le graph dans l'image
  filename = function() {paste(input$selectSp,"graph24", '.png', sep='') },
  content = function(file) {
    
    png(file)
    print(graph24())
    dev.off() 
  }
  
)

err <- reactive({
  req(input$file)
  
  LengthOk <- 0
  SpOk <- 0
  CamOk <- 0
  SiOk <- 0
  InOk <- 0
  DaOk <- 0
  HoOk <- 0
  ImOk <- 0
  x <- c(1,2,3,4,5,6,7)
  
  x <- names(data())
  if (length(x) == 7) { LengthOk <- 1 }
   
  if (LengthOk == 1) {LOk <- "Length Ok"}
  else { LOk <- "La feuille de donnée contient trop de colonnes"} 
  
  if (x[1] == "Species") {SpOk <- 1 }
  else if (x[2] == "Species") {SpOk <-1 }
  else if (x[3] == "Species") {SpOk <-1 }
  else if (x[4] == "Species") {SpOk <-1 }
  else if (x[5] == "Species") {SpOk <-1 }
  else if (x[6] == "Species") {SpOk <-1 }
  else if (x[7] == "Species") {SpOk <-1 }
  
  if (SpOk == 1) {SOk <- "Species Ok"}
  else { SOk <- "Erreur, impossible de trouver la colonne 'Species'. vérifiez la syntaxe du jeu de donnée"}
  
  if (x[1] == "Camera") {CamOk <- 1 }
  else if (x[2] == "Camera") {CamOk <-1 }
  else if (x[3] == "Camera") {CamOk <-1 }
  else if (x[4] == "Camera") {CamOk <-1 }
  else if (x[5] == "Camera") {CamOk <-1 }
  else if (x[6] == "Camera") {CamOk <-1 }
  else if (x[7] == "Camera") {CamOk <-1 }
  
  if (CamOk == 1) {COk <- "Camera Ok"}
  else { COk <- "Erreur, impossible de trouver la colonne 'Camera'. vérifiez la syntaxe du jeu de donnée"}
  
  if (x[1] == "Site") {SiOk <- 1 }
  else if (x[2] == "Site") {SiOk <-1 }
  else if (x[3] == "Site") {SiOk <-1 }
  else if (x[4] == "Site") {SiOk <-1 }
  else if (x[5] == "Site") {SiOk <-1 }
  else if (x[6] == "Site") {SiOk <-1 }
  else if (x[7] == "Site") {SiOk <-1 }
  
  if (SiOk == 1) {StOk <- "Site Ok"}
  else { StOk <- "Erreur, impossible de trouver la colonne 'Site'. vérifiez la syntaxe du jeu de donnée"}
  
  if (x[1] == "Individuals") {InOk <- 1 }
  else if (x[2] == "Individuals") {InOk <-1 }
  else if (x[3] == "Individuals") {InOk <-1 }
  else if (x[4] == "Individuals") {InOk <-1 }
  else if (x[5] == "Individuals") {InOk <-1 }
  else if (x[6] == "Individuals") {InOk <-1 }
  else if (x[7] == "Individuals") {InOk <-1 }

  if (InOk == 1) {IOk <- "Individuals Ok"}
  else { IOk <- "Erreur, impossible de trouver la colonne 'Individuals'. vérifiez la syntaxe du jeu de donnée"}
  
  if (x[1] == "Date") {DaOk <- 1 }
  else if (x[2] == "Date") {DaOk <-1 }
  else if (x[3] == "Date") {DaOk <-1 }
  else if (x[4] == "Date") {DaOk <-1 }
  else if (x[5] == "Date") {DaOk <-1 }
  else if (x[6] == "Date") {DaOk <-1 }
  else if (x[7] == "Date") {DaOk <-1 }
  
  if (DaOk == 1) {DOk <- "Data Ok"}
  else { DOk <- "Erreur, impossible de trouver la colonne 'Date'. vérifiez la syntaxe du jeu de donnée"}

  
  if (x[1] == "Hour") {HoOk <- 1 }
  else if (x[2] == "Hour") {HoOk <-1 }
  else if (x[3] == "Hour") {HoOk <-1 }
  else if (x[4] == "Hour") {HoOk <-1 }
  else if (x[5] == "Hour") {HoOk <-1 }
  else if (x[6] == "Hour") {HoOk <-1 }
  else if (x[7] == "Hour") {HoOk <-1 }
  
  if (HoOk == 1) {HOk <- "Hour Ok"}
  else { HOk <- "Erreur, impossible de trouver la colonne 'Hour'. vérifiez la syntaxe du jeu de donnée"}
  
  if (x[1] == "Image1") {ImOk <- 1 }
  else if (x[2] == "Image1") {ImOk <-1 }
  else if (x[3] == "Image1") {ImOk <-1 }
  else if (x[4] == "Image1") {ImOk <-1 }
  else if (x[5] == "Image1") {ImOk <-1 }
  else if (x[6] == "Image1") {ImOk <-1 }
  else if (x[7] == "Image1") {ImOk <-1 }
  
  if (ImOk == 1) {IgOk <- "Image Ok"}
  else { IgOk <- "Erreur, impossible de trouver la colonne 'Image'. vérifiez la syntaxe du jeu de donnée"}

err <- c(LOk, SOk, DOk, COk, StOk, IOk, DOk, HOk, IgOk)
err
  
})

output$erreur1 <- renderText({
 req(input$file)
  err()[1]
})


output$erreur2 <- renderText({
  req(input$file)
  err()[2]
})


output$erreur3 <- renderText({
  req(input$file)
  err()[3]
})


output$erreur4 <- renderText({
  req(input$file)
  err()[4]
})


output$erreur5 <- renderText({
  req(input$file)
  err()[5]
})


output$erreur6 <- renderText({
  req(input$file)
  err()[6]
})


output$erreur7 <- renderText({
  req(input$file)
  err()[7]
})


output$erreur8 <- renderText({
  req(input$file)
  err()[8]
})


output$erreur9 <- renderText({
  req(input$file)
  err()[9]
})




}

## Run the app ---------------------------------------------------
shinyApp(ui = ui, server = server)
##Ok
##Voyez-vous ce message ? (Flo)