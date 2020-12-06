library(shinycssloaders)
#install.packages("shinycssloaders")
library(ggplotify)
#install.packages("ggplotify")
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
library(shinycssloaders)
#install.packages("shinycssloaders")
library(shinythemes)
#install.packages("shinythemes")
library(data.table)
#install.packages("data.table")
library(shinydashboard)
#install.packages("shinydashboard")

#-------------------------------
#-------------------------------
#Elaboration de l'UI
#-------------------------------
#-------------------------------
ui <- dashboardPage(
  skin = "yellow",
  #-------------------------------
  #Titre
  #-------------------------------
  dashboardHeader(
    title = "Application",
    titleWidth = 300,
    disable = F
  ),
  #-------------------------------
  #Sidebar
  #-------------------------------
  dashboardSidebar(
    width = 300,
    #Menu et onglets divers
    sidebarMenu(
      menuItem(text = "Chargement des données", 
               tabName = "upload_donnees",
               icon = icon("upload"),
               selected = TRUE
      ),
      menuItem(text = "Caractéristiques des communautés", 
               tabName = "caract_com", 
               icon = icon("book-open")
      ),
      menuItem(text = "Analyse par espèce", 
               tabName = "anal_esp", 
               icon = icon("paw")
      ),
      menuItem(text = "Cartes", 
               tabName = "carto", 
               icon = icon("map")
      )
    ),
    
    br(),
    br(),
    br(),
    
    column(width = 12,
           offset = 0,
           p(style = "text-align:justify;",
             "Cette application Shiny est dédiée à l’analyse de données issues d’inventaire par pièges photographiques."),
           br(),
           p(style = "text-align:justify;",
             "Elle permet par une analyse automatisée de fournir quelques indicateurs qui caractérisent les inventaires de faune menés, 
             la communauté et les espèces animale détectées le tout sous forme de tableaux, 
             graphiques et cartes facilement téléchargeables."),
           br(),
           br(),
           br(),
           img(src = "gxabt_logo.png",
               width = 270)
    )
  ), #Fermeture sidebar
  #-------------------------------
  #Corps principal
  #-------------------------------
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload_donnees",
              fluidRow(
                box(title = "Données de pièges photographiques",
                    width = 4,
                    status = "warning",
                    solidHeader = T,
                    fileInput(inputId = "file", 
                              label = "Table de données"),
                    div(textOutput("fichier1"), style = "color:green"),
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
                    fileInput(inputId = "status",
                              label = "Statuts IUCN"),
                    h5("Le fichier à uploader ci-dessus est téléchargeable dans l'onglet Analyse & reporting du site FauneFac, il est intitulé statuts.csv"),
                    h5("(ou à uploader du serveur de la fac : soit fait par l'utilisateur, soit automatique)")
                ),#Fermeture box
                
                box(title = "Données spatiales",
                    width = 4,
                    status = "warning",
                    solidHeader = T,
                    fileInput(inputId = "infocam",
                              label = "Informations de localisation des caméras"),
                    div(textOutput("fichier2"), style = "color:green"),
                    br(),
                    br(),
                    div(textOutput("erreurCam1"), style = "color:red"),
                    div(textOutput("erreurCam2"), style = "color:red"),
                    div(textOutput("erreurCam3"), style = "color:red"),
                    div(textOutput("erreurCam4"), style = "color:red"),
                    div(textOutput("erreurCam5"), style = "color:red"),
                    br(),
                    br(),
                    numericInput("epsg","Sélectionnez le code EPSG utilisé pour la cartographie",32632),
                    h5("Le code EPSG sélectionné doit correspondre au code de la zone UTM de prise des points GPS, dans le datum WGS84. Il est renseigné sur le site 'www.epsg.io' avec une recherche selon un terme composé du nom du datum, séparé du nom de la zone UTM par une barre oblique, respectivement comme ceci : 'WGS 84 / UTM zone 32N'")
                ),
                #Fermeture box
                
                box(title = "Note d'utilisateur",
                    width = 4,
                    status = "danger",
                    solidHeader = T,
                    # Note utilisateur
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
                    p("2) Les individus identifiés doivent être renseignés dans le champ 'Species' par leur", strong("genre en majuscule"), "et leur", strong("espèce en minuscule"), "exemple :"),
                    div(em("'Loxodonta cyclotis'"),style = "color:blue"),
                    br(),
                    p("- S'ils ne sont pas visibles, ils doivent être renseignés par :"),
                    div(em("'no_sp'"), style = "color:blue"),
                    br(),
                    p("- S'ils ne peuvent être identifiés, ils doivent être renseignés par :"),
                    div(em("'indetermined'"), style = "color:blue")
                    
                )#Fermeture box
                
              )#fermeture fluidrow
      ),#Fermeture tabitem "charg_donn"
      
      
      #Caract communautés
      tabItem(tabName = "caract_com",
              fluidRow(
                box(title = "Tableau caractéristique",
                    width = 6,
                    status = "warning",
                    solidHeader = T,
                    "Vous trouverez ci-dessous un tableau récapitulatif de la communauté détectée durant votre/vos inventaire(s).",
                    br(),
                    actionButton("info","Info"),
                    div(style ='overflow-x:scroll',
                        withSpinner(tableOutput("richesse"))),
                    downloadButton("downloadCom", "Download")
                ),#Fermeture box
                
                box(title = "Indice de détection nocturne",
                    width = 3,
                    status = "warning",
                    solidHeader = T,
                    "Détection nocturne en pourcents",
                    tableOutput("indnoc")
                ),#fermeture box
                
                box(title = "Indice de présence humaine",
                    width = 3,
                    status = "warning",
                    solidHeader = T,
                    "Nombre moyen d'hommes détectés sur une durée d'un mois (30 jours)",
                    tableOutput("homme")
                ),
                
                box(title = "Effort d'échantillonnage",
                    width = 6,
                    status = "warning",
                    solidHeader = T,
                    "La richesse spécifique peut également s’analyser en fonction de l’effort d’échantillonnage réalisé, ce qui permet d’analyser l’exhaustivité de l’inventaire.",
                    selectizeInput(inputId = "selectSite",
                                   label = "Affichage des courbes",
                                   choices = "",
                                   selected = "",
                                   multiple = TRUE),
                    withSpinner(plotOutput("accumul")),
                    downloadButton("downloadAccumul", "Download Graph")
                )
              )#Fluidrow
              
              
      ),#fermeture tabitem caract_comm      
      
      
      #Analyse par espèce
      tabItem(tabName = "anal_esp",
              fluidRow(
                box(title = "Tableau analytique",
                    width = 6,
                    #height = 900,
                    status = "warning",
                    solidHeader = T,
                    p(style = "text-align:justify;",
                      "De multiples indices peuvent être générés à partir des données obtenues par pièges photographiques. Nous avons décidé de vous présenter seulement le taux de détection standardisé par l’effort d’inventaire, communément décrit sous le terme de RAI en anglais (Relative Abundance index) . D’autres analyses sont possibles et des ressources sont mobilisables dans la partie « Pour en savoir plus »"),
                    p(style = "text-align:justify;",
                      "Il est possible de sélectionner une, plusieurs ou toutes les espèces. Il en est de même concernant les sites."),
                    selectizeInput(inputId = "selectSp_tab",
                                   label = "Sélection de l'espèce",
                                   choices = "",
                                   selected = "",
                                   multiple = TRUE),
                    selectizeInput(inputId = "selectLoc_tab",
                                   label = "Sélection du site",
                                   choices = "",
                                   multiple = TRUE),
                    actionButton("infoTableEsp","Info"),
                    div(style = 'overflow-y:scroll;height:520px',
                        withSpinner(tableOutput("ab_rel"))),
                    downloadButton("downloadData", "Download")
                ),#fermeture box
                
                box(title = "Figures",
                    status = "warning",
                    solidHeader = T,
                    tabBox(
                      title = NULL,
                      width = 12,
                      tabPanel(title = "Graphique chelou à nommer",
                               width = 12,
                               status = "warning",
                               solidHeader = T,
                               selectizeInput(inputId = "selectSp_graph",
                                              label = "Sélection de l'espèce",
                                              choices = "",
                                              selected = ""),
                               selectizeInput(inputId = "selectLoc_graph",
                                              label = "Sélection du site",
                                              choices = "",
                                              selected =""),
                               actionButton("infoRythmeActiv","Info"),
                               withSpinner(plotOutput("graph24h")),
                               downloadButton("downloadGraph", "Télécharger le Graphique en .png"),
                               downloadButton("downloadGraphSVG", "Télécharger le Graphique en .SVG")
                      ),
                      
                      tabPanel(title = "Diagramme en tarte",
                               width = 12,
                               status = "warning",
                               solidHeader = T
                               
                      )
                    )#Fermeture tabbox
                    )#Fermeture box
                
              )#Fermeture fluidrow
              
      ),#fermeture tabitem anal_esp
      
      tabItem(tabName = "carto",
              fluidRow(
                box(title = "Carte de richesse spécifique",
                    width = 6,
                    status = "warning",
                    solidHeader = T,
                    withSpinner(plotOutput("carte_richesse_spe")),
                    downloadButton("downloadMap1", "Download Map")
                ),
                
                box(title = "Carte des espèces menacées",
                    width = 6,
                    status = "warning",
                    solidHeader = T,
                    withSpinner(plotOutput("carte_espèces_men")),
                    downloadButton("downloadMap2", "Download Map")
                ),
                
                box(title = "Téléchargement des données cartographiques",
                    width = 12,
                    status = "warning",
                    solidHeader = T,
                    h5("Pour de potentielles applications dans un GIS, le bouton ci-dessous permet le téléchargement, au format .csv, des données GPS de chaque caméra, avec des métadonnées concernant le nombre d'individus pour chaque caméra et pour chaque espèce, la richesse spécifique par caméra, le nombre d'espèces menacées par caméra (colonne N_espece_EN), et le RAI des espèces menacées pour chaque caméra (colonne RAI_espece_EN)."),
                    downloadButton("downloadtabletot", "Télécharger les données cartographiques en .csv")
                )
                
                
              )#fermeture fluidrow
      )#fermeture tabitem carto
      
    )#fermeture tabitems
  )#fermeture dashboardbody
) #Fermeture ui


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
  

  
  infoCam <- reactive({
    req(input$infocam)
    infocamera <- read.csv(input$infocam$datapath,
                           header = TRUE,
                           sep = ";",
                           quote = '"',
                           colClasses = "character")
    
    
    
    
    infocamera
    
  })
  
  coordcam <- reactive({
    
    req(input$infocam)
    infocamera <- infoCam()
    
    infocamera$utm_x <- as.numeric(infocamera$utm_x)
    infocamera$utm_y <- as.numeric(infocamera$utm_y)
    infocamera1 <- na.omit(infocamera)
    infocamera1
  })
  
  CameraJour <- reactive({   ### ! Note + Verif colonnes + utiliser directement infoCam()################
    req(input$infocam)
    req(data())
    infocamera <- infoCam()
    
    
    test <- merge(infocamera,data(), by = "Camera")
    
    #  JourSite <- aggregate()
    test$Jours <- as.numeric(test$Jours)
    fin <- aggregate(Jours ~ Camera+Site, data = test, mean)
    fin <- aggregate(Jours ~ Site, data = fin, sum)
    fin
    
  })
  
  
  data <- reactive({
    
    req(input$file)
    req(err()[8] == 7)
    
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
    
    # Tri ici : 
    
    # On souhaite retirer des entrées qui sont des répétitions. Comme le fichier de base n'est pas
    # classé chronologiquement, il faut le faire manuellement. Pour celà, afin que les données
    # des différents sites/caméras ne se mélangent pas pendant le calcul probable de répétitions temporelles,
    # il est nécessaire de séparer le d.f en parties correspondant aux données respectives de chaque caméra.
    
    # On sépare en créant une largelist contenant des d.f au nom des facteurs.
    LargeList <- split(df,df$Camera)
    # Pour repérer les sites à trier chronologiquement dans un calcul en boucle plus loin,
    # on fait un facteur selon le champ Caméra, et on obtient le nombre de niveaux de ce facteur (36 pour les 36 caméras donc).
    
    ncam <- df$Camera
    facteurs <- factor(ncam)
    niveaux <- nlevels(facteurs)
    # Avant de lancer la boucle, on initie une nouvelle liste dans laquelle vont être stockés les d.f
    # de chaque niveaux/sites, mais cette fois-ci triés dans l'ordre chronologique
    Y <- list()
    
    # On lance la boucle pour chaque niveaux j du facteur "niveaux" (pour chaque caméra donc)
    for (j in 1:as.numeric(niveaux)) {
      # On crée un d.f avec 8 colonnes (Une en plus pour les données compilées "Date&Hour" qui viennent après) nommées arbitrairement (nommées automatiquement de X1 à X8)
      datf <- data.frame(1,2,3,4,5,6,7,8)
      # Ce d.f va permettre de stocker les lignes retenues et sera lui même stocké dans la liste Y au passage de la boucle au niveau suivant.
      # On extrait de la largelist le df qui concerne une caméra particulière
      dfSite <- as.data.frame(LargeList[[j]])
      # On crée la colonne YMDHMS qui regroupe "Date&Hour" afin d'effectuer un classement chronologique selon une seule colonne :
      dfSite$YMDHMS <- paste(dfSite$Date,dfSite$Hour,sep="_")
      # On reclasse les données de chaque df (pour chaque site donc) par ordre chronologique 
      # On commence d'abord par définir la nlle colonne comme un character pour le traitement lubridate
      dfSite$YMDHMS <- as.character(dfSite$YMDHMS)
      # Avec lubridate, on défini les valeurs de la nlle colonne comme des POSIXct (au format dmy_hms = DayMonthYear_HourMinSec) pour effectuer des opérations dessus. 
      dfSite$YMDHMS <- lubridate::dmy_hms(dfSite$YMDHMS)
      # On trie le fichier chronologiquement avec arrange() et on met tout dans un nouveau data.frame trié, on est prêt pour les calculs !
      dfSite_trie <- dplyr::arrange(dfSite, YMDHMS)
      
      # On prépare maintenant la boucle de tri des données qui vient après (selon des critères de différence interlignes entre les moments de prise en note des obs et entre les noms d'espèces)
      # On met les noms de colonne du df trié chronologiquement dans un vecteur character k
      k <- colnames(dfSite_trie)
      # On attribue les noms de colonnes stockés dans k au nouveau fichier datf : Remplacement des X1:X7 par les noms de col de df
      colnames(datf) <- k 
      # On crée une varbale l dans laquelle on met le nombre de lignes du df trié chronologiquement
      l <- nrow(dfSite_trie)
      # On crée 2 variables s et r dans lesquelles on met chaque fois la valeur 1 (on initie les valeurs qui entrent dans la boucle qui arrive)
      s <- 1
      r <- 1
      # On crée une boucle lisant chaque ligne et la comparant avec la suivante, si plus de 30 min écoulées, écrit la ligne,
      # si moins de 30 min écoulé, observe l'espèce, si elle sont égales, passe la ligne sans écrire la deuxième ligne
      
      # Pour un nombre i compris de 1 à l (donc de 1 à nligne (pour un site donné) après nettoyage)
      for (i in 1:l) {
        
        #Si i<l (donc tant qu'on à pas atteint la dernière ligne), on met dans s la valeur de i + 1 :
        if (i < l) {s <- i + 1}
        
        #On stoque également les heures indiquées dans la colonne YMDHMS dans les variables i et s, la variable s étant toujours devant la i ! 
        heurea <- dfSite_trie$YMDHMS[c(i)]
        heureb <- dfSite_trie$YMDHMS[c(s)]
        
        
        # Une fois au bon format, grâce à lubridate, on peut faire des calculs sur les heures. 
        # Ici, on calcule chaque fois l'écart de temps (secondes car format POSIXct) qui sépare deux lignes successives (pour rappel, classées chronologiquement à cet effet au préalable)  
        
        g <- lubridate::as.duration(heureb-heurea)
        
        # vérification du critère temps (si la durée entre 2 lignes dépasse 1800 sec = 30minutes, on écrit la donnée de la ligne i à la ligne r d'un nouveau data.frame : datf)
        # Cette ligne r correspond à la ligne i : Rappel; on initie r à 1 au début, en même temps que i.
        # Dans le cas ou plus de 30 minutes séparent les lignes, on écrit la ligne dans le nouveau data.frame.
        if (g > 1800)  { datf[c(r),] <- dfSite_trie[c(i),]
        # On passe alors à la ligne suivante après qu'il y ait eu écriture.
        r <- r+1}
        #vérification du critère espèce 
        # Sinon, si la durée entre les deux obs <30min, et que l'espèce à la ligne suivante correspond à celle à la ligne précédente, on n'écrit rien !
        else if (dfSite_trie$Species[c(i)] == dfSite_trie$Species[c(s)]) {}
        # Dans le dernier cas (donc l'espèce ligne suivante est différente de la précédente ET il y a moins de 30 min entre les obs)
        else {datf[c(r),] <- dfSite_trie[c(i),] # On écrit la ligne
        r <- r+1 # Puis on passe à la ligne r suivante !
        }
      } 
      # Enfin on réaffiche les df dans une largelist, pour compilation juste après
      Y[[j]] <- datf }
    # compilation des objets de la liste Y dans le d.f dfinal avec l'outil rbindlist (package data.table)
    dfinal <- rbindlist(Y)
    
    dfinal
    
  })
  
  ####################################################################################
  ## message d'erreur ==> verification des noms d'especes
  
  noms <- reactive({
    req(input$status)
    req(input$file)
    
    
    verif_noms <- merge(data(),IUCN(),all.x=T)
    verif_noms <- unique(verif_noms[which(is.na(verif_noms$IUCN)),"Species"])
    verif_noms
    #liste <- as.character(verif_noms[1])
    #for(i in 2:length(verif_noms)){
    #  liste <- paste(liste,verif_noms[i],sep=", ")
    #}
    #liste
  })
  
  
  observeEvent(EspecesRatee(), {
    showModal(modalDialog(
      title = "Vérification des noms d'espèces",
      paste("L'application ne reconnait pas les espèces suivantes :", sep =""),
      br(),
      br(),
      paste(noms()),
      br(),
      br(),
      paste("Cela est dû au format incorrect du nom de l'espèce dans votre jeu de données. 
      Veuillez vous référer aux noms scientifiques présents sur le site de l'IUCN, remplacez-les dans 
      votre jeu de données et rechargez vos fichiers."),
      br(),
      br(),
      paste("Si le problème persiste après avoir modifié les noms, 
      cela signifie que l'espèce n'est pas présente dans notre base de données. "),
      br(),
      br(),
      paste("Vous pouvez alors l'ajouter vous même dans le fichier statuts.csv et relancer l'application. 
      Les analyses fournies restent valables, mais les espèces restantes dans cette liste ne pourront pas 
      être prises en compte dans le recensement et la répartition des espèces menacées.", sep=""),
      footer = modalButton("Fermer")
    ))
  })
  ####################################################################################
  
  
  #Test Message d'erreur de fichier non conforme
  ################
  observeEvent(probleme(), {
    showModal(modalDialog(
      title = "fichier non conforme",
      paste("Le fichier chargé ne correspond pas au format requis. veuillez charger une table de données conforme pour obtenir vos résultats. ", sep=""),
      br(),
      paste(err()[1]),
      br(),
      paste(err()[2]),
      br(),
      paste(err()[3]),
      br(),
      paste(err()[4]),
      br(),
      paste(err()[5]),
      br(),
      paste(err()[6]),
      br(),
      paste(err()[7]),
      br(),
      footer = modalButton("Fermer")
    ))
  })
  
  observeEvent(ProblemeCam(), {
    showModal(modalDialog(
      title = "fichier d'info caméra non conforme",
      paste("Le fichier chargé ne correspond pas au format requis. veuillez charger une table de données conforme pour obtenir vos résultats. ", sep=""),
      br(),
      br(),
      paste(errcam()[1]),
      br(),
      paste(errcam()[2]),
      br(),
      paste(errcam()[3]),
      br(),
      paste(errcam()[4]),
      
      footer = modalButton("Fermer")
    ))
  })
  
  #Message d'information du tableau par communauté
  
 observeEvent(input$info, {
    showModal(modalDialog(
      title = "Information du tableau récapitulatif de la communauté",
      paste("Ce tableau reprend des informations générales sur votre campagne d'inventaire.", sep =""),
      br(),
      br(),
      paste("Celles-ci sont reprises pour chaque site étudié", sep =""),
      br(),
      br(),
      paste("Les informations regroupées dans les différentes colonnes du tableau sont les suivantes : ", sep =""),
      br(),
      br(),
      paste("Le Site tel que repris dans la table de donnée chargée", sep =""),
      br(),
      br(),
      paste("Le nombre de caméras déployées pour le site concerné", sep =""),
      br(),
      br(),
      paste("L'effort d'inventaire repris en caméra-jours. Cette information est obtenue en additionnant le nombre de jours de déployement respectif de chaque caméra installée sur le site concerné", sep =""),
      br(),
      br(),
      paste("La richesse spécifique ou richesse en espèce. Il s'agit du nombre d'espèces différentes identifiées pour le site concerné", sep = ""),
      br(),
      br(),
      paste("Le nombre d'espèces menacées. La determination des espèces menacées se fait en se basant sur le fichier des statuts fournis sur le site FauneFAC. Celui-ci se basant initialement sur la liste rouge de L'UICN.  ", sep = ""),
      br(),
      paste("Les espèces reprises dans ce calcul sont celles considérées comme 'EN' (en danger) ou 'CR' (en danger critique)", sep = ""),
      footer = modalButton("Fermer")
      
    ))
    
  })
  
 
 observeEvent(input$infoTableEsp, {
   showModal(modalDialog(
     title = "Information du tableau récapitulatif par espèces",
     paste("Ce tableau reprend par colonne des informations concernant chaque espèce prise individuelement. Les informations reprises sont les suivantes :", sep= ""),
     br(),
     br(),
     paste("Le nom des ou de l'espèce(s) tel que demandé dans la boite 'Sélection de l'espèce'", sep= ""),
     br(),
     br(),
     paste("Le Site pour lequel les observations et calculs sont faits. Ces sites sont sélectionnés dans la boite 'Sélection du site'", sep= ""),
     br(),
     paste("Si plusieurs site sont sélectionnés, certaines espèces partagée entre eux se retrouveront plusieurs fois dans ce tableau avec les données obtenues pour chacun des sites respectifs", sep= ""),
     br(),
     br(),
     paste("Le nombre de détections. Ce chiffre reprends, pour le site concerné le nombre d'évènement indépendant pour lequel l'espèce apparait."),
     br(),
     paste("Par exemple un groupe de potamochères aperçut sur plusieurs vidéos proche l'une de l'autre dans le temps correspondra à une détection", sep= ""),
     br(),
     br(),
     paste("Le taux de détection ou RAI. Ce taux est calculé en reprenant le nombre de détection précédement définis pour le site concerné divisé par le nombre de caméra jours pour ce même site."),
     br(),
     paste("Le nombre de caméra jours est précédement définis dans le tableau 'Communauté' et correspond à la somme du temps de déployement en jour de chaque piège photographique placé sur le site.", sep= ""),
     br(),
     br(),
     paste("Le nombre moyen d'individus par détection. Cette donnée est obtenue en reprenant chaque détection indépendante de l'espèce définie pour le site défini et en effectuant la moyenne du nombre d'individus détectés.", sep= ""),
     br(),
     br(),
     paste("Le Statut UICN tel que repris dans le fichier téléchargeable sur le site internet FauneFAC", sep= ""),
     br(),
     paste("Si votre escpèce ne se retrouve pas dans la liste fournie, le statut suivant sera indiqué 'NA'. ", sep= ""),
     br(),
     paste("Si l'espèce est contenue dans le fichier mais qu'elle n'est pas reprise dans la liste de l'UICN, le statut suivant sera indiqué 'NA*'", sep= ""),
     footer = modalButton("Fermer")
     
   ))
   
 })
 
 
 observeEvent(input$infoRythmeActiv, {
   showModal(modalDialog(
     title = "Information Rythme d'activité",
     paste("Le Graphique du rythme d'activité reprends par heure la somme des individus observés pour le site et l'espèce concernée ", sep =""),
     br(),
     paste("Ce graphique permet de renseigner les heures d'activité prédominante chez l'espèce observée dans le site demandé", sep =""),
     footer = modalButton("Fermer")
     
   ))
   
 })
 
 
  # Traitement des données de la partie communauté --------------------------------------
  # table des informations sur les communautés par site 
  tableCom <- reactive({
    effort <- CameraJour()
    ncam <- aggregate(Individuals ~ Site+Camera, data = data(), sum)
    ncam <- aggregate(Individuals ~ Site, data = ncam, length)
    tab1 <- merge(ncam,effort,by=c("Site","Site"),all=T)
    rich <- aggregate(Individuals ~ Site+Species, data = data(), sum)
    rich <- aggregate(Individuals ~ Site, data = rich, length)
    tab1 <- merge(tab1,rich,by=c("Site","Site"),all=T)
    datEN <- merge(data(),IUCN(),by="Species",all.x=T)
    datEN <- subset(datEN,datEN$IUCN=="EN"|datEN$IUCN=="CR")
    if(nrow(datEN)>=1){
      EN <- aggregate(Individuals ~ Site+Species, data = datEN, sum)
      EN <- aggregate(Individuals ~ Site, data = EN, length)
      tab1 <- merge(tab1,EN,by=c("Site","Site"),all=T)
    } else {
      tab1$EN <- 0
    }
    names(tab1) <- c("Site","Nombre de caméras","Effort d'inventaire",
                     "Richesse spécifique","Nombre d'espèces menacées")
    if(nrow(rich)>1){
      n <- length(tab1$Site)
      tab1[n+1,1] <- "TOTAL"
      tab1[n+1,2] <- sum(tab1[-(n+1),2])
      tab1[n+1,3] <- sum(tab1[-(n+1),3])
      richTot <- aggregate(Individuals ~ Species, data = data(), length)
      tab1[n+1,4] <- nrow(richTot)
      ENTot <- aggregate(Individuals ~ Species, data = datEN, length)
      tab1[n+1,5] <- nrow(ENTot)
    }
    tab1
  })
  
  output$richesse <- renderTable({
    req(input$file)
    
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
  observe({
    updateSelectizeInput(
      session,
      inputId = "selectSite",
      choices = c("Tous sites confondus", "Une courbe par site"),
      selected = "Une courbe par site"
    )  
  })
  
  
  ##PLUSIEURS SITES PAR GRAPHIQUE
  accumul <- function (){
    if(input$selectSite == "Tous sites confondus"){
      matriceTot <- aggregate(Individuals ~ Date+Species,data=data(),sum)
      matriceTot$Date <- dmy(matriceTot$Date)
      matriceTot <- matriceTot[order(matriceTot$Date),]
      matriceTot <- dcast(matriceTot,Date~Species,fill=0)
      rownames(matriceTot) <- matriceTot[,1]
      matriceTot <- matriceTot[,-1]
      
      accumTot <- specaccum(matriceTot)
      plot(accumTot,xlab = "Nb de jours d'inventaire cumulés",ylab = "Nombre d'espèces",ci=0)
      title(main="Courbe d'accumulation") }
    else {
      matriceSite <- aggregate(Individuals ~ Date+Site+Species,data=data(),sum)
      matriceSite$Date <- dmy(matriceSite$Date)
      matriceSite <- matriceSite[order(matriceSite$Date),]
      
      sites <- aggregate(Individuals ~ Site, data = data(), sum)
      sites <- sites$Site
      nSites <- length(sites)
      
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
             ci=0,add=T,col=i+1)} #ajouter legende : couleurs selon les sites
    }
  }
  
  # Encodage du graphique réactif en output de manière à l'afficher
  output$accumul <- renderPlot({
    req(input$file)
    
    accumul()
  })
  
  # gérer le téléchargement du graphique d'accumulation 
  output$downloadAccumul <- downloadHandler(
    # filename pour définir le nom par défaut du fichier produit, Content pour choisir le graph dans l'image
    filename = function() {paste('accumul', '.png', sep='') }, #ou //input$selectSp si choix avec tot
    content = function(file) {
      
      png(file)
      accumul()
      dev.off()
    }
    
    
    #filename = "Modified_image.jpeg",
    #contentType = "image/jpeg",
    #content = function(file) {
    ## copy the file from the updated image location to the final download location
    # file.copy(updatedImageLoc(), file)
    #  content = function(file) {
    # ggsave(file, plot = plotInput(), device = "png")
  )
  
  
  # indice d'occurence nocturne
  output$indnoc <- renderTable({
    req(input$file)
    
    
    
    sites <- aggregate(Individuals ~ Site, data = data(), sum)
    sites <- sites$Site
    nSites <- length(sites)
    Def <- data.frame(Sites = sites, indice =1)
    ligne <- nrow(data())
    
    for(j in 1:nSites) {
      z <- 0
      LeSite <- sites[j]
      
      b <- 0
      #comptage des détection nocturne en observant chaque lingne de Data
      
      for (i in 1:ligne) {
        if (data()$Site[i]== LeSite) {
          heure <- factor(data()$Hour[c(i)])
          a <- lubridate::hms(as.character(heure))
          c <- lubridate::hour(a)
          if (c[c(1)] > 18) { }
          else  if (c[c(1)] < 6) {b <- b + data()$Individuals[c(i)]}
          z <- z +1
          
        }
      }
      # nombre de détections nocturne sur nombre de détections totales
      
      d <- (b / z) *100
      Def$indice[j] <- d
    }
    
    Def
  })
  
  # Indice de présence humaine
  output$homme <- renderTable({
    req(input$file)
    req(err()[8] == 7)
    
    
    sites <- aggregate(Individuals ~ Site, data = data(), sum)
    sites <- sites$Site
    nSites <- length(sites)
    Def <- data.frame(Sites = sites, indice =1)
    ligne <- nrow(data())
    
    
    
    for(j in 1:nSites) {
      z <- 0
      LeSite <- sites[j]
      
      b <- 0
      
      
      # compte le nombre de lignes correspondante à Homo sapiens
      for (i in 1:ligne) {
        if (data()$Site[i]== LeSite) {
          if (data()$Species[c(i)] == "Homo sapiens")
          {b <- b + data()$Individuals[c(i)]
          
          }
        }
      }
      
      
      # différence entre le premier et le dernier jour d'inventaire en jours
      
      
      nbjours <- subset(CameraJour(),CameraJour()$Site == LeSite)
      nbjourssite <- nbjours[1,2] 
      
      # rapport du nombre de "Homo sapiens" sur le nombre de jours d'inventaires remis au mois (30jours) 
      d <- (b /nbjourssite)*30
      Def$indice[j] <- d
    }
    Def
    
  })
  
  # message de chargement de table de données
  output$fichier1 <- renderText({
    req(err()[8] == 7)
    if (is.data.frame(input$file) == TRUE ) {texte1 <- "Table de donnée chargée"}
    else texte1 <- {"Veuillez charger la table de données"}
    
    texte1
    
  })
  
  # message de chargement des informations caméras
  output$fichier2 <- renderText({
    if (is.data.frame(input$infocam) == TRUE ) {texte1 <- "Informations Caméras chargée"}
    else texte2 <- {"Veuillez charger les Informations Caméras"}
    
    
    
  })
  
  
  observe({
    updateSelectizeInput(
      session,
      inputId = "selectSp_tab",
      choices = c("All", data()$Species),
      selected = "All"
    )  
  })
  
  observe({
    updateSelectizeInput(
      session,
      inputId = "selectLoc_tab",
      choices = c("All", data()$Site),
      selected = "All"
    )  
  })
  
  # Traitement des données des espèces : ----------------------------------------------
  # table des informations par espèces , abondance relative, nombre d'individus détecté
  # faudrait-il ajouer détection par mois ? 
  tableEsp <- reactive({
    req(input$selectSp_tab, input$selectLoc_tab)
    
    #Tentative de caser le ALL
    if(input$selectSp_tab == "All")
      selesp <- as.data.frame(data()$Species)
    else 
      selesp <- as.data.frame(input$selectSp_tab, 
                              row.names = NULL)
    colnames(selesp) <- "Species"
    #Loc
    if(input$selectLoc_tab == "All")
      selloc <- as.data.frame(data()$Site)
    else
      selloc <- as.data.frame(input$selectLoc_tab,
                              row.names = NULL)
    colnames(selloc) <- "Site"
    #bondocde
    if(input$selectLoc_tab == "All")
      {
      nb <- aggregate(Individuals ~ Species, data = data(), length)
      data_alt <- data()
      data_alt <- subset(data_alt, select = -Site)
      data_alt$Site <- rep("All", length(data_alt$Species))
      nj <- data.frame(nb$Species,rep(sum(CameraJour()$Jours),length(nb$Species)))
      names(nj) <- c("Species","Jours")
      table <- merge(nb,nj,by=c("Species"))
      nmoy <- aggregate(Individuals ~ Species+Site, data = data_alt, mean)
      names(nmoy) <- c("Species","Site","nmoy")
      table <- merge(table,nmoy,by="Species")
      }
    else {
      nb <- aggregate(Individuals ~ Species+Site, data = data(), sum)
      jours <- CameraJour()
      nj <- merge(nb,jours,by="Site")[,c("Species","Site","Jours")]
      table <- merge(nb,nj,by=c("Species","Site"))
      nmoy <- aggregate(Individuals ~ Species+Site, data = data(), mean)
      names(nmoy) <- c("Species","Site","nmoy")
      table <- merge(table,nmoy,by=c("Species","Site"))
    }
    if(input$selectLoc_tab != "All")
      table <- merge(table, selloc, by = "Site")
    if(input$selectSp_tab != "All")
      table <- merge(table, selesp, by = "Species")
    table$Jours <- table$Individuals/table$Jours
    table <- table[,c("Species","Site","Individuals","Jours","nmoy")]
    table <- merge(table,IUCN(),by="Species",all.x=T)
    names(table) <- c("Espèce","Site","Nombre de détections","Taux de détection (RAI)",
                      "Nombre moyen d'individus par détection","Statut IUCN")
    table  })
  
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
  
  
  
  # Création d'une cartographie des abondances et richesses spé + IUCN et présence d'espèce menacées ------------------------------
  
  # Traitement et préparation des données utiles aux cartes d'abondance et de richesse 
  
  observe({
    updateSelectizeInput(
      session,
      inputId = "selectSp_carto",
      choices = c("All", data()$Species),
      selected = "All"
    )  
  })
  
  EPSG <- reactive ({
    as.numeric(input$epsg)
  })
  
  donnees_cartes_richesse_spe <- reactive({ 
    
    req(data())
    dfinal <- data()
    # Création du jeu de données aggrégées sans coordonnées : 
    # On aggrège d'abord les données des individus par espèce et par caméra !
    n_indiv_cam_esp <- aggregate(Individuals ~ Species+Camera, data = dfinal, sum)
    # Ensuite, dans une colonne individuals, on calcule le nombre d'espèce par caméra ! 
    n_esp_cam <- aggregate(Individuals ~ Camera, data = n_indiv_cam_esp, length)
    # On renomme chaque fois les champs issus de double aggregate par length, par un nom qui correspond mieux aux données obtenues
    n_esp_cam <- rename(n_esp_cam,"Nbesp"="Individuals" )
    
    # On cherche à inclure le statut IUCN 
    # On lie la table initiale à la table IUCN par l'espèce (ainsi on à le statut pour chaque espèce présente dans la liste)
    req(IUCN())
    UICN <- IUCN()
    datEN <- merge(dfinal,UICN,by="Species",all.x=T)
    # On ne garde dans un nouveau d.f que les individus qui ont un statut IUCN EN ou CR.
    datEN <- dplyr::filter(datEN, IUCN=="EN" | IUCN=="CR")
    # On aggrège alors les nb d'indiv par espèce et par caméra.
    EN <- aggregate(Individuals ~ Species + Camera, data = datEN, sum)
    # On aggrège le nombre d'entrées par caméra pour obtenir le nb d'esp en danger (CR ou EN) par caméra.
    ENCAM <- aggregate (Individuals ~ Camera, data = EN, length)
    ENCAM <- rename(ENCAM,"NespEN"="Individuals" ) 
    # On aggrège aussi le nb d'individus en danger obvervés par caméra (effectif en danger)
    ENIND <- aggregate (Individuals ~ Camera, data= EN, sum)
    ENIND <- rename(ENIND,"EffespEN"="Individuals")
    
    # On merge tout dans une table qui contient, pour chaque caméra, le nb d'espèce, 
    # le nb d'espèce en danger (CR ou EN) et le nb d'individus observés de ces espèces en danger : 
    Camdon <- merge(n_esp_cam,ENCAM,by=c("Camera","Camera"),all=T)
    Camdon <- merge(Camdon,ENIND,by=c("Camera","Camera"),all=T)
    
    # On vire les N.A (créés quand aucune espèce EN ou CR n'est présente
    # pour une caméra particulière) et on les remplace par 0 :
    Camdon[is.na(Camdon)]<-0
    
    req(coordcam())
    infos_cam <- coordcam()
    
    infos_cam$Camera <- as.character(infos_cam$Camera)
    infos_cam1 = dplyr::select(infos_cam,utm_x:utm_y,Camera)
    
    
    infos_cam1$Camera=as.character(infos_cam1$Camera)
    Camdon$Camera=as.character(Camdon$Camera) # On transforme les valeurs des champs Camera en character afin de pouvoir effectuer la jointure
    
    Camdon2 = left_join(Camdon,infos_cam1, by = c("Camera" = "Camera")) # C'est ainsi que l'on effectue la jointure,
    # La fonction left_join permet de réaliser une jointure gauche, on va associer à chaque Camera de la variable Camdon les données des caméras d'infos_cam. Si un champ de la variable 
    # infos_cam ne contenait pas de valeur pour une ou certaines des camera de Camdon, 
    # on aurai des NO_DATA sur Camdon2 pour les lignes manquantes. De plus, s'il y avait des 
    # données manquante de caméra sur Camdon1 (ex : pas de détection pour l'une des caméras présentes
    # dans le jeu de données info_cam), la ligne ne serai pas reprise dans Camdon2 et on aurai aucune valeur/NO.DATA supplémentaire !
    
    # Il est également possible d'effectuer des jointures droites, ou des jointures internes
    # Petite précision, on ne peut pas joindre deux couches sf comme celà ! -> pour une jointure gauche 
    # par exemple, le premier élément peut être une couche sf, mais le 2nd doit obligatoirement être un d.f !
    
    # Conversion du .csv en objet sf ! (par défaut SCR = 4326/WGS84 pour ces coordonnées UTM) --------------------
    
    req(EPSG())
    epsg <- EPSG()
    
    Camdon2=st_as_sf(Camdon2,coords=c("utm_y","utm_x"),crs=epsg)
    
    
    Camdon2
    
  })
  
  
  
  # Test carto richesse spé :
  
  carte_richesse_spe1 <- reactive ({ 
    # Calcul et affectation des données de l'emprise de la carte :
    req(EPSG())
    epsg <- EPSG()
    req(donnees_cartes_richesse_spe())
    richespe <- donnees_cartes_richesse_spe()
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
    coefx <- as.numeric(diffx/6)
    coefy <- as.numeric(diffy/6)
    coefx
    coefy
    
    
    carte1 <- ggplot() +
      geom_sf(mapping=aes(size=Nbesp, color=Nbesp) ,data=richespe) +
      coord_sf(crs = st_crs(epsg),xlim=c(xmin-coefx,xmax+coefx),ylim=c(ymin-coefy,ymax+coefy), datum = sf::st_crs(4326), expand = FALSE) +
      scale_size_continuous(name = "Richesse spécifique", range=c(1,6)) +
      scale_colour_gradientn(name = "Richesse spécifique", colours = terrain.colors(5)) + 
      guides(size=FALSE) +
      labs(title = "Cartographie de la richesse spécifique",
           subtitle = "Toutes caméras confondues",
           caption = "La taille des points est également proportionnelle à la richesse spécifique",
           x = "utm_x", y = "utm_y") +
      theme_dark() +
      theme(
        legend.position = c(1.11, 0.5),
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
                             pad_x = unit(0.2, "cm"), pad_y = unit(0.6, "cm"),
                             style = north_arrow_fancy_orienteering)
    
    carte1
  })
  
  
  carte_espèces_men1 <- reactive ({ 
    # Calcul et affectation des données de l'emprise de la carte :
    req(EPSG())
    epsg <-EPSG()
    req(donnees_cartes_richesse_spe())
    richespe <- donnees_cartes_richesse_spe()
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
    coefx <- as.numeric(diffx/6)
    coefy <- as.numeric(diffy/6)
    coefx
    coefy
    
    
    carte2 <- ggplot() +
      geom_sf(mapping=aes(size=EffespEN, color=NespEN) ,data=richespe) +
      coord_sf(crs = st_crs(epsg),xlim=c(xmin-coefx,xmax+coefx),ylim=c(ymin-coefy,ymax+coefy), datum = sf::st_crs(4326), expand = FALSE) +
      scale_size_continuous(name = "Nb d'individus menacés observés", range=c(1,6)) +
      scale_colour_gradientn(name = "Nb d'espèces menacées", colours = terrain.colors(5)) + 
      guides(size=FALSE) +
      labs(title = "Cartographie des observations d'espèces au statut IUCN 'EN' et/ou 'CR'",
           subtitle = "Toutes caméras confondues",
           caption = "La taille des points est proportionnelle à l'effectif des espèces menacées observées pour ces caméras",
           x = "utm_x", y = "utm_y") +
      theme_dark() +
      theme(
        legend.position = c(1.11, 0.5),
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
                             pad_x = unit(0.2, "cm"), pad_y = unit(0.6, "cm"),
                             style = north_arrow_fancy_orienteering)
    
    carte2
  })
  
  # Gestion des affichages des cartes : 
  
  
  output$carte_richesse_spe <- renderPlot({req(carte_richesse_spe1())
    carte_richesse_spe1()})
  
  output$carte_espèces_men <- renderPlot({req(carte_espèces_men1()) 
    carte_espèces_men1()})
  
  # Gestion des téléchargements des cartes : 
  
  # Carte des richesses spécifiques
  
  output$downloadMap1 <- downloadHandler( 
    # filename pour définir le nom par défaut du fichier produit, Content pour choisir le graph dans l'image
    filename = function() {paste("Map_richesspe", '.png', sep='') },
    content = function(file) {
      
      ggsave(file,plot=carte_richesse_spe1(),width=18,height = 6)
      
    }
    
  )
  
  # Carte des espèces menacées :
  
  
  output$downloadMap2 <- downloadHandler(
    # filename pour définir le nom par défaut du fichier produit, Content pour choisir le graph dans l'image
    filename = function() {paste("Map_IUCN",'.png', sep='') },
    content = function(file) {
      
      ggsave(file,plot=carte_espèces_men1(),width=18,height = 6)
      
    }
    
  )
  
  
  # Création des données et export csv selon les envies de Davy : ----------------------------
  
  # On crée d'abord la matrice espèce/caméra/nb d'indiv pour ces champs :
  mat <- reactive({
  # On va chercher le fichier de données
  req(data())
  dfinal <- data()
  # On crée à présent la matrice nb d'indiv par caméra et par espèce : 
  # Pour celà, on s'assure d'abord bien que les champs Camera et Species sont des character : 
  dfinal$Camera <- as.character(dfinal$Camera)
  dfinal$Species <- as.character(dfinal$Species)
  # Ensuite, on veut faire de ces champs des facteurs pour construire la matrice. 
  #Pour celà, comme on veut que le facteur ait le même nombre de valeurs que de niveaux, 
  # on aggrège les donnée des espèces et des caméras séparément et on les transforme en facteurs
  # avec autant de niveaux que de valeurs :
  cam <- aggregate(Individuals ~ Camera, data=dfinal, sum)    
  camvec <- factor(cam$Camera)
  spec <- aggregate(Individuals ~ Species, data=dfinal, sum)
  speciesvec <- factor(spec$Species)
  # On extrait de ces facteurs/vecteurs le nombre de niveaux pour réaliser les boucles de 
  # remplissage de la matrice :
  nivcamvec <- as.numeric(nlevels(camvec))
  nivspeciesvec <- as.numeric(nlevels(speciesvec))
  
  # On intitialise alors les boucles en créant une matrice X avec autant de colonnes que de 
  # niveaux pour le facteur espèce et autant de lignes que de niveaux pour le facteur caméra.
  # Enfin, on initialise également les valeurs i et j qui permettent de faire tourner la boucle : 
  
  # X à pour noms de colonnes les noms d'espèces et pour noms de lignes les noms de caméras.
  X = matrix(data=0, nrow=nivcamvec, ncol=nivspeciesvec)
  colnames(X) <- as.character(levels(speciesvec))
  rownames(X) <- as.character(levels(camvec))
  j=1
  i=1
  
  # La boucle de remplissage est comme ci-après, pour chaque colone (= chaque espèce = chaque niveaux du facteur espèce)
  # on va réaliser une écriture du nombre d'individus de telle caméra, et passer à la caméra 
  # suivante jusqu'à finir la colonne, et passer à la colonne suivante, etc...
  # Les écritures se font par filtration des entrées qui concernent une espèce et une caméra 
  # en particulier. S'il n'y à aucune entrée correspondante, on attribue à la case matricielle 
  # un numérique = 0. Sinon, on attribue à cette case le nombre d'individus sommés de chaque entrées.
  for (i in 1:nivspeciesvec) {
    for (j in 1:nivcamvec) {
      sub <- subset(dfinal, dfinal$Species==speciesvec[i] & dfinal$Camera==camvec[j])
      if (is.null(sum(sub$Individuals))) { X[j,i]=0}
      else {X[j,i]=sum(sub$Individuals)}
    }
  }
  
  X <- as.data.frame(X)
  setDT(X, keep.rownames = TRUE)[]
  X <- rename(X, "Camera"="rn")
  
  X
})
  
# Ensuite on crée un d.f richesse spécifique par caméra, selon une méthode analogue : 
  richspecam <- reactive({
    req(data())
    dfinal <- data()
    dfinal$Camera <- as.character(dfinal$Camera)
    cam <- aggregate(Individuals ~ Camera, data=dfinal, sum)    
    camvec <- factor(cam$Camera)
    nivcamvec <- as.numeric(nlevels(camvec))
    
    Richmat = matrix(data=0, nrow=nivcamvec, ncol=1)
    rownames(Richmat) <- as.character(levels(camvec))
    colnames(Richmat) <- "Richesse"
    for (i in 1:nivcamvec) {
      tri <- dplyr::select(dfinal,Camera,Species)
      sub1 <- subset(tri, tri$Camera==camvec[i])
      sub1 <- factor(sub1$Species)
      Richmat[i,1] <- nlevels(sub1)
    }
    
    Rich <- as.data.frame(Richmat)
    setDT(Rich, keep.rownames = TRUE)[]
    Rich <- rename(Rich, "Camera"="rn")
    
    Rich
  })
  
  # On calcule à présent les données concernant les sp en danger EN et CR (RAI et Nb d'espèces pour chaque caméra)
  
  RAI_nb_EN <- reactive ({
  # On va d'abord chercher les données : 
  req(data())
  dfinal <- data()
  req(IUCN())
  IUCN <- IUCN()
  # On joint les données IUCN au data par les noms d'espèces :
  datEN <- merge(dfinal,IUCN,by="Species",all.x=T)
  # On ne garde dans un nouveau d.f que les individus qui ont un statut IUCN EN ou CR.
  datEN <- subset(datEN,datEN$IUCN=="EN"|datEN$IUCN=="CR")
  # On aggrège le nombre d'entrées par caméra pour obtenir le nb d'esp en danger (CR ou EN)
  # par caméra (avec les 2 aggregate ci_dessous)
  EN <- aggregate(Individuals ~ Species + Camera, data = datEN, sum)
  ENCAM <- aggregate (Individuals ~ Camera, data = EN, length)
  # On renome la colonne individuals de ENCAM qui n'indique désormais plus que le nombre 
  # d'espèce en danger CR ou EN
  ENCAM <- rename(ENCAM,"N_espece_EN" = "Individuals")
  # On aggrège datEN (qui contient les espèces en danger) selon 
  # le nombre d'entrées par caméra pour obtenir le nb de détection
  nbdetection <- aggregate(Individuals~Camera,data=datEN, length)
  # On renomme à nouveau Individuals qui ne contient plus le nb d'individus, mais le nb de détections par cam
  nbdetection <- rename(nbdetection,"NbdetEN"="Individuals")
  # On colle le nb d'espèces en danger et le nb de détection d'espèces en danger par Caméra
  donEN <- merge(nbdetection,ENCAM,by="Camera",all=T)
  # On va chercher les infos des caméras pour jointure des coordonnées et calcul du RAI (durée de mise en fonction des caméra contenu dans coordcam)
  req(coordcam())
  infos_cam <- coordcam()
  # On sélectionne les colonnes d'intérêt dans un nouveau df info_cam1
  infos_cam1 <- dplyr::select(infos_cam,Camera,utm_x,utm_y,Jours)
  infos_cam1$Jours <- as.numeric(infos_cam1$Jours)
  # On s'assure que toutes les caméras sont bien écrites en character
  infos_cam1$Camera=as.character(infos_cam1$Camera)
  # On joint les données du fichier contenant les infos (Jours, utm_x et utm_y) aux autres infos (nb de dét et nb esp EN) par caméra
  donEN <- merge(donEN,infos_cam1, by="Camera",all=T )
  # On vire les NA pour les caméra qui n'ont aucune espèce EN ou CR et on les remplace par des 0
  donEN[is.na(donEN)]<-0
  # On crée une nouvelle colonne RAI pour les espèce EN et CR
  donEN$RAI <- (donEN$NbdetEN/donEN$Jours)
  
  donEN
})
  
  # Jointure des 3 tableaux de données pour obtenir le d.f final : 
  tabexpmap <- reactive ({
    # On va chercher le dernier tableau : 
    req(RAI_nb_EN())
    donEN <-  RAI_nb_EN()
    # On sélectionne et renomme les colonne du dernier tableau pour la mise en forme finale :
  donENfin <- dplyr::select(donEN,Camera,N_espece_EN,RAI_espece_EN = RAI, utm_x, utm_y)
  # On à plus qu'à coller le d.f X au vecteur richesse, et joindre par après la matrice des sp EN et CR :
  # On va chercher les deux tables : 
  req(richspecam())
  Richvec <- richspecam()
  req(mat())
  X <- mat()
  # Dans un nouveau d.f, on lie la matrice 1 à la richesse spé par cam
  Ok1 <- merge(X,Richvec,by="Camera",all.x=T)
  Ok2 <- merge(Ok1,donENfin,by="Camera",all.x=T)
  Ok2 <- rename(Ok2, "CT"="Camera")
  
  Ok2
  })
 
# Gestion du téléchargement du tableau GPS .csv demandé par davy : 
  output$downloadtabletot <- downloadHandler(
    filename = "matrice_indiv_espece_camera_Rich&RAI_EN.csv",
    content = function(file) {
      write.table(tabexpmap(), file,quote = TRUE, sep = ";",dec=".",row.names = FALSE, col.names = TRUE)
    } 
    
  )
  
  #Sélection esp et site pour le graphe d'acti en 24h
  observe({
    updateSelectizeInput(
      session,
      inputId = "selectSp_graph",
      choices = c("All", data()$Species),
      selected = "All"
    )  
  })
  
  observe({
    updateSelectizeInput(
      session,
      inputId = "selectLoc_graph",
      choices = c("All", data()$Site),
      selected = "All"
    )  
  })
  
  # Création du graphique d'activité en 24h en réactive de façon à pouvoir le télécharger -----------------
  
  graph24 <- function (){
    # récupérer l'espèce encodée 
    df <- data()
    
    k <- as.character(input$selectLoc_graph)
    
    x <- as.character(input$selectSp_graph)
    
    # récupérer le dataframe nettoyé
    #df <- data()
    # si "All" est encodé, graphique de toute les epsèces, si le nome d'une espèe est encodé, le prend en compte
    if(input$selectLoc_graph != "All")
     df <- df[df$Site == k,]
    
    if (input$selectSp_graph != "All")
      df <- df[df$Species == x,]
    
    
    # récupérer les heures concernées de l'espèce choisie
    
    heurea <- df$Hour
    a <- lubridate ::hms(as.character(heurea))
    
    hour_of_event <- lubridate::hour(a)
    
    eventdata <- data.frame(datetime = df$Date, eventhour = hour_of_event)
    
    # désignation de la période nocturne (entre 6h et 18h)
    eventdata$Day <- eventdata$eventhour %in% seq(6, 18)
    
    # création du graphique, mise en couleur du "jour", nombre de 0 à 24h
    frete<- ggplot(eventdata, aes(x = eventhour, fill = Day)) + geom_histogram(breaks = seq(0, 
                                                                                            24)) + coord_polar(start = 0) + theme_minimal() + 
      scale_fill_brewer() + ylab("Nombre de détection") + ggtitle(paste(input$selectSp_graph,"Répartition des détection par heure", sep=" ")) + 
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 
                                                                                  24))
    
    frete
  }
  
  # Encodage du graphique réactif en output de manière à l'afficher
  output$graph24h <- renderPlot({
    req(input$file)
    
    graph24()
  })
  
  # réception de l'spèce choisie en réactif
  
  chosenSp <- reactive ({
    
    Input$selectSp_graph
  })
  
  # gérer le téléchargement du graphique circulaire
  output$downloadGraph <- downloadHandler(
    # filename pour définir le nom par défaut du fichier produit, Content pour choisir le graph dans l'image
    filename = function() {paste(input$selectSp_graph,"graph24", '.png', sep='') },
    content = function(file) {
      
      png(file)
      print(graph24())
      dev.off() 
    }
    
  )
  
  ######### 
  # Test de DL en .SVG
  output$downloadGraphSVG <- downloadHandler(
    # filename pour définir le nom par défaut du fichier produit, Content pour choisir le graph dans l'image
    filename = function() {paste(input$selectSp_graph,"graph24", '.svg', sep='') },
    content = function(file) {
      
    svg(file)
    print(graph24())
    dev.off()
    }
    
  )
  
  ## message d'erreur ==> verification des noms de colonnes
  
  err <- reactive({
    req(input$file)
    
    df<- read.csv(input$file$datapath,
                  header = TRUE,
                  sep = ";",
                  quote = '"',
                  colClasses = "character")
    
    SpOk <- 0
    CamOk <- 0
    SiOk <- 0
    InOk <- 0
    DaOk <- 0
    HoOk <- 0
    ImOk <- 0
    AllOk <- 0
    x <- c(1,2,3,4,5,6,7)
    
    x <- names(df)
    
    
    if (x[1] == "Species") {SpOk <- 1 }
    else if (x[2] == "Species") {SpOk <-1 }
    else if (x[3] == "Species") {SpOk <-1 }
    else if (x[4] == "Species") {SpOk <-1 }
    else if (x[5] == "Species") {SpOk <-1 }
    else if (x[6] == "Species") {SpOk <-1 }
    else if (x[7] == "Species") {SpOk <-1 }
    
    if (SpOk == 1) {SOk <- ""}
    else { SOk <- "Erreur, impossible de trouver la colonne 'Species'. vérifiez la syntaxe du jeu de donnée"}
    
    if (x[1] == "Camera") {CamOk <- 1 }
    else if (x[2] == "Camera") {CamOk <-1 }
    else if (x[3] == "Camera") {CamOk <-1 }
    else if (x[4] == "Camera") {CamOk <-1 }
    else if (x[5] == "Camera") {CamOk <-1 }
    else if (x[6] == "Camera") {CamOk <-1 }
    else if (x[7] == "Camera") {CamOk <-1 }
    
    if (CamOk == 1) {COk <- ""}
    else { COk <- "Erreur, impossible de trouver la colonne 'Camera'. vérifiez la syntaxe du jeu de donnée"}
    
    if (x[1] == "Site") {SiOk <- 1 }
    else if (x[2] == "Site") {SiOk <-1 }
    else if (x[3] == "Site") {SiOk <-1 }
    else if (x[4] == "Site") {SiOk <-1 }
    else if (x[5] == "Site") {SiOk <-1 }
    else if (x[6] == "Site") {SiOk <-1 }
    else if (x[7] == "Site") {SiOk <-1 }
    
    if (SiOk == 1) {StOk <- ""}
    else { StOk <- "Erreur, impossible de trouver la colonne 'Site'. vérifiez la syntaxe du jeu de donnée"}
    
    if (x[1] == "Individuals") {InOk <- 1 }
    else if (x[2] == "Individuals") {InOk <-1 }
    else if (x[3] == "Individuals") {InOk <-1 }
    else if (x[4] == "Individuals") {InOk <-1 }
    else if (x[5] == "Individuals") {InOk <-1 }
    else if (x[6] == "Individuals") {InOk <-1 }
    else if (x[7] == "Individuals") {InOk <-1 }
    
    if (InOk == 1) {IOk <- ""}
    else { IOk <- "Erreur, impossible de trouver la colonne 'Individuals'. vérifiez la syntaxe du jeu de donnée"}
    
    if (x[1] == "Date") {DaOk <- 1 }
    else if (x[2] == "Date") {DaOk <-1 }
    else if (x[3] == "Date") {DaOk <-1 }
    else if (x[4] == "Date") {DaOk <-1 }
    else if (x[5] == "Date") {DaOk <-1 }
    else if (x[6] == "Date") {DaOk <-1 }
    else if (x[7] == "Date") {DaOk <-1 }
    
    if (DaOk == 1) {DOk <- ""}
    else { DOk <- "Erreur, impossible de trouver la colonne 'Date'. vérifiez la syntaxe du jeu de donnée"}
    
    
    if (x[1] == "Hour") {HoOk <- 1 }
    else if (x[2] == "Hour") {HoOk <-1 }
    else if (x[3] == "Hour") {HoOk <-1 }
    else if (x[4] == "Hour") {HoOk <-1 }
    else if (x[5] == "Hour") {HoOk <-1 }
    else if (x[6] == "Hour") {HoOk <-1 }
    else if (x[7] == "Hour") {HoOk <-1 }
    
    if (HoOk == 1) {HOk <- ""}
    else { HOk <- "Erreur, impossible de trouver la colonne 'Hour'. vérifiez la syntaxe du jeu de donnée"}
    
    if (x[1] == "Image1") {ImOk <- 1 }
    else if (x[2] == "Image1") {ImOk <-1 }
    else if (x[3] == "Image1") {ImOk <-1 }
    else if (x[4] == "Image1") {ImOk <-1 }
    else if (x[5] == "Image1") {ImOk <-1 }
    else if (x[6] == "Image1") {ImOk <-1 }
    else if (x[7] == "Image1") {ImOk <-1 }
    
    if (ImOk == 1) {IgOk <- ""}
    else { IgOk <- "Erreur, impossible de trouver la colonne 'Image'. vérifiez la syntaxe du jeu de donnée"}
    
    AllOk <- (SpOk + CamOk + SiOk + InOk + DaOk + HoOk + ImOk)
    
    
    if (AllOk == 7) {AlOk <- ""}
    else {AlOk <- "Le fichier chargé ne correspond pas au format requis. veuillez charger une table de données conforme pour obtenir vos résultats. "}
    
    err <- c(SOk, DOk, COk, StOk, IOk, HOk, IgOk, AllOk,AlOk)
    err
    
  })
  
  # encodage des texte en output
  
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
    err()[9]
  })
  
  probleme <- reactive({
    req(err())
    req(err()[8] != 7)
    
    1
  })
  
  EspecesRatee <- reactive({
    req(input$file)
    req(input$status)
    req(paste(noms()) != 'character(0)')
    1
  })




###################
# Erreur Caméra

errcam <- reactive({
  req(infoCam())
  
  df <- infoCam()
  
  CamOk <- 0
  DurOk <- 0
  UtXOk <- 0
  utYOk <- 0

  x <- c(1,2,3,4,5,6,7,8,9)
  
  x <- names(df)
  

  
  if (x[1] == "Jours") {DurOk <- 1 }
  else if (x[2] == "Jours") {DurOk <-1 }
  else if (x[3] == "Jours") {DurOk <-1 }
  else if (x[4] == "Jours") {DurOk <-1 }
  else if (x[5] == "Jours") {DurOk <-1 }
  else if (x[6] == "Jours") {DurOk <-1 }
  else if (x[7] == "Jours") {DurOk <-1 }
  else if (x[8] == "Jours") {DurOk <-1 }
  else if (x[9] == "Jous") {DurOk <-1 }
  
  if (DurOk == 1) {DurOkk <- ""}
  else { DurOkk <- "Erreur, impossible de trouver la colonne 'Jours'. vérifiez la syntaxe du jeu de donnée"}
  
  if (x[1] == "Camera") {CamOk <- 1 }
  else if (x[2] == "Camera") {CamOk <-1 }
  else if (x[3] == "Camera") {CamOk <-1 }
  else if (x[4] == "Camera") {CamOk <-1 }
  else if (x[5] == "Camera") {CamOk <-1 }
  else if (x[6] == "Camera") {CamOk <-1 }
  else if (x[7] == "Camera") {CamOk <-1 }
  else if (x[8] == "Camera") {CamOk <-1 }
  else if (x[9] == "Camera") {CamOk <-1 }

  if (CamOk == 1) {CamOkk <- ""}
  else { CamOkk <- "Erreur, impossible de trouver la colonne 'Camera'. vérifiez la syntaxe du jeu de donnée"}
  
  if (x[1] == "utm_x") {UtXOk <- 1 }
  else if (x[2] == "utm_x") {UtXOk <-1 }
  else if (x[3] == "utm_x") {UtXOk <-1 }
  else if (x[4] == "utm_x") {UtXOk <-1 }
  else if (x[5] == "utm_x") {UtXOk <-1 }
  else if (x[6] == "utm_x") {UtXOk <-1 }
  else if (x[7] == "utm_x") {UtXOk <-1 }
  else if (x[8] == "utm_x") {UtXOk <-1 }
  else if (x[9] == "utm_x") {UtXOk <-1 }
  
  if (UtXOk == 1) {UtXOkk <- ""}
  else { UtXOkk <- "Erreur, impossible de trouver la colonne 'utm_x'. vérifiez la syntaxe du jeu de donnée"}
  
  if (x[1] == "utm_y") {UtYOk <- 1 }
  else if (x[2] == "utm_y") {UtYOk <-1 }
  else if (x[3] == "utm_y") {UtYOk <-1 }
  else if (x[4] == "utm_y") {UtYOk <-1 }
  else if (x[5] == "utm_y") {UtYOk <-1 }
  else if (x[6] == "utm_y") {UtYOk <-1 }
  else if (x[7] == "utm_y") {UtYOk <-1 }
  else if (x[8] == "utm_y") {UtYOk <-1 }
  else if (x[9] == "utm_y") {UtYOk <-1 }
  
  if (UtYOk == 1) {UtYOkk <- ""}
  else { UtYOkk <- "Erreur, impossible de trouver la colonne 'utm_y'. vérifiez la syntaxe du jeu de donnée"}
  
  
  
  AllOk <- (CamOk + DurOk + UtXOk +UtYOk )
  
  
  if (AllOk == 4) {AlOk <- ""}
  else {AlOk <- "Le fichier chargé ne correspond pas au format requis. veuillez charger une table de données conforme pour obtenir vos résultats. "}
  
  camerr <- c(CamOkk,DurOkk,UtXOkk,UtYOkk,AllOk,AlOk)
  camerr
  
})

# encodage des texte en output

output$erreurCam1 <- renderText({
  req(input$infocam)
  errcam()[1]
})


output$erreurCam2 <- renderText({
  req(input$infocam)
  errcam()[2]
})

output$erreurCam3 <- renderText({
  req(input$infocam)
  errcam()[3]
})

output$erreurCam4 <- renderText({
  req(input$infocam)
  errcam()[4]
})
output$erreurCam5 <- renderText({
  req(input$infocam)
  errcam()[6]
})

ProblemeCam <- reactive({
  req(errcam())
  req(errcam()[5] != 4)
  
  1
})

}


## Run the app ---------------------------------------------------
shinyApp(ui = ui, server = server)
##Ok
##Voyez-vous ce message ? (Flo)