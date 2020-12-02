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
library(shinycssloaders)
#install.packages("shinycssloaders")
library(shinythemes)
#install.packages("shinythemes")
library(data.table)
#install.packages("data.table")
library(sp)
#install.packages("sp")

## Agencement fluipage sur la page, outils d'interactions (uploader un fichier, case à cocher, sliders,...)----
ui <- fluidPage(
## Sélection du thème pour tester
  themeSelector(),
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
   
    fileInput("file",h2("Table de données")), # fileIput est l'outil permettant de lire un fichier de son choix à uploader
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
    br(),
   
    fileInput("infocam",h2("Informations de localisation des caméras")),
    div(textOutput("fichier2"), style = "color:green"),
   
    fileInput("shp",h3("Ajouter un polygone de délimitation de zones")),
    div(textOutput("fichier3"), style = "color:green"),
    numericInput("epsg",h3("Sélectionnez l'EPSG souhaité pour la cartographie"),32632),
    
    
   
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
# les withSpinners servent a implémenter l'animation de chargement
  mainPanel(br(),
            "Cette application Shiny est dédiée à l’analyse de données issues d’inventaire 
                        par pièges photographiques. Elle permet par une analyse automatisée de fournir 
                        quelques indicateurs qui caractérisent les inventaires de faune menés, la communauté 
                        et les espèces animale détectées le tout sous forme de tableaux, graphiques et cartes 
                        facilement téléchargeables.",
            br(),
            br(),
    tabsetPanel(
## Onglet "Caractéristique des communautés" ---------------------------------------------
    tabPanel("Caractéristiques des communautés",
             fluidRow(
               column(width = 12,
                      br(),
                      "Vous trouverez ci-dessous un tableau récapitulatif de la communauté détectée durant votre/vos inventaire(s).",
                      br(),
                      withSpinner(tableOutput("richesse")),
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
                      withSpinner(plotOutput("accumul")),
                      downloadButton("downloadAccumul", "Download Graph"),
                      br()),
               column(width = 4,offset=1,
                      h3("Indices :"),
                      br(),
                      h5("Pourcentage de détections nocturnes"),
                      tableOutput("indnoc"),
                      br(),
                      h5("Présence humaine"),
                      h6("(nombre d'hommes sur la durée en jours de l'inventaire"),
                      br(),
                      tableOutput("homme"))
               )
             ),
## Onglet "Analyse par espèce" (sélection de l'espèce et du site) ---------------------------------
    tabPanel("Analyse par espèce",
             fluidRow(
               column(width = 12,
                                 br(),
                                 "De multiples indices peuvent être générés à partir des données 
                                  obtenues par pièges photographiques. Nous avons décidé de vous 
                                  présenter seulement le taux de détection standardisé par l’effort 
                                  d’inventaire, communément décrit sous le terme de RAI en anglais 
                                  (Relative Abundance index) . D’autres analyses sont possibles et des 
                                  ressources sont mobilisables dans la partie « Pour en savoir plus »",
                                  br(),
                                  br(),
                      selectizeInput(inputId = "selectSp",
                                     label = "Sélection de l'espèce",
                                     choices = "",
                                     selected = "",
                                     multiple = TRUE),
                      br(),
                      selectizeInput(inputId = "selectLoc",
                                     label = "Sélection du site",
                                     choices = "",
                                     multiple = TRUE),
                      br(),
                      downloadButton("downloadData", "Download"),
                      br(),
                                 withSpinner(tableOutput("ab_rel")),
                      helpText("* Ces NA correspondent aux espèces classifiées « Not Applicable (regional category) » par l'IUCN.
                               Les autres NA de la liste correspondent aux espèces ne figurant pas dans notre base de données
                               (non reprises ou dont le nom possède un format incorrect)."),
                      h3("Graphique des observation de l'espèce reprise par heure"),
                    
                      withSpinner(plotOutput("graph24h")),
                                  downloadButton("downloadGraph", "Download Graph"),
              
                      withSpinner(plotOutput("carte_abon_paresp")),
                                  downloadButton("downloadMap3", "Download Map"),
                                  downloadButton("downloadCSV2", "Dowload CSV")
                          
    ))),

## Onglet "Cartes" ----------------------------------------------
    tabPanel("Cartes",
             fluidRow(
               column(width = 12,
                      br(),
                      withSpinner(plotOutput("carte_richesse_spe")),
                      downloadButton("downloadMap1", "Download Map"),
                      br(),
                      br(),
                      downloadButton("downloadCSV1", "Dowload two maps CSV's"),
                      br(),
                      br(),
                      withSpinner(plotOutput("carte_espèces_men")),
                      downloadButton("downloadMap2", "Download Map"),
                      
                      
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
     shape <- st_read(as.character(input$shp$datapath),stringsAsFactors = F)
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
  
  
  observeEvent(input$file, {
    showModal(modalDialog(
      title = "Vérification des noms d'espèces",
      paste("L'application ne reconnait pas les espèces suivantes : ",noms(),". 
      Cela est dû au format incorrect du nom de l'espèce dans votre jeu de données. 
      Veuillez vous référer aux noms scientifiques présents sur le site de l'IUCN, remplacez-les dans 
      votre jeu de données et rechargez vos fichiers. Si le problème persiste après avoir modifié les noms, 
      cela signifie que l'espèce n'est pas présente dans notre base de données. 
      Vous pouvez alors l'ajouter vous même dans le fichier statuts.csv et relancer l'application. 
      Les analyses fournies restent valables, mais les espèces restantes dans cette liste ne pourront pas 
      être prises en compte dans le recensement et la répartition des espèces menacées.", sep=""),
      footer = modalButton("Fermer")
    ))
  })
####################################################################################
    
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
  if(nrow(rich)>1){
    EN <- aggregate(Individuals ~ Site+Species, data = datEN, sum)
    EN <- aggregate(Individuals ~ Site, data = EN, length)
    tab1 <- merge(tab1,EN,by=c("Site","Site"),all=T)
  } else {
    tab1$EN <- 0
  }
  names(tab1) <- c("Site","Nombre de caméras","Effort d'inventaire",
                   "Richesse spécifique","Nombre d'espèces menacées")
  if(nrow(datEN)>=1){
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
##PLUSIEURS SITES PAR GRAPHIQUE
accumul <- function (){
  
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
    #datEN <- subset(datEN,datEN$IUCN=="EN"|datEN$IUCN=="CR")
    donnees <- data()
    donnees <- subset(donnees, donnees$Site == LeSite)
  jour <- as.character(donnees$Date)
  jour <- dmy(jour)
  premier <- min(jour)
  dernier <- max(jour)
  nbjour <- as.numeric(dernier - premier)
# rapport du nombre de "Homo sapiens" sur le nombre de jours d'inventaires  
  d <- (b /nbjour)
  Def$indice[j] <- d
}
Def
})

# message de chargement de table de données
output$fichier1 <- renderText({
  if (is.data.frame(input$file) == TRUE ) {texte1 <- "Table de donnée chargée"}
  else texte1 <- {"Veuillez charger la table de données"}
    
  texte1
  
})

# message de chargement des informations caméras
output$fichier2 <- renderText({
  if (is.data.frame(input$infocam) == TRUE ) {texte1 <- "Informations Caméras chargée"}
  else texte2 <- {"Veuillez charger les Informations Caméras"}
  

  
})

# message de chargement optionnelle du shapefile
output$ichier3 <- renderText({
  req(input$shp)
 texte3 <- "Shapefile chargé"
 texte3
  
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



# Création d'une cartographie des abondances et richesses spé + IUCN et présence d'espèce menacées ------------------------------

# Traitement et préparation des données utiles aux cartes d'abondance et de richesse 

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
  IUCN <- IUCN()
  datEN <- merge(dfinal,IUCN,by="Species",all.x=T)
  # On ne garde dans un nouveau d.f que les individus qui ont un statut IUCN EN ou CR.
  datEN <- subset(datEN,datEN$IUCN=="EN"|datEN$IUCN=="CR")
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
  
  infos_cam$name <- as.character(infos_cam$name)
  infos_cam1 = dplyr::select(infos_cam,utm_x:utm_y,Camera = name)
  

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


donnees_cartes_abun <- reactive ({
  
  req(data())
  dfinal <- data()
  
  # On réalise également les valeurs à afficher pour la carte de l'onglet analyse par espèce : 
  # On aggrège à nouveau les données du nb d'indiv par espèce et par caméra :
  n_indiv_cam_esp2 <- aggregate(Individuals ~ Species+Camera, data = dfinal, sum)
  
  # Sélection de l'espèce qui nous intéresse :
  req(input$selectSp)
  y <- as.character(input$selectSp)
  nb_indiv_selectesp <- n_indiv_cam_esp2
  # si "All" est encodé, graphique de toute les epsèces, si le nom d'une espèe est encodé, le prend en compte
  if (input$selectSp != "All")
    nb_indiv_selectesp <- n_indiv_cam_esp2[n_indiv_cam_esp2 == y,]
  
  # Pour l'abondance :
  # On calcule d'abord le nb total d'individus toutes caméras confondues : 
  tot <- sum(dfinal$Individuals)
  # On défini deux nlles colonnes abondance générale et abondance par rapport à la caméra:
  # On commence par calculer le nb d'individus observés par caméra, dans un nouveau df
  nind_cam <- aggregate(Individuals ~ Camera, data = dfinal, sum)
  # On renomme la colonne Effcam
  nind_cam <- rename(nind_cam,"Tot_individuals_cam"="Individuals") 
  # On joint alors la colonne EffCam contenan le nb d'indiv/cam au d.f portant sur le nb d'individus d'une espèce sélectionnée
  nb_indiv_cam_selectesp <- merge(nb_indiv_selectesp,nind_cam,by="Camera",all.x=T)
  req(coordcam())
  infos_cam <- coordcam()
  infos_cam$name <- as.character(infos_cam$name)
  infos_cam2 <- dplyr::select(infos_cam,duration,Camera = name)
  nb_indiv_cam_selectesp <- merge(nb_indiv_cam_selectesp,infos_cam2,by="Camera",all.x=T)
  nb_indiv_cam_selectesp$RAI <- as.numeric((nb_indiv_cam_selectesp$Individuals))/as.numeric((nb_indiv_cam_selectesp$duration))
  # On défini la colonne abuntot comme l'abondance relative totale de l'espèce sélectionnée pour une caméra donnée.
  # On défini la colonne abuncam comme l'abondance relative partielle (rapport non pas avec le nb tot d'individus, mais avec le nb 
  # d'indiv pour cette caméra)
  # de l'espèce sélectionnée pour une caméra donnée.
  nb_indiv_cam_selectesp$abondance_rel <- as.numeric(nb_indiv_cam_selectesp$Individuals)/as.numeric(nb_indiv_cam_selectesp$Tot_individuals_cam)*100
  
  # On recommmence la manip précédente pour la jointure des coordonnées et transformation en df.sf :
  infos_cam1 = dplyr::select(infos_cam,utm_x:utm_y,Camera = name)
  
  infos_cam1$Camera=as.character(infos_cam1$Camera)
  nb_indiv_cam_selectesp$Camera=as.character(nb_indiv_cam_selectesp$Camera) # On transforme les valeurs des champs Camera en character afin de pouvoir effectuer la jointure
  
  nb_indiv_cam_selectesp2 = left_join(nb_indiv_cam_selectesp,infos_cam1, by = c("Camera" = "Camera"))
  
  req(EPSG())
  epsg <- EPSG()
  nb_indiv_cam_selectesp2=st_as_sf(nb_indiv_cam_selectesp2,coords=c("utm_y","utm_x"),crs=epsg)
  
  nb_indiv_cam_selectesp2
  
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
 
 
 carte_abon_paresp1 <- reactive ({ 
   # Calcul et affectation des données de l'emprise de la carte :
   req(EPSG())
   epsg <-EPSG()
   req(donnees_cartes_abun())
   richespe <- donnees_cartes_abun()
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
   
   
   carte3 <- ggplot() +
     geom_sf(mapping=aes(size=abondance_rel, color=RAI) ,data=richespe) +
     coord_sf(crs = st_crs(epsg),xlim=c(xmin-coefx,xmax+coefx),ylim=c(ymin-coefy,ymax+coefy), datum = sf::st_crs(4326), expand = FALSE) +
     scale_size_continuous(name = "Abondance (en %)", range=c(2,7)) +
     scale_colour_gradientn(name = "RAI (nb/jour)", colours = terrain.colors(5)) + 
     labs(title = "Cartographie du RAI et de l'abondance relative des espèces sélectionnées",
          subtitle = "Pour les caméras ou l'espèce à été détectée",
          caption = "La taille des points indique l'ordre d'abondance relative de l'espèce pour chaque caméra",
          x = "utm_x", y = "utm_y") +
     theme_dark() +
     theme(
       legend.position = c(1.11, 0.5),
       legend.direction = "vertical",
       legend.key.size = unit(0.4, "cm"),
       legend.key.width = unit(0.4,"cm"),
       legend.title = element_text(color = "red", size = 9, face = "bold"),
       legend.text = element_text(color = "red", size = 8),
       legend.background = element_rect(fill = "white"),
       plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
       plot.subtitle = element_text(hjust = 0.5),
       axis.title.x = element_text(color = "red", size = 10, face = "bold"),
       axis.title.y = element_text(color = "red", size = 10, face = "bold")) +
     annotation_scale(location = "tr", width_hint = 0.3) +
     annotation_north_arrow(location = "tr", which_north = "true", 
                            pad_x = unit(0.2, "cm"), pad_y = unit(0.6, "cm"),
                            style = north_arrow_fancy_orienteering)
   
   carte3
 })

 # Gestion des affichages des cartes : 
 

output$carte_richesse_spe <- renderPlot({req(carte_richesse_spe1())
  carte_richesse_spe1()})

output$carte_espèces_men <- renderPlot({req(carte_espèces_men1()) 
  carte_espèces_men1()})

output$carte_abon_paresp <- renderPlot({req(carte_abon_paresp1()) 
  carte_abon_paresp1()})

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

# Carte des abondances par espèces : 


output$downloadMap3 <- downloadHandler(
  # filename pour définir le nom par défaut du fichier produit, Content pour choisir le graph dans l'image
  filename = function() {paste("Map_RAI",input$selectSp, '.png', sep=' ') },
  content = function(file) {
    
    ggsave(file,plot=carte_abon_paresp1(),width=18,height = 6)
  }
  
)

# Préparation des données pour export csv :

donnees_cartes_abun_df <- reactive ({
  req(donnees_cartes_abun())
  donneesf <- donnees_cartes_abun()
  req(coordcam())
  coordocam <- coordcam()
  donneesdf = as.data.frame(st_drop_geometry(donneesf))
  coordocam = dplyr::select(coordocam,utm_x:utm_y,Camera = name)
  coordocam$Camera=as.character(coordocam$Camera)
  donneesdf$Camera=as.character(donneesdf$Camera) 
  donneesdf2 = left_join(donneesdf,coordocam, by = c("Camera" = "Camera"))
  donneesdf2
})

donnees_cartes_richesse_spe_df <- reactive ({
  req(donnees_cartes_richesse_spe())
  donneesf1 <- donnees_cartes_richesse_spe()
  req(coordcam())
  coordocam <- coordcam()
  donneesdf1 = as.data.frame(st_drop_geometry(donneesf1))
  coordocam = dplyr::select(coordocam,utm_x:utm_y,Camera = name)
  coordocam$Camera=as.character(coordocam$Camera)
  donneesdf1$Camera=as.character(donneesdf1$Camera) 
  donneesdf3 = left_join(donneesdf1,coordocam, by = c("Camera" = "Camera"))
  donneesdf3
})


# Download du csv pour la richesse spé et les espèces EN et CR IUCN
output$downloadCSV1 <- downloadHandler(
  filename = paste("Richesse_IUCN.csv"),
  content = function(file) {
    write.table(donnees_cartes_richesse_spe_df(), file,quote = TRUE, sep = ";",dec=".",row.names = FALSE, col.names = TRUE)
  } 
  
)

# Download du csv pour le RAI et l'abondance
output$downloadCSV2 <- downloadHandler(
  filename = paste("RAI_abondance",input$selectSp, '.csv', sep=' '),
  content = function(file) {
    write.table(donnees_cartes_abun_df(), file,quote = TRUE, sep = ";",dec=".",row.names = FALSE, col.names = TRUE)
  } 
  
)

 
# Création du graphique d'activité en 24h en réactive de façon à pouvoir le télécharger -----------------

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
  
  hour_of_event <- lubridate::hour(a)
  
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

## message d'erreur ==> verification des noms de colonnes

err <- reactive({
  req(input$file)
  
 
  SpOk <- 0
  CamOk <- 0
  SiOk <- 0
  InOk <- 0
  DaOk <- 0
  HoOk <- 0
  ImOk <- 0
  x <- c(1,2,3,4,5,6,7)
  
  x <- names(data())
 
  
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

err <- c(SOk, DOk, COk, StOk, IOk, DOk, HOk, IgOk)
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
  err()[8]
})



}

## Run the app ---------------------------------------------------
shinyApp(ui = ui, server = server)
##Ok
##Voyez-vous ce message ? (Flo)