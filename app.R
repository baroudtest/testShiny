library(shiny)
library(lubridate)
library(circular)
library(lubridate)
library(ggplot2)


## Agencement fluipage sur la page, outils d'interactions (uploader un fichier, case à cocher, sliders,...)----
ui <- fluidPage(
## Contrôle sidebar -------------------------------------------
sidebarLayout(
  sidebarPanel(
    
# File input 
    fileInput("file",h2("Table de données")) # fileIput est l'outil permettant de lire un fichier de son choix à uploader
    ,
  
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
  p("- La colonne :", br(), div(em("'inventory_ID'"),style = "color:blue"),br(),"Doit être rajoutée afin de renseigner le numéro d'inventaire pour une comparaison périodique. Il doit être compris entre 1 et n avec n le numéro du dernier inventaire."),
  
  p("2) Les individus identifiés doivent être renseignés dans le champ 'Species' par leur", strong("genre en majuscule"), "et leur", strong("espèce en minuscule"), "exemple :"),
  div(em("'Loxodonta cyclotis'"),style = "color:blue"),
  br(),
  p("- S'ils ne sont pas visibles, ils doivent être renseignés par :"),
  div(em("'no_sp'"), style = "color:blue"),
  br(),
  p("- S'ils ne peuvent être identifiés, ils doivent être renseignés par :"),
  div(em("'indetermined'"), style = "color:blue")
),
## Ouverture des onglets ----------------------------------------------
  mainPanel(tabsetPanel(
## Onglet "Caractéristique des communautés" ---------------------------------------------
    tabPanel("Caractéristiques des communautés",
             fluidRow(
               column(width = 12,
                      "Cette application Shiny est dédiée à l’analyse de données issues d’inventaire par pièges photographiques. Elle permet par une analyse automatisée de fournir quelques indicateurs qui caractérisent les inventaires de faune menés, la communauté et les espèces animale détectées le tout sous forme de tableaux, graphiques et cartes facilement téléchargeables.",
                      br(),
                      br(),
                      br(),
                      p("Indice de détection nocturne en pourcents"),
                      textOutput("indnoc"),
                      br(),
                      br(),
                      p("Détection d'hommes en pourcents"),
                      br(),
                      textOutput("homme")
               ))),
## Onglet "Analyse par espèce" ---------------------------------
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
                                  tableOutput("ab_rel"),
                                  downloadButton("downloadData", "Download"),
                                  textInput("selectSp", h3("choisissez votre espèce"), 
                                              value = "All"),
                                  plotOutput("graph24h"),
                                  downloadButton("downloadGraph", "Download Graph")
                                
                          
    ))),

## Onglet "Cartes" ----------------------------------------------
    tabPanel("Cartes",
             fluidRow(
               column(width = 12,
                      h1("HI!")
    )))

## Fermeture des onglets ----------------------------------------

  ) #Close tabset panel
  
  ) #Close mainpanel

)

)

## Partie Server ------------------------------------------------ 
# traitement de données, récupération des inputs, préparation des outputs--
server <- function(input, output) {
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

# table des informations par espèces , abondance relative, nombre d'individus détecté
# faudrait-il ajouer détection par mois ? 
output$ab_rel <- renderTable({
  nb <- aggregate(Individuals ~ Species+Site, data = data(), sum)
  jours <- aggregate(Individuals ~ Species+Site+Date, data = data(), sum) # ! colonne site ou choix prealable ?
  # utiliser length et unique
  tot <- sum(data()$Individuals)
  abondance <- aggregate$Individuals/tot
  cbind(nb,abondance)
})


# Gérer le télchargement de la liste d'info par espèce
output$downloadData <- downloadHandler(
  filename = function() {
    paste("Liste", ".csv", sep = "")
  },
  content = function(file) {
    write.table(datasetInput(), file,quote = TRUE, sep = ";" ,row.names = FALSE, col.names = FALSE)
  } # !!! datasetInput n'est plus a jour depuis les modifs de la variable pretraitee (je regarde à ca bientot)
)

# Création du graphique d'activité en 24h en réactive de façon à pouvoir le télécharger

  graph24 <- reactive ({
# récupérer l'espèce encodée 
  
  x <- as.character(input$selectSp)
  
# récupérer le dataframe nettoyé
  df <- data()
# si "All" est encodé, graphique de toute les epsèces, si le nome d'une espèe est encodé, le prend en compte
  if (input$selectSp != "All")
  df <- df[df$Species == x,]
  
  # récupéré les heures concernées de l'espèce choisie
  
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

}

## Run the app ---------------------------------------------------
shinyApp(ui = ui, server = server)
##Ok
##Voyez-vous ce message ? (Flo)