library(shiny)
library(lubridate)

## Interface utilisateur, agencement sur la page, outils d'interactions (uploader un fichier, case à cocher, sliders,...) ------
ui <- fluidPage( 
  
tabsetPanel(
  ## Onglet 1 ------------------------
 tabPanel("Caractéristiques des communautés", fluid = TRUE,
  sidebarLayout(
    
    ## Contrôle sidebar onglet 1 ----------------
    sidebarPanel(
      fileInput("file",h2("Table de données")) # fileIput est l'outil permettant de lire un fichier de son choix à uploader
      ,
      h4(div("Note importante concernant le format du jeu de données d'entrée :", style = "color:red")),
      p("1) Le jeu de donnée doit être au", strong("format de sortie CameraBase suivant"), "et doit comporter des colonnes ayant ces noms exacts, écrits dans cet ordre respectif :"),
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
      p("- La colonne :",div(em("'inventory_ID'"),style = "color:blue"),", doit être rajoutée afin de renseigner le numéro d'inventaire pour une comparaison périodique"),
      
      p("2) Les individus identifiés doivent être renseignés dans le champ 'Species' par leur", strong("genre en majuscule"), "et leur", strong("espèce en minuscule"), "exemple :"),
      div(em("'Loxodonta cyclotis'"),style = "color:blue"),
      br(),
      p("- S'ils ne sont pas visibles, ils doivent être renseignés par :"),
      div(em("'no_sp'"), style = "color:blue"),
      br(),
      p("- S'ils ne peuvent être identifiés, ils doivent être renseignés par :"),
      div(em("'indetermined'"), style = "color:blue")
    ),
    
    ## Contrôle mainpanel onglet 1 ----------------------------
    mainPanel( "Cette application Shiny est dédiée à l’analyse de données issues d’inventaire par pièges photographiques. Elle permet par une analyse automatisée de fournir quelques indicateurs qui caractérisent les inventaires de faune menés, la communauté et les espèces animale détectées le tout sous forme de tableaux, graphiques et cartes facilement téléchargeables.",

                        br(),
                        br(),
                        p("Indice de détection nocturne en pourcents"),
                        textOutput("indnoc"),
                        br(),
                        br(),
                        p("Détection d'hommes en pourcents"),
                        br(),
                        textOutput("homme")
                 ) # Fin du mainpanel
    ) # Fin de sidebar layout 
  ) # Fin de tabpanel
 ,
  ## Onglet 2 -----------------------------------------------
      tabPanel("Analyse par espèce", fluid = TRUE, 
            sidebarLayout(
    ## Contrôle sidebar onglet 2 ------------------------------
               sidebarPanel( 
                 fileInput("file",h2("Table de données")) # fileIput est l'outil permettant de lire un fichier de son choix à uploader
                 ) # fin du sidebarPanel
                  , 
    ## Contrôle mainpanel onglet 2 ----------------------------
               mainPanel( "De multiples indices peuvent être générés à partir des données 
                          obtenues par pièges photographiques. Nous avons décidé de vous 
                          présenter seulement le taux de détection standardisé par l’effort 
                          d’inventaire, communément décrit sous le terme de RAI en anglais 
                          (Relative Abundance index) . D’autres analyses sont possibles et des 
                          ressources sont mobilisables dans la partie « Pour en savoir plus »"
                         ) # fin du mainpanel 
            )# fin du sidebarLayout
      )# fin du tabpanel
 ,
              
  ## Onglet 3 -------------------------------------------------
 tabPanel("Cartes", fluid = TRUE, 
          sidebarLayout(
    ## Contrôle sidebar onglet 3 ------------------------------
            sidebarPanel( 
              fileInput("file",h2("Table de données")) # fileIput est l'outil permettant de lire un fichier de son choix à uploader
            ) # fin du sidebarPanel
            , 
    ## Contrôle mainpanel onglet 3 ------------------------------
            mainPanel(
              br(),
              h2("Ici viendra la carte")
            ) # fin du mainpanel
          ) # fin de sidebarLayout
 ) # fin de l'onglet 3


)     # fin des onglets
)     # fin de la mise en forme fluidpage

## traitement de données, récupération des inputs, préparation des outputs ----------------
server <- function(input, output) {
  
  output$ab_rel <- renderTable({
    req(input$file)
    df<- read.csv(input$file$datapath,
                  header = TRUE,
                  sep = ";",
                  quote = '"')
    
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
    for (i in 1:l) {
      
      if (i < l) {s <- i +1 }
      #heure <- factor(df$Hour[c(i)])
      #a <- lubridate::hms(as.character(heure))
      #c <- hour(a)
      #if (c[c(1)] > 18) { }
      #else  if (c[c(1)] < 6) {b <- b + df$Individuals[c(i)]}
      
      heurea <- factor(df$Hour[c(i)])
      heureb <- factor(df$Hour[c(s)])
      
      a <- lubridate ::hms(as.character(heurea))
      b <- lubridate ::hms(as.character(heureb))
      
      g <- as.duration(b-a)
      
      
      if (g > 1800)  {datf[c(r),] <- df[c(i),]
      
      r <- r+1}
      else if (df$Species[c(i)] == df$Species[c(s)]) {}
      else {datf[c(r),] <- df[c(i),]
      r <- r+1
      
      }
    }
    datf
    
    
    
    datf$Individuals <- as.numeric(datf$Individuals) 
    aggregate <- aggregate(Individuals ~ Species+Site, data = datf, sum) 
    tot <- sum(datf$Individuals)
    abondance <- aggregate$Individuals/tot
    cbind(aggregate,abondance)
    
  })
  
  output$indnoc <- renderText({
    req(input$file)
    df<- read.csv(input$file$datapath,
                  header = TRUE,
                  sep = ";",
                  quote = '"')
    
    ligne <- nrow(df)
    b <- 0
    for (i in 1:ligne) {
      heure <- factor(df$Hour[c(i)])
      a <- lubridate::hms(as.character(heure))
      c <- hour(a)
      if (c[c(1)] > 18) { }
      else  if (c[c(1)] < 6) {b <- b + df$Individuals[c(i)]}
      
    }
    
    d <- (b / ligne) *100
    d
  })
  
  output$homme <- renderText({
    req(input$file)
    df<- read.csv(input$file$datapath,
                  header = TRUE,
                  sep = ";",
                  quote = '"')
    
    ligne <- nrow(df)
    b <- 0
    for (i in 1:ligne) {
      if (df$Species[c(i)] == "Homo sapiens")
      {b <- b + df$Individuals[c(i)]}
      
    }
    
    d <- (b / ligne) *100
    d
  })
  
  
}

# Run the app ---------------------------------
shinyApp(ui = ui, server = server)