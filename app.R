library(shiny)
library(lubridate)

# Interface utilisateur, agencement sur la page, outils d'interactions (uploader un fichier, case à cocher, sliders,...)----
ui <- fluidPage(

sidebarLayout(
  sidebarPanel(
    fileInput("file",h2("table de données")), # fileIput est l'outil permettant de lire un fichier de son choix à uploader
    ),

  mainPanel(tabsetPanel(
    tabPanel("Caractéristiques des communautés",
             fluidRow(
               column(width = 12,
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
    tabPanel("Analyse par espèce",
             tabsetPanel(
               tabPanel("Liste",
                        fluidRow(
                          column(width = 4,
                                 tableOutput("ab_rel") 
                          )
                        ),
                        downloadButton("downloadData", "Download")
               ),
               tabPanel("Graphique",
                        fluidRow(
                          column(width = 4,
                                 h1("HI!") 
                          ) 
                        )
               )
             ) #Close inner tabsetPanel
    ),
    
    tabPanel("Cartes",
             fluidRow(
               column(width = 12,
                      h1("HI!")
               )
             )
    )
  ) #Close tabset panel
  
  )

)

)

# mainPanel ..... : l'emplacement ou apparaitra le fichier exporté sousle bon format, le fichier est enregistré parmis les Outputs sous le nom "contents"

 
# traitement de données, récupération des inputs, préparation des outputs--
server <- function(input, output) {
  datasetInput <- reactive({

  req(input$file)
  df<- read.csv(input$file$datapath,
                header = TRUE,
                sep = ";",
                quote = '"')
  df$Species <- as.character(df$Species)
  df$Camera <- as.character(df$Camera)
  df$Site <- as.character(df$Site)
  df$Image1 <- as.character(df$Image1)
  df$Date <- as.character(df$Date)
  df$Hour <- as.character (df$Hour)
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
output$ab_rel <- renderTable({
  datasetInput()
})

output$downloadData <- downloadHandler(
  filename = function() {
    paste("Liste", ".csv", sep = "")
  },
  content = function(file) {
    write.table(datasetInput(), file,quote = TRUE, sep = ";" ,row.names = FALSE, col.names = FALSE)
  }
)

}

# Run the app ----
shinyApp(ui = ui, server = server)