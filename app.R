### INSTALLATION ET CHARGEMENT DES PACKAGES NECESSAIRES 

requiredPackages = c("shiny","shinydashboard",'DT','plotly','tidyr','dplyr','ggplot2','lubridate','rpart','rpart.plot','colourpicker', 'shinyWidgets', 'sparkline', 'visNetwork')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}


### IMPORT DES DONNEES
rm(list=ls(all=TRUE)) # nettoyage de l'environnement

# importation du fichier issu du serveur  Syslog-NG
src_fw <- read.csv2("log_challenge.csv",sep=";")

# renommage des colonnes
src_fw$datetime <-strptime(src_fw$datetime, "%d/%m/%Y %H:%M")


ui <- dashboardPage(skin = "blue",
              dashboardHeader(title = "Challenge de sécurité",
                              titleWidth = 250),
              
              ### DIFFERENTS ONGLETS ET SOUS ONGLETS
              dashboardSidebar(width = 250,
                sidebarMenu(
                  menuItem("Accueil",tabName = "pageprincipale",icon = icon("home")),
                  menuItem("Visualisation des données", tabName = "vis", icon=icon("line-chart"),
                           menuSubItem("Données brutes",tabName='donnees', icon=icon("table")),
                           menuSubItem("Analyse des flux",tabName='flux', icon=icon("poll")),
                           menuSubItem("Analyse des ports",tabName = "ports",icon = icon("dot-circle-o")),
                           menuSubItem("Visualisation intéractive",tabName='vis_int', icon=icon("chart-bar")),
                           menuSubItem("Statistiques",tabName='stats', icon=icon("trophy")),
                           menuSubItem("Analyse temporelle",tabName='temp',icon = icon("clock"))),
                  menuItem("Data Mining", tabName = "analytique", icon=icon("dashboard"),
                           menuSubItem("Clustering",tabName='clust', icon=icon("sitemap")),
                           menuSubItem("Arbre de décision",tabName='arbre', icon=icon("project-diagram")))
                )),
              
              ### CONTENU DE L APPLI POUR CHAQUE ONGLET 
              dashboardBody(
                ## Page principale
                tabItems(tabItem(tabName = "pageprincipale",
                    h1("Bienvenue !", align = "center"),
                    br(),
                  p(style="text-align: justify; font-size:20px;", "Cette application a pour but d'analyser des données issues d'un équipement de sécurité.
                            Elle est réalisée dans le cadre du cours commun de sécurité entre les étudiants du M2 OPSIE et ceux du M2 SISE de l'Université Lumière Lyon 2.
                            Il s'agit donc de visualiser et d'analyser des logs provenant d'un firewall."),
                  br(),
                  p(style="text-align: justify; font-size:20px;", "Dans un premier temps, nous avons construit et implémenté des règles de firewall. Puis nous avons lancé des connexions licites et illicites afin de récupérer des données de journalisation."),
                  br(),
                  p(style="text-align: justify; font-size:20px;", "Ensuite, il s'agissait de mettre en place une application de visualisation dynamique.
                    Pour cela, il a été mis en place plusieurs onglets dans l'application qui sont les suivants :"),
                  br(),
                  p(style="text-align: justify; font-size:20px;", " - Le premier onglet permet une visualisation des données.
                    Avec notamment la possibilité de parcourir le jeu de données, d'analyser les flux rejetés et acceptés en fonction des protocoles,
                    d'analyser les contacts des IP sources, ainsi que celles les plus émettrices, mais également d'analyser les ports de destinations.
                    Enfin, une analyse temporelle permet d'observer l'activité par heure et minute."),
                  br(),
                  p(style="text-align: justify; font-size:20px;", " - Le deuxième onglet est une aide à la décision avec un arbre de décision et une CAH."),
                  br(),
                  ),
                  ## Données brutes
                  tabItem(tabName = "donnees",
                          fluidRow(
                            valueBoxOutput("nb_ipsrc"),
                            valueBoxOutput("nb_ipdst"),
                            valueBoxOutput("nb_portdst"),
                            valueBoxOutput("nb_regle"),
                            valueBoxOutput("nb_permit"),
                            valueBoxOutput("nb_deny")),
                          br(),
                          fluidRow(
                          dataTableOutput('table'))
                          ),
                  ## Analyse des flux
                  tabItem(tabName = "flux",fluidRow(
                    selectInput("barproto", "Filtrer par protocole :",unique(src_fw$proto)),
                    plotlyOutput("pie"))),
                  ## Analyse des ports
                  tabItem(tabName = "ports",fluidRow(
                    box(selectInput("choix_acces", label = "Sélection du type d'action", choices = list("Permit et Deny"="both","Permit" = "Permit", "Deny" = "Deny" ))),
                    box(radioButtons("choix_port", label = "Sélection du port de destination", choices =list("Tous les ports"="both","Inférieurs ou égaux à 1024" = "inf1024", "Supérieurs 1024" = "sup1024")))),
                    fluidRow(status = "primary", plotlyOutput("barplot2", height = 500))),
                  ## Vision intéractive
                  tabItem(tabName = "vis_int",
                          fluidRow(
                              sliderInput("occurences", label = h4("Nombre d'occurences contactées"), min = 0, 
                                          max = 8000, value = c(0, 50), width = '50%')),
                              fluidRow(plotlyOutput("graph", height = 'auto'))),
                  ## Statistiques
                  tabItem(tabName = "stats",
                          fluidRow(box(sliderInput("barmax","TOP:",min = 5,  max = 15,  value = 10)),
                                   box(selectInput("select_port", "Choix du port :", choices = c('Tous',20, 21, 23, 80, 8080, 3306)))
                                   ),
                          fluidRow(status = "primary", plotlyOutput("barplot", height = 700)
                          )),
                  ## Analyse temporelle
                  tabItem(tabName = "temp",
                          fluidRow(selectInput("barproto2", "Filtrer par protocole :",unique(src_fw$proto)),
                                   plotlyOutput("lines_min"),
                                   br(),
                                   plotlyOutput("lines_hour"))),
                  ## Arbre de décision
                  tabItem(tabName = "arbre",
                          fluidRow(box(selectInput("cible", "Sélection de la variable cible:",colnames(src_fw))),
                                   box(selectInput("expli", "Sélection des variables explicatives:",colnames(src_fw), multiple = T)),
                                               plotOutput("tree"))),
                  ## CAH
                  tabItem(tabName = "clust",
                          fluidRow(box(selectInput("cah_cible", "Sélection de la variable cible:",colnames(src_fw))),
                                   box(selectInput("cah_expli", "Sélection des variables explicatives:",colnames(src_fw), multiple = T)),
                                   plotOutput("cah_plot")),
                          fluidRow(dataTableOutput('sim'))
                          )
                  ))
)
                  

server <- function(input, output, session) {
  
  ### ONGLET DONNEES BRUTES
  output$table = DT::renderDataTable({
    src_fw
  })
  
  output$nb_ipsrc <- renderValueBox({
    valueBox(
      paste0(length(unique(src_fw$ipsrc))), "IP sources", icon = icon("map-marker"),
      color = "purple"
    )
  })
  
  output$nb_ipdst <- renderValueBox({
    valueBox(
      paste0(length(unique(src_fw$ipdst))), "IP destinations", icon = icon("location-arrow"),
      color = "yellow"
    )
  })
  
  output$nb_portdst <- renderValueBox({
    valueBox(
      paste0(length(unique(src_fw$dstport))), "ports destinations", icon = icon("plane"),
      color = "teal"
    )
  })
  
  output$nb_regle <- renderValueBox({
    valueBox(
      paste0(length(unique(src_fw$policyid))), "règles", icon = icon("shield"),
      color = "blue"
    )
  })
    
  output$nb_permit <- renderValueBox({
    valueBox(
      paste0(nrow(src_fw[src_fw$action=="Permit",])), "actions permises", icon = icon("check"),
      color = "green"
    )
  })
  
  output$nb_deny <- renderValueBox({
    valueBox(
      paste0(nrow(src_fw[src_fw$action=="Deny",])), "actions refusées", icon = icon("times"),
      color = "red"
    )
  })
    
  
  ### ONGLET ANALYSE DES FLUX
  output$pie <- renderPlotly({

    #Permet de filtrer les donnees grace aux inputs
    bar = src_fw %>% filter(proto==input$barproto)
    colors <- c('red', 'blue')
    plot_ly(bar, labels = ~action, type = 'pie') %>%
      layout(height = 500, showlegend = TRUE, legend = list(font = list(size = 20)))

  })
  
  ### ONGLET ANALYSE DES PORTS
  output$barplot2 <- renderPlotly({
    if (input$choix_port=="both")
    {
      if (input$choix_acces=="both")
      {
        test_top_portdst=sort(table(src_fw$dstport),decreasing = TRUE)
        
      } else {
        test_top_portdst=sort(table(subset(src_fw,src_fw$action==input$choix_acces,select=c(dstport))),decreasing = TRUE)
        
      }
    }else if (input$choix_port == "inf1024"){
      if (input$choix_acces=="both"){
        test_top_portdst=sort(table(subset(src_fw,dstport<1024,select=c(dstport))),decreasing = TRUE)
      } else {
        test_top_portdst=sort(table(subset(src_fw,dstport<1024 & src_fw$action==input$choix_acces,select=c(dstport))),decreasing = TRUE)
      }
    } else if (input$choix_port == "sup1024")
      if (input$choix_acces=="both"){
        test_top_portdst=sort(table(subset(src_fw,dstport>=1024,select=c(dstport))),decreasing = TRUE)
      } else
      {
        test_top_portdst=sort(table(subset(src_fw,dstport>=1024 & src_fw$action==input$choix_acces,select=c(dstport))),decreasing = TRUE)
        
      }
    top_portdst=as.data.frame(test_top_portdst)
    rownames(top_portdst)=top_portdst$var1
    colnames(top_portdst)=c("portdst","Nb")
    
    g2 <- ggplot(head(top_portdst,input$barmax), aes(x = reorder(portdst,-Nb), y = Nb, text = paste(
      "Port : ", {portdst},
      "\nFrequence : ", Nb))) +
      geom_bar(stat="identity",width = .7, fill = 'steelblue')+
      xlab("Ports") + ylab("Frequence") +
      ggtitle("Ports de destination") + theme_classic() +
      theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))
    
    gg2 = ggplotly(g2, dynamicTicks = TRUE, tooltip = "text")
    gg2
    
  })
  
  
  ### ONGLET VISUALISATION INTERACTIVE
  output$graph <- renderPlotly({
    n=table(src_fw$ipsrc,src_fw$action)
    res = as.data.frame.matrix(addmargins(n))
    res = res[-nrow(res),]
    res = cbind(rownames(res), res)
    res = pivot_longer(res,cols = c('Deny','Permit'), names_to = "Variable", values_to = "Value")
    colnames(res) = c('IP','Sum','Variable','Value')
    
    selection = res[res$Sum>=input$occurences[1]&res$Sum<=input$occurences[2],]
    
    g_plot <- ggplot(selection, aes(fill=factor(Variable), y=Value, x=IP, text = paste(
      "Frequence : ", Value))) + geom_bar(position="stack",stat="identity",
                                          width = 0.6,
                                          color = "black",
                                          size = 0.5,
                                          alpha = 0.7) +
      xlab("Adresses IP sources") + ylab("Nombre d'occurences contactées") +
      ggtitle("Nombre d'occurences contactées par IP source en fonction de leur type") + theme_classic() +
      theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=45,hjust=1,vjust=0.5), legend.title = element_text(size=12), legend.text = element_text(size=12)) +
      labs(fill = "Action")
    gg_plot = ggplotly(g_plot, dynamicTicks = TRUE, tooltip = "text") %>% layout(height = 700)
    gg_plot
    
  })

  ### ONGLET STATISTIQUES
  output$barplot <- renderPlotly({
    if (input$select_port != 'Tous')
    {
      src_fw = filter(src_fw, dstport==input$select_port)
    }
    test_top_ipsrc=sort(table(src_fw$ipsrc),decreasing = TRUE)
    top_ipsrc=as.data.frame(test_top_ipsrc)
    rownames(top_ipsrc)=top_ipsrc$var1
    colnames(top_ipsrc)=c("ipsrc","Nb")
    
    g <- ggplot(head(top_ipsrc,input$barmax), aes(x = reorder(ipsrc,-Nb), y = Nb, text = paste(
      "Adresse IP source : ", {ipsrc},
      "\nFrequence : ", Nb))) +
      geom_bar(stat="identity",width = .7, fill = 'steelblue')+
      xlab("Adresse IP source") + ylab("Frequence") +
      ggtitle("Adresses IP sources les plus émettrices") + theme_classic() +
      theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))
    
    gg = ggplotly(g, dynamicTicks = TRUE, tooltip = "text")
    gg
  })
  
  
  ### ONGLET ANALYSE TEMPORELLE
  output$lines_min <- renderPlotly({
    data_min = src_fw %>% filter(proto==input$barproto2) %>% group_by(action, minute(datetime)) %>% summarise(count = n())
    
    
    fig_min <- plot_ly(data_min, x = ~`minute(datetime)`, y = ~count, type = 'scatter', mode = 'lines', color = ~action)
    
    fig_min <- fig_min %>% layout(title = 'Evolution des actions en fonction des minutes',
                                  xaxis = list(title = 'Minutes',
                                               zeroline = TRUE),
                                  yaxis = list(title = "Nombre d'actions"))
    
    fig_min
    
  })
  
  output$lines_hour <- renderPlotly({
    data_heure = src_fw %>% filter(proto==input$barproto2) %>% group_by(action, hour(datetime)) %>% summarise(count = n())
    
    
    fig_heure <- plot_ly(data_heure, x = ~`hour(datetime)`, y = ~count, type = 'scatter', mode = 'lines', color = ~action)
    
    fig_heure <- fig_heure %>% layout(title = 'Evolution des actions en fonction des heures',
                                      xaxis = list(title = 'Heures',
                                                   zeroline = TRUE),
                                      yaxis = list(title = "Nombre d'actions"))
    
    fig_heure
  })
  
  ### ONGLET ARBRE DE DECISION
  output$tree <- renderPlot({
    src_fw$dstport=as.factor(src_fw$dstport)
    src_fw$policyid=as.factor(src_fw$policyid)
    
    explicatives = unlist(as.character(input$expli))
    cible = as.character(input$cible)
    
    sel <- as.data.frame(src_fw[,c(explicatives, cible)])
    t <- rpart(sel[,cible]~., data=sel[,explicatives], method='class', model = T)
    
    prp(t,
        type = 3,
        extra = 101,
        fallen.leaves = T,
        box.palette = 'auto',
        round = 2,
        branch.lty = 2,
        branch.lwd = 1,
        space = -1,
        varlen = 0,
        faclen = 0)
  })
  ### ONGLET CAH
  output$cah_plot <- renderPlot({
    exp = unlist(as.character(input$cah_expli))
    target = as.character(input$cah_cible)
    data_selection = as.data.frame(src_fw[,c(exp, target)])
    data_selection[] = lapply(data_selection,as.factor)
    
    # Fonction de calcul du V de Cramer
    cramer = function(y,x){
      K = nlevels(y)
      L = nlevels(x)
      n = length(y)
      chi2 = chisq.test(y,x,correct=F)
      v = sqrt(chi2$statistic/(n*min(K-1,L-1)))
      return(v)
    }
    
    # Matrice de simmilarité 
    sim = matrix(1,nrow=ncol(data_selection),ncol=ncol(data_selection))
    rownames(sim) = colnames(data_selection)
    colnames(sim) = colnames(data_selection)
    for (i in 1:(nrow(sim)-1)){
      for (j in (i+1):ncol(sim)){
        y = data_selection[,i]
        x = data_selection[,j]
        sim[i,j] = cramer(y,x)
        sim[j,i] = sim[i,j]
      }
    }
    
    # Matrice des distance 
    dissim = as.dist(1-sim)
    
    # Clustering
    tree = hclust(dissim,method="ward.D")
    plot(tree,main="Dendrogramme")
    
  })
  
  output$sim <- DT::renderDataTable({
    exp = unlist(as.character(input$cah_expli))
    target = as.character(input$cah_cible)
    data_selection = as.data.frame(src_fw[,c(exp, target)])
    data_selection[] = lapply(data_selection,as.factor)
    
    # Fonction de calcul du V de Cramer
    cramer = function(y,x){
      K = nlevels(y)
      L = nlevels(x)
      n = length(y)
      chi2 = chisq.test(y,x,correct=F)
      v = sqrt(chi2$statistic/(n*min(K-1,L-1)))
      return(v)
    }
    
    sim = matrix(1,nrow=ncol(data_selection),ncol=ncol(data_selection))
    rownames(sim) = colnames(data_selection)
    colnames(sim) = colnames(data_selection)
    for (i in 1:(nrow(sim)-1)){
      for (j in (i+1):ncol(sim)){
        y = data_selection[,i]
        x = data_selection[,j]
        sim[i,j] = cramer(y,x)
        sim[j,i] = sim[i,j]
      }
    }
    sim
  })
}

shinyApp(ui, server)