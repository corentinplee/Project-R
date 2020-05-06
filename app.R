library(shiny)
library(shinythemes)
library(RMySQL)
library(pingr)

# (c) Gervasio Marchand, https://g3rv4.com/2017/08/shiny-detect-mobile-browsers

mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}
# Détection du navigateur + version de l'utilisateur à modifier en fichier js
js <- "
// execute the code after the shiny session has started
$(document).on('shiny:sessioninitialized', function(event) {
  // browser detection from https://stackoverflow.com/a/5918791/8099834
  navigator.sayswho= (function(){
    var ua= navigator.userAgent, tem, 
    M= ua.match(/(opera|chrome|safari|firefox|msie|trident(?=\\/))\\/?\\s*(\\d+)/i) || [];
    if(/trident/i.test(M[1])){
        tem=  /\\brv[ :]+(\\d+)/g.exec(ua) || [];
        return 'IE '+(tem[1] || '');
    }
    if(M[1]=== 'Chrome'){
        tem= ua.match(/\\b(OPR|Edge)\\/(\\d+)/);
        if(tem!= null) return tem.slice(1).join(' ').replace('OPR', 'Opera');
    }
    M= M[2]? [M[1], M[2]]: [navigator.appName, navigator.appVersion, '-?'];
    if((tem= ua.match(/version\\/(\\d+)/i))!= null) M.splice(1, 1, tem[1]);
    return M.join(' ');
  })(); 
  // pass browser info from JS to R
  Shiny.onInputChange('myBrowser', navigator.sayswho); 
});
"

#informations on database
options(mysql = list(
  "host" = "mysql-corentin-plee.alwaysdata.net",
  "user" = "202831",
  "password" = "mabd2020"
))

databaseName <- "corentin-plee_project_r"
epochTime <- function(){
  as.integer(Sys.time())
}

countries.list <- read.table("country.txt", header =FALSE,stringsAsFactors = FALSE,quote = "", col.names = "countryname")
choice.country <- as.list(countries.list$countryname)

# informations upload to database
fieldsAll <- c("age","country","gender","language","earphones","impairment","browser","connection")
ipadress <- my_ip()

ui <- fluidPage(#theme=shinytheme("slate"),
  headerPanel(
    #h2(textOutput("currentPing"))),
   h6(textOutput("currentPing"),align ="right", display = "inline"),"app"),
  sidebarLayout(
    sidebarPanel(
      div(img(src="amu.jpg",width=230), style="text-align: center;"),
      HTML("<br>"),
      div(img(src="logo_ILCB.jpg",width=230), style="text-align: center;")
      ),
    mainPanel(
    
    # Render a reactive output variable as HTML within an application page
    # Intended to be used with renderUI on the server side
    # Is currently just an alias for htmlOutput
  
    uiOutput("MainAction"),
    tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"),   # Prevents gray screen during Sys.sleep()
    tags$head(
      tags$script(HTML(js)),
      tags$script('
  $(document).ready(function () {
    navigator.geolocation.getCurrentPosition(onSuccess, onError);

    function onError (err) {
    Shiny.onInputChange("geolocation", false);
    }
    
   function onSuccess (position) {
      setTimeout(function () {
          var coords = position.coords;
          console.log(coords.latitude + ", " + coords.longitude);
          Shiny.onInputChange("geolocation", true);
          Shiny.onInputChange("lat", coords.latitude);
          Shiny.onInputChange("long", coords.longitude);
      }, 1100)
  }
  });
')
    ),
    )
  ) # end of sidebarLayout
  
) # end of fluidPage

server <- function(input, output, session) {
  
  #create an object for storing reactive values
  
  CurrentValues <- reactiveValues(page = "testGeo")
                                  
  # Send dynamic UI to ui - DON'T CHANGE!
  output$MainAction <- renderUI({
    PageLayouts()
  })
  # Wraps a normal expression to create a reactive expression.
  # A reactive expression is an expression whose result will change over time.
#server <- function(input,output){
#  output$value <- renderPrint({
#    input$gt_mobile_detection
#    isolate(input$gender)
 # })
#}
  #send data when a field is filled 
  formData <- reactive({
    data <- sapply(fieldsAll,function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data
  })
  #connection to database, construct a query of the data, submit it and disconnect
  saveData <- function(data) {
    db <- dbConnect(MySQL(), dbname = databaseName, host =options()$mysql$host,
                    user = options()$mysql$user,
                    password = options()$mysql$password)
    query <- sprintf(
      "INSERT INTO Users (%s) VALUES ('%s')",
      #table,
      paste(names(data), collapse =", "),
      paste(data,collapse = "', '")
    )
    
    dbGetQuery(db,query)
    dbDisconnect(db)

  }
 
  PageLayouts <- reactive(
    {
    if (CurrentValues$page == "testGeo")
  {
    return(
      list(titlePanel("Localisation"),
           
           tags$script('
      $(document).ready(function () {
        navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
        function onError (err) {
          Shiny.onInputChange("geolocation", false);
        }
              
        function onSuccess (position) {
          setTimeout(function () {
            var coords = position.coords;
            console.log(coords.latitude + ", " + coords.longitude);
            Shiny.onInputChange("geolocation", true);
            Shiny.onInputChange("lat", coords.latitude);
            Shiny.onInputChange("long", coords.longitude);
          }, 1100)
        }
      });
              '),
           
           # Show a plot of the generated distribution
           fluidRow(column(width = 2,
                           verbatimTextOutput("lat"),
                           verbatimTextOutput("long"),
                           verbatimTextOutput("geolocation"))
           ),
           conditionalPanel(
             condition =c("input.geolocation == true"),
             HTML("<p1> Veuillez continuer si la valeur du dessus est TRUE.</p1>")
           ),    
      
           
      actionButton(inputId = "index",label = "Étape suivante")
    )
    )
  }
    if (CurrentValues$page == "index")
    {
      return(
        list(
          HTML("<h1>Page d'identification</h1>"),
          numericInput('age','Entrez votre âge','',min= 1, max= 120),
          selectizeInput("country","Dans quel pays réalisez-vous ce test?",choices= choice.country),
          radioButtons("gender","Quel est votre sexe? ",choices=c("Homme","Femme"),selected=character(0),inline=TRUE),
          radioButtons("language","La langue française est-elle votre langue maternelle?",choices=c("oui","non"),selected=character(0),inline=FALSE),
          radioButtons("earphones","Êtes-vous bien sur ordinateur avec des écouteurs?",choices=c("oui","non"),selected=character(0),inline=FALSE),
          radioButtons("impairment","Avez-vous des troubles auditifs ou visuels connus?",choices=c("oui","non"),selected=character(0),inline=FALSE),
          textInput('browser','Quel est votre navigateur et sa version?'),
          HTML("<p>Si vous ne savez pas la version ou le navigateur que vous utilisez <br> veuillez trouver la réponse vous concernant ici :</p>"),
          textOutput("myBrowserOutput"),
          #my_ip(),# A voir car affiche une Ip mais il semblerait que ce ne soit pas la bonne IP...
          #ping(ipadress,count = 1),
          #ping(ipadress,count = 1),
          #print(ipadress),
          #is_online(),
          #ping_port("www.google.com", port = 80, count = 1), #fait vraisemblablement apparaitre le ping de l'utilisateur, à tester.
          radioButtons("connection","Vous êtes connecté à Internet en",choices=c("filaire","wi-fi"),selected=character(0),inline=TRUE),
          HTML("<br>"),
          conditionalPanel(
          condition =c("input.language == 'oui' && input.earphones =='oui' && input.impairment =='non'"),
            actionButton(inputId = "gt_mobile_detection",label = "Étape suivante")
          ),
          conditionalPanel(
          condition =c("input.language == 'non' || input.earphones =='non' || input.impairment =='oui'"),
          h3("Mauvaise réponse.Vous ne pouvez pas continuer.")
          )
          
        )
      )
    }

    if (CurrentValues$page == "mobile_detection")
      {
      return(
        list(
          HTML("<h3>Identification du type de votre appareil<br>(mobile / non-mobile)</h3><hr><br>"),
          mobileDetect('isMobile'),
          verbatimTextOutput("value"),
          h4(textOutput('isItMobile')),
          HTML("<br><br>"),
          if (length(input$isMobile) > 0) {
            actionButton(inputId = "gt_assessing_Fr_level",label = "Étape suivante")
          }
        )
      )
      }
          
    if (CurrentValues$page == "assessing_French_level")
    {
      
      return(
        list(
          h3("Évaluation de votre niveau en français"),
          hr(),
          radioButtons("q1","Sophie habite à Paris. Elle est...",choices=c("français.","française.","franceise."),selected=character(0),inline=FALSE),
          radioButtons("q2","Vous avez quel âge ?",choices=c("J'en ai trois.","J'ai vingt ans.","J'ai vingt années."),selected=character(0),inline=FALSE),
          radioButtons("q3","Je suis malade. Je vais chez...",choices=c("le médecin.","le garagiste.","le vendeur."),selected=character(0),inline=FALSE),
          radioButtons("q4","Après l'été, c'est...",choices=c("l'hiver.","le printemps.","l'automne."),selected=character(0),inline=FALSE),
          HTML("<br>"),
          h4(textOutput("French_level")),
          HTML("<br><br>"),
          actionButton(inputId = "gt_welcome",label = "Étape suivante")          
        )
      )
    }
          
    # 1) WELCOME PAGE
    if (CurrentValues$page == "welcome")
      {
      
      return(
        list(
          h3("Contrôle des écouteurs"),
          HTML("<hr><br>"),
          h4("Test destiné à contrôler la qualité de vos écouteurs."),
          HTML("<br>"),
          HTML("<p><b>Référence</b> : Woods, K. J., Siegel, M. H., Traer, J., & McDermott, J. H. (2017). Headphone screening to facilitate web-based auditory experiments. <i>Attention, Perception, & Psychophysics</i> 79(7), 2064-2072.</p>"),
          p("http://mcdermottlab.mit.edu/"),
          p("https://github.com/mcdermottLab/HeadphoneCheck"),
          HTML("<br>"),
          HTML("<br>"),
          # This displays the action putton Next.
          actionButton(inputId = "gt_inst1",label = "Démarrer !")
        )
      )
    } # end of Welcome Page
    
    # 2) SETTING COMPUTER VOLUME
    if (CurrentValues$page == "inst1")
      {
        
      return(
        list(
          h3("Réglage du volume"),
          HTML("<hr><br>"),
          p("Veuillez placer vos écouteurs sur vos oreilles."),
          HTML("<br>"),
          p("Régler le volume de votre ordinateur à environ 25% du maximum."),
          HTML("<br>"),
          p("Cliquer sur Play, puis augmenter graduellement le volume jusqu'à ce que le bruit de calibration soit à un niveau élevé mais confortable."),
          HTML("<br>"),
          p("Rejouer le bruit de calibration autant de fois que vous le souhaitez."),
          HTML("<br>"),
          tags$audio(src = "noise_calib_stim.wav", type = "audio/wav", controls = NA),
          HTML("<br><br><br><br>"),
          p("Cliquer sur Continuer quand la calibration est achevée."),
          HTML("<br><br>"),
          actionButton(inputId = "gt_inst2",label = "Continuer")
          
        )
      )
      
    } # end of calibration noise page
          
    if (CurrentValues$page == "inst2")
      {
        
        return(
          list(
            h3("Première partie du test"),
            HTML("<hr>"),
            HTML("<p>En cliquant sur Play, vous entendrez une séquence de trois sons.<br>Dites simplement LEQUEL DES TROIS SONS EST LE PLUS FAIBLE -- 1, 2, ou 3 ?<br>Vous ne pouvez écouter chaque séquence qu'une fois !</p>"),
            HTML("<br>"),
            tags$audio(src = "antiphase_HC_OIS.wav", type = "audio/wav", controls = NA),
            radioButtons("stim_1","",choices=c(1,2,3),selected=character(0),inline=TRUE),
            HTML("<br>"),
            tags$audio(src = "antiphase_HC_IOS.wav", type = "audio/wav", controls = NA),
            radioButtons("stim_2","",choices=c(1,2,3),selected=character(0),inline=TRUE),
            HTML("<br>"),
            tags$audio(src = "antiphase_HC_SOI.wav", type = "audio/wav", controls = NA),
            radioButtons("stim_3","",choices=c(1,2,3),selected=character(0),inline=TRUE),
            HTML("<br>"),
            p("Cliquer sur Continuer pour passer à la seconde partie."),
            HTML("<br>"),
            actionButton(inputId = "gt_inst3", label = "Continuer")
          )
          
        )
    } # end of first part

    if (CurrentValues$page == "inst3")
      {
        
      return(
        list(
          h3("Seconde partie du test"),
          HTML("<hr>"),
          tags$audio(src = "antiphase_HC_ISO.wav", type = "audio/wav", controls = NA),
          radioButtons("stim_4","",choices=c(1,2,3),selected=character(0),inline=TRUE),          
          HTML("<br>"),
          tags$audio(src = "antiphase_HC_OSI.wav", type = "audio/wav", controls = NA),
          radioButtons("stim_5","",choices=c(1,2,3),selected=character(0),inline=TRUE),          
          HTML("<br>"),
          tags$audio(src = "antiphase_HC_SIO.wav", type = "audio/wav", controls = NA),
          radioButtons("stim_6","",choices=c(1,2,3),selected=character(0),inline=TRUE),          
          HTML("<br>"),
          p("Cliquer sur Continuer."),
          HTML("<br>"),
          actionButton(inputId = "prepa_questionnaire", label = "Continuer")
          )
          
        )
      } # end of second part
      
    if (CurrentValues$page == "pquestionnaire")
    {
      return(
        list(
          h3("Apprentissage"),
          HTML("<hr>"),
          tags$audio(src = "1.wav", type = "audio/wav", controls = NA),
          HTML("<hr>"),
          tags$img(src="test1.png", type="img/png",controls = NA),
          tags$img(src="test2bis.png", type="img/png",controls = NA),
          tags$img(src="test3.png", type="img/png",controls = NA),
          tags$img(src="test4.png", type="img/png",controls = NA),
          HTML("<hr>"),
          radioButtons("iconschoices","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
          HTML("<br>"),
          conditionalPanel(
            condition =c("input.iconschoices == '1' || input.iconschoices =='2' || input.iconschoices =='3' || input.iconschoices == '4'"),
            actionButton(inputId = "gt_inst4",label = "Étape suivante")
          )
        )
      )
    }
      
    if (CurrentValues$page == "inst4")
      {
        
      return(
        list(
          HTML("<br><br><br>"),
          h4(textOutput("headphone_check")),
          HTML("<br><br><br>"),
          h3("Merci pour votre participation !")
        )
      )
    } # end of test
      
    }
  ) # end of PageLayouts
  
  # (c) Gervasio Marchand, https://g3rv4.com/2017/08/shiny-detect-mobile-browsers
  
  output$currentPing <- renderText({
    invalidateLater(5000,session)
    paste ("ping :",ping(ipadress,count = 1))
  })
  
  output$myBrowserOutput <- renderText({
    input$myBrowser # contains the value returned by the JS function
  })
  
  output$isItMobile <- renderText({
    ifelse(input$isMobile, "Votre appareil est un mobile.", "Votre appareil n'est pas un mobile. Vous pouvez passer à l'étape suivante !")
  })
  
  output$lat <- renderPrint({
    input$lat
  })
  
  output$long <- renderPrint({
    input$long
  })
  
  output$geolocation <- renderPrint({
    input$geolocation
  })
  
  #save datas when clicked on the button next
  observeEvent(input$gt_mobile_detection,{
    saveData(formData())
  })
  
  observeEvent(input$q4, {  #Check whether an input has been made:
    score <- 0
    if (input$q1 == "français.") {
      score <- 1
    }
    if (input$q2 == "J'ai vingt ans.") {
      score <- score + 1
    }
    if (input$q3 == "le médecin.") {
      score <- score + 1
    }
    if (input$q4 == "l'automne.") {
      score <- score + 1
    }
    output$French_level <- renderText(paste("Votre score :",score,"/ 4"))
    }
  )
  observeEvent(input$stim_6, {  #Check whether an input has been made:
    score <- 0
    if (input$stim_1 == 3) {
      score <- 1
    }
    if (input$stim_2 == 3) {
      score <- score + 1
    }
    if (input$stim_3 == 1) {
      score <- score + 1
    }
    if (input$stim_4 == 2) {
      score <- score + 1
    }
    if (input$stim_5 == 2) {
      score <- score + 1
    }
    if (input$stim_6 == 1) {
      score <- score + 1
    }
    output$headphone_check <- renderText(paste("Votre score :",score,"/ 6"))
    }
  )
  
  observeEvent(input$index,{
    CurrentValues$page <- "index"
  })
  
  observeEvent(input$gt_mobile_detection, {
    CurrentValues$page <- "mobile_detection"
  }
  )

  observeEvent(input$gt_assessing_Fr_level, {  #Check whether an input has been made:
    CurrentValues$page <- "assessing_French_level"
  }
  )
  
  observeEvent(input$gt_welcome, {  #Check whether an input has been made:
    CurrentValues$page <- "welcome"
  }
  )
  
  observeEvent(input$gt_inst1, {  #Check whether an input has been made:
      CurrentValues$page <- "inst1"
    }
  )
  
  observeEvent(input$gt_inst2, {  #Check whether an input has been made:
    CurrentValues$page <- "inst2"
    }
  )
  
  observeEvent(input$gt_inst3, {  #Check whether an input has been made:
    CurrentValues$page <- "inst3"
  }
  )
  
  observeEvent(input$prepa_questionnaire,{
    CurrentValues$page <- "pquestionnaire"
  }
               )
  observeEvent(input$gt_inst4, {  #Check whether an input has been made:
    CurrentValues$page <- "inst4"
  }
  )
  
} # end of server

shinyApp(ui = ui, server = server)
