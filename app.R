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
global <- reactiveValues(info  = "public info: I can be seen by everyone", amountUser = 0, userIdToPlay = 1, survey = 0)

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
    tags$head( #script geolocalisation
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
  
  local <- reactiveValues(secret = paste0("My secret number is ", sample(6, 1)))
  
  observe({
    isolate(global$amountUser <-  global$amountUser + 1)
    isolate(local$userId <- global$amountUser)
  })
                                  
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
  # page of geolocalisation
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
           conditionalPanel( #condition for continue on the app
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
          actionButton(inputId = "l_phase", label = "Continuer")
          )
          
        )
      } # end of second part
    if (CurrentValues$page =="learning_phase")
    {
      return(
        list(
          h3("Debut de l'éxperience"),
          HTML("<p>Vous allez devoir mémoriser les sons et images associer ci-dessous et sur les pages suivantes. Faites de votre mieux pour vous rappeller des
               associations le test est prévu dans les pages suivantes.</p>"),
          tags$audio(src = "1.wav", type = "audio/wav", controls = NA),
          tags$img(src="forme1sn.png", type="img/png",controls = NA),
          HTML("<hr>"),
          tags$audio(src = "4.wav", type = "audio/wav", controls = NA),
          tags$img(src="forme2sn.png", type="img/png",controls = NA),
          HTML("<hr>"),
          tags$audio(src = "13.wav", type = "audio/wav", controls = NA),
          tags$img(src="forme3sn.png", type="img/png",controls = NA),
          HTML("<hr>"),
          tags$audio(src = "16.wav", type = "audio/wav", controls = NA),
          tags$img(src="forme4sn.png", type="img/png",controls = NA),
          HTML("<hr>"),
          
          actionButton(inputId = "l_phase2", label = "Continuer")
        )
      )
    }
    if (CurrentValues$page == "learning_phase2")
  {
    return(
      list(
        h5("Ce sont les memes formes et sons mais dans un ordre différent."),
        tags$audio(src = "13.wav", type = "audio/wav", controls = NA),
        tags$img(src="forme3sn.png", type="img/png",controls = NA),
        HTML("<hr>"),
        tags$audio(src = "16.wav", type = "audio/wav", controls = NA),
        tags$img(src="forme4sn.png", type="img/png",controls = NA),
        HTML("<hr>"),
        tags$audio(src = "1.wav", type = "audio/wav", controls = NA),
        tags$img(src="forme1sn.png", type="img/png",controls = NA),
        HTML("<hr>"),
        tags$audio(src = "4.wav", type = "audio/wav", controls = NA),
        tags$img(src="forme2sn.png", type="img/png",controls = NA),
        HTML("<hr>"),
        actionButton(inputId = "l_phase3", label = "Continuer")
      )
    )
    }
      
    if (CurrentValues$page == "learning_phase3")
    {
      return(
        list(
          h5("C'est la dernière partie de reconnaissance des pairs.Vous serez tester sur ce que vous avez retenus ensuite."),
          tags$audio(src = "16.wav", type = "audio/wav", controls = NA),
          tags$img(src="forme4sn.png", type="img/png",controls = NA),
          HTML("<hr>"),
          tags$audio(src = "4.wav", type = "audio/wav", controls = NA),
          tags$img(src="forme2sn.png", type="img/png",controls = NA),
          HTML("<hr>"),
          tags$audio(src = "13.wav", type = "audio/wav", controls = NA),
          tags$img(src="forme3sn.png", type="img/png",controls = NA),
          HTML("<hr>"),
          tags$audio(src = "1.wav", type = "audio/wav", controls = NA),
          tags$img(src="forme1sn.png", type="img/png",controls = NA),
          HTML("<hr>"),
          actionButton(inputId = "prepa1_questionnaire", label = "Continuer")
        )
      )
    }
    if (CurrentValues$page == "p1questionnaire")
    {
      return(
        list(
          h3("Apprentissage"),
          HTML("<hr>"),
          tags$audio(src = "4.wav", type = "audio/wav", controls = NA),
          HTML("<hr>"),
          tags$img(src="test1.png", type="img/png",controls = NA),
          tags$img(src="test2bis.png", type="img/png",controls = NA),
          tags$img(src="test3.png", type="img/png",controls = NA),
          tags$img(src="test4.png", type="img/png",controls = NA),
          HTML("<hr>"),
          radioButtons("iconschoices1","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
          HTML("<br>"),
         # h4(textOutput("ScorePictures")),
          conditionalPanel(
            condition =c("input.iconschoices1 == '1' || input.iconschoices1 =='2' || input.iconschoices1 =='3' || input.iconschoices1 == '4'"),
            actionButton(inputId = "prepa2_questionnaire",label = "Étape suivante")
          )
        )
      )
    }
      
    if (CurrentValues$page == "p2questionnaire")
    {
      return(
        list(
          h3("Apprentissage"),
          HTML("<hr>"),
          tags$audio(src = "16.wav", type = "audio/wav", controls = NA),
          HTML("<hr>"),
          tags$img(src="test1.png", type="img/png",controls = NA),
          tags$img(src="test2bis.png", type="img/png",controls = NA),
          tags$img(src="test3.png", type="img/png",controls = NA),
          tags$img(src="test4.png", type="img/png",controls = NA),
          HTML("<hr>"),
          radioButtons("iconschoices2","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
          HTML("<br>"),
         # h4(textOutput("ScorePictures")),
          conditionalPanel(
            condition =c("input.iconschoices2 == '1' || input.iconschoices2 =='2' || input.iconschoices2 =='3' || input.iconschoices2 == '4'"),
            actionButton(inputId = "prepa3_questionnaire",label = "Étape suivante")
          )
        )
      )
    }
    
    if (CurrentValues$page == "p3questionnaire")
    {
      return(
        list(
          h3("Apprentissage"),
          HTML("<hr>"),
          tags$audio(src = "13.wav", type = "audio/wav", controls = NA),
          HTML("<hr>"),
          tags$img(src="test1.png", type="img/png",controls = NA),
          tags$img(src="test2bis.png", type="img/png",controls = NA),
          tags$img(src="test3.png", type="img/png",controls = NA),
          tags$img(src="test4.png", type="img/png",controls = NA),
          HTML("<hr>"),
          radioButtons("iconschoices3","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
          HTML("<br>"),
          conditionalPanel(
            condition =c("input.iconschoices3 == '1' || input.iconschoices3 =='2' || input.iconschoices3 =='3' || input.iconschoices3 == '4'"),
            actionButton(inputId = "prepa4_questionnaire",label = "Étape suivante")
          )
        )
      )
    }
      
    if (CurrentValues$page == "p4questionnaire")
    {
      return(
        list(
          h3("Apprentissage"),
          HTML("<hr>"),
          tags$audio(src = "16.wav", type = "audio/wav", controls = NA),
          HTML("<hr>"),
          tags$img(src="test1.png", type="img/png",controls = NA),
          tags$img(src="test2bis.png", type="img/png",controls = NA),
          tags$img(src="test3.png", type="img/png",controls = NA),
          tags$img(src="test4.png", type="img/png",controls = NA),
          HTML("<hr>"),
          radioButtons("iconschoices4","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
          HTML("<br>"),
          conditionalPanel(
            condition =c("input.iconschoices4 == '1' || input.iconschoices4 =='2' || input.iconschoices4 =='3' || input.iconschoices4 == '4'"),
            actionButton(inputId = "prepa5_questionnaire",label = "Étape suivante")
          )
        )
      )
    }
    if (CurrentValues$page == "p5questionnaire")
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
          radioButtons("iconschoices5","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
          HTML("<br>"),
          conditionalPanel(
            condition =c("input.iconschoices5 == '1' || input.iconschoices5 =='2' || input.iconschoices5 =='3' || input.iconschoices5 == '4'"),
            actionButton(inputId = "prepa6_questionnaire",label = "Étape suivante")
        )
      )
    )
    }
      if (CurrentValues$page == "p6questionnaire")
      {
        return(
          list(
            h3("Apprentissage"),
            HTML("<hr>"),
            tags$audio(src = "13.wav", type = "audio/wav", controls = NA),
            HTML("<hr>"),
            tags$img(src="test1.png", type="img/png",controls = NA),
            tags$img(src="test2bis.png", type="img/png",controls = NA),
            tags$img(src="test3.png", type="img/png",controls = NA),
            tags$img(src="test4.png", type="img/png",controls = NA),
            HTML("<hr>"),
            radioButtons("iconschoices6","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
            HTML("<br>"),
            conditionalPanel(
              condition =c("input.iconschoices6 == '1' || input.iconschoices6 =='2' || input.iconschoices6 =='3' || input.iconschoices6 == '4'"),
              actionButton(inputId = "gt_inst4",label = "Étape suivante")
            )
          )
        )
      }
      
    if (CurrentValues$page == "inst4")
      {
        
      return(
        list(
          h3("Merci pour votre participation à la phase d'apprentissage!"),
          HTML("<br><br><br>"),
          h4(textOutput("headphone_check")),
          h4(textOutput("ScorePictures")),
          h4(textOutput("ScorePictures2")),
          h4(textOutput("ScorePictures3")),
          HTML("<br><br><br>"),
          conditionalPanel( # a voir la condition ne semble pas prise en compte
            condition = "output.ScorePictures",
            actionButton(inputId = "test_multi",label = "Étape suivante")
          ),
          conditionalPanel(
            condition ="output.ScorePictures3",
            actionButton(inputId = "LastChance_questionnaire1", label ="Étape suivante")
          )
          
        )
      )
    } # end of test
    if (CurrentValues$page == "LastChancequestionnaire1")
    {
      return(
        list(
          h3("Dernière chance "),
          HTML("<hr>"),
          tags$audio(src = "1.wav", type = "audio/wav", controls = NA),
          HTML("<hr>"),
          tags$img(src="test1.png", type="img/png",controls = NA),
          tags$img(src="test2bis.png", type="img/png",controls = NA),
          tags$img(src="test3.png", type="img/png",controls = NA),
          tags$img(src="test4.png", type="img/png",controls = NA),
          HTML("<hr>"),
          radioButtons("LCiconschoices1","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
          HTML("<br>"),
          conditionalPanel(
            condition =c("input.LCiconschoices1 == '1' || input.LCiconschoices1 =='2' || input.LCiconschoices1 =='3' || input.LCiconschoices1 == '4'"),
            actionButton(inputId = "LastChance_questionnaire2",label = "Étape suivante")
          )
          
        )
      )
    }
    if (CurrentValues$page == "LastChancequestionnaire2") 
    {
     return(
       list(
         h3("Dernière chance "),
         HTML("<hr>"),
         tags$audio(src = "16.wav", type = "audio/wav", controls = NA),
         HTML("<hr>"),
         tags$img(src="test1.png", type="img/png",controls = NA),
         tags$img(src="test2bis.png", type="img/png",controls = NA),
         tags$img(src="test3.png", type="img/png",controls = NA),
         tags$img(src="test4.png", type="img/png",controls = NA),
         HTML("<hr>"),
         radioButtons("LCiconschoices2","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
         HTML("<br>"),
         conditionalPanel(
           condition =c("input.LCiconschoices2 == '1' || input.LCiconschoices2 =='2' || input.LCiconschoices2 =='3' || input.LCiconschoices2 == '4'"),
           actionButton(inputId = "LastChance_questionnaire3",label = "Étape suivante")
         )
         
       )
     )
    }
    if (CurrentValues$page == "LastChancequestionnaire3")  
    {
     return(
       list(
         h3("Dernière chance "),
         HTML("<hr>"),
         tags$audio(src = "4.wav", type = "audio/wav", controls = NA),
         HTML("<hr>"),
         tags$img(src="test1.png", type="img/png",controls = NA),
         tags$img(src="test2bis.png", type="img/png",controls = NA),
         tags$img(src="test3.png", type="img/png",controls = NA),
         tags$img(src="test4.png", type="img/png",controls = NA),
         HTML("<hr>"),
         radioButtons("LCiconschoices3","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
         HTML("<br>"),
         conditionalPanel(
           condition =c("input.LCiconschoices3 == '1' || input.LCiconschoices3 =='2' || input.LCiconschoices3 =='3' || input.LCiconschoices3 == '4'"),
           actionButton(inputId = "LastChance_questionnaire4",label = "Étape suivante")
         ) 
       )
     )
    }
    if(CurrentValues$page == "LastChancequestionnaire4")
    {
      return(
        list(
          h3("Dernière chance "),
          HTML("<hr>"),
          tags$audio(src = "13.wav", type = "audio/wav", controls = NA),
          HTML("<hr>"),
          tags$img(src="test1.png", type="img/png",controls = NA),
          tags$img(src="test2bis.png", type="img/png",controls = NA),
          tags$img(src="test3.png", type="img/png",controls = NA),
          tags$img(src="test4.png", type="img/png",controls = NA),
          HTML("<hr>"),
          radioButtons("LCiconschoices4","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
          HTML("<br>"),
          conditionalPanel(
            condition =c("input.LCiconschoices4 == '1' || input.LCiconschoices4 =='2' || input.LCiconschoices4 =='3' || input.LCiconschoices4 == '4'"),
            actionButton(inputId = "gt_inst5",label = "Étape suivante")
          )
        )
      )
    }
    
      if(CurrentValues$page == "inst5")  
        return(
          list(
            h3("Merci pour votre participation à la phase de rattrapage!"),
            HTML("<br><br><br>"),
            h4(textOutput("ScorePictures2")),
            h4(textOutput("ScorePictures4")),
            conditionalPanel(
              condition ="output.ScorePictures4",
              actionButton(inputId = "test_multi", label ="Étape suivante")
            )
          )
        )
  
      
    if (CurrentValues$page == "testmulti")
    {
      return(
        list(
          uiOutput("moreControls")
        )
      )
    }
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
  scoredata2 <- 0
  observeEvent(input$gt_inst5, {
    if (input$LCiconschoices1 == "1"){
      scoredata2 <- scoredata2 + 1
    }
    if (input$LCiconschoices2 == "4"){
      scoredata2 <- scoredata2 + 1
    }
    if (input$LCiconschoices3 == "2"){
      scoredata2 <- scoredata2 + 1
    }
    if (input$LCiconschoices4 == "3"){
      scoredata2 <- scoredata2 + 1
    }
    if (scoredata2 == 4){
      output$ScorePictures4 <- renderText(paste("Votre score est de :",scoredata2,"/ 4. Félicitations. Vous pouvez continuer le test "))  
    }
    else
      output$ScorePictures2 <- renderText(paste("Merci de votre participation.Malheureusement,votre score est trop faible pour pouvoir continuer."))
  }
               )
  #score for the first survey
  
  #observeEvent(input$finish, {
   # enterfinish <- 0
    #enterfinish <- enterfinish + 1
    #enterfinish2 <- enterfinish + 1
   # output$enterfinish2 <- renderText(paste("ceci est le nombre de fois ou vous avez appuyer sur finish:",enterfinish))  
   # })
  
  scoredata1 <- 0
  observeEvent(input$gt_inst4, {
    if (input$iconschoices1 == "2"){
      
      scoredata1 <- scoredata1 + 1
    }
    if (input$iconschoices2 == "4"){
      scoredata1 <- scoredata1 + 1
    }
    if (input$iconschoices3 == "3"){
      scoredata1 <- scoredata1 + 1
    }
    if (input$iconschoices4 == "4"){
      scoredata1 <- scoredata1 + 1
    }
    if (input$iconschoices5 == "1"){
      scoredata1 <- scoredata1 + 1
    }
    if (input$iconschoices6 == "3"){
      scoredata1 <- scoredata1 + 1
    }
    if (scoredata1 == 5 ){
    output$ScorePictures <- renderText(paste("Votre score est de :",scoredata1,"/ 6. Felicitation vous pouvez continuer en appuyant sur le bouton suivant."))
    
    }
    if (scoredata1 == 6 ){
      output$ScorePictures <- renderText(paste("Votre score est de :",scoredata1,"/ 6. Felicitation vous pouvez continuer en appuyant sur le bouton suivant."))
      
    }
    if (scoredata1 == 4){
    output$ScorePictures3 <- renderText(paste("Votre score est de :",scoredata1,"/ 6. Vous allez devoir procéder à un autre test.Il faut que vous ayez tout juste pour pouvoir continuer ne lacher rien.Bonne chance."))  
    }
    else if (scoredata1 < 4){
    output$ScorePictures2 <- renderText(paste("Merci de votre participation.Malheureusement,votre score est trop faible pour pouvoir continuer."))
    }
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
  observeEvent(input$finish8,{
    scoreduo <- 0
    if (input$DuoSurvey1J2 == 3) {
      scoreduo <- scoreduo + 1
    }
    if (input$DuoSurvey2J2 == 3) {
      
      scoreduo <- scoreduo + 1
    }
    if (input$DuoSurvey3J2 == 3 ) {
      scoreduo <- scoreduo + 1
    }
    if (input$DuoSurvey4J2 == 3 ) {
      scoreduo <- scoreduo + 1
    }
    output$FinalScoreDuo <- renderText(paste("Votre score en duo est de :",scoreduo,"/8"))
    
  })
  observeEvent(input$finish,{
    global$survey <- 1
    global$userIdToPlay <- 3 - global$userIdToPlay # assumes two players (for MVE)
  })
  
  observeEvent(input$finish2,{
    global$survey <- 2
    global$userIdToPlay <- 3 - global$userIdToPlay # assumes two players (for MVE)
  })
  observeEvent(input$finish3,{
    global$survey <- 3
    global$userIdToPlay <- 3 - global$userIdToPlay # assumes two players (for MVE)
  })
  observeEvent(input$finish4,{
    global$survey <- 4
    global$userIdToPlay <- 3 - global$userIdToPlay # assumes two players (for MVE)
  })
  observeEvent(input$finish5,{
    global$survey <- 5
    global$userIdToPlay <- 3 - global$userIdToPlay # assumes two players (for MVE)
  })
  observeEvent(input$finish6,{
    global$survey <- 6
    global$userIdToPlay <- 3 - global$userIdToPlay # assumes two players (for MVE)
  })
  observeEvent(input$finish7,{
    global$survey <- 7
    global$userIdToPlay <- 3 - global$userIdToPlay # assumes two players (for MVE)
  })
  observeEvent(input$finish8,{
    global$survey <- 8
    global$userIdToPlay <- 3 - global$userIdToPlay # assumes two players (for MVE)
    output$End <- renderText(paste("Merci d'avoir rempli le test vous pouvez sortir avec le bouton suivant."))
  })
  
  output$moreControls <- renderUI({
    global$userIdToPlay
    isolate({
      if(local$userId == global$userIdToPlay && global$survey == 0){
        return(
          tagList(
            h2("A votre tour de jouer.Veuillez choisir la figure correspondant au son suivant:"),
            tags$audio(src = "13.wav", type = "audio/wav", controls = NA),
            HTML("<hr>"),
            tags$img(src="test1.png", type="img/png",controls = NA),
            tags$img(src="test2bis.png", type="img/png",controls = NA),
            tags$img(src="test3.png", type="img/png",controls = NA),
            tags$img(src="test4.png", type="img/png",controls = NA),
            HTML("<hr>"),
            radioButtons("Duosurvey1J1","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
            HTML("<br>"),
            #h4(textOutput("enterfinish2")),
            #selectInput("a", "b", letters),
            conditionalPanel(
              condition =c("input.Duosurvey1J1 == '1' || input.Duosurvey1J1 =='2' || input.Duosurvey1J1 =='3' || input.Duosurvey1J1 == '4'"),
              actionButton("finish", "Valider J1Q1")
            )
            
            
          )
        )
      }else if(local$userId == global$userIdToPlay && global$survey == 1){
        return(
          tagList(
            h2("A votre tour de jouer.Veuillez choisir la figure correspondant au son suivant:"),
            tags$audio(src = "13.wav", type = "audio/wav", controls = NA),
            HTML("<hr>"),
            tags$img(src="test1.png", type="img/png",controls = NA),
            tags$img(src="test2bis.png", type="img/png",controls = NA),
            tags$img(src="test3.png", type="img/png",controls = NA),
            tags$img(src="test4.png", type="img/png",controls = NA),
            HTML("<hr>"),
            radioButtons("DuoSurvey1J2","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
            HTML("<br>"),
            #h4(textOutput("enterfinish2")),
            #selectInput("a", "b", letters),
            conditionalPanel(
              condition =c("input.Duosurvey1J2 == '1' || input.Duosurvey1J2 =='2' || input.Duosurvey1J2 =='3' || input.Duosurvey1J2 == '4'"),
              actionButton("finish2", "Valider J2Q1")
            )
          )
        )
      }else if(local$userId == global$userIdToPlay && global$survey == 2){
        return(
          tagList(
            h2("A votre tour de jouer.Veuillez choisir la figure correspondant au son suivant:"),
            tags$audio(src = "13.wav", type = "audio/wav", controls = NA),
            HTML("<hr>"),
            tags$img(src="test4.png", type="img/png",controls = NA),
            tags$img(src="test4.png", type="img/png",controls = NA),
            tags$img(src="test4.png", type="img/png",controls = NA),
            tags$img(src="test4.png", type="img/png",controls = NA),
            HTML("<hr>"),
            radioButtons("DuoSurvey2J1","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
            HTML("<br>"),
            #h4(textOutput("enterfinish2")),
            #selectInput("a", "b", letters),
            conditionalPanel(
              condition =c("input.DuoSurvey2J1 == '1' || input.DuoSurvey2J1 =='2' || input.DuoSurvey2J1 =='3' || input.DuoSurvey2J1 == '4'"),
              actionButton("finish3", "Valider J1Q2")
            )
            
            
            
          )
        )
      }else if(local$userId == global$userIdToPlay && global$survey == 3){
        return(
          tagList(
            h2("A votre tour de jouer.Veuillez choisir la figure correspondant au son suivant:"),
            tags$audio(src = "13.wav", type = "audio/wav", controls = NA),
            HTML("<hr>"),
            tags$img(src="test4.png", type="img/png",controls = NA),
            tags$img(src="test4.png", type="img/png",controls = NA),
            tags$img(src="test4.png", type="img/png",controls = NA),
            tags$img(src="test4.png", type="img/png",controls = NA),
            HTML("<hr>"),
            radioButtons("DuoSurvey2J2","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
            HTML("<br>"),
            #h4(textOutput("enterfinish2")),
            #selectInput("a", "b", letters),
            conditionalPanel(
              condition =c("input.DuoSurvey2J2 == '1' || input.DuoSurvey2J2 =='2' || input.DuoSurvey2J2 =='3' || input.DuoSurvey2J2 == '4'"),
              actionButton("finish4", "Valider J2Q2")
            )
            
            
          )
        )
      }else if(local$userId == global$userIdToPlay && global$survey == 4){
        return(
          tagList(
            h2("A votre tour de jouer.Veuillez choisir la figure correspondant au son suivant:"),
            tags$audio(src = "13.wav", type = "audio/wav", controls = NA),
            HTML("<hr>"),
            tags$img(src="test3.png", type="img/png",controls = NA),
            tags$img(src="test3.png", type="img/png",controls = NA),
            tags$img(src="test3.png", type="img/png",controls = NA),
            tags$img(src="test3.png", type="img/png",controls = NA),
            HTML("<hr>"),
            radioButtons("DuoSurvey3J1","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
            HTML("<br>"),
            #h4(textOutput("enterfinish2")),
            #selectInput("a", "b", letters),
            conditionalPanel(
              condition =c("input.DuoSurvey3J1 == '1' || input.DuoSurvey3J1 =='2' || input.DuoSurvey3J1 =='3' || input.DuoSurvey3J1 == '4'"),
              actionButton("finish5", "Valider J1Q3")
            )
            
            
            
          )
        )
      }else if(local$userId == global$userIdToPlay && global$survey == 5){
        return(
          tagList(
            h2("A votre tour de jouer.Veuillez choisir la figure correspondant au son suivant:"),
            tags$audio(src = "13.wav", type = "audio/wav", controls = NA),
            HTML("<hr>"),
            tags$img(src="test3.png", type="img/png",controls = NA),
            tags$img(src="test3.png", type="img/png",controls = NA),
            tags$img(src="test3.png", type="img/png",controls = NA),
            tags$img(src="test3.png", type="img/png",controls = NA),
            HTML("<hr>"),
            radioButtons("DuoSurvey3J2","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
            HTML("<br>"),
            #h4(textOutput("enterfinish2")),
            #selectInput("a", "b", letters),
            conditionalPanel(
              condition =c("input.DuoSurvey3J2 == '1' || input.DuoSurvey3J2 =='2' || input.DuoSurvey3J2 =='3' || input.DuoSurvey3J2 == '4'"),
              actionButton("finish6", "Valider J2Q3")
            )
            
            
          )
        )
      }else if(local$userId == global$userIdToPlay && global$survey == 6){
        return(
          tagList(
            h2("A votre tour de jouer.Veuillez choisir la figure correspondant au son suivant:"),
            tags$audio(src = "13.wav", type = "audio/wav", controls = NA),
            HTML("<hr>"),
            tags$img(src="test1.png", type="img/png",controls = NA),
            tags$img(src="test1.png", type="img/png",controls = NA),
            tags$img(src="test1.png", type="img/png",controls = NA),
            tags$img(src="test1.png", type="img/png",controls = NA),
            HTML("<hr>"),
            radioButtons("DuoSurvey4J1","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
            HTML("<br>"),
            #h4(textOutput("enterfinish2")),
            #selectInput("a", "b", letters),
            conditionalPanel(
              condition =c("input.DuoSurvey4J1 == '1' || input.DuoSurvey4J1 =='2' || input.DuoSurvey4J1 =='3' || input.DuoSurvey4J1 == '4'"),
              actionButton("finish7", "Valider J1Q4")
            )
            
          )
        )
      }else if(local$userId == global$userIdToPlay && global$survey == 7){
        return(
          tagList(
            h2("A votre tour de jouer.Veuillez choisir la figure correspondant au son suivant:"),
            HTML("<hr>"),
            tags$audio(src = "13.wav", type = "audio/wav", controls = NA),
            HTML("<hr>"),
            tags$img(src="test1.png", type="img/png",controls = NA),
            tags$img(src="test1.png", type="img/png",controls = NA),
            tags$img(src="test1.png", type="img/png",controls = NA),
            tags$img(src="test1.png", type="img/png",controls = NA),
            HTML("<hr>"),
            radioButtons("DuoSurvey4J2","",choices=c(1,2,3,4),selected=character(0),inline=TRUE),
            HTML("<br>"),
            #h4(textOutput("enterfinish2")),
            #selectInput("a", "b", letters),
            conditionalPanel(
              condition =c("input.DuoSurvey4J2 == '1' || input.DuoSurvey4J2 =='2' || input.DuoSurvey4J2 =='3' || input.DuoSurvey4J2 == '4'"),
              actionButton("finish8", "Valider J2Q4")
            )
            
            
            
          )
        )
      }else if(local$userId == global$userIdToPlay && global$survey == 8){
        return(
          tagList(
            h2("Bravo"),
            HTML("<hr>"),
            h4(textOutput("FinalScoreDuo")),
            HTML("<hr>"),
            h4(" Merci d'avoir rempli ces formulaires vous avez maintenant terminer le test."),
            actionButton(inputId = "test_multi", label ="Étape suivante")
            
          )
        )
      }else{
        return(
          list(
          h2("Au tour de l'autre utilisateur de répondre.Veuillez patienter"),
          HTML("<hr>"),
          #h4(textOutput("enterfinish2")),
          h4(textOutput("FinalScoreDuo")),
          HTML("<hr>"),
          h4(textOutput("End")),
         # actionButton(inputId = "gt_inst4",label = "Étape suivante"),
          conditionalPanel(
            condition ="output.End",
            actionButton(inputId = "test_multi", label ="Étape suivante")
          )
          )
        )
      }
    })
  })
  
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
  
  observeEvent(input$l_phase,  {
    CurrentValues$page <- "learning_phase"
    }
    )
  observeEvent(input$l_phase2,  {
    CurrentValues$page <- "learning_phase2"
  }
  )
  observeEvent(input$l_phase3,  {
    CurrentValues$page <- "learning_phase3"
  }
  )
  observeEvent(input$prepa1_questionnaire,{
    CurrentValues$page <- "p1questionnaire"
  }
               )
  observeEvent(input$prepa2_questionnaire,{
    CurrentValues$page <- "p2questionnaire"
  }
  )
  observeEvent(input$prepa3_questionnaire,{
    CurrentValues$page <- "p3questionnaire"
  }
  )
  observeEvent(input$prepa4_questionnaire,{
    CurrentValues$page <- "p4questionnaire"
  }
  )
  observeEvent(input$prepa5_questionnaire,{
    CurrentValues$page <- "p5questionnaire"
  }
  )
  observeEvent(input$prepa6_questionnaire,{
    CurrentValues$page <- "p6questionnaire"
  }
  )
  observeEvent(input$LastChance_questionnaire1,{
    CurrentValues$page <- "LastChancequestionnaire1"
  }
  )
  observeEvent(input$LastChance_questionnaire2,{
    CurrentValues$page <- "LastChancequestionnaire2"
  }
  )
  observeEvent(input$LastChance_questionnaire3,{
    CurrentValues$page <- "LastChancequestionnaire3"
  }
  )
  observeEvent(input$LastChance_questionnaire4,{
    CurrentValues$page <- "LastChancequestionnaire4"
  }
  )
  observeEvent(input$gt_inst5, {  #Check whether an input has been made:
    CurrentValues$page <- "inst5"
  }
  )
  observeEvent(input$gt_inst4, {  #Check whether an input has been made:
    CurrentValues$page <- "inst4"
  }
  )
  observeEvent(input$test_multi, {  #Check whether an input has been made:
    CurrentValues$page <- "testmulti"
  }
  )
  
} # end of server

shinyApp(ui = ui, server = server)
