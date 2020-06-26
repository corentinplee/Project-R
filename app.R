library(shiny)
library(shinythemes)
library(RMySQL)
library(pingr)
library(shinyjs)

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
trueanswer <- reactiveValues(correctAnswer = 0,correctAnswer2 = 0,correctAnswer3 = 0,correctAnswer4 = 0,correctAnswer5 = 0,correctAnswer6 = 0,correctAnswer7 = 0,correctAnswer8 = 0,correctAnswer9 = 0,correctAnswer10 = 0,correctAnswer11 = 0,correctAnswer12 = 0,correctAnswer13 = 0)
globalscore <- reactiveValues(duoscore = 0, duoscore2 = 0)
global <- reactiveValues(info  = "public info: I can be seen by everyone", amountUser = 0)
user<- reactiveValues(ready = 0)

#informations on database
options(mysql = list(
  "host" = "mysql-projetr.alwaysdata.net",
  "user" = "projetr",
  "password" = "PQ2Jsx;6pQ4o"
))

databaseName <- "projetr_bdd"
epochTime <- function(){
  as.integer(Sys.time())
}

countries.list <- read.table("country.txt", header =FALSE,stringsAsFactors = FALSE,quote = "", col.names = "countryname")
choice.country <- as.list(countries.list$countryname)

# informations upload to database
fieldsAll <- c("age","gender","nativeLand","language","impairmentEars","impairmentEyes","impairmentEyes2","country","computer","connection","browser","earphones","phase_choice","phase_choicebis","phase_choice2","phase_choice2bis","phase_choice3","phase_choice3bis",
               "phase_choice4","phase_choice4bis","phase_choice5","phase_choice5bis","phase_choice6","phase_choice6bis")

ipadress <- my_ip()

ui <- fluidPage(#theme=shinytheme("slate"),
  headerPanel(
    h6(textOutput("currentPing"),align ="right", display = "inline"),"app"),
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
  #) # end of sidebarLayout
  
) # end of fluidPage

server <- function(input, output, session) {
  
  #modify?
  local <- reactiveValues(secret = paste0("My secret number is ", sample(6, 1)))
  
  observe({
    isolate(global$amountUser <-  global$amountUser + 1)
    isolate(local$userId <- global$amountUser)
  })
  
  #create an object for storing reactive values
  CurrentValues <- reactiveValues(page = "welcome")
  
  # Send dynamic UI to ui - DON'T CHANGE!
  output$MainAction <- renderUI({
    PageLayouts()
  })
  # Wraps a normal expression to create a reactive expression.
  # A reactive expression is an expression whose result will change over time.
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
      if (CurrentValues$page == "welcome")
      {
        return(
          list(
            includeCSS("www/stylewelcomesalut.css"),
            actionButton(inputId = "prepa1_questionnaire",label = "Aller directement au duo"),
            h1("Deux paires d'oreilles valent mieux qu'une!"),
            h2("Jouez à relier sons et images avec votre partenaire."),
            HTML("<br>"),
            HTML("<br>"),
            p("Dans ce jeu, vous allez devoir faire correspondre des sons de parole, avec des images. Vous accomplirez ce jeu avec une autre personne
               qui est également connectée, et qui sera votre partenaire. L'objectif sera de gagner des points avec votre partenaire en répondant de
               manière identique à elle ou lui!"),
            p("Dans un premier temps nous allons vous apprendre quatre correspondances de base entre sons et images.Lorsque cela sera fait,vous
               pourrez commencer à jouer à deux!"),
            HTML("<br>"),
            h4("Vous ne pouvez participer à ce jeu qu'une fois."),
            radioButtons("participation","Est-ce votre première participation?",choices=c("Oui","Non"),selected=character(0),inline=FALSE),
            conditionalPanel(
              condition ="input.participation == 'Oui'",
              actionButton(inputId = "after_Welcome",label = "Étape suivante")
            ),
            #diriger vers une page de deco?
            #voir pour les id qu'il ne prenne pas le 1 sinon le 2 ne peut jouer ou faire descendre le numid general de 1 quand il quitte
            conditionalPanel(
              condition ="input.participation == 'Non'",
              h3("Vous ne pouvez participer à ce jeu qu'une fois.Nous vous remercions.")
            )
          )
        )
      }
      if (CurrentValues$page == "afterWelcome")
      {
        return(
          list(
            includeCSS("www/styleafterWelcome.css"),
            h1("Deux paires d'oreilles valent mieux qu'une!"),
            HTML("<br>"),
            HTML("<br>"),
            h4("Pour passer cette expérience, vous devez être de langue maternelle française, et ne pas avoir de trouble connu de l’audition.
               Vous devez également avoir une vision normale ou bien porter des verres correcteurs."),
            h4("Vous devez passer cette expérience à partir d’un ordinateur (et non d’une tablette ou d’un smartphone),
            dans une pièce calme, à l’écart de votre entourage. Il est préférable que l’ordinateur soit relié à internet par une connexion filaire plutôt qu’en wifi."),
            h4("Vous devez porter sur les oreilles des écouteurs reliés à la prise casque de votre ordinateur."),
            h3("Remplissez-vous bien ces différentes conditions ? Nous allons à présent le vérifier avec vous."),
            HTML("<br>"),
            actionButton(inputId = "waiting_Room",label = "Étape suivante")
            
          )
        )
      }
      if (CurrentValues$page == "waitingRoom")
      {
        return(
          list(
            includeCSS("www/stylewaitingRoom.css"),
            sidebarLayout(
              sidebarPanel(
                h2("Salle d'attente"),
                HTML("<hr>"),
                h4(textOutput("player1")),
                h4(textOutput("player2")),
                HTML("<br>"),
                h4(textOutput("playersFull")),
                h4(textOutput("numberOfUsers"))
              ),
              mainPanel(
                h3("Veuillez patienter jusqu'a ce que vous soyez 2 dans la salle d'attente. Le nombre d'utilisateur dans la salle est indiqué dans la partie gauche de l'écran."),
                actionButton(inputId = "test_Geo",label = "Continuer"),
                #conditionalPanel(
                #  condition = "output.player1",
                #  actionButton(inputId = "checkingButton1",label="Faites part de votre arriver en appuyant ici!")
                #),
                #conditionalPanel(
                #  condition = "output.player2",
                #  actionButton(inputId = "checkingButton2",label="Faites part de votre arriver en appuyant ici!")
                #),
                #h4(textOutput("Only1user")),
                #h4(textOutput("Only2user")),
                #conditionalPanel( #condition for continue on the app
                #  condition = "output.Only2user",
                #  actionButton(inputId = "test_Geo",label = "Étape suivante")
                #),
              )
            )
          )
        )
      }
      
      if (CurrentValues$page == "testGeo")
      {
        return(
          list(
            h3("Nous avons besoin d’établir vos coordonnées GPS.
               Cela vise à nous permettre d’établir la distribution géographique de nos participants."),
            HTML("<br>"),
            h4("Acceptez-vous d’être géolocalisé(e) ?"),
            radioButtons("acceptationOfLocalisation","Dans le cas contraire, il ne vous sera pas proposé de poursuivre l’expérience.",choices=c("Oui","Non"),selected=character(0),inline=FALSE),
            conditionalPanel(
              condition ="input.acceptationOfLocalisation == 'Oui'",
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
              #conditionalPanel( #condition for continue on the app
              #  condition =c("input.geolocation == true"),
              #  HTML("<p1> Vous pouvez continuer : </p1>")
              #),    
              
              
              actionButton(inputId = "index",label = "Étape suivante")
            ),
            #diriger vers une page de deco?
            #voir pour les id qu'il ne prenne pas le 1 sinon le 2 ne peut jouer ou faire descendre le numid general de 1 quand il quitte
            conditionalPanel(
              condition ="input.acceptationOfLocalisation == 'Non'",
              h3("« Vous ne souhaitez pas être géolocalisé(). L’expérience est annulée. Nous vous remercions. ")
            )   
            
          )
        )
      }
      if (CurrentValues$page == "index")
      {
        return(
          list(
            includeCSS("www/styleindex.css"),
            HTML("<h1>Page d'identification</h1>"),
            numericInput('age','Indiquez votre âge','',min= 1, max= 120),
            conditionalPanel(
              condition = "input.age",
              radioButtons("gender","Quel est votre sexe? ",choices=c("Homme","Femme","Neutre"),selected=character(0),inline=TRUE),
            ),
            conditionalPanel(
              condition = "input.gender",
              selectizeInput("nativeLand","Dans quel pays êtes-vous né(e)?",choices= choice.country),
            ),
            conditionalPanel(
              condition = "input.nativeLand && input.gender",
              radioButtons("language","La langue française est-elle votre langue maternelle?",choices=c("oui","non"),selected=character(0),inline=FALSE),
            ),
            conditionalPanel(
              condition = "input.language",
              radioButtons("impairmentEars","Votre audition est-elle normale ou inférieure à la normale ?",choices=c("Normale","Inférieure à la normale"),selected=character(0),inline=FALSE),
            ),
            conditionalPanel(
              condition ="input.impairmentEars == 'Inférieure à la normale'",
              h3("Pour passer cette expérience, il est nécessaire d’avoir une audition normale. Nous vous remercions.")
            ),
            conditionalPanel(
              condition = "input.impairmentEars == 'Normale'",
              radioButtons("impairmentEyes","Votre vision non corrigée est-elle normale ou inférieure à la normale ?",choices=c("Normale","Inférieure à la normale"),selected=character(0),inline=FALSE),
            ),
            conditionalPanel(
              condition ="input.impairmentEyes == 'Inférieure à la normale' ",
              radioButtons("impairmentEyes2","Si votre vision non corrigée est inférieure à la normale, portez-vous vos lunettes ou lentilles correctrices ?",choices=c("Oui","Non"),selected=character(0),inline=FALSE),
            ),
            conditionalPanel(
              condition ="input.impairmentEyes2 == 'Non' ",
              h3("Pour passer cette expérience, il est nécessaire d’avoir une vision normale ou de porter des verres correcteurs.
                 Nous vous remercions. ")
            ),
            conditionalPanel(
              condition = "input.impairmentEyes == 'Normale' || input.impairmentEyes2 == 'Oui'",
              selectizeInput("country","Dans quel pays réalisez-vous ce test?",choices= choice.country),
            ),
            conditionalPanel(
              condition = "input.country && input.impairmentEyes == 'Normale' || input.impairmentEyes2 == 'Oui'",
              h5("Cette expérience doit être passée sur ordinateur (et non sur une tablette ou un smartphone),
               dans une pièce calme et à l'écart de votre entourage."),
              radioButtons("computer","Êtes-vous sur ordinateur ?",choices=c("Oui","Non"),selected=character(0),inline=FALSE),
            ),
            conditionalPanel(
              condition ="input.computer == 'Non' ",
              h3("Cette expérience doit être passée sur ordinateur. Nous vous remercions.")
            ),
            conditionalPanel(
              condition = "input.computer == 'Oui'",
              h5("Il est préférable que votre ordinateur soit relié à internet par une connexion filaire (plutôt que par wifi)."),
              radioButtons("connection","Votre connexion est-elle de type filaire ou wifi ?",choices=c("Filaire","Wifi"),selected=character(0),inline=TRUE),
            ),
            conditionalPanel(
              condition = "input.connection",
              textInput('browser','Indiquez votre navigateur et sa version'),
              HTML("<p>Si vous ne connaissez pas la réponse, voici celle que nous vous proposons : </p>"),
              textOutput("myBrowserOutput"),
            ),
            conditionalPanel(
              condition = "input.browser",
              h5("Pour passer cette expérience, vous devez porter des écouteurs reliés à la prise casque de votre ordinateur."),
              radioButtons("earphones","Avez-vous placé ces écouteurs sur vos oreilles ?",choices=c("Oui","Non"),selected=character(0),inline=FALSE),
            ),
            conditionalPanel(
              condition =c("input.earphones == 'Non'"),
              h3("Pour passer cette expérience, vous devez porter des écouteurs. Nous vous remercions. ")
            ),
            HTML("<br>"),
            conditionalPanel(
              condition =c("input.earphones == 'Oui'"),
              actionButton(inputId = "gt_mobile_detection",label = "Étape suivante")
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
            actionButton(inputId = "earphones_Quality",label = "Étape suivante")          
          )
        )
      }
      
      # 1) WELCOME PAGE
      if (CurrentValues$page == "earphonesQuality")
      {
        
        return(
          list(
            h3("Contrôle des écouteurs"),
            HTML("<hr><br>"),
            h4("Nous allons à présent contrôler la qualité de vos écouteurs. Assurez-vous qu’ils sont bien connectés à la prise haut-parleur de votre ordinateur. "),
            HTML("<br>"),
            HTML("<br>"),
            HTML("<br>"),
            # This displays the action putton Next.
            actionButton(inputId = "gt_inst1",label = "Démarrer !"),
            #add html footer in a file
            HTML("<br><br><br><br><br><br><br><br><br><br><br><br>"),
            HTML("<br>"),
            HTML("<br>"),
            HTML("<p><b>Référence</b> : Woods, K. J., Siegel, M. H., Traer, J., & McDermott, J. H. (2017). Headphone screening to facilitate web-based auditory experiments. <i>Attention, Perception, & Psychophysics</i> 79(7), 2064-2072.</p>")
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
            actionButton(inputId = "index_test",label = "Continuer")
            
          )
        )
        
      } # end of calibration noise page
      if (CurrentValues$page =="itest")
      {
        return(
          list(
            h4("Vous allez entendre trois séquences de trois sons chacune."),
            HTML("<br>"),
            h4("Pour chaque séquence, dites lequel des trois sons est le plus faible – 1, 2, ou 3 ?"),
            HTML("<br>"),
            h4("Attention, tendez-bien l’oreille, vous n’entendrez chaque séquence qu’une fois ! "),
            HTML("<br>"),
            actionButton(inputId = "index_test1",label = "Continuer")
          )
        )
      }
      if (CurrentValues$page == "test1")
      {
        return(
          list(
            tags$audio(src = "antiphase_HC_OIS.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            radioButtons("stim_1","",choices=c(1,2,3),selected=character(0),inline=TRUE),
            HTML("<br>"),  
            actionButton(inputId = "index_test2",label = "Continuer")
            
          )
        )
      }
      if (CurrentValues$page == "test2")
      {
        return(
          list(
            tags$audio(src = "antiphase_HC_IOS.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            radioButtons("stim_2","",choices=c(1,2,3),selected=character(0),inline=TRUE),
            HTML("<br>"),  
            actionButton(inputId = "index_test3",label = "Continuer")
            
          )
        )
      }
      if (CurrentValues$page =="test3")
      {
        return(
          list(
            tags$audio(src = "antiphase_HC_SOI.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            radioButtons("stim_3","",choices=c(1,2,3),selected=character(0),inline=TRUE),
            HTML("<br>"),  
            actionButton(inputId = "index_test4",label = "Continuer")
            
          )
        )
      }
      if (CurrentValues$page =="test4")
      {
        return(
          list(
            tags$audio(src = "antiphase_HC_ISO.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            radioButtons("stim_4","",choices=c(1,2,3),selected=character(0),inline=TRUE),    
            HTML("<br>"),  
            actionButton(inputId = "index_test5",label = "Continuer")
            
          )
        )
      }
      if (CurrentValues$page =="test5")
      {
        return(
          list(
            tags$audio(src = "antiphase_HC_OSI.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            radioButtons("stim_5","",choices=c(1,2,3),selected=character(0),inline=TRUE), 
            HTML("<br>"),  
            actionButton(inputId = "index_test6",label = "Continuer")
            
          )
        )
      }
      if (CurrentValues$page == "test6")
      {
        return(
          list(
            tags$audio(src = "antiphase_HC_SIO.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            radioButtons("stim_6","",choices=c(1,2,3),selected=character(0),inline=TRUE), 
            HTML("<br>"), 
            actionButton(inputId = "resultat_Test_Sons",label = "Continuer")
            
          )
        )
      }
      if (CurrentValues$page == "resultatTestSons")
      {
        return(
          list(
            h4(textOutput("headphone_check")),
            h4(textOutput("headphone_check2")),
            h4(textOutput("headphone_check3")),
            conditionalPanel(
              condition = "output.headphone_check",
              actionButton(inputId = "L_survey",label = "Continuer")
            ),
            conditionalPanel(
              condition = "output.headphone_check2",
              actionButton(inputId = "L_survey",label = "Continuer")
            )
          )
        )
      }
      if (CurrentValues$page =="Learning_survey")
        return(
          list(
            h3("Vous allez être diriger vers la phase d'apprentissage"),
            HTML("<hr>"),
            HTML("<p> Toutes les pages suivantes contiendront une images lorsque vous allez arriver sur la page un son sera jouer.<hr>
               Vous devez pour toutes les images retenir au mieux le son associer a celle ci. Rester concentré pendant l'apprentissage vous serez tester après.</p>"),
            HTML("<br>"),
            actionButton(inputId = "L1_survey", label = "Continuer")
            
          )
        )
      #Version de l'apprentissage en 1 forme/son par page(son instantané)
      if (CurrentValues$page =="Learning1_survey")
        return(
          list(
            h3("Voici l'image et vous entendez le son associé une fois."),
            HTML("<br>"),
            tags$audio(src = "1.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            tags$img(src="GForme.png", type="img/png",controls = NA),
            HTML("<hr>"),
            actionButton(inputId = "L2_survey", label = "Continuer")
          )
        )
      if (CurrentValues$page =="Learning2_survey")
        return(
          list(
            h3("Voici l'image et vous entendez le son associé une fois."),
            HTML("<br>"),
            tags$audio(src = "4.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            tags$img(src="GForme2.png", type="img/png",controls = NA),
            HTML("<hr>"),
            actionButton(inputId = "L3_survey", label = "Continuer")
          )
        )
      if (CurrentValues$page =="Learning3_survey")
        return(
          list(
            h3("Voici l'image et vous entendez le son associé une fois."),
            HTML("<br>"),
            tags$audio(src = "13.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            tags$img(src="GForme3.png", type="img/png",controls = NA),
            HTML("<hr>"),
            actionButton(inputId = "L4_survey", label = "Continuer")
          )
        )
      if (CurrentValues$page =="Learning4_survey")
        return(
          list(
            h3("Voici l'image et vous entendez le son associé une fois."),
            HTML("<br>"),
            tags$audio(src = "16.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            tags$img(src="GForme4.png", type="img/png",controls = NA),
            HTML("<hr>"),
            actionButton(inputId = "L5_survey", label = "Continuer")
          )
        )
      if (CurrentValues$page =="Learning5_survey")
        return(
          list(
            h3("Voici l'image et vous entendez le son associé une fois."),
            HTML("<br>"),
            tags$audio(src = "13.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            tags$img(src="GForme3.png", type="img/png",controls = NA),
            HTML("<hr>"),
            actionButton(inputId = "L6_survey", label = "Continuer")
          )
        )
      if (CurrentValues$page =="Learning6_survey")
        return(
          list(
            h3("Voici l'image et vous entendez le son associé une fois."),
            HTML("<br>"),
            tags$audio(src = "16.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            tags$img(src="GForme4.png", type="img/png",controls = NA),
            HTML("<hr>"),
            actionButton(inputId = "L7_survey", label = "Continuer")
          )
        )
      if (CurrentValues$page =="Learning7_survey")
        return(
          list(
            h3("Voici l'image et vous entendez le son associé une fois."),
            HTML("<br>"),
            tags$audio(src = "1.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            tags$img(src="GForme.png", type="img/png",controls = NA),
            HTML("<hr>"),
            actionButton(inputId = "L8_survey", label = "Continuer")
          )
        )
      if (CurrentValues$page =="Learning8_survey")
        return(
          list(
            h3("Voici l'image et vous entendez le son associé une fois."),
            HTML("<br>"),
            tags$audio(src = "4.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            tags$img(src="GForme2.png", type="img/png",controls = NA),
            HTML("<hr>"),
            actionButton(inputId = "L9_survey", label = "Continuer")
          )
        )
      if (CurrentValues$page =="Learning9_survey")
        return(
          list(
            h3("Voici l'image et vous entendez le son associé une fois."),
            HTML("<br>"),
            tags$audio(src = "16.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            tags$img(src="GForme4.png", type="img/png",controls = NA),
            HTML("<hr>"),
            actionButton(inputId = "L10_survey", label = "Continuer")
          )
        )
      if (CurrentValues$page =="Learning10_survey")
        return(
          list(
            h3("Voici l'image et vous entendez le son associé une fois."),
            HTML("<br>"),
            tags$audio(src = "4.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            tags$img(src="GForme2.png", type="img/png",controls = NA),
            HTML("<hr>"),
            actionButton(inputId = "L11_survey", label = "Continuer")
          )
        )
      if (CurrentValues$page =="Learning11_survey")
        return(
          list(
            h3("Voici l'image et vous entendez le son associé une fois."),
            HTML("<br>"),
            tags$audio(src = "13.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            tags$img(src="GForme3.png", type="img/png",controls = NA),
            HTML("<hr>"),
            actionButton(inputId = "L12_survey", label = "Continuer")
          )
        )
      if (CurrentValues$page =="Learning12_survey")
        return(
          list(
            h3("Voici l'image et vous entendez le son associé une fois."),
            HTML("<br>"),
            tags$audio(src = "1.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            tags$img(src="GForme.png", type="img/png",controls = NA),
            HTML("<hr>"),
            actionButton(inputId = "phase_Questionnaire", label = "Continuer")
          )
        )
      if (CurrentValues$page =="phaseQuestionnaire")
      {
        return(
          list(
            h3("Vous allez être diriger vers la phase de test de ce que vous avez pu retenir"),
            HTML("<hr>"),
            HTML("<p> Toutes les pages suivantes contiendront une images lorsque vous allez arriver sur la page un son sera jouer.<hr>
               Vous devez pour tout les sons qui vous seront présentés retrouver l'image associée à celui-ci présenté dans la phase apprentissage. Rester concentré, vous aurez besoin d'un bon score pour continuer.</p>"),
            HTML("<br>"),
            actionButton(inputId = "prepa1_questionnaire", label = "Continuer")
          )
        )
      }
      
      if (CurrentValues$page == "p1questionnaire")
      {
        return(
          list(
            h3("Test sur votre apprentissage"),
            HTML("<hr>"),
            conditionalPanel(
              condition= c("input.test_phase == 'Test1Question1Answer1' || input.test_phase =='Test1Question1Answer2' ||input.test_phase == 'Test1Question1Answer3' || input.test_phase == 'Test1Question1Answer4' "),
              actionButton(inputId = "prepa2_questionnaire",label = "Étape suivante")
            ),
            includeCSS("www/surveyp.css"),
            includeHTML("www/survey_p1q1.html"),
            tags$audio(src = "4.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            HTML("<hr>"),
            
            h4(textOutput("ScorePictures"))
          )
        )
      }
      
      if (CurrentValues$page == "p2questionnaire")
      {
        return(
          list(
            h3("Test sur votre apprentissage"),
            HTML("<hr>"),
            tags$audio(src = "16.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            HTML("<hr>"),
            conditionalPanel(
              condition= c("input.test_phase2 == 'Test1Question2Answer1' || input.test_phase2 =='Test1Question2Answer2' ||input.test_phase2 == 'Test1Question2Answer3' || input.test_phase2 == 'Test1Question2Answer4' "),
              actionButton(inputId = "prepa3_questionnaire",label = "Étape suivante")
            ),
            includeCSS("www/surveyp.css"),
            includeHTML("www/survey_p2q2.html"),
            
            h4(textOutput("ScorePictures"))
          )
        )
      }
      
      if (CurrentValues$page == "p3questionnaire")
      {
        return(
          list(
            h3("Test sur votre apprentissage"),
            HTML("<hr>"),
            tags$audio(src = "13.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            HTML("<hr>"),
            conditionalPanel(
              condition= c("input.test_phase3 == 'Test1Question3Answer1' || input.test_phase3 =='Test1Question3Answer2' ||input.test_phase3 == 'Test1Question3Answer3' || input.test_phase3 == 'Test1Question3Answer4' "),
              actionButton(inputId = "prepa4_questionnaire",label = "Étape suivante")
            ),
            includeCSS("www/surveyp.css"),
            includeHTML("www/survey_p3q3.html")
            
          )
        )
      }
      
      if (CurrentValues$page == "p4questionnaire")
      {
        return(
          list(
            h3("Test sur votre apprentissage"),
            HTML("<hr>"),
            tags$audio(src = "16.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            HTML("<hr>"),
            conditionalPanel(
              condition= c("input.test_phase4 == 'Test1Question4Answer1' || input.test_phase4 =='Test1Question4Answer2' ||input.test_phase4 == 'Test1Question4Answer3' || input.test_phase4 == 'Test1Question4Answer4' "),
              actionButton(inputId = "prepa5_questionnaire",label = "Étape suivante")
            ),
            includeCSS("www/surveyp.css"),
            includeHTML("www/survey_p4q4.html")
          )
        )
      }
      if (CurrentValues$page == "p5questionnaire")
      {
        return(
          list(
            h3("Test sur votre apprentissage"),
            HTML("<hr>"),
            tags$audio(src = "1.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            HTML("<hr>"),
            conditionalPanel(
              condition= c("input.test_phase5 == 'Test1Question5Answer1' || input.test_phase5 =='Test1Question5Answer2' ||input.test_phase5 == 'Test1Question5Answer3' || input.test_phase5 == 'Test1Question5Answer4' "),
              actionButton(inputId = "prepa6_questionnaire",label = "Étape suivante")
            ),
            includeCSS("www/surveyp.css"),
            includeHTML("www/survey_p5q5.html")
          )
        )
      }
      if (CurrentValues$page == "p6questionnaire")
      {
        return(
          list(
            h3("Test sur votre apprentissage"),
            HTML("<hr>"),
            tags$audio(src = "13.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            HTML("<hr>"),
            conditionalPanel(
              condition= c("input.test_phase6 == 'Test1Question6Answer1' || input.test_phase6 =='Test1Question6Answer2' ||input.test_phase6 == 'Test1Question6Answer3' || input.test_phase6 == 'Test1Question6Answer4' "),
              actionButton(inputId = "gt_inst4",label = "Étape suivante"),
              includeCSS("www/surveyp.css"),
              includeHTML("www/survey_p6q6.html"),
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
            tags$audio(src = "1.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            HTML("<hr>"),
            conditionalPanel(
              condition= c("input.Last_questionnaire1 == 'Last_chanceq1' || input.Last_questionnaire1 =='Last_chanceq2' ||input.Last_questionnaire1 == 'Last_chanceq3' || input.Last_questionnaire1 == 'Last_chanceq4' "),
              actionButton(inputId = "LastChance_questionnaire2",label = "Étape suivante")
            ),
            includeCSS("www/surveyp.css"),
            includeHTML("www/LastChance_questionnaire1.html"),
            
          )
        )
      }
      if (CurrentValues$page == "LastChancequestionnaire2") 
      {
        return(
          list(
            h3("Dernière chance "),
            HTML("<hr>"),
            conditionalPanel(
              condition= c("input.Last_questionnaire2 == 'Last_chanceq5' || input.Last_questionnaire2 =='Last_chanceq6' ||input.Last_questionnaire2 == 'Last_chanceq7' || input.Last_questionnaire2 == 'Last_chanceq8' "),
              actionButton(inputId = "LastChance_questionnaire3",label = "Étape suivante")
            ),
            includeCSS("www/surveyp.css"),
            includeHTML("www/LastChance_questionnaire2.html"),
            tags$audio(src = "16.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;")
          )
        )
      }
      if (CurrentValues$page == "LastChancequestionnaire3")  
      {
        return(
          list(
            h3("Dernière chance "),
            HTML("<hr>"),
            conditionalPanel(
              condition= c("input.Last_questionnaire3 == 'Last_chanceq9' || input.Last_questionnaire3 =='Last_chanceq10' ||input.Last_questionnaire3 == 'Last_chanceq11' || input.Last_questionnaire3 == 'Last_chanceq12' "),
              actionButton(inputId = "LastChance_questionnaire4",label = "Étape suivante")
            ),
            includeCSS("www/surveyp.css"),
            includeHTML("www/LastChance_questionnaire3.html"),
            tags$audio(src = "4.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;")
          )
        )
      }
      if(CurrentValues$page == "LastChancequestionnaire4")
      {
        return(
          list(
            h3("Dernière chance "),
            HTML("<hr>"),
            tags$audio(src = "13.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
            HTML("<hr>"),
            conditionalPanel(
              condition= c("input.Last_questionnaire4 == 'Last_chanceq13' || input.Last_questionnaire4 =='Last_chanceq14' ||input.Last_questionnaire4 == 'Last_chanceq15' || input.Last_questionnaire4 == 'Last_chanceq16' "),
              actionButton(inputId = "gt_inst5",label = "Étape suivante")
            ),
            includeCSS("www/surveyp.css"),
            includeHTML("www/LastChance_questionnaire4.html")
          )
        )
      }
      
      if(CurrentValues$page == "inst5")  
        return(
          list(
            h4("Vous allez être rediriger vers la page de test en duo. Le test suivant vous présentera un son que vous devrez associer à un tangram."),
            h4("Votre duo aura le meme son que vous et devra aussi associer un tangram a celui-ci.Vous devez donc vous adaptez à votre duo car vous n'aurez un point que si vous avez associer le meme tangram au son entendu."),
            actionButton(inputId = "test_multi", label ="Étape suivante")
          )
        )
      if (CurrentValues$page =="testmulti" && local$userId == 2)
      {
        return(
          list(
            h2("Vous êtes le joueur 2."),
            h3("A votre tour de jouer.Veuillez choisir la figure correspondant au son suivant:"),
            HTML("<hr>"),
            sidebarLayout(
              sidebarPanel(
                h4("Explications"),
                HTML("<br>"),
                p("Ce jeu en duo est fait de facon a ce que vous et votre partenaires répondiez en meme temps.Vous ne pourrez avir un point que si vous et votre 
                  partenaire avait une réponse similaire.Suite a chaque associations vous serez rediriger vers une page de résultat. Bon courage !"),
              ),
              mainPanel(
                conditionalPanel(
                  condition= c("input.phase_choicebis == 'Question1Answer1bis' || input.phase_choicebis =='Question1Answer2bis' ||input.phase_choicebis == 'Question1Answer3bis' || input.phase_choicebis == 'Question1Answer4bis' "),
                  actionButton("finish2", "Valider J2Q1", onclick = "myFunction()")
                ),
                HTML("<hr>"),
                includeCSS("www/survey.css"),
                includeHTML("www/survey_q1bis.html"),
                tags$audio(src = "4.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
                HTML("<hr>"),
                h4(textOutput("reponsedonnée")),
              ),
            )
            #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
          )
        )
      } 
      if (CurrentValues$page == "testmulti" && local$userId == 1)
      {
        return(
          list(
            h2("Vous êtes le joueur 1."),
            h3("C'est à vous de répondre.Associer le son que vous venez d'entendre à l'un des tangrams ci-dessous."),
            sidebarLayout(
              sidebarPanel(
                h5("Explications"),
                HTML("<br>"),
                p("Ce jeu en duo est fait de facon a ce que vous et votre partenaires répondiez en meme temps.Vous ne pourrez avir un point que si vous et votre 
                  partenaire avait une réponse similaire.Suite a chaque associations vous serez rediriger vers une page de résultat. Bon courage !"),
              ),
              mainPanel(
                conditionalPanel(
                  condition= c("input.phase_choice == 'Question1Answer1' || input.phase_choice =='Question1Answer2' ||input.phase_choice == 'Question1Answer3' || input.phase_choice == 'Question1Answer4' "),
                  actionButton("finish", "Valider J1Q1")
                ),
                HTML("<hr>"),
                includeCSS("www/survey.css"),
                includeHTML("www/survey_q1.html"),
                tags$audio(src = "4.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
                HTML("<hr>"),
                
              )
            )
            #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
          )
        )
      }
      if (CurrentValues$page == "result_1" && local$userId == 1)
      {
        return(
          list(
            includeCSS("www/imageresult.css"),
            sidebarLayout(
              sidebarPanel(
                h5("Explications"),
                HTML("<br>"),
                p("Vous êtes sur la page de résultat, lorsque vous appuyerai sur le bouton d'affichage vous pourrez voir la réponse de votre
                  partenaire. Nous vous conseillons de bien prendre en compte les réponses de votre partenaire pour que vous puissiez augmenter votre score."),
              ),
              mainPanel(
                actionButton("voirreponse", "Afficher la réponse de votre duo"),
                h4(textOutput("rep1")),
                h4(textOutput("rep2")),
                h4(textOutput("rep3")),
                h4(textOutput("rep4")),
                h4(textOutput("rep5")),
                h4(textOutput("rep6")),
                h4(textOutput("rep7")),
                h4(textOutput("rep8")),
                conditionalPanel(
                  condition= c("output.rep1 && output.rep5"),
                  tags$img(src="GFormeVO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep1 && output.rep6"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep1 && output.rep7"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep1 && output.rep8"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep2 && output.rep5"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep2 && output.rep6"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2VO.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep2 && output.rep7"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep2 && output.rep8"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep3 && output.rep5"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep3 && output.rep6"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep3 && output.rep7"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3VO.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep3 && output.rep8"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep4 && output.rep5"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep4 && output.rep6"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep4 && output.rep7"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep4 && output.rep8"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4VO.png", type="img/png",controls = NA)
                ),
                h4(textOutput("seeduoscore")),
                #tags$div(`id`= "MyProgress",
                #         h4(textOutput("voirduoscore"))
                #),
                #),
                
                #tags$div(`id`= "MyProgress2",
                #         h4(textOutput("voirduoscore2"))
                #),
                #conditionalPanel(
                #  condition = "output.voirduoscore",
                #  tags$progress(`id`="progress",
                #                `value`="20",
                #                `max` = "100",
                #                
                #                tags$span("22%"))
                #),
                #conditionalPanel(
                #  condition = "output.voirduoscore2",
                #  tags$progress(`id`="progress",
                #                `value`="0",
                #                `max` = "100")
                #),
                actionButton("result", "Passer au suivant")
                #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
              )
            )
          )
        )
      }
      if (CurrentValues$page == "result_6" && local$userId == 2)
      {
        return(
          list(
            includeCSS("www/imageresult.css"),
            sidebarLayout(
              sidebarPanel(
                h5("Explications"),
                HTML("<br>"),
                p("Vous êtes sur la page de résultat, lorsque vous appuyerai sur le bouton d'affichage vous pourrez voir la réponse de votre
                  partenaire. Nous vous conseillons de bien prendre en compte les réponses de votre partenaire pour que vous puissiez augmenter votre score."),
              ),
              mainPanel(
                actionButton("voirreponse2", "Afficher la réponse de votre duo"),
                h4(textOutput("rep1")),
                h4(textOutput("rep2")),
                h4(textOutput("rep3")),
                h4(textOutput("rep4")),
                h4(textOutput("rep5")),
                h4(textOutput("rep6")),
                h4(textOutput("rep7")),
                h4(textOutput("rep8")),
                conditionalPanel(
                  condition= c("output.rep5 && output.rep1"),
                  tags$img(src="GFormeVO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep5 && output.rep2"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep5 && output.rep3"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep5 && output.rep4"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep6 && output.rep1"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep6 && output.rep2"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2VO.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep6 && output.rep3"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep6 && output.rep4"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep7 && output.rep1"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep7 && output.rep2"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep7 && output.rep3"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3VO.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep7 && output.rep4"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep8 && output.rep1"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep8 && output.rep2"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep8 && output.rep3"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep8 && output.rep4"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4VO.png", type="img/png",controls = NA)
                ),
                HTML("<hr>"),
                h4(textOutput("seeduoscore")),
                #h4(textOutput("voirduoscore")),
                #h4(textOutput("voirduoscore2")),
                #conditionalPanel(
                #  condition = "output.voirduoscore",
                #  tags$progress(`id`="progress",
                #                `value`="20",
                #                `max` = "100",
                #                
                #                tags$span("22%"))
                #),
                #conditionalPanel(
                #  condition = "output.voirduoscore2",
                #  tags$progress(`id`="progress",
                #                `value`="0",
                #                `max` = "100")
                #),
                actionButton("result6", "Passer au suivant")
                #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
              )
            )
          )
        )
      }
      if (CurrentValues$page =="finish_3" && local$userId == 1)
      {
        return(
          list(
            h2("A votre tour de jouer.Veuillez choisir la figure correspondant au son suivant:"),
            tags$div(`id`= "Nouvelledivision2",
                     # h4(textOutput("questionsuivante2")),
                     #conditionalPanel(
                     # condition="output.questionsuivante2",
                     
                     sidebarLayout(
                       sidebarPanel(
                         p("Ce jeu en duo est fait de facon a ce que vous et votre partenaires répondiez en meme temps.Vous ne pourrez avir un point que si vous et votre 
                  partenaire avait une réponse similaire.Suite a chaque associations vous serez rediriger vers une page de résultat. Bon courage !"),
                       ),
                       mainPanel(
                         conditionalPanel(
                           condition= c("input.phase_choice2 == 'Question2Answer1' || input.phase_choice2 =='Question2Answer2' ||input.phase_choice2 == 'Question2Answer3' || input.phase_choice2 == 'Question2Answer4' "),
                           actionButton("finish3", "Valider J1Q2")
                         ),
                         tags$div(`id`="Nouvelledivision",
                                  `style`="display:none"),
                         tags$audio(src = "16.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
                         #condtionalPanel(
                         #  condition = "input.seeduoscore == 'Le score du duo est de : 1/1'",
                         #  includeScript("www/survey.js"),
                         
                         #tags$div(`id`= "MyProgress",
                         #         h4(textOutput("voirduoscore"))
                         #),
                         #),
                         
                         #tags$div(`id`= "MyProgress2",
                         #         h4(textOutput("voirduoscore2"))
                         #),
                         #conditionalPanel(
                         #  condition = "output.voirduoscore",
                         #  tags$progress(`id`="progress",
                         #                `value`="16",
                         #                `max` = "100",
                         #                
                         #                tags$span("22%"))
                         #),
                         #conditionalPanel(
                         #  condition = "output.voirduoscore2",
                         #  tags$progress(`id`="progress",
                         #                `value`="0",
                         #                `max` = "100")
                         #),
                         #HTML("<hr>"),
                         
                         h4(textOutput("reponsedonnée2")),
                         #tags$link(rel="stylesheet",type="text/css",href="cacher.css"),
                         includeCSS("www/survey.css"),
                         includeCSS("www/barprogress.css"),
                         includeHTML("www/survey_q2.html"),
                         #),
                       )
                     ),
            )
            #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
          )
        )
      } 
      if (CurrentValues$page == "finish_4" && local$userId == 2)
      {
        return(
          list(
            h2("C'est à vous de répondre.Associer le son que vous venez d'entendre à l'un des tangrams ci-dessous."),
            # actionButton("verificationButton","Voir si J2 a répondu"),
            # h4(textOutput("verification2")),
            sidebarLayout(
              sidebarPanel(
                p("Ce jeu en duo est fait de facon a ce que vous et votre partenaires répondiez en meme temps.Vous ne pourrez avir un point que si vous et votre 
                  partenaire avait une réponse similaire.Suite a chaque associations vous serez rediriger vers une page de résultat. Bon courage !")
              ),
              mainPanel(
                conditionalPanel(
                  condition= c("input.phase_choice2bis == 'Question2Answer1bis' || input.phase_choice2bis =='Question2Answer2bis' ||input.phase_choice2bis == 'Question2Answer3bis' || input.phase_choice2bis == 'Question2Answer4bis' "),
                  actionButton("finish4", "Valider J2Q2")
                ),
                HTML("<hr>"),
                tags$audio(src = "16.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
                includeCSS("www/survey.css"),
                includeHTML("www/survey_q2bis.html"),
                
              ),
            )
            #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
          )
        )
      }
      if (CurrentValues$page == "result_2" && local$userId == 1)
      {
        return(
          list(
            includeCSS("www/imageresult.css"),
            sidebarLayout(
              sidebarPanel(
                h5("Explications"),
                HTML("<br>"),
                p("Vous êtes sur la page de résultat, lorsque vous appuyerai sur le bouton d'affichage vous pourrez voir la réponse de votre
                  partenaire. Nous vous conseillons de bien prendre en compte les réponses de votre partenaire pour que vous puissiez augmenter votre score."),
              ),
              mainPanel(
                actionButton("voirreponse3", "Afficher la réponse de votre duo"),
                h4(textOutput("rep9")),
                h4(textOutput("rep10")),
                h4(textOutput("rep11")),
                h4(textOutput("rep12")),
                h4(textOutput("rep13")),
                h4(textOutput("rep14")),
                h4(textOutput("rep15")),
                h4(textOutput("rep16")),
                conditionalPanel(
                  condition= c("output.rep9 && output.rep13"),
                  tags$img(src="GFormeVO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep9 && output.rep14"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep9 && output.rep15"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep9 && output.rep16"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep10 && output.rep13"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep10 && output.rep14"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2VO.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep10 && output.rep15"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep10 && output.rep16"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep11 && output.rep13"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep11 && output.rep14"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep11 && output.rep15"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3VO.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep11 && output.rep16"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep12 && output.rep13"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep12 && output.rep14"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep12 && output.rep15"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep12 && output.rep16"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4VO.png", type="img/png",controls = NA)
                ),
                HTML("<hr>"),
                
                h4(textOutput("seeduoscore2")),
                
                #h4(textOutput("voirduoscore")),
                #h4(textOutput("voirduoscore2")),
                #h4(textOutput("voirduoscore3")),
                #conditionalPanel(
                #  condition = "output.voirduoscore",
                #  tags$progress(`id`="progress",
                #                `value`="20",
                #                `max` = "100",
                #                
                #                tags$span("22%"))
                #),
                #conditionalPanel(
                #  condition = "output.voirduoscore2",
                #  tags$progress(`id`="progress",
                #                `value`="0",
                #                `max` = "100")
                #),
                #conditionalPanel(
                #  condition = "output.voirduoscore3",
                #  tags$progress(`id`="progress",
                #                `value`="40",
                #                `max` = "100")
                #),
                actionButton("result2", "Passer au suivant")
                #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
              )
            )
          )
        )
      }
      if (CurrentValues$page == "result_7" && local$userId == 2)
      {
        return(
          list(
            includeCSS("www/imageresult.css"),
            sidebarLayout(
              sidebarPanel(
                h5("Explications"),
                HTML("<br>"),
                p("Vous êtes sur la page de résultat, lorsque vous appuyerai sur le bouton d'affichage vous pourrez voir la réponse de votre
                  partenaire. Nous vous conseillons de bien prendre en compte les réponses de votre partenaire pour que vous puissiez augmenter votre score."),
              ),
              mainPanel(
                actionButton("voirreponse4", "Afficher la réponse de votre duo"),
                h4(textOutput("rep9")),
                h4(textOutput("rep10")),
                h4(textOutput("rep11")),
                h4(textOutput("rep12")),
                h4(textOutput("rep13")),
                h4(textOutput("rep14")),
                h4(textOutput("rep15")),
                h4(textOutput("rep16")),
                conditionalPanel(
                  condition= c("output.rep13 && output.rep9"),
                  tags$img(src="GFormeVO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep13 && output.rep10"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep13 && output.rep11"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep13 && output.rep12"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep14 && output.rep9"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep14 && output.rep10"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2VO.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep14 && output.rep11"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep14 && output.rep12"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep15 && output.rep9"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep15 && output.rep10"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep15 && output.rep11"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3VO.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep15 && output.rep12"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep16 && output.rep9"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep16 && output.rep10"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep16 && output.rep11"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep16 && output.rep12"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4VO.png", type="img/png",controls = NA)
                ),
                HTML("<hr>"),
                h4(textOutput("seeduoscore2")),
                #h4(textOutput("voirduoscore")),
                #h4(textOutput("voirduoscore2")),
                #h4(textOutput("voirduoscore3")),
                #conditionalPanel(
                #  condition = "output.voirduoscore",
                #  tags$progress(`id`="progress",
                #                `value`="20",
                #                `max` = "100",
                #                
                #                tags$span("22%"))
                #),
                #conditionalPanel(
                #  condition = "output.voirduoscore2",
                #  tags$progress(`id`="progress",
                #                `value`="0",
                #                `max` = "100")
                #),
                #conditionalPanel(
                #  condition = "output.voirduoscore3",
                #  tags$progress(`id`="progress",
                #                `value`="40",
                #                `max` = "100")
                #),
                actionButton("result7", "Passer au suivant")
                #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
              )
            )
          )
        )
      }
      if (CurrentValues$page =="finish_5" && local$userId == 1)
      {
        return(
          list(
            h2("C'est à vous de répondre.Associer le son que vous venez d'entendre à l'un des tangrams ci-dessous."),
            sidebarLayout(
              sidebarPanel(
                p("Ce jeu en duo est fait de facon a ce que vous et votre partenaires répondiez en meme temps.Vous ne pourrez avir un point que si vous et votre 
                  partenaire avait une réponse similaire.Suite a chaque associations vous serez rediriger vers une page de résultat. Bon courage !")
              ),
              mainPanel(
                conditionalPanel(
                  condition= c("input.phase_choice3 == 'Question3Answer1' || input.phase_choice3 =='Question3Answer2' ||input.phase_choice3 == 'Question3Answer3' || input.phase_choice3 == 'Question3Answer4' "),
                  actionButton("finish5", "Valider J1Q3")
                ),
                HTML("<hr>"),
                tags$audio(src = "13.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
                includeCSS("www/survey.css"),
                includeHTML("www/survey_q3.html"),
                
              ),
            )
            #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
          )
        )
      } 
      if (CurrentValues$page == "finish_6" && local$userId == 2)
      {
        return(
          list(
            h2("A votre tour de jouer.Veuillez choisir la figure correspondant au son suivant:"),
            HTML("<hr>"),
            h4(textOutput("questionsuivante3")),
            sidebarLayout(
              sidebarPanel(
                p("Ce jeu en duo est fait de facon a ce que vous et votre partenaires répondiez en meme temps.Vous ne pourrez avir un point que si vous et votre 
                  partenaire avait une réponse similaire.Suite a chaque associations vous serez rediriger vers une page de résultat. Bon courage !")
              ),
              mainPanel(
                conditionalPanel(
                  condition= c("input.phase_choice3bis == 'Question3Answer1bis' || input.phase_choice3bis =='Question3Answer2bis' ||input.phase_choice3bis == 'Question3Answer3bis' || input.phase_choice3bis == 'Question3Answer4bis' "),
                  actionButton("finish6", "Valider J2Q3")
                ),
                HTML("<hr>"),
                tags$audio(src = "13.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
                includeCSS("www/survey.css"),
                includeHTML("www/survey_q3bis.html"),
                h4(textOutput("reponsedonnée3")),
                
              ),
            )
            # ),
            #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
          )
        )
      }
      if (CurrentValues$page == "result_3" && local$userId == 1)
      {
        return(
          list(
            includeCSS("www/imageresult.css"),
            sidebarLayout(
              sidebarPanel(
                h5("Explications"),
                HTML("<br>"),
                p("Vous êtes sur la page de résultat, lorsque vous appuyerai sur le bouton d'affichage vous pourrez voir la réponse de votre
                  partenaire. Nous vous conseillons de bien prendre en compte les réponses de votre partenaire pour que vous puissiez augmenter votre score."),
              ),
              mainPanel(
                actionButton("voirreponse5", "Afficher la réponse de votre duo"),
                h4(textOutput("rep17")),
                h4(textOutput("rep18")),
                h4(textOutput("rep19")),
                h4(textOutput("rep20")),
                h4(textOutput("rep21")),
                h4(textOutput("rep22")),
                h4(textOutput("rep23")),
                h4(textOutput("rep24")),
                conditionalPanel(
                  condition= c("output.rep17 && output.rep21"),
                  tags$img(src="GFormeVO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep17 && output.rep22"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep17 && output.rep23"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep17 && output.rep24"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep18 && output.rep21"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep18 && output.re22"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2VO.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep18 && output.rep23"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep18 && output.rep24"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep19 && output.rep21"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep19 && output.rep22"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep19 && output.rep23"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3VO.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep19 && output.rep24"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep20 && output.rep21"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep20 && output.rep22"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep20 && output.rep23"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep20 && output.rep24"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4VO.png", type="img/png",controls = NA)
                ),
                HTML("<hr>"),
                h4(textOutput("seeduoscore3")),
                #h4(textOutput("voirduoscore")),
                #h4(textOutput("voirduoscore2")),
                #h4(textOutput("voirduoscore3")),
                #h4(textOutput("voirduoscore4")),
                #conditionalPanel(
                #  condition = "output.voirduoscore",
                #  tags$progress(`id`="progress",
                #                `value`="20",
                #                `max` = "100",
                #                
                #                tags$span("22%"))
                #),
                # conditionalPanel(
                #  condition = "output.voirduoscore2",
                # tags$progress(`id`="progress",
                #                `value`="0",
                #                `max` = "100")
                #),
                #conditionalPanel(
                #  condition = "output.voirduoscore3",
                #  tags$progress(`id`="progress",
                #                `value`="40",
                #                `max` = "100")
                #),
                #conditionalPanel(
                # condition = "output.voirduoscore4",
                # tags$progress(`id`="progress",
                #                `value`="60",
                #                `max` = "100")
                #),
                actionButton("result3", "Passer au suivant")
                #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
              )
            )
          )
        )
      }
      if (CurrentValues$page == "result_8" && local$userId == 2)
      {
        return(
          list(
            includeCSS("www/imageresult.css"),
            sidebarLayout(
              sidebarPanel(
                h5("Explications"),
                HTML("<br>"),
                p("Vous êtes sur la page de résultat, lorsque vous appuyerai sur le bouton d'affichage vous pourrez voir la réponse de votre
                  partenaire. Nous vous conseillons de bien prendre en compte les réponses de votre partenaire pour que vous puissiez augmenter votre score."),
              ),
              mainPanel(
                actionButton("voirreponse6", "Afficher la réponse de votre duo"),
                h4(textOutput("rep17")),
                h4(textOutput("rep18")),
                h4(textOutput("rep19")),
                h4(textOutput("rep20")),
                h4(textOutput("rep21")),
                h4(textOutput("rep22")),
                h4(textOutput("rep23")),
                h4(textOutput("rep24")),
                conditionalPanel(
                  condition= c("output.rep21 && output.rep17"),
                  tags$img(src="GFormeVO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep21 && output.rep18"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep21 && output.rep19"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep21 && output.rep20"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep22 && output.rep17"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep22 && output.rep18"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2VO.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep22 && output.rep19"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep22 && output.rep20"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep23 && output.rep17"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep23 && output.rep18"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep23 && output.rep19"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3VO.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep23 && output.rep20"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep24 && output.rep17"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep24 && output.rep18"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep24 && output.rep19"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep24 && output.rep20"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4VO.png", type="img/png",controls = NA)
                ),
                HTML("<hr>"),
                HTML("<hr>"),
                h4(textOutput("seeduoscore3")),
                #h4(textOutput("voirduoscore")),
                #h4(textOutput("voirduoscore2")),
                #h4(textOutput("voirduoscore3")),
                #h4(textOutput("voirduoscore4")),
                #conditionalPanel(
                #  condition = "output.voirduoscore",
                #  tags$progress(`id`="progress",
                #                `value`="20",
                #                `max` = "100",
                #                
                #                tags$span("22%"))
                #),
                # conditionalPanel(
                #  condition = "output.voirduoscore2",
                #  tags$progress(`id`="progress",
                #                `value`="0",
                #                `max` = "100")
                #),
                #conditionalPanel(
                #  condition = "output.voirduoscore3",
                #  tags$progress(`id`="progress",
                #                `value`="40",
                #                `max` = "100")
                #),
                #conditionalPanel(
                #  condition = "output.voirduoscore4",
                #  tags$progress(`id`="progress",
                #                `value`="60",
                #                `max` = "100")
                #),
                actionButton("result8", "Passer au suivant")
                #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
              )
            )
          )
        )
      }
      if (CurrentValues$page =="finish_7" && local$userId == 1)
      {
        return(
          list(
            h2("A votre tour de jouer.Veuillez choisir la figure correspondant au son suivant:"),
            HTML("<hr>"),
            sidebarLayout(
              sidebarPanel(
                p("Ce jeu en duo est fait de facon a ce que vous et votre partenaires répondiez en meme temps.Vous ne pourrez avir un point que si vous et votre 
                  partenaire avait une réponse similaire.Suite a chaque associations vous serez rediriger vers une page de résultat. Bon courage !")
              ),
              mainPanel(
                conditionalPanel(
                  condition= c("input.phase_choice4 == 'Question4Answer1' || input.phase_choice4 =='Question4Answer2' ||input.phase_choice4 == 'Question4Answer3' || input.phase_choice4 == 'Question4Answer4' "),
                  actionButton("finish7", "Valider J1Q4")
                ),
                HTML("<hr>"),
                tags$audio(src = "16.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
                includeCSS("www/survey.css"),
                includeHTML("www/survey_q4.html"),
                h4(textOutput("reponsedonnée4")),
                
              ),
            )
            #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
          )
        )
      } 
      if (CurrentValues$page == "finish_8" && local$userId == 2)
      {
        return(
          list(
            h2("C'est à vous de répondre.Associer le son que vous venez d'entendre à l'un des tangrams ci-dessous."),
            sidebarLayout(
              sidebarPanel(
                p("Ce jeu en duo est fait de facon a ce que vous et votre partenaires répondiez en meme temps.Vous ne pourrez avir un point que si vous et votre 
                  partenaire avait une réponse similaire.Suite a chaque associations vous serez rediriger vers une page de résultat. Bon courage !")
              ),
              mainPanel(
                conditionalPanel(
                  condition= c("input.phase_choice4bis == 'Question4Answer1bis' || input.phase_choice4bis =='Question4Answer2bis' ||input.phase_choice4bis == 'Question4Answer3bis' || input.phase_choice4bis == 'Question4Answer4bis' "),
                  actionButton("finish8", "Valider J2Q4")
                ),
                HTML("<hr>"),
                tags$audio(src = "16.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
                includeCSS("www/survey.css"),
                includeHTML("www/survey_q4bis.html"),
              ),
            )
            #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
          )
        )
      }
      if (CurrentValues$page == "result_4" && local$userId == 1)
      {
        return(
          list(
            includeCSS("www/imageresult.css"),
            sidebarLayout(
              sidebarPanel(
                h5("Explications"),
                HTML("<br>"),
                p("Vous êtes sur la page de résultat, lorsque vous appuyerai sur le bouton d'affichage vous pourrez voir la réponse de votre
                  partenaire. Nous vous conseillons de bien prendre en compte les réponses de votre partenaire pour que vous puissiez augmenter votre score."),
              ),
              mainPanel(
                actionButton("voirreponse7", "Afficher la réponse de votre duo"),
                h4(textOutput("rep25")),
                h4(textOutput("rep26")),
                h4(textOutput("rep27")),
                h4(textOutput("rep28")),
                h4(textOutput("rep29")),
                h4(textOutput("rep30")),
                h4(textOutput("rep31")),
                h4(textOutput("rep32")),
                conditionalPanel(
                  condition= c("output.rep25 && output.rep29"),
                  tags$img(src="GFormeVO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep25 && output.rep30"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep25 && output.rep31"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep25 && output.rep32"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep26 && output.rep29"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep26 && output.rep30"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2VO.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep26 && output.rep31"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep26 && output.rep32"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep27 && output.rep29"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep27 && output.rep30"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep27 && output.rep31"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3VO.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep27 && output.rep32"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep28 && output.rep29"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep28 && output.rep30"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep28 && output.rep31"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep28 && output.rep32"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4VO.png", type="img/png",controls = NA)
                ),
                HTML("<hr>"),
                HTML("<hr>"),
                h4(textOutput("seeduoscore4")),
                #h4(textOutput("voirduoscore")),
                #h4(textOutput("voirduoscore2")),
                #h4(textOutput("voirduoscore3")),
                #h4(textOutput("voirduoscore4")),
                #h4(textOutput("voirduoscore5")),
                #conditionalPanel(
                #  condition = "output.voirduoscore",
                #  tags$progress(`id`="progress",
                #                `value`="20",
                #                `max` = "100",
                #                
                #                tags$span("22%"))
                #),
                #conditionalPanel(
                # condition = "output.voirduoscore2",
                #  tags$progress(`id`="progress",
                #                `value`="0",
                #                `max` = "100")
                # ),
                #conditionalPanel(
                #  condition = "output.voirduoscore3",
                #   tags$progress(`id`="progress",
                #                  `value`="40",
                #                 `max` = "100")
                # ),
                # conditionalPanel(
                #   condition = "output.voirduoscore4",
                #   tags$progress(`id`="progress",
                #                 `value`="60",
                #                 `max` = "100")
                # ),
                # conditionalPanel(
                #   condition = "output.voirduoscore5",
                #   tags$progress(`id`="progress",
                #                 `value`="80",
                #                 `max` = "100")
                # ),
                actionButton("result4", "Passer au suivant")
                #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
              )
            )
          )
        )
      }
      if (CurrentValues$page == "result_9" && local$userId == 2)
      {
        return(
          list(
            includeCSS("www/imageresult.css"),
            sidebarLayout(
              sidebarPanel(
                h5("Explications"),
                HTML("<br>"),
                p("Vous êtes sur la page de résultat, lorsque vous appuyerai sur le bouton d'affichage vous pourrez voir la réponse de votre
                  partenaire. Nous vous conseillons de bien prendre en compte les réponses de votre partenaire pour que vous puissiez augmenter votre score."),
              ),
              mainPanel(
                actionButton("voirreponse8", "Afficher la réponse de votre duo"),
                h4(textOutput("rep25")),
                h4(textOutput("rep26")),
                h4(textOutput("rep27")),
                h4(textOutput("rep28")),
                h4(textOutput("rep29")),
                h4(textOutput("rep30")),
                h4(textOutput("rep31")),
                h4(textOutput("rep32")),
                conditionalPanel(
                  condition= c("output.rep29 && output.rep25"),
                  tags$img(src="GFormeVO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep29 && output.rep26"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep29 && output.rep27"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep29 && output.rep28"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep30 && output.rep25"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep30 && output.rep26"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2VO.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep30 && output.rep27"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep30 && output.rep28"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep31 && output.rep25"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep31 && output.rep26"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep31 && output.rep27"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3VO.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep31 && output.rep28"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep32 && output.rep25"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep32 && output.rep26"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep32 && output.rep27"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep32 && output.rep28"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4VO.png", type="img/png",controls = NA)
                ),
                HTML("<hr>"),
                HTML("<hr>"),
                h4(textOutput("seeduoscore4")),
                #h4(textOutput("voirduoscore")),
                #h4(textOutput("voirduoscore2")),
                #h4(textOutput("voirduoscore3")),
                #h4(textOutput("voirduoscore4")),
                #h4(textOutput("voirduoscore5")),
                #conditionalPanel(
                #  condition = "output.voirduoscore",
                #  tags$progress(`id`="progress",
                #                `value`="20",
                #                `max` = "100",
                #                
                #                tags$span("22%"))
                #),
                #conditionalPanel(
                #  condition = "output.voirduoscore2",
                #  tags$progress(`id`="progress",
                #               `value`="0",
                #               `max` = "100")
                #),
                #conditionalPanel(
                #  condition = "output.voirduoscore3",
                #  tags$progress(`id`="progress",
                #                `value`="40",
                #                `max` = "100")
                #),
                # conditionalPanel(
                # condition = "output.voirduoscore4",
                # tags$progress(`id`="progress",
                #                `value`="60",
                #                 `max` = "100")
                # ),
                # conditionalPanel(
                #  condition = "output.voirduoscore5",
                #  tags$progress(`id`="progress",
                #               `value`="80",
                #                `max` = "100")
                #),
                actionButton("result9", "Passer au suivant")
                #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
              )
            )
          )
        )
      }
      if (CurrentValues$page =="finish_9" && local$userId == 1)
      {
        return(
          list(
            h2("C'est à vous de répondre.Associer le son que vous venez d'entendre à l'un des tangrams ci-dessous."),
            #actionButton("verificationButton","Voir si J2 a répondu"),
            #h4(textOutput("verification5")),
            sidebarLayout(
              sidebarPanel(
                p("Ce jeu en duo est fait de facon a ce que vous et votre partenaires répondiez en meme temps.Vous ne pourrez avir un point que si vous et votre 
                  partenaire avait une réponse similaire.Suite a chaque associations vous serez rediriger vers une page de résultat. Bon courage !")
              ),
              mainPanel(
                conditionalPanel(
                  condition= c("input.phase_choice5 == 'Question5Answer1' || input.phase_choice5 =='Question5Answer2' ||input.phase_choice5 == 'Question5Answer3' || input.phase_choice5 == 'Question5Answer4' "),
                  actionButton("finish9", "Valider J1Q5")
                ),
                HTML("<hr>"),
                tags$audio(src = "1.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
                includeCSS("www/survey.css"),
                includeHTML("www/survey_q5.html"),
                
              ),
            )
            #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
          )
        )
      } 
      if (CurrentValues$page == "finish_10" && local$userId == 2)
      {
        return(
          list(
            h2("A votre tour de jouer.Veuillez choisir la figure correspondant au son suivant:"),
            
            #h4(textOutput("questionsuivante5")),
            #conditionalPanel(
            #condition="output.questionsuivante5",
            sidebarLayout(
              sidebarPanel(
                p("Ce jeu en duo est fait de facon a ce que vous et votre partenaires répondiez en meme temps.Vous ne pourrez avir un point que si vous et votre 
                  partenaire avait une réponse similaire.Suite a chaque associations vous serez rediriger vers une page de résultat. Bon courage !")
              ),
              mainPanel(
                conditionalPanel(
                  condition= c("input.phase_choice5bis == 'Question5Answer1bis' || input.phase_choice5bis =='Question5Answer2bis' ||input.phase_choice5bis == 'Question5Answer3bis' || input.phase_choice5bis == 'Question5Answer4bis' "),
                  actionButton("finish10", "Valider J2Q5")
                ),
                HTML("<hr>"),
                tags$audio(src = "1.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
                includeCSS("www/survey.css"),
                includeHTML("www/survey_q5bis.html"),
                h4(textOutput("reponsedonnée5")),
                
              ),
            )
            # )
            #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
          )
        )
      }
      if (CurrentValues$page == "result_5" && local$userId == 1)
      {
        return(
          list(
            includeCSS("www/imageresult.css"),
            sidebarLayout(
              sidebarPanel(
                h5("Explications"),
                HTML("<br>"),
                p("Vous êtes sur la page de résultat, lorsque vous appuyerai sur le bouton d'affichage vous pourrez voir la réponse de votre
                  partenaire. Nous vous conseillons de bien prendre en compte les réponses de votre partenaire pour que vous puissiez augmenter votre score."),
              ),
              mainPanel(
                actionButton("voirreponse9", "Afficher la réponse de votre duo"),
                h4(textOutput("rep33")),
                h4(textOutput("rep34")),
                h4(textOutput("rep35")),
                h4(textOutput("rep36")),
                h4(textOutput("rep37")),
                h4(textOutput("rep38")),
                h4(textOutput("rep39")),
                h4(textOutput("rep40")),
                conditionalPanel(
                  condition= c("output.rep33 && output.rep37"),
                  tags$img(src="GFormeVO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep33 && output.rep38"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep33 && output.rep39"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep33 && output.rep40"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep34 && output.rep37"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep34 && output.rep38"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2VO.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep34 && output.rep39"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep34 && output.rep40"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep35 && output.rep37"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep35 && output.rep38"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep35 && output.rep39"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3VO.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep35 && output.rep40"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep36 && output.rep37"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep36 && output.rep38"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep36 && output.rep39"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep36 && output.rep40"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4VO.png", type="img/png",controls = NA)
                ),
                
                h4(textOutput("seeduoscore5")),
                actionButton("result5", "Passer au suivant")
                #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
              )
            )
          )
        )
      }
      if (CurrentValues$page == "result_10" && local$userId == 2)
      {
        return(
          list(
            includeCSS("www/imageresult.css"),
            sidebarLayout(
              sidebarPanel(
                h5("Explications"),
                HTML("<br>"),
                p("Vous êtes sur la page de résultat, lorsque vous appuyerai sur le bouton d'affichage vous pourrez voir la réponse de votre
                  partenaire. Nous vous conseillons de bien prendre en compte les réponses de votre partenaire pour que vous puissiez augmenter votre score."),
              ),
              mainPanel(
                actionButton("voirreponse10", "Afficher la réponse de votre duo"),
                h4(textOutput("rep33")),
                h4(textOutput("rep34")),
                h4(textOutput("rep35")),
                h4(textOutput("rep36")),
                h4(textOutput("rep37")),
                h4(textOutput("rep38")),
                h4(textOutput("rep39")),
                h4(textOutput("rep40")),
                conditionalPanel(
                  condition= c("output.rep37 && output.rep33"),
                  tags$img(src="GFormeVO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep37 && output.rep34"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep37 && output.rep35"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep37 && output.rep36"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep38 && output.rep33"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep38 && output.rep34"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2VO.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep38 && output.rep35"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep38 && output.rep36"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep39 && output.rep33"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep39 && output.rep34"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep39 && output.rep35"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3VO.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep39 && output.rep36"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep40 && output.rep33"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep40 && output.rep34"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep40 && output.rep35"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep40 && output.rep36"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4VO.png", type="img/png",controls = NA)
                ),
                HTML("<hr>"),
                HTML("<hr>"),
                h4(textOutput("seeduoscore5")),
                
                actionButton("result10", "Passer au suivant")
                #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
              )
            )
          )
        )
      }
      if (CurrentValues$page =="finish_11" && local$userId == 1)
      {
        return(
          list(
            h2("A votre tour de jouer.Veuillez choisir la figure correspondant au son suivant:"),
            # h4(textOutput("questionsuivante6")),
            #conditionalPanel(
            #condition="output.questionsuivante6",
            sidebarLayout(
              sidebarPanel(
                p("Ce jeu en duo est fait de facon a ce que vous et votre partenaires répondiez en meme temps.Vous ne pourrez avir un point que si vous et votre 
                  partenaire avait une réponse similaire.Suite a chaque associations vous serez rediriger vers une page de résultat. Bon courage !")
              ),
              mainPanel(
                conditionalPanel(
                  condition= c("input.phase_choice6 == 'Question6Answer1' || input.phase_choice6 =='Question6Answer2' ||input.phase_choice6 == 'Question6Answer3' || input.phase_choice6 == 'Question6Answer4' "),
                  actionButton("finish11", "Valider J1Q6")
                ),
                HTML("<hr>"),
                tags$audio(src = "13.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
                includeCSS("www/survey.css"),
                includeHTML("www/survey_q6.html"),
                h4(textOutput("reponsedonnée6")),
                
              ),
            )
            # )
            #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
          )
        )
      } 
      if (CurrentValues$page == "finish_12" && local$userId == 2)
      {
        return(
          list(
            h2("C'est à vous de répondre.Associer le son que vous venez d'entendre à l'un des tangrams ci-dessous."),
            #actionButton("verificationButton","Voir si J2 a répondu"),
            #h4(textOutput("verification6")),
            sidebarLayout(
              sidebarPanel(
                p("Ce jeu en duo est fait de facon a ce que vous et votre partenaires répondiez en meme temps.Vous ne pourrez avir un point que si vous et votre 
                  partenaire avait une réponse similaire.Suite a chaque associations vous serez rediriger vers une page de résultat. Bon courage !")
              ),
              mainPanel(
                conditionalPanel(
                  condition= c("input.phase_choice6bis == 'Question6Answer1bis' || input.phase_choice6bis =='Question6Answer2bis' ||input.phase_choice6bis == 'Question6Answer3bis' || input.phase_choice6bis == 'Question6Answer4bis' "),
                  actionButton("finish12", "Valider J2Q6")
                ),
                HTML("<hr>"),
                tags$audio(src = "13.wav", type = "audio/wav", autoplay = TRUE, controls = NA, style ="display:none;"),
                includeCSS("www/survey.css"),
                includeHTML("www/survey_q6bis.html"),
              ),
            )
            #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
          )
        )
      }
      if (CurrentValues$page == "result_12" && local$userId == 1)
      {
        return(
          list(
            includeCSS("www/imageresult.css"),
            sidebarLayout(
              sidebarPanel(
                h5("Explications"),
                HTML("<br>"),
                p("Vous êtes sur la page de résultat, lorsque vous appuyerai sur le bouton d'affichage vous pourrez voir la réponse de votre
                  partenaire. Nous vous conseillons de bien prendre en compte les réponses de votre partenaire pour que vous puissiez augmenter votre score."),
              ),
              mainPanel(
                actionButton("voirreponse11", "Afficher la réponse de votre duo"),
                h4(textOutput("rep41")),
                h4(textOutput("rep42")),
                h4(textOutput("rep43")),
                h4(textOutput("rep44")),
                h4(textOutput("rep45")),
                h4(textOutput("rep46")),
                h4(textOutput("rep47")),
                h4(textOutput("rep48")),
                conditionalPanel(
                  condition= c("output.rep41 && output.rep45"),
                  tags$img(src="GFormeVO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep41 && output.rep46"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep41 && output.rep47"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep41 && output.rep48"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep42 && output.rep45"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep42 && output.rep46"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2VO.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep42 && output.rep47"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep42 && output.rep48"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep43 && output.rep45"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep43 && output.rep46"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep43 && output.rep47"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3VO.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep43 && output.rep48"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep44 && output.rep45"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep44 && output.rep46"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep44 && output.rep47"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep44 && output.rep48"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4VO.png", type="img/png",controls = NA)
                ),
                HTML("<hr>"),
                HTML("<hr>"),
                h4(textOutput("seeduoscore6")),
                actionButton("resultfinal1", "Finir")
                #h3("Merci pour votre participation!Vous nous avez été d'une grande aide.J1")
                #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
              )
            )
          )
        )
      }
      
      if (CurrentValues$page == "result_11" && local$userId == 2)
      {
        return(
          list(
            includeCSS("www/imageresult.css"),
            sidebarLayout(
              sidebarPanel(
                h5("Explications"),
                HTML("<br>"),
                p("Vous êtes sur la page de résultat, lorsque vous appuyerai sur le bouton d'affichage vous pourrez voir la réponse de votre
                  partenaire. Nous vous conseillons de bien prendre en compte les réponses de votre partenaire pour que vous puissiez augmenter votre score."),
              ),
              mainPanel(
                actionButton("voirreponse12", "Afficher la réponse de votre duo"),
                h4(textOutput("rep41")),
                h4(textOutput("rep42")),
                h4(textOutput("rep43")),
                h4(textOutput("rep44")),
                h4(textOutput("rep45")),
                h4(textOutput("rep46")),
                h4(textOutput("rep47")),
                h4(textOutput("rep48")),
                conditionalPanel(
                  condition= c("output.rep45 && output.rep41"),
                  tags$img(src="GFormeVO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep45 && output.rep42"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep45 && output.rep43"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep45 && output.rep44"),
                  tags$img(src="GFormeV.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                  
                ),
                conditionalPanel(
                  condition= c("output.rep46 && output.rep41"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep46 && output.rep42"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2VO.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep46 && output.rep43"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep46 && output.rep44"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2V.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep47 && output.rep41"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep47 && output.rep42"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep47 && output.rep43"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3VO.png", type="img/png",controls = NA),
                  tags$img(src="test4.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep47 && output.rep44"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3V.png", type="img/png",controls = NA),
                  tags$img(src="GForme4O.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep48 && output.rep41"),
                  tags$img(src="GFormeO.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep48 && output.rep42"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="GForme2O.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep48 && output.rep43"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="GForme3O.png", type="img/png",controls = NA),
                  tags$img(src="GForme4V.png", type="img/png",controls = NA)
                ),
                conditionalPanel(
                  condition= c("output.rep48 && output.rep44"),
                  tags$img(src="test1.png", type="img/png",controls = NA),
                  tags$img(src="test2bis.png", type="img/png",controls = NA),
                  tags$img(src="test3.png", type="img/png",controls = NA),
                  tags$img(src="GForme4VO.png", type="img/png",controls = NA)
                ),
                HTML("<hr>"),
                HTML("<hr>"),
                h4(textOutput("seeduoscore6")),
                actionButton("resultfinal2", "Finir")
                #h3("Merci pour votre participation!Vous nous avez été d'une grande aide.J2")
                #need une fonction de restart serveur soit bouton soit automatique quand on arrive ici
              )
            )
          )
        )
      }
      if (CurrentValues$page == "result_final1" && local$userId == 1)
      {
        return(
          list(
            includeCSS("www/resultfinal.css"),
            h1("Merci d'avoir participer"),
            h2("Le score du duo est de :",globalscore$duoscore,"/6"),
            h3("Vous pouvez maintenant vous déconnectez bonne continuation.")
            #renderText(paste("Le score du duo est de :",globalscore$duoscore,"/6")) 
          )
        )
      }
      if (CurrentValues$page == "result_final2" && local$userId == 2)
      {
        return(
          list(
            includeCSS("www/resultfinal.css"),
            h1("Merci d'avoir participer"),
            h2("Le score du duo est de :",globalscore$duoscore2,"/6"),
            h3("Vous pouvez maintenant vous déconnectez bonne continuation.")
            #renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/6")) 
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
    ifelse(input$isMobile, "Votre appareil est un mobile.", "Nous avons établi que vous êtes bien sur ordinateur.")
  })
  
  output$lat <- renderPrint({
    input$lat
  })
  
  output$long <- renderPrint({
    input$long
  })
  
  #modify
  output$geolocation <- renderPrint({
    input$geolocation
    output$AntiTrue <- renderText(paste("TEST"))
  })
  
  
  #Rename
  observeEvent(input$waiting_Room,{
    if(local$userId == 1){
      output$player1 <- renderText(paste("Vous êtes le joueur numéro 1. Veuillez bien le retenir!"))
    }
    else if (local$userId == 2){
      output$player2 <- renderText(paste("Vous êtes le joueur numéro 2.Veuillez bien le retenir!"))
    }else
      output$playersFull <- renderText(paste("Désolé mais l'application est actuellement pleine veuillez réessayer ultérieurement"))
  })
  
  output$numberOfUsers <- renderText(paste("Il y a actuellement : ",global$amountUser," participant(e) dans la salle d’attente. Dès que vous serez deux, vous pourrez commencer."))
  
  #modify
  observeEvent(input$checkingButton1,{
    user$ready <- user$ready + 1
    if(user$ready == 1){
      output$Only1user <- renderText(paste("Veuillez patientez. Votre duo n'est pas encore là."))
    }
    else if(user$ready == 2){
      output$Only2user <- renderText(paste("Vous êtes à présent deux dans la salle d'attente, et vous pouvez commencer. "))  
    }
  })
  observeEvent(input$checkingButton2,{
    user$ready <- user$ready + 1
    if(user$ready == 1){
      output$Only1user <- renderText(paste("Veuillez patientez. Votre duo n'est pas encore là."))
    }
    else if(user$ready == 2){
      output$Only2user <- renderText(paste("Vous êtes à présent deux dans la salle d'attente, et vous pouvez commencer. "))  
    }
  })
  
  #save datas when clicked on the button next
  observeEvent(input$resultfinal2,{
    saveData(formData())
  })
  observeEvent(input$resultfinal1,{
    saveData(formData())
  })
  scoredata2 <- 0
  observeEvent(input$gt_inst5, {
    if (input$Last_questionnaire1 == "Last_chanceq1"){
      scoredata2 <- scoredata2 + 1
    }
    if (input$Last_questionnaire2 == "Last_chanceq8"){
      scoredata2 <- scoredata2 + 1
    }
    if (input$Last_questionnaire3 == "Last_chanceq10"){
      scoredata2 <- scoredata2 + 1
    }
    if (input$Last_questionnaire4 == "Last_chanceq15"){
      scoredata2 <- scoredata2 + 1
    }
    if (scoredata2 == 4){
      output$ScorePictures4 <- renderText(paste("Votre score est de :",scoredata2,"/ 4. Félicitations. Vous pouvez continuer le test "))  
    }
    else
      output$ScorePictures2 <- renderText(paste("Votre score est de :",scoredata2,"/6.Merci de votre participation.Malheureusement,votre score est trop faible pour pouvoir continuer."))
  }
  )
  # joueur 2 reponse 1
  
  # increment a variable if this answer is selected 
  observeEvent(input$finish2,{
    if(input$phase_choicebis == "Question1Answer1bis"){
      trueanswer$correctAnswer <- 5
    }
  })
  # display the answer of the player himself
  observeEvent(input$finish2,{
    if(trueanswer$correctAnswer == 5){
      output$rep5 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  #display the answer of the other player in the duo
  observeEvent(input$voirreponse,{
    if(trueanswer$correctAnswer == 5){
      output$rep5 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish2,{
    if(input$phase_choicebis == "Question1Answer2bis"){
      trueanswer$correctAnswer <- 6
    }
  })
  observeEvent(input$finish2,{
    if(trueanswer$correctAnswer == 6){
      output$rep6 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse,{
    if(trueanswer$correctAnswer == 6){
      output$rep6 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish2,{
    if(input$phase_choicebis == "Question1Answer3bis"){
      trueanswer$correctAnswer <- 7
      
    }
  })
  observeEvent(input$finish2,{
    if(trueanswer$correctAnswer == 7){
      output$rep7 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse,{
    if(trueanswer$correctAnswer == 7){
      output$rep7 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  observeEvent(input$finish2,{
    if(input$phase_choicebis == "Question1Answer4bis"){
      trueanswer$correctAnswer <- 8
    }
    
  })
  observeEvent(input$finish2,{
    if(trueanswer$correctAnswer == 8){
      output$rep8 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse,{
    if(trueanswer$correctAnswer == 8){
      output$rep8 <- renderText(paste("La réponse de votre duo(en orange)."))  
    }
  })
  #fin joueur 2 reponse 1 debut reponse 1 joueur 1
  observeEvent(input$finish,{
    if(input$phase_choice == "Question1Answer1"){
      trueanswer$correctAnswer2 <- 1
    }
  })
  observeEvent(input$finish,{
    if(trueanswer$correctAnswer2 == 1){
      output$rep1 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse2,{
    if(trueanswer$correctAnswer2 == 1){
      output$rep1 <- renderText(paste("La réponse de votre duo(en orange)."))  
    }
  })
  observeEvent(input$finish,{
    if(input$phase_choice == "Question1Answer2"){
      trueanswer$correctAnswer2 <- 2
    }
  })
  observeEvent(input$finish,{
    if(trueanswer$correctAnswer2 == 2){
      output$rep2 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse2,{
    if(trueanswer$correctAnswer2 == 2){
      output$rep2 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish,{
    if(input$phase_choice == "Question1Answer3"){
      trueanswer$correctAnswer2 <- 3
    }
  })
  observeEvent(input$finish,{
    if(trueanswer$correctAnswer2 == 3){
      output$rep3 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse2,{
    if(trueanswer$correctAnswer2 == 3){
      output$rep3 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish,{
    if(input$phase_choice == "Question1Answer4"){
      trueanswer$correctAnswer2 <- 4
    }
    
  })
  observeEvent(input$finish,{
    if(trueanswer$correctAnswer2 == 4){
      output$rep4 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse2,{
    if(trueanswer$correctAnswer2 == 4){
      output$rep4 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  observeEvent(input$voirreponse,{
    if(trueanswer$correctAnswer + trueanswer$correctAnswer2 ==  6){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/1")) 
    }else if(trueanswer$correctAnswer + trueanswer$correctAnswer2 ==  8){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/1")) 
    }else if(trueanswer$correctAnswer + trueanswer$correctAnswer2 ==  10){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/1")) 
    }else if(trueanswer$correctAnswer + trueanswer$correctAnswer2 ==  12){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/1")) 
    }else 
      output$seeduoscore <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/1")) 
  })
  observeEvent(input$voirreponse,{
    if(globalscore$duoscore == 1){
      output$voirduoscore <- renderText(paste(globalscore$duoscore,"/6")) 
    }
    else if (globalscore$duoscore == 0){
      output$voirduoscore2 <- renderText(paste(globalscore$duoscore,"/6")) 
    }
  })
  observeEvent(input$voirreponse2,{
    if(trueanswer$correctAnswer + trueanswer$correctAnswer2 ==  6){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/1")) 
    }else if(trueanswer$correctAnswer + trueanswer$correctAnswer2 ==  8){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/1")) 
    }else if(trueanswer$correctAnswer + trueanswer$correctAnswer2 ==  10){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/1")) 
    }else if(trueanswer$correctAnswer + trueanswer$correctAnswer2 ==  12){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/1")) 
    }else 
      output$seeduoscore <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/1")) 
  })
  observeEvent(input$voirreponse2,{
    if(globalscore$duoscore2 == 1){
      output$voirduoscore <- renderText(paste(globalscore$duoscore2,"/6")) 
    }
    else if (globalscore$duoscore2 == 0){
      output$voirduoscore2 <- renderText(paste(globalscore$duoscore2,"/6")) 
    }
  })
  #fin reponse 1 joueur 1
  
  # joueur 2 reponse 2
  observeEvent(input$finish4,{
    if(input$phase_choice2bis == "Question2Answer1bis"){
      trueanswer$correctAnswer3 <- 13
    }
  })
  observeEvent(input$finish4,{
    if(trueanswer$correctAnswer3 == 13){
      output$rep13 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse3,{
    if(trueanswer$correctAnswer3 == 13){
      output$rep13 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish4,{
    if(input$phase_choice2bis == "Question2Answer2bis"){
      trueanswer$correctAnswer3 <- 14
    }
  })
  observeEvent(input$finish4,{
    if(trueanswer$correctAnswer3 == 14){
      output$rep14 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse3,{
    if(trueanswer$correctAnswer3 == 14){
      output$rep14 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish4,{
    if(input$phase_choice2bis == "Question2Answer3bis"){
      trueanswer$correctAnswer3 <- 15
      
    }
  })
  observeEvent(input$finish4,{
    if(trueanswer$correctAnswer3 == 15){
      output$rep15 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse3,{
    if(trueanswer$correctAnswer3 == 15){
      output$rep15 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  observeEvent(input$finish4,{
    if(input$phase_choice2bis == "Question2Answer4bis"){
      trueanswer$correctAnswer3 <- 16
    }
    
  })
  observeEvent(input$finish4,{
    if(trueanswer$correctAnswer3 == 16){
      output$rep16 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse3,{
    if(trueanswer$correctAnswer3 == 16){
      output$rep16 <- renderText(paste("La réponse de votre duo(en orange)."))  
    }
  })
  
  #fin joueur 2 reponse 2 debut reponse 2 joueur 1
  observeEvent(input$finish3,{
    if(input$phase_choice2 == "Question2Answer1"){
      trueanswer$correctAnswer4 <- 9
    }
  })
  observeEvent(input$finish3,{
    if(trueanswer$correctAnswer4 == 9){
      output$rep9 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse4,{
    if(trueanswer$correctAnswer4 == 9){
      output$rep9 <- renderText(paste("La réponse de votre duo(en orange)."))  
    }
  })
  observeEvent(input$finish3,{
    if(input$phase_choice2 == "Question2Answer2"){
      trueanswer$correctAnswer4 <- 10
    }
  })
  observeEvent(input$finish3,{
    if(trueanswer$correctAnswer4 == 10){
      output$rep10 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse4,{
    if(trueanswer$correctAnswer4 == 10){
      output$rep10 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish3,{
    if(input$phase_choice2 == "Question2Answer3"){
      trueanswer$correctAnswer4 <- 11
    }
  })
  observeEvent(input$finish3,{
    if(trueanswer$correctAnswer4 == 11){
      output$rep11 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse4,{
    if(trueanswer$correctAnswer4 == 11){
      output$rep11 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish3,{
    if(input$phase_choice2 == "Question2Answer4"){
      trueanswer$correctAnswer4 <- 12
    }
    
  })
  observeEvent(input$finish3,{
    if(trueanswer$correctAnswer4 == 12){
      output$rep12 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse4,{
    if(trueanswer$correctAnswer4 == 12){
      output$rep12 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  observeEvent(input$voirreponse3,{
    if(trueanswer$correctAnswer3 + trueanswer$correctAnswer4 ==  22){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore2 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/2")) 
    }else if(trueanswer$correctAnswer3 + trueanswer$correctAnswer4 ==  24){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore2 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/2")) 
    }else if(trueanswer$correctAnswer3 + trueanswer$correctAnswer4 ==  26){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore2 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/2")) 
    }else if(trueanswer$correctAnswer3 + trueanswer$correctAnswer4 ==  28){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore2 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/2")) 
    }else 
      output$seeduoscore2 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/2")) 
  })
  observeEvent(input$voirreponse3,{
    if(globalscore$duoscore == 2){
      output$voirduoscore3 <- renderText(paste(globalscore$duoscore,"/2")) 
    }
    if(globalscore$duoscore == 1){
      output$voirduoscore <- renderText(paste(globalscore$duoscore,"/2")) 
    }
    else if (globalscore$duoscore == 0){
      output$voirduoscore2 <- renderText(paste(globalscore$duoscore,"/2")) 
    }
  })
  observeEvent(input$voirreponse4,{
    if(trueanswer$correctAnswer3 + trueanswer$correctAnswer4 ==  22){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore2 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/2")) 
    }else if(trueanswer$correctAnswer3 + trueanswer$correctAnswer4 ==  24){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore2 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/2")) 
    }else if(trueanswer$correctAnswer3 + trueanswer$correctAnswer4 ==  26){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore2 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/2")) 
    }else if(trueanswer$correctAnswer3 + trueanswer$correctAnswer4 ==  28){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore2 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/2")) 
    }else 
      output$seeduoscore2 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/2")) 
  })
  observeEvent(input$voirreponse4,{
    if(globalscore$duoscore2 == 2){
      output$voirduoscore3 <- renderText(paste(globalscore$duoscore2,"/2")) 
    }
    if(globalscore$duoscore2 == 1){
      output$voirduoscore <- renderText(paste(globalscore$duoscore2,"/2")) 
    }
    else if (globalscore$duoscore2 == 0){
      output$voirduoscore2 <- renderText(paste(globalscore$duoscore2,"/2")) 
    }
  })
  #fin reponse 2 joueur 1
  
  # joueur 2 reponse 3
  observeEvent(input$finish6,{
    if(input$phase_choice3bis == "Question3Answer1bis"){
      trueanswer$correctAnswer5 <- 21
    }
  })
  observeEvent(input$finish6,{
    if(trueanswer$correctAnswer5 == 21){
      output$rep21 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse5,{
    if(trueanswer$correctAnswer5 == 21){
      output$rep21 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish6,{
    if(input$phase_choice3bis == "Question3Answer2bis"){
      trueanswer$correctAnswer5 <- 22
    }
  })
  observeEvent(input$finish6,{
    if(trueanswer$correctAnswer5 == 22){
      output$rep22 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse5,{
    if(trueanswer$correctAnswer5 == 22){
      output$rep22 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish6,{
    if(input$phase_choice3bis == "Question3Answer3bis"){
      trueanswer$correctAnswer5 <- 23
      
    }
  })
  observeEvent(input$finish6,{
    if(trueanswer$correctAnswer5 == 23){
      output$rep23 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse5,{
    if(trueanswer$correctAnswer5 == 23){
      output$rep23 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  observeEvent(input$finish6,{
    if(input$phase_choice3bis == "Question3Answer4bis"){
      trueanswer$correctAnswer5 <- 24
    }
    
  })
  observeEvent(input$finish6,{
    if(trueanswer$correctAnswer5 == 24){
      output$rep24 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse5,{
    if(trueanswer$correctAnswer5 == 24){
      output$rep24 <- renderText(paste("La réponse de votre duo(en orange)."))  
    }
  })
  
  #fin joueur 2 reponse 3 debut reponse 3 joueur 1
  observeEvent(input$finish5,{
    if(input$phase_choice3 == "Question3Answer1"){
      trueanswer$correctAnswer7 <- 17
    }
  })
  observeEvent(input$finish5,{
    if(trueanswer$correctAnswer7 == 17){
      output$rep17 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse6,{
    if(trueanswer$correctAnswer7 == 17){
      output$rep17 <- renderText(paste("La réponse de votre duo(en orange)."))  
    }
  })
  observeEvent(input$finish5,{
    if(input$phase_choice3 == "Question3Answer2"){
      trueanswer$correctAnswer7 <- 18
    }
  })
  observeEvent(input$finish5,{
    if(trueanswer$correctAnswer7 == 18){
      output$rep18 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse6,{
    if(trueanswer$correctAnswer7 == 18){
      output$rep18 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish5,{
    if(input$phase_choice3 == "Question3Answer3"){
      trueanswer$correctAnswer7 <- 19
    }
  })
  observeEvent(input$finish5,{
    if(trueanswer$correctAnswer7 == 19){
      output$rep19 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse6,{
    if(trueanswer$correctAnswer7 == 19){
      output$rep19 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish5,{
    if(input$phase_choice3 == "Question3Answer4"){
      trueanswer$correctAnswer7 <- 20
    }
    
  })
  observeEvent(input$finish5,{
    if(trueanswer$correctAnswer7 == 20){
      output$rep20 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse6,{
    if(trueanswer$correctAnswer7 == 20){
      output$rep20 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  #fin reponse 3 joueur 1
  observeEvent(input$voirreponse5,{
    if(trueanswer$correctAnswer5 + trueanswer$correctAnswer7 ==  38){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore3 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/3")) 
    }else if(trueanswer$correctAnswer5 + trueanswer$correctAnswer7 ==  40){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore3 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/3")) 
    }else if(trueanswer$correctAnswer5 + trueanswer$correctAnswer7 ==  42){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore3 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/3")) 
    }else if(trueanswer$correctAnswer5 + trueanswer$correctAnswer7 ==  44){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore3 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/3")) 
    }else 
      output$seeduoscore3 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/3")) 
  })
  observeEvent(input$voirreponse5,{
    if(globalscore$duoscore == 3){
      output$voirduoscore4 <- renderText(paste(globalscore$duoscore,"/3")) 
    }
    if(globalscore$duoscore == 2){
      output$voirduoscore3 <- renderText(paste(globalscore$duoscore,"/3")) 
    }
    if(globalscore$duoscore == 1){
      output$voirduoscore <- renderText(paste(globalscore$duoscore,"/3")) 
    }
    else if (globalscore$duoscore == 0){
      output$voirduoscore2 <- renderText(paste(globalscore$duoscore,"/3")) 
    }
  })
  observeEvent(input$voirreponse6,{
    if(trueanswer$correctAnswer5 + trueanswer$correctAnswer7 ==  38){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore3 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/3")) 
    }else if(trueanswer$correctAnswer5 + trueanswer$correctAnswer7 ==  40){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore3 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/3")) 
    }else if(trueanswer$correctAnswer5 + trueanswer$correctAnswer7 ==  42){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore3 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/3")) 
    }else if(trueanswer$correctAnswer5 + trueanswer$correctAnswer7 ==  44){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore3 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/3")) 
    }else 
      output$seeduoscore3 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/3")) 
  })
  observeEvent(input$voirreponse6,{
    if(globalscore$duoscore2 == 3){
      output$voirduoscore4 <- renderText(paste(globalscore$duoscore2,"/3")) 
    }
    if(globalscore$duoscore2 == 2){
      output$voirduoscore3 <- renderText(paste(globalscore$duoscore2,"/3")) 
    }
    if(globalscore$duoscore2 == 1){
      output$voirduoscore <- renderText(paste(globalscore$duoscore2,"/3")) 
    }
    else if (globalscore$duoscore2 == 0){
      output$voirduoscore2 <- renderText(paste(globalscore$duoscore2,"/3")) 
    }
  })
  
  # joueur 2 reponse 4
  observeEvent(input$finish8,{
    if(input$phase_choice4bis == "Question4Answer1bis"){
      trueanswer$correctAnswer8 <- 29
    }
  })
  observeEvent(input$finish8,{
    if(trueanswer$correctAnswer8 == 29){
      output$rep29 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse7,{
    if(trueanswer$correctAnswer8 == 29){
      output$rep29 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish8,{
    if(input$phase_choice4bis == "Question4Answer2bis"){
      trueanswer$correctAnswer8 <- 30
    }
  })
  observeEvent(input$finish8,{
    if(trueanswer$correctAnswer8 == 30){
      output$rep30 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse7,{
    if(trueanswer$correctAnswer8 == 30){
      output$rep30 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish8,{
    if(input$phase_choice4bis == "Question4Answer3bis"){
      trueanswer$correctAnswer8 <- 31
      
    }
  })
  observeEvent(input$finish8,{
    if(trueanswer$correctAnswer8 == 31){
      output$rep31 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse7,{
    if(trueanswer$correctAnswer8 == 31){
      output$rep31 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  observeEvent(input$finish8,{
    if(input$phase_choice4bis == "Question4Answer4bis"){
      trueanswer$correctAnswer8 <- 32
    }
    
  })
  observeEvent(input$finish8,{
    if(trueanswer$correctAnswer8 == 32){
      output$rep32 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse7,{
    if(trueanswer$correctAnswer8 == 32){
      output$rep32 <- renderText(paste("La réponse de votre duo(en orange)."))  
    }
  })
  
  #fin joueur 2 reponse 4 debut reponse 4 joueur 1
  observeEvent(input$finish7,{
    if(input$phase_choice4 == "Question4Answer1"){
      trueanswer$correctAnswer9 <- 25
    }
  })
  observeEvent(input$finish7,{
    if(trueanswer$correctAnswer9 == 25){
      output$rep25 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse8,{
    if(trueanswer$correctAnswer9 == 25){
      output$rep25 <- renderText(paste("La réponse de votre duo(en orange)."))  
    }
  })
  observeEvent(input$finish7,{
    if(input$phase_choice4 == "Question4Answer2"){
      trueanswer$correctAnswer9 <- 26
    }
  })
  observeEvent(input$finish7,{
    if(trueanswer$correctAnswer9 == 26){
      output$rep26 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse8,{
    if(trueanswer$correctAnswer9 == 26){
      output$rep26 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish7,{
    if(input$phase_choice4 == "Question4Answer3"){
      trueanswer$correctAnswer9 <- 27
    }
  })
  observeEvent(input$finish7,{
    if(trueanswer$correctAnswer9 == 27){
      output$rep27 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse8,{
    if(trueanswer$correctAnswer9 == 27){
      output$rep27 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish7,{
    if(input$phase_choice4 == "Question4Answer4"){
      trueanswer$correctAnswer9 <- 28
    }
    
  })
  observeEvent(input$finish7,{
    if(trueanswer$correctAnswer9 == 28){
      output$rep28 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse8,{
    if(trueanswer$correctAnswer9 == 28){
      output$rep28 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  observeEvent(input$voirreponse7,{
    if(trueanswer$correctAnswer8 + trueanswer$correctAnswer9 ==  54){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore4 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/4")) 
    }else if(trueanswer$correctAnswer8 + trueanswer$correctAnswer9 ==  56){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore4 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/4")) 
    }else if(trueanswer$correctAnswer8 + trueanswer$correctAnswer9 ==  58){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore4 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/4")) 
    }else if(trueanswer$correctAnswer8 + trueanswer$correctAnswer9 ==  60){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore4 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/4")) 
    }else 
      output$seeduoscore4 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/4")) 
  })
  observeEvent(input$voirreponse7,{
    if(globalscore$duoscore == 4){
      output$voirduoscore5 <- renderText(paste(globalscore$duoscore,"/4")) 
    }
    if(globalscore$duoscore == 3){
      output$voirduoscore4 <- renderText(paste(globalscore$duoscore,"/4")) 
    }
    if(globalscore$duoscore == 2){
      output$voirduoscore3 <- renderText(paste(globalscore$duoscore,"/4")) 
    }
    if(globalscore$duoscore == 1){
      output$voirduoscore <- renderText(paste(globalscore$duoscore,"/4")) 
    }
    else if (globalscore$duoscore == 0){
      output$voirduoscore2 <- renderText(paste(globalscore$duoscore,"/4")) 
    }
  })
  observeEvent(input$voirreponse8,{
    if(trueanswer$correctAnswer8 + trueanswer$correctAnswer9 ==  54){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore4 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/4")) 
    }else if(trueanswer$correctAnswer8 + trueanswer$correctAnswer9 ==  56){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore4 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/4")) 
    }else if(trueanswer$correctAnswer8 + trueanswer$correctAnswer9 ==  58){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore4 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/4")) 
    }else if(trueanswer$correctAnswer8 + trueanswer$correctAnswer9 ==  60){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore4 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/4")) 
    }else 
      output$seeduoscore4 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/4")) 
  })
  observeEvent(input$voirreponse8,{
    if(globalscore$duoscore2 == 4){
      output$voirduoscore5 <- renderText(paste(globalscore$duoscore2,"/4")) 
    }
    if(globalscore$duoscore2 == 3){
      output$voirduoscore4 <- renderText(paste(globalscore$duoscore2,"/4")) 
    }
    if(globalscore$duoscore2 == 2){
      output$voirduoscore3 <- renderText(paste(globalscore$duoscore2,"/4")) 
    }
    if(globalscore$duoscore2 == 1){
      output$voirduoscore <- renderText(paste(globalscore$duoscore2,"/4")) 
    }
    else if (globalscore$duoscore2 == 0){
      output$voirduoscore2 <- renderText(paste(globalscore$duoscore2,"/4")) 
    }
  })
  #fin reponse 4 joueur 1
  
  # joueur 2 reponse 5
  observeEvent(input$finish10,{
    if(input$phase_choice5bis == "Question5Answer1bis"){
      trueanswer$correctAnswer10 <- 37
    }
  })
  observeEvent(input$finish10,{
    if(trueanswer$correctAnswer10 == 37){
      output$rep37 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse9,{
    if(trueanswer$correctAnswer10 == 37){
      output$rep37 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish10,{
    if(input$phase_choice5bis == "Question5Answer2bis"){
      trueanswer$correctAnswer10 <- 38
    }
  })
  observeEvent(input$finish10,{
    if(trueanswer$correctAnswer10 == 38){
      output$rep38 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse9,{
    if(trueanswer$correctAnswer10 == 38){
      output$rep38 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish10,{
    if(input$phase_choice5bis == "Question5Answer3bis"){
      trueanswer$correctAnswer10 <- 39
      
    }
  })
  observeEvent(input$finish10,{
    if(trueanswer$correctAnswer10 == 39){
      output$rep39 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse9,{
    if(trueanswer$correctAnswer10 == 39){
      output$rep39 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  observeEvent(input$finish10,{
    if(input$phase_choice5bis == "Question5Answer4bis"){
      trueanswer$correctAnswer10 <- 40
    }
    
  })
  observeEvent(input$finish10,{
    if(trueanswer$correctAnswer10 == 40){
      output$rep40 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse9,{
    if(trueanswer$correctAnswer10 == 40){
      output$rep40 <- renderText(paste("La réponse de votre duo(en orange)."))  
    }
  })
  
  #fin joueur 2 reponse 4 debut reponse 4 joueur 1
  observeEvent(input$finish9,{
    if(input$phase_choice5 == "Question5Answer1"){
      trueanswer$correctAnswer11 <- 33
    }
  })
  observeEvent(input$finish9,{
    if(trueanswer$correctAnswer11 == 33){
      output$rep33 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse10,{
    if(trueanswer$correctAnswer11 == 33){
      output$rep33 <- renderText(paste("La réponse de votre duo(en orange)."))  
    }
  })
  observeEvent(input$finish9,{
    if(input$phase_choice5 == "Question5Answer2"){
      trueanswer$correctAnswer11 <- 34
    }
  })
  observeEvent(input$finish9,{
    if(trueanswer$correctAnswer11 == 34){
      output$rep34 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse10,{
    if(trueanswer$correctAnswer11 == 34){
      output$rep34 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish9,{
    if(input$phase_choice5 == "Question5Answer3"){
      trueanswer$correctAnswer11 <- 35
    }
  })
  observeEvent(input$finish9,{
    if(trueanswer$correctAnswer11 == 35){
      output$rep35 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse10,{
    if(trueanswer$correctAnswer11 == 35){
      output$rep35 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish9,{
    if(input$phase_choice5 == "Question5Answer4"){
      trueanswer$correctAnswer11 <- 36
    }
    
  })
  observeEvent(input$finish9,{
    if(trueanswer$correctAnswer11 == 36){
      output$rep36 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse10,{
    if(trueanswer$correctAnswer11 == 36){
      output$rep36 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  observeEvent(input$voirreponse9,{
    if(trueanswer$correctAnswer10 + trueanswer$correctAnswer11 ==  70){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore5 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/5")) 
    }else if(trueanswer$correctAnswer10 + trueanswer$correctAnswer11 ==  72){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore5 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/5")) 
    }else if(trueanswer$correctAnswer10 + trueanswer$correctAnswer11 ==  74){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore5 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/5")) 
    }else if(trueanswer$correctAnswer10 + trueanswer$correctAnswer11 ==  76){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore5 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/5")) 
    }else 
      output$seeduoscore5 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/5")) 
  })
  observeEvent(input$voirreponse9,{
    if(globalscore$duoscore == 5){
      output$voirduoscore6 <- renderText(paste(globalscore$duoscore,"/5")) 
    }
    if(globalscore$duoscore == 4){
      output$voirduoscore5 <- renderText(paste(globalscore$duoscore,"/5")) 
    }
    if(globalscore$duoscore == 3){
      output$voirduoscore4 <- renderText(paste(globalscore$duoscore,"/5")) 
    }
    if(globalscore$duoscore == 2){
      output$voirduoscore3 <- renderText(paste(globalscore$duoscore,"/5")) 
    }
    if(globalscore$duoscore == 1){
      output$voirduoscore <- renderText(paste(globalscore$duoscore,"/5")) 
    }
    else if (globalscore$duoscore == 0){
      output$voirduoscore2 <- renderText(paste(globalscore$duoscore,"/5")) 
    }
  })
  observeEvent(input$voirreponse10,{
    if(trueanswer$correctAnswer10 + trueanswer$correctAnswer11 ==  70){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore5 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/5")) 
    }else if(trueanswer$correctAnswer10 + trueanswer$correctAnswer11 ==  72){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore5 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/5")) 
    }else if(trueanswer$correctAnswer10 + trueanswer$correctAnswer11 ==  74){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore5 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/5")) 
    }else if(trueanswer$correctAnswer10 + trueanswer$correctAnswer11 ==  76){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore5 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/5")) 
    }else 
      output$seeduoscore5 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/5")) 
  })
  observeEvent(input$voirreponse10,{
    if(globalscore$duoscore2 == 5){
      output$voirduoscore6 <- renderText(paste(globalscore$duoscore2,"/5")) 
    }
    if(globalscore$duoscore2 == 4){
      output$voirduoscore5 <- renderText(paste(globalscore$duoscore2,"/5")) 
    }
    if(globalscore$duoscore2 == 3){
      output$voirduoscore4 <- renderText(paste(globalscore$duoscore2,"/5")) 
    }
    if(globalscore$duoscore2 == 2){
      output$voirduoscore3 <- renderText(paste(globalscore$duoscore2,"/5")) 
    }
    if(globalscore$duoscore2 == 1){
      output$voirduoscore <- renderText(paste(globalscore$duoscore2,"/5")) 
    }
    else if (globalscore$duoscore2 == 0){
      output$voirduoscore2 <- renderText(paste(globalscore$duoscore2,"/5")) 
    }
  })
  #fin reponse 4 joueur 1
  
  # joueur 2 reponse 6
  observeEvent(input$finish12,{
    if(input$phase_choice6bis == "Question6Answer1bis"){
      trueanswer$correctAnswer12 <- 45
    }
  })
  observeEvent(input$finish12,{
    if(trueanswer$correctAnswer12 == 45){
      output$rep45 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse11,{
    if(trueanswer$correctAnswer12 == 45){
      output$rep45 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish12,{
    if(input$phase_choice6bis == "Question6Answer2bis"){
      trueanswer$correctAnswer12 <- 46
    }
  })
  observeEvent(input$finish12,{
    if(trueanswer$correctAnswer12 == 46){
      output$rep46 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse11,{
    if(trueanswer$correctAnswer12 == 46){
      output$rep46 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish12,{
    if(input$phase_choice6bis == "Question6Answer3bis"){
      trueanswer$correctAnswer12 <- 47
      
    }
  })
  observeEvent(input$finish12,{
    if(trueanswer$correctAnswer12 == 47){
      output$rep47 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse11,{
    if(trueanswer$correctAnswer12 == 47){
      output$rep47 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  observeEvent(input$finish12,{
    if(input$phase_choice6bis == "Question6Answer4bis"){
      trueanswer$correctAnswer12 <- 48
    }
    
  })
  observeEvent(input$finish12,{
    if(trueanswer$correctAnswer12 == 48){
      output$rep48 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse11,{
    if(trueanswer$correctAnswer12 == 48){
      output$rep48 <- renderText(paste("La réponse de votre duo(en orange)."))  
    }
  })
  
  #fin joueur 2 reponse 4 debut reponse 4 joueur 1
  observeEvent(input$finish11,{
    if(input$phase_choice6 == "Question6Answer1"){
      trueanswer$correctAnswer13 <- 41
    }
  })
  observeEvent(input$finish11,{
    if(trueanswer$correctAnswer13 == 41){
      output$rep41 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse12,{
    if(trueanswer$correctAnswer13 == 41){
      output$rep41 <- renderText(paste("La réponse de votre duo(en orange)."))  
    }
  })
  observeEvent(input$finish11,{
    if(input$phase_choice6 == "Question6Answer2"){
      trueanswer$correctAnswer13 <- 42
    }
  })
  observeEvent(input$finish11,{
    if(trueanswer$correctAnswer13 == 42){
      output$rep42 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse12,{
    if(trueanswer$correctAnswer13 == 42){
      output$rep42 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish11,{
    if(input$phase_choice6 == "Question6Answer3"){
      trueanswer$correctAnswer13 <- 43
    }
  })
  observeEvent(input$finish11,{
    if(trueanswer$correctAnswer13 == 43){
      output$rep43 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse12,{
    if(trueanswer$correctAnswer13 == 43){
      output$rep43 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  
  observeEvent(input$finish11,{
    if(input$phase_choice6 == "Question6Answer4"){
      trueanswer$correctAnswer13 <- 44
    }
    
  })
  observeEvent(input$finish11,{
    if(trueanswer$correctAnswer13 == 44){
      output$rep44 <- renderText(paste("Votre réponse(en vert).")) 
    }  
  })
  observeEvent(input$voirreponse12,{
    if(trueanswer$correctAnswer13 == 44){
      output$rep44 <- renderText(paste("La réponse de votre duo(en orange).")) 
    }
  })
  observeEvent(input$voirreponse11,{
    if(trueanswer$correctAnswer12 + trueanswer$correctAnswer13 ==  86){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore6 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/6")) 
    }else if(trueanswer$correctAnswer12 + trueanswer$correctAnswer13 ==  88){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore6 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/6")) 
    }else if(trueanswer$correctAnswer12 + trueanswer$correctAnswer13 ==  90){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore6 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/6")) 
    }else if(trueanswer$correctAnswer12 + trueanswer$correctAnswer13 ==  92){
      globalscore$duoscore = globalscore$duoscore + 1
      output$seeduoscore6 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/6")) 
    }else 
      output$seeduoscore6 <- renderText(paste("Le score du duo est de :",globalscore$duoscore,"/6")) 
  })
  observeEvent(input$voirreponse11,{
    if(globalscore$duoscore == 6){
      output$voirduoscore6 <- renderText(paste(globalscore$duoscore,"/6")) 
    }
    if(globalscore$duoscore == 5){
      output$voirduoscore6 <- renderText(paste(globalscore$duoscore,"/6")) 
    }
    if(globalscore$duoscore == 4){
      output$voirduoscore5 <- renderText(paste(globalscore$duoscore,"/6")) 
    }
    if(globalscore$duoscore == 3){
      output$voirduoscore4 <- renderText(paste(globalscore$duoscore,"/6")) 
    }
    if(globalscore$duoscore == 2){
      output$voirduoscore3 <- renderText(paste(globalscore$duoscore,"/6")) 
    }
    if(globalscore$duoscore == 1){
      output$voirduoscore <- renderText(paste(globalscore$duoscore,"/6")) 
    }
    else if (globalscore$duoscore == 0){
      output$voirduoscore2 <- renderText(paste(globalscore$duoscore,"/6")) 
    }
  })
  observeEvent(input$voirreponse12,{
    if(trueanswer$correctAnswer12 + trueanswer$correctAnswer13 ==  86){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore6 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/6")) 
    }else if(trueanswer$correctAnswer12 + trueanswer$correctAnswer13 ==  88){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore6 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/6")) 
    }else if(trueanswer$correctAnswer12 + trueanswer$correctAnswer13 ==  90){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore6 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/6")) 
    }else if(trueanswer$correctAnswer12 + trueanswer$correctAnswer13 ==  92){
      globalscore$duoscore2 = globalscore$duoscore2 + 1
      output$seeduoscore6 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/6")) 
    }else 
      output$seeduoscore6 <- renderText(paste("Le score du duo est de :",globalscore$duoscore2,"/6")) 
  })
  observeEvent(input$voirreponse12,{
    if(globalscore$duoscore2 == 6){
      output$voirduoscore6 <- renderText(paste(globalscore$duoscore2,"/6")) 
    }
    if(globalscore$duoscore2 == 5){
      output$voirduoscore6 <- renderText(paste(globalscore$duoscore2,"/6")) 
    }
    if(globalscore$duoscore2 == 4){
      output$voirduoscore5 <- renderText(paste(globalscore$duoscore2,"/6")) 
    }
    if(globalscore$duoscore2 == 3){
      output$voirduoscore4 <- renderText(paste(globalscore$duoscore2,"/6")) 
    }
    if(globalscore$duoscore2 == 2){
      output$voirduoscore3 <- renderText(paste(globalscore$duoscore2,"/6")) 
    }
    if(globalscore$duoscore2 == 1){
      output$voirduoscore <- renderText(paste(globalscore$duoscore2,"/6")) 
    }
    else if (globalscore$duoscore2 == 0){
      output$voirduoscore2 <- renderText(paste(globalscore$duoscore2,"/6")) 
    }
  })
  
  
  
  scoredata1 <- 0
  observeEvent(input$gt_inst4, {
    if (input$test_phase == "Test1Question1Answer2"){
      
      scoredata1 <- scoredata1 + 1
    }
    if (input$test_phase2 == "Test1Question2Answer4"){
      scoredata1 <- scoredata1 + 1
    }
    if (input$test_phase3 == "Test1Question3Answer3"){
      scoredata1 <- scoredata1 + 1
    }
    if (input$test_phase4 == "Test1Question4Answer4"){
      scoredata1 <- scoredata1 + 1
    }
    if (input$test_phase5 == "Test1Question5Answer1"){
      scoredata1 <- scoredata1 + 1
    }
    if (input$test_phase6 == "Test1Question6Answer3"){
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
      output$ScorePictures2 <- renderText(paste("Votre score est de :",scoredata1,"/6.Merci de votre participation.Malheureusement,votre score est trop faible pour pouvoir continuer."))
    }
  })
  
  observeEvent(input$q4, {  #Check whether an input has been made:
    score <- 0
    if (input$q1 == "française.") {
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
    if (input$q4 != "l'automne."){
      
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
    if (score == 6){
      output$headphone_check <- renderText(paste("Votre score est de :",score,"/ 6. Bravo vous pouvez continuer."))
    }
    if (score == 5){
      output$headphone_check2 <- renderText(paste("Votre score est de :",score,"/ 6. Bravo vous pouvez continuer."))  
    }
    else if (score == 4){
      output$headphone_check3 <- renderText(paste("Votre score est de :",score,"/ 6. Malheuresement il n'est pas suffisant pour pouvoir continuer."))
    }
    else if (score < 4){
      output$headphone_check3 <- renderText(paste("Votre score est de :",score,"/ 6. Malheuresement il n'est pas suffisant pour pouvoir continuer."))
    }
  })
  #azerty 
  observeEvent(input$test_Geo,{
    CurrentValues$page <- "testGeo"
  })
  observeEvent(input$waiting_Room,{
    CurrentValues$page <- "waitingRoom"
  })
  observeEvent(input$after_Welcome,{
    CurrentValues$page <- "afterWelcome"
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
  
  observeEvent(input$earphones_Quality, {  #Check whether an input has been made:
    CurrentValues$page <- "earphonesQuality"
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
  observeEvent(input$resultat_Test_Sons, {  #Check whether an input has been made:
    CurrentValues$page <- "resultatTestSons"
  }
  )
  observeEvent(input$L_survey,  {
    CurrentValues$page <- "Learning_survey"
  }
  )
  observeEvent(input$L1_survey,  {
    CurrentValues$page <- "Learning1_survey"
  }
  )
  observeEvent(input$L2_survey,  {
    CurrentValues$page <- "Learning2_survey"
  }
  )
  observeEvent(input$L3_survey,  {
    CurrentValues$page <- "Learning3_survey"
  }
  )
  observeEvent(input$L4_survey,  {
    CurrentValues$page <- "Learning4_survey"
  }
  )
  observeEvent(input$L5_survey,  {
    CurrentValues$page <- "Learning5_survey"
  }
  )
  observeEvent(input$L6_survey,  {
    CurrentValues$page <- "Learning6_survey"
  }
  )
  observeEvent(input$L7_survey,  {
    CurrentValues$page <- "Learning7_survey"
  }
  )
  observeEvent(input$L8_survey,  {
    CurrentValues$page <- "Learning8_survey"
  }
  )
  observeEvent(input$L9_survey,  {
    CurrentValues$page <- "Learning9_survey"
  }
  )
  observeEvent(input$L10_survey,  {
    CurrentValues$page <- "Learning10_survey"
  }
  )
  observeEvent(input$L11_survey,  {
    CurrentValues$page <- "Learning11_survey"
  }
  )
  observeEvent(input$L12_survey,  {
    CurrentValues$page <- "Learning12_survey"
  }
  )
  observeEvent(input$L13_survey,  {
    CurrentValues$page <- "Learning13_survey"
  }
  )
  observeEvent(input$L14_survey,  {
    CurrentValues$page <- "Learning14_survey"
  }
  )
  observeEvent(input$L15_survey,  {
    CurrentValues$page <- "Learning15_survey"
  }
  )
  observeEvent(input$L16_survey,  {
    CurrentValues$page <- "Learning16_survey"
  }
  )
  observeEvent(input$phase_Questionnaire,  {
    CurrentValues$page <- "phaseQuestionnaire"
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
    CurrentValues$page <- "Question1Answer3"
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
  observeEvent(input$index_test,{
    CurrentValues$page <- "itest"
  })
  observeEvent(input$index_test1,{
    CurrentValues$page <- "test1"
  })
  observeEvent(input$index_test2,{
    CurrentValues$page <- "test2"
  })
  observeEvent(input$index_test3,{
    CurrentValues$page <- "test3"
  })
  observeEvent(input$index_test4,{
    CurrentValues$page <- "test4"
  })
  observeEvent(input$index_test5,{
    CurrentValues$page <- "test5"
  })
  observeEvent(input$index_test6,{
    CurrentValues$page <- "test6"
  })
  observeEvent(input$test_multi, {  #Check whether an input has been made:
    CurrentValues$page <- "testmulti"
  }
  )
  observeEvent(input$finish, {  #Check whether an input has been made:
    CurrentValues$page <- "result_1"
  }
  )
  observeEvent(input$finish2, {  #Check whether an input has been made:
    CurrentValues$page <- "result_6"
  }
  )
  observeEvent(input$result, {  #Check whether an input has been made:
    CurrentValues$page <- "finish_3"
  }
  )
  observeEvent(input$result6, {  #Check whether an input has been made:
    CurrentValues$page <- "finish_4"
  }
  )
  observeEvent(input$finish3, {  #Check whether an input has been made:
    CurrentValues$page <- "result_2"
  }
  )
  observeEvent(input$finish4, {  #Check whether an input has been made:
    CurrentValues$page <- "result_7"
  }
  )
  
  observeEvent(input$result2, {  #Check whether an input has been made:
    CurrentValues$page <- "finish_5"
  }
  )
  observeEvent(input$result7, {  #Check whether an input has been made:
    CurrentValues$page <- "finish_6"
  }
  )
  observeEvent(input$finish5, {  #Check whether an input has been made:
    CurrentValues$page <- "result_3"
  }
  )
  observeEvent(input$finish6, {  #Check whether an input has been made:
    CurrentValues$page <- "result_8"
  }
  )
  observeEvent(input$result3, {  #Check whether an input has been made:
    CurrentValues$page <- "finish_7"
  }
  )
  observeEvent(input$result8, {  #Check whether an input has been made:
    CurrentValues$page <- "finish_8"
  }
  )
  
  observeEvent(input$finish7, {  #Check whether an input has been made:
    CurrentValues$page <- "result_4"
  }
  )
  observeEvent(input$finish8, {  #Check whether an input has been made:
    CurrentValues$page <- "result_9"
  }
  )
  observeEvent(input$result4, {  #Check whether an input has been made:
    CurrentValues$page <- "finish_9"
  }
  )
  observeEvent(input$result9, {  #Check whether an input has been made:
    CurrentValues$page <- "finish_10"
  }
  )
  observeEvent(input$finish9, {  #Check whether an input has been made:
    CurrentValues$page <- "result_5"
  }
  )
  observeEvent(input$finish10, {  #Check whether an input has been made:
    CurrentValues$page <- "result_10"
  }
  )
  observeEvent(input$result5, {  #Check whether an input has been made:
    CurrentValues$page <- "finish_11"
  }
  )
  observeEvent(input$result10, {  #Check whether an input has been made:
    CurrentValues$page <- "finish_12"
  }
  )
  observeEvent(input$finish11, {  #Check whether an input has been made:
    CurrentValues$page <- "result_12"
  }
  )
  observeEvent(input$finish12, {  #Check whether an input has been made:
    CurrentValues$page <- "result_11"
  }
  )
  observeEvent(input$resultfinal1, {  #Check whether an input has been made:
    CurrentValues$page <- "result_final1"
  }
  )
  observeEvent(input$resultfinal2, {  #Check whether an input has been made:
    CurrentValues$page <- "result_final2"
  }
  )
  observeEvent(input$resultats_finaux, {  #Check whether an input has been made:
    CurrentValues$page <- "resultatsfinaux"
  }
  )
  
} # end of server

shinyApp(ui = ui, server = server)
