library(dplyr)
library(ggplot2)
library(grid)
library(shiny)
library(shinythemes)
library(plotly)
library(readxl)
library(DT)
library(patchwork)
# ___________________#
# column( 2, icon("paw","fa-5x"), align="center")


# options(shiny.launch.browser = .rs.invokeShinyPaneViewer)




# __________DEBUT APP_________#

ui <- fluidPage(includeScript("sliderU.js"), includeScript("sliderS.js"), includeScript("sliderF.js"), includeScript("sliderC.js"),
  theme = shinytheme("cerulean"),
  titlePanel(title = div(icon("paw"), "CABIAS", tags$sup("TM"), icon("paw"))),
  navbarPage("Cat Behaviour Issues Assessment Scale",
    id = "inTabset",

    # Tab "Home"
    tabPanel("Home",
      icon = icon("home"), value = "panel1",
      tags$style(".fa-database {color:#E87722}"),
      h3(p(em("Introduction"), lib = "font-awesome"), style = "color:black;text-align:center"),
      fluidRow(
        column(1),
        column(
          10,
          p("On this webpage you can evaluate your cat’s behaviour using the CABIAS", tags$sup("TM"), " developed by Ceva Santé Animale.",
            "For this you will be asked to answer questions concerning your cat problem behaviour such as scratching, urine marking, inter-cat cohabitation and fear the week preceding this questionnaire.",
            br(),
            strong("Please carefully read the following definition of each problem behaviour that will be assessed."),
            style = "text-align:justify;color:#2F2763;background-color:lavender;padding:15px;border-radius:10px"
          )
        ),
        column(1)
      ),
      fluidRow(
        column(2, img(src = "Uimage.jpg", height = 80, width = 80, style = "display: block; margin-left: 30px; ")),
        column(8, p(strong("Urine marking "), "is defined as depositing urine in a standing position INDOORS against a vertical surface oustide the litter box.",
          style = "text-align:justify;color:lavender;background-color:#2F2763;padding:15px;border-radius:10px"
        ))
      ),
      fluidRow(
        column(2),
        column(8, p(strong("Undesirable scratching "), "is defined as g on vertical surfaces INDOORS other than the scratching post (such as sofa, carpet, curtains, furniture ...).",
          style = "text-align:justify;color:lavender;background-color:#2F2763;padding:15px;border-radius:10px"
        )),
        column(2, img(src = "Simage.jpg", height = 80, width = 80, style = "text-align:justify"))
      ),
      fluidRow(
        column(2, img(src = "Fimage.jpg", height = 80, width = 80, style = "display: block; margin-left: 30px; ")),
        column(8, p(strong("Excessive Fear"), "in a cat is defined as hiding or running away.",
          style = "text-align:justify;color:lavender;background-color:#2F2763;padding:15px;border-radius:10px"
        ))
      ),
      fluidRow(
        column(2),
        column(8, p(strong("Difficulties of cohabitation"), "is defined as Difficulties of cohabitation with other cat of the household INDOOR (cats fighting, biting, conflict, cry, chasing, staring, hissing, blocking ...).",
          style = "text-align:justify;color:lavender;background-color:#2F2763;padding:15px;border-radius:10px"
        )),
        column(2, img(src = "Cimage.jpg", height = 80, width = 80, style = "text-align:justify"))
      ),
      fluidRow(
        column(1),
        column(
          10,
          p("The CABIAS", tags$sup("TM"), "  scale is an original Ceva creation that allow reliable evaluation of the cat’s behaviour directly by the owner.",
            "This scale uses an index Score combining aspects of frequency and intensity of the behaviour problem.",
            style = "text-align:justify;color:#2F2763;background-color:lavender;padding:15px;border-radius:10px"
          ),
          br(),
          p("The original article was published on ANIMALS journal on September 21, 2023 → ", a(href = "https://www.mdpi.com/2076-2615/13/18/2992", icon("newspaper"), target = "_blank"), style = "text-align:center;color:#2F2763;background-color:lavender")
        )
      ),

      ######  Bouton "Start the CABIAS"
      tags$head(tags$style(HTML("#jumpToP2{background-color:blue;color:lavender;border-color:lavender;border-radius:50px;padding:20px;background:#2F2763;font-size:1.2em;font-weight:bold}
                               #jumpToP2:hover{background-color:#E87722}"))),
      p(actionButton("jumpToP2", "Start the CABIAS"), style = "text-align: center"),
      fluidRow(column(
        hr(),
        p(em("Developed by"), br("Ceva, Sante Animale"), style = "text-align:center; font-family: times"),
        width = 12
      )),
      fluidRow(div(img(src = "ceva.jpg", height = 30, width = 30), style = "text-align: center;"))
    ),


    # Tab "Assessment"
    tabPanel("Assessment",
      icon = icon("ruler-combined"), value = "panel2",
      tags$style(".fa-database {color:#E87722}"),
      h3(p(em("CABIAS evaluation"), lib = "font-awesome"), style = "color:black;text-align:center"),
      h4(p("To obtain the results please fill the following questions about your cat’s behaviour", lib = "font-awesome"), style = "color:black;text-align:center"),
      fluidRow("First of all, please enter how many cat(s) you have at home", style = "color: darkblue; font-weight: bold;"),
      fluidRow(
        column(2, wellPanel(selectInput("nbcat", label = " ", c("", "1", ">1"))))
      ),


      # Les comportements urine, scratching et fear
      fluidRow(
        p("Urine marking section", style = "color: darkblue; font-weight: bold;"),
        p("In the last 7 days, how often have you noticed directly or indirectly (found urine deposit on vertical surface) your cat has been urine marking ?", style = "color: darkgrey;"),
        radioButtons(
          inputId = "Ufreq",
          label = "",
          choices = list("6. Every day, more than twice a day", "5. Every day, once or twice a day ", "4. Almost every day", "3. Every other day", "2. twice a week", "1. Once a week", "0. Never"),
          inline = FALSE
        ),
        conditionalPanel(
          condition = "input.Ufreq != '0. Never'",
          p("Disregarding frequency, what do you think is the current average intensity of urinary marking? Please place the cursor on this scale to describe the current intensity of this problem behaviour.", style = "color: darkgrey;"),
          # sliderInput("Uint", label = " ",
          #             "",
          #             min = 1,
          #             max = 10,
          #             step = 0.1,
          #             value = 1)
          includeScript("sliderU.js"),
          div(
            class = "my_slider", # to be able to manipulate it with JQuery
            sliderInput("Uint",
              " ",
              ticks = F,
              min = 1, max = 10, step = 0.1, value = 4.5
            )
          )
        )
      ),
      fluidRow(
        p("Scratching section", style = "color: darkblue; font-weight: bold;"),
        p("In the last 7 days, how often have you noticed directly or indirectly (new damage on vertical surface) your cat has been scratching ?", style = "color: darkgrey; "),
        radioButtons(
          inputId = "Sfreq",
          label = " ",
          choices = list("6. Every day, more than twice a day", "5. Every day, once or twice a day ", "4. Almost every day", "3. Every other day", "2. twice a week", "1. Once a week", "0. Never"),
          # choices = c(unique(as.character(file_data()[,4]))),
          inline = FALSE
        ),
        conditionalPanel(
          condition = "input.Sfreq != '0. Never'",
          p("Disregarding frequency, what do you think is the current average intensity of his scratching (duration/severity)? Please place the cursor on this scale to describe the current intensity of this problem behaviour.", style = "color: darkgrey; "),
          includeScript("sliderS.js"),
          div(
            class = "my_slider", # to be able to manipulate it with JQuery
            sliderInput("Sint",
              " ",
              ticks = F,
              min = 1, max = 10, step = 0.1, value = 4.5
            )
          )
        )
      ),
      # sliderInput("Sint", label = " ",
      #             "",
      #             min = 1,
      #             max = 10,
      #             step = 0.01,
      #             value =1))),

      fluidRow(
        p("Fear section", style = "color: darkblue; font-weight: bold;"),
        p("In the last 7 days, how often have you observed your cat has been displaying fear ?", style = "color: darkgrey; "),
        radioButtons(
          inputId = "Ffreq",
          label = " ",
          choices = list("6. Every day, more than twice a day", "5. Every day, once or twice a day ", "4. Almost every day", "3. Every other day", "2. twice a week", "1. Once a week", "0. Never"),
          # choices = c(unique(as.character(file_data()[,4]))),
          inline = FALSE
        ),
        conditionalPanel(
          condition = "input.Ffreq != '0. Never'",
          p("Disregarding frequency, what do you think is the current average intensity of his or her fears (duration/severity/number of behaviour sign of fear…)? Please place the cursor on this scale to describe the current intensity of this problem behaviour.", style = "color: darkgrey; "),
          includeScript("sliderF.js"),
          div(
            class = "my_slider", # to be able to manipulate it with JQuery
            sliderInput("Fint",
              " ",
              ticks = F,
              min = 1, max = 10, step = 0.1, value = 4.5
            )
          )
        )
      ),

      # Le comportement pb de cohab
      conditionalPanel(
        condition = "input.nbcat == '>1'",
        fluidRow(
          p("Difficulties of cohabitation section", style = "color: darkblue; font-weight: bold;"),
          p("In the last 7 days, how often have you observed your cat has been displaying cohabitation problem ?", style = "color: darkgrey; "),
          radioButtons(
            inputId = "Cfreq",
            label = " ",
            choices = list("6. Every day, more than twice a day", "5. Every day, once or twice a day ", "4. Almost every day", "3. Every other day", "2. twice a week", "1. Once a week", "0. Never"),
            # choices = c(unique(as.character(file_data()[,4]))),
            inline = FALSE
          ),
          conditionalPanel(
            condition = "input.Cfreq != '0. Never'",
            p("Disregarding frequency, what do you think is the current average intensity of his or her cohabitation problem (duration/severity/number of behaviour sign of cohabitation problem…)? Please place the cursor on this scale to describe the current intensity of this problem behaviour.", style = "color: darkgrey; "),
            includeScript("sliderC.js"),
            div(
              class = "my_slider", # to be able to manipulate it with JQuery
              sliderInput("Cint",
                " ",
                ticks = F,
                min = 1, max = 10, step = 0.1, value = 4.5
              )
            )
          )
        ),
      ),
      ######  Bouton "Click here to see results"
      tags$head(tags$style(HTML("#jumpToP3{background-color:blue;color:lavender;border-color:lavender;border-radius:50px;padding:20px;background:#2F2763;font-size:1.2em;font-weight:bold}
                                                                #jumpToP3:hover{background-color:#E87722}"))),
      p(actionButton("jumpToP3", "Click here to see results"), style = "text-align: center;"),
    ),

    # Tab "Results"
    tabPanel("Results",
      icon = icon("square-poll-vertical"), value = "panel3",
      tags$style(".fa-database {color:#E87722}"),
      h3(p(em("CABIAS Index results"), lib = "font-awesome"), style = "color:black;text-align:center"),

      # ajout tableau résultats
      fluidRow("Overall results", style = "color: darkblue; font-weight: bold;font-size:1.2em"),
      fluidRow(uiOutput("GRes1")),
      # fluidRow(plotlyOutput(outputId = "Utab")),
      conditionalPanel(
        condition = "input.nbcat == '>1'",
        fluidRow(DT::dataTableOutput("mytable"))
      ),
      conditionalPanel(
        condition = "input.nbcat == '1'",
        fluidRow(DT::dataTableOutput("mytable2"))
      ),
      fluidRow(uiOutput("GRes2")),
      ####

      fluidRow("Urine marking", style = "color: darkblue; font-weight: bold;"),
      fluidRow(uiOutput("Uindex")),
      fluidRow(plotlyOutput(outputId = "Ugraph")),
      fluidRow("Scratching", style = "color: darkblue; font-weight: bold;"),
      fluidRow(uiOutput("Sindex")),
      fluidRow(plotlyOutput(outputId = "Sgraph")),
      fluidRow("Fear", style = "color: darkblue; font-weight: bold;"),
      fluidRow(uiOutput("Findex")),
      fluidRow(plotlyOutput(outputId = "Fgraph")),
      conditionalPanel(
        condition = "input.nbcat == '>1'",
        fluidRow("Cohabitation problem", style = "color: darkblue ; font-weight: bold;"),
        fluidRow(uiOutput("Cindex")),
        fluidRow(plotlyOutput(outputId = "Cgraph"))
      )
    )
  )
)





# ______________________________________________________________________________#
# ___________________________________ Server ___________________________________#

server <- function(input, output, session) {
  # ________________________________
  # Just a small fix to reactivate Labels when Min/Max value is chosen

  observeEvent(input$jumpToP2, {
    updateTabsetPanel(session, "inTabset",
      selected = "panel2"
    )
  })

  observeEvent(input$jumpToP3, {
    updateTabsetPanel(session, "inTabset",
      selected = "panel3"
    )
  })

  output$ui <- renderUI({
    if (is.null(input$nbcat)) {
      return()
    }

    # Depending on input$nbcat, we'll generate a different
    # UI component and send it to the client.
    switch(input$nbcat,
      "1" = fluidRow("End of the questions", style = "color: darkblue; font-weight: bold;"),
      ">1" =
        fluidRow(
          p("Difficulties of cohabitation section", style = "color: darkblue; font-weight: bold;"),
          p("Problem behaviours in cats such as scratching, urine marking, inter-cat cohabitation issues and fear are a welfare problem for both cat and caregiver.", style = "color: darkgrey"),
          radioButtons(
            inputId = "Cfreq",
            label = " ",
            choices = list("6. Every day, more than twice a day", "5. Every day, once or twice a day ", "4. Almost every day", "3. Every other day", "2. twice a week", "1. Once a week", "0. Never"),
            # choices = c(unique(as.character(file_data()[,4]))),
            inline = FALSE
          ),
          p("Disregarding frequency, what do you think is the current average intensity of his or her cohabitation problem (duration/severity/number of behaviour sign of cohabitation difficulties….)? Please place the cursor on this scale to describe the current intensity of this problem behaviour.", style = "color: darkgrey; "),
          sliderInput("Cint",
            label = " ",
            "",
            min = 0,
            max = 10,
            step = 0.01,
            value = 0
          ),
          p("End of the questions", style = "color: darkblue; font-weight: bold;")
        )
    )
  })

  # SDO <- data.frame(read_excel("~/shareddata/Laurianne/Shiny app/CABIAS/Data/SDO.xlsx"))
  # FDO <- data.frame(read_excel("~/shareddata/Laurianne/Shiny app/CABIAS/Data/FDO.xlsx"))
  # CDO <- data.frame(read_excel("~/shareddata/Laurianne/Shiny app/CABIAS/Data/CDO.xlsx"))





  # Overall results




  output$GRes1 <- renderUI({
    tagList(tags$hr(tags$div("Your score results (the score can vary from 0 to 60)")))
  })

  output$mytable <- DT::renderDataTable({
    Ufreq_num <- stringr::str_extract(input$Ufreq, "^.{1}")
    Sfreq_num <- stringr::str_extract(input$Sfreq, "^.{1}")
    Ffreq_num <- stringr::str_extract(input$Ffreq, "^.{1}")
    Cfreq_num <- stringr::str_extract(input$Cfreq, "^.{1}")
    dat <- data.frame(
      Logocat = c(
        '<img src="Uimage.jpg" height="52"></img>',
        '<img src="Simage.jpg" height="52"></img>',
        '<img src="Fimage.jpg" height="52"></img>',
        '<img src="Cimage.jpg" height="52"></img>'
      ),
      PB = c(
        "Problem of Urine marking", "Problem of Scratching",
        "Problem of Fear", "Problem of Cohabitation"
      ),
      result = c(
        as.numeric(Ufreq_num) * as.numeric(input$Uint), as.numeric(Sfreq_num) * as.numeric(input$Sint),
        as.numeric(Ffreq_num) * as.numeric(input$Fint), as.numeric(Cfreq_num) * as.numeric(input$Cint)
      )
    )

    DT::datatable(dat,
      escape = FALSE, colnames = "", rownames = FALSE,
      options = list(
        pageLength = 15, lengthChange = FALSE,
        sDom = '<"top">lrt<"bottom">ip', paging = FALSE,
        info = FALSE
      )
    ) %>% formatStyle("PB",
      target = "row",
      backgroundColor = styleEqual(
        c(
          "Problem of Urine marking", "Problem of Scratching",
          "Problem of Fear", "Problem of Cohabitation"
        ),
        c("lavender", "lightgrey", "lavender", "lightgrey")
      )
    )
  })

  output$mytable2 <- DT::renderDataTable({
    Ufreq_num <- stringr::str_extract(input$Ufreq, "^.{1}")
    Sfreq_num <- stringr::str_extract(input$Sfreq, "^.{1}")
    Ffreq_num <- stringr::str_extract(input$Ffreq, "^.{1}")
    Cfreq_num <- stringr::str_extract(input$Cfreq, "^.{1}")
    dat2 <- data.frame(
      Logocat = c(
        '<img src="Uimage.jpg" height="52"></img>',
        '<img src="Simage.jpg" height="52"></img>',
        '<img src="Fimage.jpg" height="52"></img>'
      ),
      PB = c(
        "Problem of Urine marking", "Problem of Scratching",
        "Problem of Fear"
      ),
      result = c(
        as.numeric(Ufreq_num) * as.numeric(input$Uint), as.numeric(Sfreq_num) * as.numeric(input$Sint),
        as.numeric(Ffreq_num) * as.numeric(input$Fint)
      )
    )

    DT::datatable(dat2,
      escape = FALSE, colnames = "", rownames = FALSE,
      options = list(
        pageLength = 15, lengthChange = FALSE,
        sDom = '<"top">lrt<"bottom">ip', paging = FALSE,
        info = FALSE
      )
    ) %>% formatStyle("PB",
      target = "row",
      backgroundColor = styleEqual(
        c("Problem of Urine marking", "Problem of Scratching", "Problem of Fear"),
        c("lavender", "lightgrey", "lavender")
      )
    )
  })

  output$GRes2 <- renderUI({
    tagList(tags$hr(tags$div("Following your situation compared to the population in our publication")))
  })




  # Résultats URINE


  output$Uindex <- renderUI({
    Ufreq_num <- stringr::str_extract(input$Ufreq, "^.{1}")
    Uind <- as.numeric(Ufreq_num) * as.numeric(input$Uint)
    tagList(tags$hr(tags$div("The urine index score of your cat is", tags$strong(Uind))))
  })

  output$Ugraph <-
    renderPlotly({
      UD0 <- data.frame(read_excel("data/UD0.xlsx"))
      UD0$index_cat <- ifelse(UD0$index == 0, 0,
        ifelse(UD0$index > 0 & UD0$index <= 5, 1,
          ifelse(UD0$index > 5 & UD0$index <= 10, 2,
            ifelse(UD0$index > 10 & UD0$index <= 15, 3,
              ifelse(UD0$index > 15 & UD0$index <= 20, 4,
                ifelse(UD0$index > 20 & UD0$index <= 25, 5,
                  ifelse(UD0$index > 25 & UD0$index <= 30, 6,
                    ifelse(UD0$index > 30 & UD0$index <= 35, 7,
                      ifelse(UD0$index > 35 & UD0$index <= 40, 8,
                        ifelse(UD0$index > 40 & UD0$index <= 45, 9,
                          ifelse(UD0$index > 45 & UD0$index <= 50, 10,
                            ifelse(UD0$index > 50 & UD0$index <= 55, 11,
                              ifelse(UD0$index > 55 & UD0$index <= 60, 12,
                                ifelse(UD0$index > 60, 13, NA)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

      UD0b <- UD0 %>% count(index_cat)
      UD0b$index_cat2 <- as.factor(UD0b$index_cat)

      levels(UD0b$index_cat2)[levels(UD0b$index_cat2) == "0"] <- "0"
      levels(UD0b$index_cat2)[levels(UD0b$index_cat2) == "1"] <- "]0-5]"
      levels(UD0b$index_cat2)[levels(UD0b$index_cat2) == "2"] <- "]5-10]"
      levels(UD0b$index_cat2)[levels(UD0b$index_cat2) == "3"] <- "]10-15]"
      levels(UD0b$index_cat2)[levels(UD0b$index_cat2) == "4"] <- "]15-20]"
      levels(UD0b$index_cat2)[levels(UD0b$index_cat2) == "5"] <- "]20-25]"
      levels(UD0b$index_cat2)[levels(UD0b$index_cat2) == "6"] <- "]25-30]"
      levels(UD0b$index_cat2)[levels(UD0b$index_cat2) == "7"] <- "]30-35]"
      levels(UD0b$index_cat2)[levels(UD0b$index_cat2) == "8"] <- "]35-40]"
      levels(UD0b$index_cat2)[levels(UD0b$index_cat2) == "9"] <- "]40-45]"
      levels(UD0b$index_cat2)[levels(UD0b$index_cat2) == "10"] <- "]45-50]"
      levels(UD0b$index_cat2)[levels(UD0b$index_cat2) == "11"] <- "]50-55]"
      levels(UD0b$index_cat2)[levels(UD0b$index_cat2) == "12"] <- "]55-60]"
      levels(UD0b$index_cat2)[levels(UD0b$index_cat2) == "13"] <- ">60"

      UD0b$group <- ifelse(UD0b$index_cat == 0, 0, 1)

      Ufreq_num <- stringr::str_extract(input$Ufreq, "^.{1}")
      Uind <- as.numeric(Ufreq_num) * as.numeric(input$Uint)
      Uind_cat <- ifelse(Uind == 0, 0,
        ifelse(Uind > 0 & Uind <= 5, 1,
          ifelse(Uind > 5 & Uind <= 10, 2,
            ifelse(Uind > 10 & Uind <= 15, 3,
              ifelse(Uind > 15 & Uind <= 20, 4,
                ifelse(Uind > 20 & Uind <= 25, 5,
                  ifelse(Uind > 25 & Uind <= 30, 6,
                    ifelse(Uind > 30 & Uind <= 35, 7,
                      ifelse(Uind > 35 & Uind <= 40, 8,
                        ifelse(Uind > 40 & Uind <= 45, 9,
                          ifelse(Uind > 45 & Uind <= 50, 10,
                            ifelse(Uind > 50 & Uind <= 55, 11,
                              ifelse(Uind > 55 & Uind <= 60, 12,
                                ifelse(Uind > 60, 13, NA)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

      p <- ggplot(UD0b, aes(x = index_cat2, y = n, fill = factor(group))) +
        geom_col(show.legend = FALSE) +
        geom_vline(xintercept = Uind_cat + 1, colour = "darkred", linetype = "dashed") +
        annotate("text", x = (Uind_cat + 2), y = 640, label = "Your cat index", color = "darkred") +
        ylab("Number of cats presenting this value") +
        xlab("Urine marking Index Values") +
        scale_y_continuous(breaks = seq(0, 650, by = 100)) +
        scale_fill_manual(values = c("lightgreen", "#F8B195")) +
        theme(legend.position = "none")
      # scale_y_cut(breaks=c(60) ,which=3)
      # scale_x_break(c(100, 600))



      ggplotly(p)
    })




  # Résultats SCRATCHING
  output$Sindex <- renderUI({
    Sfreq_num <- stringr::str_extract(input$Sfreq, "^.{1}")
    Sind <- as.numeric(Sfreq_num) * as.numeric(input$Sint)
    tagList(tags$hr(tags$div("The scratching index score of your cat is", tags$strong(Sind))))
  })

  output$Sgraph <-
    renderPlotly({
      SD0 <- data.frame(read_excel("data/SD0.xlsx"))
      SD0$index_cat <- ifelse(SD0$index == 0, 0,
        ifelse(SD0$index > 0 & SD0$index <= 5, 1,
          ifelse(SD0$index > 5 & SD0$index <= 10, 2,
            ifelse(SD0$index > 10 & SD0$index <= 15, 3,
              ifelse(SD0$index > 15 & SD0$index <= 20, 4,
                ifelse(SD0$index > 20 & SD0$index <= 25, 5,
                  ifelse(SD0$index > 25 & SD0$index <= 30, 6,
                    ifelse(SD0$index > 30 & SD0$index <= 35, 7,
                      ifelse(SD0$index > 35 & SD0$index <= 40, 8,
                        ifelse(SD0$index > 40 & SD0$index <= 45, 9,
                          ifelse(SD0$index > 45 & SD0$index <= 50, 10,
                            ifelse(SD0$index > 50 & SD0$index <= 55, 11,
                              ifelse(SD0$index > 55 & SD0$index <= 60, 12,
                                ifelse(SD0$index > 60, 13, NA)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

      SD0b <- SD0 %>% count(index_cat)
      SD0b$index_cat2 <- as.factor(SD0b$index_cat)

      levels(SD0b$index_cat2)[levels(SD0b$index_cat2) == "0"] <- "0"
      levels(SD0b$index_cat2)[levels(SD0b$index_cat2) == "1"] <- "]0-5]"
      levels(SD0b$index_cat2)[levels(SD0b$index_cat2) == "2"] <- "]5-10]"
      levels(SD0b$index_cat2)[levels(SD0b$index_cat2) == "3"] <- "]10-15]"
      levels(SD0b$index_cat2)[levels(SD0b$index_cat2) == "4"] <- "]15-20]"
      levels(SD0b$index_cat2)[levels(SD0b$index_cat2) == "5"] <- "]20-25]"
      levels(SD0b$index_cat2)[levels(SD0b$index_cat2) == "6"] <- "]25-30]"
      levels(SD0b$index_cat2)[levels(SD0b$index_cat2) == "7"] <- "]30-35]"
      levels(SD0b$index_cat2)[levels(SD0b$index_cat2) == "8"] <- "]35-40]"
      levels(SD0b$index_cat2)[levels(SD0b$index_cat2) == "9"] <- "]40-45]"
      levels(SD0b$index_cat2)[levels(SD0b$index_cat2) == "10"] <- "]45-50]"
      levels(SD0b$index_cat2)[levels(SD0b$index_cat2) == "11"] <- "]50-55]"
      levels(SD0b$index_cat2)[levels(SD0b$index_cat2) == "12"] <- "]55-60]"
      levels(SD0b$index_cat2)[levels(SD0b$index_cat2) == "13"] <- ">60"

      SD0b$group <- ifelse(SD0b$index_cat == 0, 0, 1)

      Sfreq_num <- stringr::str_extract(input$Sfreq, "^.{1}")
      Sind <- as.numeric(Sfreq_num) * as.numeric(input$Sint)
      Sind_cat <- ifelse(Sind == 0, 0,
        ifelse(Sind > 0 & Sind <= 5, 1,
          ifelse(Sind > 5 & Sind <= 10, 2,
            ifelse(Sind > 10 & Sind <= 15, 3,
              ifelse(Sind > 15 & Sind <= 20, 4,
                ifelse(Sind > 20 & Sind <= 25, 5,
                  ifelse(Sind > 25 & Sind <= 30, 6,
                    ifelse(Sind > 30 & Sind <= 35, 7,
                      ifelse(Sind > 35 & Sind <= 40, 8,
                        ifelse(Sind > 40 & Sind <= 45, 9,
                          ifelse(Sind > 45 & Sind <= 50, 10,
                            ifelse(Sind > 50 & Sind <= 55, 11,
                              ifelse(Sind > 55 & Sind <= 60, 12,
                                ifelse(Sind > 60, 13, NA)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

      p <- ggplot(SD0b, aes(x = index_cat2, y = n, fill = factor(group))) +
        geom_col(show.legend = FALSE) +
        geom_vline(xintercept = Sind_cat + 1, colour = "darkred", linetype = "dashed") +
        annotate("text", x = (Sind_cat + 2), y = 400, label = "Your cat index", color = "darkred") +
        ylab("Number of cats presenting this value") +
        xlab("Scratching Index Values") +
        scale_y_continuous(breaks = seq(0, 450, by = 50)) +
        scale_fill_manual(values = c("lightgreen", "#F8B195")) +
        theme(legend.position = "none")
      # scale_y_cut(breaks=c(60) ,which=3)
      # scale_x_break(c(100, 600))



      ggplotly(p)
    })


  # Résultats FEAR
  output$Findex <- renderUI({
    Ffreq_num <- stringr::str_extract(input$Ffreq, "^.{1}")
    Find <- as.numeric(Ffreq_num) * as.numeric(input$Fint)
    tagList(tags$hr(tags$div("The fear index score of your cat is", tags$strong(Find))))
  })
  output$Fgraph <-
    renderPlotly({
      FD0 <- data.frame(read_excel("data/FD0.xlsx"))
      FD0$index_cat <- ifelse(FD0$index == 0, 0,
        ifelse(FD0$index > 0 & FD0$index <= 5, 1,
          ifelse(FD0$index > 5 & FD0$index <= 10, 2,
            ifelse(FD0$index > 10 & FD0$index <= 15, 3,
              ifelse(FD0$index > 15 & FD0$index <= 20, 4,
                ifelse(FD0$index > 20 & FD0$index <= 25, 5,
                  ifelse(FD0$index > 25 & FD0$index <= 30, 6,
                    ifelse(FD0$index > 30 & FD0$index <= 35, 7,
                      ifelse(FD0$index > 35 & FD0$index <= 40, 8,
                        ifelse(FD0$index > 40 & FD0$index <= 45, 9,
                          ifelse(FD0$index > 45 & FD0$index <= 50, 10,
                            ifelse(FD0$index > 50 & FD0$index <= 55, 11,
                              ifelse(FD0$index > 55 & FD0$index <= 60, 12,
                                ifelse(FD0$index > 60, 13, NA)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

      FD0b <- FD0 %>% count(index_cat)
      FD0b$index_cat2 <- as.factor(FD0b$index_cat)

      levels(FD0b$index_cat2)[levels(FD0b$index_cat2) == "0"] <- "0"
      levels(FD0b$index_cat2)[levels(FD0b$index_cat2) == "1"] <- "]0-5]"
      levels(FD0b$index_cat2)[levels(FD0b$index_cat2) == "2"] <- "]5-10]"
      levels(FD0b$index_cat2)[levels(FD0b$index_cat2) == "3"] <- "]10-15]"
      levels(FD0b$index_cat2)[levels(FD0b$index_cat2) == "4"] <- "]15-20]"
      levels(FD0b$index_cat2)[levels(FD0b$index_cat2) == "5"] <- "]20-25]"
      levels(FD0b$index_cat2)[levels(FD0b$index_cat2) == "6"] <- "]25-30]"
      levels(FD0b$index_cat2)[levels(FD0b$index_cat2) == "7"] <- "]30-35]"
      levels(FD0b$index_cat2)[levels(FD0b$index_cat2) == "8"] <- "]35-40]"
      levels(FD0b$index_cat2)[levels(FD0b$index_cat2) == "9"] <- "]40-45]"
      levels(FD0b$index_cat2)[levels(FD0b$index_cat2) == "10"] <- "]45-50]"
      levels(FD0b$index_cat2)[levels(FD0b$index_cat2) == "11"] <- "]50-55]"
      levels(FD0b$index_cat2)[levels(FD0b$index_cat2) == "12"] <- "]55-60]"
      levels(FD0b$index_cat2)[levels(FD0b$index_cat2) == "13"] <- ">60"

      FD0b$group <- ifelse(FD0b$index_cat == 0, 0, 1)

      Ffreq_num <- stringr::str_extract(input$Ffreq, "^.{1}")
      Find <- as.numeric(Ffreq_num) * as.numeric(input$Fint)
      Find_cat <- ifelse(Find == 0, 0,
        ifelse(Find > 0 & Find <= 5, 1,
          ifelse(Find > 5 & Find <= 10, 2,
            ifelse(Find > 10 & Find <= 15, 3,
              ifelse(Find > 15 & Find <= 20, 4,
                ifelse(Find > 20 & Find <= 25, 5,
                  ifelse(Find > 25 & Find <= 30, 6,
                    ifelse(Find > 30 & Find <= 35, 7,
                      ifelse(Find > 35 & Find <= 40, 8,
                        ifelse(Find > 40 & Find <= 45, 9,
                          ifelse(Find > 45 & Find <= 50, 10,
                            ifelse(Find > 50 & Find <= 55, 11,
                              ifelse(Find > 55 & Find <= 60, 12,
                                ifelse(Find > 60, 13, NA)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

      p <- ggplot(FD0b, aes(x = index_cat2, y = n, fill = factor(group))) +
        geom_col(show.legend = FALSE) +
        geom_vline(xintercept = Find_cat + 1, colour = "darkred", linetype = "dashed") +
        annotate("text", x = (Find_cat + 2), y = 400, label = "Your cat index", color = "darkred") +
        ylab("Number of cats presenting this value") +
        xlab("Fear Index Values") +
        scale_y_continuous(breaks = seq(0, 450, by = 50)) +
        scale_fill_manual(values = c("lightgreen", "#F8B195")) +
        theme(legend.position = "none")
      # scale_y_cut(breaks=c(60) ,which=3)
      # scale_x_break(c(100, 600))

      ggplotly(p)
    })


  # Résultats COHABITATION
  output$Cindex <- renderUI({
    Cfreq_num <- stringr::str_extract(input$Cfreq, "^.{1}")
    Cind <- as.numeric(Cfreq_num) * as.numeric(input$Cint)
    tagList(tags$hr(tags$div("The cohabitation problem index score of your cat is", tags$strong(Cind))))
  })
  output$Cgraph <-
    renderPlotly({
      CD0 <- data.frame(read_excel("data/CD0.xlsx"))
      CD0$index_cat <- ifelse(CD0$index == 0, 0,
        ifelse(CD0$index > 0 & CD0$index <= 5, 1,
          ifelse(CD0$index > 5 & CD0$index <= 10, 2,
            ifelse(CD0$index > 10 & CD0$index <= 15, 3,
              ifelse(CD0$index > 15 & CD0$index <= 20, 4,
                ifelse(CD0$index > 20 & CD0$index <= 25, 5,
                  ifelse(CD0$index > 25 & CD0$index <= 30, 6,
                    ifelse(CD0$index > 30 & CD0$index <= 35, 7,
                      ifelse(CD0$index > 35 & CD0$index <= 40, 8,
                        ifelse(CD0$index > 40 & CD0$index <= 45, 9,
                          ifelse(CD0$index > 45 & CD0$index <= 50, 10,
                            ifelse(CD0$index > 50 & CD0$index <= 55, 11,
                              ifelse(CD0$index > 55 & CD0$index <= 60, 12,
                                ifelse(CD0$index > 60, 13, NA)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

      CD0b <- CD0 %>% count(index_cat)
      CD0b$index_cat2 <- as.factor(CD0b$index_cat)

      levels(CD0b$index_cat2)[levels(CD0b$index_cat2) == "0"] <- "0"
      levels(CD0b$index_cat2)[levels(CD0b$index_cat2) == "1"] <- "]0-5]"
      levels(CD0b$index_cat2)[levels(CD0b$index_cat2) == "2"] <- "]5-10]"
      levels(CD0b$index_cat2)[levels(CD0b$index_cat2) == "3"] <- "]10-15]"
      levels(CD0b$index_cat2)[levels(CD0b$index_cat2) == "4"] <- "]15-20]"
      levels(CD0b$index_cat2)[levels(CD0b$index_cat2) == "5"] <- "]20-25]"
      levels(CD0b$index_cat2)[levels(CD0b$index_cat2) == "6"] <- "]25-30]"
      levels(CD0b$index_cat2)[levels(CD0b$index_cat2) == "7"] <- "]30-35]"
      levels(CD0b$index_cat2)[levels(CD0b$index_cat2) == "8"] <- "]35-40]"
      levels(CD0b$index_cat2)[levels(CD0b$index_cat2) == "9"] <- "]40-45]"
      levels(CD0b$index_cat2)[levels(CD0b$index_cat2) == "10"] <- "]45-50]"
      levels(CD0b$index_cat2)[levels(CD0b$index_cat2) == "11"] <- "]50-55]"
      levels(CD0b$index_cat2)[levels(CD0b$index_cat2) == "12"] <- "]55-60]"
      levels(CD0b$index_cat2)[levels(CD0b$index_cat2) == "13"] <- ">60"

      CD0b$group <- ifelse(CD0b$index_cat == 0, 0, 1)

      Cfreq_num <- stringr::str_extract(input$Cfreq, "^.{1}")
      Cind <- as.numeric(Cfreq_num) * as.numeric(input$Cint)
      Cind_cat <- ifelse(Cind == 0, 0,
        ifelse(Cind > 0 & Cind <= 5, 1,
          ifelse(Cind > 5 & Cind <= 10, 2,
            ifelse(Cind > 10 & Cind <= 15, 3,
              ifelse(Cind > 15 & Cind <= 20, 4,
                ifelse(Cind > 20 & Cind <= 25, 5,
                  ifelse(Cind > 25 & Cind <= 30, 6,
                    ifelse(Cind > 30 & Cind <= 35, 7,
                      ifelse(Cind > 35 & Cind <= 40, 8,
                        ifelse(Cind > 40 & Cind <= 45, 9,
                          ifelse(Cind > 45 & Cind <= 50, 10,
                            ifelse(Cind > 50 & Cind <= 55, 11,
                              ifelse(Cind > 55 & Cind <= 60, 12,
                                ifelse(Cind > 60, 13, NA)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

      p <- ggplot(CD0b, aes(x = index_cat2, y = n, fill = factor(group))) +
        geom_col(show.legend = FALSE) +
        geom_vline(xintercept = Cind_cat + 1, colour = "darkred", linetype = "dashed") +
        annotate("text", x = (Cind_cat + 2), y = 100, label = "Your cat index", color = "darkred") +
        ylab("Number of cats presenting this value") +
        xlab("Cohabitation Index Values") +
        scale_y_continuous(breaks = seq(0, 150, by = 25)) +
        scale_fill_manual(values = c("lightgreen", "#F8B195")) +
        theme(legend.position = "none")
      # scale_y_cut(breaks=c(60) ,which=3)
      # scale_x_break(c(100, 600))

      ggplotly(p)
    })
}

shinyApp(ui, server)
