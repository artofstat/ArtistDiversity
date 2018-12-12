library(shiny)
library(DT)
library(ggplot2)
library(shinyWidgets)
library(markdown)

shinyUI(
  navbarPage(
    title=a(tags$b("Diversity of Artists in Major U.S. Museums by Topaz et al."), href='http://www.plosone.org') ,
    windowTitle="Artist Diversity",
    id="mytabs",
    tabPanel(title="Artist Demographics",
      value="Demos",       
      sidebarLayout(
        sidebarPanel(
          tags$b("The dataset to the right contains crowdsourced information on a random sample of thousands of individual, identifiable artists in various U.S. museums."),
          br(),br(),
          tags$em("Not Inferred values indicate that we were not able to confidently determine the value based on crowdsourcing approach."),
          awesomeCheckbox("filter", "Allow subsetting?", value=TRUE),
          width = 3), #end first sidebarpanel
        mainPanel(
          DTOutput('artistdata'), tags$em('Note that the artist name is scraped from the web and is not a cleaned version due to character encodings.')
        ) #end first main panel
      ) #end sidebarLayout first panel
    ), #end tabPanel first panel
    tabPanel(title="Graphs",
      value="Graphs",       
      sidebarLayout(
        sidebarPanel(
          tags$b("The graphics to the right display crowdsourced information on a random sample of thousands of individual, identifiable artists in various U.S. museums."),
          br(),br(),
          tags$em("Not Inferred values indicate that we were not able to confidently determine the value based on crowdsourcing approach."),
          br(),br(),
          selectInput("demovar", "Choose a Demographic Variable:", choices=list("Gender"= "gender", "Ethnicity"="ethnicity", 
            "Birth Year"="birthyear", "Nationality"="nationality"),selected='Gender'),
          awesomeCheckbox("unknownfilter", "Exclude artists with not inferred values", value=TRUE),
          #awesomeCheckboxGroup("order", "Prefer bar plots", value=FALSE)
          awesomeCheckbox("barplot", "Prefer bar plots", value=FALSE),
          width = 3), #end third sidebarPanel
        mainPanel(
          plotOutput("demoplot", height = 700)
        ) #end third mainPanel
      ) #end sidebarLayout third panel
    ), #end tabPanel third panel
    tabPanel(title="Survey Instrument",
      value="SI",       
      sidebarLayout(
        sidebarPanel(
          tags$b("The survey instrument to the right was used by Mechanical Turk workers to provide information on a random sample of thousands of records scrapped from various U.S. museums."),
          br(),br(),
          width = 3), #end second sidebarpanel
        mainPanel(
          includeMarkdown("include.md")
        ) #end second main panel
      ) #end sidebarLayout second panel
    ) #end tabPanel second panel
  ) #end navBar
) #end ui