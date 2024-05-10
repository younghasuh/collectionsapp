# Load libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(urbnmapr)
library(here)
library(usmap)
library(leaflet)
library(sf)
library(htmltools)
library(plotly)
library(shinydashboard)
library(DT)
library(rclipboard)


shinyUI(
  
  fluidPage(
    
    titlePanel(title =  div(img(src="NHM_logo_black_250.png", width="50px", height="50px"), 
                            "Ornithology Collections"), windowTitle = "Ornithology Collections"),
    p("Updated May 2024"),
    
    tabsetPanel(
      type = "tabs",
      
      # tab 1 ----
      tabPanel(
        titlePanel("Species summary"),
        
        sidebarLayout(
          sidebarPanel(
            h2("Species"),
            p("Start typing species and select from drop-down list."),
            
            selectizeInput(
              inputId = 'sp',
              label = 'Species',
              choices = NULL,
              selected = NULL,
              multiple = FALSE, # allow for multiple inputs
              options = NULL)),
          
          # main panel for displaying outputs for the selectize Input (species name)
          mainPanel(
            
            tabsetPanel(type = "tabs",
                        # sub tab 1
                        tabPanel(title = "Summary", 
                                 fluidRow(column(12, h4("Specimen count by specimen type/nature"), tableOutput("specnat"))),
                                 fluidRow(column(6, h4("By sex", tableOutput("sexcount"))))),
                        # sub tab 2
                        tabPanel(title = "Count figures", 
                                 fluidRow(column(12, h4("Specimen count by year"), plotOutput("trend"))),
                                 fluidRow(column(12, h4("Specimen count by month"), plotOutput("trend2"))),
                                 fluidRow(column(12, h4("Specimen count by state (US only)"), plotOutput("state"))),
                                 fluidRow(column(12, h4("Specimen count by county"), plotOutput("county"))),
                                 fluidRow(column(12, h4("Global specimen distribution"), leafletOutput(outputId = 'map')))
                        ),
                        # sub tab 3
                        tabPanel(title = "Data explorationg with Plotly",
                                 fluidRow(column(12, h4("Plotly boxplots"), 
                                                 box(selectInput("xaxis", "Select independent variable (x-axis)",
                                                                 choices = c("Sex", "state", "month"))))),
                                 fluidRow(column(12, plotlyOutput("plot")))
                        ),
                        # sub tab 4
                        tabPanel(title = "Table of all specimens",
                                 fluidRow(column(12, h4("List of all specimens. Use shift or ctrl to select multiple rows for copying onto clipboard."),
                                                 DTOutput("spectab"))))
            )
          ) # mainpanel end
        ) # sidebar layout end
      ), # tabPanel end
      
      # tab 2 ----
      # start new tabPanel --
      tabPanel(
        titlePanel("Catalog lookup"),
        selectInput(inputId = "collx", label = "Collection", choices = c("LACM", "CUMV")),
        textInput("catnum", "Catalog number:"),
        fluidRow(column(12, h5("Examples: use CUMV - 10078, LACM - 121077"), tableOutput("catcount"))),
        fluidRow(column(12, h4("Leaftlet map"), leafletOutput(outputId = 'catmap')))
      ),
      
      # tab 3 ----
      tabPanel(
        titlePanel("Compare species"),
        selectizeInput(inputId = 'sp1', label = 'Species 1', choices = NULL, selected = NULL, multiple = FALSE, options = NULL),
        selectizeInput(inputId = 'sp2', label = 'Species 2', choices = NULL, selected = NULL, multiple = FALSE, options = NULL),
        plotlyOutput("plot_spcomp")
      ),
      
      # tab 4 ----
      tabPanel(
        titlePanel("Explore the collections"),
        fluidRow(h4("Last updated Oct 2023")),
        fluidRow(column(12, h4("Total number of specimens"), tableOutput("summ"))),
        fluidRow(column(12, selectInput("category", "Select category:",
                                        choices = c("Sex", "family", "genus", "year", "country", "state", "county")))),
        fluidRow(column(12, tableOutput("toptable"))),
        fluidRow(column(12, tableOutput("toptable2")))
      )
      
    )
  )
)
