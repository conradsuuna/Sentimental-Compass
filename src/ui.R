library(shiny)
library(shinydashboard)
library(rworldmap)
library("shinythemes")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("plotrix")
library(stringr)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)

dashboardPage(
  dashboardHeader(title = "NSSF ANALYSIS"),
  dashboardSidebar(width="300",
    sidebarMenu(
      menuItem("Load a file",icon=icon("upload"),fileInput('file1', 'CHOOSE CSV FILE',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv'))),
      
     # menuItem("wordcloud",icon=icon("upload"),plotOutput("Wordcloud") )
     menuItem("BRIEF EXPLANATION",icon = icon("dashboard"))
    ),
    
    conditionalPanel(
      'input.free==="RAW DATASET"',
       box(width="12",helpText('This is the  csv file that you have uploaded, it contains all the data concerning the application that NSSF uses to chat ,answer queries and to monitor there customer satisfaction with there services')),
      
       menuItem("OBSERVATION",icon = icon("dashboard")),
       box(width="12",helpText('Data that has not been subjected to any form of processing or any other manipulation')),
    
       menuItem("CONCLUSION",icon = icon("dashboard")),
       box(width="12",helpText('One cannot rely on such data to draw a final conclusion because it possibly contains errors not validated in different colloquial formats'))
      ),
    
    conditionalPanel(
      'input.free==="DOCUMENT MATRIX"',
      box(width="12",helpText('This is a mathematical matrix that describes the frequency of words in our chat content')),
      
      menuItem("OBSERVATION",icon = icon("dashboard")),
      box(width="12",helpText('It shows the most used words in the chat content and their frequency of use arrranged in the tabular format')),
    
      menuItem("CONCLUSION",icon = icon("dashboard")),
      box(width="12",helpText('It shows us the most discussed topics and used words in the chat content'))
      ),
    
    conditionalPanel(
      'input.free==="WORD CLOUD"',
      box(width="12",helpText('This is the visual representation of data that is used to depict keyword metadata in formats of font size and colour')),
       
      menuItem("OBSERVATION",icon = icon("dashboard")),
      box(width="12",helpText('It shows the most used words in the chat content in bigger font sizes as it goes on reducing with the rarely used words ')),
    
      menuItem("CONCLUSION",icon = icon("dashboard")),
      box(width="12",helpText('Words with the biggest font sizes are the most focussed on topics in our chat content'))
      ),
    
    conditionalPanel(
      'input.free==="WORD FREQUENCY"',
      box(width="12",helpText('This graph shows the frequency at which a particular word is repeated in the chat content in graphical format')),
     
       menuItem("OBSERVATION",icon = icon("dashboard")),
       box(width="12",helpText('This helps the organisation to determine the words that are frquentily used and the questions that are usually asked from there respective frequencies')),
      
      menuItem("CONCLUSION",icon = icon("dashboard")),
      box(width="12",helpText('This may help the NSSF organisation to make an artificial intelligence customer care to takkle the most asked questions without actual involvement of the customer care personel'))
    ),
    
    conditionalPanel(
      'input.free==="SUMMARY"',
      box(width="12",helpText('It shows the raw dataset uploaded in a shorter format')),
      
      menuItem("OBSERVATION",icon = icon("dashboard")),
      box(width="12",helpText('Communication of the raw dataset as simple as possibles')),
    
      menuItem("CONCLUSION",icon = icon("dashboard")),
      box(width="12",helpText('One cannot draw a conclusion from the summary but its quite easier to pick basic data statistics'))
    ),
    
    conditionalPanel(
      'input.free==="MAP"',
      
      box(width="12",helpText('This map shows the countries where the clients that chat with the NSSF customer personel are stationed')),
      
      menuItem("OBSERVATION",icon = icon("dashboard")),
      box(width="12",helpText('It helps the organisation to determine how popular their organistion is and to track the places where their clients work and stay. coloured regions represent client distribution in their different countries and the uncoloured show countries where no one has used the application')),
    
      menuItem("CONCLUSION",icon = icon("dashboard")),
      box(width="12",helpText('Most countries using this application are English speaking countries. There is need for the application to support different languages in order to satisfy all their clients'))
      ),
    
    conditionalPanel(
      'input.free==="PLOTS"',
      box(width="12",helpText('These plots include two bar graphs and one piechart '))
    ),
    
    conditionalPanel(
      'input.free==="Bar graph"',
      box(width="12",helpText('This is a graph that presents grouped data with rectangular bars with length propotional to values they represent.')),
      
      menuItem("OBSERVATION",icon = icon("dashboard")),
      box(width="12",helpText('This determines the most active customer care personel in the organistation and how effecient other workers are.')),
      
      menuItem("CONCLUSION",icon = icon("dashboard")),
      box(width="12",helpText('It may help the organisation to determine wether to lay off the inactive customer care personels, or to change them in departments and also to recruit other customer care workers for efficient service delivery'))
    ),
    
    conditionalPanel(
      'input.free==="pie chart"',
      box(width="12",helpText('This is the graphical representation of quantitative data (vote status) by means of circle divided into sectors by which the sectors correspond to the relative quantities')), 
      
      menuItem("OBSERVATION",icon = icon("dashboard")),
      box(width="12",helpText('This determines the leading vote status of the clients, it helps to deternine how satisfied they are always with the customer care services')),
      
      menuItem("CONCLUSION",icon = icon("dashboard")),
      box(width="12",helpText('Generally the clients are not always satisfied'))
    ),
    
    conditionalPanel(
      'input.free==="Bar graph1"',
      box(width="12",helpText('This is a graph that presents grouped data with rectangular bars with length propotional to values they represent.')),
     
      menuItem("OBSERVATION",icon = icon("dashboard")), 
      box(width="12",helpText('This graph basically shows the leading country with many clients as per the map shows the countries where the clients are stationed')),
     
      menuItem("CONCLUSION",icon = icon("dashboard")),
     box(width="12",helpText('This helps the organisation to determine which coutry to invest in more of their services and pay attention to. more so get reasons of how to expand their market and raise the popularity in the already conquired regions'))
    ),
    
    conditionalPanel(
      'input.free==="WAITING TIME"',
      box(width="12",helpText('This shows the different number of seconds the clients wait while in the que before the clients attend to them.'))
      
    )
    
    
  ),
  ## Body content
  dashboardBody(
   
        fluidRow(
      
        box(width = "20",

          tabsetPanel(id = "free",
          tabPanel("RAW DATASET",icon=icon("table"), tableOutput("table1")),
          tabPanel("DOCUMENT MATRIX",icon=icon("table"), tableOutput("table")),

          tabPanel("WORD CLOUD",plotOutput("Wordcloud") ),
          tabPanel("WORD FREQUENCY",icon=icon("spinner"), plotOutput("freq")),
          tabPanel("SUMMARY", verbatimTextOutput("summary")),
          tabPanel("MAP",icon=icon("map"), plotOutput("mPlot", height="700px", width="950px")),
          navbarMenu("PLOTS",icon = icon("bar-chart-o"),
                     tabPanel("Bar graph", plotOutput("bar")),
                     tabPanel("pie chart", plotOutput("distPlot")),
                     tabPanel("Bar graph1",  plotOutput("yella"))),
          
          navbarMenu("WAITING TIME",icon=icon("clock"),
                    tabPanel("Worst waiting time",tableOutput("WWT")),
                    tabPanel(" Details of client with WWT",tableOutput("DWWT")),
                    tabPanel("Best waiting time(BWT)",tableOutput("BWT")),
                    tabPanel("Details of client with BWT", tableOutput("DBWT")),
                    tabPanel("Average Waiting time(AWT)",tableOutput("AWT")))),
        
        tabsetPanel(  
          
          tableOutput('table1'),
          tableOutput('table'),

          tableOutput('summary'),
          tableOutput('Wordcloud'),
          tableOutput('freq'),
          tableOutput('mPlot'),
          tableOutput('bar'),
          tableOutput('distPlot'),
          tableOutput('yella'),
          tableOutput('DWWT'),
          tableOutput('WWWT'),
          tableOutput('BWT'),
          tableOutput('DBWT'),
          tableOutput('AWT')
        )                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
       

        )
    )
  ))