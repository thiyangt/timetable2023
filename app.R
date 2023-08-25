# app.R ##

## Libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(DT)
library(randomcoloR)
library(readr)
library(hrbrthemes)
library(viridis)
library(shinyBS)

## Data sets

## Data for Course view Tab

### Overview data1
courseViewData <- read_csv("Data/courseViewData.csv")
courseViewData <- courseViewData %>%
  mutate(Day = factor(Day, levels = c('Sunday','Saturday','Friday','Thursday','Wednesday', 'Tuesday','Monday')))

courseViewData1 <- courseViewData  %>%
  distinct(Course.Title, .keep_all = TRUE)

tot.lec.day.dep <- courseViewData1 %>%
  group_by(Department, Day,Academic.Year) %>%
  summarise(Count = n())

tot.lec.day <- courseViewData1 %>%
  group_by(Day,Academic.Year) %>%
  summarise(Count = n())

tot.lec.day <- na.omit(tot.lec.day)
Department <- rep("All",nrow(tot.lec.day))
tot.lec.day <- data.frame(Department,tot.lec.day)

overview_data1_my <- rbind(tot.lec.day,tot.lec.day.dep)
overview_data1 <- overview_data1_my %>% pivot_wider(names_from = Academic.Year, values_from = Count)
overview_data1 <- overview_data1 %>% rename( "Year.1" = "1","Year.2" = "2","Year.3" = "3","Year.4" = "4")
overview_data1 <- overview_data1 %>%
  mutate(Day = factor(Day, levels = c('Sunday','Saturday','Friday','Thursday','Wednesday', 'Tuesday','Monday')))




## Overview KPIs

tot.dep.year <- courseViewData1 %>%
  group_by(Department, Academic.Year) %>%
  summarise(Count = n())
tot.dep.year

all.year <- courseViewData1 %>%
  group_by(Academic.Year) %>%
  summarise(Count = n())
all.year
all.year1 <- na.omit(all.year)

Department <- rep("All", nrow(all.year1))
tot.lec.year <- data.frame(Department, all.year1)
academic_kpi_data1 <- rbind(tot.lec.year,tot.dep.year)
ind <- seq(1:nrow(academic_kpi_data1))
academic_kpi_data1 <- cbind(ind, academic_kpi_data1)
academic_kpi_data <- academic_kpi_data1 %>%
  rename("...1" = ind, "Course.Count" = Count)



## Data for Lecture Hall View Tab
availabilty_data <- read_csv("Data/availability_data.csv")
lecture_hall_data <- read_csv("Data/lecture_hall_data.csv")



##################################################################################################

# Lecture halls character vector

lecture_halls_names <- unique(availabilty_data$Location)
lecture_halls_sizes <- c("All", "less than or equal to 250 seats", "less than or equal to 200 seats",
                         "less than or equal to 150 seats", "less than or equal to 100 seats",
                         "less than or equal to 50 seats")

lecture_halls_sizes1 <- c("600 - 250 Seats", "200 - 150 Seats", "140 - 100 Seats",
                          "85 - 60 Seats", "50 - 40 Seats",
                          "30 - 16 Seats") 

lecture_hall_days <-c("NA","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
time_slots <- c("700-150", "150-100", "100-50", "50 - 40", "40-10")

Lecture_Hall <- lecture_hall_data$`Lecture.Hall`
Seating_Capacity <- lecture_hall_data$`Seating.Capacity`
Lecture_count <- lecture_hall_data$`Lecture.count`

new_df7 <- data.frame(Lecture_Hall, Seating_Capacity, Lecture_count)

combined_df7 <- merge(new_df7, availabilty_data, by.x = "Lecture_Hall", by.y = "Location")

availabilty_data$Day <- ordered(availabilty_data$Day,
                                c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))


# Student availability KPI
student.sub.count <- courseViewData %>%
  group_by(Degree.Type, Academic.Year, Subject.Code) %>%
  summarise(Count = n())


## Lec hall Overview data
tot.dep.loc <- courseViewData %>%
  group_by(Department, Location) %>%
  summarise(Count = n())
tot.dep.loc

all.loc <- courseViewData %>%
  group_by(Location) %>%
  summarise(Count = n())
all.loc
all.loc1 <- na.omit(all.loc)

all.dat <- data.frame()
for (i in 1:nrow(all.loc1)) {
  if(all.loc1[i,2] > 10){
    all.dat <- rbind(all.dat , all.loc1[i,])
  }
}

Department <- rep("All",nrow(all.dat))
tot.lec.loc <- data.frame(cbind(Department, all.dat))
overview_loc_data <- rbind(tot.lec.loc,tot.dep.loc)




# to remove select all option in picker Input function
my_css <- "
.bs-select-all {
  display: none;
}
.bs-deselect-all {
  width: 100%;
}
"

# Creating the color palette
set.seed(34) # Set random seed
color_palette <- distinctColorPalette(34)
color_palette


############################################   UI    ###########################################


ui <- dashboardPage(skin="purple",
                    dashboardHeader(title = "FAS Timetable"),
                    dashboardSidebar(disable = TRUE),
                    
                    
                    dashboardBody(
                      
                      tags$head(
                        tags$style(HTML('
                            .main-header .logo {
                              font-family: "Calibri", Times, "Calibri", serif;
                              font-weight: bold;
                              font-size: 32px;
                              color: white;
                            }
                            .navbar-default {
                            font-family: "Calibri", Times, "Calibri", serif;
                              font-weight: bold;
                              font-size: 18px;
                              color: white;
                            background-color: #ADD8E6;
                            border-color: #ADD8E6;
                            }               
                           .nav.navbar-nav > li > a {
                            border-right: 1px solid white;
                          }
                          .nav.navbar-nav > li:last-child > a {
                            border-right: none;
                          }
                           body {
                            background-color:#ADD8E6 !important; /* Change the body background color using !important */
                          }
                        '    
                        )
                        )
                      ),
                      
                      
                      navbarPage(
                        "",
                        id = "navbarid",
                        tabPanel(
                          "Overview",
                          fluidRow(
                            
                            selectInput(
                              inputId = "option",
                              label = "Select Department",
                              choices = sort(unique(overview_data1$Department)),
                              width = "auto"
                            ),
                            
                            
                            #kp1
                            div(id='clickdiv1',valueBoxOutput("overview_kpi_1", width=3)),
                            bsModal("modalExample1", "Data Table", "clickdiv1", size = "large",dataTableOutput("table1")),
                            #kpi2
                            div(id='clickdiv2',valueBoxOutput("overview_kpi_2", width=3)),
                            bsModal("modalExample2", "Data Table", "clickdiv2", size = "large",dataTableOutput("table2")),
                            #kpi3
                            div(id='clickdiv3',valueBoxOutput("overview_kpi_3", width=3)),
                            bsModal("modalExample3", "Data Table", "clickdiv3", size = "large",dataTableOutput("table3")),
                            #kpi4
                            div(id='clickdiv4',valueBoxOutput("overview_kpi_4", width=3)),
                            bsModal("modalExample4", "Data Table", "clickdiv4", size = "large",dataTableOutput("table4")),
                            
                            actionButton("show", "Show")),
                          
                          fluidRow(
                            box(plotlyOutput("overview_plot1", height = "450"), width = 6),
                            box(plotlyOutput("overview_plot2", height = "450"), width = 6)
                          )
                        ),
                        
                        
                        tabPanel( "Course View",
                                  tabsetPanel(
                                    # Add two tabs to the main panel
                                    id = "tabs",
                                    tabPanel("Time Table",  fluidRow(
                                      box(width = 3,
                                          height = 300,
                                          
                                          ## user input 1 degree
                                          selectInput(
                                            inputId = "Degree.Type",
                                            label = h4("Select Degree"), 
                                            choices = unique(courseViewData$Degree.Type)),
                                          
                                          ## user input 2 year
                                          selectInput(
                                            inputId = "Academic.Year",
                                            label = h4("Select Academic Year"), 
                                            choices = NULL),
                                          
                                          ## user input 3 subject
                                          tags$head(tags$style(HTML(my_css))),
                                          
                                          pickerInput(
                                            inputId = "Subject.Code",
                                            label = h4("Select Subject/s"), 
                                            choices = NULL,
                                            options = list(`actions-box` = TRUE, size = 5)
                                          )
                                      ),
                                      
                                      # 1st visualization
                                      box(plotlyOutput("course_view_plot1", height = 270), width = 9, height = 300),
                                      
                                      # Course finder table
                                      box(downloadButton('download1',"Download Timetable"),
                                          dataTableOutput("CourseData"), width = 12))
                                    ),
                                    tabPanel("Course", # 2nd visualization
                                             fluidRow(
                                               column(width=6,
                                                      box(width = NULL,
                                                          height = 50,
                                                          
                                                          selectInput("optionh", label = h5("Select Department"),
                                                                      choices = sort(unique(courseViewData$Department)))))
                                               ,
                                               column(width=6,box(width =NULL,height = 50,
                                                                  selectInput(
                                                                    inputId = "Academic",
                                                                    label = h4("Select Academic Year"),
                                                                    choices = NULL)))),
                                             
                                             fluidRow(box( plotlyOutput("course_view_plot2", height = 380), width = 12,height=390),
                                                      box(plotlyOutput("course_plot", height="200"),width=12,height=210))))
                        ),  
                        
                        tabPanel(
                          "Student View",
                          fluidRow(
                            box(width = 3,
                                height = 500,
                                selectInput(
                                  inputId = "Degree.Type.student",
                                  label = h4("Select Degree"),
                                  choices = unique(courseViewData$Degree.Type)
                                ),
                                selectInput(
                                  inputId = "Academic.Year.student",
                                  label = h4("Select Academic Year"),
                                  choices = NULL
                                ),
                                tags$head(tags$style(HTML(my_css))),
                                pickerInput(
                                  inputId = "Subject.Code.student",
                                  label = h4("Select Subject/s"),
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE, size = 5,"max-options" = 3)
                                )
                            ),
                            box(plotlyOutput("student_view_plot1", height = 470), width = 9, height = 500)
                          )
                        ),
                        
                        tabPanel(
                          "Lecture Hall Capacity ",
                          fluidPage(
                            splitLayout(
                              # Treemap 
                              fluidPage(
                                fluidRow(
                                  box( width = 200, 
                                       height = 250,
                                       
                                       selectInput("opt1",
                                                   label = h4("Select Lecture Hall Size"),
                                                   choices = lecture_halls_sizes),
                                       
                                       selectInput("opt17",
                                                   label = h4("Lecture Halls with at least one vacant timeslot in that day"),
                                                   choices = lecture_hall_days)),
                                  
                                  box(width = 200,
                                      height = 470,
                                      
                                      selectInput("opt26",
                                                  label = h4("Select Seating Capacity"),
                                                  choices = lecture_halls_sizes1),
                                      #New Box plot , align="center"
                                      plotlyOutput("hall_view_plot3", height = 330, width = 450 ))
                                )
                              ),
                              
                              fluidPage(
                                verticalLayout(  
                                  box(plotlyOutput("hall_view_plot1", height = 675),width = 20)
                                )
                              )
                            ))
                        ),
                        
                        tabPanel(
                          "Lecture Hall Availability ",
                          fluidRow(
                            selectInput(
                              inputId = "opt", 
                              label = h4("Select Lecture Hall"), 
                              choices = lecture_halls_names,
                              width = "auto"
                            ),
                            valueBoxOutput("hall_view_kpi1", width = 3),
                            valueBoxOutput("hall_view_kpi2", width = 3),
                            valueBoxOutput("hall_view_kpi3", width = 3),
                            valueBoxOutput("hall_view_kpi4", width = 3)
                          ),
                          fluidRow(
                            box(plotOutput("hall_view_plot2",height = "450"),width=6,align="center"),
                            box(plotOutput("hall_view_plot4",height = "450"),width=6,align="center")
                          )
                        ),
                        
                        tabPanel(
                          "Input Data",
                          fluidRow(
                            column(
                              width = 4,
                              tags$h2("Upload Your Data"),
                              tags$hr(),
                              box(
                                width = 12,
                                height = 400,  
                                align = "center",
                                background = "olive",  
                                #fileInput("academic_kpi_data", "Upload academic_kpi_data CSV file"),
                                fileInput("availability_data", "Upload availability_data CSV file"),
                                fileInput("courseViewData", "Upload courseViewData CSV file"),
                                fileInput("lecture_hall_data", "Upload lecture_hall_data CSV file"),
                                #fileInput("overview_data1", "Upload overview_data1 CSV file"),
                                #fileInput("overview_data2", "Upload overview_data2 CSV file"),
                                br(),
                                actionButton("uploadButton", "Upload Files")
                              )
                            ),
                            column(
                              width = 4,
                              tags$h2("View"),
                              tags$hr(),
                             
                              style = 'width:6;overflow-x: scroll;height:500px;overflow-y: scroll;',
                              tableOutput("contents")
                            ),
                            column(
                              width = 4,
                              tags$h2("Download CSV Files"),
                              tags$hr(),
                              box(
                                width = 12,
                                height = 400,
                                align = "center",
                                background = "olive",  
                                div(style = "display: flex; flex-direction: column; align-items: center;",
                                    # Example files download section
                                    
                                    # Original data files download section
                                    tags$div(
                                      tags$h3("Original Data Files(Please Download these files before uploading new data)"),
                                      #downloadButton("download_original_academic_kpi_data", "Download original_academic_kpi_data",
                                      #               style = "height: 30px; width: 300px;"),
                                      downloadButton("download_original_availability_data", "Download availability_data",
                                                     style = "height: 30px; width: auto;"),
                                      downloadButton("download_original_courseViewData", "Download courseViewData",
                                                     style = "height: 30px; width: auto;"),
                                      downloadButton("download_original_lecture_hall_data", "Download lecture_hall_data",
                                                     style = "height: 30px; width: auto;")
                                    ),
                                    # Example files download section
                                    tags$div(
                                      tags$h3("Example Files(Use these files to input your data)"),
                                      #downloadButton("download_academic_kpi_data", "Download example_academic_kpi_data",
                                      #               style = "height: 30px; width: 300px;"),
                                      downloadButton("download_availability_data", "Download availability_data",
                                                     style = "height: 30px; width: auto;"),
                                      downloadButton("download_courseViewData", "Download courseViewData",
                                                     style = "height: 30px; width: auto;"),
                                      downloadButton("download_lecture_hall_data", "Download lecture_hall_data",
                                                     style = "height: 30px; width: auto;")
                                    ),
                                )
                              )
                            )
                          )
                        ),
                        
                        tabPanel(
                          "About",
                          tags$head(
                            tags$style(HTML("
                                code {
                                  display:block;
                                  padding:9.5px;
                                  margin:0 0 15px;
                                  margin-top:10px;
                                  font-size:20px;
                                  line-height:20px;
                                  word-break:break-all;
                                  word-wrap:break-word;
                                  white-space:pre-wrap;
                                  background-color:#F5F5F5;
                                  border:1px solid rgba(0,0,0,0.15);
                                  border-radius:4px;
                                  font-family:monospace;
                                }"
                            )
                            )
                          ),
                          
                          fluidRow(
                            column(width = 7,
                                   h4(strong("Created by")),
                                   p("- Sadrushi Dissanayake"),
                                   p("- Thisaakhya Jayakody"),
                                   p("- Lakna Perera"),
                                   p("- Menasha Senanayaka"),
                                   p("- Kalani Siriwardena"),
                                   p("- Trishika Wickramarathne"),
                                   p("Under the supervision of",
                                     tags$a(href = "https://thiyanga.netlify.app/", "Dr. Thiyanga S. Talagala.")
                                   ),
                                   h4(strong("Code")),
                                   p("The code used to construct this dashboard can be found on",
                                     tags$a(href = "https://github.com/STA474Y22G1/FAS_2nd_Semester_Timetable_Visualization", "GitHub.")
                                   ),
                                   h4(strong("Data")),
                                   p("Data for this dashboard was collected from the 12 departments operating under the Faculty of Applied Sciences of the University of Sri Jayewardenepura."),
                                   h4(strong("Update")),
                                   p("The data used is as per the first semester timetables finalized as of ****23rd December 2022 by the Dean's office.")
                            ),
                            
                            column(width = 5,
                                   h4(strong("Updated by")),
                                   p("- Nipuni Opatha"),
                                   p("- Thimali Fernando"),
                                   p("- Dinithi Hewawasam"),
                                   p("- Thilina Withanage"),
                                   p("- Ishara Wijayaratne"),
                                   p("- Sumedha Sandeepanee"),
                                   p("- Tharushi Kaveesha"),
                                   p("- Oshini Kulasekara"),
                                   p("- Upeksha Herath"),
                                   p("- Hansani Nawagamuwa"),
                                   p("- Denali Joseph"),
                                   p("Under the supervision of",
                                     tags$a(href = "https://thiyanga.netlify.app/", "Dr. Thiyanga S. Talagala.")
                                   )
                            ),
                          )
                        )
                      ) 
                    ))


#############################################  Server  ##############################################

# Server Function
server <- function(input, output, session) {
  
#######################################################

  uploadedData <- reactiveValues(data = NULL)
  
  # Read the uploaded CSV file and store it in the reactive value
  observeEvent(input$courseViewData, {
    inFile <- input$courseViewData
    if (!is.null(inFile)) {
      uploadedData$data <- read.csv(inFile$datapath)
      uploadedData$courseViewData <- uploadedData$data
    }
  })
  
  observeEvent(input$academic_kpi_data, {
    inFile <- input$academic_kpi_data
    if (!is.null(inFile)) {
      uploadedData$data <- read.csv(inFile$datapath)
      uploadedData$academic_kpi_data <- uploadedData$data
    }
  })
  
  observeEvent(input$lecture_hall_data, {
    inFile <- input$lecture_hall_data
    if (!is.null(inFile)) {
      uploadedData$data <- read.csv(inFile$datapath)
      uploadedData$lecture_hall_data <- uploadedData$data
    }
  })
  
  observeEvent(input$availability_data, {
    inFile <- input$availability_data
    if (!is.null(inFile)) {
      uploadedData$data <- read.csv(inFile$datapath)
      uploadedData$availability_data <- uploadedData$data
    }
  })
  
  observeEvent(input$overview_data1, {
    inFile <- input$overview_data1
    if (!is.null(inFile)) {
      uploadedData$data <- read.csv(inFile$datapath)
      uploadedData$overview_data1 <- uploadedData$data
    }
  })
  
  observeEvent(input$overview_data2, {
    inFile <- input$overview_data2
    if (!is.null(inFile)) {
      uploadedData$data <- read.csv(inFile$datapath)
      uploadedData$overview_data2 <- uploadedData$data
    }
  })
  
  output$contents <- renderTable({
    uploadedData$data
  })
  
  
  
  
  
  
  
  
  
  
######################################################
  output$download_academic_kpi_data <- downloadHandler(
    filename = function() {
      "academic_kpi_data.csv"
    },
    content = function(file) {
      file.copy("Data/example_academic_kpi_data.csv", file)
    }
  )
  
  output$download_availability_data <- downloadHandler(
    filename = function() {
      "availability_data.csv"
    },
    content = function(file) {
      file.copy("Data/example_availability_data.csv", file)
    }
  )
  
  output$download_courseViewData <- downloadHandler(
    filename = function() {
      "courseViewData.csv"
    },
    content = function(file) {
      file.copy("Data/example_courseViewData.csv", file)
    }
  )
  
  output$download_lecture_hall_data <- downloadHandler(
    filename = function() {
      "lecture_hall_data.csv"
    },
    content = function(file) {
      file.copy("Data/example_lecture_hall_data.csv", file)
    }
  )
  
  # Add downloadHandlers for the "Original Data Files" section
  output$download_original_academic_kpi_data <- downloadHandler(
    filename = function() {
      "original_academic_kpi_data.csv"
    },
    content = function(file) {
      file.copy("Data/original_academic_kpi_data.csv", file)
    }
  )
  
  output$download_original_availability_data <- downloadHandler(
    filename = function() {
      "original_availability_data.csv"
    },
    content = function(file) {
      file.copy("Data/original_availability_data.csv", file)
    }
  )
  
  output$download_original_courseViewData <- downloadHandler(
    filename = function() {
      "original_courseViewData.csv"
    },
    content = function(file) {
      file.copy("Data/original_courseViewData.csv", file)
    }
  )
  
  output$download_original_lecture_hall_data <- downloadHandler(
    filename = function() {
      "original_lecture_hall_data.csv"
    },
    content = function(file) {
      file.copy("Data/original_lecture_hall_data.csv", file)
    }
  )
  
  
  uploadedData <- reactiveValues()
  
  # Update the observeEvent for file uploads
  observeEvent(input$uploadButton, {
    # Check if the password is correct
    # Check if at least one file input has a file selected
    if (
        isTruthy(input$availability_data) ||
        isTruthy(input$courseViewData) ||
        isTruthy(input$lecture_hall_data)) {
      
      # Overwrite the existing files with the new uploaded files
      
      if (isTruthy(input$availability_data)) {
        file.copy(input$availability_data$datapath, "Data/availability_data.csv", overwrite = TRUE)
        uploadedData$availability_data <- read.csv("Data/availability_data.csv")
      }
      if (isTruthy(input$courseViewData)) {
        file.copy(input$courseViewData$datapath, "Data/courseViewData.csv", overwrite = TRUE)
        uploadedData$courseViewData <- read.csv("Data/courseViewData.csv")
      }
      if (isTruthy(input$lecture_hall_data)) {
        file.copy(input$lecture_hall_data$datapath, "Data/lecture_hall_data.csv", overwrite = TRUE)
        uploadedData$lecture_hall_data <- read.csv("Data/lecture_hall_data.csv")
      }
      
      
      # Show a success message
      showModal(modalDialog(
        title = "Upload Complete",
        "Files have been successfully uploaded and overwritten.",
        easyClose = TRUE
      ))
      
      ### inside this section whole code is repeated ###
#----------------------reading new data -------------------------------------------#
      ## Data sets
      
      ## Data for Course view Tab
      
      ### Overview data1
      courseViewData <- read_csv("Data/courseViewData.csv")
      courseViewData <- courseViewData %>%
        mutate(Day = factor(Day, levels = c('Sunday','Saturday','Friday','Thursday','Wednesday', 'Tuesday','Monday')))
      
      courseViewData1 <- courseViewData  %>%
        distinct(Course.Title, .keep_all = TRUE)
      
      tot.lec.day.dep <- courseViewData1 %>%
        group_by(Department, Day,Academic.Year) %>%
        summarise(Count = n())
      
      tot.lec.day <- courseViewData1 %>%
        group_by(Day,Academic.Year) %>%
        summarise(Count = n())
      
      tot.lec.day <- na.omit(tot.lec.day)
      Department <- rep("All",nrow(tot.lec.day))
      tot.lec.day <- data.frame(Department,tot.lec.day)
      
      overview_data1_my <- rbind(tot.lec.day,tot.lec.day.dep)
      overview_data1 <- overview_data1_my %>% pivot_wider(names_from = Academic.Year, values_from = Count)
      overview_data1 <- overview_data1 %>% rename( "Year.1" = "1","Year.2" = "2","Year.3" = "3","Year.4" = "4")
      overview_data1 <- overview_data1 %>%
        mutate(Day = factor(Day, levels = c('Sunday','Saturday','Friday','Thursday','Wednesday', 'Tuesday','Monday')))
      
      
      
      
      ## Overview KPIs
      
      tot.dep.year <- courseViewData1 %>%
        group_by(Department, Academic.Year) %>%
        summarise(Count = n())
      tot.dep.year
      
      all.year <- courseViewData1 %>%
        group_by(Academic.Year) %>%
        summarise(Count = n())
      all.year
      all.year1 <- na.omit(all.year)
      
      Department <- rep("All", nrow(all.year1))
      tot.lec.year <- data.frame(Department, all.year1)
      academic_kpi_data1 <- rbind(tot.lec.year,tot.dep.year)
      ind <- seq(1:nrow(academic_kpi_data1))
      academic_kpi_data1 <- cbind(ind, academic_kpi_data1)
      academic_kpi_data <- academic_kpi_data1 %>%
        rename("...1" = ind, "Course.Count" = Count)
      
      
      
      ## Data for Lecture Hall View Tab
      availabilty_data <- read_csv("Data/availability_data.csv")
      lecture_hall_data <- read_csv("Data/lecture_hall_data.csv")
      
      
      
      ##################################################################################################
      
      # Lecture halls character vector
      
      lecture_halls_names <- unique(availabilty_data$Location)
      lecture_halls_sizes <- c("All", "less than or equal to 250 seats", "less than or equal to 200 seats",
                               "less than or equal to 150 seats", "less than or equal to 100 seats",
                               "less than or equal to 50 seats")
      
      lecture_halls_sizes1 <- c("600 - 250 Seats", "200 - 150 Seats", "140 - 100 Seats",
                                "85 - 60 Seats", "50 - 40 Seats",
                                "30 - 16 Seats") 
      
      lecture_hall_days <-c("NA","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
      time_slots <- c("700-150", "150-100", "100-50", "50 - 40", "40-10")
      
      Lecture_Hall <- lecture_hall_data$`Lecture.Hall`
      Seating_Capacity <- lecture_hall_data$`Seating.Capacity`
      Lecture_count <- lecture_hall_data$`Lecture.count`
      
      new_df7 <- data.frame(Lecture_Hall, Seating_Capacity, Lecture_count)
      
      combined_df7 <- merge(new_df7, availabilty_data, by.x = "Lecture_Hall", by.y = "Location")
      
      availabilty_data$Day <- ordered(availabilty_data$Day,
                                      c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
      
      
      # Student availability KPI
      student.sub.count <- courseViewData %>%
        group_by(Degree.Type, Academic.Year, Subject.Code) %>%
        summarise(Count = n())
      
      
      ## Lec hall Overview data
      tot.dep.loc <- courseViewData %>%
        group_by(Department, Location) %>%
        summarise(Count = n())
      tot.dep.loc
      
      all.loc <- courseViewData %>%
        group_by(Location) %>%
        summarise(Count = n())
      all.loc
      all.loc1 <- na.omit(all.loc)
      
      all.dat <- data.frame()
      for (i in 1:nrow(all.loc1)) {
        if(all.loc1[i,2] > 10){
          all.dat <- rbind(all.dat , all.loc1[i,])
        }
      }
      
      Department <- rep("All",nrow(all.dat))
      tot.lec.loc <- data.frame(cbind(Department, all.dat))
      overview_loc_data <- rbind(tot.lec.loc,tot.dep.loc)
      
      
      
      
      # to remove select all option in picker Input function
      my_css <- "
.bs-select-all {
  display: none;
}
.bs-deselect-all {
  width: 100%;
}
"

# Creating the color palette
set.seed(34) # Set random seed
color_palette <- distinctColorPalette(34)
color_palette


#-------------------------New plots---------------------------------------------------#

  ## Overview Tab
  
  # KPIs
  # KPI 1
  # Number of lectures for 1st year
  output$overview_kpi_1 <- renderValueBox({
    option_kpi1 <- academic_kpi_data %>%
      filter(Department == input$option & Academic.Year == 1) 
    
    valueBox(value = option_kpi1$Course.Count,
             subtitle = "Courses offered in 1st Year",
             color = "fuchsia",
             icon = icon("book",lib='glyphicon'))
  })
  
  output$table1 <- renderDataTable({
    courseViewData %>% filter(Department == input$option)%>%
      filter(Lecture.Type=='Lecture')%>%filter(Academic.Year==1)%>% select(c(Department,Course.Title))
    
  })
  
  # KPI 2
  # Number of lectures for 2nd year
  output$overview_kpi_2 <- renderValueBox({
    option_kpi2 <- academic_kpi_data %>%
      filter(Department == input$option & Academic.Year == 2) 
    
    valueBox(value = option_kpi2$Course.Count,
             subtitle = "Courses offered in 2nd Year",
             color = "blue",
             icon = icon("pencil",lib='glyphicon'))
  })
  
  output$table2 <- renderDataTable({
    courseViewData %>% filter(Department == input$option) %>% filter(Lecture.Type=='Lecture')%>%filter(Academic.Year==2)%>% select(c(Department,Course.Title))
    
    
  })
  
  # KPI 3
  # Number of lectures for 3rd year
  output$overview_kpi_3 <- renderValueBox({
    
    option_kpi3 <- academic_kpi_data %>%
      filter(Department == input$option & Academic.Year == 3) 
    
    valueBox(value = option_kpi3$Course.Count,
             subtitle = "Courses offered in 3rd Year",
             color = "green",
             icon = icon("blackboard",lib='glyphicon'))
  })
  
  output$table3 <- renderDataTable({
    courseViewData %>%  filter(Department == input$option) %>% filter(Lecture.Type=='Lecture')%>%filter(Academic.Year==3)%>% select(c(Department,Course.Title))
    
    
  })
  
  # KPI 4
  # Number of lectures for 4th year
  output$overview_kpi_4 <- renderValueBox({
    option_kpi4 <- academic_kpi_data %>%
      filter(Department == input$option & Academic.Year == 4) 
    
    valueBox(value = option_kpi4$Course.Count,
             subtitle = "Courses offered in 4th Year",
             color = "yellow",
             icon = icon("education",lib='glyphicon'))
  })
  
  output$table4 <- renderDataTable({
    courseViewData %>% filter(Department == input$option)%>% filter(Lecture.Type=='Lecture')%>%filter(Academic.Year==4)%>% select(c(Department,Course.Title))
    
    
  })
  
  observeEvent(input$show, {
    showNotification("Click value boxes to view the courses")
  })
  
  
  # Cluster Bar Plot
  
  output$overview_plot1<- renderPlotly({
    overview_data1 %>%
      filter(Department==input$option) %>%
      plot_ly(y= ~Day, type = 'bar', orientation='h',x = ~Year.4, name = 'Year 4',height='450',width = 500,
              marker=list(color='#800000')) %>%
      add_trace(x = ~Year.3, name = 'Year 3',marker=list(color='#B22222')) %>%
      add_trace(x = ~Year.2, name = 'Year 2',marker=list(color='#F08080')) %>%
      add_trace(x = ~Year.1, name = 'Year 1',marker=list(color='#FFA07A')) %>%
      layout(
        title= "Distribution of Lectures by Day",
        yaxis = list(title = "Day",tickangle=-45,categoryorder =c( "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") ),
        xaxis = list(title = "Total Number of Lectures"),font = list(color = 'black'),
        barmode = 'cluster',
        autosize=FALSE,
        legend = list(traceorder = 'reversed') 
      )
  })
  
  
  
  # Treemap
  output$overview_plot2 <- renderPlotly({
    overview_loc_data %>%
      filter(Department==input$option) %>%
      plot_ly(
        type="treemap",
        source = "treemapplot",
        labels = ~ `Location`,
        parents = ~ "",
        values = ~ `Count`,
        texttemplate='%{label: labels } <br> %{value: 0.2s} <br> %{percentRoot}',
        domain = list(column = 0),
        textinfo = "label+value",
        marker = list(colors=c("#000",
                               "#EE82EE",
                               "#A9A9A9",
                               "purple",
                               "#F4A460",
                               "#90EE90",
                               "#48D1CC",
                               "#F0E68C",
                               "#32CD32")),
        colors = color_palette) %>%
      layout(title = "Number of Lectures in each location",
             uniformtext=list(minsize=16, mode="hide"),
             font = list(color = 'black'), margin=list(l=0, r=0, b=0, t=40))
    
  })
  
  
  
  ## Course View Tab
  
  # updating filters
  degree <- reactive({
    req(input$Degree.Type)
    filter(courseViewData, Degree.Type == input$Degree.Type)
  })
  
  year <- reactive({
    req(input$Academic.Year)
    filter(degree(), Academic.Year == input$Academic.Year)
  })
  
  subject <- reactive({
    
    # message to display when subject not selected
    validate(need(input$Subject.Code != "", "Please Select a Subject/s"))
    
    req(input$Subject.Code)
    filter(year(), Subject.Code %in% input$Subject.Code)
  })
  
  # observing event to update next filter
  observeEvent(degree(), {
    updateSelectInput(session, "Academic.Year",
                      choices = sort(unique(degree()$Academic.Year)), selected = 1)
  })
  
  observeEvent(year(), {
    updatePickerInput(session, "Subject.Code",
                      choices = sort(unique(year()$Subject.Code)), selected = c("STA","FST"))
  })
  
  
  # course view Plot1
  output$course_view_plot1 <- renderPlotly({
    
    plot1 <- subject() %>% ggplot(aes(label1 = Course,
                                      label2 = Lecture.Time)) +
      geom_linerange(aes(x = Starting.Time, xmin = Starting.Time,
                         xmax = Ending.Time, y = Day, color = Subject.Code),
                     linewidth = 2, position = position_dodge(0.5)) +
      geom_point(aes(Starting.Time, Day, color = Subject.Code), position = position_dodge(0.5)) +
      geom_point(aes(Ending.Time, Day, color = Subject.Code), position = position_dodge(0.5)) +
      scale_x_datetime(name = "Time", date_labels = "%H:%M", date_breaks = "1 hour") +
      theme_bw() +
      labs(title = "Student Availability", color = "Subject") +
      theme(axis.text.x = element_text(angle = 45), plot.title = element_text(hjust = 0.5))
    
    # interactive plot
    ggplotly(plot1, tooltip = c("label1", "label2"))
    
  })
  
  
  # course finder
  # Data set for course finder
  course <- reactive({
    subject() %>% filter(Lecture.Type == "Lecture") %>%
      select(Department, Stream, Day, Lecture.Time,  Course.Code, Course.Title,
             Lecturer.in.charge, Location)
  })
  
  
  # Course finder table
  output$CourseData <- renderDataTable(course(),
                                       options = list(paging = TRUE,
                                                      pageLength = 5,
                                                      dom = 'ftip'),
                                       colnames = c('Lecture Time' = 'Lecture.Time',
                                                    'Course Code' = 'Course.Code',
                                                    'Course Title' = 'Course.Title',
                                                    'Lecturer in Charge' = 'Lecturer.in.charge'),
                                       caption = htmltools::tags$caption(
                                         style = 'caption-side: top; text-align: Center; font-style: normal;
                                         color: black; font-family: arial; font-size: 1.8rem;',
                                         htmltools::em('Course Finder'))
  )
  
  # Making the data downloadable as a csv file
  output$download1 <- downloadHandler(
    filename = function(){"timetable.csv"},
    content = function(fname){
      write.csv(course(), fname)
    }
  )
  
  
  ## Student View Tab
  
  
  degrees <- reactive({
    req(input$Degree.Type.student)
    filter(courseViewData, Degree.Type == input$Degree.Type.student)
  })
  
  
  years <- reactive({
    req(input$Academic.Year.student)
    filter(degrees(), Academic.Year == input$Academic.Year.student)
  })
  
  subjects <- reactive({
    
    # message to display when subject not selected
    validate(need(input$Subject.Code.student != "", "Please Select a Subject/s"))
    
    req(input$Subject.Code.student)
    filter(years(), Subject.Code %in% input$Subject.Code.student)
  })
  
  
  # observing event to update next filter
  observeEvent(degrees(), {
    updateSelectInput(session, "Academic.Year.student",
                      choices = sort(unique(degrees()$Academic.Year)), selected = 1)
  })
  
  observeEvent(years(), {
    updatePickerInput(session, "Subject.Code.student",
                      choices = sort(unique(years()$Subject.Code)), selected = c("STA","CSC"))
  })
  
  
  #### Course tab
  
  department<- reactive({
    
    filter(courseViewData, Department == input$optionh)
  })
  year_department <- reactive({
    req(input$Academic)
    filter(department(), Academic.Year == input$Academic)
  })
  
  # observing event to update next filter
  observeEvent(department(), {
    updateSelectInput(session, "Academic",
                      choices = sort(unique(department()$Academic.Year)), selected = 1)
  })
  
  
  output$course_view_plot2 <- renderPlotly({
    
    df1 <-  year_department() %>% filter(Lecture.Type== "Lecture")
    df2<- df1%>% complete(Day,Lecturer.in.charge)
    df2$Day <- factor(df2$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    plot1 = df2 %>%  ggplot(aes(x = Day, y = Lecturer.in.charge ,label1= Lecture.Time, label2=Course )) + 
      theme(legend.position  =  "none", text =element_text(size=12)) +
      geom_tile(aes(fill=Subject.Code),color="black",linewidth = 1)+
      scale_fill_brewer(palette = "Set1", na.value= "white") +
      scale_fill_discrete(na.value= "white") +
      scale_x_discrete( position="top",expand=c(0,0))+
      scale_y_discrete(expand=c(0,0))  +
      geom_text(data = subset(df2, !is.na(Course.Code)), aes(label = paste(Course.Code, "\nLocation:", Location)), size = 2.5, nudge_y = 0.05)+
      theme_bw()+
      labs(title = "Lecturer by course")
    # interactive plot
    ggplotly(plot1, tooltip = c("label1", "label2"))    
    
  })
  output$course_plot <- renderPlotly({
    
    # make the cluster bar chart
    year_department()   %>% ggplot() +
      geom_bar(aes(x = Day, fill =Lecture.Type),position = position_dodge2(preserve = "single"))+
      scale_x_discrete(expand=c(0,0))+
      labs(title = "Count of Lecture type")+
      scale_y_discrete(limits = seq(0,5, 1))
    
  })
  
  
  
  ## Student view plot
  output$student_view_plot1 <- renderPlotly({
    
    plot2 <- subjects() %>% ggplot() +
      geom_rect(aes( x = Starting.Time, xmin = Starting.Time, xmax = Ending.Time,
                     y = Day, ymin = Day, ymax = Day, color = Subject.Code),
                position = position_dodge(0), size=18) +
      scale_x_datetime(name = "Time", date_labels = "%H:%M", date_breaks = "1 hour") +
      theme(
        panel.background = element_rect(fill = "#FFFAF0" ),
        panel.grid.major.x = element_line(color = "#8B4513",size = 1),
        panel.grid.minor.x = element_line(color = "#8B4513",size = 1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(color = "#8B4513", fill = NA,size = 1)
      )+
      labs(title = "Student Availability", color = "Subject") +
      theme(axis.text.x = element_text(angle = 45), plot.title = element_text(hjust = 0.5)) +
      
      geom_hline(yintercept = seq(0.5, 5-0.5, length.out = 5),linetype = "solid",size=0.2)+
      geom_line(aes(x = Starting.Time,
                    y = Day,
                    label1 = Course,
                    label2 = Lecture.Time),
                color='transparent') +
      geom_point(aes(x = Ending.Time,
                     y = Day,
                     label1 = Course,
                     label2 = Lecture.Time),
                 color='transparent')
    
    # interactive plot
    ggplotly(plot2, tooltip = c("label1", "label2"))
    
  })
  
  
  # Treemap 
  output$hall_view_plot1 <- renderPlotly({
    opt1_values <- c(600, 250, 200, 150, 100, 50)
    opt1_labels <- c("All", "less than or equal to 250 seats", "less than or equal to 200 seats",
                     "less than or equal to 150 seats", "less than or equal to 100 seats",
                     "less than or equal to 50 seats")
    
    selected_opt1 <- opt1_values[match(input$opt1, opt1_labels)]
    
    filtered_data <- combined_df7 %>%
      filter(Seating_Capacity <= as.numeric(selected_opt1))
    
    if (input$opt17 != "NA") {
      filtered_data <- filtered_data %>%
        filter(Day == input$opt17, Availability == 0) #available
    } 
    
    if (input$opt17 == "NA") {
      filtered_data <- filtered_data %>%
        distinct(Lecture_Hall, Seating_Capacity, .keep_all = TRUE)
    } else { 
      filtered_data <- filtered_data %>%
        distinct(Lecture_Hall, Seating_Capacity, Day, Availability, .keep_all = TRUE)
    }
    
    plot_ly(
      data = filtered_data,
      type = "treemap",
      source = "treemapplot",
      labels = ~ Lecture_Hall,
      parents = ~ "",
      values = ~ Seating_Capacity,
      domain = list(column = 0),
      textinfo = "label+value",
      colors = color_palette,
      uniformtext = list(minsize = 10, mode = "hide")
    ) %>%
      layout(
        title = "Lecture Hall Capacity",
        font = list(color = 'black'),
        margin = list(l = 0, r = 0, b = 0, t = 40)
      )
  })
  
  
  #Histogram Plot  
  output$hall_view_plot3 <- renderPlotly({
    opt26_values <- list(list(min = 250, max = 600), 
                         list(min = 150, max = 200), 
                         list(min = 100, max = 140), 
                         list(min = 60, max = 85), 
                         list(min = 40, max = 50), 
                         list(min = 16, max = 30))
    
    opt26_labels <- c("600 - 250 Seats", "200 - 150 Seats", "140 - 100 Seats",
                      "85 - 60 Seats", "50 - 40 Seats",
                      "30 - 16 Seats")
    
    selected_opt26 <- opt26_values[[match(input$opt26, opt26_labels)]]
    
    filtered_data26 <- lecture_hall_data %>%
      filter(`Seating.Capacity` >= as.numeric(selected_opt26$min),
             `Seating.Capacity` <= as.numeric(selected_opt26$max))
    
    # Plotting using plot_ly
    plot_ly(data = filtered_data26, x = ~`Lecture.Hall`, y = ~`Lecture.count`, color = ~`Lecture.Hall`, type = 'bar',
            colors = color_palette, legendgroup = ~`Lecture.Hall`) %>%
      layout(
        title = "Lecture Count by Seating Capacity",
        xaxis = list(
          title = "Lecture Hall",
          tickangle = -45
        ),
        yaxis = list(title = "Weekly Lecture Count"),
        font = list(color = 'black'),
        showlegend = TRUE
      ) 
  })
  
  
  # Heatmap
  output$hall_view_plot2 <- renderPlot({
    
    availabilty_data %>%
      filter(Location == input$opt) %>%
      mutate(Day = as.factor(Day)) %>%
      ggplot(aes(TimeSlot, Day, fill = Availability)) +
      geom_tile(color = "white", lwd = 1.0, linetype = 1) +
      coord_fixed() +
      scale_fill_continuous(breaks = 0:1, labels = c("Vacant", "Occupied")) +
      coord_fixed() +
      labs(title = "Lecture Hall Availability")+
      theme(plot.title = element_text(hjust = 0.5,size=18), axis.text.x = element_text(angle = 90))
  })
  
  
  #bar chart - new one 
  output$hall_view_plot4 <- renderPlot({
    filtered_data <- courseViewData %>%
      filter(Location == input$opt)
    
    
    lecture_counts <- filtered_data %>%
      group_by(Day) %>%
      summarize(lecture_count = n())
    lecture_counts$Day <- factor(lecture_counts$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    
    ggplot(lecture_counts, aes(x = Day, y = lecture_count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      xlab("Day") +
      ylab("Lecture Count") +
      ggtitle(paste("Number of Lectures per Day at Location:", input$opt))+
      scale_y_discrete(limits = seq(0, max(lecture_counts$lecture_count), 1))
    
  })
  
  
  
  # KPI 1
  # Number of courses per week
  output$hall_view_kpi1 <- renderValueBox({
    filtered_data <- courseViewData %>%
      filter(Location == input$opt)
    lecture_counts <- filtered_data %>%
      group_by(Day) %>%
      summarise(total_lectures = n())
    total_lectures_per_week <- sum(lecture_counts$total_lectures)
    
    
    valueBox(
      value = tags$p(paste0(total_lectures_per_week), style = "font-size: 40%;"),
      subtitle = "Number of lectures per week",
      color = "yellow"
    )
  })
  
  # KPI 2
  # Busiest Days of the Lecture Hall
  output$hall_view_kpi2 <- renderValueBox({
    option_kpi2 <- lecture_hall_data %>%
      filter(`Lecture.Hall` == input$opt)
    
    valueBox(value = tags$p(paste0(option_kpi2$`Bussiest.Day`), style = "font-size: 40%;"),
             subtitle = "Busiest Day/s of the Lecture Hall",
             color = "olive")
    
  })
  
  
  # KPI 3
  # capacity
  output$hall_view_kpi3 <- renderValueBox({
    option_kpi3 <- lecture_hall_data %>%
      filter(`Lecture.Hall` == input$opt)
    
    valueBox(value = tags$p(paste0(option_kpi3$`Seating.Capacity`), style = "font-size: 40%;"),
             subtitle = "Hall Capacity",
             # color = "blue")
             color = "red")
    
  })
  
  # KPI 4
  # capacity
  output$hall_view_kpi4 <- renderValueBox({
    option_kpi4 <- lecture_hall_data %>%
      filter(`Lecture.Hall` == input$opt)
    
    valueBox(value = tags$p(paste0(option_kpi4$`Description`), style = "font-size: 40%;"),
             subtitle = "Hall Description",
             # color = "blue")
             color = "navy")
    
  })
#-------------------------------------------------------------------------------------#
    } else {
      # Show an error message if no file input is selected
      showModal(modalDialog(
        title = "Error",
        "Please select at least one file to upload.",
        easyClose = TRUE
      ))
    }
  }
  )  
  ############------------------- Data Upload End ---------------------- ###############
  
  
  
  ## Overview Tab
  
  # KPIs
  # KPI 1
  # Number of lectures for 1st year
  output$overview_kpi_1 <- renderValueBox({
    option_kpi1 <- academic_kpi_data %>%
      filter(Department == input$option & Academic.Year == 1) 
    
    valueBox(value = option_kpi1$Course.Count,
             subtitle = "Courses offered in 1st Year",
             color = "fuchsia",
             icon = icon("book",lib='glyphicon'))
  })
  
  output$table1 <- renderDataTable({
    courseViewData %>% filter(Department == input$option)%>%
      filter(Lecture.Type=='Lecture')%>%filter(Academic.Year==1)%>% select(c(Department,Course.Title))
    
  })
  
  # KPI 2
  # Number of lectures for 2nd year
  output$overview_kpi_2 <- renderValueBox({
    option_kpi2 <- academic_kpi_data %>%
      filter(Department == input$option & Academic.Year == 2) 
    
    valueBox(value = option_kpi2$Course.Count,
             subtitle = "Courses offered in 2nd Year",
             color = "blue",
             icon = icon("pencil",lib='glyphicon'))
  })
  
  output$table2 <- renderDataTable({
    courseViewData %>% filter(Department == input$option) %>% filter(Lecture.Type=='Lecture')%>%filter(Academic.Year==2)%>% select(c(Department,Course.Title))
    
    
  })
  
  # KPI 3
  # Number of lectures for 3rd year
  output$overview_kpi_3 <- renderValueBox({
    
    option_kpi3 <- academic_kpi_data %>%
      filter(Department == input$option & Academic.Year == 3) 
    
    valueBox(value = option_kpi3$Course.Count,
             subtitle = "Courses offered in 3rd Year",
             color = "green",
             icon = icon("blackboard",lib='glyphicon'))
  })
  
  output$table3 <- renderDataTable({
    courseViewData %>%  filter(Department == input$option) %>% filter(Lecture.Type=='Lecture')%>%filter(Academic.Year==3)%>% select(c(Department,Course.Title))
    
    
  })
  
  # KPI 4
  # Number of lectures for 4th year
  output$overview_kpi_4 <- renderValueBox({
    option_kpi4 <- academic_kpi_data %>%
      filter(Department == input$option & Academic.Year == 4) 
    
    valueBox(value = option_kpi4$Course.Count,
             subtitle = "Courses offered in 4th Year",
             color = "yellow",
             icon = icon("education",lib='glyphicon'))
  })
  
  output$table4 <- renderDataTable({
    courseViewData %>% filter(Department == input$option)%>% filter(Lecture.Type=='Lecture')%>%filter(Academic.Year==4)%>% select(c(Department,Course.Title))
    
    
  })
  
  observeEvent(input$show, {
    showNotification("Click value boxes to view the courses")
  })
  
  
  # Cluster Bar Plot
  
  output$overview_plot1<- renderPlotly({
    overview_data1 %>%
      filter(Department==input$option) %>%
      plot_ly(y= ~Day, type = 'bar', orientation='h',x = ~Year.4, name = 'Year 4',height='450',width = 500,
              marker=list(color='#800000')) %>%
      add_trace(x = ~Year.3, name = 'Year 3',marker=list(color='#B22222')) %>%
      add_trace(x = ~Year.2, name = 'Year 2',marker=list(color='#F08080')) %>%
      add_trace(x = ~Year.1, name = 'Year 1',marker=list(color='#FFA07A')) %>%
      layout(
        title= "Distribution of Lectures by Day",
        yaxis = list(title = "Day",tickangle=-45,categoryorder =c( "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") ),
        xaxis = list(title = "Total Number of Lectures"),font = list(color = 'black'),
        barmode = 'cluster',
        autosize=FALSE,
        legend = list(traceorder = 'reversed') 
      )
  })
  
  
  
  # Treemap
  output$overview_plot2 <- renderPlotly({
    overview_loc_data %>%
      filter(Department==input$option) %>%
      plot_ly(
        type="treemap",
        source = "treemapplot",
        labels = ~ `Location`,
        parents = ~ "",
        values = ~ `Count`,
        texttemplate='%{label: labels } <br> %{value: 0.2s} <br> %{percentRoot}',
        domain = list(column = 0),
        textinfo = "label+value",
        marker = list(colors=c("#000",
                               "#EE82EE",
                               "#A9A9A9",
                               "purple",
                               "#F4A460",
                               "#90EE90",
                               "#48D1CC",
                               "#F0E68C",
                               "#32CD32")),
        colors = color_palette) %>%
      layout(title = "Number of Lectures in each location",
             uniformtext=list(minsize=16, mode="hide"),
             font = list(color = 'black'), margin=list(l=0, r=0, b=0, t=40))
    
  })
  
  
  
  ## Course View Tab
  
  # updating filters
  degree <- reactive({
    req(input$Degree.Type)
    filter(courseViewData, Degree.Type == input$Degree.Type)
  })
  
  year <- reactive({
    req(input$Academic.Year)
    filter(degree(), Academic.Year == input$Academic.Year)
  })
  
  subject <- reactive({
    
    # message to display when subject not selected
    validate(need(input$Subject.Code != "", "Please Select a Subject/s"))
    
    req(input$Subject.Code)
    filter(year(), Subject.Code %in% input$Subject.Code)
  })
  
  # observing event to update next filter
  observeEvent(degree(), {
    updateSelectInput(session, "Academic.Year",
                      choices = sort(unique(degree()$Academic.Year)), selected = 1)
  })
  
  observeEvent(year(), {
    updatePickerInput(session, "Subject.Code",
                      choices = sort(unique(year()$Subject.Code)), selected = c("STA","FST"))
  })
  
  
  # course view Plot1
  output$course_view_plot1 <- renderPlotly({
    
    plot1 <- subject() %>% ggplot(aes(label1 = Course,
                                      label2 = Lecture.Time)) +
      geom_linerange(aes(x = Starting.Time, xmin = Starting.Time,
                         xmax = Ending.Time, y = Day, color = Subject.Code),
                     linewidth = 2, position = position_dodge(0.5)) +
      geom_point(aes(Starting.Time, Day, color = Subject.Code), position = position_dodge(0.5)) +
      geom_point(aes(Ending.Time, Day, color = Subject.Code), position = position_dodge(0.5)) +
      scale_x_datetime(name = "Time", date_labels = "%H:%M", date_breaks = "1 hour") +
      theme_bw() +
      labs(title = "Lecture Times", color = "Subject") +
      theme(axis.text.x = element_text(angle = 45), plot.title = element_text(hjust = 0.5))
    
    # interactive plot
    ggplotly(plot1, tooltip = c("label1", "label2"))
    
  })
  
  
  # course finder
  # Data set for course finder
  course <- reactive({
    subject() %>% filter(Lecture.Type == "Lecture") %>%
      select(Department, Stream, Day, Lecture.Time,  Course.Code, Course.Title,
             Lecturer.in.charge, Location)
  })
  
  
  # Course finder table
  output$CourseData <- renderDataTable(course(),
                                       options = list(paging = TRUE,
                                                      pageLength = 5,
                                                      dom = 'ftip'),
                                       colnames = c('Lecture Time' = 'Lecture.Time',
                                                    'Course Code' = 'Course.Code',
                                                    'Course Title' = 'Course.Title',
                                                    'Lecturer in Charge' = 'Lecturer.in.charge'),
                                       caption = htmltools::tags$caption(
                                         style = 'caption-side: top; text-align: Center; font-style: normal;
                                         color: black; font-family: arial; font-size: 1.8rem;',
                                         htmltools::em('Course Finder'))
  )
  
  # Making the data downloadable as a csv file
  output$download1 <- downloadHandler(
    filename = function(){"timetable.csv"},
    content = function(fname){
      write.csv(course(), fname)
    }
  )
  
  
  ## Student View Tab
  
  
  degrees <- reactive({
    req(input$Degree.Type.student)
    filter(courseViewData, Degree.Type == input$Degree.Type.student)
  })
  
  
  years <- reactive({
    req(input$Academic.Year.student)
    filter(degrees(), Academic.Year == input$Academic.Year.student)
  })
  
  subjects <- reactive({
    
    # message to display when subject not selected
    validate(need(input$Subject.Code.student != "", "Please Select a Subject/s"))
    
    req(input$Subject.Code.student)
    filter(years(), Subject.Code %in% input$Subject.Code.student)
  })
  
  
  # observing event to update next filter
  observeEvent(degrees(), {
    updateSelectInput(session, "Academic.Year.student",
                      choices = sort(unique(degrees()$Academic.Year)), selected = 1)
  })
  
  observeEvent(years(), {
    updatePickerInput(session, "Subject.Code.student",
                      choices = sort(unique(years()$Subject.Code)), selected = c("STA","CSC"))
  })
  
  
  #### Course tab
  
  department<- reactive({
    
    filter(courseViewData, Department == input$optionh)
  })
  year_department <- reactive({
    req(input$Academic)
    filter(department(), Academic.Year == input$Academic)
  })
  
  # observing event to update next filter
  observeEvent(department(), {
    updateSelectInput(session, "Academic",
                      choices = sort(unique(department()$Academic.Year)), selected = 1)
  })
  
  
  output$course_view_plot2 <- renderPlotly({
    
    df1 <-  year_department() %>% filter(Lecture.Type== "Lecture")
    df2<- df1%>% complete(Day,Lecturer.in.charge)
    df2$Day <- factor(df2$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    plot1 = df2 %>%  ggplot(aes(x = Day, y = Lecturer.in.charge ,label1= Lecture.Time, label2=Course )) + 
      theme(legend.position  =  "none", text =element_text(size=12)) +
      geom_tile(aes(fill=Subject.Code),color="black",linewidth = 1)+
      scale_fill_brewer(palette = "Set1", na.value= "white") +
      scale_fill_discrete(na.value= "white") +
      scale_x_discrete( position="top",expand=c(0,0))+
      scale_y_discrete(expand=c(0,0))  +
      geom_text(data = subset(df2, !is.na(Course.Code)), aes(label = paste(Course.Code, "\nLocation:", Location)), size = 2.5, nudge_y = 0.05)+
      theme_bw()+
      labs(title = "Lecturer by course")
    # interactive plot
    ggplotly(plot1, tooltip = c("label1", "label2"))    
    
  })
  output$course_plot <- renderPlotly({
    
    # make the cluster bar chart
    year_department()   %>% ggplot() +
      geom_bar(aes(x = Day, fill =Lecture.Type),position = position_dodge2(preserve = "single"))+
      scale_x_discrete(expand=c(0,0))+
      labs(title = "Count of Lecture type")+
      scale_y_discrete(limits = seq(0,5, 1))
    
  })
  
  
  
  ## Student view plot
  output$student_view_plot1 <- renderPlotly({
    
    plot2 <- subjects() %>% ggplot() +
      geom_rect(aes( x = Starting.Time, xmin = Starting.Time, xmax = Ending.Time,
                     y = Day, ymin = Day, ymax = Day, color = Subject.Code),
                position = position_dodge(0), size=18) +
      scale_x_datetime(name = "Time", date_labels = "%H:%M", date_breaks = "1 hour") +
      theme(
        panel.background = element_rect(fill = "#FFFAF0" ),
        panel.grid.major.x = element_line(color = "#8B4513",size = 1),
        panel.grid.minor.x = element_line(color = "#8B4513",size = 1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(color = "#8B4513", fill = NA,size = 1)
      )+
      labs(title = "Student Availability", color = "Subject") +
      theme(axis.text.x = element_text(angle = 45), plot.title = element_text(hjust = 0.5)) +
      
      geom_hline(yintercept = seq(0.5, 5-0.5, length.out = 5),linetype = "solid",size=0.2)+
      geom_line(aes(x = Starting.Time,
                    y = Day,
                    label1 = Course,
                    label2 = Lecture.Time),
                color='transparent') +
      geom_point(aes(x = Ending.Time,
                     y = Day,
                     label1 = Course,
                     label2 = Lecture.Time),
                 color='transparent')
    
    # interactive plot
    ggplotly(plot2, tooltip = c("label1", "label2"))
    
  })
  
  
  # Treemap 
  output$hall_view_plot1 <- renderPlotly({
    opt1_values <- c(600, 250, 200, 150, 100, 50)
    opt1_labels <- c("All", "less than or equal to 250 seats", "less than or equal to 200 seats",
                     "less than or equal to 150 seats", "less than or equal to 100 seats",
                     "less than or equal to 50 seats")
    
    selected_opt1 <- opt1_values[match(input$opt1, opt1_labels)]
    
    filtered_data <- combined_df7 %>%
      filter(Seating_Capacity <= as.numeric(selected_opt1))
    
    if (input$opt17 != "NA") {
      filtered_data <- filtered_data %>%
        filter(Day == input$opt17, Availability == 0) #available
    } 
    
    if (input$opt17 == "NA") {
      filtered_data <- filtered_data %>%
        distinct(Lecture_Hall, Seating_Capacity, .keep_all = TRUE)
    } else { 
      filtered_data <- filtered_data %>%
        distinct(Lecture_Hall, Seating_Capacity, Day, Availability, .keep_all = TRUE)
    }
    
    plot_ly(
      data = filtered_data,
      type = "treemap",
      source = "treemapplot",
      labels = ~ Lecture_Hall,
      parents = ~ "",
      values = ~ Seating_Capacity,
      domain = list(column = 0),
      textinfo = "label+value",
      colors = color_palette,
      uniformtext = list(minsize = 10, mode = "hide")
    ) %>%
      layout(
        title = "Lecture Hall Capacity",
        font = list(color = 'black'),
        margin = list(l = 0, r = 0, b = 0, t = 40)
      )
  })
  
  
  #Histogram Plot  
  output$hall_view_plot3 <- renderPlotly({
    opt26_values <- list(list(min = 250, max = 600), 
                         list(min = 150, max = 200), 
                         list(min = 100, max = 140), 
                         list(min = 60, max = 85), 
                         list(min = 40, max = 50), 
                         list(min = 16, max = 30))
    
    opt26_labels <- c("600 - 250 Seats", "200 - 150 Seats", "140 - 100 Seats",
                      "85 - 60 Seats", "50 - 40 Seats",
                      "30 - 16 Seats")
    
    selected_opt26 <- opt26_values[[match(input$opt26, opt26_labels)]]
    
    filtered_data26 <- lecture_hall_data %>%
      filter(`Seating.Capacity` >= as.numeric(selected_opt26$min),
             `Seating.Capacity` <= as.numeric(selected_opt26$max))
    
    # Plotting using plot_ly
    plot_ly(data = filtered_data26, x = ~`Lecture.Hall`, y = ~`Lecture.count`, color = ~`Lecture.Hall`, type = 'bar',
            colors = color_palette, legendgroup = ~`Lecture.Hall`) %>%
      layout(
        title = "Lecture Count by Seating Capacity",
        xaxis = list(
          title = "Lecture Hall",
          tickangle = -45
        ),
        yaxis = list(title = "Weekly Lecture Count"),
        font = list(color = 'black'),
        showlegend = TRUE
      ) 
  })
  
  
  # Heatmap
  output$hall_view_plot2 <- renderPlot({
    
    availabilty_data %>%
      filter(Location == input$opt) %>%
      mutate(Day = as.factor(Day)) %>%
      ggplot(aes(TimeSlot, Day, fill = Availability)) +
      geom_tile(color = "white", lwd = 1.0, linetype = 1) +
      coord_fixed() +
      scale_fill_continuous(breaks = 0:1, labels = c("Vacant", "Occupied")) +
      coord_fixed() +
      labs(title = "Lecture Hall Availability")+
      theme(plot.title = element_text(hjust = 0.5,size=18), axis.text.x = element_text(angle = 90))
  })
  
  
  output$hall_view_plot4 <- renderPlot({
    filtered_data <- courseViewData %>%
      filter(Location == input$opt)
    
    lecture_counts <- filtered_data %>%
      group_by(Day) %>%
      summarize(lecture_count = n())
    lecture_counts$Day <- factor(lecture_counts$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    
    ggplot(lecture_counts, aes(x = Day, y = lecture_count, fill = lecture_count)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "#BEBEBE", high = "#A52A2A") +  # Ash color to brown
      xlab("Day") +
      ylab("Lecture Count") +
      ggtitle(paste("Number of Lectures per Day at Location:", input$opt)) +
      scale_y_continuous(
        limits = c(0, max(lecture_counts$lecture_count) + 1),
        breaks = seq(0, max(lecture_counts$lecture_count), 1)
      ) +
      theme_minimal()
  })
  
  
  
  
  
  # KPI 1
  # Number of courses per week
  output$hall_view_kpi1 <- renderValueBox({
    filtered_data <- courseViewData %>%
      filter(Location == input$opt)
    lecture_counts <- filtered_data %>%
      group_by(Day) %>%
      summarise(total_lectures = n())
    total_lectures_per_week <- sum(lecture_counts$total_lectures)
    
    
    valueBox(
      value = tags$p(paste0(total_lectures_per_week), style = "font-size: 40%;"),
      subtitle = "Number of lectures per week",
      color = "yellow"
    )
  })
  
  # KPI 2
  # Busiest Days of the Lecture Hall
  output$hall_view_kpi2 <- renderValueBox({
    option_kpi2 <- lecture_hall_data %>%
      filter(`Lecture.Hall` == input$opt)
    
    valueBox(value = tags$p(paste0(option_kpi2$`Bussiest.Day`), style = "font-size: 40%;"),
             subtitle = "Busiest Day/s of the Lecture Hall",
             color = "olive")
    
  })
  
  
  # KPI 3
  # capacity
  output$hall_view_kpi3 <- renderValueBox({
    option_kpi3 <- lecture_hall_data %>%
      filter(`Lecture.Hall` == input$opt)
    
    valueBox(value = tags$p(paste0(option_kpi3$`Seating.Capacity`), style = "font-size: 40%;"),
             subtitle = "Hall Capacity",
             # color = "blue")
             color = "red")
    
  })
  
  # KPI 4
  # capacity
  output$hall_view_kpi4 <- renderValueBox({
    option_kpi4 <- lecture_hall_data %>%
      filter(`Lecture.Hall` == input$opt)
    
    valueBox(value = tags$p(paste0(option_kpi4$`Description`), style = "font-size: 40%;"),
             subtitle = "Hall Description",
             # color = "blue")
             color = "navy")
    
  })
}

shinyApp(ui, server)
