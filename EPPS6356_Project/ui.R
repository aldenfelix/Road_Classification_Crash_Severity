fluidPage(
  # Navbar allows for the ui to slit into multiple different pages with different layout parameters
  navbarPage(
    
    # Navbar project name that appears in the top left corner
    "Project",
    
    # First Tab Panel that servers as a landing page to describe the basics of the data before
    # Anything else is said about the data and model ----
    tabPanel("About",
             sidebarLayout(
               sidebarPanel(p("This project was created by:"),
                            p("Alden Felix"),
                            p("Jim Pan"),
                            p("Nicholas Champagne"),
                            p("Will Kilcoyne")),
               mainPanel(
                 tabsetPanel(
                  tabPanel("Description",
                         p("This project looks at the effect of Road Classification on
                           the severity of injuries in alcohol related crashes, using data from 2018."),
                         p("The data for the project comes from the Texas Deparment of Transportation Crash Records Information System ")
                         ),
                  tabPanel("Model",
                         p("The model for this project uses a cumulative logit regression,
                           where the serverity of the crash ranges from 1 to 5, where 5 is the most severe (death)."),
                         p("The regression uses a stopping ratio, which means that the regression predicts the log odds of being at or lower than 
                           the current category versus being in a higher category. Therefore, when beta is positive, the crash is less likely to be severe, while
                           when beta is negative, the crash is more likely to severe")
                         )
                 )
               )
             )
    ),
    
    # Page dedicated to the Descriptive statistics of the model ----
    navbarMenu("Descriptive",
      
      # Descriptive Frequency Chart                 
      tabPanel("Frequency",
             
             # Sidebar-mainpanel layout for Descriptive Statistics
             sidebarLayout(
               
               #Input Parameters for Descriptive Statistics
               sidebarPanel(
                 
                 # Input: Radio Buttons for variable selection
                 radioButtons("DescRadio",
                              label = "Select Varaible:",
                              choices = c("Crash Severity", "Day.of.Week", "Road Class", "Speed Limit", "Weather", "Time of Day", "Type of Collision", "At Intersection Flag"),
                              selected = "Crash Severity")
                 
               ),
               mainPanel(
                 
                 # Output: Dynamic plot for Viewing Data
                 tableOutput("FreqPlot")
                 
               )
            )
    ),
    
      # Descriptive Pie Chart                 
      tabPanel("Pie",
               
               # Sidebar-mainpanel layout for Descriptive Statistics
               sidebarLayout(
                 
                 #Input Parameters for Descriptive Statistics
                 sidebarPanel(
                   
                   # Input: Radio Buttons for variable selection
                   radioButtons("DescRadio2",
                                label = "Select Varaible:",
                                choices = c("Crash.Severity", "Day.of.Week", "Road.Class", "Speed.Limit", "Weather", "Weather.Condition", "Time.of.Day", "Type.of.Collision", "At.Intersection.Flag"),
                                selected = "Crash.Severity")
                   
                 ),
                 mainPanel(
                   
                   # Output: Dynamic plot for Viewing Data
                   plotlyOutput("PiePlot")
                 )
               )
      ),
    
      # Descriptive Bar Chart                 
      tabPanel("Bar",
               
               # Sidebar-mainpanel layout for Descriptive Statistics
               sidebarLayout(

                 # #Input Parameters for Descriptive Statistics
                 sidebarPanel(

                   # Input: Radio Buttons for variable selection
                   radioButtons("DescRadio3",
                                label = "Select Varaible:",
                                choices = c("Crash.Severity", "Day.of.Week", "Road.Class", "Speed.Limit", "Weather", "Weather.Condition", "Time.of.Day", "Type.of.Collision", "At.Intersection.Flag"),
                                selected = "Crash.Severity")

                 ),
                 mainPanel(
                   
                   # Output: Dynamic plot for Viewing Data
                   plotlyOutput("BarPlot")
                 )
              )
      )
  ),
    
    # Page dedicated to the Inferential statistics of the model ----
    navbarMenu("Inferential",
               
      # Coefficient Plot of Variables
      tabPanel("Coefficient Table",
               
               # Sidebar-mainpanel layout for Dot-Whisker Plot
               sidebarLayout(
                 
                 # Input Parameters for Coefficient Plot
                 sidebarPanel(
                   
                   # Input: check-box for models
                   checkboxGroupInput("InfCoefCheck",
                                      label = "Select Models:",
                                      choices = c("Stage 1", "Stage 2", "Stage 3", "Stage 4"),
                                      selected = c("Stage 1", "Stage 2", "Stage 3", "Stage 4")
                   )
                 ),
                 
                # Output Panel
                 mainPanel(
                   
                   # Output: Coefficient Plot for parameters
                   tableOutput("InfCoefPlot")
                   
                 )
               )
      ),
      
      tabPanel("Coefficient Plot",
               
               # Sidebar-mainpanel layout for Dot-Whisker Plot
               sidebarLayout(
                 
                 # Input Parameters for Coefficient Plot
                 sidebarPanel(
                   
                   # Input: check-box for models
                   checkboxGroupInput("PlotCoefCheck",
                                      label = "Select Models:",
                                      choices = c("Stage 1", "Stage 2", "Stage 3", "Stage 4"),
                                      selected = c("Stage 1", "Stage 2", "Stage 3", "Stage 4")
                   )
                 ),
                 
                 # Output Panel
                 mainPanel(
                   
                   # Output: Coefficient Plot for parameters
                   plotOutput("CoefPlot")
                   
                 )
               )
      ),
      
      # Probability Plot of Variables
      tabPanel("Probability Plot",
             
             # Sidebar-mainpanel layout for Inferential Statistics
             sidebarLayout(
               
               # Input Parameters for Probability Plot
               sidebarPanel(
                 
                # Input: Check boxes for Models to Display
                checkboxGroupInput("InfProbCheck",
                                    label = "Select Models:",
                                    choices = c("Stage 1", "Stage 2", "Stage 3", "Stage 4"),
                                    selected = c("Stage 1", "Stage 2", "Stage 3", "Stage 4")
                 ),
                 
                # Input: Radio Buttons for Intersection Flag
                radioButtons("InfInterInput",
                              label = "At Intersection:",
                              choices = c("True", "False"),
                              selected = "False"
                ),
                
                # Input: Radio Buttons for Time of Day
                radioButtons("InfTimeInput",
                             label = "Select Time of Day:",
                             choices = c("Morning", "Day", "Evening", "Night"),
                             selected = "Morning"
                ),
          
                # Input: Slider for Speed limit
                sliderInput("InfSpeedInput",
                             label = "Select Speed:",
                             min = 5,
                             max = 85,
                             step = 5,
                             value = 45
                ),
                
                # Input: Radio Buttons for Weather
                radioButtons("InfWeathInput",
                             label = "Select Weather",
                             choices = c("Normal Weather", "Dangerous Weather"),
                             selected = "Normal Weather"
                ),
                
                # Input: Radio Buttons for Type of Collision
                radioButtons("InfCollInput",
                             label = "Select Type of Collision",
                             choices = c("One Car", "Same Direction", "Angular", "Opposite Direction", "Other"),
                             selected = "Opposite Direction"
                ),
                
                # Input: Numeric Input for Alpha level
                numericInput("InfAlphInput",
                             label = "Input alpha level as integer:",
                             min = 1,
                             max = 10,
                             value = 5)
                  
                 
               ),
               
               # Output Panel
               mainPanel(
                 
                 # Output: Plot for viewing results
                 tableOutput("InfProbPlot")
               )
            ) 
    )
    ),
    
    # Page dedicated to anything else ----
    tabPanel("Other",
             sidebarLayout(
               sidebarPanel(p("text")),
               mainPanel(p("main"))
            )
    )
  )
)