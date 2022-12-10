fluidPage(
    
  theme = shinytheme("flatly"),

  # Navbar allows for the ui to slit into multiple different pages with different layout parameters
  navbarPage(
    
    # Navbar project name that appears in the top left corner
    "Crash",
    
    # First Tab Panel that servers as a landing page to describe the basics of the data before
    # Anything else is said about the data and model ----
    tabPanel("About",
             withMathJax(),
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
                           the severity of injuries in alcohol related crashes, using data from the
                           Texas Deparment of Transportation Crash Records Information System in 2018."),
                         p("Crash Severity ranges in order of:"),
                         p("N - Not Injured"),
                         p("C - Possible Injury"),
                         p("B - Suspected Minor Injury"),
                         p("A - Suspected Serious Injury"),
                         p("K - Fatal Injury")
                         ),
                  tabPanel("Model",
                         p("The model for this project uses a cumulative logit regression with a stopping ratio,
                           where the serverity of the crash is N,C,B,A,K, where K is the most severe (death).
                           Since there are 5 categories of crash severity, there will be 4 regressions, with j representing the crash severity.
                           Each model takes the form of:"),
                         helpText("\\begin{align*}
                                  ln\\left(\\frac{Pr(Y_i \\le j)}{Pr(Y_i > j)}\\right) &= \\beta_1 + \\beta_2 RoadClass + \\beta_3 SpeedLimit + \\beta_4 Weather
                                  \\\\ &+ \\beta_5 TimeofDay + \\beta_6 TypeofCollision + \\beta_7 Intersection
                                  \\end{align*}"),
                         p("The regression uses a stopping ratio, which means that the regression predicts the log odds of being at or lower than 
                           the current category versus being in a higher category. Therefore, when beta is positive, the crash is less likely to be severe, while
                           when beta is negative, the crash is more likely to severe."),
                         p("These are the reference levels for the model:"),
                         p("Road Class - County Road"),
                         p("Weather - Normal Weather"),
                         p("Time of Day - Morning"),
                         p("Type of Collision - One Car"),
                         p("At Intersection - False")
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
                                         label = "Select Variable:",
                                         choices = c("Crash Severity", "Road Class", "Speed Limit", "Weather", "Time of Day", "Type of Collision", "At Intersection Flag"),
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
                                         label = "Select Variable:",
                                         choices = c("Crash Severity", "Road Class", "Speed Limit", "Weather", "Time of Day", "Type of Collision", "At Intersection Flag"),
                                         selected = "Crash Severity"
                            )
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
                                         label = "Select Variable:",
                                         choices = c("Crash Severity", "Road Class", "Speed Limit", "Weather", "Time of Day", "Type of Collision", "At Intersection Flag"),
                                         selected = "Crash Severity")
                            
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
      tabPanel("Coefficient Plot",
               
               # Sidebar-mainpanel layout for Dot-Whisker Plot
               sidebarLayout(
                 
                 # Input Parameters for Coefficient Plot
                 sidebarPanel(
                   
                   # Input: check-box for models
                   checkboxGroupInput("InfCoefCheck",
                                      label = "Select Models:",
                                      choices = c("Stage 1", "Stage 2", "Stage 3", "Stage 4", "All"),
                                      selected = c("Stage 1", "Stage 2", "Stage 3", "Stage 4", "All")
                   ),
                   
                   # Download Button
                   downloadButton("InfCoefDwn",
                                  label = "Download")
                 ),
                 
                # Output Panel
                 mainPanel(
                   
                   # Output: Coefficient Plot for parameters
                   plotOutput("InfCoefPlot",
                              height = "800px")
                   
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
                
                # Input: Check boxes for Classes to Display
                checkboxGroupInput("InfProbClass",
                                   label = "Select Road Class:",
                                   choices = c("County Road", "City Street", "Farm to Market", "Interstate", "Non-Trafficway", "Other Road", "Tollways", "US & State Highways"),
                                   selected = c("County Road", "City Street", "Farm to Market", "Interstate", "Non-Trafficway", "Other Road", "Tollways", "US & State Highways")
                ),
                
                #Input: Radio Buttons for Bar building method
                radioButtons("InfSortInput",
                             label = "Sort by:",
                             choices = c("Stage", "Class"),
                             selected = "Stage"
                ),
                
                # Input: Invert Probabilities
                checkboxInput("InfProbInv",
                              label = "Invert Probability",
                              value = F
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
                             choices = c("Normal", "Dangerous"),
                             selected = "Normal"
                ),
                
                # Input: Radio Buttons for Type of Collision
                radioButtons("InfCollInput",
                             label = "Select Type of Collision",
                             choices = c("One Car", "Same", "Angular", "Opposite", "Other"),
                             selected = "Opposite"
                ),
                
                # Input: Numeric Input for Alpha level
                numericInput("InfAlphInput",
                             label = "Input alpha level as integer:",
                             min = 1,
                             max = 10,
                             value = 5),
                
                # Download Button
                downloadButton("InfProbDwn",
                               label = "Download")
                  
                 
               ),
               
               # Output Panel
               mainPanel(
                 
                 # Output: Plot for viewing results
                 plotOutput("InfProbPlot",
                            height = "800px")
               )
            ) 
    )
    )
  )
)