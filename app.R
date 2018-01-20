# Shiny App of NASS EDR data
#
# Version pushed to Shiny server January 18, 2018

# setwd("C:/_Files/NASS/deployed") # delete when pushing to the server

# Grabbing libraries
library(shiny)
library(dplyr)
library(ggvis)
#library(ggplot2)
library(data.table)

# Get make/model lists
edr <- read.csv("nass_edr.csv", stringsAsFactors = FALSE)
years <- sort(as.numeric(unique(edr$veh_year)))
make_list <- rbind("All", sort(as.character(unique(edr$veh_make))))
model_list <- rbind("All", sort(as.character(unique(edr$veh_model))))

# Creating dV buckets
r_factor <- c('0-4.9', '5-9.9', '10-14.9', '15-19.9', '20-24.9', '25-29.9', '30-34.9', '35-39.9')

# Creating link to NASS website
createLink <- function(x) {
  sprintf(paste0('<a href="http://www-nass.nhtsa.dot.gov/nass/cds/CaseForm.aspx?xsl=main.xsl&CaseID=',x,'" target="_blank">',x,'</a>', sep=""))
}

# Creating link to EDR printouts
createEDRlink <- function(case, vehnum, year1, pdf) {
  sprintf(paste('<a href="http://www.collisionintelligence.com/edr_reports/', case, '-v',vehnum, pdf,'" target="_blank">',case,'</a>', sep=""))
}

# Calculating deployment ratios (5 mph bins)
calc_ratios <- function(x) {
  # Assigning to buckets
  x <- mutate(x, bucket0 = -trunc(edr_dV_long / 5,0) * 5)
  x <- mutate(x, bucket = paste0(bucket0, "-", bucket0+4.9))
  
  # Assigning deployed state
  x <- mutate(x, sw_ND = ifelse(sw_deploy == "Deployed", 0, 1))
  x <- mutate(x, sw_D = ifelse(sw_deploy == "Deployed", 1, 0))

  # Aggregating
  ratios <- x %>%
    group_by(bucket0, bucket) %>%
    summarise(deployed = sum(sw_D), nonDeployed = sum(sw_ND))
  
  # Calculating ratio
  ratios <- as.data.frame(ratios)
  ratios <- mutate(ratios, ratio = 100 * round(deployed/(deployed+nonDeployed),2))
  ratios <- mutate(ratios, n = deployed + nonDeployed)

  # Changing bucket to a factor variable
  ratios$bucket <- factor(ratios$bucket, levels=r_factor, ordered=TRUE)

  # Returning ratios
  ratios
}

ratios <- calc_ratios(edr)



# *****************************
# *    User interface code    *
# *****************************

ui <- fluidPage(
  # Google Analytics Code
  tags$head(includeScript('google-analytics.js')),
  
  titlePanel("Airbag Explorer"),
  
  # Hiding Error Messages
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # Input functions
  sidebarLayout(
    sidebarPanel(
      selectInput("make_select", "Vehicle Make:", make_list,  selected="All", width = 200),
      selectInput("model_select", "Vehicle Model:", model_list,  selected="All", width = 200),
      selectInput("start_year", "From Year:", years,  selected=1993, width = 90),
      selectInput("end_year", "To Year:", years,  selected=2016, width = 90)
    ),
    
    # Output functions
    mainPanel(
     ggvisOutput("p")
    )
  ), 
  
  dataTableOutput("dtable")
)

# ****************************
# *      Server code         *
# ****************************

server <- function(input, output, session) {
  
  ratios2 <- reactive({
      # Gathering User Input
      make_select_x <- as.character(input$make_select)
      model_select_x <- as.character(input$model_select)
      year1_select_x <- as.numeric(input$start_year)
      year2_select_x <- as.numeric(input$end_year)
      
      # Applying filters
      edr2 <- edr
      edr2 <- filter(edr2, veh_year >= year1_select_x)
      edr2 <- filter(edr2, veh_year <= year2_select_x)
      
      if (make_select_x != "All") { edr2 <- filter(edr2, veh_make == make_select_x) }
      if (model_select_x != "All") { edr2 <- filter(edr2, veh_model == model_select_x)}
      
      # Returning filtered results
      calc_ratios(edr2)

  })
  
  # Plotting deployment ratios
  ratios2 %>%
    ggvis(~bucket, ~ratio, fill:='lightblue') %>%
    layer_bars() %>%
    add_axis("x", title = "Longitudinal Delta-V [mph]", properties = axis_props(grid=list(stroke = NA))) %>%
    add_axis("y", title = "Events where the front airbags deployed [%]") %>%
    set_options(renderer = "canvas") %>%
    #add_tooltip(function(data){ paste0("Sample Size:", as.character(data$ratio)) }, "hover") %>%
    hide_legend("fill") %>%
    bind_shiny("p", "p_ui")

  # Creating data table output
  output$dtable = renderDataTable({
    temp_df <- select(edr, caseid, year, veh_no, casestr, veh_year, veh_make, veh_model, veh_severity, front_ab, edr_speed, edr_dV_long) 
    if(input$make_select != "All") { temp_df <- filter(temp_df, veh_make == input$make_select) }
    if(input$model_select != "All") { temp_df <- filter(temp_df, veh_model == input$model_select) }
    temp_df <- filter(temp_df, veh_year >= input$start_year, veh_year <= input$end_year)
    temp_df <- rename(temp_df, Year = veh_year, Make = veh_make, Model = veh_model, Damage =  veh_severity,  Deployment =  front_ab, Speed = edr_speed, DeltaV = edr_dV_long)
    temp_df <- mutate(temp_df, CaseID = createLink(caseid))
    temp_df <- mutate(temp_df, pdf = ifelse(year < 2008, ".PDF", ".pdf"))
    temp_df <- mutate(temp_df, EDR.Report = ifelse(year > 2013, "", createEDRlink(casestr,veh_no, year, pdf)))
    temp_df <- select(temp_df, -caseid, -casestr, -veh_no, -year, -pdf)
    names(temp_df) <- c("Year", "Make", "Model", "Damage", "Front Airbag Status", "Impact Speed [mph]", "Longitudinal dV [mph]", "NASS CaseID", "EDR Report") 
    temp_df
  },
  
  options = list(searching = FALSE), escape = FALSE)
 
  # Linking models to the veh make 
  observe({
    make_select_x <- as.character(input$make_select)
    if (make_select_x == "All") {   updateSelectInput(session, "model_select", choices = rbind("All", sort(as.character(unique(edr$veh_model))))) }
    else { updateSelectInput(session, "model_select", choices = rbind("All", sort(as.character(unique(filter(edr,veh_make==make_select_x)$veh_model))))) }
  })
   
}

shinyApp(ui=ui, server = server)