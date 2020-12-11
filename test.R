rm(list = ls())
library(DiceOptim)
library(ggplot2)
library(dplyr)
source("singlerunnoisyoptimizer.R")
source("optim_sithara.R")
library(shinycssloaders)

set.seed(123)



ui <- fluidPage(
  
  titlePanel("Adaptive Experiment Designer"), #App's title
  hr(),
  p(div(HTML(""))),
  
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(width=12,
               
               
               # Data Input:
               fileInput('datafile', 'Select your data CSV file',
                         accept=c('csv', 'comma-separated-values','.csv')),
               
               # Variables Inputs:
               uiOutput("selectize1"),
               uiOutput("selectize2"),
               uiOutput("action1"),
               hr(),
               uiOutput("lower1"),
               
               uiOutput("higher1"),
               
               
               
               # Run Button
               actionButton(inputId = "run", label = "Run"),
               hr(),
               # Download Button
               uiOutput("button_title"),
               uiOutput("download_button")
               
        ))),
    
    
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: HTML table with requested number of observations ----
      navbarPage("Output:",
                 tabPanel("Main",  
                          fluidPage(
                            fluidRow(
                              
                              textOutput("text1"),
                              uiOutput("setting") ,
                              uiOutput("new_value"),
                              hr(),
                              #plotOutput("plot") %>% withSpinner(color="#1E90FF")
                              uiOutput("plott")
                            )
                          )
                 ),
                 
                 tabPanel("New Dataset",  
                          fluidPage(
                            fluidRow(
                              p("The new dataset will appear here only after all the needed information is given to the app."),
                              tableOutput("table"),
                              tableOutput("rerunTest")
                              
                            )
                          )
                 )
                 
                 
                 
      )
    )
    
    ########################
  ) 
)




server <- function(input, output, session) {
  
  observeEvent(input$datafile, { #1. Getting data and variable determination 
    
    Dataset = read.csv(input$datafile$datapath)
    
    varnames <- names(Dataset)
    
    output$selectize1 <- renderUI({
      
      selectizeInput('invar',"Select Input Variables", choices = varnames, multiple = TRUE)
      
    }) 
    
    output$selectize2 <- renderUI({
      
      selectizeInput('dvar',"Select target Variable", choices = varnames, multiple = TRUE)
      
    }) 
    
    output$action1 <- renderUI({
      
      actionButton("next1", "Next")
      
      
    }) 
    
  }) #End of observation 1 for dataset
  
  
  observeEvent(input$next1, { #2 Giving lower/higher bound values
    
    
    
    output$lower1 <- renderUI({
      
      textInput("lower", "Replace each variable's name with its lower bound value", paste0(input$invar, collapse= " ,"))
      
    }) 
    
    output$higher1 <- renderUI({
      
      textInput("higher", "Replace each variable's name with its higher bound value", paste0(input$invar, collapse= " ,"))
      
    })
    
  }) #End of observation 2 after setting input and response variables 
  
  
  
  observeEvent(input$run, { #3 Running the model
    
    req(input$higher != paste0(input$invar, collapse= " ,"))
    
    # These are the acquisiton functions used to optimise the design space
    acquisitions = c("Random","AKG","EQI","Grid")
    
    # I'm not sure how to do a loop type thing in the app while still getting feedback (experimental result input) from the user!
    # I would like the default to be tocycle through the 4 acquisition funtions  but for now just picking at random
    i = sample(c(1:4),1)
    # i = 4
    #for(i in 1:4){
    acq = acquisitions[i] 
    
    res <- reactive({
      
      req(input$datafile)
      
      if(isFALSE(exists("acq"))){print("bad")
      }
      else{print("good") }
      
      #doe.size = dim(data)[2]-1 # size of the design space i.e. number of x / input variables
      data = read.csv(input$datafile$datapath)
      
      lower_x = as.numeric(unlist(strsplit(input$lower,",")))
      upper_x = as.numeric(unlist(strsplit(input$higher,",")))
      
      #prior_data = data[,1:doe.size]
      prior_data = data[,input$invar]
      #prior_response = -data[,4] # minimise the negative to optimise the positive
      prior_response = -data[,input$dvar]
      exp_type = rep("Prior",dim(data)[1])
      
      # First fit a noiseless model to estimate the nugget effect using MLE
      model_nugget <- km(y~1, # constant linear trend
                         design=prior_data, # current design
                         response=prior_response, # experimental results
                         covtype="matern5_2", # Matern covariance with smoothness 5/2 same as Weaver et al
                         optim.method="BFGS", # genetic optimizer, could also use bfgs here
                         nugget.estim=TRUE,
                         #noise.var=rep(noise.var,1,doe.size), # variance in the noise in the observations
                         lower=lower_x, # lower bounds of the correlation parameters for optimization
                         upper=upper_x, # upper bounds of the correlation parameters for optimization
                         control=list(trace=FALSE))
      
      # pull out the maximum likelihood estimate of the nugget
      noise.var= model_nugget@covariance@nugget
      # compare with var(prior_response[10:14])
      # Now use this nugget as our homogeneous noise estimate for the noisy GP model fit (noise.var and nugget are mutually exclusive in the diceoptim package)
      gp_model <- km(y~1,
                     design = prior_data,
                     response = prior_response,
                     covtype = "matern5_2",
                     optim.method = "BFGS",
                     noise.var = rep(noise.var,1,dim(prior_data)[1]),
                     lower = lower_x,
                     upper = upper_x,
                     control = list(trace = FALSE))
      
      ################################################################################################################
      ########### STEP 3. Optimise the aquisition to find the next design points #############################
      ################################################################################################################
      
      
      # This is the optimisation step. It can take from a few seconds (acq="Random") to a couple of minutes (acq="Grid")
      res <- optim_sithara(acq,gp_model,prior_data,prior_response,lower_x,upper_x)
      res <- round(t(res),1) # might need to be changed to 2 decimal places or a user input in a later version of the app
      return(res)
      
    })
    
    output$text1 <- renderText({
      
      req(input$datafile)
      
      
      paste0("Based on the ", acq, " acquisition function, please set up your next experiment at: ")
      
    })
    
    output$setting <- renderTable({
      
      res() 
      
    })
    # 
    
    data = reactive({
      
      read.csv(input$datafile$datapath)
      
    })
    
    output$new_value = renderUI({
      
      numericInput("efficiency", "Please enter the result (target response) of the experiment below to continue:", value = NA)
      
    })
    
    
    res_dframe = reactive({
      
      
      if(!is.na(input$efficiency)) {
        
        #prior_data = data[,1:doe.size]
        prior_data = data()[,input$invar]
        #prior_response = -data[,4] # minimise the negative to optimise the positive
        prior_response = -data()[,input$dvar]
        
        # doe.size = dim(data())[2]-1
        # prior_data = data()[,1:doe.size]
        # prior_response = -data()[,4]
        exp_type = rep("Prior",dim(data())[1])
        
        
        ## USER APP INPUT
        new_efficiency= input$efficiency # provided by experiment via engineer via app!
        
        ################################################################################################################
        ########### STEP 4. # Add the new datapoint to the dataset.  ###################################################
        ################################################################################################################
        
        # Not sure about this rounding. For now maybe keep everything at 1 decimal place. It depends on the sensitivity of the experimental inputs!
        res2 = round(res(),1) # Sithara requires rounding of 1 decimal place for her machine settings
        
        prior_data = rbind(prior_data,res2)
        prior_response = c(prior_response,-new_efficiency)
        exp_type = c(exp_type,acq)
        
        current_data = cbind(prior_data,prior_response,exp_type)
        
        # This dataset can be used for the output plot.
        current_data = cbind(c(1:dim(current_data)[1]),prior_data,prior_response,exp_type)
        
        ################################################################################################################
        ########### STEP 5. Update the plot  ###################################################
        ################################################################################################################
        names(current_data)=c("index",names(prior_data),"target_response","Acquisition")
        
        res_dframe=data.frame(current_data)
        
        return(res_dframe)
      }
      
    })
    
    
    
    
    max_response = reactive({
      
      prior_response = -data()[,input$dvar]
      #prior_response = -data()[,4]
      max_response <- -min(prior_response)
      return(max_response)
      
    })
    
    min_response  = reactive({
      
      prior_response = -data()[,input$dvar]    
      #prior_response = -data()[,4]
      min_response <- -max(prior_response)
      return(min_response)
      
    })
    
    
    output$plot <- renderPlot({
      
      if(!is.na(input$efficiency)) {
        
        p=ggplot(data = res_dframe())+
          geom_point( aes(x = index, y = -target_response, color = Acquisition),size=5) +
          xlab('Experiment number') +
          ylab('Target response') +
          expand_limits(y=c(min_response(),max_response()))
        
        p + theme_bw()+ #+  geom_text(aes(x = scan_speed_mmpers, y = efficiency_mgperl))+
          theme(text = element_text(size=20))
      } 
    })
    
    output$plott <- renderUI({
      
      plotOutput("plot") %>% withSpinner(color="#1E90FF")
      
    }) 
    
    
    output$button_title  =  renderUI({
      
      h4(div(HTML("Download new data")))
      
      
    })
    
    
    
    
    output$download_button  =  renderUI({
      
      
      downloadButton("downloadData", "Download")
      
    })
    
    new_data = reactive({
      
      test = res_dframe() %>% mutate(target_response = target_response * -1)
      #res_dframe()[,4] = res_dframe()[,4] * (-1)
      return(test)
      
    })
    
    
    output$table <- renderTable({
      
      if(!is.na(input$efficiency)) {
      new_data()
      }
      })
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      
      filename = function(){"New_data.csv"}, 
      content = function(fname){
        write.csv(new_data(), fname)
      }
    )
    
  }) #End of observation 3 after clicking run
  
  #Reset all inputs:
  # observeEvent(input$datafile,{ # 4 Reseting everything when a new dataset is observed
  #   
  #   # output$selectize1 = NULL
  #   # output$selectize2 = NULL
  #   output$button_title = NULL
  #   output$download_button = NULL
  #   output$text1 = NULL
  #   output$setting = NULL
  #   output$plot = NULL
  #   output$new_value = NULL
  #   
  # }) #End of observaion 4
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)

