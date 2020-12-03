rm(list = ls())
library(DiceOptim)
library(ggplot2)
source("singlerunnoisyoptimizer.R")
source("optim_sithara.R")
set.seed(123)



ui <- fluidPage(
  
  titlePanel("Application"), #App's title
  hr(),
  p(div(HTML(""))),
  
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(width=12,
               
               h4(div(HTML("<em>Initial values:</em>"))),

            # Data Input:
               fileInput('datafile', 'Select your data CSV file',
                         accept=c('csv', 'comma-separated-values','.csv')),
               
            # Variables Inputs:
               textInput("lower", "Enter lower_d vector (comma delimited)", "10,1.8,10"),
               
               textInput("higher", "Enter upper_d vector (comma delimited)", "30,2.6,30"),
               
               numericInput("efficiency", "new_efficiency", value = NA),
            
               uiOutput("reset_button"),
            
               
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
                 tabPanel("Tab 1",  
                          fluidPage(
                            fluidRow(
                              
                              textOutput("text1"),
                             uiOutput("setting"),
                             hr(),
                             plotOutput("plot")
                              
                              
                            )
                          )
                 ),
                 
                 tabPanel("About",  
                          fluidPage(
                            fluidRow(
                          
                              
                              
                            )
                          )
                 )
                 
                 
                 
      )
    )
    
    ########################
  ) 
)




server <- function(input, output, session) {
 
  
  
  observeEvent(input$run, {
  
    req(input$datafile)
    
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
    
    data = read.csv(input$datafile$datapath)
    
    doe.size = dim(data)[2]-1 # size of the design space i.e. number of x / input variables
    

    lower_x = as.numeric(unlist(strsplit(input$lower,",")))
    upper_x = as.numeric(unlist(strsplit(input$higher,",")))
    
    prior_data = data[,1:doe.size]
    prior_response = -data[,4] # minimise the negative to optimise the positive
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
     
  res_dframe = reactive({
    
     doe.size = dim(data())[2]-1 
     prior_data = data()[,1:doe.size]
     prior_response = -data()[,4]
     exp_type = rep("Prior",dim(data())[1])
     
     
    ## USER APP INPUT
    new_efficiency=0.42 # provided by experiment via engineer via app!

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
    
     })

  
  
  
  max_response = reactive({
          
          prior_response = -data()[,4]
          max_response <- -min(prior_response)
          return(max_response)
    
             })
    
  min_response  = reactive({
        
              prior_response = -data()[,4]
               min_response <- -max(prior_response)
               return(min_response)
               
              })
               
               
    output$plot <- renderPlot({
      
    p=ggplot(data = res_dframe())+
      geom_point( aes(x = index, y = -target_response, color = Acquisition),size=5) +
      xlab('Experiment number') +
      ylab('Target response') +
      expand_limits(y=c(min_response(),max_response()))

    p + theme_bw()+ #+  geom_text(aes(x = scan_speed_mmpers, y = efficiency_mgperl))+
      theme(text = element_text(size=20))

  })
    
    output$reset_button  =  renderUI({
      

    actionButton("reset", "Reset all")
    
    
    })
    
    
             output$button_title  =  renderUI({
               
               h4(div(HTML("Download new data")))
               
               
             })
    
    
    
    
          output$download_button  =  renderUI({
               
               downloadButton("downloadData", "Download")
               
             })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function(){"New_data.csv"}, 
    content = function(fname){
      write.csv(data(), fname)
    }
  )
  

  }) #End of observation
  
  #Reset all inputs:
  observeEvent(input$reset,{
    
    updateTextInput(session,"lower", "lower_x vector", "10,1.8,10")
    updateTextInput(session, "higher", "upper_x vector", "30,2.6,30")
    output$button_title = NULL
    output$download_button = NULL
    output$reset_button = NULL
    output$text1 = NULL
    output$setting = NULL
    output$plot = NULL
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

