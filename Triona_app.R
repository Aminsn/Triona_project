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
                              tableOutput("setting") %>% withSpinner(color="#1E90FF"),
                              uiOutput("new_value"),
                              uiOutput("proc"),
                              #uiOutput("reRun"),
                              hr(),
                              #plotOutput("plot") %>% withSpinner(color="#1E90FF")
                              uiOutput("plott")
                            )
                          )
                 ),
                 
                 tabPanel("Dataset",  
                          fluidPage(
                            fluidRow(
                              p("The new dataset will appear here only after all the needed information is given to the app."),
                              tableOutput("table"),
                             plotOutput("plot2")
                            )
                          )
                 )
                 
                 
                 
      )
    )
    
    ########################
  ) 
)




server <- function(input, output, session) {
  
  
  
  df_dim = reactiveVal(NULL)
  df_text = reactiveVal(NULL)
  df_text1 = reactiveVal(NULL)
  
  values = reactiveValues()
  
  
  observeEvent(input$datafile, { #1. Getting data and variable determination

      Dataset = read.csv(input$datafile$datapath)
      df_dim(   rep("Prior", nrow(Dataset))  )

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

  
  data = reactive({ #Check if there is new data, if not use the previous one
    
    
    if(is.null(values$DF)){
      
                 #df0 = 
                   read.csv(input$datafile$datapath)
                 # df0$Acquisition = rep("Prior", nrow(df0))
                 # return(df0)
      
      }
    else{values$DF}
    
  })

  

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
    values$ACQ = acq

      req(input$datafile)
      
      # if(is.null(values$DF)){
      # data = read.csv(input$datafile$datapath)
      # }else{data = values$DF}
      #doe.size = dim(data)[2]-1 # size of the design space i.e. number of x / input variables
tryCatch({
      lower_x = as.numeric(unlist(strsplit(input$lower,",")))
      upper_x = as.numeric(unlist(strsplit(input$higher,",")))

      #prior_data = data[,1:doe.size]
      prior_data = data()[,input$invar]
      #prior_response = -data[,4] # minimise the negative to optimise the positive
      prior_response = -data()[,input$dvar]
      #exp_type = rep("Prior",nrow(data()))

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
      values$RES = res
  }, error = function(err){
    showNotification(paste0("Something went wrong! Please make sure you entered correct values."), type = 'err')
  })

    }) #End of 1st run

  
    
    observe({
      
      
      #req(values$RES)
      req(input$run)
      
      df_text(
      numericInput("efficiency", "Please enter the result (target response) of the experiment below and push the Proceed button to update the data for the next run:", value = NA)
      ) 
      
      df_text1(
        paste0("Based on the ", values$ACQ, " acquisition function, please set up your next experiment at: ")
      )
      
    })
    
    output$text1 <- renderText({ #Report based on the run
      
      req(values$RES)
      req(input$run)
      df_text1()
      
    })
    
    output$setting <- renderTable({ #Report based on the run
      
      #req(values$RES)
      req(input$run)
      
      values$RES
    })
    
    
    
    output$new_value = renderUI({ #Ask for new input (efficiency)

      req(input$run)
      req(values$RES)
      df_text()

      #numericInput("efficiency", "Please enter the result (target response) of the experiment below to continue:", value = NA)

    })
    
 
    output$proc = renderUI({ #Permition to proceed
      
      req(values$RES)
      
          actionButton("proceed", "Proceed")  
          
    })
    
    
    res_dframe = eventReactive(input$proceed, { #Updating dataset


      req(input$efficiency)

        #prior_data = data[,1:doe.size]
        prior_data = data()[,c(input$invar)]
        #prior_response = -data[,4] # minimise the negative to optimise the positive
        prior_response = -data()[,input$dvar]

        # doe.size = dim(data())[2]-1
        # prior_data = data()[,1:doe.size]
        # prior_response = -data()[,4]
        
        

        ## USER APP INPUT
        new_efficiency= input$efficiency # provided by experiment via engineer via app!

        ################################################################################################################
        ########### STEP 4. # Add the new datapoint to the dataset.  ###################################################
        ################################################################################################################

        # Not sure about this rounding. For now maybe keep everything at 1 decimal place. It depends on the sensitivity of the experimental inputs!
        res2 = round(values$RES,1) # Sithara requires rounding of 1 decimal place for her machine settings

        prior_data = rbind(prior_data,res2)
        prior_response = c(prior_response,-new_efficiency)
        #exp_type = data()$Acquisition #data()$Acquisition
        #exp_type2 = c(data()$Acquisition,"test") #values$ACQ
        df_dim(c(df_dim(),values$ACQ))

        current_data = cbind(prior_data,prior_response,df_dim())

        # This dataset can be used for the output plot.
        current_data = cbind(c(1:dim(prior_data)[1]),prior_data,prior_response,df_dim())

        names(current_data)=c("index",names(prior_data),"target_response","Acquisition")

        res_dframe=data.frame(current_data)

        return(res_dframe)


    })

    
 
    
    
    observeEvent(input$proceed,{
      
      req(res_dframe())
      
      values$DF = res_dframe() %>% mutate(target_response = target_response * -1)
      
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
      
      req(input$proceed)

      if(!is.null(values$DF)){

      p=ggplot(data = values$DF)+
        geom_point( aes(x = index, y = target_response, color = Acquisition),size=5) +
        xlab('Experiment number') +
        ylab('Target response') +
        expand_limits(y=c(min_response(),max_response()))
      
      p + theme_bw()+ #+  geom_text(aes(x = scan_speed_mmpers, y = efficiency_mgperl))+
        theme(text = element_text(size=20))
      }else{}
      
    })
    
    
    output$plot2= renderPlot({
      
      data = values$DF
      doe.size = length(values$DF)-1 # size of the design space i.e. number of x / input variables
      # Input lower and upper range of values.
      # Min and max from prior and a slider?
      # Vector input by the user
      ## USER APP INPUT
      lower_x = as.numeric(unlist(strsplit(input$lower,",")))
      upper_x = as.numeric(unlist(strsplit(input$higher,",")))
      # Inputs to the optimisation rgenoud algorithms bfgs
      # These can be set as defaults (not a user input)
      prior_data = data[,1:doe.size]
      prior_response = -data[,4] # minimise the negative to optimise the positive
      #exp_type = rep("Prior",dim(data)[1])
      ################################################################################################################
      ######## STEP 2. Fit a GP to this initial data #################################################################
      ################################################################################################################
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
                         control=list(trace=FALSE)) # this just prints out the summary # removing control=list(trace=FALSE) gives details of optimizer bfgs or genoud
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
  
      #}
      ## USER APP INPUT
      new_efficiency=0.42 # provided by experiment via engineer via app!
      # This is the optimisation step. It can take from a few seconds (acq="Random") to a couple of minutes (acq="Grid")
      res1 <- optim_sithara("EQI" ,gp_model,prior_data,prior_response,lower_x,upper_x)
      res1 <- round(t(res1),1) # might need to be changed to 2 decimal places or a user input in a later version of the app
      #values$RES1 = cbind(res1,values$ACQ1)
      #values$RES1 = data.frame(values$RES1)
      #names(values$RES1) = c(names(prior_data), "Acquisition")
      res2 <- optim_sithara("AKG",gp_model,prior_data,prior_response,lower_x,upper_x)
      res2 <- round(t(res2),1) # might need to be changed to 2 decimal places or a user input in a later version of the app
      #values$RES2 = cbind(res2,values$ACQ2)
      #values$RES2 = data.frame(values$RES2)
      #names(values$RES2) = c(names(prior_data), "Acquisition")
      res3 <- optim_sithara("Grid",gp_model,prior_data,prior_response,lower_x,upper_x)
      res3 <- round(t(res3),1) # might need to be changed to 2 decimal places or a user input in a later version of the app
      #values$RES3 = cbind(res3,values$ACQ3)
      #values$RES3 = data.frame(values$RES3)
      #names(values$RES3) = c(names(prior_data), "Acquisition")
      res4 <- optim_sithara("Random",gp_model,prior_data,prior_response,lower_x,upper_x)
      res4 <- round(t(res4[1:3]),1) # might need to be changed to 2 decimal places or a user input in a later version of the app
      #values$RES4 = cbind(res4,values$ACQ4)
      #values$RES4 = data.frame(values$RES4)
      #names(values$RES4) = c(names(prior_data), "Acquisition")
      ################################################################################################################
      ########### STEP 4. # Add the new datapoint to the dataset.  ###################################################
      ################################################################################################################
      # Not sure about this rounding. For now maybe keep everything at 1 decimal place. It depends on the sensitivity of the experimental inputs!
      res = round(res,1) # Sithara requires rounding of 1 decimal place for her machine settings
      prior_data = rbind(prior_data,res)
      prior_response = c(prior_response,-new_efficiency)
      #exp_type = c(exp_type,acq)
      current_data = cbind(prior_data,prior_response)
      # This dataset can be used for the output plot.
      current_data = cbind(c(1:dim(current_data)[1]),prior_data,prior_response)
      ################################################################################################################
      ########### STEP 5. Update the plot  ###################################################
      ################################################################################################################
      names(current_data)=c("index",names(prior_data),"target_response")
      res_dframe=data.frame(current_data)
      max_response <- -min(prior_response)
      min_response <- -max(prior_response)
      p=ggplot(data = res_dframe)+
        geom_point( aes(x = index, y = -target_response),size=5) +
        xlab('Experiment number') +
        ylab('Target response') +
        expand_limits(y=c(min_response,max_response))
      p + theme_bw()+ #+  geom_text(aes(x = scan_speed_mmpers, y = efficiency_mgperl))+
        theme(text = element_text(size=20))
      # ggsave(paste("adaptiveresultsmg.pdf",sep=""))
      ##########################################################################################
      ######### OUTPUT TAB WITH 3 MORE PLOTS ###################################################
      ##########################################################################################
      # PLot for the first design variable d_1
      j=1
      min_input <- lower_x[j]
      max_input <- upper_x[j]
      plusminus <- max_input*.1
      gg_cols = c('#F8766D', '#7CAE00', '#00BFC4', '#C77CFF')
      the_next_suggested_settings = c(res1[j],res2[j],res3[j],res4[j])
      p=ggplot(data = res_dframe())+
        geom_point( aes(x = d_1, y = -target_response),size=7,colour="lightblue") +
        xlab('d_1') +
        ylab('Target response') +
        expand_limits(y=c(min_response,max_response))+
        expand_limits(x=c(min_input-plusminus,max_input+plusminus))+
        #expand_limits(x=c((min(res_dframe$d_1))*1.5,(max(res_dframe$d_1))*1.5))+
        geom_vline(aes(xintercept=the_next_suggested_settings[1]),colour = gg_cols[1]) +
        geom_vline(aes(xintercept=the_next_suggested_settings[2]),colour = gg_cols[2]) +
        geom_vline(aes(xintercept=the_next_suggested_settings[3]),colour = gg_cols[3]) +
        geom_vline(aes(xintercept=the_next_suggested_settings[4]),colour = gg_cols[4])  +
        annotate("text", the_next_suggested_settings, c(max_response+c(0.03,0.06,0.09,.12)), hjust = -.25,
                 label =     acquisitions, colour = gg_cols
        )
      p + theme_bw() +  geom_text(aes(x = d_1, y = -target_response,label = c(1:15)))+
        theme(text = element_text(size=20))
      
      
      
      
    })

    output$plott <- renderUI({
      
      req(input$proceed)
      req(input$efficiency)

      plotOutput("plot") %>% withSpinner(color="#1E90FF")

    })


    output$button_title  =  renderUI({
      
      #req(input$proceed)
      req(input$datafile)
      
      #req(input$efficiency)

      h4(div(HTML("Download Data")))


    })




    output$download_button  =  renderUI({
      
      #req(input$proceed)
      req(input$datafile)

      downloadButton("downloadData", "Download")

    })
     

     
    output$table <- renderTable({
      
      req(input$datafile)
      data()
      
      
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(

      filename = function(){"New_data.csv"},
      content = function(fname){
        write.csv(values$DF, fname)
      }
    )
    

  #Reset all inputs:
   observeEvent(input$datafile, { # 4 Reseting everything when a new dataset is observed
  # 

     df_text(NULL)
     values$DF = NULL
     values$RES = NULL
     df_text1(NULL)
     
     
   }) #End of observaion 4
  #, 
  
}

# Run the application
shinyApp(ui = ui, server = server)

