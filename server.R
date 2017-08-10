#    "Chromatography & TDM Server - Estimation Compilation Evaluation Reutlisation Furtherance Interpretation (CTS-ECERFI)"
#     The program written on R shiny language is intended for Therapeutic drug monitoring centers worldwide.

#     We intended to develop this program for the following purposes:

#     The ease of calculating the unknown concentration of drug in the sample evaluated based on the concentration and spectral value of
#     the standards (Obtained through HPLC,LC-MS/MS or any similar assay methods) instantaneously.
#     Reutlisation of the data for standard concentrations, for multiple samples.
#     Creation of database containing the details of the samples evaluated with their results.
#     Maintenance of easily accessible database within the laboratory along with functionality like downloading the database in csv format.
#     Evaluation of the database for finding out patients in the interested range of concentration in a instantaneous manner. 
#     In addition,the the database can be accessed for conducting prospective/retrospective studies
#     Interactive analysis of the database to visualise out of range results within seconds. For identifying the samples which fall out 
#     of range,the date of entry of the results into the database or sample number of the patient will be taken as identifiers by this program.
#     Interpretation of results hence obtained on basis of various parameters like the nature of the drug, patient criteria including 
#     age and any significant clinical conditions, drug interaction.
#     Calculation of adjusted concentrations of drug in special conditions as suggested by published literature. An e.g. is, Correction 
#     of phenytoin concentration according to blood albumin concentration of the patient.
 #    Copyright (C) 2017  Praveen Kumar M

 #   This program is free software: you can redistribute it and/or modify
 #  it under the terms of the GNU General Public License as published by
 #  the Free Software Foundation, either version 3 of the License, or
 #  (at your option) any later version.

 #   This program is distributed in the hope that it will be useful,
 #  but WITHOUT ANY WARRANTY; without even the implied warranty of
 #  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 #  GNU General Public License for more details.

 #  You should have received a copy of the GNU General Public License
 #  along with this program.  If not, see <http://www.gnu.org/licenses/>.




#This is for calling the required packages which are used in the app . In our case it is the shiny and ggplot2 packages
library(shiny)
library(ggplot2)
library(broom)
library(plotly)

c1 <- NULL
cdetemp <- NULL

data13s <- NULL
data23s <- NULL
fields <-
  c("Time&date", "Final", "Standards", "Name", "ID", "Drug", "Conc")
# fields <- c("timedate","Final3", "Standards3", "Name3", "ID3", "Drug3", "Conc3")
fields13s <- c("C13s", "C23s", "C33s")
fields23s <- c("V13s", "V23s", "V33s")
fields14s <- c("C14s", "C24s", "C34s", "C44s")
fields24s <- c("V14s", "V24s", "V34s", "V44s")
fields15s <- c("C15s", "C25s", "C35s", "C45s", "C55s")
fields25s <- c("V15s", "V25s", "V35s", "V45s", "V55s")
fields16s <- c("C16s", "C26s", "C36s", "C46s", "C56s", "C66s")
fields26s <- c("V16s", "V26s", "V36s", "V46s", "V56s", "V66s")
fields17s <-
  c("C17s", "C27s", "C37s", "C47s", "C57s", "C67s", "C77s")
fields27s <-
  c("V17s", "V27s", "V37s", "V47s", "V57s", "V67s", "V77s")
fields18s <-
  c("C18s", "C28s", "C38s", "C48s", "C58s", "C68s", "C78s", "C88s")
fields28s <-
  c("V18s", "V28s", "V38s", "V48s", "V58s", "V68s", "V78s", "V88s")
fields19s <-
  c("C19s",
    "C29s",
    "C39s",
    "C49s",
    "C59s",
    "C69s",
    "C79s",
    "C89s",
    "C99s")
fields29s <-
  c("V19s",
    "V29s",
    "V39s",
    "V49s",
    "V59s",
    "V69s",
    "V79s",
    "V89s",
    "V99s")
fields110s <-
  c(
    "C110s",
    "C210s",
    "C310s",
    "C410s",
    "C510S",
    "C610s",
    "C710s",
    "C810s",
    "C910s",
    "C1010s"
  )
fields210s <-
  c(
    "V110s",
    "V210s",
    "V310s",
    "V410s",
    "V510S",
    "V610s",
    "V710s",
    "V810s",
    "V910s",
    "V1010s"
  )

#This place is for the code which represent the functions and variables which wont be reactive to the input ( that is not dependent on "input$X")

# concentration<-NULL
# spectra<-NULL
# datarn<-data.frame(arn,brn)

downloadstandard<-read.csv("standard2017-08-062017-08-06 17-45-50.csv")
downloadtemplate<-read.csv("template file.csv")
downloadtemplate2<-read.csv("chromatography2017-08-062017-08-06 19-36-06.csv")
downloadtemplate3<-read.csv("chromatography2017-08-062017-08-06 19-36-06.csv")
downloadtemplate4<-read.csv("larger database.csv")

#This is the theme of ggplot2
theme_custom <- theme_set(theme_bw())

#This is the form for defining the user-input dependent functions for output variable
shinyServer(function(input, output, session) {
  #This brackets denotes the starting of "shinyServer"
  
  saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("response")) {
      response <<- rbind(response, data)
    } else {
      response <<- data
    }
  }
  
  loadData <- function() {
    if (exists("response")) {
      response
    }
  }
  
  
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x)
      input[[x]])
    # data <-cbind(Sys.Date(),Sys.time())
    data
  })
  
  
  # saveData3main <- function(data) {
  #   data <- as.data.frame(t(data))
  #   if (exists("response3")) {
  #     response3 <<- rbind(response3, data)
  #   } else {
  #     response3<<- data
  #   }
  # }
  #
  # loadData3main <- function() {
  #   if (exists("response3")) {
  #     response3
  #   }
  # }
  #
  # formData3main <- reactive({
  #   data3main <- sapply(fields3, function(x)input[[x]])
  #   data3main
  # })
  #
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
  })
  
  observeEvent(input$submit3, {
    saveData(formData3main())
  })
  
  observeEvent(input$submit4, {
    saveData(formData())
  })
  observeEvent(input$submit5, {
    saveData(formData())
  })
  observeEvent(input$submit6, {
    saveData(formData())
  })
  observeEvent(input$submit7, {
    saveData(formData())
  })
  observeEvent(input$submit8, {
    saveData(formData())
  })
  observeEvent(input$submit9, {
    saveData(formData())
  })
  observeEvent(input$submit10, {
    saveData(formData())
  })
  
  # output$response3main<-DT::renderDataTable({
  #   input$submit3
  #   loadData3main()
  # })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$response <- DT::renderDataTable({
    input$submit
    input$submit4
    input$submit5
    input$submit6
    input$submit7
    input$submit8
    input$submit9
    input$submit10
    loadData()
  })
  
  # datadownload <- data.frame(response)
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("chromatography", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(loadData(), file)
    }
  )
  
  
  #  column13s<-rbind("C13s", "C23s", "C33s")
  #  column23s<-rbind("V13s", "V23s","V33s")
  #  datastd3s<-data.frame(column13s, column23s)
  #
  #
  #
  # output$downloadData3 <- downloadHandler(
  #     filename = function() {
  #       paste("standard", Sys.Date(),Sys.time(),".csv", sep = "")
  #     },
  #
  #     content = function(file) {
  #       write.csv(datastd3s, file)
  #     }
  #   )
  #Generate a plot of the data and the plot is made using the inputs from user
  
  #################
  # saveData3s <- function(data) {
  #   data <- as.data.frame(data)
  # }
  #
  # observeEvent(input$submitn, {
  #   saveData3s(formData3s())
  # })
  #################
  
  formData3s <- reactive({
    data13s <- sapply(fields13s, function(x)
      input[[x]])
    data23s <- sapply(fields23s, function(x)
      input[[x]])
    dataa13s <- as.data.frame(data13s)
    dataa23s <- as.data.frame(data23s)
    datamain3s <- data.frame(dataa13s, data23s)
  })
  
  ##########################
  output$downloadDataexample <- downloadHandler(
    filename = function() {
      paste("standard", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(downloadstandard, file,row.names = FALSE)
    }
  )
  
  output$downloadDatatemplate <- downloadHandler(
    filename = function() {
      paste("standard", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(downloadtemplate, file,row.names = FALSE)
    }
  )
  
  output$downloadDatatemplate2 <- downloadHandler(
    filename = function() {
      paste("standard", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(downloadtemplate2, file,row.names = FALSE)
    }
  )
  
  output$downloadDatatemplate3 <- downloadHandler(
    filename = function() {
      paste("chromatography", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(downloadtemplate2, file,row.names = FALSE)
    }
  )
  
  output$downloadDatatemplate4 <- downloadHandler(
    filename = function() {
      paste("chromatography", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(downloadtemplate4, file,row.names = FALSE)
    }
  )
  
  ###########################
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste("standard", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(formData3s(), file)
    }
  )
  
  
  formData4s <- reactive({
    data14s <- sapply(fields14s, function(x)
      input[[x]])
    data24s <- sapply(fields24s, function(x)
      input[[x]])
    dataa14s <- as.data.frame(data14s)
    dataa24s <- as.data.frame(data24s)
    datamain4s <- data.frame(dataa14s, data24s)
  })
  
  
  output$downloadData4 <- downloadHandler(
    filename = function() {
      paste("standard", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(formData4s(), file)
    }
  )
  
  formData5s <- reactive({
    data15s <- sapply(fields15s, function(x)
      input[[x]])
    data25s <- sapply(fields25s, function(x)
      input[[x]])
    dataa15s <- as.data.frame(data15s)
    dataa25s <- as.data.frame(data25s)
    datamain5s <- data.frame(dataa15s, data25s)
  })
  
  
  output$downloadData5 <- downloadHandler(
    filename = function() {
      paste("standard", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(formData5s(), file)
    }
  )
  
  formData6s <- reactive({
    data16s <- sapply(fields16s, function(x)
      input[[x]])
    data26s <- sapply(fields26s, function(x)
      input[[x]])
    dataa16s <- as.data.frame(data16s)
    dataa26s <- as.data.frame(data26s)
    datamain6s <- data.frame(dataa16s, data26s)
  })
  
  
  output$downloadData6 <- downloadHandler(
    filename = function() {
      paste("standard", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(formData6s(), file)
    }
  )
  
  formData7s <- reactive({
    data17s <- sapply(fields17s, function(x)
      input[[x]])
    data27s <- sapply(fields27s, function(x)
      input[[x]])
    dataa17s <- as.data.frame(data17s)
    dataa27s <- as.data.frame(data27s)
    datamain7s <- data.frame(dataa17s, data27s)
  })
  
  
  output$downloadData7 <- downloadHandler(
    filename = function() {
      paste("standard", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(formData7s(), file)
    }
  )
  
  formData8s <- reactive({
    data18s <- sapply(fields18s, function(x)
      input[[x]])
    data28s <- sapply(fields28s, function(x)
      input[[x]])
    dataa18s <- as.data.frame(data18s)
    dataa28s <- as.data.frame(data28s)
    datamain8s <- data.frame(dataa18s, data28s)
  })
  
  
  output$downloadData8 <- downloadHandler(
    filename = function() {
      paste("standard", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(formData8s(), file)
    }
  )
  
  formData9s <- reactive({
    data19s <- sapply(fields19s, function(x)
      input[[x]])
    data29s <- sapply(fields29s, function(x)
      input[[x]])
    dataa19s <- as.data.frame(data19s)
    dataa29s <- as.data.frame(data29s)
    datamain9s <- data.frame(dataa19s, data29s)
  })
  
  
  output$downloadData9 <- downloadHandler(
    filename = function() {
      paste("standard", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(formData9s(), file)
    }
  )
  
  formData10s <- reactive({
    data110s <- sapply(fields110s, function(x)
      input[[x]])
    data210s <- sapply(fields210s, function(x)
      input[[x]])
    dataa110s <- as.data.frame(data110s)
    dataa210s <- as.data.frame(data210s)
    datamain10s <- data.frame(dataa110s, data210s)
  })
  
  
  output$downloadData10 <- downloadHandler(
    filename = function() {
      paste("standard", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(formData10s(), file)
    }
  )
  
  #This is for just displaying the output based on the print command data
  output$final3s <-
    renderPrint({
      #This is the starting of the renderprint identified as "rp1"
      
      #This is for collecting the inputs from the user widgets
      
      #C13s # This identifies the chromatographic technique with number of standard equal to '3' and 'C13s' representing first widget for entering concentration. So essentially 'g3' will be the identification
      
      C13s <-
        input$C13s #The variable C13s is definded to take the input value based on the widget with the code identification value of "C13s"
      C23s <- input$C23s
      C33s <- input$C33s
      V13s <- input$V13s
      V23s <- input$V23s
      V33s <- input$V33s
      U3s <-  input$U3s
      
      a3s <-
        c(C13s, C23s, C33s)# This is defining a new value a which is a concatanation of C13s to C6
      b3s <-
        c(V13s, V23s, V33s)# This is defining a new value a which is a concatanation of V13s to v6
      
      c3s <-
        lm(a3s ~ b3s)#This is the linear regression between a3s and b3s
      coeffs3s = coefficients(c3s)
      #We are determining the coefficient of c3s which is the linear regression equation
      O3s <-
        coeffs3s[1] + coeffs3s[2] * U3s #coeffs3s [1] is the intercept and coeffs3s[2] is the slope of the regression equation
      #r3s<- summary(c3s)$r3s.squared #Not required
      
      print(O3s)
      
      #Calculate function of Dose, V, KA and KE (where F = 1)
      #if (KA==KE) A <- 0 else A <- (D*KA)/(V*(KA-KE)) #This is how you define a if and if equation
      
      #Calculate concentration
      #CONC <- A*exp(-KE*TIME) - A*exp(-KA*TIME)
      
      #Create a dataframe of time and concentration data
      #conc.data3s <- data.frame(CONC = CONC , TIME =TIME)
      
      #ggplot2 object
      #	plotobj3s <- ggplot(data = conc.data3s)
      #	plotobj3s <- plotobj3s + geom_line(aes(x = TIME, y = CONC), colour = "red", size = 1)
      #	plotobj3s <- plotobj3s + scale_y_continuous("Concentration (mg/L) \n", lim = c(0,50))
      #	plotobj3s <- plotobj3s + scale_x_continuous("\nTime (hours)", lim = c(0,24))
      #	print(plotobj3s)
      
    })	#This the end of the renderprint identified as "rp1"
  
  output$final23s <-
    renderTable({
      #This is the starting of the renderprint identified as "rp2"
      
      C13s <-
        input$C13s #The variable C13s is definded to take the input value based on the widget with the code identification value of "C13s" and we are using the same variable which
      #we had identified earlier
      C23s <- input$C23s
      C33s <- input$C33s
      V13s <- input$V13s
      V23s <- input$V23s
      V33s <- input$V33s
      U3s <-  input$U3s
      
      a3s <- c(C13s, C23s, C33s)
      b3s <- c(V13s, V23s, V33s)
      c3s <- lm(a3s ~ b3s)
      #coeffs3s = coefficients(c3s)
      #a3s.lm is for representing that we are finding a3s through lm
      #O3s<- coeffs3s[1] + coeffs3s[2]*U3s
      #r3s <- summary(c3s)$r.squared
      #print(r3s)
      s3s <- glance(c3s)
      t3s <- t(as.matrix(s3s))
      t3s <- as.table.default(t3s)
      t3s
    })#This is the end of the renderprint "rp2"
  
  output$final33s <-
    renderPlot({
      #This is the starting of the renderplot identified as "rplot1"
      
      C13s <- input$C13s
      C23s <- input$C23s
      C33s <- input$C33s
      V13s <- input$V13s
      V23s <- input$V23s
      V33s <- input$V33s
      U3s <-  input$U3s
      
      a3s <- c(C13s, C23s, C33s)
      b3s <- c(V13s, V23s, V33s)
      c3s <- lm(a3s ~ b3s)
      coeffs3s = coefficients(c3s)
      
      #O3s<- coeffs3s[1] + coeffs3s[2]*U3s
      #r3s<- summary(a3s.lm)$r3s.squared
      #plotobj3s<- ggplot(plot(a3s,b3s))+  abline(lm(a3s~b3s))
      #print(plotobj3s)
      conc.data3s <-
        data.frame(a3s = a3s, b3s = b3s)#This is for forming the dataframe with a3s and b3s as the data
      
      #This is for the ggplot2 object output
      plotobj3s <- ggplot(data = conc.data3s)
      plotobj3s <-
        plotobj3s + geom_line(aes(x = a3s, y = b3s), colour = "red", size = 1)
      plotobj3s <-
        plotobj3s + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobj3s <-
        plotobj3s + scale_y_continuous("\n Spectral value ")
      print(plotobj3s) #This is for plotting the plotobj3s
      
    })#This is the end of the renderplot "rplot1"
  
  ##############################################################################################################
  
  ##From here starts the shinyserver for the standard 4
  #Generate a plot of the data and the plot is made using the inputs from user
  
  #This is for just displaying the output based on the print command data
  output$final4s <-
    renderPrint({
      #This is the starting of the renderprint identified as "rp1"
      
      #This is for collecting the inputs from the user widgets
      
      #C14s # This identifies the chromatographic technique with number of standard equal to '3' and 'C14s' representing first widget for entering concentration. So essentially 'g3' will be the identification
      
      C14s <-
        input$C14s #The variable C14s is definded to take the input value based on the widget with the code identification value of "C14s"
      C24s <- input$C24s
      C34s <- input$C34s
      C44s <- input$C44s
      V14s <- input$V14s
      V24s <- input$V24s
      V34s <- input$V34s
      V44s <- input$V44s
      U4s <-  input$U4s
      
      a4s <-
        c(C14s, C24s, C34s, C44s)# This is defining a new value a which is a concatanation of C14s to C6
      b4s <-
        c(V14s, V24s, V34s, V44s)# This is defining a new value a which is a concatanation of V14s to v6
      
      c4s <-
        lm(a4s ~ b4s)#This is the linear regression between a4s and b4s
      coeffs4s = coefficients(c4s)
      #We are determining the coefficient of c4s which is the linear regression equation
      O4s <-
        coeffs4s[1] + coeffs4s[2] * U4s #coeffs4s [1] is the intercept and coeffs4s[2] is the slope of the regression equation
      #r4s<- summary(c4s)$r4s.squared #Not required
      
      print(O4s)
      
      #Calculate function of Dose, V, KA and KE (where F = 1)
      #if (KA==KE) A <- 0 else A <- (D*KA)/(V*(KA-KE)) #This is how you define a if and if equation
      
      #Calculate concentration
      #CONC <- A*exp(-KE*TIME) - A*exp(-KA*TIME)
      
      #Create a dataframe of time and concentration data
      #conc.data4s <- data.frame(CONC = CONC , TIME =TIME)
      
      #ggplot2 object
      #	plotobj4s <- ggplot(data = conc.data4s)
      #	plotobj4s <- plotobj4s + geom_line(aes(x = TIME, y = CONC), colour = "red", size = 1)
      #	plotobj4s <- plotobj4s + scale_y_continuous("Concentration (mg/L) \n", lim = c(0,50))
      #	plotobj4s <- plotobj4s + scale_x_continuous("\nTime (hours)", lim = c(0,24))
      #	print(plotobj4s)
      
    })	#This the end of the renderprint identified as "rp1"
  
  output$final24s <-
    renderTable({
      #This is the starting of the renderprint identified as "rp2"
      
      C14s <-
        input$C14s #The variable C14s is definded to take the input value based on the widget with the code identification value of "C14s"
      C24s <- input$C24s
      C34s <- input$C34s
      C44s <- input$C44s
      V14s <- input$V14s
      V24s <- input$V24s
      V34s <- input$V34s
      V44s <- input$V44s
      
      a4s <-
        c(C14s, C24s, C34s, C44s)# This is defining a new value a which is a concatanation of C14s to C6
      b4s <-
        c(V14s, V24s, V34s, V44s)# This is defining a new value a which is a concatanation of V14s to v6
      
      c4s <- lm(a4s ~ b4s)
      coeffs4s = coefficients(c4s)
      #coeffs4s = coefficients(c4s)
      #a4s.lm is for representing that we are finding a4s through lm
      #O4s<- coeffs4s[1] + coeffs4s[2]*U4s
      #r4s <- summary(c4s)$r.squared
      #print(r4s)
      s4s <- glance(c4s)
      t4s <- t(as.matrix(s4s))
      t4s <- as.table.default(t4s)
      t4s
      
    })#This is the end of the renderprint "rp2"
  
  output$final34s <-
    renderPlot({
      #This is the starting of the renderplot identified as "rplot1"
      
      C14s <-
        input$C14s #The variable C14s is definded to take the input value based on the widget with the code identification value of "C14s"
      C24s <- input$C24s
      C34s <- input$C34s
      C44s <- input$C44s
      V14s <- input$V14s
      V24s <- input$V24s
      V34s <- input$V34s
      V44s <- input$V44s
      
      a4s <-
        c(C14s, C24s, C34s, C44s)# This is defining a new value a which is a concatanation of C14s to C6
      b4s <-
        c(V14s, V24s, V34s, V44s)# This is defining a new value a which is a concatanation of V14s to v6
      
      c4s <- lm(a4s ~ b4s)
      coeffs4s = coefficients(c4s)
      
      #O4s<- coeffs4s[1] + coeffs4s[2]*U4s
      #r4s<- summary(a4s.lm)$r4s.squared
      #plotobj4s<- ggplot(plot(a4s,b4s))+  abline(lm(a4s~b4s))
      #print(plotobj4s)
      conc.data4s <-
        data.frame(a4s = a4s, b4s = b4s)#This is for forming the dataframe with a4s and b4s as the data
      
      #This is for the ggplot2 object output
      plotobj4s <- ggplot(data = conc.data4s)
      plotobj4s <-
        plotobj4s + geom_line(aes(x = a4s, y = b4s), colour = "red", size = 1)
      plotobj4s <-
        plotobj4s + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobj4s <-
        plotobj4s + scale_y_continuous("\n Spectral value ")
      print(plotobj4s) #This is for plotting the plotobj4s
      
    })#This is the end of the renderplot "rplot1"
  
  ##############################################################################################################
  
  ##From here starts the shinyserver for the standard 5
  #Generate a plot of the data and the plot is made using the inputs from user
  
  #This is for just displaying the output based on the print command data
  output$final5s <-
    renderPrint({
      #This is the starting of the renderprint identified as "rp1"
      
      #This is for collecting the inputs from the user widgets
      
      #C15s # This identifies the chromatographic technique with number of standard equal to '3' and 'C15s' representing first widget for entering concentration. So essentially 'g3' will be the identification
      
      C15s <-
        input$C15s #The variable C15s is definded to take the input value based on the widget with the code identification value of "C15s"
      C25s <- input$C25s
      C35s <- input$C35s
      C45s <- input$C45s
      C55s <- input$C55s
      V15s <- input$V15s
      V25s <- input$V25s
      V35s <- input$V35s
      V45s <- input$V45s
      V55s <- input$V55s
      U5s <-  input$U5s
      
      a5s <-
        c(C15s, C25s, C35s, C45s, C55s)# This is defining a new value a which is a concatanation of C15s to C6
      b5s <-
        c(V15s, V25s, V35s, V45s, V55s)# This is defining a new value a which is a concatanation of V15s to v6
      
      c5s <-
        lm(a5s ~ b5s)#This is the linear regression between a5s and b5s
      coeffs5s = coefficients(c5s)
      #We are determining the coefficient of c5s which is the linear regression equation
      O5s <-
        coeffs5s[1] + coeffs5s[2] * U5s #coeffs5s [1] is the intercept and coeffs5s[2] is the slope of the regression equation
      #r5s<- summary(c5s)$r5s.squared #Not required
      
      print(O5s)
      
      #Calculate function of Dose, V, KA and KE (where F = 1)
      #if (KA==KE) A <- 0 else A <- (D*KA)/(V*(KA-KE)) #This is how you define a if and if equation
      
      #Calculate concentration
      #CONC <- A*exp(-KE*TIME) - A*exp(-KA*TIME)
      
      #Create a dataframe of time and concentration data
      #conc.data5s <- data.frame(CONC = CONC , TIME =TIME)
      
      #ggplot2 object
      #	plotobj5s <- ggplot(data = conc.data5s)
      #	plotobj5s <- plotobj5s + geom_line(aes(x = TIME, y = CONC), colour = "red", size = 1)
      #	plotobj5s <- plotobj5s + scale_y_continuous("Concentration (mg/L) \n", lim = c(0,50))
      #	plotobj5s <- plotobj5s + scale_x_continuous("\nTime (hours)", lim = c(0,24))
      #	print(plotobj5s)
      
    })	#This the end of the renderprint identified as "rp1"
  
  output$final25s <-
    renderTable({
      #This is the starting of the renderprint identified as "rp2"
      
      C15s <-
        input$C15s #The variable C15s is definded to take the input value based on the widget with the code identification value of "C15s"
      C25s <- input$C25s
      C35s <- input$C35s
      C45s <- input$C45s
      C55s <- input$C55s
      V15s <- input$V15s
      V25s <- input$V25s
      V35s <- input$V35s
      V45s <- input$V45s
      V55s <- input$V55s
      U5s <-  input$U5s
      
      a5s <-
        c(C15s, C25s, C35s, C45s, C55s)# This is defining a new value a which is a concatanation of C15s to C6
      b5s <-
        c(V15s, V25s, V35s, V45s, V55s)# This is defining a new value a which is a concatanation of V15s to v6
      
      
      c5s <- lm(a5s ~ b5s)
      coeffs5s = coefficients(c5s)
      #coeffs5s = coefficients(c5s)
      #a5s.lm is for representing that we are finding a5s through lm
      #O5s<- coeffs5s[1] + coeffs5s[2]*U5s
      #r5s <- summary(c5s)$r.squared
      #print(r5s)
      s5s <- glance(c5s)
      t5s <- t(as.matrix(s5s))
      t5s <- as.table.default(t5s)
      t5s
      
    })#This is the end of the renderprint "rp2"
  
  output$final35s <-
    renderPlot({
      #This is the starting of the renderplot identified as "rplot1"
      
      C15s <-
        input$C15s #The variable C15s is definded to take the input value based on the widget with the code identification value of "C15s"
      C25s <- input$C25s
      C35s <- input$C35s
      C45s <- input$C45s
      C55s <- input$C55s
      V15s <- input$V15s
      V25s <- input$V25s
      V35s <- input$V35s
      V45s <- input$V45s
      V55s <- input$V55s
      U5s <-  input$U5s
      
      a5s <-
        c(C15s, C25s, C35s, C45s, C55s)# This is defining a new value a which is a concatanation of C15s to C6
      b5s <-
        c(V15s, V25s, V35s, V45s, V55s)# This is defining a new value a which is a concatanation of V15s to v6
      
      c5s <- lm(a5s ~ b5s)
      coeffs5s = coefficients(c5s)
      
      #O5s<- coeffs5s[1] + coeffs5s[2]*U5s
      #r5s<- summary(a5s.lm)$r5s.squared
      #plotobj5s<- ggplot(plot(a5s,b5s))+  abline(lm(a5s~b5s))
      #print(plotobj5s)
      conc.data5s <-
        data.frame(a5s = a5s, b5s = b5s)#This is for forming the dataframe with a5s and b5s as the data
      
      #This is for the ggplot2 object output
      plotobj5s <- ggplot(data = conc.data5s)
      plotobj5s <-
        plotobj5s + geom_line(aes(x = a5s, y = b5s), colour = "red", size = 1)
      plotobj5s <-
        plotobj5s + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobj5s <-
        plotobj5s + scale_y_continuous("\n Spectral value ")
      print(plotobj5s) #This is for plotting the plotobj5s
      
    })#This is the end of the renderplot "rplot1"
  
  ##############################################################################################################
  
  ##From here starts the shinyserver for the standard 6
  #Generate a plot of the data and the plot is made using the inputs from user
  
  #This is for just displaying the output based on the print command data
  output$final6s <-
    renderPrint({
      #This is the starting of the renderprint identified as "rp1"
      
      #This is for collecting the inputs from the user widgets
      
      #C16s # This identifies the chromatographic technique with number of standard equal to '3' and 'C16s' representing first widget for entering concentration. So essentially 'g3' will be the identification
      
      C16s <-
        input$C16s #The variable C16s is definded to take the input value based on the widget with the code identification value of "C16s"
      C26s <- input$C26s
      C36s <- input$C36s
      C46s <- input$C46s
      C56s <- input$C56s
      C66s <- input$C66s
      V16s <- input$V16s
      V26s <- input$V26s
      V36s <- input$V36s
      V46s <- input$V46s
      V56s <- input$V56s
      V66s <- input$V66s
      U6s <-  input$U6s
      
      a6s <-
        c(C16s, C26s, C36s, C46s, C56s, C66s)# This is defining a new value a which is a concatanation of C16s to C6
      b6s <-
        c(V16s, V26s, V36s, V46s, V56s, V66s)# This is defining a new value a which is a concatanation of V16s to v6
      
      c6s <-
        lm(a6s ~ b6s)#This is the linear regression between a6s and b6s
      coeffs6s = coefficients(c6s)
      #We are determining the coefficient of c6s which is the linear regression equation
      O6s <-
        coeffs6s[1] + coeffs6s[2] * U6s #coeffs6s [1] is the intercept and coeffs6s[2] is the slope of the regression equation
      #r6s<- summary(c6s)$r6s.squared #Not required
      
      print(O6s)
      
      #Calculate function of Dose, V, KA and KE (where F = 1)
      #if (KA==KE) A <- 0 else A <- (D*KA)/(V*(KA-KE)) #This is how you define a if and if equation
      
      #Calculate concentration
      #CONC <- A*exp(-KE*TIME) - A*exp(-KA*TIME)
      
      #Create a dataframe of time and concentration data
      #conc.data6s <- data.frame(CONC = CONC , TIME =TIME)
      
      #ggplot2 object
      #	plotobj6s <- ggplot(data = conc.data6s)
      #	plotobj6s <- plotobj6s + geom_line(aes(x = TIME, y = CONC), colour = "red", size = 1)
      #	plotobj6s <- plotobj6s + scale_y_continuous("Concentration (mg/L) \n", lim = c(0,50))
      #	plotobj6s <- plotobj6s + scale_x_continuous("\nTime (hours)", lim = c(0,24))
      #	print(plotobj6s)
      
    })	#This the end of the renderprint identified as "rp1"
  
  output$final26s <-
    renderTable({
      #This is the starting of the renderprint identified as "rp2"
      
      C16s <-
        input$C16s #The variable C16s is definded to take the input value based on the widget with the code identification value of "C16s"
      C26s <- input$C26s
      C36s <- input$C36s
      C46s <- input$C46s
      C56s <- input$C56s
      C66s <- input$C66s
      V16s <- input$V16s
      V26s <- input$V26s
      V36s <- input$V36s
      V46s <- input$V46s
      V56s <- input$V56s
      V66s <- input$V66s
      U6s <-  input$U6s
      
      a6s <-
        c(C16s, C26s, C36s, C46s, C56s, C66s)# This is defining a new value a which is a concatanation of C16s to C6
      b6s <-
        c(V16s, V26s, V36s, V46s, V56s, V66s)# This is defining a new value a which is a concatanation of V16s to v6
      
      
      c6s <- lm(a6s ~ b6s)
      coeffs6s = coefficients(c6s)
      #coeffs6s = coefficients(c6s)
      #a6s.lm is for representing that we are finding a6s through lm
      #O6s<- coeffs6s[1] + coeffs6s[2]*U6s
      #r6s <- summary(c6s)$r.squared
      #print(r6s)
      s6s <- glance(c6s)
      t6s <- t(as.matrix(s6s))
      t6s <- as.table.default(t6s)
      t6s
      
    })#This is the end of the renderprint "rp2"
  
  output$final36s <-
    renderPlot({
      #This is the starting of the renderplot identified as "rplot1"
      
      C16s <-
        input$C16s #The variable C16s is definded to take the input value based on the widget with the code identification value of "C16s"
      C26s <- input$C26s
      C36s <- input$C36s
      C46s <- input$C46s
      C56s <- input$C56s
      C66s <- input$C66s
      V16s <- input$V16s
      V26s <- input$V26s
      V36s <- input$V36s
      V46s <- input$V46s
      V56s <- input$V56s
      V66s <- input$V66s
      U6s <-  input$U6s
      
      a6s <-
        c(C16s, C26s, C36s, C46s, C56s, C66s)# This is defining a new value a which is a concatanation of C16s to C6
      b6s <-
        c(V16s, V26s, V36s, V46s, V56s, V66s)# This is defining a new value a which is a concatanation of V16s to v6
      
      
      c6s <- lm(a6s ~ b6s)
      coeffs6s = coefficients(c6s)
      
      #O6s<- coeffs6s[1] + coeffs6s[2]*U6s
      #r6s<- summary(a6s.lm)$r6s.squared
      #plotobj6s<- ggplot(plot(a6s,b6s))+  abline(lm(a6s~b6s))
      #print(plotobj6s)
      conc.data6s <-
        data.frame(a6s = a6s, b6s = b6s)#This is for forming the dataframe with a6s and b6s as the data
      
      #This is for the ggplot2 object output
      plotobj6s <- ggplot(data = conc.data6s)
      plotobj6s <-
        plotobj6s + geom_line(aes(x = a6s, y = b6s), colour = "red", size = 1)
      plotobj6s <-
        plotobj6s + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobj6s <-
        plotobj6s + scale_y_continuous("\n Spectral value ")
      print(plotobj6s) #This is for plotting the plotobj6s
      
    })#This is the end of the renderplot "rplot1"
  
  ##############################################################################################################
  
  ##From here starts the shinyserver for the standard 7
  #Generate a plot of the data and the plot is made using the inputs from user
  
  #This is for just displaying the output based on the print command data
  output$final7s <-
    renderPrint({
      #This is the starting of the renderprint identified as "rp1"
      
      #This is for collecting the inputs from the user widgets
      
      #C17s # This identifies the chromatographic technique with number of standard equal to '3' and 'C17s' representing first widget for entering concentration. So essentially 'g3' will be the identification
      
      C17s <-
        input$C17s #The variable C17s is definded to take the input value based on the widget with the code identification value of "C17s"
      C27s <- input$C27s
      C37s <- input$C37s
      C47s <- input$C47s
      C57s <- input$C57s
      C67s <- input$C67s
      C77s <- input$C77s
      V17s <- input$V17s
      V27s <- input$V27s
      V37s <- input$V37s
      V47s <- input$V47s
      V57s <- input$V57s
      V67s <- input$V67s
      V77s <- input$V77s
      U7s <-  input$U7s
      
      a7s <-
        c(C17s, C27s, C37s, C47s, C57s, C67s, C77s)# This is defining a new value a which is a concatanation of C17s to C6
      b7s <-
        c(V17s, V27s, V37s, V47s, V57s, V67s, V77s)# This is defining a new value a which is a concatanation of V17s to v6
      
      c7s <-
        lm(a7s ~ b7s)#This is the linear regression between a7s and b7s
      coeffs7s = coefficients(c7s)
      #We are determining the coefficient of c7s which is the linear regression equation
      O7s <-
        coeffs7s[1] + coeffs7s[2] * U7s #coeffs7s [1] is the intercept and coeffs7s[2] is the slope of the regression equation
      #r7s<- summary(c7s)$r7s.squared #Not required
      
      print(O7s)
      
      #Calculate function of Dose, V, KA and KE (where F = 1)
      #if (KA==KE) A <- 0 else A <- (D*KA)/(V*(KA-KE)) #This is how you define a if and if equation
      
      #Calculate concentration
      #CONC <- A*exp(-KE*TIME) - A*exp(-KA*TIME)
      
      #Create a dataframe of time and concentration data
      #conc.data7s <- data.frame(CONC = CONC , TIME =TIME)
      
      #ggplot2 object
      #	plotobj7s <- ggplot(data = conc.data7s)
      #	plotobj7s <- plotobj7s + geom_line(aes(x = TIME, y = CONC), colour = "red", size = 1)
      #	plotobj7s <- plotobj7s + scale_y_continuous("Concentration (mg/L) \n", lim = c(0,50))
      #	plotobj7s <- plotobj7s + scale_x_continuous("\nTime (hours)", lim = c(0,24))
      #	print(plotobj7s)
      
    })	#This the end of the renderprint identified as "rp1"
  
  output$final27s <-
    renderTable({
      #This is the starting of the renderprint identified as "rp2"
      
      C17s <-
        input$C17s #The variable C17s is definded to take the input value based on the widget with the code identification value of "C17s"
      C27s <- input$C27s
      C37s <- input$C37s
      C47s <- input$C47s
      C57s <- input$C57s
      C67s <- input$C67s
      C77s <- input$C77s
      V17s <- input$V17s
      V27s <- input$V27s
      V37s <- input$V37s
      V47s <- input$V47s
      V57s <- input$V57s
      V67s <- input$V67s
      V77s <- input$V77s
      U7s <-  input$U7s
      
      a7s <-
        c(C17s, C27s, C37s, C47s, C57s, C67s, C77s)# This is defining a new value a which is a concatanation of C17s to C6
      b7s <-
        c(V17s, V27s, V37s, V47s, V57s, V67s, V77s)# This is defining a new value a which is a concatanation of V17s to v6
      
      
      c7s <- lm(a7s ~ b7s)
      coeffs7s = coefficients(c7s)
      #coeffs7s = coefficients(c7s)
      #a7s.lm is for representing that we are finding a7s through lm
      #O7s<- coeffs7s[1] + coeffs7s[2]*U7s
      #r7s <- summary(c7s)$r.squared
      #print(r7s)
      s7s <- glance(c7s)
      t7s <- t(as.matrix(s7s))
      t7s <- as.table.default(t7s)
      t7s
    })#This is the end of the renderprint "rp2"
  
  output$final37s <-
    renderPlot({
      #This is the starting of the renderplot identified as "rplot1"
      
      C17s <-
        input$C17s #The variable C17s is definded to take the input value based on the widget with the code identification value of "C17s"
      C27s <- input$C27s
      C37s <- input$C37s
      C47s <- input$C47s
      C57s <- input$C57s
      C67s <- input$C67s
      C77s <- input$C77s
      V17s <- input$V17s
      V27s <- input$V27s
      V37s <- input$V37s
      V47s <- input$V47s
      V57s <- input$V57s
      V67s <- input$V67s
      V77s <- input$V77s
      U7s <-  input$U7s
      
      a7s <-
        c(C17s, C27s, C37s, C47s, C57s, C67s, C77s)# This is defining a new value a which is a concatanation of C17s to C6
      b7s <-
        c(V17s, V27s, V37s, V47s, V57s, V67s, V77s)# This is defining a new value a which is a concatanation of V17s to v6
      
      
      c7s <- lm(a7s ~ b7s)
      coeffs7s = coefficients(c7s)
      
      #O7s<- coeffs7s[1] + coeffs7s[2]*U7s
      #r7s<- summary(a7s.lm)$r7s.squared
      #plotobj7s<- ggplot(plot(a7s,b7s))+  abline(lm(a7s~b7s))
      #print(plotobj7s)
      conc.data7s <-
        data.frame(a7s = a7s, b7s = b7s)#This is for forming the dataframe with a7s and b7s as the data
      
      #This is for the ggplot2 object output
      plotobj7s <- ggplot(data = conc.data7s)
      plotobj7s <-
        plotobj7s + geom_line(aes(x = a7s, y = b7s), colour = "red", size = 1)
      plotobj7s <-
        plotobj7s + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobj7s <-
        plotobj7s + scale_y_continuous("\n Spectral value ")
      print(plotobj7s) #This is for plotting the plotobj7s
      
    })#This is the end of the renderplot "rplot1"
  
  ##############################################################################################################
  
  ##From here starts the shinyserver for the standard 8
  #Generate a plot of the data and the plot is made using the inputs from user
  
  #This is for just displaying the output based on the print command data
  output$final8s <-
    renderPrint({
      #This is the starting of the renderprint identified as "rp1"
      
      #This is for collecting the inputs from the user widgets
      
      #C18s # This identifies the chromatographic technique with number of standard equal to '3' and 'C18s' representing first widget for entering concentration. So essentially 'g3' will be the identification
      
      C18s <-
        input$C18s #The variable C18s is definded to take the input value based on the widget with the code identification value of "C18s"
      C28s <- input$C28s
      C38s <- input$C38s
      C48s <- input$C48s
      C58s <- input$C58s
      C68s <- input$C68s
      C78s <- input$C78s
      C88s <- input$C88s
      V18s <- input$V18s
      V28s <- input$V28s
      V38s <- input$V38s
      V48s <- input$V48s
      V58s <- input$V58s
      V68s <- input$V68s
      V78s <- input$V78s
      V88s <- input$V88s
      U8s <-  input$U8s
      
      a8s <-
        c(C18s, C28s, C38s, C48s, C58s, C68s, C78s, C88s)# This is defining a new value a which is a concatanation of C18s to C6
      b8s <-
        c(V18s, V28s, V38s, V48s, V58s, V68s, V78s, V88s)# This is defining a new value a which is a concatanation of V18s to v6
      
      c8s <-
        lm(a8s ~ b8s)#This is the linear regression between a8s and b8s
      coeffs8s = coefficients(c8s)
      #We are determining the coefficient of c8s which is the linear regression equation
      O8s <-
        coeffs8s[1] + coeffs8s[2] * U8s #coeffs8s [1] is the intercept and coeffs8s[2] is the slope of the regression equation
      #r8s<- summary(c8s)$r8s.squared #Not required
      
      print(O8s)
      
      #Calculate function of Dose, V, KA and KE (where F = 1)
      #if (KA==KE) A <- 0 else A <- (D*KA)/(V*(KA-KE)) #This is how you define a if and if equation
      
      #Calculate concentration
      #CONC <- A*exp(-KE*TIME) - A*exp(-KA*TIME)
      
      #Create a dataframe of time and concentration data
      #conc.data8s <- data.frame(CONC = CONC , TIME =TIME)
      
      #ggplot2 object
      #	plotobj8s <- ggplot(data = conc.data8s)
      #	plotobj8s <- plotobj8s + geom_line(aes(x = TIME, y = CONC), colour = "red", size = 1)
      #	plotobj8s <- plotobj8s + scale_y_continuous("Concentration (mg/L) \n", lim = c(0,50))
      #	plotobj8s <- plotobj8s + scale_x_continuous("\nTime (hours)", lim = c(0,24))
      #	print(plotobj8s)
      
    })	#This the end of the renderprint identified as "rp1"
  
  output$final28s <-
    renderTable({
      #This is the starting of the renderprint identified as "rp2"
      
      C18s <-
        input$C18s #The variable C18s is definded to take the input value based on the widget with the code identification value of "C18s"
      C28s <- input$C28s
      C38s <- input$C38s
      C48s <- input$C48s
      C58s <- input$C58s
      C68s <- input$C68s
      C78s <- input$C78s
      C88s <- input$C88s
      V18s <- input$V18s
      V28s <- input$V28s
      V38s <- input$V38s
      V48s <- input$V48s
      V58s <- input$V58s
      V68s <- input$V68s
      V78s <- input$V78s
      V88s <- input$V88s
      U8s <-  input$U8s
      
      a8s <-
        c(C18s, C28s, C38s, C48s, C58s, C68s, C78s, C88s)# This is defining a new value a which is a concatanation of C18s to C6
      b8s <-
        c(V18s, V28s, V38s, V48s, V58s, V68s, V78s, V88s)# This is defining a new value a which is a concatanation of V18s to v6
      
      
      c8s <- lm(a8s ~ b8s)
      coeffs8s = coefficients(c8s)
      #coeffs8s = coefficients(c8s)
      #a8s.lm is for representing that we are finding a8s through lm
      #O8s<- coeffs8s[1] + coeffs8s[2]*U8s
      #r8s <- summary(c8s)$r.squared
      #print(r8s)
      s8s <- glance(c8s)
      t8s <- t(as.matrix(s8s))
      t8s <- as.table.default(t8s)
      t8s
      
    })#This is the end of the renderprint "rp2"
  
  output$final38s <-
    renderPlot({
      #This is the starting of the renderplot identified as "rplot1"
      
      C18s <-
        input$C18s #The variable C18s is definded to take the input value based on the widget with the code identification value of "C18s"
      C28s <- input$C28s
      C38s <- input$C38s
      C48s <- input$C48s
      C58s <- input$C58s
      C68s <- input$C68s
      C78s <- input$C78s
      C88s <- input$C88s
      V18s <- input$V18s
      V28s <- input$V28s
      V38s <- input$V38s
      V48s <- input$V48s
      V58s <- input$V58s
      V68s <- input$V68s
      V78s <- input$V78s
      V88s <- input$V88s
      U8s <-  input$U8s
      
      a8s <-
        c(C18s, C28s, C38s, C48s, C58s, C68s, C78s, C88s)# This is defining a new value a which is a concatanation of C18s to C6
      b8s <-
        c(V18s, V28s, V38s, V48s, V58s, V68s, V78s, V88s)# This is defining a new value a which is a concatanation of V18s to v6
      
      
      c8s <- lm(a8s ~ b8s)
      coeffs8s = coefficients(c8s)
      
      #O8s<- coeffs8s[1] + coeffs8s[2]*U8s
      #r8s<- summary(a8s.lm)$r8s.squared
      #plotobj8s<- ggplot(plot(a8s,b8s))+  abline(lm(a8s~b8s))
      #print(plotobj8s)
      conc.data8s <-
        data.frame(a8s = a8s, b8s = b8s)#This is for forming the dataframe with a8s and b8s as the data
      
      #This is for the ggplot2 object output
      plotobj8s <- ggplot(data = conc.data8s)
      plotobj8s <-
        plotobj8s + geom_line(aes(x = a8s, y = b8s), colour = "red", size = 1)
      plotobj8s <-
        plotobj8s + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobj8s <-
        plotobj8s + scale_y_continuous("\n Spectral value ")
      print(plotobj8s) #This is for plotting the plotobj8s
      
    })#This is the end of the renderplot "rplot1"
  
  ##############################################################################################################
  
  ##From here starts the shinyserver for the standard 9
  #Generate a plot of the data and the plot is made using the inputs from user
  
  #This is for just displaying the output based on the print command data
  output$final9s <-
    renderPrint({
      #This is the starting of the renderprint identified as "rp1"
      
      #This is for collecting the inputs from the user widgets
      
      #C19s # This identifies the chromatographic technique with number of standard equal to '3' and 'C19s' representing first widget for entering concentration. So essentially 'g3' will be the identification
      
      C19s <-
        input$C19s #The variable C19s is definded to take the input value based on the widget with the code identification value of "C19s"
      C29s <- input$C29s
      C39s <- input$C39s
      C49s <- input$C49s
      C59s <- input$C59s
      C69s <- input$C69s
      C79s <- input$C79s
      C89s <- input$C89s
      C99s <- input$C99s
      V19s <- input$V19s
      V29s <- input$V29s
      V39s <- input$V39s
      V49s <- input$V49s
      V59s <- input$V59s
      V69s <- input$V69s
      V79s <- input$V79s
      V89s <- input$V89s
      V99s <- input$V99s
      U9s <-  input$U9s
      
      a9s <-
        c(C19s, C29s, C39s, C49s, C59s, C69s, C79s, C89s, C99s)# This is defining a new value a which is a concatanation of C19s to C6
      b9s <-
        c(V19s, V29s, V39s, V49s, V59s, V69s, V79s, V89s, V99s)# This is defining a new value a which is a concatanation of V19s to v6
      
      c9s <-
        lm(a9s ~ b9s)#This is the linear regression between a9s and b9s
      coeffs9s = coefficients(c9s)
      #We are determining the coefficient of c9s which is the linear regression equation
      O9s <-
        coeffs9s[1] + coeffs9s[2] * U9s #coeffs9s [1] is the intercept and coeffs9s[2] is the slope of the regression equation
      #r9s<- summary(c9s)$r9s.squared #Not required
      
      print(O9s)
      
      #Calculate function of Dose, V, KA and KE (where F = 1)
      #if (KA==KE) A <- 0 else A <- (D*KA)/(V*(KA-KE)) #This is how you define a if and if equation
      
      #Calculate concentration
      #CONC <- A*exp(-KE*TIME) - A*exp(-KA*TIME)
      
      #Create a dataframe of time and concentration data
      #conc.data9s <- data.frame(CONC = CONC , TIME =TIME)
      
      #ggplot2 object
      #	plotobj9s <- ggplot(data = conc.data9s)
      #	plotobj9s <- plotobj9s + geom_line(aes(x = TIME, y = CONC), colour = "red", size = 1)
      #	plotobj9s <- plotobj9s + scale_y_continuous("Concentration (mg/L) \n", lim = c(0,50))
      #	plotobj9s <- plotobj9s + scale_x_continuous("\nTime (hours)", lim = c(0,24))
      #	print(plotobj9s)
      
    })	#This the end of the renderprint identified as "rp1"
  
  output$final29s <-
    renderTable({
      #This is the starting of the renderprint identified as "rp2"
      
      C19s <-
        input$C19s #The variable C19s is definded to take the input value based on the widget with the code identification value of "C19s"
      C29s <- input$C29s
      C39s <- input$C39s
      C49s <- input$C49s
      C59s <- input$C59s
      C69s <- input$C69s
      C79s <- input$C79s
      C89s <- input$C89s
      C99s <- input$C99s
      V19s <- input$V19s
      V29s <- input$V29s
      V39s <- input$V39s
      V49s <- input$V49s
      V59s <- input$V59s
      V69s <- input$V69s
      V79s <- input$V79s
      V89s <- input$V89s
      V99s <- input$V99s
      U9s <-  input$U9s
      
      a9s <-
        c(C19s, C29s, C39s, C49s, C59s, C69s, C79s, C89s, C99s)# This is defining a new value a which is a concatanation of C19s to C6
      b9s <-
        c(V19s, V29s, V39s, V49s, V59s, V69s, V79s, V89s, V99s)# This is defining a new value a which is a concatanation of V19s to v6
      
      
      c9s <- lm(a9s ~ b9s)
      coeffs9s = coefficients(c9s)
      #coeffs9s = coefficients(c9s)
      #a9s.lm is for representing that we are finding a9s through lm
      #O9s<- coeffs9s[1] + coeffs9s[2]*U9s
      #r9s <- summary(c9s)$r.squared
      #print(r9s)
      s9s <- glance(c9s)
      t9s <- t(as.matrix(s9s))
      t9s <- as.table.default(t9s)
      t9s
      
    })#This is the end of the renderprint "rp2"
  
  output$final39s <-
    renderPlot({
      #This is the starting of the renderplot identified as "rplot1"
      
      C19s <-
        input$C19s #The variable C19s is definded to take the input value based on the widget with the code identification value of "C19s"
      C29s <- input$C29s
      C39s <- input$C39s
      C49s <- input$C49s
      C59s <- input$C59s
      C69s <- input$C69s
      C79s <- input$C79s
      C89s <- input$C89s
      C99s <- input$C99s
      V19s <- input$V19s
      V29s <- input$V29s
      V39s <- input$V39s
      V49s <- input$V49s
      V59s <- input$V59s
      V69s <- input$V69s
      V79s <- input$V79s
      V89s <- input$V89s
      V99s <- input$V99s
      U9s <-  input$U9s
      
      a9s <-
        c(C19s, C29s, C39s, C49s, C59s, C69s, C79s, C89s, C99s)# This is defining a new value a which is a concatanation of C19s to C6
      b9s <-
        c(V19s, V29s, V39s, V49s, V59s, V69s, V79s, V89s, V99s)# This is defining a new value a which is a concatanation of V19s to v6
      
      c9s <- lm(a9s ~ b9s)
      coeffs9s = coefficients(c9s)
      
      #O9s<- coeffs9s[1] + coeffs9s[2]*U9s
      #r9s<- summary(a9s.lm)$r9s.squared
      #plotobj9s<- ggplot(plot(a9s,b9s))+  abline(lm(a9s~b9s))
      #print(plotobj9s)
      conc.data9s <-
        data.frame(a9s = a9s, b9s = b9s)#This is for forming the dataframe with a9s and b9s as the data
      
      #This is for the ggplot2 object output
      plotobj9s <- ggplot(data = conc.data9s)
      plotobj9s <-
        plotobj9s + geom_line(aes(x = a9s, y = b9s), colour = "red", size = 1)
      plotobj9s <-
        plotobj9s + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobj9s <-
        plotobj9s + scale_y_continuous("\n Spectral value ")
      print(plotobj9s) #This is for plotting the plotobj9s
      
    })#This is the end of the renderplot "rplot1"
  
  ##############################################################################################################
  
  ##From here starts the shinyserver for the standard 10
  #Generate a plot of the data and the plot is made using the inputs from user
  
  #This is for just displaying the output based on the print command data
  output$final10s <-
    renderPrint({
      #This is the starting of the renderprint identified as "rp1"
      
      #This is for collecting the inputs from the user widgets
      
      #C110s # This identifies the chromatographic technique with number of standard equal to '3' and 'C110s' representing first widget for entering concentration. So essentially 'g3' will be the identification
      
      C110s <-
        input$C110s #The variable C110s is definded to take the input value based on the widget with the code identification value of "C110s"
      C210s <- input$C210s
      C310s <- input$C310s
      C410s <- input$C410s
      C510s <- input$C510s
      C610s <- input$C610s
      C710s <- input$C710s
      C810s <- input$C810s
      C910s <- input$C910s
      C1010s <- input$C1010s
      V110s <- input$V110s
      V210s <- input$V210s
      V310s <- input$V310s
      V410s <- input$V410s
      V510s <- input$V510s
      V610s <- input$V610s
      V710s <- input$V710s
      V810s <- input$V810s
      V910s <- input$V910s
      V1010s <- input$V1010s
      U10s <-  input$U10s
      
      a10s <-
        c(C110s,
          C210s,
          C310s,
          C410s,
          C510s,
          C610s,
          C710s,
          C810s,
          C910s,
          C1010s)# This is defining a new value a which is a concatanation of C110s to C6
      b10s <-
        c(V110s,
          V210s,
          V310s,
          V410s,
          V510s,
          V610s,
          V710s,
          V810s,
          V910s,
          V1010s)# This is defining a new value a which is a concatanation of V110s to v6
      
      c10s <-
        lm(a10s ~ b10s)#This is the linear regression between a10s and b10s
      coeffs10s = coefficients(c10s)
      #We are determining the coefficient of c10s which is the linear regression equation
      O10s <-
        coeffs10s[1] + coeffs10s[2] * U10s #coeffs10s [1] is the intercept and coeffs10s[2] is the slope of the regression equation
      #r10s<- summary(c10s)$r10s.squared #Not required
      
      print(O10s)
      
      #Calculate function of Dose, V, KA and KE (where F = 1)
      #if (KA==KE) A <- 0 else A <- (D*KA)/(V*(KA-KE)) #This is how you define a if and if equation
      
      #Calculate concentration
      #CONC <- A*exp(-KE*TIME) - A*exp(-KA*TIME)
      
      #Create a dataframe of time and concentration data
      #conc.data10s <- data.frame(CONC = CONC , TIME =TIME)
      
      #ggplot2 object
      #	plotobj10s <- ggplot(data = conc.data10s)
      #	plotobj10s <- plotobj10s + geom_line(aes(x = TIME, y = CONC), colour = "red", size = 1)
      #	plotobj10s <- plotobj10s + scale_y_continuous("Concentration (mg/L) \n", lim = c(0,50))
      #	plotobj10s <- plotobj10s + scale_x_continuous("\nTime (hours)", lim = c(0,24))
      #	print(plotobj10s)
      
    })	#This the end of the renderprint identified as "rp1"
  
  output$final210s <-
    renderTable({
      #This is the starting of the renderprint identified as "rp2"
      
      C110s <-
        input$C110s #The variable C110s is definded to take the input value based on the widget with the code identification value of "C110s"
      C210s <- input$C210s
      C310s <- input$C310s
      C410s <- input$C410s
      C510s <- input$C510s
      C610s <- input$C610s
      C710s <- input$C710s
      C810s <- input$C810s
      C910s <- input$C910s
      C1010s <- input$C1010s
      V110s <- input$V110s
      V210s <- input$V210s
      V310s <- input$V310s
      V410s <- input$V410s
      V510s <- input$V510s
      V610s <- input$V610s
      V710s <- input$V710s
      V810s <- input$V810s
      V910s <- input$V910s
      V1010s <- input$V1010s
      U10s <-  input$U10s
      
      a10s <-
        c(C110s,
          C210s,
          C310s,
          C410s,
          C510s,
          C610s,
          C710s,
          C810s,
          C910s,
          C1010s)# This is defining a new value a which is a concatanation of C110s to C6
      b10s <-
        c(V110s,
          V210s,
          V310s,
          V410s,
          V510s,
          V610s,
          V710s,
          V810s,
          V910s,
          V1010s)# This is defining a new value a which is a concatanation of V110s to v6
      
      
      c10s <- lm(a10s ~ b10s)
      coeffs10s = coefficients(c10s)
      #coeffs10s = coefficients(c10s)
      #a10s.lm is for representing that we are finding a10s through lm
      #O10s<- coeffs10s[1] + coeffs10s[2]*U10s
      #r10s <- summary(c10s)$r.squared
      #print(r10s)
      s10s <- glance(c10s)
      t10s <- t(as.matrix(s10s))
      t10s <- as.table.default(t10s)
      t10s
      
    })#This is the end of the renderprint "rp2"
  
  output$final310s <-
    renderPlot({
      #This is the starting of the renderplot identified as "rplot1"
      
      C110s <-
        input$C110s #The variable C110s is definded to take the input value based on the widget with the code identification value of "C110s"
      C210s <- input$C210s
      C310s <- input$C310s
      C410s <- input$C410s
      C510s <- input$C510s
      C610s <- input$C610s
      C710s <- input$C710s
      C810s <- input$C810s
      C910s <- input$C910s
      C1010s <- input$C1010s
      V110s <- input$V110s
      V210s <- input$V210s
      V310s <- input$V310s
      V410s <- input$V410s
      V510s <- input$V510s
      V610s <- input$V610s
      V710s <- input$V710s
      V810s <- input$V810s
      V910s <- input$V910s
      V1010s <- input$V1010s
      U10s <-  input$U10s
      
      a10s <-
        c(C110s,
          C210s,
          C310s,
          C410s,
          C510s,
          C610s,
          C710s,
          C810s,
          C910s,
          C1010s)# This is defining a new value a which is a concatanation of C110s to C6
      b10s <-
        c(V110s,
          V210s,
          V310s,
          V410s,
          V510s,
          V610s,
          V710s,
          V810s,
          V910s,
          V1010s)# This is defining a new value a which is a concatanation of V110s to v6
      
      c10s <- lm(a10s ~ b10s)
      coeffs10s = coefficients(c10s)
      
      #O10s<- coeffs10s[1] + coeffs10s[2]*U10s
      #r10s<- summary(a10s.lm)$r10s.squared
      #plotobj10s<- ggplot(plot(a10s,b10s))+  abline(lm(a10s~b10s))
      #print(plotobj10s)
      conc.data10s <-
        data.frame(a10s = a10s, b10s = b10s)#This is for forming the dataframe with a10s and b10s as the data
      
      #This is for the ggplot2 object output
      plotobj10s <- ggplot(data = conc.data10s)
      plotobj10s <-
        plotobj10s + geom_line(aes(x = a10s, y = b10s), colour = "red", size = 1)
      plotobj10s <-
        plotobj10s + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobj10s <-
        plotobj10s + scale_y_continuous("\n Spectral value ")
      print(plotobj10s) #This is for plotting the plotobj10s
      
    })#This is the end of the renderplot "rplot1"
  
  # output$table <- DT::renderDataTable(DT::datatable({
  #   # input$file1 will be NULL initially. After the user selects
  #   # and uploads a file, it will be a data frame with 'name',
  #   # 'size', 'type', and 'datapath' columns. The 'datapath'
  #   # column will contain the local filenames where the data can
  #   # be found.
  #   inFile <- input$file1
  #
  #   if (is.null(inFile))
  #     {return(NULL)}
  #   else{
  #   abc<-read.csv(inFile)
  #   }
  #   abc
  #   #abexplore<-as.matrix(aexplore)
  #   #abcexplore<-as.table(abexplore)
  #   })
  # )
  
  data <- reactive({
    file1 <- input$file
    if (is.null(file1)) {
      return()
    }
    abc <-
      read.table(
        file = file1$datapath,
        sep = input$sep,
        header = input$header,
        stringsAsFactors = TRUE
      )
    # abcd<-data.frame(abc)
  })
  # output$tabletrial<-DT::renderDataTable(DT::datatable({
  #   data}))
  # this reactive output contains the summary of the dataset and display the summary in table format
  # output$filedf <- renderTable({
  #   if(is.null(data())){return ()}
  #   input$file
  # })
  
  # output$sum <- renderTable({
  #   if(is.null(data())){return ()}
  #   summary(data())
  #
  # })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- DT::renderDataTable({
    if (is.null(data())) {
      return ()
    }
    data()
  })
  
  # This reactive output contains the dataset and display the dataset in table format
  
  output$visualdrug<-renderPlot({
    if(input$drugvis == "allvis"){
      abcvis<-data()[,7]
      cbdvis<-table(abcvis)
      barplot(cbdvis,width = 2,xlim = c(0,10), col =  rainbow(3),xlab = "Different drugs assayed in the laboratory",
              ylab = "The frequency of drugs assayed",
              main = "The frequency of different drugs assayed in laboratory")
      # efgvis<-tapply(data()[,8],data()[,7],mean)
      # barplot(efgvis,width = 2,xlim = c(0,10), col =  rainbow(7),xlab = "Different drugs assayed in the laboratory",
      #         ylab = "Mean of the concentration in the measured unit",
      #         main = "The mean of concentration of different drugs assayed in the laboratory")
     # ggplot(cbdmean)
    # print("The mean of Phenytoin is ")
    }else if(input$drugvis == "phenvis"){
      print ("Under Construction")
    }else if(input$drugvis == "carbmvis"){
      print("Under Construction")
    } else {
      print("Under Construction")
    }
      
  })
  
  output$visualdrug2<-renderPlot({
    if(input$drugvis == "allvis"){
      # abcvis<-data()[,7]
      # cbdvis<-table(abcvis)
      # barplot(cbdvis,width = 2,xlim = c(0,10), col =  rainbow(3),xlab = "Different drugs assayed in the laboratory",
      #         ylab = "The frequency of drugs assayed",
      #         main = "Frequency of different drugs assayed in the laboratory")
      efgvis<-tapply(data()[,8],data()[,7],mean)
      barplot(efgvis,width = 2,xlim = c(0,10), col =  rainbow(3),xlab = "Different drugs assayed in the laboratory",
              ylab = "Mean of the concentration in the measured unit",
              main = "The mean of concentration of different drugs assayed in the laboratory")
      # ggplot(cbdmean)
      # print("The mean of Phenytoin is ")
    }else if(input$drugvis == "phenvis"){
      print ("Under Construction")
    }else if(input$drugvis == "carbmvis"){
      print("Under Construction")
    } else {
      print("Under Construction")
    }
    
  })
  
  
  
  # output$visualfind<renderPlot({
  # 
  #   if(input$drugvisat == "allvisat"){
  # 
  #    visdat<-data()[,c(7,8)]
  #    abcvisdat<-visdat[,2]
  #    barplot(abcvisdat,col="black", main = "Concentration of drug in the different samples assayed in laboratory")
  # 
  #   }else{
  #     
  #   }
  #   
  #   
  #   # else if(input$drugvisat == "phenvisat"){
  #   # 
  #   #  visdat<-data()
  #   #   visdat<-visdat[visdat[,7] == "phenytoin",]
  #   #   abcvisdat<-visdat[,8]
  #   #   barplot(abcvisdat,col = "blue", main = "Concentration of phenytoin in the different samples assayed in laboratory")
  #   # 
  #   # }else if(input$drugvisat == "carbmvisat"){
  #   # 
  #   #   visdat<-data()
  #   #   visdat<-visdat[visdat[,7] == "carbamazepine",]
  #   #   abcvisdat<-visdat[,8]
  #   #   barplot(abcvisdat,col = "red", main = "Concentration of carbamazepine in the different samples assayed in laboratory")
  #   # 
  #   # } else {
  #   # 
  #   #   visdat<-data()
  #   #   visdat<-visdat[visdat[,7] == "phenobarbitone",]
  #   #   abcvisdat<-visdat[,8]
  #   #   barplot(abcvisdat,col = "green", main = "Concentration of phenobarbitone in the different samples assayed in laboratory")
  #   # 
  #   # }
  # 
  # 
  # 
  # })
  
  output$visualfind2<-renderPlot({
    if(input$drugvisat == "allvisat")
    barplot(data()[,8],col = "black", main = "Concentration of drug in the different samples assayed in laboratory", 
            ylab = "Values of drug conc. in measured units" ,
            xlab = "Sequential order of time")
    
    else if( input$drugvisat == "phenvisat"){
      abcvisat<-data()[data()[,7]== "phenytoin",]
      abcvisat<-abcvisat[,8]
      barplot(abcvisat,col = "blue", main = "Concentration of phenytoin in the different samples assayed in laboratory", 
              ylab = "Values of phenytoin conc. in measured units" ,
              xlab = "Sequential order of time")
      
    }else if( input$drugvisat == "carbmvisat"){
      abcvisat<-data()[data()[,7]== "carbamazepine",]
      abcvisat<-abcvisat[,8]
      barplot(abcvisat,col = "red", main = "Concentration of carbamazepine in the different samples assayed in laboratory", 
              ylab = "Values of carbamazepine conc. in measured units" ,
              xlab = "Sequential order of time")
    
    }else{
      abcvisat<-data()[data()[,7]== "phenobarbitone",]
      abcvisat<-abcvisat[,8]
      barplot(abcvisat,col = "green", main = "Concentration of phenobarbitone in the different samples assayed in laboratory", 
              ylab = "Values of phenobarbitone conc. in measured units" ,
              xlab = "Sequential order of time")
    }
    
    
  })
  
  # output$visualfind3<-renderPlotly({
  #   if(input$drugvisat == "allvisat")
  #     plot_ly(data(),x= data()[,1], y= data()[,8], color = data()[,7], size = data()[,8])
  #     # barplot(data()[,8],col = "black", main = "Concentration of drug in the different samples assayed in laboratory", 
  #     #         ylab = "Values of drug conc. in measured units" ,
  #     #         xlab = "Sequential order of time")
  #   
  #   else if( input$drugvisat == "phenvisat"){
  #     abcvisat<-data()[data()[,7]== "phenytoin",]
  #     abcvisat<-abcvisat[,8]
  #     plot_ly(abcvisat,x= ~Time.date, y= ~Conc, color = ~Drug, size = ~Conc)
  #     
  #   }else if( input$drugvisat == "carbmvisat"){
  #     abcvisat<-data()[data()[,7]== "carbamazepine",]
  #     abcvisat<-abcvisat[,8]
  #     plot_ly(abcvisat,x= ~Time.date, y= ~Conc, color = ~Drug, size = ~Conc)
  #     
  #   }else{
  #     abcvisat<-data()[data()[,7]== "phenobarbitone",]
  #     abcvisat<-abcvisat[,8]
  #     plot_ly(abcvisat,x= ~Time.date, y= ~Conc, color = ~Drug, size = ~Conc)
  #   }
  #   
  #   
  # })
  
  output$visualfind4<-renderPlotly({
    plot_ly(data(),x= data()[,2], y= data()[,8], color = data()[,7], size = data()[,8])
    
  })
  
  # output$visualfind5<-renderPlotly({
  #   data()[,6]<-as.numeric(data()[,6])
  #   plot_ly(data(),x= data()[,1], y= data()[,8], color = data()[,7], size = data()[,8])
  #   
  # })
  
  output$visualfind5<-renderPlotly({
    plot_ly(data(),x= data()[,6], y= data()[,8], color = data()[,7], size = data()[,8])

  })

  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  
  output$event2 <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  
  
  output$attendrug<-renderPrint({
    if(input$drugsel == "allsel"){
      newdata<-data()[(data()[,8]>input$startconc) & (data()[,8]<input$endconc) ,]
      print(newdata)
      # ggplot(cbdmean)
      # print("The mean of Phenytoin is ")
    }else if(input$drugsel == "phensel"){
      newdata<-data()[which(data()[,7] == "phenytoin" &  (data()[,8]>input$startconc) & (data()[,8]<input$endconc) ),]
      print (newdata)
    }else if(input$drugsel == "carbmsel"){
      newdata<-data()[which(data()[,7] == "carbamazepine" &  (data()[,8]>input$startconc) & (data()[,8]<input$endconc) ),]
      print (newdata)
    } else {
      newdata<-data()[which(data()[,7] == "phenobarbitone" &  (data()[,8]>input$startconc) & (data()[,8]<input$endconc) ),]
      print(newdata)
    }
    
  })
  
  output$meandrug<-renderPrint({
    abcmean<-data()
    summary(abcmean)
  })
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$uploaded <- renderUI({
    if (is.null(data()))
      h3("R Web Server based Data Handling")
    else
      tabsetPanel(tabPanel
                  ("Data", fluidPage(# fluidRow(column(4,selectInput("drug", "Drug",c("All","phenytoin","carbamazepine","phenobarbitone"))),
                    #                           column(4,selectInput("level","Drug level",c("All","Range to be selected")))),
                    #                           # if(input$level="All")
                    #                  #            else(
                    # fluidRow(
                    #          column(10,sliderInput("value",label = "Starting Range of Drug Level",min = 0,max = 400,value = 2,step = 0.1,width = 1000)),
                    #          column(10,sliderInput("value",label = "Ending Range of Drug Level",min = 0,max = 400,value = 2,step = 0.1,width = 1000)))),
                    
                    
                    fluidRow(
                      DT::dataTableOutput("table")
                    ))),
    tabPanel("Find which patients need attention",selectInput("drugsel","Select the drug of interest",c("All" = "allsel" , "phenytoin" = "phensel","carbamazepine" = "carbmsel","phenobarbitone" = "phenosel")),
             sliderInput("startconc","Select the starting concentratoin",min = 0,max = 100, step = 0.1,value = 25,width = 1000),
             sliderInput("endconc","Select the ending concentratoin",min = 0,max = 100, step = 0.1,value = 25,width = 1000),
             verbatimTextOutput("attendrug")),tabPanel("Visual inspection - static",selectInput("drugvisat","Select the drug of interest",c("All" = "allvisat" , "phenytoin" = "phenvisat","carbamazepine" = "carbmvisat","phenobarbitone" = "phenovisat")),
                                                       plotOutput("visualfind2")),
    tabPanel("Date-wise visual inspection - dynamic",fluidPage(plotlyOutput("visualfind4"),br(),hr(),verbatimTextOutput("event")
                                                    #plotlyOutput("visualfind5",br(),hr(),verbatimTextOutput("event2"))
                                                    )),
    tabPanel("Id-wise visual inspection - dynamic", fluidPage(plotlyOutput("visualfind5"), br(),hr(), verbatimTextOutput("event2"))),
    tabPanel("Visualisation", selectInput("drugvis","Select the drug of interest",c("All" = "allvis")),
                                               plotOutput("visualdrug")),tabPanel("Summary",verbatimTextOutput("meandrug"),width=1000))
    
  })
  
  # output$table3 <- DT::renderDataTable({
  #   # if (input$level != "All") {
  #   #   datanew <- data()[data()[,5] == "phenytoin",]
  #   #   datanew
  #   # }
  #   data()
  #
  # })
  
  # output$selectionrange<-renderUI({
  #   if(input$level="All")
  #     h3("No selection range")
  #   else
  #     fluidRow(
  #                 column(10,sliderInput("value",label = "Starting Range of Drug Level",min = 0,max = 400,value = 2,step = 0.1,width = 1000)),
  #                 column(10,sliderInput("value",label = "Ending Range of Drug Level",min = 0,max = 400,value = 2,step = 0.1,width = 1000)))
  #    })
  #
  #
  #
  #   ("Data picker for DMdat.csv file ",
  #     fluidPage(
  #       fluidRow(column(
  #         4,
  #         selectInput("treat",
  #                     "Treatment",
  #                     c("All",
  #                       unique(
  #                         as.character(DMdat$Treat)))
  #         )),
  #         column(
  #           4,
  #           selectInput("age",
  #                       "Age",
  #                       c("All",
  #                         unique(
  #                           as.character(DMdat$Age)))
  #           )),
  #         column(
  #           4,
  #           selectInput("priorx",
  #                       "Insulin or Non Insulin:",
  #                       c("All",
  #                         unique(
  #                           as.character(DMdat$PrioRx)
  #                         )))
  #         )),#This is the end of fluidrow
  #       # Create a new row for the table.
  #       fluidRow(DT::dataTableOutput("table"))
  #     ))#This is the ending of tabpanel named picker for DMdat
  
  
  ####Start comment
  
  datastd <- reactive({
    file2 <- input$file2
    if (is.null(file2)) {
      return()
    }
    abcd <-
      read.table(
        file = file2$datapath,
        sep = input$sep2,
        header = input$header2,
        stringsAsFactors = TRUE
      )
    # anew<-abcd$data13s
    # bnew<-abcd$data23s
    # cnew<-lm(anew~bnew)
    # abcd<-data.frame(abc)
  })
  
  
  ############End comment
  
  # output$tabletrial<-DT::renderDataTable(DT::datatable({
  #   data}))
  # this reactive output contains the summary of the dataset and display the summary in table format
  # output$filedf <- renderTable({
  #   if(is.null(data())){return ()}
  #   input$file
  # })
  
  # output$sum <- renderTable({
  #   if(is.null(data())){return ()}
  #   summary(data())
  #
  # })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table2 <- renderTable({
    if (is.null(datastd())) {
      return ()
    }
    datastd()
  })
  
  output$regress <- renderPrint({
    datastd <- reactive({
      file2 <- input$file2
      if (is.null(file2)) {
        return()
      }
      abcd <-
        read.table(
          file = file2$datapath,
          sep = input$sep2,
          header = input$header2,
          stringsAsFactors = TRUE
        )
    })
    assign('dataglobal', datastd(), envir = .GlobalEnv)
    summary(dataglobal[, 2])
    data13s <- dataglobal[, 2]
    data23s <- dataglobal[, 3]
    regresseq <- lm(data13s ~ data23s)
    print(summary(regresseq)$call)
  })
  
  ##############I am pasting the equation of the standard number 3
  
  output$regresspat <-
    renderPrint({
      # #This is the starting of the renderprint identified as "rp1"
      #
      # #This is for collecting the inputs from the user widgets
      #
      # #C13s # This identifies the chromatographic technique with number of standard equal to '3' and 'C13s' representing first widget for entering concentration. So essentially 'g3' will be the identification
      #
      # C13s <-
      #   input$C13s #The variable C13s is definded to take the input value based on the widget with the code identification value of "C13s"
      # C23s <- input$C23s
      # C33s <- input$C33s
      # V13s <- input$V13s
      # V23s <- input$V23s
      # V33s <- input$V33s
      # U3s <-  input$U3s
      #
      # a3s <-
      #   c(C13s, C23s, C33s)# This is defining a new value a which is a concatanation of C13s to C6
      # b3s <-
      #   c(V13s, V23s, V33s)# This is defining a new value a which is a concatanation of V13s to v6
      #
      # c3s <-
      #   lm(a3s ~ b3s)#This is the linear regression between a3s and b3s
      
      datastd <- reactive({
        file2 <- input$file2
        if (is.null(file2)) {
          return()
        }
        abcd <-
          read.table(
            file = file2$datapath,
            sep = input$sep2,
            header = input$header2,
            stringsAsFactors = TRUE
          )
      })
      
      Uregress2 <- input$Uregress
      
      assign('dataglobal', datastd(), envir = .GlobalEnv)
      summary(dataglobal[, 2])
      data13s <- dataglobal[, 2]
      data23s <- dataglobal[, 3]
      regresseq <- lm(data13s ~ data23s)
      coeffsregress = coefficients(regresseq)
      #We are determining the coefficient of c3s which is the linear regression equation
      Oregress <-
        coeffsregress[1] + coeffsregress[2] * Uregress2 #coeffs3s [1] is the intercept and coeffs3s[2] is the slope of the regression equation
      #r3s<- summary(c3s)$r3s.squared #Not required
      
      print(Oregress)
      # sregress<-glance(regresseq)
      # tregress<-t(as.matrix(regresseq))
      # tregress<-as.table.default(tregress)
      # tregress
      
      #Calculate function of Dose, V, KA and KE (where F = 1)
      #if (KA==KE) A <- 0 else A <- (D*KA)/(V*(KA-KE)) #This is how you define a if and if equation
      
      #Calculate concentration
      #CONC <- A*exp(-KE*TIME) - A*exp(-KA*TIME)
      
      #Create a dataframe of time and concentration data
      #conc.data3s <- data.frame(CONC = CONC , TIME =TIME)
      
      #ggplot2 object
      #	plotobj3s <- ggplot(data = conc.data3s)
      #	plotobj3s <- plotobj3s + geom_line(aes(x = TIME, y = CONC), colour = "red", size = 1)
      #	plotobj3s <- plotobj3s + scale_y_continuous("Concentration (mg/L) \n", lim = c(0,50))
      #	plotobj3s <- plotobj3s + scale_x_continuous("\nTime (hours)", lim = c(0,24))
      #	print(plotobj3s)
      
    })	#This the end of the renderprint identified as "rp1"
  
  output$regresstab <- renderTable({
    # #This is the starting of the renderprint identified as "rp1"
    #
    # #This is for collecting the inputs from the user widgets
    #
    # #C13s # This identifies the chromatographic technique with number of standard equal to '3' and 'C13s' representing first widget for entering concentration. So essentially 'g3' will be the identification
    #
    # C13s <-
    #   input$C13s #The variable C13s is definded to take the input value based on the widget with the code identification value of "C13s"
    # C23s <- input$C23s
    # C33s <- input$C33s
    # V13s <- input$V13s
    # V23s <- input$V23s
    # V33s <- input$V33s
    # U3s <-  input$U3s
    #
    # a3s <-
    #   c(C13s, C23s, C33s)# This is defining a new value a which is a concatanation of C13s to C6
    # b3s <-
    #   c(V13s, V23s, V33s)# This is defining a new value a which is a concatanation of V13s to v6
    #
    # c3s <-
    #   lm(a3s ~ b3s)#This is the linear regression between a3s and b3s
    
    datastd <- reactive({
      file2 <- input$file2
      if (is.null(file2)) {
        return()
      }
      abcd <-
        read.table(
          file = file2$datapath,
          sep = input$sep2,
          header = input$header2,
          stringsAsFactors = TRUE
        )
    })
    
    
    Uregress2 <- input$Uregress
    
    assign('dataglobal', datastd(), envir = .GlobalEnv)
    summary(dataglobal[, 2])
    data13sx <- dataglobal[, 2]
    data23sx <- dataglobal[, 3]
    regresseqx <- lm(data13sx ~ data23sx)
    # coeffsregressx = coefficients(regresseqx)
    # #We are determining the coefficient of c3s which is the linear regression equation
    # Oregress <-coeffsregress[1] + coeffsregress[2] * Uregress2 #coeffs3s [1] is the intercept and coeffs3s[2] is the slope of the regression equation
    # #r3s<- summary(c3s)$r3s.squared #Not required
    
    #print(Oregress)
    
    sregress <- glance(regresseqx)
    tregress <- t(as.matrix(sregress))
    tregress <- as.table.default(tregress)
    tregress
    
    #Calculate function of Dose, V, KA and KE (where F = 1)
    #if (KA==KE) A <- 0 else A <- (D*KA)/(V*(KA-KE)) #This is how you define a if and if equation
    
    #Calculate concentration
    #CONC <- A*exp(-KE*TIME) - A*exp(-KA*TIME)
    
    #Create a dataframe of time and concentration data
    #conc.data3s <- data.frame(CONC = CONC , TIME =TIME)
    
    #ggplot2 object
    #	plotobj3s <- ggplot(data = conc.data3s)
    #	plotobj3s <- plotobj3s + geom_line(aes(x = TIME, y = CONC), colour = "red", size = 1)
    #	plotobj3s <- plotobj3s + scale_y_continuous("Concentration (mg/L) \n", lim = c(0,50))
    #	plotobj3s <- plotobj3s + scale_x_continuous("\nTime (hours)", lim = c(0,24))
    #	print(plotobj3s)
    
  })	#This the end of the renderprint identified as "rp1"
  
  output$regressplot <-
    renderPlot({
      #This is the starting of the renderplot identified as "rplot1"
      
      # C13s <- input$C13s
      # C23s <- input$C23s
      # C33s <- input$C33s
      # V13s <- input$V13s
      # V23s <- input$V23s
      # V33s <- input$V33s
      # U3s <-  input$U3s
      #
      # a3s <- c(C13s, C23s, C33s)
      # b3s <- c(V13s, V23s, V33s)
      # c3s <- lm(a3s ~ b3s)
      # coeffs3s = coefficients(c3s)
      
      #O3s<- coeffs3s[1] + coeffs3s[2]*U3s
      #r3s<- summary(a3s.lm)$r3s.squared
      #plotobj3s<- ggplot(plot(a3s,b3s))+  abline(lm(a3s~b3s))
      #print(plotobj3s)
      
      datastd <- reactive({
        file2 <- input$file2
        if (is.null(file2)) {
          return()
        }
        abcd <-
          read.table(
            file = file2$datapath,
            sep = input$sep2,
            header = input$header2,
            stringsAsFactors = TRUE
          )
      })
      
      
      Uregress2 <- input$Uregress
      
      assign('dataglobal', datastd(), envir = .GlobalEnv)
      summary(dataglobal[, 2])
      data13sp <- dataglobal[, 2]
      data23sp <- dataglobal[, 3]
      # regresseqp<-lm(data13sx~data23sx)
      
      conc.dataregressp <-
        data.frame(data13sp = data13sp, data23sp = data23sp)#This is for forming the dataframe with a3s and b3s as the data
      
      #This is for the ggplot2 object output
      plotobjregressp <- ggplot(data = conc.dataregressp)
      plotobjregressp <-
        plotobjregressp + geom_line(aes(x = data13sp, y = data23sp),
                                    colour = "red",
                                    size = 1)
      plotobjregressp <-
        plotobjregressp + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobjregressp <-
        plotobjregressp + scale_y_continuous("\n Spectral value ")
      print(plotobjregressp) #This is for plotting the plotobj3s
      
    })#This is the end of the renderplot "rplot1"
  
  ##############I am ending the equation of the standard number 3
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$uploaded2 <- renderUI({
    if (is.null(datastd()))
      h3("Reusing the already created concentration vs spectral value file")
    else
      tabsetPanel(
        tabPanel(
          "Data",
          hr(),
          h4("The representation of the data of standard in tabular column"),
          tableOutput("table2"),
          hr(),
          h5(
            "Please check whether the data in csv file and the data in table matches"
          )
        ),
        tabPanel(
          "Regression",
          hr(),
          h4(
            "The formula of the regression equation on the basis of which the calculation will be made"
          ),
          h4(textOutput("regress")),
          
          h5(
            "Please check whether, data13s and data23s forms the component of the regression equation"
          ),
          hr(),
          h4(
            numericInput("Uregress", label = "The spectra value as calculated through chromatographic method for the patient sample", value = 1)
          ),
          hr(),
          h4(
            "Result denoting the concentration of drug in the given sample as determined by Chromatographic technique"
          ),
          h4(
            "The calculated concentration of the drug in patient biological sample is"
          ),
          #This is identification of concentration of drug
          h4(textOutput("regresspat")),
          hr(),
          
          h4(
            "Result representing the important statistics of the regression equation determined using the standard"
          ),
          
          
          h5("The statistics are tabulated here under"),
          #This is identification of regression value of the equation
          br(),
          
          h4(tableOutput("regresstab")),
          hr(),
          plotOutput("regressplot", width = 1000),
          align = "center"
        )
        # # anew<-abcd$data13s,
        # # bnew<-abcd$data23s,
        # # anew<-as.numeric(anew),
        # # bnew<-as.numeric(bnew),
        # # cnew<-lm(anew~bnew),
        # glance(cnew),
        # print(glance(cnew))
        
      )
    
  })
  
  
  
  output$calcalbuminout <- renderPrint({
    phen1 <- input$concphen
    alb1 <- input$concalbumin
    #alb1<-10000*alb1
    corrphen <- phen1 / ((alb1 * 0.2) + 0.1)
    print(corrphen)
  })
  
  output$calcalbuminout2 <- renderPrint({
    phen2 <- input$concphen2
    alb2 <- input$concalbumin2
    #alb2<-10000*alb2
    corrphen2 <- phen2 / ((alb2 * 0.1) + 0.1)
    print(corrphen2)
  })
  
  
  #
  # output$abc123<-renderPrint(
  #   {
  #   abc123<-input$concphen
  #   abc234<-input$concalbumin
  #   abc567<-abc123/abc234
  #   }
  #   )
  
  
  output$albumin <- renderUI({
    # if(input$units == "microgm/ml"& input$crcl =="YES"){
    #   h3("Result 1")}
    # if(input$units == "microgm/ml"& input$crcl =="NO"){
    #   h3("Result 2")}
    # if(input$units == "micromol/l"& input$crcl == "YES"){
    #   h3("Result 3")}
    if (input$crcl == "YES")
      tabsetPanel("Albumin correction in phenytoin concentration",
        tabPanel(
          "Adjustment = 0.2",
          h4(
            "An adjstment of 0.2 will be taken to calculate the corrected phenytoin concentration"
          ),
          hr(),
          sliderInput(
            "concphen",
            "Please select the concentration of phenytoin (in microg/ml)",
            min = 0,
            max = 100,
            value = 24,
            step = 0.1,
            width = 1000
          ),
          helpText(
            "Please note that the units of phenytoin concentration should only be in microg/ml"
          ),
          sliderInput(
            "concalbumin",
            "Please select the concentration of albumin (in g/dl)",
            min = 1,
            max = 30,
            value = 4.4,
            step = 0.1,
            width = 1000
          ),
          helpText(
            "Please note that the units of albumin level should only be in g/dl"
          ),
          hr(),
          h3("The corrected phenytoin concentration is (in microgm/ml) is"),
          hr(),
          h3(textOutput("calcalbuminout"))
        )
      )
    else
      tabsetPanel("Albumin correction in phenytoin concentration",
        tabPanel(
          "Adjustment =0.1",
          h4(
            "An adjstment of 0.1 will be taken to calculate the corrected phenytoin concentration"
          ),
          hr(),
          sliderInput(
            "concphen2",
            "Please select the concentration of phenytoin (in mircrog/ml)",
            min = 0,
            max = 100,
            value = 22,
            step = 0.1,
            width = 1000
          ),
          helpText(
            "Please note that the units of phenytoin should only be in microg/ml"
          ),
          sliderInput(
            "concalbumin2",
            "Please select the concentration of albumin (in g/dl)",
            min = 0,
            max = 100,
            value = 4.4,
            step = 0.1,
            width = 1000
          ),
          helpText("Please note that the units of albumin should only be in g/dl"),
          hr(),
          h3("The corrected phenytoin concentration (in microgm/ml) is"),
          hr(),
          h3(textOutput("calcalbuminout2"))
        )
        
      )
    
  })
  
  output$resultinter1 <- renderPrint({
    if (input$phenslideinter < 10 & input$age1 == "adults1") {
      print("Concentration of phenytoin is lower than normal in the sample")
    } else if (((input$phenslideinter >= 10) &
                (input$phenslideinter <= 20)) &&
               input$age1 == "adults1") {
      print("Concentration of phenytoin is in the normal range in the sample")
    } else if (((input$phenslideinter > 20) &
                (input$phenslideinter <= 50)) &&
               input$age1 == "adults1") {
      print("Concentration of phenytoin is in the higher range in the sample")
    } else if (input$phenslideinter > 50 &
               input$age1 == "adults1") {
      print(
        "Highly toxic concentration of phenytoin is seen in the sample. Please correlate clinically. Stop administering phenytoin."
      )
    } else if (input$phenslideinter < 8 & input$age1 == "neon1") {
      print("Concentration of phenytoin is lower than normal in the sample")
    } else if (((input$phenslideinter >= 8) &
                (input$phenslideinter <= 15)) &&
               input$age1 == "neon1") {
      print("Concentration of phenytoin is in the normal range in the sample")
    } else if (((input$phenslideinter > 15) &
                (input$phenslideinter <= 50)) &&
               input$age1 == "neon1") {
      print("Concentration of phenytoin is in the higher range in the sample")
    } else if (input$phenslideinter > 50 & input$age1 == "neon1") {
      print(
        "Highly toxic concentration of phenytoin seen in the sample. Please correlate clinically. Stop administering phenytoin."
      )
      
      ###########
      
      
        
     ###############
    } else {
      print("Sorry. The server could not give you interpretation")
    }
    
  })
  
  output$resultinter2<-renderPrint({
    
   if (input$carbmslideinter < 4 & input$age2 == "adults2") {
    print("Concentration of carbamazepine is lower than normal in the sample")
  } else if (((input$carbmslideinter >= 4) &
              (input$carbmslideinter <= 12)) &&
             input$age2 == "adults2") {
    print("Concentration of carbamazepine is in the normal range in the sample")
  } else if (((input$carbmslideinter > 12) &
              (input$carbmslideinter <= 30)) &&
             input$age2 == "adults2") {
    print("Concentration of carbamazepine is in the higher range in the sample")
  } else if (input$carbmslideinter > 30 &
             input$age2 == "adults2") {
    print(
      "Highly toxic concentration of carbamazepine seen in the sample. Please correlate clinically. Stop administering carbamazepine."
    )
  } else if (input$carbmslideinter < 4 & input$age2 == "neon2") {
    print("Concentration is carbamazepine is lower than normal in the sample")
  } else if (((input$carbmslideinter >= 4) &
              (input$carbmslideinter <= 12)) &&
             input$age2 == "neon2") {
    print("Concentration of carbamazepine is in the normal range in the sample")
  } else if (((input$carbmslideinter > 12) &
              (input$carbmslideinter <= 30)) &&
             input$age2 == "neon2") {
    print("Concentration of carbamazepine is in the higher range in the sample")
  } else if (input$carbmslideinter > 30 & input$age2 == "neon2") {
    print(
      "Highly toxic concentration of carbamazepine seen in the sample. Please correlate clinically. Stop administering carbamazepine."
    )
  } else {
    print("Sorry. The server could not give you interpretation")
  }
   
  })
  
  output$resultinter3<-renderPrint({
    
    
    if (input$phenoslideinter < 15 & input$age3 == "adults3") {
      print("Concentration of phenobarbitone is lower than normal in the sample")
    } else if (((input$phenoslideinter >= 15) &
                (input$phenoslideinter <= 40)) &&
               input$age3 == "adults3") {
      print("Concentration of phenobarbitone is in the normal range in the sample")
    } else if (((input$phenoslideinter > 40) &
                (input$phenoslideinter <= 60)) &&
               input$age3 == "adults3") {
      print("Concentration of phenobarbitone is in the higher range in the sample")
    } else if (input$phenoslideinter > 60 &
               input$age3 == "adults3") {
      print(
        "Highly toxic concentration of phenobarbitone seen in the sample. Please correlate clinically. Stop administering phenobarbitone."
      )
    } else if (input$phenoslideinter < 15 & input$age3 == "neon3") {
      print("Concentration of phenobarbitone is lower than normal in the sample")
    } else if (((input$phenoslideinter >= 15) &
                (input$phenoslideinter <= 30)) &&
               input$age3 == "neon3") {
      print("Concentration of phenobarbitone is in the normal range in the sample")
    } else if (((input$phenoslideinter > 30) &
                (input$phenoslideinter <= 60)) &&
               input$age3 == "neon3") {
      print("Concentration of phenobarbitone is in the higher range in the sample")
    } else if (input$phenoslideinter > 60 & input$age3 == "neon3") {
      print(
        "Highly toxic concentration of phenobarbitone seen in the sample. Please correlate clinically. Stop administering phenobarbitone."
      )
    } else {
      print("Sorry. The server could not give you interpretation")
    }
    
    
    
  })
  ##################################
  
  output$uploaded3<- renderUI({
    # if(input$units == "microgm/ml"& input$crcl =="YES"){
    #   h3("Result 1")}
    # if(input$units == "microgm/ml"& input$crcl =="NO"){
    #   h3("Result 2")}
    # if(input$units == "micromol/l"& input$crcl == "YES"){
    #   h3("Result 3")}
    if (input$druginterpret == "pheninter")
      {tabsetPanel(
        tabPanel(
          h4("Phenytoin"),
          hr(),
          sliderInput("phenslideinter","Please select the concentration of phenytoin (in microg/ml)",
          min = 0, max = 100, value = 15,step = 0.1,width = 1000),
                helpText("Please note that the phenytoin level which you are prompted to enter above is total pheytoin concentration"),
          selectInput("age1","Please select the age group of patient",c("Children and Adults"="adults1","Neonates"="neon1") ),
          hr(),
          h4("The interpretation for the level of phenytoin in the plasma sample of patient is"),
       
        h3(textOutput("resultinter1"))),
        hr(),
        helpText("The interpretation given is only for reference and requires clinical conformation. The server(and the associated people) is not liable legally"),
        hr()
        )
      }
    else if (input$druginterpret == "carbminter")
      {tabsetPanel(
        tabPanel(
          h4("Carbamazepine"),
          hr(),
          sliderInput("carbmslideinter","Please select the concentration of carbamzepine (in mg/l)",
                      min = 0, max = 70, value = 15,step = 0.1,width = 1000),
          hr(),
          selectInput("age2","Please select the age group of patient",c("Children and Adults"="adults2","Neonates"="neon2") ),
          hr(),
          h4("The interpretation for the level of carbamazepine in the plasma sample of patient is"),
          
        h3(textOutput("resultinter2")),
        hr(),
        helpText("The interpretation given is only for reference and requires clinical conformation. The server(and the associated people) is not liable legally"),
        hr()
        )
      )}
    else
      tabsetPanel(
        tabPanel(
          h4("Phenobarbitone"),
          hr(),
          sliderInput("phenoslideinter","Please select the concentration of phenobarbitone (in microg/ml)",
                      min = 0, max = 100, value = 15,step = 0.1,width = 1000),
          hr(),
          selectInput("age3","Please select the age group of patient",c("Adults and children >5 yrs"="adults3","Infants and children < 5 years"="neon3") ),
        hr(),
        h4("The interpretation for the level of phenobarbitone in the plasma sample of patient is"),
        h3(textOutput("resultinter3")),
        hr(),
        helpText("The interpretation given is only for reference and requires clinical conformation. The server(and the associated people) is not liable legally"),
        hr()
        )

      )

  })
  
  
  output$uploaded4<-renderUI({
    
    if(input$fillable2 == "png")
      img(src='pngfig.png')
      
    else if(input$fillable2 == "downloadable"){
      tags$iframe(style="height:700px; width:100%; scrolling=yes", 
                  src="web.pdf")
    
    }else if (input$fillable2 == "fillable"){
      tags$iframe(style="height:700px; width:100%; scrolling=yes", 
                  src="fillable.pdf")
    }
    else {
      htmlOutput("increase")
    }
    
  })
  
  
  getPage<-function(){
    return((HTML(readLines('https://drive.google.com/drive/folders/0BwvOvYK5wRDUcmk3SGppYm0yUUE?usp=sharing'))))
  }
  
  output$increase <-renderUI(
    getPage()
  )
  
  # getPage2<-function(){
  #   return((HTML(readLines('https://drive.google.com/file/d/0BwvOvYK5wRDUTWNjWWl5OE5xazQ/view?usp=sharing'))))
  # }
  # 
  # output$increase2 <-renderUI(
  #   getPage2()
  # )
  # 
  
  # output$uploaded3<-renderUI({
  #   if(input$druginterpret=="pheninter")
  #    {tabsetPanel(tabPanel(h3("Results1")))}
  #   else if(input$druginterpret=="carbminter")
  #     {tabsetPanel(tabPanel(h3("Results2")))}
  #   else
  #     tabsetPanel(tabPanel(h3("Results3")))
  # })
  # 
})#This brackets denotes the end of "shinyServer"
