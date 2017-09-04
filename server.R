

"Shyam's Chromatography & TDM Server - Estimation Compilation Evaluation Reutlisation Epitransformation Interpretation (CTS-ECEREI)"
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


downloadstandard <-
  read.csv("standard2017-08-062017-08-06 17-45-50.csv")
downloadtemplate <- read.csv("template file.csv")
downloadtemplate2 <-
  read.csv("chromatography2017-08-062017-08-06 19-36-06.csv")
downloadtemplate3 <-
  read.csv("chromatography2017-08-062017-08-06 19-36-06.csv")
downloadtemplate4 <- read.csv("larger database.csv")

theme_custom <- theme_set(theme_bw())

shinyServer(function(input, output, session) {
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
  
  
  
  formData <- reactive({
    data <- sapply(fields, function(x)
      input[[x]])
    data
  })
  
  
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
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("chromatography", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(loadData(), file)
    }
  )
  
  
  
  formData3s <- reactive({
    data13s <- sapply(fields13s, function(x)
      input[[x]])
    data23s <- sapply(fields23s, function(x)
      input[[x]])
    dataa13s <- as.data.frame(data13s)
    dataa23s <- as.data.frame(data23s)
    datamain3s <- data.frame(dataa13s, data23s)
  })
  
  output$downloadDataexample <- downloadHandler(
    filename = function() {
      paste("standard", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(downloadstandard, file, row.names = FALSE)
    }
  )
  
  output$downloadDatatemplate <- downloadHandler(
    filename = function() {
      paste("standard", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(downloadtemplate, file, row.names = FALSE)
    }
  )
  
  output$downloadDatatemplate2 <- downloadHandler(
    filename = function() {
      paste("standard", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(downloadtemplate2, file, row.names = FALSE)
    }
  )
  
  output$downloadDatatemplate3 <- downloadHandler(
    filename = function() {
      paste("chromatography", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(downloadtemplate2, file, row.names = FALSE)
    }
  )
  
  output$downloadDatatemplate4 <- downloadHandler(
    filename = function() {
      paste("chromatography", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(downloadtemplate4, file, row.names = FALSE)
    }
  )
  
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
  
  output$final3s <-
    renderPrint({
      C13s <-
        input$C13s
      C23s <- input$C23s
      C33s <- input$C33s
      V13s <- input$V13s
      V23s <- input$V23s
      V33s <- input$V33s
      U3s <-  input$U3s
      
      a3s <-
        c(C13s, C23s, C33s)
      b3s <-
        c(V13s, V23s, V33s)
      
      c3s <-
        lm(a3s ~ b3s)
      coeffs3s = coefficients(c3s)
      
      O3s <-
        coeffs3s[1] + coeffs3s[2] * U3s
      
      print(O3s)
      
      
      
    })
  
  output$final23s <-
    renderTable({
      C13s <-
        input$C13s
      C23s <- input$C23s
      C33s <- input$C33s
      V13s <- input$V13s
      V23s <- input$V23s
      V33s <- input$V33s
      U3s <-  input$U3s
      
      a3s <- c(C13s, C23s, C33s)
      b3s <- c(V13s, V23s, V33s)
      c3s <- lm(a3s ~ b3s)
      
      s3s <- glance(c3s)
      t3s <- t(as.matrix(s3s))
      t3s <- as.table.default(t3s)
      t3s
    })
  
  output$final33s <-
    renderPlot({
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
      
      
      conc.data3s <-
        data.frame(a3s = a3s, b3s = b3s)
      
      plotobj3s <- ggplot(data = conc.data3s)
      plotobj3s <-
        plotobj3s + geom_line(aes(x = a3s, y = b3s), colour = "red", size = 1)
      plotobj3s <-
        plotobj3s + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobj3s <-
        plotobj3s + scale_y_continuous("\n Spectral value ")
      print(plotobj3s)
    })
  
  output$final4s <-
    renderPrint({
      C14s <-
        input$C14s
      C24s <- input$C24s
      C34s <- input$C34s
      C44s <- input$C44s
      V14s <- input$V14s
      V24s <- input$V24s
      V34s <- input$V34s
      V44s <- input$V44s
      U4s <-  input$U4s
      
      a4s <-
        c(C14s, C24s, C34s, C44s)
      b4s <-
        c(V14s, V24s, V34s, V44s)
      
      c4s <-
        lm(a4s ~ b4s)
      coeffs4s = coefficients(c4s)
      
      O4s <-
        coeffs4s[1] + coeffs4s[2] * U4s
      
      print(O4s)
      
      
    })
  
  output$final24s <-
    renderTable({
      C14s <-
        input$C14s
      C24s <- input$C24s
      C34s <- input$C34s
      C44s <- input$C44s
      V14s <- input$V14s
      V24s <- input$V24s
      V34s <- input$V34s
      V44s <- input$V44s
      
      a4s <-
        c(C14s, C24s, C34s, C44s)
      b4s <-
        c(V14s, V24s, V34s, V44s)
      
      c4s <- lm(a4s ~ b4s)
      coeffs4s = coefficients(c4s)
      s4s <- glance(c4s)
      t4s <- t(as.matrix(s4s))
      t4s <- as.table.default(t4s)
      t4s
      
    })
  
  output$final34s <-
    renderPlot({
      C14s <-
        input$C14s
      C24s <- input$C24s
      C34s <- input$C34s
      C44s <- input$C44s
      V14s <- input$V14s
      V24s <- input$V24s
      V34s <- input$V34s
      V44s <- input$V44s
      
      a4s <-
        c(C14s, C24s, C34s, C44s)
      b4s <-
        c(V14s, V24s, V34s, V44s)
      
      c4s <- lm(a4s ~ b4s)
      coeffs4s = coefficients(c4s)
      
      
      conc.data4s <-
        data.frame(a4s = a4s, b4s = b4s)
      plotobj4s <- ggplot(data = conc.data4s)
      plotobj4s <-
        plotobj4s + geom_line(aes(x = a4s, y = b4s), colour = "red", size = 1)
      plotobj4s <-
        plotobj4s + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobj4s <-
        plotobj4s + scale_y_continuous("\n Spectral value ")
      print(plotobj4s)
    })
  
  
  output$final5s <-
    renderPrint({
      C15s <-
        input$C15s
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
        c(C15s, C25s, C35s, C45s, C55s)
      b5s <-
        c(V15s, V25s, V35s, V45s, V55s)
      
      c5s <-
        lm(a5s ~ b5s)
      coeffs5s = coefficients(c5s)
      
      O5s <-
        coeffs5s[1] + coeffs5s[2] * U5s
      
      print(O5s)
      
      
    })
  
  output$final25s <-
    renderTable({
      C15s <-
        input$C15s
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
        c(C15s, C25s, C35s, C45s, C55s)
      b5s <-
        c(V15s, V25s, V35s, V45s, V55s)
      
      
      c5s <- lm(a5s ~ b5s)
      coeffs5s = coefficients(c5s)
      s5s <- glance(c5s)
      t5s <- t(as.matrix(s5s))
      t5s <- as.table.default(t5s)
      t5s
      
    })
  
  output$final35s <-
    renderPlot({
      C15s <-
        input$C15s
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
        c(C15s, C25s, C35s, C45s, C55s)
      b5s <-
        c(V15s, V25s, V35s, V45s, V55s)
      
      c5s <- lm(a5s ~ b5s)
      coeffs5s = coefficients(c5s)
      
      
      conc.data5s <-
        data.frame(a5s = a5s, b5s = b5s)
      
      plotobj5s <- ggplot(data = conc.data5s)
      plotobj5s <-
        plotobj5s + geom_line(aes(x = a5s, y = b5s), colour = "red", size = 1)
      plotobj5s <-
        plotobj5s + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobj5s <-
        plotobj5s + scale_y_continuous("\n Spectral value ")
      print(plotobj5s)
      
    })
  
  output$final6s <-
    renderPrint({
      C16s <-
        input$C16s
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
        c(C16s, C26s, C36s, C46s, C56s, C66s)
      b6s <-
        c(V16s, V26s, V36s, V46s, V56s, V66s)
      
      c6s <-
        lm(a6s ~ b6s)
      coeffs6s = coefficients(c6s)
      O6s <-
        coeffs6s[1] + coeffs6s[2] * U6s
      
      print(O6s)
      
      
      
    })
  
  output$final26s <-
    renderTable({
      C16s <-
        input$C16s
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
        c(C16s, C26s, C36s, C46s, C56s, C66s)
      b6s <-
        c(V16s, V26s, V36s, V46s, V56s, V66s)
      
      
      c6s <- lm(a6s ~ b6s)
      coeffs6s = coefficients(c6s)
      s6s <- glance(c6s)
      t6s <- t(as.matrix(s6s))
      t6s <- as.table.default(t6s)
      t6s
      
    })
  
  output$final36s <-
    renderPlot({
      C16s <-
        input$C16s
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
        c(C16s, C26s, C36s, C46s, C56s, C66s)
      b6s <-
        c(V16s, V26s, V36s, V46s, V56s, V66s)
      
      
      c6s <- lm(a6s ~ b6s)
      coeffs6s = coefficients(c6s)
      
      
      conc.data6s <-
        data.frame(a6s = a6s, b6s = b6s)
      
      plotobj6s <- ggplot(data = conc.data6s)
      plotobj6s <-
        plotobj6s + geom_line(aes(x = a6s, y = b6s), colour = "red", size = 1)
      plotobj6s <-
        plotobj6s + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobj6s <-
        plotobj6s + scale_y_continuous("\n Spectral value ")
      print(plotobj6s)
      
    })
  
  output$final7s <-
    renderPrint({
      C17s <-
        input$C17s
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
        c(C17s, C27s, C37s, C47s, C57s, C67s, C77s)
      b7s <-
        c(V17s, V27s, V37s, V47s, V57s, V67s, V77s)
      
      c7s <-
        lm(a7s ~ b7s)
      coeffs7s = coefficients(c7s)
      O7s <-
        coeffs7s[1] + coeffs7s[2] * U7s
      
      print(O7s)
      
      
      
    })
  
  output$final27s <-
    renderTable({
      C17s <-
        input$C17s
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
        c(C17s, C27s, C37s, C47s, C57s, C67s, C77s)
      b7s <-
        c(V17s, V27s, V37s, V47s, V57s, V67s, V77s)
      
      
      c7s <- lm(a7s ~ b7s)
      coeffs7s = coefficients(c7s)
      s7s <- glance(c7s)
      t7s <- t(as.matrix(s7s))
      t7s <- as.table.default(t7s)
      t7s
    })
  
  output$final37s <-
    renderPlot({
      C17s <-
        input$C17s
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
        c(C17s, C27s, C37s, C47s, C57s, C67s, C77s)
      b7s <-
        c(V17s, V27s, V37s, V47s, V57s, V67s, V77s)
      
      
      c7s <- lm(a7s ~ b7s)
      coeffs7s = coefficients(c7s)
      
      
      conc.data7s <-
        data.frame(a7s = a7s, b7s = b7s)
      
      plotobj7s <- ggplot(data = conc.data7s)
      plotobj7s <-
        plotobj7s + geom_line(aes(x = a7s, y = b7s), colour = "red", size = 1)
      plotobj7s <-
        plotobj7s + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobj7s <-
        plotobj7s + scale_y_continuous("\n Spectral value ")
      print(plotobj7s)
      
    })
  
  
  output$final8s <-
    renderPrint({
      C18s <-
        input$C18s
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
        c(C18s, C28s, C38s, C48s, C58s, C68s, C78s, C88s)
      b8s <-
        c(V18s, V28s, V38s, V48s, V58s, V68s, V78s, V88s)
      c8s <-
        lm(a8s ~ b8s)
      coeffs8s = coefficients(c8s)
      O8s <-
        coeffs8s[1] + coeffs8s[2] * U8s
      
      print(O8s)
      
      
    })
  
  output$final28s <-
    renderTable({
      C18s <-
        input$C18s
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
        c(C18s, C28s, C38s, C48s, C58s, C68s, C78s, C88s)
      b8s <-
        c(V18s, V28s, V38s, V48s, V58s, V68s, V78s, V88s)
      
      
      c8s <- lm(a8s ~ b8s)
      coeffs8s = coefficients(c8s)
      s8s <- glance(c8s)
      t8s <- t(as.matrix(s8s))
      t8s <- as.table.default(t8s)
      t8s
      
    })
  
  output$final38s <-
    renderPlot({
      C18s <-
        input$C18s
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
        c(C18s, C28s, C38s, C48s, C58s, C68s, C78s, C88s)
      b8s <-
        c(V18s, V28s, V38s, V48s, V58s, V68s, V78s, V88s)
      
      
      c8s <- lm(a8s ~ b8s)
      coeffs8s = coefficients(c8s)
      
      
      conc.data8s <-
        data.frame(a8s = a8s, b8s = b8s)
      
      plotobj8s <- ggplot(data = conc.data8s)
      plotobj8s <-
        plotobj8s + geom_line(aes(x = a8s, y = b8s), colour = "red", size = 1)
      plotobj8s <-
        plotobj8s + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobj8s <-
        plotobj8s + scale_y_continuous("\n Spectral value ")
      print(plotobj8s)
      
    })
  
  
  output$final9s <-
    renderPrint({
      C19s <-
        input$C19s
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
        c(C19s, C29s, C39s, C49s, C59s, C69s, C79s, C89s, C99s)
      b9s <-
        c(V19s, V29s, V39s, V49s, V59s, V69s, V79s, V89s, V99s)
      
      c9s <-
        lm(a9s ~ b9s)
      coeffs9s = coefficients(c9s)
      
      O9s <-
        coeffs9s[1] + coeffs9s[2] * U9s
      
      
      print(O9s)
      
      
    })
  
  output$final29s <-
    renderTable({
      C19s <-
        input$C19s
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
        c(C19s, C29s, C39s, C49s, C59s, C69s, C79s, C89s, C99s)
      b9s <-
        c(V19s, V29s, V39s, V49s, V59s, V69s, V79s, V89s, V99s)
      
      
      c9s <- lm(a9s ~ b9s)
      coeffs9s = coefficients(c9s)
      s9s <- glance(c9s)
      t9s <- t(as.matrix(s9s))
      t9s <- as.table.default(t9s)
      t9s
      
    })
  
  output$final39s <-
    renderPlot({
      C19s <-
        input$C19s
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
        c(C19s, C29s, C39s, C49s, C59s, C69s, C79s, C89s, C99s)
      b9s <-
        c(V19s, V29s, V39s, V49s, V59s, V69s, V79s, V89s, V99s)
      
      c9s <- lm(a9s ~ b9s)
      coeffs9s = coefficients(c9s)
      
      
      conc.data9s <-
        data.frame(a9s = a9s, b9s = b9s)
      
      
      plotobj9s <- ggplot(data = conc.data9s)
      plotobj9s <-
        plotobj9s + geom_line(aes(x = a9s, y = b9s), colour = "red", size = 1)
      plotobj9s <-
        plotobj9s + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobj9s <-
        plotobj9s + scale_y_continuous("\n Spectral value ")
      print(plotobj9s)
      
    })
  
  
  output$final10s <-
    renderPrint({
      C110s <-
        input$C110s
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
          C1010s)
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
          V1010s)
      
      c10s <-
        lm(a10s ~ b10s)
      coeffs10s = coefficients(c10s)
      
      O10s <-
        coeffs10s[1] + coeffs10s[2] * U10s
      
      print(O10s)
      
      
      
    })
  
  output$final210s <-
    renderTable({
      C110s <-
        input$C110s
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
          C1010s)
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
          V1010s)
      
      
      c10s <- lm(a10s ~ b10s)
      coeffs10s = coefficients(c10s)
      s10s <- glance(c10s)
      t10s <- t(as.matrix(s10s))
      t10s <- as.table.default(t10s)
      t10s
      
    })
  
  output$final310s <-
    renderPlot({
      C110s <-
        input$C110s
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
          C1010s)
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
          V1010s)
      
      c10s <- lm(a10s ~ b10s)
      coeffs10s = coefficients(c10s)
      
      conc.data10s <-
        data.frame(a10s = a10s, b10s = b10s)
      
      
      plotobj10s <- ggplot(data = conc.data10s)
      plotobj10s <-
        plotobj10s + geom_line(aes(x = a10s, y = b10s), colour = "red", size = 1)
      plotobj10s <-
        plotobj10s + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobj10s <-
        plotobj10s + scale_y_continuous("\n Spectral value ")
      print(plotobj10s)
      
    })
  
  
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
  })
  
  
  output$table <- DT::renderDataTable({
    if (is.null(data())) {
      return ()
    }
    data()
  })
  
  
  
  output$visualdrug <- renderPlot({
    if (input$drugvis == "allvis") {
      abcvis <- data()[, 7]
      cbdvis <- table(abcvis)
      barplot(
        cbdvis,
        width = 2,
        xlim = c(0, 10),
        col =  rainbow(3),
        xlab = "Different drugs assayed in the laboratory",
        ylab = "The frequency of drugs assayed",
        main = "The frequency of different drugs assayed in laboratory"
      )
      
    } else if (input$drugvis == "phenvis") {
      print ("Under Construction")
    } else if (input$drugvis == "carbmvis") {
      print("Under Construction")
    } else {
      print("Under Construction")
    }
    
  })
  
  output$visualdrug2 <- renderPlot({
    if (input$drugvis == "allvis") {
      efgvis <- tapply(data()[, 8], data()[, 7], mean)
      barplot(
        efgvis,
        width = 2,
        xlim = c(0, 10),
        col =  rainbow(3),
        xlab = "Different drugs assayed in the laboratory",
        ylab = "Mean of the concentration in the measured unit",
        main = "The mean of concentration of different drugs assayed in the laboratory"
      )
    } else if (input$drugvis == "phenvis") {
      print ("Under Construction")
    } else if (input$drugvis == "carbmvis") {
      print("Under Construction")
    } else {
      print("Under Construction")
    }
    
  })
  
  
  
  
  
  output$visualfind2 <- renderPlot({
    if (input$drugvisat == "allvisat")
      barplot(
        data()[, 8],
        col = "black",
        main = "Concentration of drug in the different samples assayed in laboratory",
        ylab = "Values of drug conc. in measured units" ,
        xlab = "Sequential order of time"
      )
    
    else if (input$drugvisat == "phenvisat") {
      abcvisat <- data()[data()[, 7] == "phenytoin",]
      abcvisat <- abcvisat[, 8]
      barplot(
        abcvisat,
        col = "blue",
        main = "Concentration of phenytoin in the different samples assayed in laboratory",
        ylab = "Values of phenytoin conc. in measured units" ,
        xlab = "Sequential order of time"
      )
      
    } else if (input$drugvisat == "carbmvisat") {
      abcvisat <- data()[data()[, 7] == "carbamazepine",]
      abcvisat <- abcvisat[, 8]
      barplot(
        abcvisat,
        col = "red",
        main = "Concentration of carbamazepine in the different samples assayed in laboratory",
        ylab = "Values of carbamazepine conc. in measured units" ,
        xlab = "Sequential order of time"
      )
      
    } else{
      abcvisat <- data()[data()[, 7] == "phenobarbitone",]
      abcvisat <- abcvisat[, 8]
      barplot(
        abcvisat,
        col = "green",
        main = "Concentration of phenobarbitone in the different samples assayed in laboratory",
        ylab = "Values of phenobarbitone conc. in measured units" ,
        xlab = "Sequential order of time"
      )
    }
    
    
  })
  
  output$visualfind4 <- renderPlotly({
    plot_ly(
      data(),
      x = data()[, 2],
      y = data()[, 8],
      color = data()[, 7],
      size = data()[, 8]
    )
    
  })
  
  output$visualfind5 <- renderPlotly({
    plot_ly(
      data(),
      x = data()[, 6],
      y = data()[, 8],
      color = data()[, 7],
      size = data()[, 8]
    )
    
  })
  
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d))
      "Hover on a point!"
    else
      d
  })
  
  output$event2 <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d))
      "Hover on a point!"
    else
      d
  })
  
  
  output$attendrug <- renderPrint({
    if (input$drugsel == "allsel") {
      newdata <-
        data()[(data()[, 8] > input$startconc) &
                 (data()[, 8] < input$endconc) ,]
      print(newdata)
    } else if (input$drugsel == "phensel") {
      newdata <-
        data()[which(data()[, 7] == "phenytoin" &
                       (data()[, 8] > input$startconc) &
                       (data()[, 8] < input$endconc)),]
      print (newdata)
    } else if (input$drugsel == "carbmsel") {
      newdata <-
        data()[which(data()[, 7] == "carbamazepine" &
                       (data()[, 8] > input$startconc) &
                       (data()[, 8] < input$endconc)),]
      print (newdata)
    } else {
      newdata <-
        data()[which(
          data()[, 7] == "phenobarbitone" &
            (data()[, 8] > input$startconc) &
            (data()[, 8] < input$endconc)
        ),]
      print(newdata)
    }
    
  })
  
  output$meandrug <- renderPrint({
    abcmean <- data()
    summary(abcmean)
  })
  
  output$uploaded <- renderUI({
    if (is.null(data()))
      h3("R Web Server based Data Handling")
    else
      tabsetPanel(
        tabPanel
        ("Data", fluidPage(fluidRow(
          DT::dataTableOutput("table")
        ))),
        tabPanel(
          "Which patients need attention",
          selectInput(
            "drugsel",
            "Select the drug of interest",
            c(
              "All" = "allsel" ,
              "phenytoin" = "phensel",
              "carbamazepine" = "carbmsel",
              "phenobarbitone" = "phenosel"
            )
          ),
          sliderInput(
            "startconc",
            "Select the lower end of the desired concentration range",
            min = 0,
            max = 100,
            step = 0.1,
            value = 25,
            width = 1000
          ),
          sliderInput(
            "endconc",
            "Select the upper end of the desired concentration range",
            min = 0,
            max = 100,
            step = 0.1,
            value = 25,
            width = 1000
          ),
          verbatimTextOutput("attendrug")
        ),
        tabPanel(
          "Visual inspection - static",
          selectInput(
            "drugvisat",
            "Select the drug of interest",
            c(
              "All" = "allvisat" ,
              "phenytoin" = "phenvisat",
              "carbamazepine" = "carbmvisat",
              "phenobarbitone" = "phenovisat"
            )
          ),
          plotOutput("visualfind2")
        ),
        tabPanel(
          "Date-wise visual inspection - dynamic",
          fluidPage(
            plotlyOutput("visualfind4"),
            br(),
            hr(),
            verbatimTextOutput("event")
          )
        ),
        tabPanel(
          "Id-wise visual inspection - dynamic",
          fluidPage(
            plotlyOutput("visualfind5"),
            br(),
            hr(),
            verbatimTextOutput("event2")
          )
        ),
        tabPanel(
          "Visualisation",
          selectInput("drugvis", "Select the drug of interest", c("All" = "allvis")),
          plotOutput("visualdrug")
        ),
        tabPanel("Summary", verbatimTextOutput("meandrug"), width = 1000)
      )
    
  })
  
  
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
  
  output$regresspat <-
    renderPrint({
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
      Oregress <-
        coeffsregress[1] + coeffsregress[2] * Uregress2
      print(Oregress)
      
      
    })
  
  output$regresstab <- renderTable({
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
    sregress <- glance(regresseqx)
    tregress <- t(as.matrix(sregress))
    tregress <- as.table.default(tregress)
    tregress
    
    
    
  })
  
  output$regressplot <-
    renderPlot({
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
      
      
      conc.dataregressp <-
        data.frame(data13sp = data13sp, data23sp = data23sp)
      
      plotobjregressp <- ggplot(data = conc.dataregressp)
      plotobjregressp <-
        plotobjregressp + geom_line(aes(x = data13sp, y = data23sp),
                                    colour = "red",
                                    size = 1)
      plotobjregressp <-
        plotobjregressp + scale_x_continuous("Concentration of the standard (mg/ml) \n")
      plotobjregressp <-
        plotobjregressp + scale_y_continuous("\n Spectral value ")
      print(plotobjregressp)
      
    })
  
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
          
          h4(textOutput("regresspat")),
          hr(),
          
          h4(
            "Result representing the important statistics of the regression equation determined using the standard"
          ),
          
          
          h5("The statistics are tabulated here under"),
          
          br(),
          
          h4(tableOutput("regresstab")),
          hr(),
          plotOutput("regressplot", width = 1000),
          align = "center"
        )
      )
    
  })
  
  
  
  output$calcalbuminout <- renderPrint({
    phen1 <- input$concphen
    alb1 <- input$concalbumin
    corrphen <- phen1 / ((alb1 * 0.2) + 0.1)
    print(corrphen)
  })
  
  output$calcalbuminout2 <- renderPrint({
    phen2 <- input$concphen2
    alb2 <- input$concalbumin2
    corrphen2 <- phen2 / ((alb2 * 0.1) + 0.1)
    print(corrphen2)
  })
  
  
  output$albumin <- renderUI({
    if (input$crcl == "YES")
      tabsetPanel(
        "Albumin correction in phenytoin concentration",
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
      tabsetPanel(
        "Albumin correction in phenytoin concentration",
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
      
    } else {
      print("Sorry. The server could not give you interpretation")
    }
    
  })
  
  output$resultinter2 <- renderPrint({
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
  
  output$resultinter3 <- renderPrint({
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
  
  
  output$uploaded3 <- renderUI({
    if (input$druginterpret == "pheninter")
    {
      tabsetPanel(
        tabPanel(
          h4("Phenytoin"),
          hr(),
          sliderInput(
            "phenslideinter",
            "Please select the concentration of phenytoin (in microg/ml)",
            min = 0,
            max = 100,
            value = 15,
            step = 0.1,
            width = 1000
          ),
          helpText(
            "Please note that the phenytoin level which you are prompted to enter above is total pheytoin concentration"
          ),
          selectInput(
            "age1",
            "Please select the age group of patient",
            c("Children and Adults" = "adults1", "Neonates" = "neon1")
          ),
          hr(),
          h4(
            "The interpretation for the level of phenytoin in the plasma sample of patient is"
          ),
          
          h3(textOutput("resultinter1"))
        ),
        hr(),
        helpText(
          "The interpretation given is only for reference and requires clinical conformation. The server(and the associated people) is not liable legally"
        ),
        hr()
      )
    }
    else if (input$druginterpret == "carbminter")
    {
      tabsetPanel(
        tabPanel(
          h4("Carbamazepine"),
          hr(),
          sliderInput(
            "carbmslideinter",
            "Please select the concentration of carbamzepine (in mg/l)",
            min = 0,
            max = 70,
            value = 15,
            step = 0.1,
            width = 1000
          ),
          hr(),
          selectInput(
            "age2",
            "Please select the age group of patient",
            c("Children and Adults" = "adults2", "Neonates" = "neon2")
          ),
          hr(),
          h4(
            "The interpretation for the level of carbamazepine in the plasma sample of patient is"
          ),
          
          h3(textOutput("resultinter2")),
          hr(),
          helpText(
            "The interpretation given is only for reference and requires clinical conformation. The server(and the associated people) is not liable legally"
          ),
          hr()
        )
      )
    }
    else
      tabsetPanel(
        tabPanel(
          h4("Phenobarbitone"),
          hr(),
          sliderInput(
            "phenoslideinter",
            "Please select the concentration of phenobarbitone (in microg/ml)",
            min = 0,
            max = 100,
            value = 15,
            step = 0.1,
            width = 1000
          ),
          hr(),
          selectInput(
            "age3",
            "Please select the age group of patient",
            c(
              "Adults and children >5 yrs" = "adults3",
              "Infants and children < 5 years" = "neon3"
            )
          ),
          hr(),
          h4(
            "The interpretation for the level of phenobarbitone in the plasma sample of patient is"
          ),
          h3(textOutput("resultinter3")),
          hr(),
          helpText(
            "The interpretation given is only for reference and requires clinical conformation. The server(and the associated people) is not liable legally"
          ),
          hr()
        )
        
      )
    
  })
  
  
  output$uploaded4 <- renderUI({
    if (input$fillable2 == "png")
      img(src = 'pngfig.png')
    
    else if (input$fillable2 == "downloadable") {
      tags$iframe(style = "height:700px; width:100%; scrolling=yes",
                  src = "web.pdf")
      
    } else if (input$fillable2 == "fillable") {
      tags$iframe(style = "height:700px; width:100%; scrolling=yes",
                  src = "fillable.pdf")
    }
    else {
      htmlOutput("increase")
    }
    
  })
  
  
  getPage <- function() {
    return((HTML(
      readLines(
        'https://drive.google.com/drive/folders/0BwvOvYK5wRDUcmk3SGppYm0yUUE?usp=sharing'
      )
    )))
  }
  
  output$increase <- renderUI(getPage())
  
  
})
