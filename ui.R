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





fluidPage(
  hr(),
  img(src = 'Untitled-2-01.png', align = "right", width = 300),
  h2(
    "Shyam's Chromatography & TDM Server - Estimation Compilation Evaluation Reutlisation Furtherance Interpretation (CTS-ECERFI)"
  ),
  hr(),
  
  fluidRow(mainPanel(
    tabsetPanel(
      tabPanel(
        "Estimation of Results",
        
        
        tabsetPanel(
          tabPanel(
            "No. of standard = 3",
            
            
            hr(),
            h4(
              "Please enter the details of the concentration and spectral value of standard for current working day"
            ),
            
            h6("Please use tab button in keyboard for swift entering of values"),
            hr(),
            
            
            br(),
            align = "center",
            
            
            fluidRow(
              column(
                6,
                
                sliderInput(
                  "C13s",
                  "Concentration of the standard 1",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C23s",
                  "Concentration of the standard 2",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C33s",
                  "Concentration of the standard 3",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                
                align = "center"
                
              ),
              
              column(
                6,
                
                br(),
                
                numericInput(
                  "V13s",
                  "Spectral Value for the standard 1",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V23s",
                  "Spectral Value for the standard 2",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V33s",
                  "Spectral Value for the standard 3",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                
                align = "center"
                
                
              ),
              
              fluidRow(
                h5("Press the download button for saving the data of standard"),
                downloadButton('downloadData3', 'Download'),
                hr()
              ),
              
              fluidRow(
                numericInput(
                  "U3s",
                  "The spectra value as calculated through chromatographic method for the patient sample",
                  value = 1,
                  min = NA,
                  max = NA,
                  step = NA,
                  width = NULL
                ),
                
                
                hr(),
                
                column(
                  12,
                  h4(
                    "Result denoting the concentration of drug in the given sample as determined by Chromatographic technique"
                  ),
                  
                  
                  
                  h4(
                    "The calculated concentration of the drug in patient biological sample is"
                  ),
                  
                  br(),
                  
                  h4(textOutput("final3s")),
                  hr(),
                  align = "center"
                )
              ),
              
              
              fluidRow(
                h4(
                  "Result representing the important statistics of the regression equation determined using the standard"
                ),
                
                
                h5("The statistics are tabulated here under"),
                br(),
                h4(tableOutput("final23s")),
                align = "center"
              ),
              
              fluidRow(
                hr(),
                h4(
                  "The plot of the concentration and spectral value of standard constructed in real time"
                ),
                plotOutput("final33s"),
                
                hr()
                
                
              )
              
            )
          ),
          
          tabPanel(
            "No of Standard = 4",
            
            hr(),
            h4(
              "Please enter the details of the concentration and spectral value of standard for current working day"
            ),
            h6("Please use tab button in keyboard for swift entering of values"),
            hr(),
            
            
            br(),
            align = "center",
            
            
            fluidRow(
              column(
                6,
                
                sliderInput(
                  "C14s",
                  "Concentration of the standard 1",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C24s",
                  "Concentration of the standard 2",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C34s",
                  "Concentration of the standard 3",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C44s",
                  "Concentration of the standard 4",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                
                
                align = "center"
                
              ),
              
              column(
                6,
                br(),
                
                numericInput(
                  "V14s",
                  "Spectral Value for the standard 1",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V24s",
                  "Spectral Value for the standard 2",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V34s",
                  "Spectral Value for the standard 3",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                
                numericInput(
                  "V44s",
                  "Spectral Value for the standard 4",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                align = "center"
                
                
              ),
              
              fluidRow(
                h5("Press the download button for saving the data of standard"),
                downloadButton('downloadData4', 'Download'),
                hr()
              ),
              
              fluidRow(
                numericInput(
                  "U4s",
                  "The spectra value as calculated through chromatographic method for the patient sample",
                  value = 1,
                  min = NA,
                  max = NA,
                  step = NA,
                  width = NULL
                ),
                
                hr(),
                column(
                  12,
                  h4(
                    "Result denoting the concentration of drug in the given sample as determined by Chromatographic technique"
                  ),
                  
                  
                  
                  h4(
                    "The calculated concentration of the drug in patient biological sample is"
                  ),
                  br(),
                  h4(textOutput("final4s")),
                  hr(),
                  align = "center"
                )
              ),
              
              
              fluidRow(
                h4(
                  "Result representing the important statistics of the regression equation determined using the standard"
                ),
                
                
                h5("The statistics are tabulated here under"),
                br(),
                h4(tableOutput("final24s")),
                align = "center"
              ),
              fluidRow(
                hr(),
                h4(
                  "The plot of the concentration and spectral value of standard constructed in real time"
                ),
                plotOutput("final34s"),
                hr()
                
                
              )
              
            )
          ),
          
          tabPanel(
            "No of Standard = 5",
            hr(),
            h4(
              "Please enter the details of the concentration and spectral value of standard for current working day"
            ),
            h6("Please use tab button in keyboard for swift entering of values"),
            
            hr(),
            
            
            br(),
            align = "center",
            
            
            
            fluidRow(
              column(
                6,
                
                
                sliderInput(
                  "C15s",
                  "Concentration of the standard 1",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C25s",
                  "Concentration of the standard 2",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C35s",
                  "Concentration of the standard 3",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C45s",
                  "Concentration of the standard 4",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C55s",
                  "Concentration of the standard 5",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                
                align = "center"
                
              ),
              
              column(
                6,
                
                br(),
                
                numericInput(
                  "V15s",
                  "Spectral Value for the standard 1",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V25s",
                  "Spectral Value for the standard 2",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V35s",
                  "Spectral Value for the standard 3",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                
                numericInput(
                  "V45s",
                  "Spectral Value for the standard 4",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V55s",
                  "Spectral Value for the standard 5",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                align = "center"
                
                
              ),
              
              fluidRow(
                h5("Press the download button for saving the data of standard"),
                downloadButton('downloadData5', 'Download'),
                hr()
              ),
              
              
              fluidRow(
                numericInput(
                  "U5s",
                  "The spectra value as calculated through chromatographic method for the patient sample",
                  value = 1,
                  min = NA,
                  max = NA,
                  step = NA,
                  width = NULL
                ),
                
                hr(),
                column(
                  12,
                  h4(
                    "Result denoting the concentration of drug in the given sample as determined by Chromatographic technique"
                  ),
                  
                  
                  
                  h4(
                    "The calculated concentration of the drug in patient biological sample is"
                  ),
                  br(),
                  h4(textOutput("final5s")),
                  hr(),
                  align = "center"
                )
              ),
              
              fluidRow(
                h4(
                  "Result representing the important statistics of the regression equation determined using the standard"
                ),
                
                
                h5("The statistics are tabulated here under"),
                br(),
                h4(tableOutput("final25s")),
                align = "center"
              ),
              
              fluidRow(
                hr(),
                h4(
                  "The plot of the concentration and spectral value of standard constructed in real time"
                ),
                plotOutput("final35s"),
                hr()
                
                
              )
              
            )
          ),
          
          tabPanel(
            "No of Standard = 6",
            hr(),
            h4(
              "Please enter the details of the concentration and spectral value of standard for current working day"
            ),
            h6("Please use tab button in keyboard for swift entering of values"),
            hr(),
            
            
            br(),
            align = "center",
            
            
            fluidRow(
              column(
                6,
                
                sliderInput(
                  "C16s",
                  "Concentration of the standard 1",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C26s",
                  "Concentration of the standard 2",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C36s",
                  "Concentration of the standard 3",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C46s",
                  "Concentration of the standard 4",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C56s",
                  "Concentration of the standard 5",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C66s",
                  "Concentration of the standard 6",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                
                align = "center"
                
              ),
              
              column(
                6,
                br(),
                
                numericInput(
                  "V16s",
                  "Spectral Value for the standard 1",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V26s",
                  "Spectral Value for the standard 2",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V36s",
                  "Spectral Value for the standard 3",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                
                numericInput(
                  "V46s",
                  "Spectral Value for the standard 4",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V56s",
                  "Spectral Value for the standard 5",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V66s",
                  "Spectral Value for the standard 6",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                align = "center"
                
                
              ),
              
              
              fluidRow(
                h5("Press the download button for saving the data of standard"),
                downloadButton('downloadData6', 'Download'),
                hr()
              ),
              
              
              fluidRow(
                numericInput(
                  "U6s",
                  "The spectra value as calculated through chromatographic method for the patient sample",
                  value = 1,
                  min = NA,
                  max = NA,
                  step = NA,
                  width = NULL
                ),
                
                hr(),
                column(
                  12,
                  h4(
                    "Result denoting the concentration of drug in the given sample as determined by Chromatographic technique"
                  ),
                  
                  
                  
                  h4(
                    "The calculated concentration of the drug in patient biological sample is"
                  ),
                  br(),
                  h4(textOutput("final6s")),
                  hr(),
                  align = "center"
                )
              ),
              
              
              fluidRow(
                h4(
                  "Result representing the important statistics of the regression equation determined using the standard"
                ),
                
                
                h5("The statistics are tabulated here under"),
                br(),
                h4(tableOutput("final26s")),
                align = "center"
              ),
              
              fluidRow(
                hr(),
                h4(
                  "The plot of the concentration and spectral value of standard constructed in real time"
                ),
                plotOutput("final36s"),
                hr()
                
                
              )
              
              
            )
          ),
          
          tabPanel(
            "No of Standard = 7",
            hr(),
            h4(
              "Please enter the details of the concentration and spectral value of standard for current working day"
            ),
            h6("Please use tab button in keyboard for swift entering of values"),
            hr(),
            
            
            br(),
            align = "center",
            
            
            fluidRow(
              column(
                6,
                
                sliderInput(
                  "C17s",
                  "Concentration of the standard 1",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C27s",
                  "Concentration of the standard 2",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C37s",
                  "Concentration of the standard 3",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C47s",
                  "Concentration of the standard 4",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C57s",
                  "Concentration of the standard 5",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C67s",
                  "Concentration of the standard 6",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C77s",
                  "Concentration of the standard 7",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                
                align = "center"
                
              ),
              
              column(
                6,
                br(),
                
                numericInput(
                  "V17s",
                  "Spectral Value for the standard 1",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V27s",
                  "Spectral Value for the standard 2",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V37s",
                  "Spectral Value for the standard 3",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                
                numericInput(
                  "V47s",
                  "Spectral Value for the standard 4",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V57s",
                  "Spectral Value for the standard 5",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V67s",
                  "Spectral Value for the standard 6",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V77s",
                  "Spectral Value for the standard 7",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                align = "center"
                
                
              ),
              
              fluidRow(
                h5("Press the download button for saving the data of standard"),
                downloadButton('downloadData7', 'Download'),
                hr()
              ),
              
              
              fluidRow(
                numericInput(
                  "U7s",
                  "The spectra value as calculated through chromatographic method for the patient sample",
                  value = 1,
                  min = NA,
                  max = NA,
                  step = NA,
                  width = NULL
                ),
                
                hr(),
                column(
                  12,
                  h4(
                    "Result denoting the concentration of drug in the given sample as determined by Chromatographic technique"
                  ),
                  
                  
                  
                  h4(
                    "The calculated concentration of the drug in patient biological sample is"
                  ),
                  br(),
                  h4(textOutput("final7s")),
                  hr(),
                  align = "center"
                )
              ),
              
              fluidRow(
                h4(
                  "Result representing the important statistics of the regression equation determined using the standard"
                ),
                
                
                h5("The statistics are tabulated here under"),
                br(),
                h4(tableOutput("final27s")),
                align = "center"
              ),
              
              fluidRow(
                hr(),
                h4(
                  "The plot of the concentration and spectral value of standard constructed in real time"
                ),
                plotOutput("final37s"),
                hr()
                
                
              )
              
            )
          ),
          
          tabPanel(
            "No of Standard = 8",
            hr(),
            h4(
              "Please enter the details of the concentration and spectral value of standard for current working day"
            ),
            h6("Please use tab button in keyboard for swift entering of values"),
            hr(),
            
            
            br(),
            align = "center",
            
            
            fluidRow(
              column(
                6,
                
                
                sliderInput(
                  "C18s",
                  "Concentration of the standard 1",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C28s",
                  "Concentration of the standard 2",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C38s",
                  "Concentration of the standard 3",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C48s",
                  "Concentration of the standard 4",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C58s",
                  "Concentration of the standard 5",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C68s",
                  "Concentration of the standard 6",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C78s",
                  "Concentration of the standard 7",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C88s",
                  "Concentration of the standard 8",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                
                
                align = "center"
                
              ),
              
              column(
                6,
                br(),
                
                numericInput(
                  "V18s",
                  "Spectral Value for the standard 1",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V28s",
                  "Spectral Value for the standard 2",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V38s",
                  "Spectral Value for the standard 3",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                
                numericInput(
                  "V48s",
                  "Spectral Value for the standard 4",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V58s",
                  "Spectral Value for the standard 5",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V68s",
                  "Spectral Value for the standard 6",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V78s",
                  "Spectral Value for the standard 7",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V88s",
                  "Spectral Value for the standard 8",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                align = "center"
                
                
              ),
              
              
              fluidRow(
                h5("Press the download button for saving the data of standard"),
                downloadButton('downloadData8', 'Download'),
                hr()
              ),
              
              fluidRow(
                numericInput(
                  "U8s",
                  "The spectra value as calculated through chromatographic method for the patient sample",
                  value = 1,
                  min = NA,
                  max = NA,
                  step = NA,
                  width = NULL
                ),
                
                hr(),
                column(
                  12,
                  h4(
                    "Result denoting the concentration of drug in the given sample as determined by Chromatographic technique"
                  ),
                  
                  
                  
                  h4(
                    "The calculated concentration of the drug in patient biological sample is"
                  ),
                  br(),
                  h4(textOutput("final8s")),
                  hr(),
                  align = "center"
                )
              ),
              
              
              fluidRow(
                h4(
                  "Result representing the important statistics of the regression equation determined using the standard"
                ),
                
                
                h5("The statistics are tabulated here under"),
                br(),
                h4(tableOutput("final28s")),
                align = "center"
              ),
              
              fluidRow(
                hr(),
                h4(
                  "The plot of the concentration and spectral value of standard constructed in real time"
                ),
                plotOutput("final38s"),
                hr()
                
                
              )
            )
          ),
          
          
          tabPanel(
            "No of Standard = 9",
            hr(),
            h4(
              "Please enter the details of the concentration and spectral value of standard for current working day"
            ),
            h6("Please use tab button in keyboard for swift entering of values"),
            hr(),
            
            
            br(),
            align = "center",
            
            
            fluidRow(
              column(
                6,
                
                sliderInput(
                  "C19s",
                  "Concentration of the standard 1",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C29s",
                  "Concentration of the standard 2",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C39s",
                  "Concentration of the standard 3",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C49s",
                  "Concentration of the standard 4",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C59s",
                  "Concentration of the standard 5",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C69s",
                  "Concentration of the standard 6",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C79s",
                  "Concentration of the standard 7",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C89s",
                  "Concentration of the standard 8",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                
                sliderInput(
                  "C99s",
                  "Concentration of the standard 9",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                
                align = "center"
                
              ),
              
              
              column(
                6,
                br(),
                
                numericInput(
                  "V19s",
                  "Spectral Value for the standard 1",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V29s",
                  "Spectral Value for the standard 2",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V39s",
                  "Spectral Value for the standard 3",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                
                numericInput(
                  "V49s",
                  "Spectral Value for the standard 4",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V59s",
                  "Spectral Value for the standard 5",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V69s",
                  "Spectral Value for the standard 6",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V79s",
                  "Spectral Value for the standard 7",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V89s",
                  "Spectral Value for the standard 8",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V99s",
                  "Spectral Value for the standard 9",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                align = "center"
                
                
              ),
              
              fluidRow(
                h5("Press the download button for saving the data of standard"),
                downloadButton('downloadData9', 'Download'),
                hr()
              ),
              
              
              fluidRow(
                numericInput(
                  "U9s",
                  "The spectra value as calculated through chromatographic method for the patient sample",
                  value = 1,
                  min = NA,
                  max = NA,
                  step = NA,
                  width = NULL
                ),
                
                hr(),
                column(
                  12,
                  h4(
                    "Result denoting the concentration of drug in the given sample as determined by Chromatographic technique"
                  ),
                  
                  
                  
                  h4(
                    "The calculated concentration of the drug in patient biological sample is"
                  ),
                  br(),
                  h4(textOutput("final9s")),
                  hr(),
                  align = "center"
                )
              ),
              
              
              fluidRow(
                h4(
                  "Result representing the important statistics of the regression equation determined using the standard"
                ),
                
                
                h5("The statistics are tabulated here under"),
                br(),
                h4(tableOutput("final29s")),
                align = "center"
              ),
              
              fluidRow(
                hr(),
                h4(
                  "The plot of the concentration and spectral value of standard constructed in real time"
                ),
                plotOutput("final39s"),
                hr()
                
                
              )
              
            )
          ),
          
          tabPanel(
            "No of Standard = 10",
            hr(),
            h4(
              "Please enter the details of the concentration and spectral value of standard for current working day"
            ),
            h6("Please use tab button in keyboard for swift entering of values"),
            hr(),
            
            
            br(),
            align = "center",
            
            
            fluidRow(
              column(
                6,
                
                sliderInput(
                  "C110s",
                  "Concentration of the standard 1",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C210s",
                  "Concentration of the standard 2",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C310s",
                  "Concentration of the standard 3",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C410s",
                  "Concentration of the standard 4",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C510s",
                  "Concentration of the standard 5",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C610s",
                  "Concentration of the standard 6",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C710s",
                  "Concentration of the standard 7",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                br(),
                
                sliderInput(
                  "C810s",
                  "Concentration of the standard 8",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                
                sliderInput(
                  "C910s",
                  "Concentration of the standard 9",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                
                sliderInput(
                  "C1010s",
                  "Concentration of the standard 10",
                  min = 0,
                  max = 400,
                  value = 2,
                  step = 2
                ),
                
                align = "center"
                
              ),
              
              
              column(
                6,
                br(),
                
                numericInput(
                  "V110s",
                  "Spectral Value for the standard 1",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V210s",
                  "Spectral Value for the standard 2",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V310s",
                  "Spectral Value for the standard 3",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                
                numericInput(
                  "V410s",
                  "Spectral Value for the standard 4",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V510s",
                  "Spectral Value for the standard 5",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V610s",
                  "Spectral Value for the standard 6",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V710s",
                  "Spectral Value for the standard 7",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V810s",
                  "Spectral Value for the standard 8",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V910s",
                  "Spectral Value for the standard 9",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                numericInput(
                  "V1010s",
                  "Spectral Value for the standard 10",
                  min = 0,
                  max = NA,
                  value = 1,
                  step = NA
                ),
                br(),
                br(),
                
                
                align = "center"
                
                
              ),
              
              
              fluidRow(
                h5("Press the download button for saving the data of standard"),
                downloadButton('downloadData10', 'Download'),
                hr()
              ),
              
              
              fluidRow(
                numericInput(
                  "U10s",
                  "The spectra value as calculated through chromatographic method for the patient sample",
                  value = 1,
                  min = NA,
                  max = NA,
                  step = NA,
                  width = NULL
                ),
                
                hr(),
                column(
                  12,
                  h4(
                    "Result denoting the concentration of drug in the given sample as determined by Chromatographic technique"
                  ),
                  
                  
                  
                  h4(
                    "The calculated concentration of the drug in patient biological sample is"
                  ),
                  br(),
                  h4(textOutput("final10s")),
                  hr(),
                  align = "center"
                )
              ),
              
              fluidRow(
                h4(
                  "Result representing the important statistics of the regression equation determined using the standard"
                ),
                
                
                h5("The statistics are tabulated here under"),
                
                br(),
                h4(tableOutput("final210s")),
                align = "center"
              ),
              
              fluidRow(
                h4(
                  "The plot of the concentration and spectral value of standard constructed in real time"
                ),
                plotOutput("final310s"),
                hr()
                
                
              )
              
            )
          )
          
        )
      ),
      
      tabPanel(
        "Compilation of Results",
        
        hr(),
        column(
          4,
          checkboxInput("Final", "Final Result???", TRUE),
          sliderInput(
            "Standards",
            "Number of standard's used for assaying",
            3,
            10,
            3,
            step = 1,
            ticks = FALSE
          ),
          textInput("Name", "Name of the sample", ""),
          selectInput(
            "Drug",
            "Name of the drug assayed",
            c(
              "Phenytoin" = "phenytoin",
              "Carbamazepine" = "carbamazepine",
              "Phenobarbitone" = "phenobarbitone",
              "Others" = "others"
            )
          ),
          hr(),
          helpText(
            "Pl note that the 'evaluation of results' section will not include drugs coded as 'others' in the current version"
          ),
          hr(),
          numericInput(
            "ID",
            "Identification number of sample",
            value = 1,
            min = NA,
            max = NA,
            step = NA,
            width = NULL
          ),
          numericInput(
            "Conc",
            "Enter the concentration of drug in patient sample",
            value = 1,
            min = NA,
            max = NA,
            step = NA,
            width = NULL
          ),
          selectInput(
            inputId = "Time&date",
            label = "Time stamp",
            choices = as.character(Sys.time())
          ),
          
          br(),
          actionButton("submit", "Submit"),
          hr(),
          
          h4("Download the database in csv format"),
          helpText("Dont forget to take periodic backup of your data to avoid dataloss"),
          downloadButton('downloadData', 'Download'),
          hr() ,
          h4(
            "This is an example file for the demonstration of format of the downloaded file above"
          ),
          helpText(
            "If you want to review the file in this server, reupload the file in 'Evaluation of results' section"
          ),
          hr(),
          downloadButton('downloadDatatemplate2', 'Example'),
          hr(),
          align = "left"
        ),
        
        column(
          6,
          h3("Database of all the reports from the laboratory"),
          DT::dataTableOutput("response", width = 700),
          tags$hr(),
          align = "left"
        ),
        
        
        hr()
        
        
        
      ),
      tabPanel(
        "Evaluation of Results",
        
        
        sidebarLayout(
          sidebarPanel(
            fileInput("file", "Please Upload the file"),
            
            h5(
              helpText(
                "Upload the csv file created from the 'Compilation of Results' \n section for better results"
              )
            ),
            tags$hr(),
            checkboxInput(
              inputId = 'header',
              label = 'Header in the file',
              value = TRUE
            ),
            br(),
            radioButtons(
              inputId = 'sep',
              label = 'Separator in the file',
              choices = c(Comma = ','),
              selected = ','
            ),
            hr(),
            h4(
              "This is an example file for understanding the format of file to be uploaded in the upload tab above"
            ),
            hr(),
            downloadButton('downloadDatatemplate3', 'Example'),
            hr(),
            h4("Instructions for uploading"),
            helpText("1. Please upload the file without manipulation"),
            helpText(
              "2. If you wish to manipulate, follow the same pattern as given in the file"
            ),
            helpText(
              "3. Dont change the format of the file. The file which is to be uploaded back to the server should be in csv format"
            ),
            helpText(
              "4. The downloaded file can be found in the default download folder of the computer"
            ),
            helpText(
              "5. Warning:Please follow the above instructions strictly for reliable results"
            ),
            hr(),
            h4(
              "This is an example file containing a larger database of 1017 patients"
            ),
            helpText("This file is for demonstrating the flexibility of the server"),
            hr(),
            downloadButton('downloadDatatemplate4', 'Larger Database')
            
          ),
          mainPanel(uiOutput("uploaded"))
          
          
          
          
        )
      ),
      tabPanel(
        "Reutilisation of Results",
        
        sidebarLayout(
          sidebarPanel(
            fileInput("file2", "Please Upload the file"),
            
            h5(
              helpText(
                "Warning: Upload the csv file created from the 'chromatographic estimation' \n section alone"
              )
            ),
            tags$hr(),
            checkboxInput(
              inputId = 'header2',
              label = 'Header in the file',
              value = TRUE
            ),
            br(),
            radioButtons(
              inputId = 'sep2',
              label = 'Separator in the file',
              choices = c(Comma = ','),
              selected = ','
            ),
            hr(),
            h4(
              "Below is the template file for entering the value using excel software. After entering the values, upload the file back to the server via the upload tab above for calculation"
            ),
            hr(),
            h4("Instructions for entering data in template file"),
            helpText(
              "1. Enter the concentration of standard under the 'Concentration' column"
            ),
            helpText(
              "2. Enter the spectral value of standard under the 'Spectral Value' column"
            ),
            helpText(
              "3. Delete the unentered rows completely. Identified as rows containing 'NA' in the downloaded template file"
            ),
            helpText(
              "4. Dont change the format of the file. The file which is to be uploaded back to the server after manipulation should be in csv format"
            ),
            helpText(
              "5. The downloaded file can be found in the default download folder of the computer"
            ),
            helpText(
              "6. Warning:Please follow the above instructions strictly for reliable results"
            ),
            hr(),
            downloadButton('downloadDatatemplate', 'Template File'),
            hr(),
            h5(
              "This is an example file for understanding the format of filling in the template file"
            ),
            downloadButton('downloadDataexample', 'Example File'),
            helpText(
              "Please download the file and then upload again to the server via the upload tab above"
            ),
            helpText(
              "Alternatively, you can also visualize the file using csv reader such as Excel or Libreoffice"
            ),
            hr()
          ),
          mainPanel(uiOutput("uploaded2"))
          
          
          
          
          
        )
      ),
      tabPanel(
        "Furtherance of Results",
        h4("Albumin Correction in Phenytoin concentration"),
        
        sidebarLayout(sidebarPanel(
          selectInput(
            "crcl",
            "Creatinine clearance greater than 20 ml/min",
            c("YES", "NO")
          )
        ),
        mainPanel(uiOutput("albumin")))
        
      ),
      
      
      
      tabPanel(
        "Interpretation of Results",
        sidebarLayout(sidebarPanel(
          selectInput(
            "druginterpret",
            "Please select the drug for which interpretation is required",
            c(
              "Phenytoin" = "pheninter",
              "Carbamazepine" =
                "carbminter",
              "Phenobarbitone" =
                "phenointer"
            )
          )
        ),
        mainPanel(uiOutput("uploaded3")))
      ),
      tabPanel(
        "Guidance for the workflow in server",
        sidebarLayout(
          sidebarPanel(
            h4(
              "The workflow depicted on the right will guide you in utlisation of server"
            ),
            hr(),
            helpText(
              "Feel free to drop a mail at contactdynamicprotocol@gmail.com for any query or technical assitance."
            ),
            hr()
          ),
          mainPanel(
            "Help Document",
            
            img(style = "height:800px; width:100%; scrolling=yes", src =
                  'ref.png', align = "center")
          )
        )
      ),
      tabPanel(
        "Reporting Template",
        h4(
          "This section is intended to assist you in report generation in your laboratory"
        ),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "fillable2",
              "Report form required",
              choices = c(
                "PNG Image of the Template" = "png",
                "Downloadable Report Form" = "downloadable",
                "Fillable Report Form" = "fillable",
                "Resources Folder" = "resource"
              ),
              selected = "png"
            ),
            helpText("Instructions:"),
            helpText(
              "1. Due to complexities involved in report generation using shiny for hospital utilisation, manual report on a predefined
              tempate is preferred"
            ),
            helpText(
              "2. Use 'Downloadble Report Form' to manually fill after printing the form"
            ),
            helpText(
              "3. Use 'Fillable Report Form' to fill the form and then print the form"
            ),
            helpText("4. You can copy paste details into the 'Fillable Report Form'"),
            helpText(
              "If you wish to customise the downloadble report form further, the illustrator file can be downloaded from the 'Resource Folder'"
            ),
            width = 600
            ),
          mainPanel(uiOutput("uploaded4"))
      )
      )
      
      )
  ))
)
