#    "Shyam's Chromatography & TDM Server - Estimation Compilation Evaluation Reutlisation Epitransformation Interpretation (CTS-ECEREI)"
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


dataglobal <- reactiveValues()
dataalbumin<-reactiveValues()
downloadstandard<-read.csv("standard2017-08-062017-08-06 17-45-50.csv")
downloadtemplate<-read.csv("template file.csv")
downloadtemplate2<-read.csv("chromatography2017-08-062017-08-06 19-36-06.csv")
downloadtemplate3<-read.csv("chromatography2017-08-062017-08-06 19-36-06.csv")
downloadtemplate4<-read.csv("larger database.csv")
