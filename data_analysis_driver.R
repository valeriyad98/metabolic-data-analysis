###############################################
## Author: Valeriya Dontsova                ###
## Driver file for analyzing metabolic data ###
## Date: 2022-2023                          ###
###############################################

# takes in argument from command line and calls on functions from utlities files
# getwd() must be the directory where the R code files are located for the functions to run

# Sources the utilities file that contains functions that perform stat analysis on the data.
stat_analysis_source <- "stat_analysis.R"

if(!file.exists(stat_analysis_source)){
  stop("User not in the right working directory. Use setwd() to set correct directory")
}
source(stat_analysis_source)

# get the name of the file to analyze from the command line
file_name <- commandArgs(trailingOnly = TRUE)

# if the user did not provide input or file_name does not reference an existing file, stop the program
# Checks if no input from command line given
if(length(file_name) == 0){
  stop("We require a filename to process as a command line argument.")
}
# Checks if input given does not match any files
if(!file.exists(file_name)){
  stop("Filename provided does not match any files.")
}

# file name exists and input was given, can proceed to call the functions
# read data from the file provided and save the data as data_table
data_table <- read.csv(file_name)