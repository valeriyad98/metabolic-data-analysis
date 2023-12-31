###############################################
## Author: Valeriya Dontsova                ###
## Driver file for analyzing metabolic data ###
## Date: 2022-2023                          ###
###############################################

# takes in argument from command line and calls on functions from utlities files
# getwd() must be the directory where the R code files are located for the functions to run

# Sources the utilities file that contains functions that perform stat analysis on the data.
stat_analysis_source <- "stat_analysis.R"
graph_source <- "graph_functions.R"

if(!file.exists(stat_analysis_source) | !file.exists(graph_source)){
  stop("User not in the right working directory. Use setwd() to set correct directory")
}
source(stat_analysis_source)
source(graph_source)

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
# read data from the file provided and save the data as met_data_table
met_data_table <- read.csv(file_name)

# save tables of normality of data tests and hypothesis testing
write.csv(rec.sw.test(met_data_table, c("time", "analyte", "treatment"), "fold.change"), "output/normality_of_data.csv")
write.csv(automated.stat.test(met_data_table, c("time", "analyte"), "fold.change", "treatment"), "output/hypothesis_testing.csv")

# graphing examples
box_plot(met_data_table[met_data_table$time == 6 & met_data_table$analyte == "Corticosterone",], "treatment", "fold.change", "Treatment", "Fold change", c("control", "trt1", "trt2"))
line_graph(met_data_table[met_data_table$analyte == "Leptin",], "time", "conc", "treatment", "Time", "Leptin (ng/ml)", c("control", "trt1", "trt2"))
