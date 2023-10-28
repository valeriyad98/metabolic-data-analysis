###############################################
## Author: Valeriya Dontsova                ###
## Functions for generating graphs          ###
## Date: 2022-2023                          ###
###############################################

library(tidyverse)
library(ggplot2)

# define a confidence interval function that computes 95% CI for a given vector
ci <- function(x) qnorm(0.975)*sd(x)/sqrt(length(x))

# define a standard error of the mean function
se <- function(x) sqrt(var(x) / length(x))

box_plot <- function(data_table, x_vector_name, y_vector_name, x_name = x_vector_name, y_name = y_vector_name, group_order = unique(data_table$Treatment), file.loc = "output/boxplot_graph.pdf"){
    # Generates a boxplot with the data_table provided

    # Create a data table using hardcoded column names for the ggplot function
    data_table <- data_table[, c(x_vector_name, y_vector_name)]
    colnames(data_table) <- c("Treatment", "value")
    
    # Factors the x variable
    data_table$Treatment <- factor(data_table$Treatment, levels = group_order)
    
    # Set the dimensions for the output file
    pdf(file.loc, width = 6, height = 8)
    
    # Generate a ggplot2 object
    ggplotobject <- ggplot(data_table, aes(x=Treatment, y=value, color = Treatment)) + xlab(x_name) + ylab(y_name) + theme_classic() + geom_boxplot(size = 1, width = 0.8, aes(color = Treatment), outlier.shape = NA) + geom_jitter(width = 0.2, size =3.4, aes(shape = Treatment)) + theme(legend.position = 'right', axis.title = element_text(size = 20), axis.text = element_text(size = 10)) + scale_color_manual(name = 'Treatment', values = c("gray32", "red4", "navyblue"), limits = group_order) + scale_shape_manual(values=c(19, 0, 2)) + theme(axis.title = element_text(size = 25), legend.position = 'none', axis.text = element_text(size = 20), axis.line=element_line(size=1.1))
    
    # print object to file
    print(ggplotobject)
    dev.off()
}

line_graph <- function(data_table, x_vector_name, y_vector_name, group_vector_name, x_name = x_vector_name, y_name = y_vector_name, group_order = unique(mean_values$Treatment), file.loc = "output/line_graph.pdf"){
    # Generates a linegraph with the data_table provided

    # Create a table of mean values with a 95% CI for a vector of y values split based on group_vector and x_vector
    mean_values <- data.frame()
    for(i in unique(data_table[, x_vector_name])){
        for(j in unique(data_table[, group_vector_name])){
            y_vector <- data_table[data_table[,x_vector_name] == i & data_table[,group_vector_name] == j, y_vector_name]
            new_row <- data.frame(Treatment = j, x = i, y = mean(y_vector), ci = ci(y_vector))
            mean_values <- rbind(mean_values, new_row)
        }
    }
    
    # Factors the group variable
    mean_values$Treatment <- factor(mean_values$Treatment, levels = group_order)
    
    # Set the dimensions for the output file
    pdf(file.loc, width = 8, height = 8)
    
    # Generate a ggplot2 object
    ggplotobject <- ggplot(mean_values, aes(x=x, y=y, group = Treatment, color = Treatment)) + geom_line(size=1.0) + xlab(x_name) + ylab(y_name) + theme_classic() + theme(legend.position = 'right', axis.title = element_text(size = 20), axis.text = element_text(size = 10)) + scale_color_manual(name = "Treatment", values = c("gray32", "red4", "navyblue"), limits = group_order) + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), size = 0.9, width=0.3, alpha = 0.9) + geom_point(size=4, aes(shape = Treatment)) + scale_shape_manual(values=c(19, 0, 2)) + theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20), axis.line=element_line(size=1.1), legend.title = element_text(size=25), legend.text = element_text(size=20))
    
    # print object to file
    print(ggplotobject)
    dev.off()
}
