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

#4 types of functions

box_plot <- function(data_table, x_vector_name, y_vector_name, x_name = x_vector_name, y_name = y_vector_name, group_order = unique(data_table$Treatment), file.loc = "graphs/new_graph.pdf"){
    # Generates a boxplot with the data_table provided

    # Create a data table using hardcoded column names for the ggplot function
    data_table <- data_table[, c(x_vector_name, y_vector_name)]
    colnames(data_table) <- c("Treatment", "value")
    
    # Factors the x variable
    data_table$Treatment <- factor(data_table$Treatment, levels = group_order)

    pdf(file.loc, width = 8, height = 8)
    ggplotobject <- ggplot(data_table, aes(x=Treatment, y=value, color = Treatment)) + xlab(x_name) + ylab(y_name) + theme_classic() + geom_boxplot(size = 1, width = 0.8, aes(color = Treatment), outlier.shape = NA) + geom_jitter(width = 0.2, size =3.4, aes(shape = Treatment)) + theme(legend.position = 'right', axis.title = element_text(size = 20), axis.text = element_text(size = 10)) + scale_color_manual(name = 'Treatment', values = c("gray32", "red4", "navyblue"), limits = group_order) + scale_shape_manual(values=c(19, 0, 2)) + theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20), axis.line=element_line(size=1.1))
    print(ggplotobject)
    dev.off()
}
