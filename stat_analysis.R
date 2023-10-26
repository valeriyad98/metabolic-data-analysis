###############################################
## Author: Valeriya Dontsova                ###
## Statistical analysis file                ###
## Date: 2022-2023                          ###
###############################################

# number of unique parameters i.e time, treatment etc

# shapiro wilk test for normality
normality.sw <- function(norm_vector, p.value = 0.05){
    # Uses norm_vector as input for the Shapiro-Wilk test for normality of data.
    # norm_vector: vector of values
    # p.value indicates the probability threshold for assigning normality to a given distribution
    # The function returns a vector of p-value, test-statistic, and normality
    
    # Shapiro-Wilk test
    sw.test <- shapiro.test(norm_vector)
    
    # Determining if norm_vector has a normal distribution
    normal.dist <- 'no'
    if(sw.test$p.value > p.value){normal.dist <- 'yes'}
    
    # Returning p-value, test-statistic, and normality of the norm_vector
    return(c(sw.test$p.value, sw.test$statistic, normal.dist))
}

anova.test <- function(test_vector, value_groups, lin.m = test_vector ~ value_groups){
    # Uses test_vector containing data split according to value_groups for analysis of variance (ANOVA)
    # test_vector: vector of values
    # value_groups: vector of corresponding group assignments for test_vector values
    # lin.m: linear model for anova computation for optional addition of other parameters
    # i.e. test_vector = c(1,2,1,1) and value_groups = c('a', 'a', 'b', 'b')
    
    # Checks that number of groups is enough for ANOVA and test_vector matches the length of value_groups
    if(length(unique(value_groups)) < 3 | length(test_vector) != length(value_groups)){
        stop("ANOVA testing requires three or more groups for comparison and data vector must be of the same length as group allocation vector.")
    }

    # Return anova test results
    return(summary(aov(lin.m))[[1]][1,5])
}

kw.test <- function(test_vector, value_groups, lin.m = test_vector ~ value_groups){
    # Uses test_vector containing data split according to value_groups for Kruskal-Wallis test
    # test_vector: vector of values
    # value_groups: vector of corresponding group assignments for test_vector values
    # lin.m: linear model for KW computation for optional addition of other parameters
    # i.e. test_vector = c(1,2,1,1) and value_groups = c('a', 'a', 'b', 'b')
    
    # Return KW test results
    return(kruskal.test(lin.m)$p.value)
}

rec.sw.test <- function(data_table, parmtrs, norm_vector_name, p.value = 0.05){
    # Uses data_table as input for analysis, stratifying based on the unique values in
    # each column specified in parmtrs. norm_vector_name indicates the column of values
    # for the Shapiro-Wilk test for normality of data. Function recursively determines every
    # combination of unique vectors for SW test based on specified columns in parmtrs
    # data_table: data frame
    # parmtrs: vector of column names
    # norm_vector_name: String
    # p.value indicates the probability threshold for assigning normality to a given distribution
    
    # Checks if given colnames in parmtrs and norm_vector_name are in the data_table
    if(sum(c(parmtrs, norm_vector_name) %in% colnames(data_table)) <= length(parmtrs)){
        stop("Column names given are not in the data table.")
    }
    
    # Initialize a return table
    return_table <- data.frame()
    
    # No need to stratify data
    if(length(parmtrs) < 1){
        # Call the SW function to calculate normality and return results
        norm_stat <- normality.sw(data_table[, norm_vector_name], p.value)
        return(data.frame(p.value = norm_stat[1], test.statistic  = norm_stat[2], normal.distribution = norm_stat[3]))
    } else if(length(parmtrs) == 1){ # One level of stratification of data
        for(i in unique(data_table[,parmtrs])){
            # Call the SW function to compute normality
            norm_stat <- normality.sw(data_table[data_table[,parmtrs] == i, norm_vector_name], p.value)
            return_table_row <- data.frame(a = i, b = norm_stat[1], c  = norm_stat[2], d = norm_stat[3])
            colnames(return_table_row) <- c(parmtrs, 'p.value', 'test.statistic', 'normal.distribution')
            # add to the return table for each unique value present in the specified column
            return_table <- rbind(return_table, return_table_row)
        }
        # return the formatted table
        return(return_table)
    } else{
        for(i in unique(data_table[,parmtrs[1]])){ # for more than one level of stratification the function uses recursion
            # Call itself with one less parameter in parmtrs
            self_table <- rec.sw.test(data_table[data_table[,parmtrs[1]] == i,], parmtrs[-1], norm_vector_name)
            
            # Add a column of current parameters which stratify the data
            return_table_row <- data.frame(a = i)
            colnames(return_table_row) <- c(parmtrs[1])
            return_table_row <- cbind(return_table_row, self_table)
            
            # add to the return table for each unique value present in the specified column
            return_table <- rbind(return_table, return_table_row)
        }
        # return the formatted table
        return(return_table)
    }
}


automated.stat.test <- function(data_table, parmtrs, y_vector_name, x_vector_name, test_type = 'anova'){
    # Uses data_table as input for analysis, stratifying based on the unique values in
    # each column specified in parmtrs. y_vector_name indicates the column of values
    # for the stat test and x_vector name specifies the column of groups for comparison.
    # Function recursively determines every combination of unique vectors based on specified columns in parmtrs
    # data_table: data frame
    # parmtrs: vector of column names
    # y_vector_name, x_vector_name: String
    # test_type indicates the tyoe of statistical test to perform, the default is the anova
    
    # Checks if given colnames in parmtrs and norm_vector_name are in the data_table
    if(sum(c(parmtrs, y_vector_name, x_vector_name) %in% colnames(data_table)) < length(parmtrs)+2){
        stop("Column names given are not in the data table.")
    }
    
    # Initialize a return table
    return_table <- data.frame()
    
    # Determine the type of statistical test requested and generate a function alias to call
    if(test_type == 'anova'){
        stat_func_alias <- function(y,x) anova.test(y,x)
    } else if(test_type == 'kw'){
        stat_func_alias <- function(y,x) kw.test(y,x)
    } else {
        stop("Test type entered does not correspond to available tests")
    }
    
    
    # No need to stratify data
    if(length(parmtrs) < 1){
        # Call the stat function to calculate statistical significance and return results
        test_stat <- stat_func_alias(data_table[, y_vector_name], data_table[, x_vector_name])
        return(data.frame(p.value = test_stat))
    } else if(length(parmtrs) == 1){ # One level of stratification of data
        for(i in unique(data_table[,parmtrs])){
            # Call the stat function to compute statistical significance
            test_stat <- stat_func_alias(data_table[data_table[,parmtrs] == i, y_vector_name], data_table[data_table[,parmtrs] == i, x_vector_name])
            return_table_row <- data.frame(a = i, b = test_stat)
            colnames(return_table_row) <- c(parmtrs, 'p.value')
            # add to the return table for each unique value present in the specified column
            return_table <- rbind(return_table, return_table_row)
        }
        # return the formatted table
        return(return_table)
    } else{
        for(i in unique(data_table[,parmtrs[1]])){ # for more than one level of stratification the function uses recursion
            # Call itself with one less parameter in parmtrs
            self_table <- automated.stat.test(data_table[data_table[,parmtrs[1]] == i,], parmtrs[-1], y_vector_name, x_vector_name, test_type)
            
            # Add a column of current parameters which stratify the data
            return_table_row <- data.frame(a = i)
            colnames(return_table_row) <- c(parmtrs[1])
            return_table_row <- cbind(return_table_row, self_table)
            
            # add to the return table for each unique value present in the specified column
            return_table <- rbind(return_table, return_table_row)
        }
        # return the formatted table
        return(return_table)
    }
}
