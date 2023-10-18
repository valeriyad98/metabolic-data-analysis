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
    # The function returns a vector of p-value, test-statistic, and normality boolean

    sw.test <- shapiro.test(norm_vector)
    return(c(sw.test$p.value, sw.test$statistic, sw.test$p.value > p.value))
}

rec.sw.test <- function(data_table, parmtrs, norm_vector_name, p.value = 0.05){
    # Uses data_table as input for analysis, stratifying based on the unique values in each column specified in parmtrs. norm_vector_name indicates the column of values
    # for the Shapiro-Wilk test for normality of data.
    # data_table: data frame
    # p.value indicates the probability threshold for assigning normality to a given distribution
    # Recursively determines every combination of unique vectors for SW test based on specified columns in parmtrs
    
    return_table <- data.frame()

    if(length(parmtrs) < 1){
        #cause error
    }
    else if(length(parmtrs) == 1){
        for(i in unique(data_table[,parmtrs])){
            norm_stat <- normality.sw(data_table[data_table[,parmtrs] == i, norm_vector_name], p.value)
            return_table_row <- data.frame(a = i, b = norm_stat[1], c  = norm_stat[2], d = norm_stat[3])
            colnames(return_table_row) <- c(parmtrs, 'p.value', 'test.statistic', 'normal.distribution')
            return_table <- rbind(return_table, return_table_row)
        }
        return(return_table)
    }
    else{
        for(i in unique(data_table[,parmtrs[1]])){
        
           # call itself
            self_table <- rec.sw.test(data_table[data_table[,parmtrs[1]] == i,], parmtrs[-1], norm_vector_name)
            
            return_table_row <- data.frame(a = i)
            colnames(return_table_row) <- c(parmtrs[1])
            
            return_table_row <- cbind(return_table_row, self_table)
            # add to return table
            return_table <- rbind(return_table, return_table_row)
        }
        return(return_table)
    }
}
