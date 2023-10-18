###############################################
## Author: Valeriya Dontsova                ###
## Statistical analysis file                ###
## Date: 2022-2023                          ###
###############################################

# number of unique parameters i.e time, treatment etc

# shapiro wilk test for normality
normality.sw <- function(data_table, norm_vector_name, p.value = 0.05){
    # Uses data_table as input for analysis, stratifying based on the unique values
    # in each column specified in parmtrs. norm_vector_name indicates the column of values
    # for the Shapiro-Wilk test for normality of data.
    # data_table: data frame
    # p.value indicates the probability threshold for assigning normality to a given distribution
    # The function returns a table of values
    
    return_table <- data.frame()
    st <- shapiro.test(data_table[,norm_vector_name])
    return_table <- rbind(return_table, data_frame(pvalue = st$p.value, stat = st$statistic))
    return(return_table)

}

rec.sw.test <- function(data_table, parmtrs, norm_vector_name){
    # Returns data frame with selected rows
    # Can be used for recursion
    
    return_table <- data.frame()

    if(length(parmtrs) < 1){
        #cause error
    }
    else if(length(parmtrs) == 1){
        for(i in unique(data_table[,parmtrs])){
            sel_vector <- data_table[data_table[,parmtrs] == i,]
            return_table <- rbind(return_table, normality.sw())
        }
        return(return_table)
    }
    else{
        for(i in unique(data_table[,parmtrs[1]])){
            # call itself
            sel_vector <- data_table[data_table[,parmtrs[1]] == i,]
            return_table <- rbind(return_table, rec.sw.test(sel_vector, parmtrs[-1]))
            # add to return table
        }
        return(return_table)
    }
}
