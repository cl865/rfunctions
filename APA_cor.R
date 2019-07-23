# (1) Correlation
# gets correlation matrix of dataframe, formats and exports to current working
# directory, or to path specified in `filename`
# input: cor.dataframe: dataframe with named columns for bivariate correlations,
#        filename: name of excel sheet including .csv extension,
#        ... additional parameters for stats::cor()
# output: Excel sheet saved, correlation matrix

APA_cor = function(cor.dataframe, filename, ...){

    # check that there is dataframe
    if(length(cor.dataframe) == 0){
      return("Error: Missing dataframe for correlation")
    }
    # first format names of dataframe
    rowName = paste0(toupper(substr(names(cor.dataframe),1,1)), 
                     substr(names(cor.dataframe), 2, nchar(names(cor.dataframe))))
    rowName = paste0(seq(1, length(rowName)), ". ", rowName)
    colName = seq(1, length(rowName))
    
    # get correlation
    cor.mat = round(cor(cor.dataframe, ...),3)
    
    # lower triangular only
    cor.mat[lower.tri(cor.mat)] = NA
    
    # fix names
    colnames(cor.mat) = colName
    rownames(cor.mat) = rowName
    
    write.csv(cor.mat, file = filename, na = "")
    return(cor.mat)
}
