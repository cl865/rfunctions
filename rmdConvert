# Jun. 2019
# function to convert RMD file to only r code
# inline r code in rmd is not saved
# saves to working directory of RMD file, or another that is user specified
# saved file name is automatically the same as the RMD file
# but there is option to change file name
# input: entire path to RMD; where you want to save (optional); 
#        file name with .R (optional)
# output: r code and r code is saved

# Example:
# The R code from Sample.RMD will be scrapped and saved as Converted.R in 
# the directory C:/RFunctions
# rmdConvert(rmd.path = "C:/RFunctions/Sample.RMD",
#            save.file = "Converted.R")

rmdConvert = function(rmd.path, save.file = NULL, save.dir = NULL){
  # first check extention is .rmd
  if(grepl(".rmd", tolower(rmd.path)) != T){
    return("Error: not a RMD file")
  }
  
  rmd = readLines(rmd.path)
  
  # code chunks start with: ```{r
  # and end with: ```
  
  start = which(grepl("```\\{r", rmd))
  end = which(grepl("```$", rmd))
 
  if(length(start) != length(end)){
    return("Error: incorrectly formatted code chunks")
  }
  
  #  keep everything in between

  rcode = vector("character")
  for(i in 1:length(start)){
    rcode = c(rcode, rmd[(start[i]+1):(end[i]-1)])
  }

  # prepare for export
  
  # (1) convert \\ to /
  rmd.path = gsub("\\\\","/", rmd.path)
  
  # (2) set dir for export
  if(length(save.dir) != 0){
    setwd(save.dir)
  }else{
    dirname(rmd.path)
  }
  
  # (3) set filename
  if(length(save.file) == 0){
    # just use RMD file name
    save.file = gsub("\\..*","",basename(rmd.path))
    save.file = paste0(save.file, ".R")
  }
  
  cat(rcode, file = save.file, sep = "\n")

  return(rcode)
}

