library(rmarkdown)
setwd("/Users/natorro/Dropbox/CURSOS DADOS/AME-2019/ame2019/ejemploparams/")
number <- c(30, 100, 500)
variance <- c(1, 10)

for(i in number){
  for (j in variance){
    render("params.Rmd", 
           params = list(number = i, variance = j), 
           output_file = paste(i,"-", j, ".pdf", sep = ""))    
  }
}

