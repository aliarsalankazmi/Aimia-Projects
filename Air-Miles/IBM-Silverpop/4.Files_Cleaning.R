#Load directories
inputDir <- "Z:\\SILVERPOP\\Silverpop_Files_Created\\Seedlist"
outputDir <- "Z:\\SILVERPOP\\Seedlist"



#Identify files
myFile <- list.files(path = inputDir, full.names = TRUE)


#Read file in
fileInput <- readLines(con <- file(myFile, encoding = "UCS-2LE"))
fileCleaned <- gsub("-+", "", fileInput) 
zeroSize <- unname(sapply(fileCleaned, function(x) nchar(x) == 0))
fileCleaned <- fileInput[!zeroSize]
unnecessaryRows <- grepl("Job|record|affected", fileCleaned, ignore.case = TRUE)
fileCleaned <- fileCleaned[!unnecessaryRows]


#Exporting the resulting file
fileFinal <- paste0(fileCleaned, collapse = "\n")
fileName <- paste(outputDir, "SILVERPOP_SEEDLIST_DAILY.TXT", sep = "/")
writeLines(text = fileFinal, con = fileName)
