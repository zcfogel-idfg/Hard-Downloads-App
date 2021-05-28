###############################################################
### Move files to Collars/Files to Process for easy viewing ###
###############################################################

####-----------------------------------------------------------------
#### Move csvs and xlsxs to folder

# need to make these folders (can be anywhere)
csv <- 'K:/Wildlife/Collars/Files to process/CSVs to process DONT ADD FILES TO THIS'
xlsx <- 'K:/Wildlife/Collars/Files to process/XLSXs to process DONT ADD FILES TO THIS'
txt <- 'K:/Wildlife/Collars/Files to process/TXTs to process DONT ADD FILES TO THIS'
csv2 <- 'K:/Wildlife/Collars/Files to process/CSVs to process pt 2 DONT ADD FILES TO THIS' # need to add second folder because some files are same name but dif data
txt2 <- 'K:/Wildlife/Collars/Files to process/TXTs to process pt 2 DONT ADD FILES TO THIS' # need to add second folder because some files are same name but dif data

# remove all existing files from folders to ensure that they contain only the files that need to be processed

unlink('K:/Wildlife/Collars/Files to process/TXTs to process DONT ADD FILES TO THIS/*')
unlink('K:/Wildlife/Collars/Files to process/CSVs to process DONT ADD FILES TO THIS/*')
unlink('K:/Wildlife/Collars/Files to process/XLSXs to process DONT ADD FILES TO THIS/*')
unlink('K:/Wildlife/Collars/Files to process/CSVs to process pt 2 DONT ADD FILES TO THIS/*')
unlink('K:/Wildlife/Collars/Files to process/TXTs to process pt 2 DONT ADD FILES TO THIS/*')

# now copy all all files to proper folder
dupe.txt <- file.copy(txts$path, txt) # store whether copy was successful in logical vector
dupe.csv <- file.copy(csvs$path, csv) # store whether copy was successful in logical vector
file.copy(xlsxs$path, xlsx)
file.copy(csvs[!dupe.csv,]$path, csv2)
file.copy(txts[!dupe.txt,]$path, txt2)














