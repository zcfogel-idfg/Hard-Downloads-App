#################################################
### ASSEMBLE AND CLEAN FILES TO BE DOWNLOADED ###
#################################################

####-------------------------------------
#### List of Existing Hard Downloads

setwd("K:/Wildlife/Shiny_Apps/ZCF/Hard-Downloads-App/code")
source('Collar_Packages.R')
# run script to access server
source("Server_Access.R")

# create table of animal_ID and collar serial no
#   con is from Server_Access.R
 records <- dbGetQuery(con, 'SELECT Animal_ID, Collar_Serial_No
                       FROM Collars_Hard_Downloads2')

#records <- combined

# create unique ID by combining animal ID and serial no, with ET in middle ('eartag')
records$ID <- paste(records$Collar_Serial_No,
                    records$Animal_ID,
                    sep = '_ET')

# get list of all files that have been downloaded -- records is all collar locations so each ID is there many times
downloaded <- unique(records$ID)
# convert to dataframe
downloaded_df <- data.frame(1:length(downloaded), downloaded)

####---------------------------------------------------------------
#### Get list of collars that still need to be imported onto server

## List all files in Downloaded Collar Files folder 
##    -> these are files that have been downloaded from collar but may not be processed/imported onto server
##    -> recursive argument extracts all files in subfolders
##    -> some of these may have already been imported
x <- list.files(path = "K:/Wildlife/Collars/Downloaded Collar Files", 
                recursive = T)


####--------------------------------------------------
#### Set up data frame for downloaded collar files

## dataframe with file paths, filenames with extension, and filenames without ext
## need to add beginning of path on 
xx <- data.frame(path = paste0('K:/Wildlife/Collars/Downloaded Collar Files/', x),     # eg K:/Wildlife/Collars/Downloaded Collar Files/2020/R1/69099_ET12345
                 # basename extracts filename at end of path
                 file.ext = basename(x))     # eg 69099_ET12345.txt
## file_path_sans_ext removes extension
xx$file <- tools::file_path_sans_ext(xx$file.ext)      # eg 69099_ET12345


## column with file extension (file type)
# regular expression looking for extension (eg .txt, .csv)
reg.ext <- "^.*\\.([[:alpha:]]{3,4})$"

# add column
xx$ext <- sub(reg.ext, '\\1', xx$file.ext)

# check to see what file types there are
# unique(xx$ext)

####----------------------------------------------
#### Clean up filenames 

## will create a column 'ID' that's just CollarSerial_ETAnimalID 
##    -> theoretically the same as file but this accounts for cases where the file is named incorrectly

## look for file names with format GPS_Collar_CollarSerial_ETAnimalID
reg.norm <- "(GPS_Collar)*([[:alnum:]]+_[ET]*[[:alnum:]]+)"
xx$ID <- gsub(reg.norm, "\\2", xx$file)

## Add in "ET before AnimalID if missing
reg.missingET <- "([[:alnum:]]+_)([^ET][[:alnum:]]+)"
xx$ID <- gsub(reg.missingET, "\\1ET\\2", xx$ID)

## take out parentheses or spaces outside of ID
reg.misc <- "\\(([[:digit:]]+_ET[[:digit:]]+)\\)"
xx$ID <- gsub(reg.misc, "\\1", xx$ID)

reg.misc2 <- "([[:digit:]]+_ET[[:digit:]]+) *\\([[:alnum:]]{,5})"
xx$ID <- gsub(reg.misc2, "\\1", xx$ID)

## replace collarserial-ETanimal with _
reg.hyph <- "^([[:alnum:]]+)-(ET[[:alnum:]]+)$"
xx$ID <- gsub(reg.hyph, "\\1_\\2", xx$ID)

## Account for CBC collars (they come in with format animalID_collarSerial, and
##    they need to be flipped and an ET added), but they're all in folders named CBC

reg.cbc <- "([[:digit:]]+)_([[:digit:]]+)_.*"

## search for anything in CBC folder (path starts with CBC/)
xx.cbc <- xx[grepl("^CBC/", xx$path),]

xx[grepl("^CBC/", xx$path),]$ID <- gsub(reg.cbc, "\\2_ET\\1", 
                                        xx[grepl("^CBC/", xx$path),]$file)

## Check to see whether ID is now in correct format (Collarserial_ETAnimalID)
## Add column showing whether format is correct
reg.correct <- "^[[:alnum:]]+_ET[[:alnum:]]+$"
xx$correct <- grepl(reg.correct, xx$ID)

#sum(xx$correct)

### Take GDFs out of list that needs to be checked (GDFs are a weird file type that come with a lot of txts and csvs)
## search for duplicates in list
xx$duplicates <- F
for (i in 1:nrow(xx)) {
  if (xx$ID[i] %in% xx$ID[c(1:(i-1),(i+1):nrow(xx))]) xx$duplicates[i] <- T
  else xx$duplicates[i] <- F
}
#sum(xx$duplicates)

## take GDF files out of list
xx <- xx[!(xx$duplicates & (xx$ext == "GDF" | xx$ext == "gdf")),]


#View(xx[xx$duplicates,])

####----------------------------------------------------------------------------------------
#### Compare hard downloads to data that have already been processed (ie stored on server)

## add column to keep track of whether a file has been processed
xx$processed <- 'Error'

### loop through xx$ID and check whether that ID is on the server (ie in downloaded_df)
for (i in 1:length(xx$ID)) {
  ## vector of values in hard downloads that match i (should be 1 or 0)
  ##   -> if sum is 1, means that collar data has been processed
  if(sum(grepl(xx$ID[i], downloaded)) == 1) xx$processed[i] <- T
  
  ## if sum is 0, means that the collar data has not been processed
  else if(sum(grepl(xx$ID[i], downloaded)) == 0) xx$processed[i] <- F
  
  ## if sum isn't 0 or 1, means that collar data has been processed multiple 
  ##    times and there is some error
  else xx$processed[i] <- "ERROR"
}

#sum(xx$processed == T)


## lists of files that are already processed, need to be processed and are in the correct format, and need to be processed but are in an incorrect format
to_process_correct <- xx[xx$processed == F & xx$correct == T,]
to_process_irregular <- xx[xx$processed == F & xx$correct == F,]
processed <- xx[xx$processed == T,]

# write csvs with those dataframes to use later
# write.csv(file = "downloads.csv", downloaded)
# write.csv(file = "need_processing_correct_format.csv", to_process_correct)
# write.csv(file = "need_processing_incorrect_format.csv", to_process_irregular)
# write.csv(file = "processed.csv", processed)

# copy irregular files to separate folder
irreg <- 'K:/Wildlife/Collars/Files to process/Incorrect formats DONT ADD FILES TO THIS'

# remove existing files
#unlink('K:/Wildlife/Collars/Files to process/Incorrect formats DONT ADD FILES TO THIS/*')

# copy files
#file.copy(to_process_irregular$path, irreg)


####################################
### RENAMING FILES ON THE SERVER ###
####################################

####---------------------------------------------------------------
#### Back up files on the server first in a separate zipped file

####---------------------------------------------------------------
#### Rename files whose original names were incorrect

correct <- xx[xx$correct == T,]

# create new file.ext column with corrected filenames
correct$newfile.ext <- paste(correct$ID, correct$ext, sep = '.')

# regex to look for end of path (ie file name)
reg.file <- '/[^/]*$'

# loop through correct table and paste together new file and folder path 
#   -> end with original path but with cleaned up file name at end (for renaming files later on)
for (i in 1:nrow(correct)) {
  correct$new.path[i] <- gsub(reg.file,
                              paste0('/', correct$newfile.ext[i]),
                              correct$path[i])
}


# loop through correct and rename files to new path (need to rename the full path, but all that is changing is the filename)
for (i in 1:nrow(correct)) {
  file.rename(from = correct$path[i], to = correct$new.path[i])
}


####-----------------------------------------------------------------------------
#### Now check again to see if any of the renamed ones were already downloaded

for (i in 1:length(correct$ID)) {
  ## vector of values in hard downloads that match i (should be 1 or 0)
  ##    if sum is 1, means that the collar data has been processed
  #if(sum(grepl(correct$ID[i], downloaded)) == 1) correct$processed[i] <- T
  if(correct$ID[i] %in% downloaded) correct$processed[i] <- T
  
  ##    if sum is 0, means that the collar data has not been processed
  #else if(sum(grepl(correct$ID[i], downloaded)) == 0) correct$processed[i] <- F
  else if(!(correct$ID[i] %in% downloaded)) correct$processed[i] <- F
  
  ##    if sum isn't 0 or 1, means that collar data has been processed multiple 
  ##    times and there is some error
  else correct$processed[i] <- "ERROR"
}

## list of files that need to be processed
to_process_correct <- correct[correct$processed == F & correct$correct == T,]
processed <- correct[correct$processed == T,]

# write.csv(file = "need_processing_correct_format.csv", to_process_correct)
# write.csv(file = "processed.csv", processed)



####-------------------------------------------------------------------
#### Move correct files to proper folders

# keep file.ext, ext, and path
yy <- to_process_correct[,c(2,4,1)]

# remove path and extension from path
yy$dir <- dirname(yy$path)


####------------------------------------------------------------------------
#### Sort collars into CSV, TXT, XLSX, and OTHER

# check for unique extensions
unique(yy$ext)

# new data frame for each type (adjust 'other' depending on what types there are)
txts <- yy[yy$ext == 'txt' | yy$ext == 'TXT',]
csvs <- yy[yy$ext == 'csv' | yy$ext == 'CSV',]
xlsxs <- yy[yy$ext == 'xlsx',]
others <- yy[yy$ext == 'gdf' | yy$ext == 'GDF' | yy$ext == 'tag' | 
               yy$ext == 'kml' | yy$ext == 'tdf',]



# nrow(txts)
# nrow(csvs)
# nrow(others)


















