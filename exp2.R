#!/usr/bin/env Rscript

# William Donahoe, 2014

args = commandArgs( trailingOnly = TRUE )

INPUT_DIR <- args[ 1 ]
SCRIPT_NAME <- "exp2.R"
OUTPUT_DIR <- "out"
FOLDER_FORMAT <- "(?:January|Febuary|March|April|May|June|July|August|September|October|November|December)_[0-9]{1,2}(_[0-9]{1,2}(AM|PM))?"
FILE_FORMAT <- "JFAADS2012-20[0-9]{6}-\\d+-DataLog_User"
SEP <- ifelse(Sys.info()['sysname'] != "Windows","/","\\")
DATE_FORMAT1 <- "%B_%d_%I%p"
DATE_FORMAT2 <- "%B_%d"
DATE_FORMAT3 <- "%Y%m%d%H%M%S"

# -----------------------------------------------------------------------------
# printlog
# Time-stamped output function
#     Arguments: msg -- A string to print
#                ts  -- Time stamp? Default = TRUE
#                cr  -- New line? Default = TRUE
#
printlog <- function( msg="", ..., ts=TRUE, cr=TRUE ) {

  if( ts ) cat( date(), " " )
  cat( msg, ... )
  if( cr ) cat( "\n")

} # printlog

# -----------------------------------------------------------------------------
# loadlibs
# Load a list of libraries
#     Arguments: liblist -- A character vector of library names.
# 
loadlibs <- function( liblist ) {

  printlog( "Loading libraries..." )
  loadedlibs <- vector()
  for( lib in liblist ) {

    printlog( "Loading", lib )
    loadedlibs[ lib ] <- require( lib, character.only=T, warn.conflicts=F )
    if( !loadedlibs[ lib ] )
      warning( "this package is not installed!" )

  }

  invisible( loadedlibs )

} # loadlibs

non.na <- function( n, m ){

	ifelse( is.na( n ) && is.na( m ), NA, TRUE )

}

proper_folder <- function( folder ){

	posix <- non.na( strptime( folder,format=DATE_FORMAT1 ),
					strptime( folder,format=DATE_FORMAT2 ) )
	return ( posix )

}

proper_file <- function( file ){

	file.split <- strsplit( file,"-" )
	datetime <- paste0( file.split[[1]][2],file.split[[1]][3] )

	return ( !is.na( strptime( datetime, format=DATE_FORMAT3 ) ) )

}

get_filenames <- function(){
	
	all <- lapply(list.files(INPUT_DIR, FOLDER_FORMAT),
		function(f){ifelse(proper_folder(f),paste0(INPUT_DIR,SEP,f),NA)})
	names <- unlist(all)
	for (i in seq(1:length(all))){

		f <- list.files(all[[i]],FILE_FORMAT)

		for (j in seq(1:length(f))){

			all[[i]][j] <- ifelse(proper_file(f[j]),paste0(names[i],"/",f[j]),NA)

		}
	}

	return(all)
}

# TODO: Read all files into a data.table.
read_files <- function( filenames ){

}


# --------------------------------------------------------------------------------
# main
main <- function() {

	stopifnot( file.exists( INPUT_DIR ) )

	loadlibs(c("data.table"))

	if( !file.exists( OUTPUT_DIR ) ) {

  		printlog( "Creating", OUTPUT_DIR )
  		dir.create( OUTPUT_DIR )

	}

	all_files <- get_filenames()
}

main()

