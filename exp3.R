#!/usr/bin/env Rscript

# William Donahoe, 2014

args = commandArgs( trailingOnly = TRUE )

INPUT_DIR <- args[ 1 ]
EXP_FILE <- args[ 2 ]
SCRIPT_NAME <- "exp3.R"
OUTPUT_DIR <- "out"
FOLDER_FORMAT <- "(?:January|Febuary|March|April|May|June|July|August|September|October|November|December)_[0-9]{1,2}(_[0-9]{1,2}(AM|PM))?"
FILE_FORMAT <- "JFAADS2012-20[0-9]{6}-\\d+-DataLog_User"
SEP <- ifelse(Sys.info()['sysname'] != "Windows","/","\\")
DATE_FORMAT1 <- "%B_%d_%I%p"
DATE_FORMAT2 <- "%B_%d"
DATE_FORMAT3 <- "%Y%m%d%H%M%S"
DATE_FORMAT4 <- "%m_%d_%Y %H:%M"
DATE_FORMAT5 <- "%m_%d_%y %H:%M"

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

read_exp <- function(){

	check_format <- function( date_time ){

		split <- strsplit(date_time[1],"_")
		posix <- ifelse(nchar(split[[1]][3]) == 4,
				as.POSIXct(paste(date_time[1], date_time[2]),format=DATE_FORMAT4),
				as.POSIXct(paste(date_time[1], date_time[2]),format=DATE_FORMAT5))

		return( posix )

	}

	raw <- read.csv(EXP_FILE,header=T,skip=1,na.strings="na")
	raw <- raw[,1:3] # date, time, pot#
	posix <- apply( raw, 1, function( x ){ check_format(x[1:2]) } )
	measurements <- data.table( unlist(posix),raw[,3] )
	setnames(measurements,c("V1","V2"),c("Time","Pot"))

	return(measurements)
}


proper_folder <- function( folder ){

	non.na <- function( n, m ){

		ifelse( is.na( n ) && is.na( m ), NA, TRUE )
	
	}

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
		function(f){
			ifelse(proper_folder(f),
				paste0(INPUT_DIR,SEP,f),
				NA)
			})
	names <- unlist(all)
	for (i in seq(1:length(all))){

		f <- list.files(all[[i]],FILE_FORMAT)

		for (j in seq(1:length(f))){

			all[[i]][j] <- ifelse(proper_file(f[j]),paste0(names[i],"/",f[j]),NA)

		}
	}
	return( unlist( all ) ) 
}

my_list <- function( l ){ vector("list",l) }

read_files <- function( filenames, raw, exp ){

	read_file <- function( fn ){

		f <- as.data.table(read.table(fn,header=T))
		f <- f[,c( "EPOCH_TIME","N2O","CO2","CH4","H2O","NH3" ),with=F]
		f$filename <- fn

		return( f )
	}

	for ( fn in filenames ){

		raw <- rbindlist( list(raw, read_file( fn ) ) )
	}

	setkey(raw,EPOCH_TIME)	

	return( raw )
}

quality_control <- function( d, exp ){
	
	

}


# --------------------------------------------------------------------------------
# main
main <- function() {

	stopifnot( file.exists( INPUT_DIR ) )
	stopifnot( file.exists( EXP_FILE ) )

	loadlibs(c("data.table"))

	if( !file.exists( OUTPUT_DIR ) ) {

  		printlog( "Creating", OUTPUT_DIR )
  		dir.create( OUTPUT_DIR )

	}
	exp_data <- read_exp()
	raw <- data.table()

	print(exp_data)

	raw <- read_files( get_filenames(), raw )
	qc <- raw[,quality_control( .SD, exp_data ),by=Filename,.SDcols=colnames(raw)]
}

main()

