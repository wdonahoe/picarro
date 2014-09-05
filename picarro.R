#!/usr/bin/env Rscript

# William Donahoe, 2014

args = commandArgs( trailingOnly = TRUE )

#INPUT_DIR <- args[ 1 ]
#EXP_FILE <- args[ 2 ]
NUM_FILES <- as.numeric(args[ 3 ])
INPUT_DIR <- "Good_Files"
EXP_FILE <- "exp.csv"

SCRIPT_NAME <- "picarro.R"
OUTPUT_DIR <- "out"
FOLDER_FORMAT <- "(?:January|Febuary|March|April|May|June|July|August|September|October|November|December)_[0-9]{1,2}(_[0-9]{1,2}(AM|PM))?"
FILE_FORMAT <- "JFAADS2012-20[0-9]{6}-\\d+-DataLog_User"
SEP <- ifelse(Sys.info()['sysname'] != "Windows","/","\\")
DATE_FORMAT1 <- "%B_%d_%I%p"
DATE_FORMAT2 <- "%B_%d"
DATE_FORMAT3 <- "%Y%m%d%H%M%S"
DATE_FORMAT4 <- "%m_%d_%Y %H:%M"
DATE_FORMAT5 <- "%m_%d_%y %H:%M"

CHAMBER_RAD <- 21.5 # cm

# TODO: Get actual volume.
PICARRO_V <- 482 # cm3 or sccm
LICOR_V <- 263 # cm3

# -----------------------------------------------------------------------------
# printlog
# Time-stamped output function
#     Arguments: msg -- A string to print
#                ts  -- Time stamp? Default = TRUE
#                cr  -- New line? Default = TRUE
#
printlog <- function( msg="", ..., ts=TRUE, cr=TRUE, pre_cr=FALSE ) {

  if ( pre_cr ) cat( "\n" )
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

# ----------------------------------------------------------
# savedata
# Save a data frame
# Arguments: df -- A data frame to save.
# extension -- file extension. Default = .csv
savedata <- function( d, extension=".csv" ) {
	
	my.write <- function(x, file, header, f = write.csv, ...){
		datafile <- file(file, open='wt')
		on.exit(close(datafile))
		if (!missing(header)) writeLines(header,con=datafile)
		return( f(x, datafile, ...) )
	}

  	stopifnot( file.exists( OUTPUT_DIR ) )
  	fn <- paste0( OUTPUT_DIR, SEP, format( Sys.time(),"%d%B%Y_%H%M%S" ),"_fluxes",extension )
  	printlog( "Saving", fn )
  	my.write( d, fn, f = write.csv, row.names=FALSE )

} # savedata

read_exp <- function(){

	check_format <- function( date_time ){

		split <- strsplit(date_time[1],"_")
		posix <- ifelse(nchar(split[[1]][3]) == 4,
				as.POSIXct(paste(date_time[1], date_time[2]),format=DATE_FORMAT4),
				as.POSIXct(paste(date_time[1], date_time[2]),format=DATE_FORMAT5))

		return( posix )

	}

	raw <- read.csv( EXP_FILE,na.strings=c( "na","nan" ) )
	posix <- apply( raw, 1, function( x ){ check_format(x[1:2]) } )
	measurements <- data.table( unlist(posix),raw[,3], raw[,5], raw[,9] )
	setnames(measurements,c("V1","V2","V3","V4"),c("Measurement_Time","Pot","Avg_Temp","Avg_Depth"))
  	setkey(measurements,Measurement_Time)

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

	str <- strptime( datetime, format=DATE_FORMAT3 )
	return ( as.POSIXct(str) )

}

get_filenames <- function(){
	
	folders <- list.files( INPUT_DIR, FOLDER_FORMAT )
	all <- lapply( list.files( INPUT_DIR, FOLDER_FORMAT ),
		function( f ){

			ifelse( proper_folder( f ),
				paste0( INPUT_DIR,SEP,f ),
				NA)

			})

	names <- unlist( all )
	sorting <- vector()
	for ( i in seq( 1:length( all ) ) ){

		f <- list.files( all[[i]],FILE_FORMAT )

		for ( j in seq( 1:length( f ) ) ){

			proper <- proper_file( f[j] )
			all[[i]][j] <- ifelse( !is.na( proper ),paste0( names[i],SEP,f[j] ),NA )
			sorting <- c( sorting,proper ) 

		}
	}

	all <- unlist( all )
	all <- all[order( sorting )]
	return( all ) 
}

my_list <- function( l ){ vector("list",l) }

read_files <- function( filenames, raw, exp_data ){

  	pb <- txtProgressBar(min = 0, max = length(filenames), style = 3)

	table_list <- my_list( length( filenames ) )
	for ( i in 1:length( filenames ) ) {

	  	# update progress bar
	  	setTxtProgressBar(pb, i)

		d <- as.data.table( read.table( filenames[i],header=T ) ) 
		rows <- nrow( d )
		d <- d[,c( "EPOCH_TIME","N2O_dry","N2O_dry30s","CO2_dry","CH4_dry","H2O","NH3" ),with=F]

		raw <- rbindlist( list( raw,d ) )
		table_list[[i]] <- exp_data[rep( c( i ),each=rows ),]

	}

	to_merge <- rbindlist( table_list )
	raw <- raw[,c( "Measurement_Time","Pot","Avg_Temp","Avg_Depth" ):=to_merge]

	return( raw )	

}

#---------------------------------------------------------
# correct_time
# Get times for a measurement w/ start time at zero.
# Arguments: d -- data frame containing data from one measurement.
# Returns: -- all times minus start time.
#
correct_time <- function( d ){ d$EPOCH_TIME - d$EPOCH_TIME[ 1 ] }


quality_control <- function( d ){

	t <- as.numeric(correct_time(d))
	mods <- my_list(6)
	
	for ( i in 2:7 ){

		mods[[i-1]] <- d[[i]]	
		
	}

	mods <- lapply(mods, function( x ){ lm( as.numeric(x) ~ t,na.action="na.exclude" ) } )
	p <- lapply(mods,function( x ){ summary( x )$coefficients[2,4] } )
	r2 <- lapply(mods,function( x ){ round( summary( x )$r.squared, 2 ) } )

	return(c(unlist(r2),unlist(p)))	

}

calc <- function( resp, depth, temp ){
  
	# We want to convert raw respiration (d[CO2]/dt) to a flux using
  	# A = dC/dt * V/S * Pa/RT (e.g. Steduto et al. 2002), where
  	# A is CO2 flux (umol/m2/s)
  	# dC/dt is raw respiration as above (mole fraction/s)
  	# V is total chamber volume (m3)
  	# ...we are correcting for varying headspaces in the cores
  	# S is ground surface area (m2)
  	# ...but we're computing per kg of soil, so using dry mass instead
  	# Pa is atmospheric pressure (kPa)
  	# R is universal gas constant (8.3 x 10-3 m-3 kPa mol-1 K-1)
  	# T is air temperature (K)

	# TODO: Flux Equation.

	S <- pi * CHAMBER_RAD ^ 2
	chamber_v <- depth * S
	V <- chamber_v + PICARRO_V + LICOR_V
	Pa <- 101 #kPA
	R <- 8.3145e-3 # m-3 kPa mol-1 K-1
  	Kelvin <- 273.15 # C to K conversion
	Temp <- temp + Kelvin

	flux <- resp * ( V / S ) * Pa / ( R * Temp )

    return( flux )
  
}

compute_flux <- function( raw ){

	time <- correct_time( raw )
  	depth <- raw$Avg_Depth[1]
  	temp <- raw$Avg_Temp[1]
  	pot <- raw$Pot[1]
  	raw <- as.data.table(raw)
	
	respirations <- apply(raw[,2:7,with=F], 2, function(x){ as.numeric( coef( lm( x ~ time ) )[ 2 ] ) })
  
  	fluxes <- sapply(respirations, calc, depth, temp, simplify=T)
	fluxes[4] <- fluxes[4] * 1.0e-3 # umol -> nmol
	ret <- c(fluxes,temp)

	names( ret ) <-  c( "N2O_dry_flux [umol m-2 s-1]","N2O_dry30s_flux [umol m-2 s-1]",
				"CO2_dry_flux [umol m-2 s-1]","CH4_dry_flux [nmol m-2 s-1]",
				"H2O_flux [umol m-2 s-1]","NH3_flux [umol m-2 s-1]", "Temp [C]")
	
	return( ret )
	
}

get_alldata <- function(raw, qc){

	fluxes <- ddply( raw, .( Measurement_Time ), .fun=compute_flux )
  	return( as.data.table( merge( fluxes, qc,by=c( "Measurement_Time" ) ) ) )
  
}


# --------------------------------------------------------------------------------
# main
#main <- function() {

	stopifnot( file.exists( INPUT_DIR ) )
	stopifnot( file.exists( EXP_FILE ) )

	loadlibs(c("data.table","plyr") )

	if( !file.exists( OUTPUT_DIR ) ) {

  		printlog( "Creating", OUTPUT_DIR )
  		dir.create( OUTPUT_DIR )

	}

  	printlog("Reading data.")
  	exp_data <- read_exp()
	files <- get_filenames()

	raw <- read_files( files, data.table(), exp_data )
	
	names_r2 <- c("N2O_dry_r2","N2O_dry30s_r2","CO2_dry_r2","CH4_dry_r2","H2O_r2","NH3_r2")
	names_p <- c("N2O_dry_p","N2O_dry30s_p","CO2_dry_p","CH4_dry_p","H2O_p","NH3_p")
	names <- c(names_r2,names_p)

  	printlog("Running quality control.",pre_cr=T)
	  qc <- ddply(raw,.(Measurement_Time),.fun=quality_control)
  	setnames(qc,c("Measurement_Time",paste0("V",as.character(1:12))),c("Measurement_Time",names))

  	printlog("Computing fluxes, combining with qc.")
  	alldata <- get_alldata(raw, qc)

  #	savedata( alldata )
  	printlog("All done with ", SCRIPT_NAME)

#main()

