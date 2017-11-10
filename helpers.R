#install.packages( "data.table" )
#install.packages( "magrittr" )
#install.packages( "ggplot2" )
#install.packages( "scales" )
# install.packages( "curry" )
library( curry )
library( data.table )
library( magrittr )
library( ggplot2 )
library( scales )



## helper to automate typical csv file import steps
import_raw <- function ( filename ) {
  fileName <- filename
  
  ## import headers first to determine classes
  initial <- read.csv( file = fileName,
                       stringsAsFactors = FALSE, 
                       nrows = 100000 )
  
  ## determine class of each column variable
  classes <- sapply( initial, class )
  
  ## import rest of data
  ptm <- proc.time()
  initial <- read.csv( file = fileName,
                       stringsAsFactors = FALSE, 
                       colClasses = classes,
                       nrows = 5611771, # 5611771
                       comment.char = "" )
  print( proc.time() - ptm )
  
  ## import data set from csv while assigning corresponding classes
  #raw = read.csv( fileName, 
  #               colClasses = classes )
  raw <- initial
}



### --- BEGIN CALC HELPER FUNCTIONS ---
#

## helper to replace char timestamps with numeric, excel-friendly datetime values
#ex: as.excel_DT( "10/01/2016 12:00:00 AM" )
as.excel_DT <- function( char_DT ) {
  # define excel time origin if not defined
  if( !exists( "origin" ) ) {
    origin <- as.POSIXct( x = "1899-12-30 00:00", tz = "UTC" ) 
  }
  
  #
  strptime( x = char_DT,
            format = "%m/%d/%Y %I:%M:%S %p", 
            tz = "UTC" ) %>%
    difftime( origin, units = "days" ) %>%
    as.numeric
}

# helper for calculating hours
to.excel_Hr <- function( excel_DT ) {
  as.integer( floor( mod( excel_DT, 1 ) * 24 ) )
}

## helper function for calculating the mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#
### --- END CALC HELPER FUNCTIONS ---



### --- BEGIN GRAPH HELPER FUNCTIONS ---
#

## helper function to show numeric decimal places
show_decimal <- function(x, k) format(round(x, k), nsmall=k)

# helper function for displaying histograms
my_histo <- function( DT, field_name, min_range, max_range, by_category ) {
  field <- field_name
  
  DT.LOS <- DT[ eval( field ) > min_range &  eval( field ) < max_range ]
  
  p <- ggplot( DT.LOS, aes( x = eval( field ) ) ) +
    geom_histogram( aes( y = (..count..) / sum( ..count.. ) ), 
                    binwidth = 1, 
                    fill = "cornflowerblue", 
                    color = "white", 
                    size = .01 ) +
    scale_y_continuous( labels = percent )
  
  # calculate the mean, median and mode of each facet
  DT.stats <- DT.LOS[, j = .SD[, list( n = .N,
                                       mean = floor( mean(eval( field )) ), 
                                       med = floor( median( eval( field ) ) ),
                                       mode = floor( Mode( eval( field ) ) ),
                                       std.dev = floor( sd( eval( field ) ) ),
                                       x_label_pos = ( max( eval( field ) ) - min( eval( field ) ) ) * 0.5,
                                       y_label_pos = 1 ) ], ## place holder
                     keyby = by_category ] ## "keyby" sorts "by" does not
  #"DISPOSITION ) ]
  
  ## faceted in wrapping arranged subpanels
  # p <- p + facet_wrap( ~ TRIAGE_CATEGORY, scales = "free" )
  
  # calculate % grand total for y_label using plotted data
  DT.plot <- as.data.table( layer_data( p ) )
  DT.plot <- DT.plot[, j = .SD[, list( max( y ) ) ], 
                     by = c( "PANEL" ) ]
  DT.stats <- DT.stats[, ':=' ( y_label_pos = DT.plot$V1 ) ]
  
  
  # add mean and median labels to plots and specified location
  p <- p + geom_text( data = DT.stats, aes( x = x_label_pos, 
                                            y = y_label_pos, 
                                            label = paste("Mean:  ", mean ) ) )
  p <- p + geom_text( data = DT.stats, aes( x = x_label_pos, 
                                            y = y_label_pos * 0.9, 
                                            label = paste("Median:  ", med ) ) )
  
  ## export data to csv
  fileName <- paste( deparse( field ), "_stats", sep = "" )
  fileExt <- ".csv"
  write.table( DT.stats, 
               file      = paste( "./", fileName, fileExt ), 
               sep       = ",",
               row.names = TRUE, 
               col.names = NA )
  
  ## use black and white theme
  p + theme_bw()
}


# helper function to save plots to file
my_ggsave <- function( plot, type, var_descr, clarifier, width, height ) {
  date_today <- as.character( Sys.Date() )
  img_type <- ".png"
  file_name <- c( date_today, type, var_descr, clarifier, img_type )
  
  ggsave(
    filename = paste( file_name, collapse = " " ),
    width = width,
    height = height,
    units = "in",
    dpi = 600,
    path = "graphs"
  )
}

# helper to print data into csv tables
my_export <- function( DT.stats, field_name ) {
  date_today <- as.character( Sys.Date() )
  fileName <- paste( field_name, "_table", sep = "" )
  fileExt <- ".csv"
  full_name <- paste( c( date_today, fileName, fileExt ), collapse = " " )
  
  write.table( DT.stats,
               file      = paste( "./results/", full_name ), 
               sep       = ",",
               row.names = TRUE, 
               col.names = NA )
  
  full_name
}


## helper function to calculate box plot stats
my_stats <- function(DT, field, by_categories) {
  # calculate the mean, median, mode and std deviation of each facet
  DT[ !is.na( eval(field) ), j = .SD[, list(
    n = .N,
    mean = mean(eval(field)),
    med = as.numeric( median(eval(field)) ),
    mode = Mode(eval(field)),
    std.dev = sd(eval(field))
  )],
  keyby = by_categories ]
}

## helper function to calculate label positions
add_label_pos <- function( p, DT.stats ) {
  # calculate positions of x and y labels for annotations and add to stats DT
  DT.plot <- as.data.table( layer_data( p ) )
  DT.plot <-
    DT.plot[, j = .SD[, list(max(ymax_final),
                             (max(x) - min(x)) * 0.5)],
            by = c("PANEL")]
  DT.stats[, ':=' (y_label_pos = DT.plot$V1,
                   x_label_pos = DT.plot$V2)]
}

## helper function to generate box plot graphs
my_boxplot <- function( DT, x_field, y_field, by_categories ) {
  # convert category string to variable for facet grid
  facet_by_cat <- parse(text = paste("~",
                                     Reduce(
                                       function(x, y)
                                         paste(x, y, sep = " + "),
                                       by_categories
                                     ),
                                     sep = " "))
  
  # collect stats
  DT.stats <- my_stats(DT, y_field, by_categories)
  
  # make variable factor so it can be discrete on x-axis
  DT[, ":=" ( eval( x_field ), as.factor( eval( x_field ) ) )  ]
  
  
  # create base plot
  p <- ggplot( DT, 
               aes( x = eval( x_field ), y = eval( y_field ) ) ) + 
    geom_boxplot() +
    facet_grid( eval( facet_by_cat ) )
  
  # calculate label positions and add them to stats table
  add_label_pos( p, DT.stats )
  
  # add mean label to plot at specified location
  p <- p + geom_text(data = DT.stats, aes(
    x = x_label_pos,
    y = y_label_pos,
    label = paste("Mean:  ",
                  show_decimal(mean, 1))
  ))
  
  p <- p + geom_hline(
    data = DT.stats,
    aes(yintercept = mean,
        color = "darkred"),
    linetype = "dashed",
    size = 1
  )
  
  #   p <- p + scale_y_continuous(
  #     trans = log2_trans(),
  #     breaks = 2^(-7:7),
  #     # breaks = trans_breaks("log2", function(x) 2^x),
  #     labels = trans_format("log2", math_format(2^.x))
  #   )
  
  p <- p + theme_bw() + theme( legend.position = "none" )
}

#
### --- END GRAPH HELPER FUNCTIONS ---