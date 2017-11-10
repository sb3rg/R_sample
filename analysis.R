source( "helpers.R" )
## --- IMPORT DATA --- 
  ## Step 1: copy data to excel spreadsheet
  
  ## Step 2: remove all formatting via clear>all formatting.
  ##  make sure that date-time fields are in excel number format i.e. 40802.09
  ##  make sure that all "" values are replaced with null in excel
  ##   copy data set into new sheet--values only > click find > search for "" text > replace with "null" (check match entire contents) > replace "null" with blank
  
  raw <- import_raw( "Data.csv" )

## --- COPY DATA SET ---

  copy <- as.data.table( raw )
  

## --- ADD ADDITIONAL FIELDS ---
  ## add durations of interest
  copy[, ':=' ( Seen.to.Dispo, ( First.ED.Dispo - Seen.MD.PA ) * 24 * 60 ) ]
  copy[, ':=' ( Roomed.to.ED.Depart, ( Ed.Depart - Seen.MD.PA ) * 24 * 60 ) ]
  copy[, ':=' ( Arrive.to.ED.Depart, ( Ed.Depart - ED.Arrive ) * 24 * 60 ) ]
  copy[, ':=' ( Dispo.to.ED.Depart, ( Ed.Depart - First.ED.Dispo ) * 24 * 60 ) ]
  
  ## add custom ESI groupings
  copy[, c( "ESI_Group" ) := list( ifelse( as.integer( Acuity.Level.C ) <= 2,
                                           "ESI 1-2",
                                           ifelse( as.integer( Acuity.Level.C ) > 3,
                                                   "ESI 4-5",
                                                   "ESI 3" ) ) ) ]

  
  ## calc posix time
  temp <- as.POSIXlt(
    as.POSIXct( copy$ED.Arrive * (60 * 60 * 24),
               origin = "1899-12-30",
               tz = "GMT"),
    origin = "1899-12-30", tz = "GMT"
  )
  
  ## add date properties to census data table
  copy[, c("mday",
           "wday",
           "yday",
           "month",
           "year",
           "hour") := list(
             temp$mday,
             ifelse(temp$wday == 0, 7, temp$wday),
             1 + temp$yday,
             1 + temp$mon,
             1900 + temp$year,
             temp$hour
           )]
  
## -- FILTERS AND GRAPHS --
  
  
  
  # helper to process data given a filter and variable description
  gen_boxplot <- function( DT, filter, x_field, y_field, by_categories, var_descr, clarifier ) {
    type <- "Box+Wh"
    by_cat <- by_categories
    x_field <- x_field
    DT <- DT[ eval( filter ) ]
    
    DT %>%
      my_boxplot( x_field, y_field, by_cat ) %>%
      my_ggsave(type = type,
                var_descr = var_descr,
                clarifier = clarifier, 11, 8.5)
    
    ## return filtered data
    DT
  }
  
  ## generate and save graphs for each variable of interest
  gen_duration_boxplots <- function( copy, clarifier, by_categories ) {
    # Filter to be used by all other filters
    common_filter <- bquote( !is.na( Acuity.Level.C ) &
                               Arrive.Hour >= 10 & Arrive.Hour <= 22)
    
    arr_Rm_filter <-
      bquote(.(common_filter) &
               `Arrive.to.Roomed` > 0  & `Arrive.to.Roomed` < 180)
    rm_MD_filter <-
      bquote(.(common_filter) &
               `Roomed.to.Seen` > 0  & `Roomed.to.Seen` < 120 )
    md_Dispo_filter <-
      bquote(.(common_filter) &
               `Seen.to.Dispo` > 0  & `Seen.to.Dispo` < 400 )
    dispo_DC_filter <-
      bquote(.(common_filter) &
               `Dispo.to.ED.Depart` > 0  & `Dispo.to.ED.Depart` < 500 )
    bed_DC_filter <-
      bquote(.(common_filter) &
               `Roomed.to.ED.Depart` > 0  & `Roomed.to.ED.Depart` < 600 )
    arr_DC_filter <-
      bquote(.(common_filter) &
               `Arrive.to.ED.Depart` > 0 & `Arrive.to.ED.Depart` <600 )
    
    ## ARRIVAL TO ROOM
    gen_boxplot(
      copy, arr_Rm_filter, quote(`Arrive.Hour`), quote(`Arrive.to.Roomed`), by_categories,"Arrival to Room by Hr", clarifier
    ) %>% my_stats( quote(`Arrive.to.Roomed`), by_categories ) %>% my_export( "Arrival to Room" )
    ## ROOM TO PROVIDER
    gen_boxplot(
      copy, rm_MD_filter, quote(`Arrive.Hour`), quote(`Roomed.to.Seen`), by_categories,"Room to Provider by Hr", clarifier
    )
    ## PROVIDER TO DISPOSITION
    gen_boxplot(
      copy, md_Dispo_filter, quote(`Arrive.Hour`), quote(`Seen.to.Dispo`), by_categories,"Provider to dispo by Hr", clarifier
    )
    ## DISPOSITION TO DISCHARGE
    gen_boxplot(
      copy, dispo_DC_filter, quote(`Arrive.Hour`), quote(`Dispo.to.ED.Depart`), by_categories,"Dispo to Discharge by Hr", clarifier
    )
    ## ROom TO DISCHARGE
    gen_boxplot(
      copy, bed_DC_filter, quote(`Arrive.Hour`), quote(`Roomed.to.ED.Depart`), by_categories,"Room to Discharge by Hr", clarifier
    )
    ## ARRIVAL TO DISCHARGE
    gen_boxplot(
      copy, arr_DC_filter, quote(`Arrive.Hour`), quote(`Arrive.to.ED.Depart`), by_categories,"Arrival to Discharge by Hr", clarifier
    )
    
    "OK"
  } 
  
  ## execute box plot creation
  gen_duration_boxplots( copy[ Std.ED.Dispo != "BOP" ], "(all days)", c("Std.ED.Dispo", "ESI_Group") )
  gen_duration_boxplots( copy[ Std.ED.Dispo != "BOP" & year == 2014 ], "(all days 2014)", c("Std.ED.Dispo", "ESI_Group") )
  gen_duration_boxplots( copy[ Std.ED.Dispo != "BOP" & year == 2015 ], "(all days 2015)", c("Std.ED.Dispo", "ESI_Group") )
  gen_duration_boxplots( copy[ Std.ED.Dispo != "BOP" & year == 2016 ], "(all days 2016)", c("Std.ED.Dispo", "ESI_Group") )
  
  ## --- FILTER SUBSET FOR TOP 80TH PERCENTILE ---
  top_80_perc <- unique( copy[, .N, by = Arrive.MM.DD.YYYY ][ N >= 185 , Arrive.MM.DD.YYYY ] )
  
  copy.top <- copy[ Arrive.MM.DD.YYYY %in% top_80_perc ]
  
  gen_duration_boxplots( copy.top, "(top 80th perc days)", c("Std.ED.Dispo", "ESI_Group") )
  
  
  
  
  
  
  
  
  
  

## --- REMOVE UNDESIRED VARIABLES ---
  
  copy[, c("Age.Years",
           "Age.Months",
           "Gender",
           "Means.Of.ARRR.C",
           "EVENT_TIME_firstbed",
           "EVENT_TIME_lastbed",
           "PROCEDURE_DESC",
           "Acuity",
           "ED.Disposition.C",
           "Disch.Disp.C",
           # "Arrive.MM.DD.YYYY",
           "Arrive.Week",
           "Arrive.DOW",
           "Arrive.DOW..",
           "Arrive.Hour",
           "LOS",
           "Arrive.to.Roomed",
           "Arrived.to.Reg.Comp",
           "Arrived.to.Seen",           
           "Arrived.to.First.Ed.Disp",
           "Arrived.to.Triage",
           "Roomed.to.Seen",
           "Roomed.to.Depart",
           "Roomed.to.First.ED.Dispo",
           "First.Ed.Disp.to.Discharge",
           "First.Ed.Disp.to.Admit",
           "Seen.to.Depart",
           "Arrival.DT.2",
           "Arrival.DT.3" ) := list( NULL ) ]
  
## --- TRANSFORM / RESHAPE DATA ---
  ## --- Melt Timestamps ---
  ## Assign melted ID vars
  keycols <- c( "CSN",
                "Ed.Episode.ID",
                "Means.Of.Arrival",
                "Acuity.Level.C",
                "Ed.Dispo",
                "Disch.Dispo",
                "Arrive.MM.DD.YYYY",
                "Means.of.Depart",
                "ROOM_ID_firstbed",
                "ROOM_NAME_firstbed",
                "BED_ID_firstbed",
                "ROOM_ID_lastbed",
                "ROOM_NAME_lastbed",
                "BED_ID_lastbed",
                "Std.ED.Dispo" )
  
  ## melt time stamps data
  melted <- as.data.table( melt( copy, id.vars = keycols ) )
  
  ## Assign table key for sorting timestamps
  keycols <- c( keycols, "value" )
  
  ## sort data and filter NA's and Zero's (make sure 'from' is of type char)
  ## add rename event type to "from"
  copy <- melted[ i = !is.na( value ) & value != 0,
                  j = list( from = as.character( variable ) ), keyby = keycols ]
  
  
  ## --- Calculate 'From --> To' Durations ---

  ## add new column that adds an index counter for the key set of interest   
  keycols <- c( "Ed.Episode.ID" ) #sub-index by billing number
  copy[, ':=' ( subid, .SD[, j = list( .I )] ), keyby = keycols ]
  
  ## --- Map 'From --> To' ---
  ## create a new column that contains 1 position shifts (upwards) of time stamp key
  copy[, c( 'to' ) := list( c( tail( from, -1 ), 
                               head( from, 1 ) ) ), by = keycols ]
  
  ## create a new column that contains 1 position shifts (upwards) of time stamp values
  copy[, c( 'to_value' ) := list( c( tail( value, -1 ),
                                     head( value, 1 ) ) ), 
       by = keycols ]
  
  ## remove last item to eliminate equivalent records
  copy[, ':=' ( maxID, .SD[, j = list( max( subid ) )]), keyby = keycols ]
  copy <- copy[ subid != maxID ]
  
  ## calculate durations
  copy[, c( 'duration' ) := list( ( to_value - value ) * 24 * 60 ) ]
  
  ## create a new column that contains concatenated start-stop character pairs
  copy[, c( 'from --> to' ) := list( paste( from, to, sep = " --> " ) ), by = keycols ]

  
  ## --- FILTER SUBSET FOR TOP 50TH PERCENTILE ---
  top_80_perc <- unique( as.data.table( raw )[, .N, by = Arrive.MM.DD.YYYY ][ N >= 185 , Arrive.MM.DD.YYYY ] )
  
  copy.top <- copy[ Arrive.MM.DD.YYYY %in% top_80_perc ]
  # copy <- as.data.table( raw )[ Arrive.MM.DD.YYYY %in% top_50_perc ]
  
  
  ## --- CONSOLIDATE AND PRINT ---

  ## export data to csv
  my_export( copy, "filtered" )
  
  
  
  
## --- GENERATE GRAPHS ---
  DT <- copy( copy )
  
  #### --- HISTOGRAMS --- 
  DT.LOS <- DT[ duration > 0 &  duration < 1440 ]
  
  # calculate the mean, median and mode of each category
  DT.stats <- DT.LOS[, j = .SD[, list( n = .N,
                                       mean = floor( mean(duration) ), 
                                       med = floor( median( duration ) ),
                                       mode = floor( Mode( duration ) ),
                                       std.dev = floor( sd( duration ) ),
                                       x_label_pos = ( max( duration ) - min( duration ) ) * 0.5,
                                       y_label_pos = 1 ) ], ## place holder
                     keyby = c( "from --> to", "from", "to", "Std.ED.Dispo", "Acuity.Level.C" ) ]
  
  # get list of valid from --> to paths based on practical frequency (99th percentile)
  keep <- c( "ETC.Expected --> Triage.Comp",
             "ETC.Expected --> Roomed",
             "ED.Arrive --> Triage.Comp",
             "ED.Arrive --> Roomed",
             "Triage.Comp --> Roomed",
             "Triage.Comp --> Reg.Comp",
             "Triage.Comp --> Seen.MD.PA",
             "Roomed --> Reg.Comp",
             "Roomed --> Seen.MD.PA",
             "Roomed --> Triage.Comp",
             "Reg.Comp --> Seen.MD.PA",
             "Reg.Comp --> Disch.Dispo.Select",
             "Reg.Comp --> Roomed",
             "Reg.Comp --> Admit.Dispo.Selected",
             "Reg.Comp --> Ed.Depart",
             "Seen.MD.PA --> Disch.Dispo.Select",
             "Seen.MD.PA --> Admit.Dispo.Selected",
             "Seen.MD.PA --> Ed.Depart",
             "Seen.MD.PA --> Reg.Comp",
             "Seen.MD.PA --> Triage.Comp",
             "First.ED.Dispo --> Ed.Depart",
             "First.ED.Dispo --> Hospital.Discharge",
             "First.ED.Dispo --> Trans.to.OTF",
             "Trans.to.OTF --> Ed.Depart",
             "Trans.to.OTF --> Hospital.Discharge" )
  
  
  
  my_export( DT.stats[ `from --> to` %in% keep ], "from-to_durations" )
  
  
  
  
  
### BELOW THIS LINE DOES NOT WORK! ###
  DT.panel.stats <- DT.LOS[, j = .SD[, list( n = .N,
                                            mean = floor( mean(duration) ), 
                                            med = floor( median( duration ) ),
                                            mode = floor( Mode( duration ) ),
                                            std.dev = floor( sd( duration ) ),
                                            x_label_pos = ( max( duration ) - min( duration ) ) * 0.5,
                                            y_label_pos = 1 ) ], ## place holder
                          keyby = c( "from --> to" ) ]

  
  
  p <- ggplot( DT.LOS[ `from --> to` %in% keep ], aes( x = duration ) ) +
    geom_histogram( aes( y = (..count..) / sum( ..count.. ) ), 
                    binwidth = 5, 
                    fill = "cornflowerblue", 
                    color = "white", 
                    size = .01 ) +
    scale_y_continuous( labels = percent )
  
  ## faceted in wrapping arranged subpanels
  p <- p + facet_wrap( from ~ to, scales = "free" )
  
  DT.plot <- as.data.table( layer_data( p ) )
  DT.plot <- DT.plot[, j = .SD[, list( max( y ) ) ], 
                     by = c( "PANEL" ) ]
  DT.panel.stats <- DT.panel.stats[ `from --> to` %in% keep]
  DT.panel.stats[, ':=' ( y_label_pos = DT.plot$V1 ) ]
  
  # add mean and median labels to plots and specified location
  p <- p + geom_text( data = DT.panel.stats, aes( x = x_label_pos, 
                                            y = y_label_pos, 
                                            label = paste("Mean:  ", mean ) ) )
  p <- p + geom_text( data = DT.panel.stats, aes( x = x_label_pos, 
                                            y = y_label_pos * 0.9, 
                                            label = paste("Median:  ", med ) ) )
  
  ## use black and white theme
  p + theme_bw()
  