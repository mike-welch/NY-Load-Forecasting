

rm(list = ls())

library('dplyr', warn.conflicts = FALSE)
#library('dynlm', warn.conflicts = FALSE)
library('ggplot2')
library('lubridate', warn.conflicts = FALSE)
library('readr')
library('stringr')
library('tidyr')

###################################################
## Data Import & Reduction

Import_NYISO_Data <- function( years ) {

  #############################
  # LBMP

  lbmp_cols =  c('Time','Zone','PTID','LBMP','Losses','Congestion')

  # Import DAM LBMP and format


  dir <- getwd()

  # Preallocate
  zonal_lbmp_DayAheadForecast <- NULL
  zonal_lbmp_HourlyIntegrated <- NULL
  zonal_load_DayAheadForecast <- NULL
  zonal_load_HourlyIntegrated <- NULL

  ###############################################################################
  # Import Data
  H <- 0
  for (year in years ) {
    year
    H <- H + ( 8760 + if(leap_year(year)){24}else{0} )

    setwd( sprintf('%s/Data/%s', dir, year) )
    zonal_lbmp_DayAheadForecast <- rbind( zonal_lbmp_DayAheadForecast, read_csv(sprintf('%s-damlbmp_zone.csv',year), skip = 1, col_names = lbmp_cols ) )
    zonal_lbmp_HourlyIntegrated <- rbind( zonal_lbmp_HourlyIntegrated, read_csv(sprintf('%s-rtlbmp_zone.csv',year), skip = 1, col_names = c(lbmp_cols,'Scarcity') )[-7] )
    zonal_load_DayAheadForecast <- rbind( zonal_load_DayAheadForecast, read_csv(sprintf('%s-isolf.csv',year)) )
    zonal_load_HourlyIntegrated <- rbind( zonal_load_HourlyIntegrated, cbind( read_csv(sprintf('%s-palIntegrated.csv',year) ), Type = 'Integrated' ) )
    setwd(dir)
  }

  #  Jan1UTC = sprintf('1/1/%s 5:00:00',years[1])
  #  Time <- with_tz( mdy_hms(Jan1UTC, tz="UTC") + (0:(H-1))*hours(1),tz='America/New_York')
  Jan1 = sprintf('1/1/%s 0:00:00',years[1])
  Time <- mdy_hms(Jan1) + (0:(H-1))*hours(1)

  ###############################################################################
  # Process zonal_load_DayAheadForecast
  zonal_lbmp_DayAheadForecast <- transform( cbind( Time = rep(Time,each=length(unique(zonal_lbmp_DayAheadForecast$Zone))), zonal_lbmp_DayAheadForecast[,-1], Type = 'DayAhead'), Price = LBMP - Losses + Congestion)
  zonal_lbmp_DayAheadForecast <- zonal_lbmp_DayAheadForecast[,c(1:2,7,4:6,8)]

  # Process zonal_lbmp_HourlyIntegrated
  zonal_lbmp_HourlyIntegrated <- transform( cbind( Time = rep(Time,each=length(unique(zonal_lbmp_HourlyIntegrated$Zone))), zonal_lbmp_HourlyIntegrated[,-1], Type = 'Integrated' ), Price = LBMP - Losses + Congestion )
  #zonal_lbmp_HourlyIntegrated$Scarcity[ zonal_lbmp_HourlyIntegrated$Scarcity == "" ] <- "N"
  zonal_lbmp_HourlyIntegrated <- zonal_lbmp_HourlyIntegrated[,c(1:2,7,4:6,8)]

  # Process zonal_load_DayAheadForecast
  zonal_load_DayAheadForecast <- cbind( Time = Time, zonal_load_DayAheadForecast[,c(-1,-13)])
  zonal_load_DayAheadForecast <- gather( zonal_load_DayAheadForecast, 'Zone', 'Load', 2:12) %>% cbind( Type = 'DayAhead' )
  zonal_load_DayAheadForecast <- zonal_load_DayAheadForecast[,c(1:2,4,3)]

  # Process zonal_load_HourlyIntegrated
  zonal_load_HourlyIntegrated <- cbind( Time = rep(Time,each=dim(zonal_load_HourlyIntegrated)[1]/H), zonal_load_HourlyIntegrated[,c(3,6,5)]  )
  names(zonal_load_HourlyIntegrated)[c(2,4)] <- c('Zone','Load')

  ###############################################################################
  # Combine LBMP data
  zonal_lbmp <- rbind( zonal_lbmp_DayAheadForecast, zonal_lbmp_HourlyIntegrated)
  for( zone in c('H Q','NPX','O H','PJM') ){
    zonal_lbmp <- zonal_lbmp[!(zonal_lbmp$Zone == zone ),]
  }
  zonal_lbmp$Zone <- as.factor( zonal_lbmp$Zone )
  zonal_lbmp$Type <- as.factor( zonal_lbmp$Type )

  # Combine Load data
  zonal_load <- rbind( zonal_load_DayAheadForecast, zonal_load_HourlyIntegrated)
  zonal_load$Zone <- as.factor( str_to_upper(zonal_load$Zone) )


  return( full_join(zonal_lbmp,zonal_load,by = c("Time", "Zone", "Type")) )

  #############################
  # Other

  # ### 5-minute data, beyond scope
  # zonal_lbmp_RT <- read_csv('./Testing/2013realtime_zone.csv')
  # # Time Stamp, Name, PTID, LBMP, Losses, Congestion
  #
  # zonal_load_RT <- read_csv('./Testing/2013pal.csv')
  # # Time Stamp, Time Zone, Name, PTID, Load
  #
  # RealTime = concatenate zonal_lbmp_RT and zonal_load_RT
}
Reduce_NYISO_Data <- function(Data){
  Combine_Zones <- function(input,z1,z2){
    Data <- subset( input, Zone == z1 | Zone == z2)

    output <- Data[,c(1,3,8)] %>% group_by( Time, Type ) %>% summarise_each( funs(sum(.,na.rm=FALSE))) %>% transform( Zone = 'ZONE_HI')

    temp <- cbind( Data[,c(1,3)], Data[,4:7]*Data$Load ) %>% group_by( Time, Type ) %>% summarise_each( funs(sum(.,na.rm=FALSE)))
    temp[,3:6] <- round(temp[,3:6]/output$Load,2)

    output <- cbind(output,temp[,3:6])
    return( rbind( subset(input,Zone!=z1&Zone!=z2), output[,c(1,4,2,5:8,3)] ) )
  }

  output <- Combine_Zones(Data,'DUNWOD','MILLWD')

  return( subset( output, Zone != 'NORTH' ) )
}

Import_Temp_Data <- function( years, stations ) {
  Import_Station_Data <- function ( years, station ) {
    Zone <- switch( station,
                    'KBUF' = "WEST",
                    'KROC' = "GENESE",
                    'KSYR' = "CENTRL",
                    'KBPG' = NA, #"NORTH" ,
                    'KMSS' = NA, #"NORTH" ,
                    'KRME' = "MHK VL", 'KBGM' = "MHK VL",
                    'KALB' = "CAPITL",
                    'KPOU' = "HUD VL",
                    'KHPN' = "ZONE_HI", #MILLWD",
                    'KLGA' = NA, #"DUNWOD",
                    'KJFK' = "N.Y.C.",
                    'KHWV' = "LONGIL", 'KISP' = "LONGIL"
    )

    dir <- getwd()

    H <- 0
    data <- NULL
    for (year in years) {
      H <- H + ( 8760 + if(leap_year(year)){24}else{0} )

      setwd(sprintf('%s/Data/%s',dir,year))
      data <- rbind( data, read_csv(sprintf('%s-%s.csv',year,station), skip = 8, col_names = c('Station','Time','Pressure','T_Air','RH','Wind_Speed','T_Dew','Pressure2') ) )
      setwd(dir)
    }

    #  Jan1UTC = sprintf('1/1/%s 5:00:00',years[1])
    #  Time <- with_tz( mdy_hms(Jan1UTC, tz="UTC") + (0:(H-1))*hours(1),tz='America/New_York')
    Jan1 = sprintf('1/1/%s 0:00:00',years[1])
    Time <- mdy_hms(Jan1) + (0:(H-1))*hours(1)


    data$Time <- ymd_hms( data$Time, tz = 'America/New_York')
    data <- cbind( Time = round(data$Time,units='hours'), data[,c(-1,-2)] ) %>%
      group_by(Time) %>%
      summarise_each( funs(mean(.,na.rm=TRUE)))

    data$Pressure <- data$Pressure / 1000 # Pascals -> kilo-Pascals

    # http://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
    # http://www.srh.noaa.gov/images/epz/wxcalc/windChill.pdf

    Temp_Data <- left_join(as.data.frame(Time),data,by='Time') %>%
      mutate(HI = ifelse( T_Air < 50, NA,
                          ifelse( T_Air < 80,
                                  0.5*( T_Air + 61 + 1.2*(T_Air-68) + 0.094*RH ),
                                  -42.379 + (2.04901523*T_Air) + (10.14333127*RH) - (0.22475541*T_Air*RH) - (6.83783*10^-3*T_Air^2) - (5.481717*10^-2*RH^2) + (1.22874*10^-3*T_Air^2*RH) + (8.5282*10^-4*T_Air*RH^2) - (1.99*10^-6*T_Air^2*RH^2)
                                  - ifelse( RH < 13 & (T_Air<=112 & T_Air>=80), ((13-RH)/4)*sqrt((17-abs(T_Air-95))/17 ), 0 )
                                  + ifelse( RH > 85 & (T_Air<=87  & T_Air>=80), (RH-85)/10 * (87-T)/5, 0 ) ) ),
             WC = ifelse( T_Air < 50,
                          ifelse( Wind_Speed > 3, 35.74 + (0.6215*T_Air) - (35.75*Wind_Speed^0.16) + (0.4275*T*Wind_Speed^0.16), 0 ),
                          NA ) )

    cbind( Time = Temp_Data[,1],  Zone, Type ='Integrated', Temp_Data[,c(-1,-7)] )



    # Wind Chill
    #
  }


  Data <- NULL
  for ( station in stations ) {
    print(station)
    Data <- rbind( Data, Import_Station_Data( years, station ))
  }

  return( Data )
}
Reduce_Temp_Data <- function(Data){
  Data$Zone[Data$Zone == 'MILLWD'] <- 'ZONE_HI'
  return( subset( Data, Zone != 'DUNWOD' & Zone != 'NORTH'  ) )
}

Import_NYISO_Temp_Forecast <- function( years ){
  dir <- getwd()
  LFnames <- c('Time','YMD','Vintage','Zone','Max.T_Dry.DA','Min.T_Dry.DA','Max.T_Wet.DA','Min.T_Wet.DA')

  Data <- NULL
  for( year in years ){
    setwd( sprintf('%s/Data/%s', dir, year) )


    Data <- rbind( Data, read_csv(sprintf('%s-lfweather.csv',year), skip = 1, col_names = LFnames ) )

  }

  for( ID in Data$Zone ){
    Zone <- switch( ID,
                    'BUF' = "WEST",
                    'ROC' = "GENESE",
                    'SYR' = "CENTRL",
                    'MSS' = "NORTH" ,
                    'BGM' = "MHK VL",
                    'ALB' = "CAPITL",
                    'POU' = "HUD VL",
                    'HPN' = "ZONE_HI", #"MILLWD",
                    'LGA' = NA, #"DUNWOD",
                    'JFK' = "N.Y.C.",
                    'ISP' = "LONGIL",
                    'ART' = NA,
                    'ELM' = NA,
                    'MSV' = NA,
                    'PLB' = NA,
                    'SWF' = NA,
                    'UCA' = NA
    )
    Data$Zone[Data$Zone == ID] <- Zone
  }
  setwd(dir)
  Data <- Data[!is.na(Data$Zone),]
  Data$Zone <- as.factor(Data$Zone)
  Data$YMD <- mdy(Data$YMD)
  return( Data[, c(-1,-3) ] )
}

years <- 2016
NYISO_Data <- Import_NYISO_Data(years)

Temp_Data <- Import_Temp_Data( years, list('KBUF','KROC','KSYR','KBGM','KALB','KPOU','KHPN','KJFK','KISP') ) # 'KMSS','KLGA',
NYISO_Temp_Data <- Import_NYISO_Temp_Forecast(years)

NYISO_Data <- NYISO_Data %>% Reduce_NYISO_Data
# Temp_Data <- Temp_Data %>% Reduce_Temp_Data

rm( Import_NYISO_Data, Reduce_NYISO_Data, Import_Temp_Data, Reduce_Temp_Data, Import_NYISO_Temp_Forecast)

###################################################
## Data Processing & Reduction

Calculate_MAPE <- function( actual, forecast ){
  return( 100*sum(abs(actual-forecast)/forecast)/length(actual))
}

Calculate_Daily_Values <- function( input ){
  output <- NULL
  for( zone in unique(input$Zone) ){
    data <- subset( input, Zone == zone )

    # idea: price metrics in $ instead of $/MW

    mysum <- data[,-5] %>% group_by( YMD, YR, MM, DY, Zone, Type ) %>% summarise_each( funs(sum(.,na.rm=FALSE))) %>% transform( Summary = 'Sum' )
    mysum[,c(7:10,12:18)] <- NA
    mymin <- data[,-5] %>% group_by( YMD, YR, MM, DY, Zone, Type ) %>% summarise_each( funs(min(.,na.rm=TRUE ))) %>% transform( Summary = 'Min' )
    mymax <- data[,-5] %>% group_by( YMD, YR, MM, DY, Zone, Type ) %>% summarise_each( funs(max(.,na.rm=TRUE ))) %>% transform( Summary = 'Max' )
    myavg <- data[,-5] %>% group_by( YMD, YR, MM, DY, Zone, Type ) %>% summarise_each( funs(mean(.,na.rm=TRUE ))) %>% transform( Summary = 'Mean' )

    temp <- bind_rows( mysum, mymin, mymax, myavg )
    output <- rbind( output, temp )
  }

  return( output[,c(1:6,19,7:18)] )
}
Add_Variables <- function( input ){

  MM <- input$MM
  DY <- input$DY

  Day.of.Week <- as.factor( wday(input$YMD,label=TRUE,abbr=FALSE) )
  is.Weekend <- Day.of.Week == 'Saturday' | Day.of.Week == 'Sunday'
  is.Holiday <- MM == 7 & ( (DY == 4 & !is.Weekend) | (DY==3 & Day.of.Week == 'Friday') | (DY==5 & Day.of.Week == 'Monday') ) # July 4


  # 1, Spring: April & May
  # 2, Summer: June -> August
  # 3, Autumn: September & October
  # 4, Winter: November -> Mar

  Season <- matrix( data = 'Winter', nrow = length(MM) )
  Season[between(MM, 4, 5)] <- 'Spring'
  Season[between(MM, 6, 8)] <- 'Summer'
  Season[between(MM, 9,10)] <- 'Autumn'
  Season <- as.factor(Season)


  return( bind_cols( input, as.data.frame(Day.of.Week), as.data.frame(is.Weekend), as.data.frame(is.Holiday), as.data.frame(Season) ) )
}

Widen_Daily_Values <- function( input ){
  temp <- NULL
  for( level in levels(input$Summary) ){

    data <- subset( input, Summary == level )
    vars <- data[,c(1:6,20:22)]
    data <- data[,8:19]
    names(data) <- paste( level, names(data), sep='.')

    temp <- bind_cols(temp,data)
  }
  temp <- bind_cols(vars,temp)

  output <- NULL
  for( level in levels(temp$Type) ){
    if( level == 'DayAhead'){ ext = 'DA' }
    else{ ext = 'RT' }

    data <- subset( temp, Type == level )
    vars <- data[,c(1:5,7:9)]
    data <- data[,-1:-9]
    names(data) <- paste( names(data), ext, sep='.')

    output <- bind_cols(output,data)
  }
  return(bind_cols(vars,output))
}

Merged.Data <- full_join( NYISO_Data, Temp_Data, by = c("Time", "Zone", "Type") )


Hourly.Data <- Merged.Data  %>%
  separate(Time,c('YMD','HMS'),sep=' ') %>%
  separate(YMD,c('YR','MM','DY'),sep='-',remove=FALSE) %>%
  separate(HMS,c('HB'),sep=':',extra='drop')
Hourly.Data$YMD <- ymd(Hourly.Data$YMD)
Hourly.Data$YR <- as.numeric(Hourly.Data$YR)
Hourly.Data$MM <- as.numeric(Hourly.Data$MM)
Hourly.Data$DY <- as.numeric(Hourly.Data$DY)
Hourly.Data$HB <- as.numeric(Hourly.Data$HB)

Hourly.Data <- subset( Hourly.Data, between(MM,6,8) )
Daily.Data  <- Add_Variables( Calculate_Daily_Values( Hourly.Data ) )
Hourly.Data <- Add_Variables( Hourly.Data )

Hourly.Data$Zone <- as.factor(Hourly.Data$Zone)
Hourly.Data$Type <- as.factor(Hourly.Data$Type)

Daily.Data$Zone <- as.factor(Daily.Data$Zone)
Daily.Data$Type <- as.factor(Daily.Data$Type)
Daily.Data$Summary <- as.factor(Daily.Data$Summary)


Daily.Data.Wide <- left_join( Daily.Data %>% Widen_Daily_Values, NYISO_Temp_Data )
Daily.Data.Wide$Zone <- as.factor( Daily.Data.Wide$Zone )



rm( NYISO_Data, Temp_Data, NYISO_Temp_Data, Merged.Data, Calculate_Daily_Values, Add_Variables, Widen_Daily_Values )



##
## Data Vis




# # Plot Load vs Air Temp
# ggplot( data = subset(Daily.Data, Summary == 'Max' & between(MM,6,8))  ) +
#   geom_point( aes( x = T_Air, y = Load ), alpha = .1) +
#   facet_grid(MM~Zone) +
#   labs(title='Load vs Temperature by Load Zone and Month') +
#   xlab('Temperature (F)') +
#   ylab('Load (MW)') +
#   scale_x_continuous(limits=c(60,100)) +
#   scale_y_continuous(limits=c(0,3000))



##
## Linear Model



Apply_LM <- function(lm.data,ci,quiet){

lag1 <- lm.data[,-1:-8]
lag1[-1,] <- lag1[-nrow(lag1),]
lag1[1,] <- NaN
names(lag1) <- paste( names(lag1), 'L1', sep='.')

lm.data <- bind_cols(lm.data,lag1)
rm(lag1)
#lm.data <- subset(lm.data, !is.Holiday)

lm.data <- lm.data %>% transform(
    is.Mon = Day.of.Week == 'Monday',
    is.Tue = Day.of.Week == 'Tuesday',
    is.Wed = Day.of.Week == 'Wednesday',
    is.Thu = Day.of.Week == 'Thursday',
    is.Fri = Day.of.Week == 'Friday',
    is.Sat = Day.of.Week == 'Saturday',
    is.Sun = Day.of.Week == 'Sunday'
)

# convert factor to integer
# lubridate

# Extract last year as test for linear model, all other years are used to define model
lm.test <- subset( lm.data, YR == max(years) )
lm.data <- subset( lm.data, YR != max(years) )

modDA.H <- lm( data = lm.data, Max.Load.RT  ~ Max.T_Dry.DA + Min.T_Dry.DA + Max.T_Wet.DA + Min.T_Wet.DA + Mean.T_Air.RT.L1 + Mean.T_Dew.RT.L1 + Mean.Load.RT.L1 + is.Mon + is.Tue + is.Wed + is.Thu + is.Fri + is.Sat + is.Holiday )
modDA.M <- lm( data = lm.data, Mean.Load.RT ~ Max.T_Dry.DA + Min.T_Dry.DA + Max.T_Wet.DA + Min.T_Wet.DA + Mean.T_Air.RT.L1 + Mean.T_Dew.RT.L1 + Mean.Load.RT.L1 + is.Mon + is.Tue + is.Wed + is.Thu + is.Fri + is.Sat + is.Holiday )
modDA.L <- lm( data = lm.data, Min.Load.RT  ~ Max.T_Dry.DA + Min.T_Dry.DA + Max.T_Wet.DA + Min.T_Wet.DA + Mean.T_Air.RT.L1 + Mean.T_Dew.RT.L1 + Mean.Load.RT.L1 + is.Mon + is.Tue + is.Wed + is.Thu + is.Fri + is.Sat + is.Holiday )


if( quiet == FALSE ){
# return( summary(modDA.H) )
# return( summary(modDA.M) )
return( summary(modDA.L) )
}else{
  return( transform( lm.test, lm.H = predict.lm(object=modDA.H,newdata=lm.test,interval='prediction',level=ci),
                              lm.M = predict.lm(object=modDA.M,newdata=lm.test,interval='prediction',level=ci),
                              lm.L = predict.lm(object=modDA.L,newdata=lm.test,interval='prediction',level=ci))
  )
  }
}
prediction <- Apply_LM( subset(Daily.Data.Wide, Zone == 'ZONE_HI' ), 0.95, TRUE )

Calculate_MAPE(prediction$Max.Load.RT ,prediction$lm.H.fit)
Calculate_MAPE(prediction$Mean.Load.RT,prediction$lm.M.fit)
Calculate_MAPE(prediction$Min.Load.RT ,prediction$lm.L.fit)




# # Plot Actual vs Predicted Load
# ggplot(data=prediction) +
#   geom_errorbar(aes(x=YMD,ymin=lm.H.lwr,ymax=lm.H.upr)) +
#   geom_point(aes(x=YMD,y=Max.Load.RT)) +
#   labs(title='N.Y.C. Actual vs. Predicted (95% Confidence)') +
#   xlab('2015')  +
#   ylab('Load (MW)')
