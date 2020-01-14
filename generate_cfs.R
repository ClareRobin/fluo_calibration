#' Generate Conversion Factors
#' Make sure the dplyr and ggplot2 libraries are installed.
#'
#' @param data_film_csv the csv data file from the calibration protocol run with a Breathe-easy film
#' @param data_nofilm_csv the csv data file from the calibration protocol run without a Breathe-easy film
#' @param layout_csv calibration plate layout csv (also includes concentrations of fluorescein and microspheres)
#' @param date input date as YYYY_MM_DD
#'
#' @return saves a csv data frame with the conversion factors and two png plots of the conversion factors (for Absorbance and fluorescence)


generate_cfs<-function(data_film_csv, data_nofilm_csv, layout_csv, date){

  `%>%` <- magrittr::`%>%` #loading %>% operator from magrittr package

  all_data <- c(data_film_csv, data_nofilm_csv)
  plate_layout <- read.csv(layout_csv)
  lid_types <- c('film','nofilm')


  #### Parse data ####
  all_values <- c()
  for (data_csv in all_data) {

    values <- read.table(data_csv, sep = ",", blank.lines.skip = T, header = F,
                         stringsAsFactors = F,fileEncoding="latin1") #read the csv file
    start_time_idx <- which(values[,1] == "Start Time") #get start and end time ids
    end_idx <- which(values[,1] == "End Time")
    names_idx <- which(values[,1] == 'Name')
    names_idx <- names_idx[2:length(names_idx)] #remove the first start time entry which just details plate type
    end_of_file <- F

    lid_type_values <- c()
    for (i in 1:length(start_time_idx)) {
      block_name <- values[names_idx[i], 2] # record name of what is being measured

      block_start <- start_time_idx[i]+4 # find start and end of measurement block
      block_end_idx <- end_idx[i]-3

      new_block <- values[(block_start):(block_end_idx),1:2] # grab and name the data
      names(new_block)[1] <- "well"
      names(new_block)[2] <- "value"

      joined_block <- dplyr::full_join(plate_layout, new_block) #join to plate layout csv, add measurement category
      joined_block$measure <- block_name

      lid_type_values <- rbind(lid_type_values, joined_block) #add to all data
    }
    lid_type_values$lid_type <- lid_types[match(data_csv,all_data)] #add the lid type
    all_values <- rbind(all_values, lid_type_values) #join to all the data values.
  }


  ### calculate mean of 4 replicates and remove saturating values ###
  sorted_values <- subset(all_values, value != 'OVER')
  means<-aggregate(as.numeric(sorted_values$value),by=list(sorted_values$measure,sorted_values$concentration,sorted_values$Calibrant,sorted_values$lid_type),FUN=mean)
  colnames(means)<-c('measure','concentration','calibrant','lid_type','mean_value') #add the correct column names
  microsphere_values<-subset(means, (measure=='Abs600'|measure=='Abs700')&(calibrant=='microspheres'))
  fluorescein_values<-subset(means, (calibrant=='fluorescein')&(measure!='Abs600'&measure!='Abs700'))
  all_values <- rbind(microsphere_values, fluorescein_values)




  #### normalise data and calculate conversion factors####
  conversion_factors <- c()
  all_normalized_conversion_factor_data <- c()

  for (i in lid_types) {
    normalized_data <- c()
    lid_type_values <- subset(all_values, lid_type == i)
    measures <- unique(lid_type_values$measure)

    for (type_of_measure in measures) {
      value_of_blank <- subset(lid_type_values, measure==type_of_measure&concentration==0)[1,5] #get the value of the blank for the specific measure (ex. Gain 40, Abs600)
      data_values_of_measure <- subset(lid_type_values, measure==type_of_measure) #get the other data values for that measure

      new_normalized_values <- data_values_of_measure%>%
        dplyr::mutate(normalized_value=mean_value-value_of_blank) #normalize the data values for the specific measure against the blank
      normalized_data<-rbind(normalized_data, new_normalized_values) #add to the normalized data
    }

    #####calculate conversion factors ####

    MEFL_per_uM <- 6.02E+13 #MEFL/uM fluorescein (value from Jacob Beal's iGEM conversion excel spreadsheet)
    fluorescence_normalized_data <- subset(normalized_data, calibrant == 'fluorescein'&concentration!=0) #exclude fluorescein concentration of 0 to avoid dividing by 0
    microsphere_normalized_data <- subset(normalized_data, calibrant == 'microspheres'&concentration>8.139678e+06) #only take the top 8 data points to avoid low concentration variability

    normalized_conversion_factor_data <- rbind(fluorescence_normalized_data,microsphere_normalized_data)
    all_normalized_conversion_factor_data <- rbind(all_normalized_conversion_factor_data,normalized_conversion_factor_data)

    #average and store final conversion factors
    lid_type_conversion_factors <- c()
    for (type_of_measure in measures) {
      measure_subset <- subset(normalized_conversion_factor_data, measure==type_of_measure)
      calibrant <- measure_subset[1,3]

      if(calibrant == 'fluorescein') {
        fitted_conversion_factor <- MEFL_per_uM/(lm(formula=normalized_value~concentration,data=measure_subset)$coefficients[2])
      } else {
        fitted_conversion_factor <- (lm(formula=normalized_value~concentration,data=measure_subset)$coefficients[2])
      }

      lid_type_conversion_factor <- data.frame(type_of_measure,fitted_conversion_factor, calibrant) #create data frame of conversion factors
      lid_type_conversion_factor$lid <- i #add lid-type to conversion factor df
      lid_type_conversion_factors <- rbind(lid_type_conversion_factors, lid_type_conversion_factor)
    }
    conversion_factors <- rbind(conversion_factors,lid_type_conversion_factors)
  }

  conversion_factors <- conversion_factors %>% #rename the columns
    dplyr::rename('measure' = type_of_measure,
                  'conversion_factor' = fitted_conversion_factor)


  ### plot the mean normalized values ###
  Abs_plot <- ggplot2::ggplot(subset(all_normalized_conversion_factor_data, calibrant=='microspheres'), ggplot2::aes(x=concentration, y=normalized_value)) + ggplot2::geom_point() +
    ggplot2::scale_y_log10()+
    ggplot2::scale_x_log10()+
    ggplot2::xlab('Number of Microspheres') +
    ggplot2::ylab('Normalized Absorbance')+
    ggplot2::geom_smooth(method='lm', formula=y~x) + #fit a line
    ggplot2::facet_grid(measure ~ lid_type, scales = "free_x") +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::theme(strip.text.x = ggplot2::element_text(size = 12), strip.text.y = ggplot2::element_text(size = 12))

  Fluo_plot <- ggplot2::ggplot(subset(all_normalized_conversion_factor_data, calibrant=='fluorescein'), ggplot2::aes(x=concentration, y=normalized_value)) +
    ggplot2::geom_point() +
    ggplot2::scale_y_log10()+
    ggplot2::scale_x_log10()+
    ggplot2::xlab('Fluorescein Concentration (uM)') +
    ggplot2::ylab('Normalized Fluorescence (a.u.)')+
    ggplot2::geom_smooth(method='lm', formula=y~x) +
    ggplot2::facet_grid(measure ~ lid_type, scales = "free_x") +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())

  #save conversion factors to a csv
  dir.create('cf_results')
  write.csv(conversion_factors,paste('cf_results/cfs_generated_',date,'.csv', sep=''), row.names = FALSE)

  #save plots
  ggplot2::ggsave(paste('cf_results/Absorbance_conversion_factors_',date,'.png', sep=''),plot=Abs_plot)
  ggplot2::ggsave(paste('cf_results/Fluorescence_conversion_factors_',date,'.png', sep=''),plot=Fluo_plot, width =12, height = 25, units='cm')

  return()
}
