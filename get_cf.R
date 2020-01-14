#' Get Conversion Factors
#' Make sure the ggplot2 library is installed.
#'
#' @param lid_type string, either 'nofilm' or 'film'.
#' @param gain numeric, range from 40-120
#' @param absorbance numeric, accepts 600 or 700
#' @param conversion_factors_csv all conversion factor csv file
#'
#'
#' @return prints the appropriate conversion factors for your settings, saves them to a csv, and saves a plot showing gain vs conversion factor for fluorescence (black circle
#' to mark the conversion factor for the specific settings)

get_cf<- function(lid_type, gain, absorbance, conversion_factors_csv) {

  conversion_factors <- read.csv(conversion_factors_csv)

  #conversion_factors <- load.csv('conversion_factors_2018')
  conversion_factors$measure_type <- ifelse(grepl("Gain",conversion_factors$measure),'fluorescein','microspheres') #add microsphere and fluorescein criteria column

  #subset absorbance conversion factors and convert to numeric data
  Abs_cf <- subset(conversion_factors, measure_type == 'microspheres')
  Abs_cf$conversion_factor <- as.numeric(as.character(Abs_cf$conversion_factor))
  Abs_cf$numeric_measure <- as.numeric(gsub('Abs','',Abs_cf$measure))

  #subset fluorescence conversion factors and convert to numeric data
  Fluo_cf <- subset(conversion_factors, measure_type == 'fluorescein')
  Fluo_cf$log10_cf <- log10(as.numeric(as.character(Fluo_cf$conversion_factor))) #takes the log10 of conversion factors
  Fluo_cf$log10_measure <- log10(as.numeric(gsub('Gain ','',Fluo_cf$measure))) #removes 'Gain ', makes gain numeric and takes the log10

  Fluo_gain_cf_fit_film <- lm(log10_cf~log10_measure,data=subset(Fluo_cf,lid=='film')) #get linear relation between gain and conversion factor of film data
  Fluo_gain_cf_fit_nofilm <- lm(log10_cf~log10_measure,data=subset(Fluo_cf,lid=='nofilm')) #get linear relation between gain and conversion factor of no film data


  #based on above linear fit, get fluroescence conversion factor for specific user settings.
  if (lid_type == 'film') {
    user_cf <- 10**(Fluo_gain_cf_fit_film$coefficients[2]*log10(gain)+Fluo_gain_cf_fit_film$coefficients[1])
  } else {
    user_cf <- 10**(Fluo_gain_cf_fit_nofilm$coefficients[2]*log10(gain)+Fluo_gain_cf_fit_nofilm$coefficients[1])
  }

  #get abs conversion conversion factor for specific user settings.
  user_abs_cf <- subset(Abs_cf, numeric_measure==absorbance & lid==lid_type)[1,2]

  #plot and display user settings and corresponding conversion factors + fit lines for gain to conversion facter relation.
  Gain_cf <- ggplot2::ggplot(Fluo_cf, ggplot2::aes(x=(as.numeric(gsub('Gain ','',Fluo_cf$measure))), y=(as.numeric(as.character(Fluo_cf$conversion_factor))), color =lid)) +
    ggplot2::geom_point() +
    ggplot2::scale_y_log10()+
    ggplot2::scale_x_log10()+
    ggplot2::xlab('Gain') +
    ggplot2::ylab('Conversion factor (MEFL/a.u.)')+
    ggplot2::geom_smooth(method='lm', formula=y~x) +
    ggplot2::geom_point(ggplot2::aes(x=gain, y=user_cf), colour='black', shape = 1, size=2)+
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::annotate(geom='text',x=40, y=1.5e+08, label=paste("Gain:", gain),hjust = 0) +
    ggplot2::annotate(geom='text',x=50, y=1.5e+08, label=paste("Lid:", lid_type),hjust = 0) +
    ggplot2::annotate(geom='text',x=63, y=1.5e+08, label=paste("Abs:", absorbance),hjust = 0) +
    ggplot2::annotate(geom='text',x=40, y=1e+08, label=paste("Fluorescence conversion:", format(signif(user_cf,digits=3),scientific=TRUE), 'MEFL/a.u.'),hjust = 0) +
    ggplot2::annotate(geom='text',x=40, y=0.7e+08, label=paste("Absorbance conversion:", format(signif(user_abs_cf,digits=3)), 'particles/Abs'),hjust = 0)

  print(paste('For your settings: Gain =',gain,' Absorbance =', absorbance,'nm and lid type =', lid_type,' the conversion factors are: ', user_cf, 'MEFL/a.u. for fluorescence and',user_abs_cf,'particles/Abs for absorbance.'))

  cf_df <- data.frame('Gain'=gain, 'Lid Type'=lid_type, 'MEFL/a.u.'= user_cf, 'Particles/Abs'=user_abs_cf, check.names=FALSE)
  write.csv(cf_df,'your_cfs.csv', row.names = FALSE)
  ggplot2::ggsave('Your_cfs_gain_relation.png',plot=Gain_cf)

  return()

}

