# Plate Reader Fluorescence Calibration

R package to generate conversion factors (cfs) for plate reader data, and to load specific cfs for specific experimental set up. 

Fluorescent cfs are in units of MEFL/a.u.
Absorbance cfs are in units of Abs/particle

To convert normalized plate reader data in units of a.u./Abs simply multiply by both cfs (MEFL/a.u. and Abs/particle). 


## Files: 
generate_cfs: creates a folder with a csv file containing cfs for each gain & absorbance, and plots of mean_normalized_value vs concentration of calibrant (should be straight lines of slope 1) 

get_cfs: creates csv file of specific cfs for specified experimental set-up based on fitting gain to cf relation, and plots gain to cf relation (with input conditions cf marked as black data point). 


## Example:

To generate cfs (example data is located in the example folder):
  1. generate_cfs ('example/calibration_20191218_film.csv','example/calibration_20191218_nofilm.csv','example/calibration_plate_layout.csv','2019_12_18') 


To extract specific cfs for a specific experiment (ex. gain=87, Abs=700, lid=no film)
  1. get_cf ('nofilm',87,700,'cf_results/cfs_generated_2019_12_18.csv')
