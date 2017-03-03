# Move NEF from SD card to computer
library(stringr)
dir_in <- 'G:/DCIM/616ND750'
dir_out <- 'C:/Users/york/Desktop/NEF'
idx <- c(152,163,169,188,255,286,301,302,307,320,328,330,337,353,374,385,
         434,473,475,508,529,565,577,611,630,643,649,663,745,749,767,
         814,820,853,865,884,888,896,900,923,1005,1014,1030,1031,1033,1046,
         1054,1066,1071,1098,1103,1105,1107,1136,1148,1162,1165,1178)
idx <- str_pad(idx,4,pad = '0')
name_file <- paste('DSC_',idx,'.NEF',sep='')
path_file <- file.path(dir_in,name_file)
file.copy(path_file[file.exists(path_file)],dir_out)
