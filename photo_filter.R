# remove nef files based on name of remaining jpg files after I clear some bad photoes
dir_jpg <- '~/hdd/Photos/20170325@武汉磨山/'
dir_nef <- '~/hdd/Photos/RAW/20170325@武汉磨山/'

fname_jpg <- list.files(dir_jpg)
fname_need_nef <- gsub('JPG','NEF',fname_jpg)
fname_remove_nef <- setdiff(list.files(dir_nef),fname_need_nef)
file.remove(file.path(dir_nef,fname_remove_nef))