#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: head.R
#
# Description: source by all R file to load global functions and project-customed variables and functions.
#              To use this framework, you should make a soft link to this file and source this file in each R file
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-20 16:52:35
#
# Last   modified: 2017-03-21 10:35:25
#
#
#

osFlag = Sys.info()[1] 
cur_dir <- getwd()
pare_dir <- dirname(cur_dir)

if(osFlag == 'Windows'){
  dir_c <- 'D:/Git/'
  dir_d <- 'D:/Data/'
}else{
  dir_c <- '~/Code/R'
  dir_d <- '~/Data'
  dir_dataCF <- file.path(dir_d,'Load_Data_Config_Failure')
  dir_dataSMT <- file.path(dir_d,'SMART')
  dir_dataDW <- file.path(dir_d,'Disk_Workload')
  dir_dataLD14 <- file.path(dir_d,'Load_Data_2014')
  dir_dataLD15 <- file.path(dir_d,'Load_Data_2015')
}

if(file.exists(file.path(cur_dir,'base_var.R'))){
  source(file.path(cur_dir,'base_var.R'))
}else if(file.exists(file.path(pare_dir,'base_var.R'))){
  source(file.path(pare_dir,'base_var.R'))
}

if(file.exists(file.path(cur_dir,'base_func.R'))){
  source(file.path(cur_dir,'base_func.R'))
}else if(file.exists(file.path(pare_dir,'base_func.R'))){
  source(file.path(pare_dir,'base_func.R'))
}
