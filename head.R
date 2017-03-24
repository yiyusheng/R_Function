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

osFlag = Sys.info()[1] == 'Windows' 
cur_dir <- getwd()
pare_dir <- dirname(cur_dir)

if(osFlag){
  dir_c <- 'D:/Git/'
  dir_d <- 'D:/Data/'
}else{
  dir_c <- '/home/yiyusheng/Code/R/'
  dir_d <- '/home/yiyusheng/Data/'
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
