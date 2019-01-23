# -*- coding: utf-8 -*-
"""
Created on Fri Jan  4 12:03:59 2019

@author: mnk5
"""
import sys, string, os, math, traceback, glob
import pandas as pd
import numpy as np


#
#with open("C:\Users\mnk5\Documents\GIS\DATA\Datasets_trimmed\RESULTS\NID_CO_table.csv") as csvfile:
#    csv.reader(csvfile, delimiter = ',')


#with open("C:\\Users\\mnk5\\Documents\\GIS\\DATA\\Datasets_trimmed\\RESULTS\\NID_CO_table.csv") as csvfile:
#    writer = csv.writer(csvfile, lineterminator = '\n')
#
df = pd.read_csv("C:\\Users\\mnk5\\Documents\\GIS\\DATA\\Datasets_trimmed\\RESULTS\\NID_CO_table.csv")
FP_df = pd.read_csv("C:\\Users\\mnk5\\Documents\\GIS\\DATA\\Datasets_trimmed\\RESULTS\\FP_area.csv")
df_results = df.merge(FP_df, on = "HUC12", how='left')


