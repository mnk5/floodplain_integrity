# -*- coding: utf-8 -*-
"""
Created on Wed Jan 16 09:28:54 2019

@author: mnk5
"""

import pandas as pd


## calculating hydrologic alteration methods with various weightings

attributefile = "C:\\Users\\mnk5\\Documents\\GIS\DATA\\Hyd_Alteration\\ByHUC12\\attributetable.txt"

df = pd.read_table(attributefile, delimiter=",")

hydfields = ["pnMH20", "pnFH1", "pnFH6", "pnFH7", "pnDH1", "pnDH2","pnDH3","pnDH4","pnDH5","pnDH15" ]

df_hydalt = df[hydfields]


# Weight by length

df_lengthweight = df_hydalt.multiply(df["Length_km"], axis="index")

df_lengthweight["HUC12"] = df["HUC12"]

df_lengthsum = df.groupby('HUC12')['Length_km'].sum()
df_HA_byLength = df_lengthweight.groupby('HUC12').sum()

df_HA_byLength = df_HA_byLength.divide(df_lengthsum, axis="index")


# Weight by Stream Order

df_SOweight = df_hydalt.multiply(df["StrmOrder"], axis="index")

df_SOweight["HUC12"] = df["HUC12"]

df_SOsum = df.groupby('HUC12')['StrmOrder'].sum()
df_HA_bySO = df_SOweight.groupby('HUC12').sum()

df_HA_bySO = df_HA_bySO.divide(df_SOsum, axis="index")

# Calc max and avg

df_hydalt["HUC12"] = df["HUC12"]

df_maxHA = df_hydalt.groupby('HUC12').max()
df_meanHA = df_hydalt.groupby('HUC12').mean()

# save files to csv to use with GIS

df_HA_byLength.to_csv("C:\\Users\\mnk5\\Documents\\GIS\DATA\\Hyd_Alteration\\ByHUC12\\LengthWeightbyHUC12.csv")
df_HA_bySO.to_csv("C:\\Users\\mnk5\\Documents\\GIS\DATA\\Hyd_Alteration\\ByHUC12\\SOWeightbyHUC12.csv")