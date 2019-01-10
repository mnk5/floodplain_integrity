#-------------------------------------------
#  Calculates density of shapefile per HUC-12
#
#-------------------------------------------

#Import system modules
import sys, string, os, arcpy, math, traceback, glob
import pandas as pd
import numpy
from arcpy.sa import *

# Allow output to overwrite...
arcpy.env.overwriteOutput = True

# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")



try:


    #INPUT ARGUMENTS FOR GIS TOOL
    SHP_FLDR = arcpy.GetParameterAsText(0)		 # Folder containing shapefiles trimmed
    FP = arcpy.GetParameterAsText(1)			 # FP intesected with HUC-12 shapefile
    
#    #INPUT ARGUMENTS FOR PYTHON DIRECTLY
#    SHP_FLDR = "C:\Users\mnk5\Documents\GIS\DATA\Datasets_trimmed	"	 # Folder containing shapefiles trimmed
#    FP = "C:\Users\mnk5\Documents\GIS\DATA\Datasets_trimmed\ForProcessing\
    
    #OUTPUTFOLDER
    Out_path= SHP_FLDR +"\\RESULTS" 

    #Creating the new folder
    if not os.path.exists(Out_path):
        os.makedirs(Out_path)

    #GETTING FILES
    arcpy.AddMessage('')
    arcpy.AddMessage('-----------------------------------------')
    arcpy.AddMessage('ACCESSING SHAPEFILES')
    arcpy.AddMessage(' ')
    
    # Get area of FP by HUC-12
    arcpy.env.workspace = SHP_FLDR
    FILES = arcpy.ListFeatureClasses()
    
    arcpy.AddMessage('TABULATING FEATURE ABUNDANCE BY HUC-12')
    arcpy.AddMessage(' ')
    
    
#    HUC12_area = arcpy.da.SearchCursor(FP, "Area_km2")
    # write Floodplain area and HUC-12 identifier to Numpy Array
    HUC12_area = arcpy.da.FeatureClassToNumPyArray(FP, ["HUC12","Area_km2"])
    # Convert Numpy array to Pandas dataframe (see http://geospatialtraining.com/tutorial-creating-a-pandas-dataframe-from-a-shapefile/)
    FP_df = pd.DataFrame(HUC12_area)
    
    for fc in FILES:
        
        filename  = os.path.splitext(fc)[0]
#        arcpy.AddMessage(filename) # to test correct files are being accessed
        
        # Trim to only floodplain extents and divide by HUC-12
        inFeatures = [fc, FP]
        arcpy.Intersect_analysis(inFeatures, Out_path + "\\OutTrim.shp")
        desc = arcpy.Describe(fc)
        
        # Location to save files
        OutTrim = Out_path + "\\OutTrim.shp"
        OutTable = Out_path + "\\" + filename + "_table.csv"
        
        if desc.shapeType == "Point":
#        # add column of count per huc 12 
            arcpy.Statistics_analysis(OutTrim, OutTable, [["FID","COUNT"]], "HUC12")
                        
        # Calculate density of points as number/ km^2 per HUC-12 and add to csv
            df = pd.read_csv(OutTable)
            # merge tables of HUC-12 FP area and objects, keeping all HUC-12 entries that have a feature in them
            df_results = df.merge(FP_df, on = "HUC12", how='left')
            df_results.rename(index=str, columns={"Area_km2": "FP_Area_km2"})
            df_results['Point_Density'] = df_results['COUNT_FID']/df_results['FP_Area_km2']
            df_results.to_csv(OutTable)
        
        
        elif desc.ShapeType == "Polyline": 
            
        # Calculate length of trimmed lines
            arcpy.AddField_management(fc,"Length_km", "FLOAT")
            arcpy.CalculateGeometryAttributes_management(fc, [["Length_km", "LENGTH"]], "KILOMETERS" )
            
        # Save sum of length by HUC-12 
            arcpy.Statistics_analysis(OutTrim, OutTable, [["Length_km","SUM"]], "HUC12")
            
        # Calculate density of lines as km/ km^2 per HUC-12 and add to csv
            df = pd.read_csv(OutTable)
            df['HUC12_Areakm2'] = FP_df['Area_km2']
            df['Line_Density'] = df['Length_km']/df['HUC12_Areakm2']
            df.to_csv(OutTable)
            
            
        else: # for polygons 
        
        # Calculate area of trimmed polygons
            arcpy.AddField_management(fc,"area_km2", "FLOAT")
            arcpy.CalculateGeometryAttributes_management(fc, [["area_km2", "AREA"]], "SQUARE_KILOMETERS" )
            
        # Save sum of area by HUC-12 
            arcpy.Statistics_analysis(OutTrim, OutTable, [["area_km2","SUM"]], "HUC12")
            
        # Calculate density of area per HUC-12 and add to csv
            df = pd.read_csv(OutTable)
            df['HUC12_Areakm2'] = HUC12_area
            df['Area_Density'] = df['area_km2']/df['HUC12_Areakm2']
            df.to_csv(OutTable)
      
#
#    arcpy.AddMessage(' ')
#    arcpy.AddMessage('FLOODPLAIN PREPROCESSING  COMPLETED!')
except:
     
    arcpy.AddError(arcpy.GetMessages())
    arcpy.AddMessage(traceback.format_exc()) 

