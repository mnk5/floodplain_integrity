#-------------------------------------------
#  Calculates density of shapefile per HUC-12
#
#-------------------------------------------

#Import system modules
import sys, string, os, arcpy, math, traceback, glob, numpy
from arcpy.sa import *

# Allow output to overwrite...
arcpy.env.overwriteOutput = True

# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")

#%%

try:


    #INPUT ARGUMENTS FOR GIS TOOL
    SHP_FLDR = arcpy.GetParameterAsText(0)		 # Folder containing shapefiles trimmed
    FP = arcpy.GetParameterAsText(1)			 # FP intesected with HUC-12 shapefile
    
#    #INPUT ARGUMENTS FOR PYTHON DIRECTLY
#    SHP_FLDR = "C:\Users\mnk5\Documents\GIS\DATA\Datasets_trimmed	"	 # Folder containing shapefiles trimmed
#    HUC12 = "C:\Users\mnk5\Documents\GIS\DATA\Datasets_trimmed\ForProcessing\HUC12_CO.shp"			 # HUC-12 shapefile
    
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

    arcpy.env.workspace = SHP_FLDR
    FILES = arcpy.ListFeatureClasses()
    
    for fc in FILES:
        inFeatures = [fc, FP]
        arcpy.Intersect_analysis(inFeatures, "OutTrim")
        desc = arcpy.Describe(fc)
        
        if desc.shapeType == "Point":
#        # add column of count per huc 12 
            arcpy.Statistics_analysis(OutTrim, OutTable, "COUNT", "HUC12")
#        else if desc.ShapeType == "Polyline": 
#        else 
      
#%%

#    #Get the pixelsize
#    pixelsize = float(arcpy.GetRasterProperties_management (DEM, "CELLSIZEX").getOutput(0) ) 
#    cellarea = pixelsize ** 2                            #calculate the cell area
#    
#
#
#
#    #Calculating the area of the cell
#    arcpy.env.cellSize = pixelsize
#    
#    #Extension environment
#    arcpy.env.extent = DEM
#    
#
# 
#
#    #Mask for the calculation
#    arcpy.env.mask = DEM     
#    
#    # Convert to float for raster calculator
#    SN_THRESH = float(SN_THRESH)
#
#    arcpy.AddMessage('Computing Fill Raster...')
#    OutFill  =  Fill(DEM)
#    OutFill.save(FILL)
#    
#    arcpy.AddMessage('Extracting Stream Network...')
#    outA = SetNull (FACC, FACC,  "VALUE < %f" % (SN_THRESH) ) 
#    outA.save(ACC_BL)
#    
#    
#    outAC = Raster(ACC_BL)*cellarea
#    outAC.save(ACC_BLC)
#    
#    outBD = SetNull (FACC, FILL,  "VALUE < %f" % (SN_THRESH) )
#    outBD.save(DEM_BL)
#
#    outBDc = Raster(DEM_BL) * 100
#    outBDc.save(DEM_BLcm)
#
#    outW = Watershed(FD, DEM_BLcm,  "VALUE")
#    outW.save(DEM_BLWAT)
#
#    outD = Raster(FILL)*100 - Raster(DEM_BLWAT)
#    outD.save(DEM_DIFF)
#
#    arcpy.Delete_management(ACC_BL)
#    arcpy.Delete_management(DEM_BL)
#    arcpy.Delete_management(DEM_BLcm)
#    arcpy.Delete_management(DEM_BLWAT)
#
#    arcpy.AddMessage(' ')
#    arcpy.AddMessage('FLOODPLAIN PREPROCESSING  COMPLETED!')
except:
     
    arcpy.AddError(arcpy.GetMessages())
    arcpy.AddMessage(traceback.format_exc()) 

