#packages
import arcpy
from arcpy import env
from arcpy import analysis
from arcpy import conversion
from arcpy import management
from arcpy import cartography
from arcpy import ddd
from arcpy.sa import *
import pandas as pd
import numpy as np
import geopandas as gpd
from shapely.geometry import Point, LineString
import time
import math
arcpy.CheckOutExtension("3D")
arcpy.CheckOutExtension("Spatial")

from os import path

#functions
def arcgis_table_to_df(in_fc, input_fields=None, query=""):
    """Function will convert an arcgis table into a pandas dataframe with an object ID index, and the selected
    input fields using an arcpy.da.SearchCursor.
    :param - in_fc - input feature class or table to convert
    :param - input_fields - fields to input to a da search cursor for retrieval
    :param - query - sql query to grab appropriate values
    :returns - pandas.DataFrame"""
    OIDFieldName = arcpy.Describe(in_fc).OIDFieldName
    if input_fields:
        final_fields = [OIDFieldName] + input_fields
    else:
        final_fields = [field.name for field in arcpy.ListFields(in_fc)]
    data = [row for row in arcpy.da.SearchCursor(in_fc,final_fields,where_clause=query)]
    fc_dataframe = pd.DataFrame(data,columns=final_fields)
    fc_dataframe = fc_dataframe.set_index(OIDFieldName,drop=True)
    return fc_dataframe


#setting workspace
env.workspace = "./Building_Network/"
env.overwriteOutput = True
start_time = time.time()

timesteps = [#"1899",
            # "1918",
            # "1933",
            # "1949",
            "1959",
            "1970",
            "1978",
            "1992"
            # "2004",
            # "2012",
            # "2020"
                    ]

for ts in timesteps:
    chull = "building_networks/chull_" + ts + "/chull_" + ts + ".shp"
    bnid = "building_networks/bn_" + ts + "_id.shp"
    snet = "settlement_net/sn_" + ts + ".shp"

    neartbl = arcpy.analysis.GenerateNearTable(chull, chull, r"temp\near_table.dbf", "6 kilometer", closest="ALL", location="LOCATION")
    dist_poly = arcgis_table_to_df(neartbl)
    un_dis_poly = dist_poly.loc[
        pd.DataFrame(np.sort(dist_poly[['IN_FID', 'NEAR_FID']], 1), index=dist_poly.index).drop_duplicates(
            keep='first').index]
    chull_table = arcgis_table_to_df(chull)
    chull_table['FID2'] = np.arange(len(chull_table))
    merged_left = pd.merge(left=un_dis_poly, right=chull_table[["ID", "FID2"]], how='left', left_on='IN_FID', right_on='FID2').drop(columns = ['FID2', "IN_FID"])
    merged_left.rename(columns = {'ID':'IN_ID'}, inplace = True)
    merged_left = pd.merge(left=merged_left, right=chull_table[["ID", "FID2"]], how='left', left_on='NEAR_FID', right_on='FID2').drop(columns = ['FID2', "NEAR_FID"])
    merged_left.rename(columns = {'ID':'NEAR_ID'}, inplace = True)
    connected = merged_left

    connected["IN"] = connected[["FROM_X", "FROM_Y"]].apply(Point, axis=1)
    connected["NEAR"] = connected[["NEAR_X", "NEAR_Y"]].apply(Point, axis=1)
    connected['line'] = connected.apply(lambda row: LineString([row['IN'], row['NEAR']]), axis=1)

    connected = gpd.GeoDataFrame(connected, geometry='line')
    connected = connected.drop(columns=['IN', "NEAR"])

    connected.to_file("./temp/sn.shp", driver='ESRI Shapefile')  # Centroid
    dsc = arcpy.Describe("Mittelland.shp")
    coord_sys = dsc.spatialReference
    arcpy.DefineProjection_management("./temp/sn.shp", coord_sys)
    arcpy.CopyFeatures_management("./temp/sn.shp", "./Building_Network.gdb/sn_copy")
    arcpy.MakeFeatureLayer_management("./Building_Network.gdb/sn_copy", "./Building_Network.gdb/temp_sn")
    arcpy.SelectLayerByLocation_management("./Building_Network.gdb/temp_sn", "INTERSECT",
                                           "./Building_Network.gdb/Lake_Buffer")
    arcpy.DeleteFeatures_management("./Building_Network.gdb/temp_sn")

    connected = arcgis_table_to_df("./Building_Network.gdb/sn_copy")

    columns = ['IN_ID', 'NEAR_ID']

    for column_name in columns:
        connected[column_name] = connected[column_name].apply(lambda x: x.replace('7e+05_', '700000_'))
        connected[column_name] = connected[column_name].apply(lambda x: x.replace('6e+05_', '600000_'))
        connected[column_name] = connected[column_name].apply(lambda x: x.replace('2e+05_', '200000_'))

    connected['IN_x'] = connected['IN_ID'].str[3:9].astype(float).astype(int)
    connected['IN_y'] = connected['IN_ID'].str[10:16].astype(float).astype(int)
    connected['NEAR_x'] = connected['NEAR_ID'].str[3:9].astype(float).astype(int)
    connected['NEAR_y'] = connected['NEAR_ID'].str[10:16].astype(float).astype(int)

    connected["IN"] = connected[["IN_x", "IN_y"]].apply(Point, axis=1)
    connected["NEAR"] = connected[["NEAR_x", "NEAR_y"]].apply(Point, axis=1)
    connected['line'] = connected.apply(lambda row: LineString([row['IN'], row['NEAR']]), axis=1)

    connected = gpd.GeoDataFrame(connected, geometry='line')
    connected = connected.drop(columns=['IN', "NEAR"])
    connected = connected.drop(columns=['Shape', "NEAR_RANK", "FROM_X", "FROM_Y", "NEAR_X", "NEAR_Y","Shape_Length", "IN_x", "IN_y", "NEAR_x", "NEAR_y"])

    connected.to_file("./temp/sn.shp", driver='ESRI Shapefile')  # Centroid

    dsc = arcpy.Describe("./Mittelland.shp")
    coord_sys = dsc.spatialReference
    arcpy.DefineProjection_management("./temp/sn.shp", coord_sys)

    arcpy.CopyFeatures_management("./temp/sn.shp", snet)
