# Import required packages
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

# Check out required extensions
arcpy.CheckOutExtension("3D")
arcpy.CheckOutExtension("Spatial")


# Function to convert an ArcGIS table into a pandas DataFrame
def arcgis_table_to_df(in_fc, input_fields=None, query=""):
    """Convert an ArcGIS table into a pandas DataFrame.
    Args:
        in_fc (str): Input feature class or table to convert.
        input_fields (list): Fields to input to a da search cursor for retrieval.
        query (str): SQL query to grab appropriate values.

    Returns:
        pandas.DataFrame: A DataFrame representation of the ArcGIS table.
    """
    OIDFieldName = arcpy.Describe(in_fc).OIDFieldName
    if input_fields:
        final_fields = [OIDFieldName] + input_fields
    else:
        final_fields = [field.name for field in arcpy.ListFields(in_fc)]
    data = [row for row in arcpy.da.SearchCursor(in_fc, final_fields, where_clause=query)]
    fc_dataframe = pd.DataFrame(data, columns=final_fields)
    fc_dataframe = fc_dataframe.set_index(OIDFieldName, drop=True)
    return fc_dataframe


# Set the workspace
env.workspace = "C:/path/to/your/workspace"
env.overwriteOutput = True
start_time = time.time()

timesteps = ["2020"]

# Use a relative path for input and output files
input_folder = "input_data/"
temp_folder = "temp/"
output_folder = "output_data/"

for ts in timesteps:
    # Input Settings
    obb_centroids = temp_folder + "obb_centroids.shp"
    base_network = temp_folder + "base_network.shp"
    noIntersection_network = temp_folder + "no_intersection_network.shp"

    # Load the footprint file and obtain its spatial reference
    footprint = input_folder + "footprint.shp"
    dsc = arcpy.Describe(footprint)
    coord_sys = dsc.spatialReference  # Get coordinate systems

    # Time the process
    print("--- %s start calculations ---" % (round(time.time() - start_time)))
    n_time = time.time()

    # Repair the footprint geometry and create an oriented bounding box
    arcpy.management.RepairGeometry(footprint)

    # Calculate rectangularity and elongation of the footprint
    footprint_rectangle = arcpy.management.MinimumBoundingGeometry(footprint, r"temp\footprint_rectangle",
                                                                   "RECTANGLE_BY_WIDTH", mbg_fields_option="MBG_FIELDS")

    # Add the "elon" field and calculate elongation values
    arcpy.AddField_management(footprint_rectangle, "elon", "Double")
    with arcpy.da.UpdateCursor(footprint_rectangle, ["MBG_Width", "MBG_Length", "elon"]) as cursor:
        for row in cursor:
            row[2] = row[0] / row[1]  # Calculate elongation as the ratio of width to length
            cursor.updateRow(row)
    print("--- %s seconds to calculate rectangularity ---" % (round(time.time() - n_time)))
    n_time = time.time()

    # Transfer the elongation values to the footprint feature class
    common_id_field = "FID"
    columns_to_transfer = ["elon"]
    arcpy.AddField_management(footprint, columns_to_transfer[0], "DOUBLE")

    values_dict = {}

    with arcpy.da.SearchCursor(footprint_rectangle, [common_id_field] + columns_to_transfer) as cursor:
        for row in cursor:
            values_dict[row[0]] = row[1:]

    with arcpy.da.UpdateCursor(footprint, [common_id_field] + columns_to_transfer) as cursor:
        for row in cursor:
            common_id = row[0]
            if common_id in values_dict:
                for i, column_value in enumerate(values_dict[common_id], start=1):
                    row[i] = column_value
                cursor.updateRow(row)

    # Calculate the main angle of the footprint polygons
    arcpy.AddField_management(footprint, "angle", "double")
    arcpy.cartography.CalculatePolygonMainAngle(footprint, "angle", "ARITHMETIC")

    # Add geometry attributes to the footprint_rectangle feature class
    arcpy.management.AddGeometryAttributes(footprint_rectangle, "AREA")

    # Calculate the closest distance between the polygons, this can take a very long time
    print("--- %s begin near table generation---" % (round(time.time() - n_time)))

    # Generate a near table with distances, locations, and angles between each pair of input features
    neartbl = arcpy.analysis.GenerateNearTable(footprint, footprint, r"temp\near_table.dbf", "50 meter", closest="ALL",
                                               location="LOCATION", angle="ANGLE")
    neartbl = r"temp\near_table.dbf"
    dist_poly = arcgis_table_to_df(neartbl)
    un_dis_poly = dist_poly.loc[
        pd.DataFrame(np.sort(dist_poly[['IN_FID', 'NEAR_FID']], 1), index=dist_poly.index).drop_duplicates(
            keep='first').index]
    print("--- %s seconds to calculate the near table---" % (round(time.time() - n_time)))
    n_time = time.time()

    # Calculate the centroids of the building footprints
    centroid = arcpy.FeatureToPoint_management(footprint, obb_centroids, "CENTROID")

    # Collect coordinates and IDs of the centroids
    centroid_coords = {"x": [], "y": [], "id": []}
    for row in arcpy.da.SearchCursor(centroid, ["SHAPE@XY", "OID@"]):
        x, y = row[0]
        id = row[1]
        centroid_coords["x"].append(x)
        centroid_coords["y"].append(y)
        centroid_coords["id"].append(id)

    # Create a DataFrame from the centroid coordinates
    df_centroids = pd.DataFrame.from_dict(centroid_coords, orient='columns', dtype=None, columns=None)

    # Join the centroid data to the building footprints
    final = un_dis_poly.join(df_centroids.set_index('id'), on='IN_FID')
    final = final.rename(columns={"x": "x1", "y": "y1"})
    final = final.join(df_centroids.set_index('id'), on='NEAR_FID')

    # Calculate the time taken for processing centroids
    print("--- %s seconds to finish the centroids ---" % (round(time.time() - n_time)))
    n_time = time.time()

    # Calculate the points for/and the edges of the base network
    final["from"] = final[["FROM_X", "FROM_Y"]].apply(Point, axis=1)
    final["to"] = final[["NEAR_X", "NEAR_Y"]].apply(Point, axis=1)
    final['line'] = final.apply(lambda row: LineString([row['from'], row['to']]), axis=1)  # Create a linestring column
    final = final.drop(['from', 'to'], axis=1)
    final = gpd.GeoDataFrame(final, geometry='line')
    final.to_file(base_network, driver='ESRI Shapefile')  # Centroid
    arcpy.DefineProjection_management(base_network, coord_sys)
    print("--- %s seconds to finish base network  ---" % (round(time.time() - n_time)))
    n_time = time.time()

    # Delete intersections with other footprints
    arcpy.AddField_management(footprint, "dummy", "short")
    with arcpy.da.UpdateCursor(footprint, ["dummy"]) as cursor:
        for row in cursor:
            row[0] = 1
            cursor.updateRow(row)

    arcpy.conversion.PolygonToRaster(footprint, "dummy", r'temp\f_raster_buchs.tif', "MAXIMUM_AREA", cellsize="2")
    print("--- %s seconds to rasterize  ---" % (round(time.time() - n_time)))
    n_time = time.time()
    outCon2 = Con(IsNull(r'temp\f_raster_buchs.tif'), 0, r'temp\f_raster_buchs.tif')
    arcpy.ddd.AddSurfaceInformation(base_network, outCon2, "Z_MEAN")
    print("--- %s seconds to raster line overlay  ---" % (round(time.time() - n_time)))
    n_time = time.time()

    # Remove intersecting lines from the base network
    arcpy.AddField_management(base_network, "int_leng", "double")
    with arcpy.da.UpdateCursor(base_network, ["int_leng", "Z_MEAN", "NEAR_DIST"]) as cursor:
        for row in cursor:
            row[0] = row[1] * row[2]
            cursor.updateRow(row)
            if row[0] > 1.9:
                cursor.deleteRow()
    print("--- %s seconds to clean intersections ---" % (round(time.time() - n_time)))
    n_time = time.time()

    # Use non-intersecting lines and project the edges on the centroids
    no_intersection_unique = gpd.read_file(base_network)
    no_intersection_unique = pd.DataFrame(no_intersection_unique)
    no_intersection_unique = no_intersection_unique.drop(['geometry'], axis=1)
    no_intersection_unique["from"] = no_intersection_unique[["x", "y"]].apply(Point, axis=1)
    no_intersection_unique["to"] = no_intersection_unique[["x1", "y1"]].apply(Point, axis=1)
    no_intersection_unique['line'] = no_intersection_unique.apply(lambda row: LineString([row['from'], row['to']]),
                                                                  axis=1)  # Create a linestring column
    no_intersection_unique = no_intersection_unique.drop(['from', 'to'], axis=1)
    no_intersection_unique = gpd.GeoDataFrame(no_intersection_unique, geometry='line')
    no_intersection_unique.to_file(noIntersection_network, driver='ESRI Shapefile')  # Centroid
    arcpy.DefineProjection_management(noIntersection_network, coord_sys)
    print("--- %s seconds to project on centroids ---" % (round(time.time() - n_time)))
    n_time = time.time()

    # Add a new field for similarity angles
    arcpy.AddField_management(noIntersection_network, "s_angle", "double")

    # Calculate the similarity angles between the two angle fields
    with arcpy.da.UpdateCursor(noIntersection_network,
                               ["angle_12", "angle_1", "s_angle"]) as cursor:  # ,"shape_ind", "shape_in_1"
        for row in cursor:
            row[2] = abs(math.cos(math.radians(row[0] - row[1])))
            cursor.updateRow(row)

    # Print the time taken for calculating similarities and total runtime
    print("--- total runtime: %s seconds ---" % (round(time.time() - start_time)))




