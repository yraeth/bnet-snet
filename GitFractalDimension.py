import arcpy
import numpy as np
import math
import os
import pandas as pd
from arcpy import env
arcpy.env.overwriteOutput = True


from concurrent.futures import ThreadPoolExecutor

arcpy.env.overwriteOutput = True



def create_fishnet_and_spatial_join(output_fishnet, output_spatial_join, cell_size):
    # Create a fishnet based on the current settlement extent
    arcpy.CreateFishnet_management(output_fishnet,
                                   f"{selected_extent.XMin} {selected_extent.YMin}",
                                   f"{selected_extent.XMin} {selected_extent.YMax}",
                                   cell_size, cell_size, 0, 0,
                                   f"{selected_extent.XMax} {selected_extent.YMax}",
                                   "NO_LABELS", "settlements_lyr", "POLYGON")

    # Perform a spatial join between the fishnet and buildings
    arcpy.SpatialJoin_analysis(output_fishnet, "building_lyr", output_spatial_join, "JOIN_ONE_TO_ONE", "KEEP_ALL",
                               match_option="INTERSECT", search_radius="", distance_field_name="")

    # Count the number of non-empty cells
    with arcpy.da.SearchCursor(output_spatial_join, "Join_Count") as cursor:
        non_empty_cells = sum(1 for row in cursor if row[0] > 0)

    return non_empty_cells


# Set arcpy environment
arcpy.env.overwriteOutput = True

timesteps = ["1899",
             "1918",
             "1933",
             "1959",
            "1970",
            "1978",
            "2020"]
ts = "1899"



for ts in timesteps:
    # Input data
    building_footprints = r"C:\Users\yraeth\Documents\EMPHASES\4_projects\211104\Building_Network\building_footrpints\bf_" + ts + "_bpl.shp"
    settlements = r"C:\Users\yraeth\Documents\EMPHASES\4_projects\211104\Building_Network\building_networks\chull_" + ts + "\chull_" + ts + ".shp"

    # Prepare layers for selection
    arcpy.MakeFeatureLayer_management(building_footprints, "building_lyr")
    arcpy.MakeFeatureLayer_management(settlements, "settlements_lyr")

    # Get a list of unique settlement IDs
    settlement_ids = [row[0] for row in arcpy.da.SearchCursor("settlements_lyr", "ID")]

    # Define box sizes for the box-counting method
    box_sizes = [16, 32, 64, 128, 256, 512, 1024]

    # Use in-memory workspace for temporary data
    file_gdb_path = "in_memory"

    # Create an empty DataFrame with the desired columns
    results_df = pd.DataFrame(columns=['ID', 'Fractal_Dimension'])

    # Loop through each settlement
    for settlement_id in settlement_ids:
        print(f"Processing settlement ID: {settlement_id}")

        # Select settlement by ID
        arcpy.SelectLayerByAttribute_management("settlements_lyr", "NEW_SELECTION", f"ID = '{settlement_id}'")

        # Get the extent of the selected settlement polygon
        selected_settlement = arcpy.FeatureSet("settlements_lyr")
        # Get the extent of the selected settlement polygon
        with arcpy.da.SearchCursor("settlements_lyr", ["SHAPE@"]) as cursor:
            for row in cursor:
                selected_extent = row[0].extent
                break
        arcpy.env.extent = selected_extent

        # Select buildings within the settlement
        arcpy.SelectLayerByLocation_management("building_lyr", "INTERSECT", "settlements_lyr")

        # Perform the box-counting method
        box_counts = []
        for box_size in box_sizes:
            output_fishnet = os.path.join(file_gdb_path, f"fishnet_{settlement_id}_{box_size}")
            output_spatial_join = os.path.join(file_gdb_path, f"spatial_join_{settlement_id}_{box_size}")
            non_empty_cells = create_fishnet_and_spatial_join(output_fishnet, output_spatial_join, box_size)
            box_counts.append(non_empty_cells)
            print(f"Box size {box_size}: {non_empty_cells} non-empty cells")

        # Calculate the box-counting fractal dimension
        log_box_sizes = [math.log(1 / box_size) for box_size in box_sizes]
        log_box_counts = [math.log(box_count) for box_count in box_counts]
        # Calculate the box-counting fractal dimension
        fractal_dimension, _ = np.polyfit(log_box_sizes, log_box_counts, 1)
        print(f"Fractal dimension for settlement ID {settlement_id}: {fractal_dimension}\n")

        # Append the results to the DataFrame
        results_df = results_df.append({'ID': settlement_id, 'Fractal_Dimension': fractal_dimension}, ignore_index=True)

    # Export the DataFrame to a CSV file
    results_df.to_csv(r"C:\Users\yraeth\Documents\EMPHASES\4_projects\211104\Building_Network\fractal" + ts + ".csv",
                      index=False)

    # Delete in-memory data after processing each timestep
    arcpy.Delete_management("in_memory")








#
#
#
# def create_fishnet_and_spatial_join(output_fishnet, output_spatial_join, cell_size):
#     # Create a fishnet based on the current settlement extent
#     arcpy.CreateFishnet_management(output_fishnet,
#                                    f"{selected_extent.XMin} {selected_extent.YMin}",
#                                    f"{selected_extent.XMin} {selected_extent.YMax}",
#                                    cell_size, cell_size, 0, 0,
#                                    f"{selected_extent.XMax} {selected_extent.YMax}",
#                                    "NO_LABELS", "settlements_lyr", "POLYGON")
#
#     # Perform a spatial join between the fishnet and buildings
#     arcpy.SpatialJoin_analysis(output_fishnet, "building_lyr", output_spatial_join, "JOIN_ONE_TO_ONE", "KEEP_ALL",
#                                match_option="INTERSECT", search_radius="", distance_field_name="")
#
#     # Count the number of non-empty cells
#     with arcpy.da.SearchCursor(output_spatial_join, "Join_Count") as cursor:
#         non_empty_cells = sum(1 for row in cursor if row[0] > 0)
#
#     return non_empty_cells
#
#
# # Set arcpy environment
# arcpy.env.overwriteOutput = True
#
# timesteps = ["1899",
#              "1918",
#              "1933",
#              "1959",
#             "1970",
#             "1978",
#             "2020"]
# ts = "1899"
# for ts in timesteps:
#     # Input data
#     building_footprints = r"C:\Users\yraeth\Documents\EMPHASES\4_projects\211104\Building_Network\building_footrpints\bf_"+ ts+ "_bpl.shp"
#     settlements = r"C:\Users\yraeth\Documents\EMPHASES\4_projects\211104\Building_Network\building_networks\chull_"+ts+"\chull_"+ts+".shp"
#
#
#     # Prepare layers for selection
#     arcpy.MakeFeatureLayer_management(building_footprints, "building_lyr")
#     arcpy.MakeFeatureLayer_management(settlements, "settlements_lyr")
#
#     # Get a list of unique settlement IDs
#     settlement_ids = [row[0] for row in arcpy.da.SearchCursor("settlements_lyr", "ID")]
#
#     # Define box sizes for the box-counting method
#     box_sizes = [16, 32, 64, 128, 256, 512, 1024]
#     # Prepare output directories
#     fishnets_dir = r"C:\Users\yraeth\Documents\EMPHASES\4_projects\211104\Building_Network\temp\fishnet"
#     spatial_joins_dir = r"C:\Users\yraeth\Documents\EMPHASES\4_projects\211104\Building_Network\spjoin"
#     os.makedirs(fishnets_dir, exist_ok=True)
#     os.makedirs(spatial_joins_dir, exist_ok=True)
#     # Create a new file geodatabase to store the fishnets and spatial joins
#     file_gdb_path = r"C:\Users\yraeth\Documents\EMPHASES\4_projects\211104\Building_Network\temp\geodatabase.gdb"
#     arcpy.CreateFileGDB_management(os.path.dirname(file_gdb_path), os.path.basename(file_gdb_path))
#
#     # Create an empty DataFrame with the desired columns
#     results_df = pd.DataFrame(columns=['ID', 'Fractal_Dimension'])
#
#     # Loop through each settlement
#     for settlement_id in settlement_ids:
#         print(f"Processing settlement ID: {settlement_id}")
#
#         # Select settlement by ID
#         arcpy.SelectLayerByAttribute_management("settlements_lyr", "NEW_SELECTION", f"ID = '{settlement_id}'")
#
#         # Get the extent of the selected settlement polygon
#         selected_settlement = arcpy.FeatureSet("settlements_lyr")
#         # Get the extent of the selected settlement polygon
#         with arcpy.da.SearchCursor("settlements_lyr", ["SHAPE@"]) as cursor:
#             for row in cursor:
#                 selected_extent = row[0].extent
#                 break
#         arcpy.env.extent = selected_extent
#
#         # Select buildings within the settlement
#         arcpy.SelectLayerByLocation_management("building_lyr", "INTERSECT", "settlements_lyr")
#
#         # Perform the box-counting method
#         box_counts = []
#         for box_size in box_sizes:
#             output_fishnet = os.path.join(file_gdb_path, f"fishnet_{settlement_id}_{box_size}")
#             output_spatial_join = os.path.join(file_gdb_path, f"spatial_join_{settlement_id}_{box_size}")
#             non_empty_cells = create_fishnet_and_spatial_join(output_fishnet, output_spatial_join, box_size)
#             box_counts.append(non_empty_cells)
#             print(f"Box size {box_size}: {non_empty_cells} non-empty cells")
#
#         # Calculate the box-counting fractal dimension
#         log_box_sizes = [math.log(1 / box_size) for box_size in box_sizes]
#         log_box_counts = [math.log(box_count) for box_count in box_counts]
#
#         # Calculate the box-counting fractal dimension
#         fractal_dimension, _ = np.polyfit(log_box_sizes, log_box_counts, 1)
#         print(f"Fractal dimension for settlement ID {settlement_id}: {fractal_dimension}\n")
#
#         # Append the results to the DataFrame
#         results_df = results_df.append({'ID': settlement_id, 'Fractal_Dimension': fractal_dimension}, ignore_index=True)
#
#     # Export the DataFrame to a CSV file
#     results_df.to_csv( r"C:\Users\yraeth\Documents\EMPHASES\4_projects\211104\Building_Network\fractal"+ ts+".csv", index=False)
#
