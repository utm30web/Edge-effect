# Point_to_edge_correction toolbox
This is a tool developed for the ArcGis Geographic Information System that allows solving the border effect that would appear when using data in the shape of points aggregated to artificially delimited spatial units of analysis. For its use, a dataset of thefts in the city of Barcelona in 2016 is made available. 

Tool installation:
1. Open ArcGIS software in administrator mode. 
2. Access ArcGIS toolbox and select the option "add toolbox".In the downloaded "Toolbox" folder, select "point to edge correction". This is added as "Custom_toolbox". 
3. Open "Custom_toolbox" and select "point to edge correction".
4. Open properties of "point to edge correction". Open the "source" tab and add the "TOOLBOX_ARCGIS_V1 (1)" file. 
5. Access the catalogue and select the folder in which all downloaded files have been saved. Select "Rintegration.pyt" and choose "Install_R_bindings" to install. 
6.Run the "Custom_toolbox" toolbox.

Load data:
1. As Input point layer load "robbery_2013"
2. Input polygon load "CENSAL_BAR"
3. Distance to axis (we have chosen 100 meters)
4. Choose linear function or normal function.
5. You will get as final result a layer in shape format. 

Important aspects to consider:
- Points and polygon layers have to be in the same projection.
- The coordinate system of points has to be in longitude and latitude. 
- The following packages need to be installed in R beforehand: "dplyr", "geosphere", "ggplot2",  "shapefiles", "maptools", "broom" .

You can find the full text of the tool at ---
