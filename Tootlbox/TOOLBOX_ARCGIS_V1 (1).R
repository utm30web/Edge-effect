tool_exec<- function(in_params, out_params){
  
	#####################################################################################################  
	### Check/Load Required Packages  
	#####################################################################################################   
	arc.progress_label("Loading packages...")
	arc.progress_pos(20)

	if(!requireNamespace("sp", quietly = TRUE))
		install.packages("sp", quiet = TRUE)
	if(!requireNamespace("rgdal", quietly = TRUE))
		install.packages("rgdal", quiet = TRUE)
	if(!requireNamespace("geosphere", quietly = TRUE))
		install.packages("geosphere", quiet = TRUE)
	if(!requireNamespace("dplyr", quietly = TRUE))
		install.packages("dplyr", quiet = TRUE)
	if(!requireNamespace("maptools", quietly = TRUE))
		install.packages("maptools", quiet = TRUE)
	if(!requireNamespace("ggplot2", quietly = TRUE))
		install.packages("ggplot2", quiet = TRUE)
	if(!requireNamespace("broom", quietly = TRUE))
		install.packages("broom", quiet = TRUE)
	if(!requireNamespace("shapefiles", quietly = TRUE))
		install.packages("shapefiles", quiet = TRUE)	

	require('sp')
	require('rgdal')
	require(geosphere)
	require(dplyr)
	require(maptools)
	require(ggplot2)
	require(broom)
	require(shapefiles)


	##################################################################################################### 
	### Define input/output parameters
	#####################################################################################################
	pts_param <- in_params[[1]]  
	pol_param <- in_params[[2]]  
	Par <-  in_params[[3]] #Par  = as.numeric(readline(prompt="Input distance: "))
	#Par <-as.numeric(Par)
	Par2 <- in_params[[4]] #Par2 = readline(prompt="Input method (linear/normal): ") 

	output_csv <- out_params[[1]]
	output_shapefile <- out_params[[2]]

	### read data
	arc.progress_label("Loading Dataset")
	arc.progress_pos(40)

	pts_DS <- arc.open(pts_param)			
	pts <- arc.select(pts_DS)	
	pts <- arc.data2sp(pts)
		
	#pts = readOGR(pts_param)
	pol_DS <- arc.open(pol_param)
	pol <- arc.select(pol_DS)
	pol <- arc.data2sp(pol)
	#pol1 = readOGR(pol_param)		
		
	pol <- spTransform(pol, CRS( arc.fromWktToP4(arc.shapeinfo(pts_DS)$WKT)) )
    #pol <- spTransform(pol, CRS(proj4string(pts)))
	pol0 = pol
    long_and_lat <- ggplot2::fortify(pol)
	
	### processing
	
	UU = CHECK(pts, pol, Par)
	UU_FULL = CHECK(pts, pol, 100000)

	map <- ggplot() + 
	  geom_polygon(data = pol, aes(x = long, y = lat, group = group), col = "blue", fill = NA)+
	  geom_point(data = data.frame(pts@coords) , aes(x = coords.x1, y = coords.x2), col = 'red')
	map

	map1 <- ggplot() + 
	  geom_polygon(data = pol, aes(x = long, y = lat, group = group), col = "blue", fill = NA, cex = 1.3)+
	  geom_point(data = data.frame(pts@coords[UU$binar, ]) , aes(x = coords.x1, y = coords.x2), col = 'red') # UU$colo
	map1

	map2 <- ggplot() + 
	  geom_polygon(data = pol, aes(x = long, y = lat, group = group), col = "blue", fill = NA, cex = 1.3)+
	  geom_point(data = data.frame(pts@coords[UU$binar, ]) , aes(x = coords.x1, y = coords.x2), col = UU$colo)  
	map2

	n = length(pol@polygons)
	centers = data.frame(matrix(NA, nco = 2, nrow = n), 
						 UU$numb_p, round(UU$numb_p/sum(UU$numb_p)*100, 1))
	names(centers) = c('x', 'y', 'number', 'percent')


	for (i in 1:n){
	  PL = pol@polygons[[i]]@Polygons[[1]]@coords 
	  centers[i,1:2 ] =  c(mean(summary(PL[,1])[c(1,6)]), mean(summary(PL[,2])[c(1,6)]))
	}

	map3 = map1  +   geom_text(data = centers,  aes(x = x, y = y, label = number),size = 4.5)+
	  ggtitle("Number of points in each polygon")
	map3

	map4 = map1  +   geom_text(data = centers,  aes(x = x, y = y, label = percent),size = 4.5)+
	  ggtitle("Percent of points in each polygon")
	map4


	# Change figure by percents
	long_and_lat <- ggplot2::fortify(pol)
	long_and_lat$group = as.numeric(long_and_lat$group)
	i = 1
	long_and_lat$group[1] = centers$percent[1]
	for (k in 2:dim(long_and_lat)[1]){
	  if (long_and_lat$order[k] == 1){
		i = i+1
	  }
	  long_and_lat$group[k] = centers$percent[i]
	}

	long_and_lat = long_and_lat[order(long_and_lat$group),]
	long_and_lat$group = as.factor(long_and_lat$group)

	centers[,5] = centers[,4]
	names(centers)[5] = 'total'

	for (i in 1:dim(centers)[1]){
	  centers[i,5] = paste(as.character(centers$number[i]),'/',as.character(UU_FULL$numb_p[i]), sep = "")
	}

	map6 = map1  +   geom_text(data = centers,  aes(x = x, y = y, label = total),size = 4.5)+
	  ggtitle("Number of points in each polygon / all points in polygon")
	map6

	## Write distance 
	DDD = data.frame(pts@coords)
	DISTANCE = data.frame(DDD$coords.x1, DDD$coords.x2, UU$dist)
	names(DISTANCE) = c('x_coor', 'y_coor', 'distance')
      
	out_path<-dirname(output_csv) 
	
	# Set the working directory to the program location
	setwd(out_path)

	write.csv(DISTANCE, 'distance.csv'	)

	X = UU$dist

	LIN = DECAY(X, METH = 'linear', DIST = DISTANCE$distance[UU$binar])
	NORM = DECAY(X, METH = 'normal', DIST = DISTANCE$distance[UU$binar])

	df = data.frame(X,LIN, NORM)
	names(df) = c('dist', 'linear', 'normal')

	map7 = ggplot()+
	  geom_line(data = df, aes(x = dist, y = linear), col = "blue")+
	  geom_line(data = df, aes(x = dist, y = normal), col = "red")+
	  ggtitle("Linear (blue) and normal (red) distance decay functions")+
	  ylab('Distance decay function')
	map7

	write.csv(df, 'decay.csv')
		
	N = length(UU$binar)
	OPPOSIT_P = matrix(NA, nrow = N, ncol = 2)
	REG = c()
	D_oppose = c()   
	print("-------calculating OPPOSIT111111----------")
	for (i in 1:N){
	  OPPOSIT_P[i,] = OPPOSITE(points = c(DDD$coords.x1[UU$binar[i]], DDD$coords.x2[UU$binar[i]]), 
							   seg_points = UU$pos[UU$binar[i],], DIST = UU$dist_map[UU$binar], METH = Par2)$point
	 	  
	  REG[i] = POLYG(OPPOSIT_P[i,],pol)
	 	  
	  D_oppose[i] = OPPOSITE(points = c(DDD$coords.x1[UU$binar[i]], DDD$coords.x2[UU$binar[i]]), 
						  seg_points = UU$pos[UU$binar[i],], DIST = UU$dist_map[UU$binar], METH = Par2)$ist_oppose
	}
   
	df1 = data.frame(OPPOSIT_P)
	names(df1)  = c('x', 'y')
		
	map7 <- ggplot() + 
	  geom_polygon(data = pol, aes(x = long, y = lat, group = group), col = "blue", fill = NA, cex = 1.3)+
	  geom_point(data = data.frame(pts@coords[UU$binar, ]) , aes(x = coords.x1, y = coords.x2), col = 'red') +
	  geom_point(data = df1 , aes(x = x, y = y), col = 'green') +
	  ggtitle("Original points (red) with opposite points (green)")
	map7

    
	df2 = data.frame(data.frame(pts@coords[UU$binar, ])$coords.x1, 
					 data.frame(pts@coords[UU$binar, ])$coords.x2, df1)
	names(df2) = c('x_coord', 'y_coord', 'x_opposite', 'y_opposite')
	head(df2,20)

       
	# Define normalization of the coordinates
	if(Par2 == 'linear'){
	  COOR = LIN[UU$binar]
	}else{
	  COOR = NORM[UU$binar]
	}
	COOR[REG == 0] = 1
	COOR[COOR<0.49] = 0.5

	
	df_final = data.frame(UU$colo, df2, UU$dist[UU$binar], NORM[UU$binar], LIN[UU$binar], COOR, 1-COOR)
	names(df_final) = c('Reg', 'x', 'y', 'x_oppos', 'y_oppose', 'dist', 'Norm', 'Lin', 'Normaliz.', 
						'Normaliz_oppse')
	head(df_final, 20)    
	
	write.csv(df2, 'opposite_points.csv' )

	## NEW PART of THE CODE
	OPPOS_REG = c()
	for (i in 1:dim(df_final)[1]){
	  OPPOS_REG[i] = POLYG(c(df_final$x_oppos[i], df_final$y_oppos[i]),pol)
	}
	
	
	
	## Only points with distance < 100 meters and oposite points
	df_map = data.frame(x = c(df_final$x, df_final$x_oppos), 
						y = c(df_final$y, df_final$y_oppos), 
						region = c(df_final$Reg, OPPOS_REG), 
						normalization = c(df_final$Normaliz., df_final$Normaliz_oppse), 
						distace = c(df_final$dist, df_final$dist*D_oppose/UU$dist_map[UU$binar]))

	## All points + opposite points
	K = rep(1, length(pts@data$Coord_X))
	K[UU$binar] = df_final$Normaliz.

	data.frame(pts@coords[UU$binar, ])$coords.x1

	df_map_1 = data.frame(x = c(data.frame(pts@coords)$coords.x1, df_final$x_oppos), 
						y = c(data.frame(pts@coords)$coords.x2, df_final$y_oppos), 
						region = c(UU_FULL$colo, OPPOS_REG), 
						normalization = c(K, df_final$Normaliz_oppse), 
						distace = c(UU_FULL$dist, df_final$dist*D_oppose/UU$dist_map[UU$binar]))

	head(df_map)
	head(df_map_1)

	long_and_lat$group = as.numeric(long_and_lat$group)
	long_and_lat = data.frame(long_and_lat, group_all = NA)

	SUM = c()
	for(i in 1:max(df_final$Reg)){
	  SUM[i] = round(sum(df_map$normalization[df_map$region == i]), 2)
	  long_and_lat$group[long_and_lat$id == i-1] = SUM[i]
	}

	SUM_1 = c()
	for(i in 1:max(df_final$Reg)){
	  SUM_1[i] = round(sum(df_map_1$normalization[df_map_1$region == i]), 2)
	  long_and_lat$group_all[long_and_lat$id == i-1] = SUM_1[i]
	}


	centers$number = SUM
	long_and_lat$group = as.factor(long_and_lat$group)
	head(long_and_lat)


	centers = data.frame(centers, number_all = NA)
	centers$number_all = SUM_1
	head(centers, 20)

	head(long_and_lat)

	#map8 <- ggplot() + 
	#  geom_polygon(data = long_and_lat, 
	#               aes(x = long, y = lat), col = "blue", alpha = 0.4)+
	#  geom_text(data = centers,  aes(x = x, y = y, label = number),
	#            size = 4.5)+
	#  ggtitle("Sum of points in each regions (using normalization)")
	#map8

	plot(pol, main = "Sum of points in each regions (using normalization)",border="blue")
	text(centers$x, centers$y, centers$number, cex = 0.5, col = 'red')

	#map8_1 <- ggplot() + 
	#  geom_polygon(data = long_and_lat, 
	#               aes(x = long, y = lat), col = "blue", alpha = 0.4)+
	#  geom_text(data = centers,  aes(x = x, y = y, label = number_all),
	#            size = 4.5)+
	#  ggtitle("Sum of points in each regions (using normalization)")
	#map8_1
	
	plot(pol, main = "Sum of points in each regions (using normalization)", border="blue")
	text(centers$x, centers$y, centers$number_all, cex = 0.6, col = 'red')

	## Plot histogrmas for all distances in all polygons
	N = length(pol@polygons)

	## 
	plot(pol, main = "Position: blue - opposite points + points with distance < PARAMETER", border="blue")
	lines(df_map[df_map$region>0,]$x, df_map[df_map$region>0,]$y, col = 'blue', type = 'p', cex = 0.5)
	text(centers$x, centers$y, centers$number, cex = 0.6, col = 'red')

	## PLOT 
	df4 =  data.frame( UU$dist[UU$binar], NORM[UU$binar], LIN[UU$binar])
	names(df4) = c( 'dist', 'Norm', 'Lin')
	head(df4, 20)
    
	## Save all data as CSV file 
	write.csv(df_map_1, output_csv)
	X = UU$df4
	names(df4) = c('dist', 'normal', 'linear')

	map9 = ggplot()+
	  geom_line(data = df4, aes(x = dist, y = linear), col = "blue")+
	  geom_line(data = df4, aes(x = dist, y = normal), col = "red")+
	  ggtitle("Linear (blue) and normal (red) distance decay functions")+
	  ylab('Distance decay function')
	map9

	## Save all data as SHP file 
	names(df_map_1)[1:2] = c('coords.x1', 'coords.x2')
	
	if (!is.null(output_shapefile))
	{		
		SSS =  pol
		SSS@data = data.frame(centers, pol@data)
		#print(SSS@data)	
		shape_info <- list(type="Point", WKT=arc.shapeinfo(pts_DS)$WKT)
		shape_info <- arc.shapeinfo(pts_DS)
		#shape_info$WKT <- arc.shapeinfo(data_shp)$WKT
		arc.write(output_shapefile, SSS, shape_info=shape_info)
	
		#writeOGR(SSS, ".", "Result", driver = "ESRI Shapefile", overwrite_layer = TRUE)		
		#pts1 <- readOGR(output_shapefile)
		pts1 <- arc.open(pts_param)			
		pts1 <- arc.select(pts1)	
		pts1 <- arc.data2sp(pts1)
	
		plot(pol, col = 'blue')
		lines(pts1@coords, col = 'red', type = 'p', cex = 0.5)	
		
	}
	
}


DIST = function(p, POL){
  n = length(POL@polygons)
  
  IND = 1
  PL = POL@polygons[[1]]@Polygons[[1]]@coords
  D = dim(PL)
  segment = PL[1:2, 1:2]
  MIN = nearestPointOnSegment(segment, p)
  SEG = segment
  
  for (i in 1:n){
    PL = POL@polygons[[i]]@Polygons[[1]]@coords
    D = dim(PL)
    for (j  in 1:(D[1]-1)){
      segment = PL[j:(j+1), 1:2]
      DIST1 = nearestPointOnSegment(segment, p)
      if(is.na(DIST1[3])){
        DIST1[3] = MIN[3]+1
      }
      if (MIN[3] > DIST1[3]){
        MIN = DIST1
        IND = i
        SEG = segment
      }
    }
  }
  
  K = 0
  for (i in 1:n){
    PL = POL@polygons[[i]]@Polygons[[1]]@coords
    K = point.in.polygon(p[1], p[2], PL[,1], PL[,2], mode.checked=FALSE)
    if (K>0){
      IND = i
    }
  }
  
  
  DIST__m = distm (c(p[1], p[2]), c(MIN[1], MIN[2]), fun = distHaversine)
  DIST_map = sqrt((p[1]-MIN[1])^2+(p[2]-MIN[2])^2)
  return(list(Dist = DIST__m, Polygon = IND, 
              Dist_map = DIST_map, 
              point = c(MIN[1], MIN[2]), 
              seg = SEG))
}


CHECK = function(PTS, POL, DISTANCE = 100){
  # points = cbind(PTS@data$Coord_X, PTS@data$Coord_Y)
  points = PTS@coords
  D = dim(points)[1]
  res = c()
  res_dis = c()
  numb_points = rep(0,length(POL@polygons))
  colo = c()
  POS = matrix(NA, nrow = dim(points)[1], ncol = 2)
  DIS_map = c()
  SEG = list()
    
  for (i in 1:D){
    DD = DIST(points[i,], POL)
    #print(c(i,D, DD))
    res_dis[i] = DD$Dist 
    POS[i, ] =  DD$point
    DIS_map[i] = DD$Dist_map
    SEG[[i]] = DD$seg
    if (DD$Dist < DISTANCE){
      res = c(res,i)
      numb_points[DD$Polygon] = numb_points[DD$Polygon] + 1
      colo = c(colo, DD$Polygon)
    }
    print(c(i,D))
  }
  return(list(dist = res_dis, binar = res, 
              numb_p = numb_points, colo = colo,
              pos = POS, dist_map = DIS_map, 
              seg = SEG))
}


## Define distance decay function

DECAY = function(x, DIST = DISTANCE$distance, METH = 'linear'){
  if (METH == 'linear'){
    m = min(DIST)
    M = max(DIST)
    res = punif(x,  min = m, max = M)#1 - punif(x,  min = m, max = M)
  }
  if (METH == 'normal'){
    m = mean(DIST)
    M = sd(DIST)
    res = pnorm(x,  mean = m, sd = M) #1 - pnorm(x,  mean = m, sd = M)
  } 
  return(res)
} 



## Define oposite points

POLYG = function(p, POL = pol){
  res = 0
  n = length(POL@polygons)
  for (i in 1:n){
    PL = POL@polygons[[i]]@Polygons[[1]]@coords
    K = point.in.polygon(p[1], p[2], PL[,1], PL[,2], mode.checked=FALSE)
    if (K>0){
      res = i
    }
  }
  return(res)
}


OPPOSITE = function(points, seg_points, DIST, METH = 'linear'){
  DIF = sqrt(sum((points-seg_points)^2))
  if (METH == 'linear'){
    m = min(DIST)
    M = max(DIST)
    res = 1 - punif(DIF,  min = m, max = M)
    dis1 = qunif(res,  min = m, max = M)
    
  }
  if (METH == 'normal'){
    m = mean(DIST)
    M = sd(DIST)
    res = 1 - pnorm(DIF,  mean = m, sd = M)
    dis1 = qnorm(res, mean = m, sd = M)
  }
  
  #print(c(res, dis1, DIF))
  x0 = seg_points[1]
  y0 = seg_points[2]
  
  x1 = points[1]
  y1 = points[2]
  
  k = (y1-y0)/(x1-x0) 
  b = -x0*k +y0
  
  co = c(1+k^2, 2*(-x0+k*(b-y0)), x0^2 +(b-y0)^2 - dis1^2)
  
  D = co[2]^2 - 4*co[1]*co[3]
  pp_x = -(co[2] + c(-1,1)*sqrt(D))/(2*co[1])
  pp_y = k*pp_x +b
  CHH = c(pp_x[1], pp_y[1], pp_x[2], pp_y[2])
  #print(CHH)
  #print(sum(is.na(CHH)))
  
  if(sum(is.na(CHH)) == 0){
    if (sum((c(pp_x[1], pp_y[1]) - points)^2)> sum((c(pp_x[2], pp_y[2]) - points)^2)){
      res_D =  c(pp_x[1], pp_y[1])
    }else{
      res_D =  c(pp_x[2], pp_y[2])
    }
  }else{
    res_D = points
  }
  return(list(quant = res, ist_oppose = dis1, point = res_D))
}


## standalone R testing
foo <- function()
{
  library(arcgisbinding)
  arc.check_product()
  tool_exec(
      list("E:\\work\\freelancer\\ArcGIS\\work\\data\\robbery_2013.shp","E:\\work\\freelancer\\ArcGIS\\work\\data\\CENSAL_BAR.shp","100","normal"),
      list("E:\\work\\freelancer\\ArcGIS\\work\\result\\result.csv","E:\\work\\freelancer\\ArcGIS\\work\\result\\result.shp")
   )
}