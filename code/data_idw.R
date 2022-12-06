# Emily Markowitz - NOAA Federal, 8 min
# oh, curious - in your code where do you make your rasterbricks of temperature rasters?
#   I was thinking I should do something similar for the spp idw rasters
# 
# Sean Rohan - NOAA Federal, 6 min
# https://github.com/sean-rohan-NOAA/coldpool/blob/64bf488fb0f90467b5b12c4f736bf7e679a453a6/R/utils.R#L73
# That's the code to make the stack from individual .grd files
# 
# Emily Markowitz - NOAA Federal, 5 min
# beaut
# 
# Sean Rohan - NOAA Federal, 5 min
# But there's a trick you have to use if you're going to save the raster brick in an R data file
# 
# Emily Markowitz - NOAA Federal, 5 min
# that's great!
#   ooo
# 
# Sean Rohan - NOAA Federal, 5 min
# Which you can find here: https://github.com/sean-rohan-NOAA/coldpool/blob/64bf488fb0f90467b5b12c4f736bf7e679a453a6/0_update_cold_pool_index.Rmd#L203
# 
# Emily Markowitz - NOAA Federal, 4 min
# woah
# 
# Sean Rohan - NOAA Federal, 4 min
# If you don't multiply by 1, it remains linked to the local source file
# 
# Emily Markowitz - NOAA Federal, 4 min
# thanks for pointing that out, I've never seen that before
# 
# Sean Rohan - NOAA Federal, 4 min
# So when you save it it will look for the .grd in whatever your local filepath is
# So not transferrable
# Or I guess maybe I went with GeoTiffs?
#   
#   Emily Markowitz - NOAA Federal, 3 min
# ah gotcha totally
# makse sense
# 
# Sean Rohan - NOAA Federal, 2 min
# https://github.com/sean-rohan-NOAA/coldpool/blob/64bf488fb0f90467b5b12c4f736bf7e679a453a6/R/interpolate_variable.R#L200
# 
