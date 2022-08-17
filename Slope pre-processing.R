#This script is the code for pre-processing slope data, extracted from Doug's code.

# Create DEM, slope, slope limited and aspect from the raw input DEM:
DEM_raw <- rast(file.path(Paths$Input, Inputs$Raw_Input[Inputs$Input_Name == "DEM"]))

DEM <- DEM_raw %>% crop(ext(project(Template, crs(DEM_raw)))+0.25) %>% project(EPSG_Study) %>% resample(Template, method = "bilinear")
Slope <- DEM_raw %>% crop(ext(project(Template, crs(DEM_raw)))+0.25) %>% project(EPSG_Study) %>% terrain(v = "slope", unit = "degrees", neighbors = 8)  %>% resample(Template, method = "bilinear")
Slope_Limited <- app(Slope, function(x){ifelse(x>20.0,20.0,x)})
Aspect <-  DEM_raw %>% crop(ext(project(Template, crs(DEM_raw)))+0.25) %>% project(EPSG_Study) %>% terrain(v = "aspect", unit = "degrees", neighbors = 8)  %>% resample(Template, method = "bilinear")


#Apparent Slope Function (Duff and Penman 2021)
Apparent.Slope <- function(Slope, Aspect, FireDirection){
  Slope*cos(Mod((Aspect - FireDirection)%% 360)*pi/180)
}

#ROS Slope adjustment (See Sullivan 2014)
ROS.Slope.Adjust <- function(ROS, Apparent_Slope) {
  #ROS*exp(0.069*Apparent_Slope)
  ROS*2^(Apparent_Slope/10.0)
}