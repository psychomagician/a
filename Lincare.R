


Get_Gas_Stations <- function(Lon, Lat, Radius){
  Package.Check <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)){ 
      install.packages(new.pkg
                       , dependencies = TRUE)
      sapply(pkg, require
             , character.only = TRUE)
    }else{
      sapply(pkg
             , require
             , character.only = TRUE)}
  }
  
  Package.Check('httr'); Package.Check("jsonlite"); Package.Check('stringi'); Package.Check('stringr')
  # coords <- Get_Coordinates_DSK('1311 fuchsia dr holiday fl')
  # Lat <- coords[,1]
  # Lon <- coords[,2]
  # Radius <- 1.5
  
  url <- paste0("http://api.mygasfeed.com/stations/radius/"
                ,Lat
                ,"/"
                ,Lon
                ,"/"
                ,Radius
                ,"/reg/distance/s2rj8il72c.json")
  
  aa <- GET(url)
  BBB <- content(aa)  
  
  a <- BBB$stations
  rows <- length(a)
  
  if (rows == 0) {
    return(cbind(G_Name = NA, G_Address = NA, G_Distance = NA, G_Lon = NA, G_Lat = NA))
  }else{
    
    Distance <- BBB$stations[[1]]$distance
    Distance <- as.numeric(substr(Distance, 1, str_locate(Distance, " ")[[1]] - 1))
    Address <- BBB$stations[[1]]$address
    City <- BBB$stations[[1]]$city
    State <- BBB$stations[[1]]$region
    Zip <- BBB$stations[[1]]$zip
    Full_Address <- paste0(Address
                           ," "
                           ,City
                           ," "
                           ,State
                           ," "
                           ,Zip)
    Lon <- BBB$stations[[1]]$lng
    Lat <- BBB$stations[[1]]$lat
    
    Name <- BBB$stations[[1]]$station
    
    return(cbind(G_Name = Name, G_Address = Full_Address, G_Distance = Distance, G_Lon = Lon, G_Lat = Lat))
    
  }
  
}


