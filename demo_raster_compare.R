
  a = raster("C:/Users/Lu000012/Documents/files/GLOBAL_LUR/air pollution_Larken_and_utrecht/LUR_Utrecht/NO2_Input/re_road_length_5000.map")
  b = raster("C:/Users/Lu000012/Documents/files/GLOBAL_LUR/predictor variables/pop_5000_wgs84.map")
  proj4string(a) =   CRS("+init=epsg:28992")
  proj4string(b) =   CRS("+init=epsg:4326")

  c = projectRaster(b, to = a)
  d = crop(c, a )
  levelplot(d)

  s =stack(a,d)
  #levelplot(s)


  nl <- nlayers(s)
  m <- matrix(1:nl, nrow=2)
  themes <- list(RdBuTheme(), BTCTheme(), GrTheme(), PuOrTheme())

  png("predictors.png")
  for (i in 1:nl){
    p <- levelplot(s, layers=i,
                   par.settings=themes[[i]],
                   margin=FALSE)
    print(p, split=c(col(m)[i], row(m)[i], ncol(m), nrow(m)), more=(i<nl))
  }
  dev.off()

    b = raster("C:/Users/Lu000012/Documents/files/GLOBAL_LUR/predictor variables/climate/wind_speed_10m_7.map
               ")

