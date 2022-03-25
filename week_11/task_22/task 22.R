library(tidyverse)
library(rio)
library(sf)

# Geospatial Data Abstraction Library(GDAL)
# Package sf reads and writes using GDAL by the functions st_read, st_write

# st_read()
fname <- system.file("shape/nc.shp", package = "sf")
fname
nc <- st_read(fname)

# Typical users will use a file name with parh for fname
# or first set R's working directory with setwd() and use file name without path

st_read("PG:dbname = postgis")

# st_layers: lists the driver and layers in a datasource
# GDAL allows for more than one geometry column for a feature layer; these are reported by st_layers.
st_layers("PG:dbname = postgis")

# st_read(): layer contains only geometries but no attributes. geometry column only
st_read("PG:dbname=postgis", "sids")

st_read(fname, stringsAsFactors = FALSE)
# =
options(stringsAsFactors = FALSE)
st_read(fname)

# st_write()
st_write(nc, "nc1.shp")

st_write(nc, dsn = "nc1.shp", layer = "nc.shp", driver = "ESRi Shapefile")

# Dataset and layer reading or creation options
st_write(st_as_sf(meuse), "PG:dbname=postgis", "meuse", 
         layer_options = "OVERWRITE=true")

# For st_read(), there is only options; for st_write, one needs to distinguish between dataset_options and layer_options, the first related to opening a dataset, the second to creating layers in the dataset.

# Reading and writing directly to and from spatial databases.

library(RPostgreSQL)
conn = dbConnect(PostgreSQL(), dbname = "postgis")
st_write(conn, meuse, drop = TRUE)
dbDisconnect(conn)

# Conversion to other formats: WKT, WKB, sp

st_point(c(0,1))
st_linestring(matrix(0:9, ncol = 2, byrow = TRUE))

x <- st_linestring(matrix(0:9, ncol = 2, byrow = TRUE))
str <- st_as_text(x) # create these well-known text (WKT) strings explicitly using st_as_text
x
st_as_sfc(str) # convert back from WKT by using st_as_sfc

x <-  st_linestring(matrix(0:9,ncol=2,byrow=TRUE))
(x <- st_as_binary(x)) # st_as_binary is of class WKB

class(x)
rawToHex(x) # hexadecimal character vector

# Converting back to sf uses st_as_sfc
x <- st_as_binary(st_sfc(st_point(0:1), st_point(5:6)))
st_as_sfc(x)

# Conversion to and from sp
# Spatial objects as maintained by package sp can be converted into simple feature objects or geometries by st_as_sf and st_as_sfc, respectively:
methods(st_as_sf)
methods(st_as_sfc)

library(sp)
data(meuse)
glimpse(meuse)
coordinates(meuse) <-  ~x+y
m.sf <- st_as_sf(meuse)
opar <- par(mar = rep(0,4))
plot(m.sf)

# Conversion of simple feature objects of class sf or sfc into corresponding Spatial* objects is done using the as method, coercing to Spatial
x <- st_sfc(st_point(c(5, 5)), st_point(c(6, 9)), crs = 4326)
as(x, "Spatial")

# Task




