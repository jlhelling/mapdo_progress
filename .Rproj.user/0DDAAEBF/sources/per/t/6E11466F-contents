
#' Loading of network-metrics table
#'
#' @return table with all network segments and the corresponding metrics
#' 
load_network_metrics <- function(){
  
  require(RPostgreSQL)
  
  # establish connection with mapdoapp-database
  con <- RPostgreSQL::dbConnect(
    RPostgres::Postgres(), 
    dbname = Sys.getenv('DBMAPDO_NAME'), 
    host = Sys.getenv('DBMAPDO_HOST'), 
    port = Sys.getenv('DBMAPDO_PORT'), 
    user = Sys.getenv('DBMAPDO_USER'), 
    password = Sys.getenv("DBMAPDO_PASS"))
  
  # load metrics with sql-statement
  network_metrics <- RPostgreSQL::dbGetQuery(conn = con, statement = 'SELECT * FROM public.network_metrics') |> 
    na.omit()
  
  # disconnect from database
  dbDisconnect(con)
  
  return(network_metrics)
}


#' Create network-sf-file for geospatial visualization
#'
#' @param metrics_tbl table with all network segments and metrics
#'
#' @return sf-file of river network
#'
load_metrics_sf <- function(metrics_tbl){
  
  require(RPostgreSQL)
  require(sf)
  
  # establish connection with mapdoapp-database
  con <- RPostgreSQL::dbConnect(
    RPostgres::Postgres(), 
    dbname = 'mapdoapp', 
    host = 'lxc-pgdb-dev.evs.ens-lyon.fr', 
    port = '5435', 
    user = 'reader', 
    password = 'LaGeoCbi1!')
  
  # load hydrographic network
  hydro_swaths <- RPostgreSQL::dbReadTable(con, "hydro_swaths")
  
  # disconnect from database
  dbDisconnect(con)
  
  # create sf-table from hydrographic network
  hydro_swaths_sf <- 
    hydro_swaths |> 
    dplyr::mutate(geometry = structure(geom, class = "WKB") |> 
                    st_as_sfc(EWKB = TRUE)) |> 
    dplyr::select(!geom) |> st_as_sf()
  
  # join metrics to sf-file
  network_sf <- 
    hydro_swaths_sf |> 
    dplyr::left_join(metrics_tbl |> 
                       dplyr::select(!matches(c(names(hydro_swaths_sf), "geom"))), by = join_by(gid == fid)) |> 
    na.omit()
}