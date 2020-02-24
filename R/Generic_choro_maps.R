#' NST Choropleth map
#'
#' \code{NST_Choro} Use this function to map your data to a CDC-508 compliant choropleth map
#' @param Data Data.frame containing your data
#'
#' This is the main mapping function, input data and indentify which variable the
#' data should be split.
#'
#' @import tidyverse

#Some test data
#' @export

state_test_data <- data.frame(state = c(sample(c(state.abb,"DC","PR"),400,replace=T),rep("MD",50),rep("TX",50)),
                              type = c(sample(c("FIRE", "WATER","EARTH","AIR"),480,replace = T),rep("Test", 20)))



#Custom legend call out boxes==================================================
#' @export
NST_Choro <- function(data, group_column, state_column) {

#Mapping=======================================================================
#Palette Colors
my.cols <- RColorBrewer::brewer.pal(7, "Blues")
my.cols[1] <- "#efefef"

#download shapefile to working directory and unzip
destfile= "./cb_2014_us_state_5m.shp"
if(!file.exists(destfile)){
  res <- tryCatch(download.file("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_state_5m.zip",
                                destfile = "state.zip",
                                method="auto"),
                  error=function(e) 1)
  unzip("state.zip")
}

#Load shapefile
us <- suppressWarnings(rgdal::readOGR(dsn = paste0(getwd(),
                                                   "/cb_2014_us_state_5m.shp"),
                                      layer = "cb_2014_us_state_5m"))

#Transform projection
us_aea <- sp::spTransform(us, sp::CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

#Move non-contiguous states/territories
alaska <- us_aea[us_aea$STATEFP=="02",]
alaska <- maptools::elide(alaska, rotate=-50)
alaska <- maptools::elide(alaska, scale=max(apply(sp::bbox(alaska), 1, diff)) / 2.3)
alaska <- maptools::elide(alaska, shift=c(-2100000, -2500000))
sp::proj4string(alaska) <-  sp::proj4string(us_aea)

hawaii <- us_aea[us_aea$STATEFP=="15",]
hawaii <- maptools::elide(hawaii, rotate=-35)
hawaii <- maptools::elide(hawaii, shift=c(5400000, -1400000))
sp::proj4string(hawaii) = sp::proj4string(us_aea)

PR <- us_aea[us_aea$STATEFP=="72",]
PR <- maptools::elide(PR, rotate=15)
PR <- maptools::elide(PR, shift=c(-1200000, 0))
sp::proj4string(PR) = sp::proj4string(us_aea)

#Append the new state/territory coordinates
us_aea <- us_aea[!us_aea$STATEFP %in% c("02", "15", "72"),]
us_aea <- rbind(us_aea, alaska, hawaii, PR)

#Use ms_simplify() if there are a lot of maps:

us_aea <- rmapshaper::ms_simplify(us_aea)

#Transform shapefile data into data.frame
us50 <- broom::tidy(us_aea, region="STUSPS")
us50 <- us50 %>% filter(!id %in% c("AS","MP","GU","VI"))

#Generate centroids for every state
centroids <- rgeos::gCentroid(us_aea, byid=TRUE) %>%
  as.data.frame() %>%
  mutate(state=us_aea@data$STUSPS) %>%
  filter(!state %in% c("AS","MP","GU","VI"))

#Generate frequency table
group <- enquo(group_column)
state <- enquo(state_column)

state_maps <- data %>% group_by(!!group) %>% count(!!state)

#List of types for apply function
list_of_types <- state_maps$type %>% unique() %>% as.vector.factor()

#Mapping Function
choro_mapping <- function(i){

  #Loop through type list and generate frequency table
  state_count <- state_maps %>%
    filter(type == list_of_types[i]) %>%
    rename(id = state)

  #Format breaks for mapping
  state_count$n2 <- cut(state_count$n,
                        breaks = c(-0.5, 0.5, 3.5, 6.5, 15.5, 25.5, 55.5, Inf),
                        labels = c('0',
                                   paste0('1', "\U2012", '3'),
                                   paste0('4', "\U2012", '6'),
                                   paste0('7', "\U2012", '15'),
                                   paste0('16', "\U2012", '25'),
                                   paste0('26', "\U2012", '55'),
                                   '>55')
  )

  #Join data to centroids data.frame
  centroids <- left_join(centroids, state_count, by= c("state" = "id"))

  centroids <- centroids %>% mutate(n = ifelse(is.na(n), 0, n),
                                    n2 = ifelse(is.na(n2),"0", as.character(n2)))

  #Filter Western states' centroids
  centroids_west <- centroids %>%
    filter(!state %in% c("AK", "HI", "PR", "MA", "RI", "CT", "NJ", "DE", "MD", "DC"))

  #Moving centroids (number labels) for readability
  centroids_west[which(centroids_west$state == "FL"),"x"] <- 1800000
  centroids_west[which(centroids_west$state == "LA"),"x"] <- 720000
  centroids_west[which(centroids_west$state == "MI"),"x"] <- 1200000
  centroids_west[which(centroids_west$state == "MI"),"y"] <- 24000
  centroids_west[which(centroids_west$state == "VT"),"y"] <- 320000
  centroids_west[which(centroids_west$state == "NH"),"y"] <- 210000

  centroids_west <- centroids_west %>% filter(n != 0)

  #Setting X/Y values for mapping East Coast callout boxes
  #Filtering East Coast states
  centroids_east <- centroids[centroids$state %in%
                                c("MA", "RI", "CT", "NJ", "DE", "MD", "DC"),]

  centroids_east <- droplevels(centroids_east)

  #Setting the x values for callout boxes, points, and line segments
  centroids_east$x3 <- 2730000
  centroids_east$x2 <- 2470000
  centroids_east$x1 <- 2450000

  #Setting y values for callout boxes based on state
  advalues <- data.frame(state= c("MA", "RI", "CT", "NJ", "DE", "MD", "DC"), y1= c((-1:5)*(-135000)))

  #Join new x,y values to East Coast centroid data
  centroids_east <- left_join(centroids_east, advalues)

  #Centroids South
  #Filter Western states' centroids
  centroids_south <- centroids %>%
    filter(state %in% c("AK", "HI", "PR"))

  #Moving centroids (number labels) for readability
  centroids_south[which(centroids_south$state == "AK"),"x"] <- -1000000
  centroids_south[which(centroids_south$state == "AK"),"x1"] <- -1000000 - 250000
  centroids_south[which(centroids_south$state == "AK"),"y"] <- -2450000
  centroids_south[which(centroids_south$state == "HI"),"x"] <- -280000
  centroids_south[which(centroids_south$state == "HI"),"x1"] <- -280000 - 250000
  centroids_south[which(centroids_south$state == "HI"),"y"] <- -2450000
  centroids_south[which(centroids_south$state == "PR"),"x"] <- 2500000
  centroids_south[which(centroids_south$state == "PR"),"x1"] <- 2500000 - 250000
  centroids_south[which(centroids_south$state == "PR"),"y"] <- -2450000

  #Join data counts with US shapefile data.frame

  state_count1 <- anti_join(centroids,state_count,by=c("state"="id")) %>%
    select(state,n,n2) %>%
    mutate(id=state,
           n=0,
           n2="0") %>%
    select(id,n,n2) %>%
    bind_rows(state_count)

  map50 <-  inner_join(as.data.frame(us50), state_count1)
  map50$n2 <- factor(map50$n2,levels = levels(state_count$n2))

  #Plotting
  ggplot(data=map50) +
    #Plot US map
    geom_map(map=map50,  aes(x=long, y=lat, map_id=id, group=group, fill=n2),
             color="dark grey", size=0.13) +
    #Plot Western label data
    geom_text(data=centroids_west,
              aes(x,
                  y,
                  label=n,
                  color = n > ifelse(centroids_west %>% count(n) %>% nrow() > 3, 6,1000)),
              size=5,
              fontface = 'bold',
              show.legend=FALSE) +
    #Set dark background to have white text
    scale_color_manual(guide = FALSE, values = c("black", "white")) +
    #Clean up lat/long axis labels and label title
    labs(x="",
         y="") +
    #Plot callout boxes with points, lines, boxes, the compliant text
    geom_point(data=centroids_east,  aes(x,y)) +
    geom_segment(data=centroids_east,  aes(x=x,y=y,xend=x1,yend=y1)) +
    geom_text(data=centroids_east,
              aes(x1,
                  y1,
                  label=state,
                  size=5,
                  hjust="left"),
              show.legend=FALSE) +
    #East callout boxes
    geom_label2(data=centroids_east,
                aes(x3, y1, fill=n2, label=n),
                size=4,
                show.legend=FALSE) +
    geom_text(data=centroids_east,
              aes(x3,
                  y1,
                  label = n,
                  color = n > ifelse(centroids_east %>% count(n) %>% nrow() > 3, 6,1000)),
              size=4.5,
              fontface = 'bold',
              show.legend=FALSE) +
    #South callout boxes
    geom_text(data=centroids_south,
              aes(x1,
                  y,
                  label=state,
                  size=5,
                  hjust="left"),
              show.legend=FALSE) +
    geom_label2(data=centroids_south,
                aes(x, y, fill=n2, label=n),
                size=4,
                show.legend=FALSE) +
    geom_text(data=centroids_south,
              aes(x,
                  y,
                  label = n,
                  color = n > ifelse(centroids_south %>% count(n) %>% nrow() > 3, 6,1000)),
              size=4.5,
              fontface = 'bold',
              show.legend=FALSE) +
    #Set theme data to clean up map
    theme(
      axis.text=element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      legend.position = c(.96, .4),
      legend.justification = c("right", "top"),
      legend.text = element_text(size=12),
      legend.title = element_text(size=12),
      plot.margin = grid::unit(c(0,0,0,-1.5), "cm"),
      plot.title = element_text(family = 'Helvetica',
                                color = '#2471A3',
                                face = 'bold',
                                size = 18,
                                vjust = 0.1,
                                hjust = 0.5),
      legend.box.spacing = grid::unit(5, "cm")
    ) +
    #Use custom color palette
    scale_fill_manual(name = "Cases", values = my.cols) +
    #Scale the map
    scale_x_continuous(expand = c(.08,0))
}
lapply(1:length(list_of_types), function(i) choro_mapping(i))
}
