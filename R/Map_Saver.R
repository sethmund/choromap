#' @param Data Data.frame containing your data
#' @param file_type Select output file type for maps, e.g. "pdf","png","eps".
#'
#' This is a saving function for 508 compliant choropleth maps

#' @export

Map_Saver <- function(Data, file_type) {

  plots <- NSTools::NST_Choro(Data)

  for(i in 1:length(unique(sort(Data$Type)))) {
    ggplot2::ggsave(plot = plots[[i]],
           file=paste0(as.vector(unique(sort(Data$Type)))[i], ".",file_type),
           height=8,
           width=11,
           device = file_type)

  }
}
