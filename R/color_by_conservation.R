color_increment <- function(conservation_visibility,bias_cons){
    lapply(1:nrow(conservation_visibility), function(i){
        color_ramp <- colorRampPalette(colors = c(conservation_visibility[i,"color"],"#ffffff", "#ffffff"),bias = bias_cons)
        color_change <- rev(color_ramp(100))[conservation_visibility[i,"visibility"]]
        return(color_change)
        }) %>% unlist #%>% do.call("rbind",.)

}


color_visibility <- function(y,bias_cons){
    #options(digits = 2)
    #on.exit()
    conser_data <- bar_data(y)
    conser_data$visibility <- conser_data$Freq / length(levels(y[[1]])) %>% round(2)
    conser_data$visibility <- conser_data$visibility * 100
    names(conser_data)[3] <- "position"
    y_filter <- y[c(-1,-3)] #%>% unique
    conser_ready <- merge(conser_data, y_filter)
    y$color <- color_increment(conser_ready,bias_cons)
    return(y)
}
