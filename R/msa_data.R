##' This function parses FASTA files or other sequence objects. And assign color to each molecule (amino acid or nucleotide) according to the selected color scheme.
##'
##'
##' @title msa_data
##' @param tidymsa sequence alignment with data frame, generated by tidy_msa().
##' @param font font families, possible values are 'helvetical', 'mono', and 'DroidSansMono', 'TimesNewRoman'. . Defaults is 'helvetical'. If you specify font = NULL, only the background box will be printed.
##' @param color A Color scheme. One of 'Clustal', 'Chemistry_AA', 'Shapely_AA', 'Zappo_AA', 'Taylor_AA','LETTER','CN6', 'Chemistry_NT', 'Shapely_NT', 'Zappo_NT', 'Taylor_NT'.Defaults is 'Chemistry_AA.
##' @param custom_color A data frame with two cloumn called "names" and "color".Customize the color scheme.
##' @param order vectors.Specified sequences order.
##' @param char_width a numeric vector. Specifying the character width in the range of 0 to 1. Defaults is 0.9.
##' @param by_conservation a logical value. The most conserved regions have the brightest colors.
##' @param consensus_views a logical value that opeaning consensus views.
##' @param use_dot a logical value. Displays characters as dots instead of fading their color in the consensus view.
##' @param disagreement a logical value. Displays characters that disagreememt to consensus(excludes ambiguous disagreements).
##' @param ignore_gaps a logical value. When selected TRUE, gaps in column are treated as if that row didn't exist.
##' @param ref a character string. Specifying the reference sequence which should be one of input sequences when 'consensus_views' is TRUE.
##' @return A data frame
##' @examples
##' fasta <- system.file("extdata/sample.fasta", package="ggmsa")
##' data <- msa_data(fasta, 20, 120, font = "helvetical", color = 'Chemistry_AA' )
## @export
##' @noRd
##' @author Guangchuang Yu, Lang Zhou
msa_data <- function(tidymsa, font = "helvetical",
                     color = "Chemistry_AA",
                     custom_color = NULL,
                     char_width = 0.9,
                     by_conservation = FALSE,
                     consensus_views = FALSE,
                     use_dot = FALSE,
                     disagreement = TRUE,
                     ignore_gaps = FALSE,
                     ref = NULL,
                     bias_cons = 0.5) {

    if (is.null(custom_color)) {
        color <- match.arg(color, c("Clustal", "Chemistry_AA", "Shapely_AA", "Zappo_AA", "Taylor_AA","Chemistry_NT",
                                    "Shapely_NT", "Zappo_NT", "Taylor_NT", "LETTER", "CN6", "Hydrophobicity" ))
    }
    y <- tidymsa

    ## add color
    if (color == "Clustal"){
        y <- color_Clustal(y)
    }else {
        if (consensus_views) {
            consensus <- get_consensus(y, ignore_gaps = ignore_gaps, ref = ref)
            tc <- color_scheme(y, color) %>% tidy_color(consensus, disagreement, ref = ref) #colors cleaned
            y <- color_scheme(consensus, color) %>% rbind(tc) #add consensus sequence

            if (use_dot){
                y[is.na(y$color), "character"] <- "."
            }else {
                y$font_color <- "#000000"
                y[is.na(y$color), "font_color"] <- "#aaacaf"
            }

        }else {
            y <- color_scheme(y, color, custom_color)
        }
    }

    if (by_conservation){
        y <- color_visibility(y, bias_cons)
    }


    if (is.null(font)) {
        return(y)
    }

    ## calling internal polygons
    font_f <- font_fam[[font]]
    data_sp <- font_f[as.character(unique(y$character))] #debug using'as.character()'

    ## To adapt to tree data
    if (!'name' %in% names(y) & !consensus_views) {
        if ('label' %in% names(y)) {
            ## y <- dplyr::rename(y, name = label)
            names(y)[names(y) == 'label'] <- "name"
        }else {
            stop("unknown sequence name...")
        }
    }

    if(!is.factor(y$name) & !consensus_views){
        lev <- unique(data.frame(y[,c("name","y")]))
        lev <- lev[order(lev$y), "name"] # y is the order of the nodes in the tree
        y$name <- factor(y$name, levels = lev)
    } else if(consensus_views) {
        y$name <- order_name(y$name, consensus_views = consensus_views, ref = ref)
    }
    y$ypos <- as.numeric(y$name)

    # for ggtreeExtra
    if ("new_position" %in% colnames(y)) {
        scale_n <- 5 * length(unique(y$name))/diff(range(y$new_position))
        char_width <- char_width * diff(range(y$new_position))/diff(range(y$position))
    }

    yy <- lapply(1:nrow(y), function(i) {
        d <- y[i, ]
        dd <- data_sp[[d$character]]
        if(d$character == "."){ # '.' without zooming
          if ("new_position" %in% colnames(d)){
              dd$x <- dd$x - min(dd$x) + d$new_position - diff(range(dd$x))/2
          }else{
              dd$x <- dd$x - min(dd$x) + d$position - diff(range(dd$x))/2
          }
          dd$y <- dd$y - min(dd$y) + d$ypos - diff(range(dd$y))/2
        }else {# other characters
            char_scale <- diff(range(dd$x))/diff(range(dd$y))#equal proportion
            if(diff(range(dd$x)) <= diff(range(dd$y))) {#y_width = char_width, x-width scaled proportionally
                dd$x <- dd$x * (char_width * char_scale)/diff(range(dd$x))
                # for ggtreeExtra
                if ("new_position" %in% colnames(d)){
                    dd$y <- (dd$y * char_width)/diff(range(dd$y)) * scale_n
                    dd$x <- dd$x - min(dd$x) + d$new_position - (char_width * char_scale)/2
                    dd$y <- dd$y - min(dd$y) + d$ypos - scale_n * char_width/2
                }else{
                    dd$y <- (dd$y * char_width)/diff(range(dd$y))
                    dd$x <- dd$x - min(dd$x) + d$position - (char_width * char_scale)/2
                    dd$y <- dd$y - min(dd$y) + d$ypos - char_width/2
                }
            }else{                                       #x_width = char_width, y-width scaled proportionally
                dd$x <- dd$x * char_width/diff(range(dd$x))
                # for ggtreeExtra
                if ("new_position" %in% colnames(d)){
                    dd$y <- dd$y * char_width/(diff(range(dd$y)) * char_scale) * scale_n
                    dd$x <- dd$x - min(dd$x) + d$new_position - char_width/2
                    dd$y <- dd$y - min(dd$y) + d$ypos - (scale_n * char_width/char_scale)/2
                }else{
                    dd$y <- dd$y * char_width/(diff(range(dd$y)) * char_scale)
                    dd$x <- dd$x - min(dd$x) + d$position - char_width/2
                    dd$y <- dd$y - min(dd$y) + d$ypos - (char_width/char_scale)/2
                }
            }
        }
        cn <- colnames(d)
        cn <- cn[!cn %in% c('x','y', 'ypos')]
        for (nn in cn) {
            dd[[nn]] <- d[[nn]]
        }
        ## dd$name <- d$name
        ## dd$position <- d$position
        dd$group <- paste0("V", d$position, "L", d$ypos)
        #dd$group <- paste0(d$position, d$ypos)
        ## dd$character <- d$character
        ## dd$color <- d$color
        #dd <- dd[order(dd$order),]
        return(dd)
    })

    ydf <- do.call(rbind, yy)
    colnames(ydf)[colnames(ydf) == 'y'] <- 'yy'
    ydf$y <- as.numeric(ydf$name)
    ydf <- cbind(label = ydf$name, ydf)
    return(ydf)
}

##' Convert msa file/object to tidy data frame.
##'
##'
##' @title tidy_msa
##' @param msa multiple sequence alignment file or sequence object in DNAStringSet, RNAStringSet, AAStringSet, BStringSet, DNAMultipleAlignment, RNAMultipleAlignment, AAMultipleAlignment, DNAbin or AAbin
##' @param start start position to extract subset of alignment
##' @param end end position to extract subset of alignemnt
##' @return tibble data frame
##' @export
##' @author Guangchuang Yu
tidy_msa <- function(msa, start = NULL, end = NULL) {
    aln <- prepare_msa(msa)
    alnmat <- lapply(seq_along(aln), function(i) {
        base::strsplit(as.character(aln[[i]]), '')[[1]]
    }) %>% do.call('rbind', .)
    ## for DNAbin and AAbin
    ## alnmat <- lapply(seq_along(aln), function(i) as.character(aln[[i]])) %>% do.call('rbind',. )
    alndf <- as.data.frame(alnmat, stringsAsFactors = F)

    if(unique(names(aln)) %>% length == length(aln)) {
        alndf$name = names(aln)
    }else{
      stop("Sequences must have unique names")
    }
    cn = colnames(alndf)
    cn <- cn[!cn %in% "name"]
    df <- gather(alndf, "position", "character", cn)

    y <- df
    y$position = as.numeric(sub("V", "", y$position))
    y$character = toupper(y$character)

    y$name = factor(y$name, levels=rev(names(aln)))


    if (is.null(start)) start <- min(y$position)
    if (is.null(end)) end <- max(y$position)

    y <- y[y$position >=start & y$position <= end, ]

    return(y)
}





##' This function converts the msa_data to the tidy data.
##'
##' @param msaData sequence alignment data generated by msa_data().
msa2tidy <- function(msaData) {
  if ("order" %in% names(msaData)) {
    msaData <- msaData[msaData$order == 1,]
  }
  df_tidy <- data.frame(name = msaData$name,
                        position = msaData$position,
                        character = msaData$character)
  df_tidy$character <- as.character(df_tidy$character)

  return(df_tidy)
}


