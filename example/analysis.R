#load the DynCommPhylo package (https://github.com/masonyoungblood/DynCommPhylo/)
library(DynCommPhylo)

#set workspace
#setwd("~/example")

# CONSTRUCT + SIMPLIFY NETWORK --------------------------------------------

#convert TILES output to network (very time consuming)
original_network <- network_construction(file = "edges.tsv", years = c(1970:1999))

#simplify network
simplified_network <- network_simplification(links = original_network$links, nodes = original_network$nodes)

# RUN LDA + GENERATE PLOT CAPTIONS ----------------------------------------

#load the parsed releases used to construct the TILES input file
load("releases.RData") #called data_clean once loaded

#get plot years from nodes
plot_years <- as.numeric(sort(unique(gsub(".*_", "", simplified_network$nodes$population))))

#create datatable for releases in the network (very time consuming)
all_releases <- data.table::data.table(population = c(0), releases = c(0))
for(i in 1:length(plot_years)){
  data_clean_sub <- data_clean[which(data_clean$year == plot_years[i]), ]
  ids <- which(gsub(".*_", "", simplified_network$nodes$population) == plot_years[i])
  if(length(ids) > 0){
    nodes_sub <- simplified_network$nodes[ids,]
    for(j in 1:nrow(nodes_sub)){
      temp <- nodes_sub$individuals[j][[1]]
      releases <- c()
      for(k in 1:length(temp)){
        releases <- c(releases, data_clean_sub$id[sapply(1:nrow(data_clean_sub), function(x){temp[k] %in% data_clean_sub$artists[[x]]})])
      }
      releases <- unique(releases)
      all_releases <- data.table::rbindlist(list(all_releases, data.table::data.table(rep(nodes_sub$population[j], length(releases)), releases)), use.names = FALSE)
    }
  }

  rm(list = c("data_clean_sub", "ids", "nodes_sub", "temp", "releases"))
  cat(paste0(i, "..."))
}
all_releases <- all_releases[-1,]

#get rid of duplicates, so releases by artists from multiple populations are excluded
all_releases <- all_releases[-which(duplicated(all_releases$releases)|duplicated(all_releases$releases, fromLast = TRUE)),]

#store styles and artists in all_releases
all_releases$styles <- sapply(1:nrow(all_releases), function(x){data_clean$styles[which(data_clean$id == all_releases$releases[x])]})
all_releases$artists <- sapply(1:nrow(all_releases), function(x){data_clean$artists[which(data_clean$id == all_releases$releases[x])]})

#add info from all_releases to nodes - all_artists is full list of non-unique artists on releases (for weighted log odds)
simplified_network$nodes$styles <- sapply(1:nrow(simplified_network$nodes), function(x){unlist(all_releases$styles[which(all_releases$population == simplified_network$nodes$population[x])])})
simplified_network$nodes$all_artists <- sapply(1:nrow(simplified_network$nodes), function(x){unlist(all_releases$artists[which(all_releases$population == simplified_network$nodes$population[x])])})

#create tibble of styles and artists (for weighted log odds and LDA)
all_pops <- sort(as.numeric(unique(gsub("_.*", "", simplified_network$nodes$population))))
tibble_styles <- data.frame(group = NA, style = NA, n = NA)
tibble_artists <- data.frame(group = NA, artist = NA, n = NA)
for(i in 1:length(all_pops)){
  temp_styles <- table(unlist(simplified_network$nodes$styles[which(gsub("_.*", "", simplified_network$nodes$population) == all_pops[i])]))
  temp_artists <- table(unlist(simplified_network$nodes$all_artists[which(gsub("_.*", "", simplified_network$nodes$population) == all_pops[i])]))
  tibble_styles <- rbind(tibble_styles, data.frame(group = rep(all_pops[i], length(temp_styles)), style = names(temp_styles), n = as.numeric(temp_styles)))
  tibble_artists <- rbind(tibble_artists, data.frame(group = rep(all_pops[i], length(temp_artists)), artist = names(temp_artists), n = as.numeric(temp_artists)))
  rm(list = c("temp_styles", "temp_artists"))
}
tibble_styles <- tibble::as_tibble(tibble_styles[-1,])
tibble_artists <- tibble::as_tibble(tibble_artists[-1,])

#calculate weighted log odds
tibble_styles <- tidylo::bind_log_odds(tibble_styles, group, style, n, uninformative = TRUE)
tibble_artists <- tidylo::bind_log_odds(tibble_artists, group, artist, n, uninformative = TRUE)

#build dtm of styles
dtm_styles <- tidytext::cast_dtm(tibble_styles, group, style, n)

#identify ten highest log odds artists, and three highest log odds styles, in each population
top_terms <- data.table::data.table(population = all_pops,
                        artists = sapply(1:length(all_pops), function(x){list(tibble_artists$artist[which(tibble_artists$group == all_pops[x])][order(tibble_artists$log_odds_weighted[which(tibble_artists$group == all_pops[x])], decreasing = TRUE)[1:10]])}),
                        styles = sapply(1:length(all_pops), function(x){list(tibble_styles$style[which(tibble_styles$group == all_pops[x])][order(tibble_styles$log_odds_weighted[which(tibble_styles$group == all_pops[x])], decreasing = TRUE)[1:3]])}))

#remove NAs
if(length(which(is.na(unlist(top_terms$artists)))) > 0){
  top_terms$artists <- sapply(1:nrow(top_terms), function(x){top_terms$artists[[x]][!is.na(top_terms$artists[[x]])]})
}
if(length(which(is.na(unlist(top_terms$styles)))) > 0){
  top_terms$styles <- sapply(1:nrow(top_terms), function(x){top_terms$styles[[x]][!is.na(top_terms$styles[[x]])]})
}

#run LDA
seed <- 1234
k <- 69
lda_styles <- topicmodels::LDA(dtm_styles, k = k, method = "VEM")

#we have skipped the step where artist IDs are converted to text names, due to the large size of the artist_thesaurus file (>150 MB)

#generate plot captions
top_terms$captions <- rep(NA, nrow(top_terms))
for(i in 1:nrow(top_terms)){
  if(length(top_terms$styles[[i]]) < 1){
    top_terms$captions[i] <- paste0("#", top_terms$cluster[i], " --- TOP STYLES: ", "NA", " --- TOP ARTISTS: ", "NA")
  }
  if(length(top_terms$styles[[i]]) > 0){
    top_terms$captions[i] <- paste0("#", top_terms$cluster[i], " --- TOP STYLES: ", paste0(top_terms$styles[[i]], collapse = ", "), " --- TOP ARTISTS: ", paste0(top_terms$artists[[i]], collapse = ", "))
  }
}
captions <- sapply(1:nrow(simplified_network$nodes), function(x){top_terms$captions[which(top_terms$population == gsub("_.*", "", simplified_network$nodes$population[x]))]})

# PLOT PHYLOGENY ----------------------------------------------------------

#plot phylogeny
plot <- phylo_plotting(links = simplified_network$links, nodes = simplified_network$nodes[,1:2], captions = captions)

#export phylogeney as interactive HTML file or static PNG
htmlwidgets::saveWidget(plot, file = "phylogeny.html", title = "Phylogeny")
r2d3::save_d3_png(plot, file = "phylogeny.png", background = "white", width = 1000, height = 600, delay = 1, zoom = 10)
