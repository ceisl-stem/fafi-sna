library(readr)
#library(ggimage)
library(tidyr)
library(dplyr)
library(igraph)
library(tidygraph)
library(ggraph)
library(AnthroTools)
library(scales)
library(ggpubr)
library(info.centrality)
library(CINNA)
library(glue)
library(rio)
#library(magick)
library(intergraph)

the.palette <<- c("FL" = "#6929c4", "NC" = "#1192e8", "PA" = "#005d5d",
                  "CF" = "#9f1853", "FF" = "#fa4d56", "IL" = "#570408",
                  "OR" = "#198038", "OT" = "#002d9c", "SA" = "#ee538b",
                  "ST" = "#b28600", "UA" = "#009d9a", "UF" = "#012749",
                  "US" = "#8a3800")

#recolor.icon <- function(the.color) {
#  the.icon <- image_colorize(person.icon, 100, the.palette[the.color])
#  the.icon <- as.raster(the.icon)
#  return(the.icon)
#}

#person.icon <<- image_read("person-icon.png")

#icon.list <<- list("FL" = recolor.icon("FL"), "NC" = recolor.icon("NC"),
#                  "FF" = recolor.icon("FF"), "IL" = recolor.icon("IL"),
#                  "OR" = recolor.icon("OR"), "OT" = recolor.icon("OT"),
#                  "SA" = recolor.icon("SA"), "ST" = recolor.icon("ST"),
#                  "UA" = recolor.icon("UA"), "UF" = recolor.icon("UF"),
#                  "US" = recolor.icon("US"))
#icon.frame <<- data.frame(code = c("FL", "NC", "FF", "IL", "OR", "OT", "SA",
#                                   "ST", "UA", "UF", "US"),
#                          icon = list(recolor.icon("FL"), recolor.icon("NC"),
#                                      recolor.icon("FF"), recolor.icon("IL"),
#                                      recolor.icon("OR"), recolor.icon("OT"),
#                                      recolor.icon("SA"), recolor.icon("ST"),
#                                      recolor.icon("UA"), recolor.icon("UF"),
#                                      recolor.icon("US")))

set.graph <- function(the.frame) {
  the.graph <- the.frame |>
    select("from", "to") |>
    graph_from_data_frame(directed = TRUE)
  return(the.graph)
}

calculate.salience <- function(the.frame, the.grouping, the.file) {
  the.filename <- glue("output/salience_{the.file}.csv")
  if(the.grouping == "none") {
    anthro.frame <- the.frame |>
      select("Subj" = "from", "Order" = "order", "CODE" = "to", "GROUPING" = "question") |>
      add_count(Subj, GROUPING) |>
      dplyr::filter(n > 1) |>
      select("Subj", "Order", "CODE") |>
      dplyr::distinct() |>
      as.data.frame()
    anthro.frame$Order <- as.numeric(anthro.frame$Order)
    the.salience <- CalculateSalience(anthro.frame)
  } else {
  anthro.frame <- the.frame |>
    select("Subj" = "from", "Order" = "order", "CODE" = "to", "GROUPING" = "question") |>
    add_count(Subj, GROUPING) |>
    dplyr::filter(n > 1) |>
    select("Subj", "Order", "CODE", "GROUPING") |>
    dplyr::distinct() |>
    as.data.frame()
  anthro.frame$Order <- as.numeric(anthro.frame$Order)
  the.salience <- CalculateSalience(anthro.frame, GROUPING = "GROUPING")
  }
  code.salience <- SalienceByCode(the.salience, dealWithDoubles = "MAX")
  write_csv(code.salience, the.filename, append = FALSE)
  return(code.salience)
}

get_icon <- function(the.code) {
  icon.list[[the.code]]
}

draw.graph <- function(the.graph, the.salience, the.file) {
  the.filename <- glue("output/sna_{the.file}-plot.pdf")
  the.salience <- the.salience %>%
    select(CODE, SmithsS) %>%
    dplyr::rename("name" = "CODE")
  node.data <- data.frame(name = V(the.graph)$name) %>%
    mutate(color_code = substr(V(the.graph)$name, 1, 2)) %>%
    dplyr::left_join(the.salience) %>%
    replace_na(list(SmithsS = 0.01))
  V(the.graph)$color_code <- node.data$color_code
  V(the.graph)$size_code <- (node.data$SmithsS) * 100
  the.plot <- the.graph |>
    ggraph(layout = "fr") +
    geom_edge_fan(color = "#A7A9AB") +
    geom_node_point(aes(color = color_code,
                        size = size_code),
                        show.legend = FALSE) +
    scale_size_continuous(range = c(2.5,10)) +
    scale_color_manual(values = the.palette) +
    geom_node_text(aes(label = name), repel = TRUE) +
    labs(edge_width = "Letters") +
    theme_graph()
  #print(the.plot)
  ggsave(the.plot, filename = the.filename, width = 11.5, height = 8, units = "in", dpi = 300)
  return(the.plot)
}

calculate.centrality <- function(the.frame, the.salience, the.file) {
  the.filename <- glue("output/table_{the.file}.csv")
  the.frame <- the.frame |>
    select(from, to)
  the.salience <- the.salience |>
    rename("actor" = "CODE") |>
    select(actor, SmithsS)
  the.graph <- simplify(
    graph_from_edgelist(as.matrix(the.frame), directed = TRUE),
    remove.loops = TRUE,
    remove.multiple = TRUE
  )
  data.degree <- centralization.degree(the.graph, mode = "all", loops = FALSE)
  #data.net.degree <- the.graph$centralization
  data.betweenness <- centralization.betweenness(the.graph, directed = FALSE)
  #data.net.betweenness <- data.betweenness$centralization
  data.closeness <- centralization.closeness(the.graph, mode = "all")
  #data.net.closeness <- data.closeness$centralization
  data.eigen <- centralization.evcent(the.graph, directed = FALSE)
  data.coreness <- coreness(the.graph, mode = "all")
  #data.nef <- network.efficiency(the.graph)
  #data.info <- info.centrality.network(the.graph)
  #data.har <- harmonic_centrality(the.graph)
  #data.grp <- group_centrality(the.graph)
  data.lbc <- local_bridging_centrality(the.graph)
  #data.alpha <- alpha_centrality(the.graph)
  #data.auth <- authority_score(the.graph)
  #data.pow <- power_centrality(the.graph)
  analysis.network.data <- data.frame(degree = data.degree$res,
                                      betweenness = data.betweenness$res,
                                      closeness = data.closeness$res,
                                      eigen = data.eigen$vector,
                                      coreness = data.coreness,
                                      #efficiency = data.nef,
                                      #info = data.info,
                                      #harmonic = data.har,
                                      #group = data.grp,
                                      lbc = data.lbc)
  analysis.network.data$actor <- rownames(analysis.network.data)
  rownames(analysis.network.data) <- NULL
  analysis.network.data <- analysis.network.data |>
    select(actor, everything()) |>
    left_join(the.salience) |>
    replace_na(list(SmithsS = 0))
  write_csv(analysis.network.data, the.filename, append = FALSE)
  return(analysis.network.data)
}

calculate.keyactors <- function(the.frame, the.file) {
  the.filename <- glue("output/keyactors_{the.file}.pdf")
  table.filename <- glue("output/table_{the.file}_keyactors.csv")
  key.frame <- the.frame %>%
    select(actor, betweenness, eigen)
  #key.frame$scaled_betweenness <-
  #  rescale(key.frame$betweenness, to = c(-1, 1))
  #key.frame$scaled_eigen <- rescale(key.frame$eigen, to = c(-1, 1))
  #key.frame$interaction_score <-
  #  (key.frame$scaled_betweenness * key.frame$scaled_eigen)
  #key.frame$interaction_score <-
  #  rescale(key.frame$interaction_score, to = c(-1, 1))
  key.res <- lm(eigen ~ betweenness, data = key.frame)$residuals
  key.frame <- transform(key.frame, residuals = key.res)
  #key.frame <- key.frame %>%
  #  select(actor, betweenness, eigen)
  node.data <- data.frame(actor = key.frame$actor) %>%
    mutate(color_code = substr(key.frame$actor, 1, 2))
  key.frame <- key.frame %>%
    left_join(node.data, by = "actor")
  key.ymedian <- median(key.frame$eigen)
  key.xmedian <- median(key.frame$betweenness)
  key.ymean <- mean(key.frame$eigen)
  key.xmean <- mean(key.frame$betweenness)
  key.xmin <- min(key.frame$betweenness)# - 0.1
  key.xmax <- max(key.frame$betweenness)# + 0.1
  key.ymin <- min(key.frame$eigen)# - 0.1
  key.ymax <- max(key.frame$eigen)# + 0.1
  key.xmid <- (key.xmin + key.xmax)/2
  key.ymid <- (key.ymin + key.ymax)/2
  key.frame <- key.frame %>%
    mutate(keystatus = case_when((eigen > key.ymean & betweenness > key.xmean) ~ "Sage",
                                 (eigen > key.ymean & betweenness < key.xmean) ~ "Steward",
                                 (eigen < key.ymean & betweenness > key.xmean) ~ "Weaver")) %>%
    na.omit()
  
#  pt.count <- key.frame %>%
#    count(keystatus) %>%
#    filter(keystatus == "Pulse-Taker") %>%
#    pull(n)
#  pt.count <- ifelse(is.numeric(pt.count), pt.count, 0)
#  pt.count <- pt.count %>% replace_na(0)

#  wv.count <- key.frame %>%
#    count(keystatus) %>%
#    filter(keystatus == "Weaver") %>%
#    pull(n)
#  wv.count <- ifelse(is.numeric(wv.count), wv.count, 0)
#  wv.count <- wv.count %>% replace_na(0)
  
#  hb.count <- key.frame %>%
#    count(keystatus) %>%
#    filter(keystatus == "Hub") %>%
#    pull(n)
#  hb.count <- ifelse(is.numeric(hb.count), hb.count, 0)
#  hb.count <- hb.count %>% replace_na(0)
    
  key.plot <-
    ggscatter(
      key.frame,
      x = "betweenness",
      y = "eigen",
      label = "actor",
      label.rectangle = FALSE,
      repel = TRUE,
      theme = theme_minimal(),
      xlab = "Betweenness Centrality",
      ylab = "Eigenvector Centrality",
      point = TRUE,
      #size = "residuals",
      ylim = c(0, 1),
      xlim = c(key.xmin, key.xmax),
      show.legend = FALSE,
      color = "color_code",
      palette = the.palette,
      conf.int = FALSE, 
      cor.coef = FALSE,
      legend = "none"
    ) +
    geom_hline(yintercept = key.ymean, color = "#243142", alpha = 0.2) +
    geom_vline(xintercept = key.xmean, color = "#243142", alpha = 0.2) +
    geom_label(aes(x = (key.xmean - key.xmin)/2, y = (key.ymean + key.ymax)/2, label = "Stewards", hjust = 0.5),
               color = "#243142", fill = "#EEEEEE") +
    geom_label(aes(x = (key.xmean + key.xmax)/2, y = (key.ymean - key.ymin)/2,
                   label = "Weavers", hjust = 0.5), color = "#243142", fill = "#EEEEEE") +
    geom_label(aes(x = (key.xmean + key.xmax)/2, y = (key.ymean + key.ymax)/2,
                   label = "Sages", hjust = 0.5), color = "#243142", fill = "#EEEEEE")
#  if(pt.count != 0) {
#    key.plot <- key.plot +
#      geom_label(aes(x = (key.xmean - key.xmin)/2, y = (key.ymean + key.ymax)/2, label = "Pulse-Takers", hjust = 0.5),
#        color = "#243142", fill = "#EEEEEE")
#  }
#  if(wv.count != 0) {
#    geom_label(aes(x = (key.xmean + key.xmax)/2, y = (key.ymean - key.ymin)/2,
#        label = "Weavers", hjust = 0.5), color = "#243142", fill = "#EEEEEE")
#  }
#  if(hb.count != 0) {
#    geom_label(aes(x = (key.xmean + key.xmax)/2, y = (key.ymean + key.ymax)/2,
#                   label = "Hubs", hjust = 0.5), color = "#243142", fill = "#EEEEEE")
#  }
#  print(key.plot)
  key.frame <- key.frame %>%
    #dplyr::group_by(keystatus) %>%
    arrange(desc(keystatus), actor)# %>%
  #slice(1:20) %>% ungroup()
  ggsave(key.plot, filename = the.filename, width = 11.5, height = 8, units = "in", dpi = 300)
  write_csv(key.frame, table.filename, append = FALSE)
  return(key.frame)
}

pates.frame <- import("https://osf.io/download/62qpa/", format = "csv") |>
  mutate_all(toupper) |>
  pivot_longer(cols = starts_with("Q"),
               names_to = "question",
               values_to = "to") |>
  drop_na() |>
  dplyr::select("question", "from" = "id", "to") |>
  separate(col = question, into = c("question", "order"), sep = "_") |>
  dplyr::filter(to != "")

ncfl.frame <- import("https://osf.io/download/ghz3c/", format = "csv") |>
  mutate_all(toupper) |>
  pivot_longer(cols = starts_with("Q"),
               names_to = "question",
               values_to = "to") |>
  drop_na() |>
  dplyr::select("question", "from" = "ID", "to") |>
  separate(col = question, into = c("question", "order"), sep = "_") |>
  dplyr::filter(to != "")

full.frame <- rbind(pates.frame, ncfl.frame)

full.salience <- calculate.salience(full.frame, "GROUPING", "full")

pates.salience <- calculate.salience(pates.frame, "GROUPING", "pates")

ncfl.salience <- calculate.salience(ncfl.frame, "GROUPING", "ncfl")

nc.salience <- ncfl.frame |>
  dplyr::filter(from == "NC01" | from == "NC02" | from == "NC03") |>
  calculate.salience("GROUPING", "nc")

fl.salience <- ncfl.frame |>
  dplyr::filter(from == "FL01" | from == "FL02" | from == "FL03") |>
  calculate.salience("GROUPING", "fl")

# Just in case weight needs to get added back in, here it is
#df <- full.frame[,2:3]
#df <- df |>
#  count(from, to, name = "weight")
#geom_edge_link(aes(edge_width = weight)) + 
#scale_edge_width_continuous(range = c(0.1, .6)) +

full.graph <- set.graph(full.frame)
full.plot <- draw.graph(full.graph, full.salience, "full")
#full.plot

pates.full.graph <- set.graph(pates.frame)
pates.full.plot <- draw.graph(pates.full.graph, pates.salience, "pates")
#pates.full.plot

ncfl.full.graph <- set.graph(ncfl.frame)
ncfl.full.plot <- draw.graph(ncfl.full.graph, ncfl.salience, "ncfl")
#ncfl.full.plot

#full.q1.graph <- full.frame |>
#  dplyr::filter(question == "Q1") |>
#  set.graph()
#full.q1.plot <- draw.graph(full.q1.graph, full.salience, "Q1_full")
#full.q1.plot

pates.q1.graph <- pates.frame |>
  filter(question == "Q1") |>
  set.graph()
pates.q1.plot <- draw.graph(pates.q1.graph, pates.salience, "q1_pates")
#pates.q1.plot

pates.q3.graph <- pates.frame |>
  filter(question == "Q3") |>
  set.graph()
pates.q3.plot <- draw.graph(pates.q3.graph, pates.salience, "q3_pates")
#pates.q3.plot

pates.q4.graph <- pates.frame |>
  filter(question == "Q4") |>
  set.graph()
pates.q4.plot <- draw.graph(pates.q4.graph, pates.salience, "q4_pates")
#pates.q4.plot

ncfl.q1.graph <- ncfl.frame |>
  filter(question == "Q1") |>
  set.graph()
ncfl.q1.plot <- draw.graph(ncfl.q1.graph, ncfl.salience, "q1_ncfl")
#ncfl.q1.plot

ncfl.q3.graph <- ncfl.frame |>
  filter(question == "Q3") |>
  set.graph()
ncfl.q3.plot <- draw.graph(ncfl.q3.graph, ncfl.salience, "q3_ncfl")
#ncfl.q3.plot

nc.q3.graph <- ncfl.frame |>
  filter(question == "Q3") |>
  filter(from == "NC01" | from == "NC02" | from == "NC03") |>
  set.graph()
nc.q3.plot <- draw.graph(nc.q3.graph, nc.salience, "q3_nc")
#nc.q3.plot

fl.q3.graph <- ncfl.frame |>
  filter(question == "Q3") |>
  filter(from == "FL01" | from == "FL02" | from == "FL03") |>
  set.graph()
fl.q3.plot <- draw.graph(fl.q3.graph, fl.salience, "q3_fl")
#fl.q3.plot

ncfl.q4.graph <- ncfl.frame |>
  filter(question == "Q4") |>
  set.graph()
ncfl.q4.plot <- draw.graph(ncfl.q4.graph, ncfl.salience, "q4_ncfl")
#ncfl.q4.plot

full.cent <- calculate.centrality(full.frame, full.salience, "full")

full.key <- calculate.keyactors(full.cent, "full")

pates.cent <- calculate.centrality(pates.frame, pates.salience, "pates")

pates.key <- calculate.keyactors(pates.cent, "pates")

ncfl.cent <- calculate.centrality(ncfl.frame, ncfl.salience, "ncfl")

ncfl.key <- calculate.keyactors(ncfl.cent, "ncfl")

#nc.cent <- ncfl.frame |>
#  select("from", "to") |>
#  dplyr::filter(from == "NC01" | from == "NC02" | from == "NC03") |>
#  calculate.centrality(nc.salience, "nc")

#nc.key <- calculate.keyactors(nc.cent, "nc")

#fl.cent <- ncfl.frame |>
#  select("from", "to") |>
#  dplyr::filter(from == "FL01" | from == "FL02" | from == "FL03") |>
#  calculate.centrality(fl.salience, "fl")

#fl.key <- calculate.keyactors(fl.cent, "fl")




