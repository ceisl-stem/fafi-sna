library(readr)
library(dplyr)
library(tidyr)
library(igraph)
library(tidygraph)
library(ggraph)
library(AnthroTools)
library(scales)
library(ggpubr)
library(info.centrality)

the.palette <<- c("FL" = "#6929c4", "NC" = "#1192e8", "PA" = "#005d5d",
                  "CF" = "#9f1853", "FF" = "#fa4d56", "IL" = "#570408",
                  "OR" = "#198038", "OT" = "#002d9c", "SA" = "#ee538b",
                  "ST" = "#b28600", "UA" = "#009d9a", "UF" = "#012749",
                  "US" = "#8a3800")


set.graph <- function(the.frame) {
  the.graph <- the.frame |>
    select("from", "to") |>
    graph_from_data_frame(directed = TRUE)
  return(the.graph)
}

calculate.salience <- function(the.frame, the.grouping) {
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
  return(code.salience)
}

draw.graph <- function(the.graph, the.salience) {
  the.salience <- the.salience %>%
    select(CODE, SmithsS) %>%
    dplyr::rename("name" = "CODE")
  node.data <- data.frame(name = V(the.graph)$name) %>%
    mutate(color_code = substr(V(the.graph)$name, 1, 2)) %>%
    dplyr::left_join(the.salience) %>%
    replace_na(list(SmithsS = 0.01))
  V(the.graph)$color_code <- node.data$color_code
  V(the.graph)$size_code <- (node.data$SmithsS) * 100
  
  the.graph <- the.graph |>
    ggraph(layout = "fr") +
    geom_edge_fan() +
    geom_node_point(aes(color = color_code,
                        size = size_code),
                    show.legend = F) +
    scale_size_continuous(range = c(2.5,10)) +
    scale_color_manual(values = the.palette) +
    geom_node_text(aes(label = name), repel = TRUE) +
    labs(edge_width = "Letters") +
    theme_graph()
  return(the.graph)
}

calculate.centrality <- function(the.frame) {
  
  the.graph <- simplify(
    graph_from_edgelist(as.matrix(the.frame), directed = TRUE),
    remove.loops = TRUE,
    remove.multiple = TRUE
  )
  
  data.degree <-
    centralization.degree(the.graph, mode = "all", loops = FALSE)
  data.net.degree <- the.graph$centralization
  data.betweenness <-
    centralization.betweenness(the.graph, directed = FALSE)
  data.net.betweenness <- data.betweenness$centralization
  data.closeness <-
    centralization.closeness(the.graph, mode = "all")
  data.net.closeness <- data.closeness$centralization
  data.eigen <- centralization.evcent(the.graph, directed = FALSE)
  data.coreness <- coreness(the.graph, mode = "all")
  
  analysis.network.data <- data.frame(
    degree = data.degree$res,
    betweenness = data.betweenness$res,
    closeness = data.closeness$res,
    eigen = data.eigen$vector,
    coreness = data.coreness
  )
  return(analysis.network.data)
}

calculate.keyactors <- function(the.frame) {
  #the.filename <- glue("output/keyactors{the.cluster}-plot.pdf")
  key.frame <- the.frame %>%
    select(betweenness, eigen)
  key.frame$scaled_betweenness <-
    rescale(key.frame$betweenness, to = c(0.01, 0.99))
  key.frame$scaled_eigen <- rescale(key.frame$eigen, to = c(0, 1))
  key.frame$interaction_score <-
    (key.frame$scaled_betweenness * key.frame$scaled_eigen)
  key.frame$interaction_score <-
    rescale(key.frame$interaction_score, to = c(0, 1))
  key.frame$agent <- rownames(key.frame)
  rownames(key.frame) <- NULL
  #key.frame$cluster <- the.cluster
  key.res <- lm(eigen ~ betweenness, data = key.frame)$residuals
  key.frame <- transform(key.frame, residuals = key.res)
  key.frame <- key.frame %>%
    select(
      agent,
      betweenness,
      scaled_betweenness,
      eigen,
      scaled_eigen,
      interaction_score,
      residuals
    ) #%>%
    #filter(!manuscriptID %in% analysis.cluster.frame$manuscriptID)
  node.data <- data.frame(agent = key.frame$agent) %>%
    mutate(color_code = substr(key.frame$agent, 1, 2))
  key.frame <- key.frame %>%
    left_join(node.data, by = "agent")
  key.ymedian <- median(key.frame$eigen)
  key.xmedian <- median(key.frame$scaled_betweenness)
  key.frame <- key.frame %>%
    mutate(keystatus = case_when((scaled_eigen > key.ymedian &
                                    scaled_betweenness > key.xmedian) ~ "Hub",
                                 (scaled_eigen > key.ymedian &
                                    scaled_betweenness < key.xmedian) ~ "Pulse-Taker",
                                 (scaled_eigen < key.ymedian &
                                    scaled_betweenness > key.ymedian) ~ "Gatekeeper"
    )) %>%
    na.omit()
  
  key.xmin <- min(key.frame$scaled_betweenness) - 0.1
  key.xmax <- max(key.frame$scaled_betweenness) + 0.1
  key.ymin <- min(key.frame$scaled_eigen) - 0.1
  key.ymax <- max(key.frame$scaled_eigen) + 0.1
  
  key.plot <-
    ggscatter(
      key.frame,
      x = "scaled_betweenness",
      y = "scaled_eigen",
      label = "agent",
      label.rectangle = FALSE,
      repel = TRUE,
      theme = theme_minimal(),
      xlab = "Betweenness Centrality (scaled)",
      ylab = "Eigenvector Centrality (scaled)",
      point = TRUE,
      ylim = c(key.ymin, key.ymax),
      xlim = c(key.xmin, key.xmax),
      show.legend = FALSE,
      color = "color_code",
      palette = the.palette,
      legend = "none",
      conf.int = FALSE, 
      cor.coef = FALSE
    ) +
    #geom_smooth(method = "lm", color = "#EEEEEE", se = FALSE, fullrange = TRUE, linetype = "solid") +
    #scale_color_gradient2(low = iu.gradient$low[the.cluster],
    #                      high = iu.gradient$high[the.cluster],
    #                      mid = iu.gradient$mid[the.cluster]) +
    geom_hline(yintercept = key.ymedian) +
    geom_vline(xintercept = key.xmedian) +
    geom_label(
      aes(
        x = key.xmax,
        y = key.ymin,
        label = "Gatekeepers",
        hjust = 1
      ),
      color = "#330D2B",
      fill = "#DECADC"
    ) +
    geom_label(
      aes(
        x = key.xmax,
        y = key.ymax,
        label = "Hubs",
        hjust = 1
      ),
      color = "#330D2B",
      fill = "#DECADC"
    ) +
    geom_label(
      aes(
        x = key.xmin,
        y = key.ymax,
        label = "Pulse-Takers",
        hjust = 0
      ),
      color = "#330D2B",
      fill = "#DECADC"
    )
  print(key.plot)
  #ggsave(
  #  key.plot,
  #  filename = the.filename,
  #  width = 11.5,
  #  height = 8,
  #  units = "in",
  #  dpi = 300
  #)
  key.frame <- key.frame %>%
    #dplyr::group_by(keystatus) %>%
    arrange(desc(keystatus), desc(interaction_score), agent)# %>%
  #slice(1:20) %>% ungroup()
  return(key.frame)
}

pates.frame <- read_csv("data/pates-sna-01.csv", show_col_types = FALSE) |>
  mutate_all(toupper) |>
  pivot_longer(cols = starts_with("Q"),
               names_to = "question",
               values_to = "to") |>
  na.omit() |>
  select("question", "from" = "id", "to") |>
  separate(col = question, into = c("question", "order"), sep = "_")

ncfl.frame <- read_csv("data/ncfl-sna-01.csv", show_col_types = FALSE) |>
  mutate_all(toupper) |>
  pivot_longer(cols = starts_with("Q"),
               names_to = "question",
               values_to = "to") |>
  na.omit() |>
  select("question", "from" = "ID", "to") |>
  separate(col = question, into = c("question", "order"), sep = "_")

full.frame <- rbind(pates.frame, ncfl.frame)

full.salience <- calculate.salience(full.frame, "GROUPING")

pates.salience <- calculate.salience(pates.frame, "GROUPING")

ncfl.salience <- calculate.salience(ncfl.frame, "GROUPING")

nc.salience <- ncfl.frame |>
  dplyr::filter(from == "NC01" | from == "NC02" | from == "NC03") |>
  calculate.salience("GROUPING")

fl.salience <- ncfl.frame |>
  dpylr::filter(from == "FL01" | from == "FL02" | from == "FL03") |>
  calculate.salience("GROUPING")

# Just in case weight needs to get added back in, here it is
#df <- full.frame[,2:3]
#df <- df |>
#  count(from, to, name = "weight")
#geom_edge_link(aes(edge_width = weight)) + 
#scale_edge_width_continuous(range = c(0.1, .6)) +


full.graph <- set.graph(full.frame)
full.plot <- draw.graph(full.graph, full.salience)
full.plot

pates.full.graph <- set.graph(pates.frame)
pates.full.plot <- draw.graph(pates.full.graph, pates.salience)
pates.full.plot

ncfl.full.graph <- set.graph(ncfl.frame)
ncfl.full.plot <- draw.graph(ncfl.full.graph, ncfl.salience)
ncfl.full.plot

full.q1.graph <- full.frame |>
  dplyr::filter(question == "Q1") |>
  set.graph()
full.q1.plot <- draw.graph(full.q1.graph, full.salience)
full.q1.plot

pates.q1.graph <- pates.frame |>
  filter(question == "Q1") |>
  set.graph()
pates.q1.plot <- draw.graph(pates.q1.graph, pates.salience)
pates.q1.plot

pates.q3.graph <- pates.frame |>
  filter(question == "Q3") |>
  set.graph()
pates.q3.plot <- draw.graph(pates.q3.graph, pates.salience)
pates.q3.plot

pates.q4.graph <- pates.frame |>
  filter(question == "Q4") |>
  set.graph()
pates.q4.plot <- draw.graph(pates.q4.graph, pates.salience)
pates.q4.plot

ncfl.q1.graph <- ncfl.frame |>
  filter(question == "Q1") |>
  set.graph()
ncfl.q1.plot <- draw.graph(ncfl.q1.graph, ncfl.salience)
ncfl.q1.plot

ncfl.q3.graph <- ncfl.frame |>
  filter(question == "Q3") |>
  set.graph()
ncfl.q3.plot <- draw.graph(ncfl.q3.graph, ncfl.salience)
ncfl.q3.plot

nc.q3.graph <- ncfl.frame |>
  filter(question == "Q3") |>
  filter(from == "NC01" | from == "NC02" | from == "NC03") |>
  set.graph()
nc.q3.plot <- draw.graph(nc.q3.graph, nc.salience)
nc.q3.plot

fl.q3.graph <- ncfl.frame |>
  filter(question == "Q3") |>
  filter(from == "FL01" | from == "FL02" | from == "FL03") |>
  set.graph()
fl.q3.plot <- draw.graph(fl.q3.graph, fl.salience)
fl.q3.plot

ncfl.q4.graph <- ncfl.frame |>
  filter(question == "Q4") |>
  set.graph()
ncfl.q4.plot <- draw.graph(ncfl.q4.graph, ncfl.salience)
ncfl.q4.plot

full.cent <- full.frame |>
  select("from", "to") |>
  calculate.centrality()

full.key <- calculate.keyactors(full.cent)

pates.full.cent <- pates.frame |>
  select("from", "to") |>
  calculate.centrality()

#pates.key <- calculate.keyactors(pates.full.cent)

ncfl.full.cent <- ncfl.frame |>
  select("from", "to") |>
  calculate.centrality()

#ncfl.key <- calculate.keyactors(ncfl.full.cent)

nc.cent <- ncfl.frame |>
  select("from", "to") |>
  dplyr::filter(from == "NC01" | from == "NC02" | from == "NC03") |>
  calculate.centrality()

#nc.key <- calculate.keyactors(nc.cent)

fl.cent <- ncfl.frame |>
  select("from", "to") |>
  dplyr::filter(from == "FL01" | from == "FL02" | from == "FL03") |>
  calculate.centrality()

#fl.key <- calculate.keyactors(fl.cent)
#proper_centralities(full.graph)

network.efficiency(full.graph)
network.efficiency(pates.full.graph)
network.efficiency(ncfl.full.graph)
info.centrality.network(full.graph)
info.centrality.network(pates.full.graph)
info.centrality.network(ncfl.full.graph)
#library(CINNA)
harmonic_centrality(full.graph)
group_centrality(full.graph)
local_bridging_centrality(full.graph)

#df.cent <- df.cent %>% filter(!grepl("^(PA|NC01|FL)", rownames(.)))


