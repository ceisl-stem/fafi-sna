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
library(png)
library(grid)

the.palette <<- c("FL" = "#6929c4", "NC" = "#1192e8", "PA" = "#005d5d",
                  "CF" = "#9f1853", "FF" = "#fa4d56", "IL" = "#570408",
                  "OR" = "#198038", "OT" = "#002d9c", "SA" = "#ee538b",
                  "ST" = "#b28600", "UA" = "#009d9a", "UF" = "#012749",
                  "US" = "#8a3800")

the.abbrev <<- data.frame(color_code = c("FL", "NC", "FF", "IL", "OR", "OT", "SA",
                                     "ST", "UA", "UF", "US", "PA", "CF"),
                          full = c("Family Leader", "Neighborhood Caucus Member",
                                   "Friend/Family", "Institutional Leader",
                                   "Other Resource", "Therapist",
                                   "School Administrator", "Teacher",
                                   "University Advisor", "University Faculty",
                                   "University Staff", "Teacher Education Student",
                                   "Child"))

network.cent.frame <- data.frame(network = character(),
                                  efficiency = numeric(),
                                  info = numeric(),
                                  harmonic = numeric())

set.graph <- function(the.frame) {
  the.frame <- the.frame |>
    select("from", "to")
  the.weight <- the.frame %>% 
    dplyr::group_by(from, to) %>%
    summarize(weight = n()) %>%
    dplyr::ungroup()
  the.frame <- merge(the.frame, the.weight, by = c('from', 'to'))
  the.graph <- the.frame |>
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

draw.graph <- function(the.graph, the.salience, the.file) {
  the.filename <- glue("output/sna_{the.file}-plot.pdf")
  the.salience <- the.salience %>%
    select(CODE, SmithsS) %>%
    dplyr::rename("name" = "CODE")
  node.data <- data.frame(name = V(the.graph)$name) %>%
    mutate(id_no = substr(V(the.graph)$name, 3, 4)) %>%
    mutate(color_code = substr(V(the.graph)$name, 1, 2)) %>%
    dplyr::left_join(the.salience) %>%
    dplyr::left_join(the.abbrev) %>%
    mutate(label = glue("{full} {id_no}")) %>%
    replace_na(list(SmithsS = 0.01))
  V(the.graph)$color_code <- node.data$color_code
  V(the.graph)$size_code <- (node.data$SmithsS) * 100
  V(the.graph)$label <- node.data$label
  the.plot <- the.graph |>
    ggraph(layout = "fr") +
    geom_edge_fan(color = "#A7A9AB") +
    geom_node_point(aes(color = color_code,
                        size = size_code),
                        show.legend = FALSE) +
    scale_size_continuous(range = c(2.5,10)) +
    scale_color_manual(values = the.palette) +
    geom_node_text(aes(label = label), repel = TRUE) +
    labs(edge_width = "Letters") +
    theme_graph()
  #print(the.plot)
  ggsave(the.plot, filename = the.filename, width = 11.5, height = 8, units = "in", dpi = 300)
  return(the.plot)
}

calculate.centrality <- function(the.graph, the.salience, the.file) {
  the.salience <- the.salience |>
    select("actor" = "CODE", "SmithsS")
  the.filename <- glue("output/table_{the.file}.csv")
  data.degree <- degree(the.graph)
  data.betweenness <- betweenness(the.graph, directed = FALSE)
  data.closeness <- closeness(the.graph)
  data.pagerank <- page_rank(the.graph, directed = TRUE)
  data.coreness <- coreness(the.graph)
  data.lbc <- local_bridging_centrality(the.graph)
  analysis.network.data <- data.frame(degree = data.degree,
                                      betweenness = data.betweenness,
                                      closeness = data.closeness,
                                      pagerank = data.pagerank$vector,
                                      coreness = data.coreness,
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

calculate.network.centrality <- function(the.graph, the.network) {
  the.graph <- full.graph
  data.nef <- network.efficiency(the.graph)
  data.info <- info.centrality.network(the.graph)
  the.row <- c(the.network, data.nef, data.info)
  return(the.row)
}

calculate.keyactors <- function(the.frame, the.salience, the.file) {
  the.salience <- full.salience
  the.salience <- the.salience %>%
    select("CODE", "SmithsS") %>%
    rename("actor" = "CODE")
  the.filename <- glue("output/keyactors_{the.file}.pdf")
  table.filename <- glue("output/table_{the.file}_keyactors.csv")
  key.frame <- the.frame %>%
    select(actor, betweenness, pagerank)
  key.res <- lm(pagerank ~ betweenness, data = key.frame)$residuals
  key.frame <- transform(key.frame, residuals = abs(key.res))
  node.data <- data.frame(actor = key.frame$actor) %>%
    mutate(color_code = substr(key.frame$actor, 1, 2))
  key.frame <- key.frame %>%
    left_join(node.data, by = "actor") %>%
    mutate(id_no = substr(key.frame$actor, 3, 4)) %>%
    dplyr::left_join(the.abbrev) %>%
    mutate(label = glue("{full} {id_no}")) %>%
    dplyr::left_join(the.salience) %>%
    replace_na(list(SmithsS = 0))
  key.ymedian <- median(key.frame$pagerank)
  key.xmedian <- median(key.frame$betweenness)
  key.ymean <- mean(key.frame$pagerank)
  key.xmean <- mean(key.frame$betweenness)
  key.xmin <- min(key.frame$betweenness)
  key.xmax <- max(key.frame$betweenness)
  key.ymin <- min(key.frame$pagerank)
  key.ymax <- max(key.frame$pagerank)
  key.xmid <- (key.xmin + key.xmax)/2
  key.ymid <- (key.ymin + key.ymax)/2
  plot.ymin <- key.ymean - key.ymax
  plot.xmin <- key.xmean - key.xmax
  key.frame <- key.frame %>%
    mutate(keystatus = case_when((pagerank > key.ymean & betweenness > key.xmean) ~ "Sage",
                                 (pagerank > key.ymean & betweenness < key.xmean) ~ "Steward",
                                 (pagerank < key.ymean & betweenness > key.xmean) ~ "Weaver")) %>%
    dplyr::group_by(keystatus) %>%
    arrange(desc(residuals), desc(SmithsS)) %>%
    slice(1:5) %>% unique() %>% ungroup() %>% na.omit()
  key.plot <-
    ggscatter(
      key.frame,
      x = "betweenness",
      y = "pagerank",
      label = "label",
      label.rectangle = FALSE,
      repel = TRUE,
      theme = theme_minimal(),
      ylab = "Page Rank Centrality",
      xlab = "Betweenness Centrality",
      point = TRUE,
      #ylim = c(plot.ymin, key.ymax),
      #xlim = c(plot.xmin, key.xmax),
      show.legend = FALSE,
      color = "color_code",
      palette = the.palette,
      conf.int = FALSE, 
      cor.coef = FALSE,
      legend = "none"
    ) +
    #scale_x_continuous(expand = c(0, 0)) +
    #scale_y_continuous(expand = c(0, 0)) +
    geom_hline(yintercept = key.ymean, color = "#243142", alpha = 0.2) +
    geom_vline(xintercept = key.xmean, color = "#243142", alpha = 0.2) +
    geom_label(aes(x = (key.xmean - key.xmin)/2, y = (key.ymean + key.ymax)/2, label = "Stewards", hjust = 0.5),
               color = "#243142", fill = "#EEEEEE") +
    geom_label(aes(x = (key.xmean + key.xmax)/2, y = (key.ymean - key.ymin)/2,
                   label = "Weavers", hjust = 0.5), color = "#243142", fill = "#EEEEEE") +
    geom_label(aes(x = (key.xmean + key.xmax)/2, y = (key.ymean + key.ymax)/2,
                   label = "Sages", hjust = 0.5), color = "#243142", fill = "#EEEEEE")# +
  key.frame <- key.frame %>%
    dplyr::select(actor, betweenness, pagerank, residuals, SmithsS, keystatus) %>%
    arrange(keystatus, desc(residuals), desc(SmithsS))
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

full.graph <- set.graph(full.frame)
full.plot <- draw.graph(full.graph, full.salience, "full")

pates.full.graph <- set.graph(pates.frame)
pates.full.plot <- draw.graph(pates.full.graph, pates.salience, "pates")

ncfl.full.graph <- set.graph(ncfl.frame)
ncfl.full.plot <- draw.graph(ncfl.full.graph, ncfl.salience, "ncfl")

pates.q1.graph <- pates.frame |>
  filter(question == "Q1") |>
  set.graph()
pates.q1.plot <- draw.graph(pates.q1.graph, pates.salience, "q1_pates")

pates.q3.graph <- pates.frame |>
  filter(question == "Q3") |>
  set.graph()
pates.q3.plot <- draw.graph(pates.q3.graph, pates.salience, "q3_pates")

pates.q4.graph <- pates.frame |>
  filter(question == "Q4") |>
  set.graph()
pates.q4.plot <- draw.graph(pates.q4.graph, pates.salience, "q4_pates")

ncfl.q1.graph <- ncfl.frame |>
  filter(question == "Q1") |>
  set.graph()
ncfl.q1.plot <- draw.graph(ncfl.q1.graph, ncfl.salience, "q1_ncfl")

ncfl.q3.graph <- ncfl.frame |>
  filter(question == "Q3") |>
  set.graph()
ncfl.q3.plot <- draw.graph(ncfl.q3.graph, ncfl.salience, "q3_ncfl")

nc.q3.graph <- ncfl.frame |>
  filter(question == "Q3") |>
  filter(from == "NC01" | from == "NC02" | from == "NC03") |>
  set.graph()
nc.q3.plot <- draw.graph(nc.q3.graph, nc.salience, "q3_nc")

fl.q3.graph <- ncfl.frame |>
  filter(question == "Q3") |>
  filter(from == "FL01" | from == "FL02" | from == "FL03") |>
  set.graph()
fl.q3.plot <- draw.graph(fl.q3.graph, fl.salience, "q3_fl")

ncfl.q4.graph <- ncfl.frame |>
  filter(question == "Q4") |>
  set.graph()
ncfl.q4.plot <- draw.graph(ncfl.q4.graph, ncfl.salience, "q4_ncfl")

full.cent <- calculate.centrality(full.graph, full.salience, "full")
full.key <- calculate.keyactors(full.cent, full.salience, "full")

pates.cent <- calculate.centrality(pates.full.graph, pates.salience, "pates")
pates.key <- calculate.keyactors(pates.cent, pates.salience, "pates")

ncfl.cent <- calculate.centrality(ncfl.full.graph, ncfl.salience, "ncfl")
ncfl.key <- calculate.keyactors(ncfl.cent, ncfl.salience, "ncfl")
