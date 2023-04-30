library(readr)
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
library(igraphdata)
library(influenceR)
library(centiserve)
library(tidyr)
library(dplyr)

the.palette <<- c("FL" = "#6929c4", "NC" = "#1192e8", "PA" = "#005d5d",
                  "CF" = "#9f1853", "FF" = "#fa4d56", "IL" = "#570408",
                  "OR" = "#198038", "OT" = "#002d9c", "SA" = "#ee538b",
                  "ST" = "#b28600", "UA" = "#009d9a", "UF" = "#012749",
                  "US" = "#8a3800")

the.abbrev <<- data.frame(color_code = c("FL", "NC", "FF", "IL", "OR", "OT", "SA",
                                     "ST", "UA", "UF", "US", "PA", "CF"),
                          full = c("Family Leader", "NC Member",
                                   "Friend/Family", "Institutional Leader",
                                   "Other Resource", "Therapist",
                                   "Administrator", "Teacher",
                                   "Advisor", "Faculty",
                                   "Staff", "Student",
                                   "Child"))

network.cent.frame <- data.frame(network = character(),
                                 effsize = numeric(),
                                 efficiency = numeric(),
                                 constraint = numeric(),
                                 hierarchy = numeric(),
                                 info = numeric())

############################
###                      ###
###     THE FUNCTIONS    ###
###                      ###
############################

set.graph <- function(the.frame, the.salience) {
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
  anthro.frame <- the.frame |>
    select("Subj" = "from", "Order" = "order", "CODE" = "to", "GROUPING" = "question") |>
    add_count(Subj, GROUPING) |>
    dplyr::filter(n > 1)
  if(the.grouping == "none") {
    anthro.frame <- anthro.frame |>
      select("Subj", "Order", "CODE") |>
      dplyr::distinct() |>
      as.data.frame()
    anthro.frame$Order <- as.numeric(anthro.frame$Order)
    the.salience <- CalculateSalience(anthro.frame)
  } else {
  anthro.frame <- anthro.frame |>
    select("Subj", "Order", "CODE", "GROUPING") |>
    dplyr::distinct() |>
    as.data.frame()
  anthro.frame$Order <- as.numeric(anthro.frame$Order)
  the.salience <- CalculateSalience(anthro.frame, GROUPING = "GROUPING")
  }
  code.salience <- SalienceByCode(the.salience, dealWithDoubles = "MAX")
  #write_csv(code.salience, the.filename, append = FALSE)
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
  set.seed(123)
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
  ggsave(the.plot, filename = the.filename, width = 11.5, height = 8, units = "in", dpi = 300)
  return(the.plot)
}

calculate.centrality <- function(the.graph, the.salience, the.file) {
  the.salience <- the.salience |>
    select("actor" = "CODE", "SmithsS")
  #the.filename <- glue("output/table_{the.file}.csv")
  data.in <- igraph::degree(the.graph, mode = "in")
  data.out <- igraph::degree(the.graph, mode = "out")
  data.betweenness <- igraph::betweenness(the.graph, directed = FALSE)
  data.leverage <- leverage(the.graph)
  data.leaderrank <- leaderrank(the.graph)
  data.constraint <- constraint(the.graph)
  analysis.network.data <- data.frame(indegree = data.in,
                                      outdegree = data.out,
                                      betweenness = data.betweenness,
                                      leaderrank = data.leaderrank,
                                      leverage = data.leverage,
                                      constraint = data.constraint)
  analysis.network.data$actor <- rownames(analysis.network.data)
  rownames(analysis.network.data) <- NULL
  analysis.network.data <- analysis.network.data |>
    select(actor, everything()) |>
    left_join(the.salience) |>
    replace_na(list(SmithsS = 0))
  rownames(analysis.network.data) <- analysis.network.data$actor
  return(analysis.network.data)
}

calculate.network.centrality <- function(the.graph, the.network) {
  data.effsize <- sapply(V(the.graph), function(v) {
    subg <- delete_vertices(the.graph, v)
    clusters <- clusters(subg)
    max(clusters$csize)}) |>
    max(data.effsize)
  data.nef <- network.efficiency(the.graph)
  data.constraint <- constraint(the.graph) |> mean()
  data.hierarchy <- hierarchy(the.graph)
  data.info <- info.centrality.network(the.graph)
  the.row <- c(the.network, data.effsize, data.nef, data.constraint,
               data.hierarchy, data.info)
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
    select(actor, leverage, leaderrank)
  key.res <- lm(leaderrank ~ leverage, data = key.frame)$residuals
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
  key.ymedian <- median(key.frame$leaderrank)
  key.xmedian <- median(key.frame$leverage)
  key.ymean <- mean(key.frame$leaderrank)
  key.xmean <- mean(key.frame$leverage)
  key.frame <- key.frame %>%
    mutate(keystatus = case_when((leaderrank > key.ymean & leverage > key.xmean) ~ "Sage",
                                 (leaderrank > key.ymean & leverage < key.xmean) ~ "Steward",
                                 (leaderrank < key.ymean & leverage > key.xmean) ~ "Weaver")) %>%
    na.omit() %>%
    dplyr::group_by(keystatus) %>%
    arrange(desc(residuals), desc(SmithsS)) %>%
    slice(1:5) %>% unique() %>% ungroup() %>% na.omit()
  key.xmin <- min(key.frame$leverage)
  key.xmax <- max(key.frame$leverage)
  key.ymin <- min(key.frame$leaderrank)
  key.ymax <- max(key.frame$leaderrank)
  key.xmid <- (key.xmin + key.xmax)/2
  key.ymid <- (key.ymin + key.ymax)/2
  plot.ymin <- key.ymean - key.ymax
  plot.xmin <- key.xmean - key.xmax
  steward.count <- key.frame %>%
    count(keystatus) %>%
    filter(keystatus == "Steward") %>%
    pull(n)
  steward.count <- ifelse(is.numeric(steward.count), steward.count, 0)
  steward.count <- steward.count %>% replace_na(0)
  weaver.count <- key.frame %>%
    count(keystatus) %>%
    filter(keystatus == "Weaver") %>%
    pull(n)
  weaver.count <- ifelse(is.numeric(weaver.count), weaver.count, 0)
  weaver.count <- weaver.count %>% replace_na(0)
  key.plot <- ggscatter(key.frame, x = "leverage", y = "leaderrank",
      label = "label", label.rectangle = FALSE, repel = TRUE,
      theme = theme_minimal(), ylab = "Page Rank Centrality", xlab = "Leverage Centrality",
      point = TRUE, show.legend = FALSE, color = "color_code", palette = the.palette,
      conf.int = FALSE, cor.coef = FALSE, legend = "none")
  if(steward.count != 0) {
    key.plot <- key.plot +
      geom_vline(xintercept = key.xmean, color = "#243142", alpha = 0.2) +
      geom_label(aes(x = key.xmin, y = key.ymax, label = "Stewards", hjust = 0),
                 color = "#243142", fill = "#EEEEEE")
  }
  if(weaver.count != 0) {
    key.plot <- key.plot +
      geom_hline(yintercept = key.ymean, color = "#243142", alpha = 0.2) +
      geom_label(aes(x = key.xmax, y = key.ymin,
                     label = "Weavers", hjust = 1), color = "#243142", fill = "#EEEEEE")
  }
  key.plot <- key.plot +
    geom_label(aes(x = key.xmax, y = key.ymax,
                   label = "Sages", hjust = 1), color = "#243142", fill = "#EEEEEE")# +
  key.frame <- key.frame %>%
    dplyr::select(actor, leverage, leaderrank, residuals, SmithsS, keystatus) %>%
    arrange(keystatus, desc(residuals), desc(SmithsS))
  ggsave(key.plot, filename = the.filename, width = 11.5, height = 8, units = "in", dpi = 300)
  #write_csv(key.frame, table.filename, append = FALSE)
  return(key.frame)
}

create.q.cent <- function(the.1, the.2, the.question) {
  the.q.cent <- bind_rows(the.1, the.2) |>
    mutate(question = the.question) |>
    select(question, actor, betweenness, outdegree, indegree, constraint, leverage,
           leaderrank, SmithsS)
}

calculate.ranks <- function(the.cent) {
  the.cent <- the.cent |>
    mutate(outdegree_rank = dense_rank(desc(outdegree))) |>
    mutate(betweenness_rank = dense_rank(desc(betweenness))) |>
    mutate(indegree_rank = dense_rank(desc(indegree))) |>
    mutate(constraint_rank = dense_rank(desc(constraint))) |>
    mutate(leverage_rank = dense_rank(desc(leverage))) |>
    mutate(leaderrank_rank = dense_rank(desc(leaderrank))) |>
    mutate(smiths_rank = dense_rank(desc(SmithsS))) |>
    select(question, actor, outdegree, outdegree_rank, indegree, indegree_rank,
           betweenness, betweenness_rank, constraint, constraint_rank,
           leverage, leverage_rank, leaderrank, leaderrank_rank,
           SmithsS, smiths_rank) |>
    dplyr::arrange(question, actor)
  rownames(the.cent) <- NULL
  return(the.cent)
}

calculate.jaccard <- function(the.set1, the.set2) {
  the.jaccard <- length(intersect(the.set1$to, the.set2$to)) / length(union(the.set1$to, the.set2$to))
  return(the.jaccard)
}

calculate.node.flexibility <- function(the.actor) {
  set1 <- flex.frame |>
    dplyr::filter(actor == the.actor & question == "Q1") |>
    dplyr::select(actor, to) |>
    as.data.frame()
  set1$actor <- as.factor(set1$actor)
  set1$to <- as.factor(set1$to)
  set2 <- flex.frame |>
    dplyr::filter(actor == the.actor & question == "Q3") |>
    dplyr::select(actor, to) |>
    as.data.frame()
  set2$actor <- as.factor(set2$actor)
  set2$to <- as.factor(set2$to)
  set3 <- flex.frame |>
    dplyr::filter(actor == the.actor & question == "Q4") |>
    dplyr::select(actor, to) |>
    as.data.frame()
  set3$actor <- as.factor(set3$actor)
  set3$to <- as.factor(set3$to)
  jaccard.1 <- calculate.jaccard(set1, set2)
  jaccard.2 <- calculate.jaccard(set2, set3)
  jaccard.3 <- calculate.jaccard(set1, set3)
  the.flexibility <- 1 - ((jaccard.1 + jaccard.2 + jaccard.3) / 3)
  response.frame <- data.frame(actor = the.actor, flexibility = the.flexibility)
  return(response.frame)
}

create.q.key <- function(the.1, the.2, the.question){
  the.q.key <- bind_rows(the.1, the.2) |>
    mutate(question = the.question) |>
    dplyr::select(question, actor, keystatus)
  return(the.q.key)
}

############################
###  THE WORK            ###
############################

pates.frame <- import("https://osf.io/download/62qpa/", format = "csv") |>
  mutate_all(toupper) |>
  filter(id != "PA09") |>
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
full.graph <- set.graph(full.frame, full.salience)
full.plot <- draw.graph(full.graph, full.salience, "full")
#full.cent <- calculate.centrality(full.graph, full.salience, "full")
#full.key <- calculate.keyactors(full.cent, full.salience, "full")

pates.q1.frame <- pates.frame |>
  filter(question == "Q1")
pates.q1.salience <- calculate.salience(pates.q1.frame, "GROUPING", "Q1")
pates.q1.graph <- set.graph(pates.q1.frame, pates.q1.salience)
pates.q1.plot <- draw.graph(pates.q1.graph, pates.q1.salience, "q1_pates")
pates.q1.cent <- calculate.centrality(pates.q1.graph, pates.q1.salience, "q1_pates")
pates.q1.key <- calculate.keyactors(pates.q1.cent, pates.q1.salience, "q1_pates")

ncfl.q1.frame <- ncfl.frame |>
  filter(question == "Q1")
ncfl.q1.salience <- calculate.salience(ncfl.q1.frame, "GROUPING", "Q1")
ncfl.q1.graph <- set.graph(ncfl.q1.frame, ncfl.q1.salience)
ncfl.q1.plot <- draw.graph(ncfl.q1.graph, ncfl.q1.salience, "q1_ncfl")
ncfl.q1.cent <- calculate.centrality(ncfl.q1.graph, ncfl.q1.salience, "q1_ncfl")
ncfl.q1.key <- calculate.keyactors(ncfl.q1.cent, ncfl.q1.salience, "q1_ncfl")

pates.q3.frame <- pates.frame |>
  filter(question == "Q3")
pates.q3.salience <- calculate.salience(pates.q3.frame, "GROUPING", "Q3")
pates.q3.graph <- set.graph(pates.q3.frame, pates.q3.salience)
pates.q3.plot <- draw.graph(pates.q3.graph, pates.q3.salience, "q3_pates")
pates.q3.cent <- calculate.centrality(pates.q3.graph, pates.q3.salience, "q3_pates")
pates.q3.key <- calculate.keyactors(pates.q3.cent, pates.q3.salience, "q3_pates")

ncfl.q3.frame <- ncfl.frame |>
  filter(question == "Q3")
ncfl.q3.salience <- calculate.salience(ncfl.q3.frame, "GROUPING", "Q3")
ncfl.q3.graph <- set.graph(ncfl.q3.frame, ncfl.q3.salience)
ncfl.q3.plot <- draw.graph(ncfl.q3.graph, ncfl.q3.salience, "q3_ncfl")
ncfl.q3.cent <- calculate.centrality(ncfl.q3.graph, ncfl.q3.salience, "q3_ncfl")
ncfl.q3.key <- calculate.keyactors(ncfl.q3.cent, ncfl.q3.salience, "q3_ncfl")

pates.q4.frame <- pates.frame |>
  filter(question == "Q4")
pates.q4.salience <- calculate.salience(pates.q4.frame, "GROUPING", "Q4")
pates.q4.graph <- set.graph(pates.q4.frame, pates.q4.salience)
pates.q4.plot <- draw.graph(pates.q4.graph, pates.q4.salience, "q4_pates")
pates.q4.cent <- calculate.centrality(pates.q4.graph, pates.q4.salience, "q4_pates")
pates.q4.key <- calculate.keyactors(pates.q4.cent, pates.q4.salience, "q4_pates")

ncfl.q4.frame <- ncfl.frame |>
  filter(question == "Q4")
ncfl.q4.salience <- calculate.salience(ncfl.q4.frame, "GROUPING", "Q4")
ncfl.q4.graph <- set.graph(ncfl.q4.frame, ncfl.q4.salience)
ncfl.q4.plot <- draw.graph(ncfl.q4.graph, ncfl.q4.salience, "q4_ncfl")
ncfl.q4.cent <- calculate.centrality(ncfl.q4.graph, ncfl.q4.salience, "q4_ncfl")
ncfl.q4.key <- calculate.keyactors(ncfl.q4.cent, ncfl.q4.salience, "q4_ncfl")

flex.frame <<- full.frame |>
  dplyr::select(question, actor = from, to)

flex.results <- lapply(unique(flex.frame$actor), calculate.node.flexibility)
flex.score <- do.call(rbind, flex.results)

q1.key <- create.q.key(pates.q1.key, ncfl.q1.key, "q1")
q3.key <- create.q.key(pates.q3.key, ncfl.q3.key, "q3")
q4.key <- create.q.key(pates.q4.key, ncfl.q4.key, "q4")
q.key <<- bind_rows(q1.key, q3.key, q4.key) |>
  select(actor, keystatus) |>
  mutate(keyscore = case_when(keystatus == "Sage" ~ 3,
                              keystatus == "Steward" ~ 2,
                              keystatus == "Weaver" ~ 1)) |>
  dplyr::group_by(actor) |>
  summarize(keyscore = sum(keyscore)) |>
  mutate(keyscore = keyscore / 9)

q1.cent <- create.q.cent(pates.q1.cent, ncfl.q1.cent, "q1") |>
  calculate.ranks()
q3.cent <- create.q.cent(pates.q3.cent, ncfl.q3.cent, "q3") |>
  calculate.ranks()
q4.cent <- create.q.cent(pates.q4.cent, ncfl.q4.cent, "q4") |>
  calculate.ranks()
q.cent <<- bind_rows(q1.cent, q3.cent, q4.cent)
q.btwn.avg <- q.cent |>
  dplyr::group_by(actor) |>
  summarize(avg_betweenness_rank = mean(betweenness_rank))
q.odeg.avg <- q.cent |>
  dplyr::group_by(actor) |>
  summarize(avg_outdegree_rank = mean(outdegree_rank))
q.ideg.avg <- q.cent |>
  dplyr::group_by(actor) |>
  summarize(avg_indegree_rank = mean(indegree_rank))
q.const.avg <- q.cent |>
  dplyr::group_by(actor) |>
  summarize(avg_constraint_rank = mean(constraint_rank))
q.lev.avg <- q.cent |>
  dplyr::group_by(actor) |>
  summarize(avg_leverage_rank = mean(leverage_rank))
q.lead.avg <- q.cent |>
  dplyr::group_by(actor) |>
  summarize(avg_leaderrank_rank = mean(leaderrank_rank))
q.smith.avg <- q.cent |>
  dplyr::group_by(actor) |>
  summarize(avg_smiths_rank = mean(smiths_rank))
q.cent.avg <- q.btwn.avg |>
  dplyr::left_join(q.odeg.avg) |>
  dplyr::left_join(q.ideg.avg) |>
  dplyr::left_join(q.const.avg) |>
  dplyr::left_join(q.lev.avg) |>
  dplyr::left_join(q.lead.avg) |>
  dplyr::left_join(q.smith.avg)
  
full.avg.cent <- q.cent.avg %>%
  left_join(flex.score) %>%
  left_join(q.key) %>%
  replace_na(list(flexibility = 0, keyscore = 0)) %>%
  dplyr::mutate(flexibility_rank = dense_rank(desc(flexibility))) %>%
  dplyr::mutate(keyscore_rank = dense_rank(desc(keyscore)))

# Looking at Participant Actors:
# Disposition: Flexibility Score, Key Actor Score
# Agency: Constraint, Leverage
# Access: Betweenness, Out Degree

participants.frame <- full.avg.cent |>
  dplyr::filter(substr(actor, 1, 2) == "PA" | substr(actor, 1, 2) == "FL" | substr(actor, 1, 2) == "NC") |>
  dplyr::select(actor, flexibility, flexibility_rank, keyscore, keyscore_rank, avg_constraint_rank,
                avg_leverage_rank, avg_betweenness_rank, avg_outdegree_rank) |>
  dplyr::mutate(disposition_score = ((1 / (flexibility_rank + keyscore_rank) / 2)) * 10) |>
  dplyr::mutate(agency_score = ((1 / (avg_constraint_rank + avg_leverage_rank) / 2)) * 10) |>
  dplyr::mutate(access_score = ((1 / (avg_betweenness_rank + avg_outdegree_rank) / 2)) * 10) |>
  dplyr::mutate(overall_score = (disposition_score + agency_score + access_score) / 3) |>
  dplyr::arrange(desc(overall_score))

write_csv(participants.frame, file = "output/participants_analysis.csv")

# Looking at Key Actors:
# Disposition: Leader Rank, Key Actor Score
# Reputation: Leverage, Smith's S
# Access: Betweenness, In Degree

keyactors.q.frame <- q.key |>
  dplyr::left_join(q1.key) |>
  dplyr::left_join(q3.key, by = "actor") |>
  dplyr::left_join(q4.key, by = "actor") |>
  dplyr::select(actor, q1_status = keystatus.x, q3_status = keystatus.y,
                q4_status = keystatus) |>
  dplyr::arrange(actor)

keyactors.frame <- full.avg.cent |>
  dplyr::filter(keyscore > 0) |>
  dplyr::mutate(disposition_score = ((1 / (avg_leaderrank_rank + keyscore_rank) / 2)) * 10) |>
  dplyr::mutate(reputation_score = ((1 / (avg_smiths_rank + avg_leverage_rank) / 2)) * 10) |>
  dplyr::mutate(access_score = ((1 / (avg_betweenness_rank + avg_indegree_rank) / 2)) * 10) |>
  dplyr::mutate(overall_score = (disposition_score + reputation_score + access_score) / 3) |>
  dplyr::select(actor, keyscore, keyscore_rank, disposition_score, reputation_score,
                access_score, overall_score) |>
  dplyr::left_join(keyactors.q.frame) |>
  dplyr::arrange(desc(overall_score))

write_csv(keyactors.frame, file = "output/keyactors_analysis.csv")
