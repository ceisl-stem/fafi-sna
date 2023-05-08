## ######################################################
## ##                                                  ##
## ##     THE ANALYSIS IS NOT THE SOCIAL NETWORK       ##
## ##  Illuminating the unruly nature of intentional   ##
## ##  social networks in educational justice efforts  ##
## ##                                                  ##
## ######################################################
## ##                                                  ##
## ##   Funded in part by a National Association for   ##
## ##     Family, School, and Community Engagement     ##
## ##                    Mini-Grant.                   ##
## ##                                                  ##
## ######################################################

## ######################################################
## ##                                                  ##
## ##                 LOAD LIBRARIES                   ##
## ##                                                  ##
## ######################################################

library(igraph)
library(tidygraph)
library(ggraph)
library(centiserve)
library(CINNA)

library(AnthroTools)

library(readr)
library(rio)
library(glue)
library(tidyr)
library(dplyr)
library(ggthemes)
library(ggpubr)
library(ggcorrplot)
library(vistime)

## ######################################################
## ##                                                  ##
## ##               DEFINE CONSTANTS                   ##
## ##                                                  ##
## ######################################################

## #####################################
## ## Define the common color palette ##
## #####################################

the.palette <<- c(
  "FL" = "#6929c4", "NC" = "#1192e8", "PA" = "#005d5d",
  "CF" = "#9f1853", "FF" = "#fa4d56", "IL" = "#570408",
  "OR" = "#198038", "OT" = "#002d9c", "SA" = "#ee538b",
  "ST" = "#b28600", "UA" = "#009d9a", "UF" = "#012749",
  "US" = "#8a3800"
)

## ##############################################
## ## Define the participant abbreviation list ##
## ##############################################

the.abbrev <<- data.frame(
  color_code = c(
    "FL", "NC", "FF", "IL", "OR", "OT", "SA",
    "ST", "UA", "UF", "US", "PA", "CF"
  ),
  full = c(
    "Family Leader", "NC Member", "Friend/Family",
    "Institutional Leader", "Other Resource", "Therapist",
    "Administrator", "Teacher", "Advisor", "Faculty",
    "Staff", "Student", "Child"
  )
)

## ######################################################
## ##                                                  ##
## ##               DEFINE FUNCTIONS                   ##
## ##                                                  ##
## ######################################################

## ###############################
## ## Save plots as PDF and PNG ##
## ###############################

plot.save <- function(the.plot, the.file) {
  # Set the filename for the PDF.
  pdf.name <- glue("output/plots/{the.file}.pdf")
  # Set the filname for the PNG.
  png.name <- glue("output/plots/{the.file}.png")
  # Save as PDF.
  ggsave(the.plot, filename = pdf.name, width = 11.5, height = 8, units = "in", dpi = 300)
  # Save as PNG.
  ggsave(the.plot, filename = png.name, width = 11.5, height = 8, units = "in", dpi = 300)
}

## #############################
## ## Create Correlation Plot ##
## #############################

plot.corr <- function(the.frame, the.file) {
  # Calculate the correlation of the provided dataframe.
  corr <- round(cor(the.frame), 1)
  # Calculate a matrix of significance.
  p.mat <- cor_pmat(corr)
  # Initialize plot.
  corr.plot <- ggcorrplot(corr,
    hc.order = TRUE, # Order according to hierarchical clustering.
    type = "lower", # Only display the bottom half.
    p.mat = p.mat, # Account for statistical significance.
    colors = c("#750e13", "#ffffff", "#003a6d")
  ) # Set colors.
  # Save the plot...
  plot.save(corr.plot, the.file)
  # ...and return it.
  return(corr.plot)
}

## ##############################
## ## Calculate Tukey's Fences ##
## ##############################

calculate.tukey <- function(the.cent) {
  # Calculate Tukey's fences
  q <- quantile(the.cent, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  the.fence <- data.frame(
    lower = q[1] - 1.5 * iqr,
    upper = q[2] + 1.5 * iqr
  )
  return(the.fence)
}

## ######################################################
## ## Prepare the data and initialize the graph object ##
## ######################################################

set.graph <- function(the.frame, the.salience) {
  # Reduce the edge list to just i (from) and j (to).
  the.frame <- the.frame |>
    select("from", "to")
  # Calculate the number of times i names j.
  the.weight <- the.frame |>
    group_by(from, to) |>
    summarize(weight = n()) |>
    ungroup()
  # Combine the dataframes into one, matching the weight to the edge.
  the.frame <- merge(the.frame, the.weight, by = c("from", "to"))
  # Create the igraph object, and set it to be a directed graph (i.e., i -> j).
  the.graph <- the.frame |>
    graph_from_data_frame(directed = TRUE)
  the.salience <- the.salience |> rename("name" = "actor")
  node.data <- data.frame(name = V(the.graph)$name) |>
    mutate(id_no = substr(V(the.graph)$name, 3, 4)) |>
    mutate(color_code = substr(V(the.graph)$name, 1, 2)) |>
    left_join(the.salience) |>
    left_join(the.abbrev) |>
    mutate(label = glue("{full} {id_no}")) |>
    replace_na(list(SmithsS = 0.01))
  V(the.graph)$color_code <- node.data$color_code
  V(the.graph)$size_code <- (node.data$SmithsS) * 100
  V(the.graph)$label <- node.data$label
  # Send the graph object back for further processing.
  return(the.graph)
}

## ###########################
## ## Plot the graph object ##
## ###########################

draw.graph <- function(the.graph, the.file) {
  # Set the filename for saving the graph.
  the.file <- glue("sna_{the.file}-plot")
  # Set a reproducible seed for randomization, used to ensure that the plot looks more or less
  # the same each time it is created.
  set.seed(123)
  # Create the plot.
  the.plot <- the.graph |>
    ggraph(layout = "fr") + # Display the graph using the Fruchterman and Reingold algorithm.
    geom_edge_fan(color = "#A7A9AB") + # Plot the edges between nodes.
    geom_node_point(
      aes(
        color = color_code, # Plot the nodes with the color determined by the
        size = size_code
      ), # participant and the size of the node determined
      show.legend = FALSE
    ) + # by the Smith's S Salience Score.
    scale_size_continuous(range = c(2.5, 10)) + # Rescale the node size.
    scale_color_manual(values = the.palette) + # Bring in the color palette.
    geom_node_text(aes(label = label), repel = TRUE) + # Place the actor name on the graph.
    labs(edge_width = "Letters") +
    theme_graph() # Set the theme to social network graph which removes all extraneous grids.
  # Save the plot.
  plot.save(the.plot, the.file)
  # Return the plot for further use as necessary.
  return(the.plot)
}

calculate.centrality <- function(the.graph, the.salience, the.file) {
  analysis.network.data <- data.frame(
    indegree = igraph::degree(the.graph, mode = "in"),
    outdegree = igraph::degree(the.graph, mode = "out"),
    leaderrank = leaderrank(the.graph),
    laplace = laplacian(the.graph),
    leverage = leverage(the.graph),
    latora = closeness.latora(the.graph)
  )
  analysis.network.data$actor <- rownames(analysis.network.data)
  rownames(analysis.network.data) <- NULL
  analysis.network.data <- analysis.network.data |>
    select(actor, everything()) |>
    left_join(the.salience) |>
    replace_na(list(SmithsS = 0))
  rownames(analysis.network.data) <- analysis.network.data$actor
  return(analysis.network.data)
}

calculate.salience <- function(the.frame, the.grouping, the.file) {
  the.filename <- glue("output/csv/salience_{the.file}.csv")
  anthro.frame <- the.frame |>
    select("Subj" = "from", "Order" = "order", "CODE" = "to", "GROUPING" = "question") |>
    add_count(Subj, GROUPING) |>
    filter(n > 1)
  if (the.grouping == "none") {
    anthro.frame <- anthro.frame |>
      select("Subj", "Order", "CODE") |>
      distinct() |>
      as.data.frame()
    anthro.frame$Order <- as.numeric(anthro.frame$Order)
    the.salience <- CalculateSalience(anthro.frame)
  } else {
    anthro.frame <- anthro.frame |>
      select("Subj", "Order", "CODE", "GROUPING") |>
      distinct() |>
      as.data.frame()
    anthro.frame$Order <- as.numeric(anthro.frame$Order)
    the.salience <- CalculateSalience(anthro.frame, GROUPING = "GROUPING")
  }
  code.salience <- SalienceByCode(the.salience, dealWithDoubles = "MAX")
  write_csv(code.salience, the.filename, append = FALSE)
  code.salience <- code.salience |>
    select("actor" = "CODE", "SmithsS")
  return(code.salience)
}

calculate.keyactors <- function(the.frame, the.file) {
  max_leverage <- max(the.frame$leverage, na.rm = TRUE)
  min_leverage <- min(the.frame$leverage, na.rm = TRUE)
  key.frame <- the.frame %>%
    select(actor, leverage, leaderrank, SmithsS)
  key.res <- lm(leaderrank ~ leverage, data = key.frame)$residuals |>
    as.data.frame() |>
    rename(res = 1) |>
    mutate(res = abs(res))
  key.res$actor <- row.names(key.res)
  row.names(key.res) <- NULL
  key.frame <- key.frame |>
    left_join(key.res)
  leaderrank.fence <- calculate.tukey(key.frame$leaderrank)
  key.frame.leaderrank.trimmed <- key.frame |>
    filter(leaderrank >= leaderrank.fence$lower & leaderrank <= leaderrank.fence$upper)
  leverage.fence <- calculate.tukey(key.frame$leverage)
  key.frame.leverage.trimmed <- key.frame |>
    filter(leverage >= leverage.fence$lower & leverage.fence$upper)
  key.ymean <<- mean(key.frame.leaderrank.trimmed$leaderrank)
  key.xmean <<- mean(key.frame.leverage.trimmed$leverage)
  key.frame <- key.frame |>
    mutate(keystatus = case_when(
      (leaderrank > key.ymean & leverage > key.xmean) ~ "Sage",
      (leaderrank > key.ymean & leverage < key.xmean) ~ "Steward",
      (leaderrank < key.ymean & leverage > key.xmean) ~ "Weaver"
    )) |>
    na.omit() |>
    group_by(keystatus) |>
    arrange(desc(res), desc(SmithsS)) |>
    unique() |>
    ungroup()
  key.frame <- key.frame |>
    select(actor, leverage, leaderrank, res, SmithsS, keystatus) |>
    arrange(keystatus, desc(res), desc(SmithsS))
  return(key.frame)
}

plot.keyactors <- function(key.frame, the.file) {
  the.filename <- glue("keyactors_{the.file}-plot")
  key.xmin <- min(key.frame$leverage)
  key.xmax <- max(key.frame$leverage)
  key.ymin <- min(key.frame$leaderrank)
  key.ymax <- max(key.frame$leaderrank)
  steward.count <- count.keyactors(key.frame, "Steward")
  sage.count <- count.keyactors(key.frame, "Sage")
  weaver.count <- count.keyactors(key.frame, "Weaver")
  key.frame <- key.frame  |>
    mutate(color_code = substr(key.frame$actor, 1, 2)) |>
    mutate(id_no = substr(key.frame$actor, 3, 4)) |>
    left_join(the.abbrev) |>
    mutate(label = glue("{full} {id_no}")) |>
    select(-full, -id_no)
  key.plot <- ggscatter(key.frame,
    x = "leverage", y = "leaderrank",
    label = "label", label.rectangle = FALSE, repel = TRUE,
    theme = theme_minimal(), ylab = "Leader Rank Centrality",
    xlab = "Leverage Centrality", point = TRUE, show.legend = FALSE,
    color = "color_code", palette = the.palette,
    conf.int = FALSE, cor.coef = FALSE, legend = "none"
  )
  if (steward.count != 0) {
    key.plot <- key.plot +
      geom_vline(xintercept = key.xmean, color = "#243142", alpha = 0.2) +
      geom_label(aes(x = key.xmin, y = key.ymax, label = "Stewards", hjust = 0),
        color = "#243142", fill = "#A7A9AB"
      )
  }
  if (weaver.count != 0) {
    key.plot <- key.plot +
      geom_hline(yintercept = key.ymean, color = "#243142", alpha = 0.2) +
      geom_label(aes(
        x = key.xmax, y = key.ymin,
        label = "Weavers", hjust = 1
      ), color = "#243142", fill = "#A7A9AB")
  }
  key.plot <- key.plot +
    geom_label(aes(
      x = key.xmax, y = key.ymax,
      label = "Sages", hjust = 1
    ), color = "#243142", fill = "#A7A9AB") +
    theme_few() +
    theme(legend.position = "none")
  plot.save(key.plot, the.filename)
  return(key.plot)
}

count.keyactors <- function(key.frame, the.actor) {
  the.count <- key.frame |>
    count(keystatus) |>
    filter(keystatus == the.actor) |>
    pull(n)
  the.count <- ifelse(is.numeric(the.count), the.count, 0)
  the.count <- the.count |> replace_na(0)
  return(the.count)
}

create.q.key <- function(the.1, the.2, the.question) {
  the.q.key <- bind_rows(the.1, the.2) |>
    mutate(question = the.question) |>
    select(question, actor, keystatus)
  return(the.q.key)
}

create.q.cent <- function(the.1, the.2, the.question) {
  the.q.cent <- bind_rows(the.1, the.2) |>
    mutate(question = the.question) |>
    select(
      question, actor, outdegree, indegree, leverage,
      laplace, leaderrank, latora, SmithsS
    )
}

calculate.ranks <- function(the.cent) {
  the.cent <- the.cent |>
    mutate(outdegree_rank = dense_rank(desc(outdegree))) |>
    mutate(indegree_rank = dense_rank(desc(indegree))) |>
    mutate(leverage_rank = dense_rank(desc(leverage))) |>
    mutate(laplacian_rank = dense_rank(desc(laplace))) |>
    mutate(leaderrank_rank = dense_rank(desc(leaderrank))) |>
    mutate(smiths_rank = dense_rank(desc(SmithsS))) |>
    mutate(latora_rank = dense_rank(desc(latora))) |>
    select(
      question, actor, outdegree, outdegree_rank, indegree, indegree_rank,
      leverage, leverage_rank, laplace, laplacian_rank,
      latora, latora_rank, leaderrank, leaderrank_rank, SmithsS, smiths_rank
    ) |>
    arrange(question, actor)
  rownames(the.cent) <- NULL
  return(the.cent)
}

calculate.jaccard <- function(the.set1, the.set2) {
  the.jaccard <- (length(intersect(the.set1$to, the.set2$to)) / length(union(the.set1$to, the.set2$to)))
  return(the.jaccard)
}

calculate.node.flexibility <- function(the.actor) {
  set1 <- flex.frame |>
    filter(actor == the.actor & question == "Q1") |>
    select(actor, to) |>
    as.data.frame()
  set1$actor <- as.factor(set1$actor)
  set1$to <- as.factor(set1$to)
  set2 <- flex.frame |>
    filter(actor == the.actor & question == "Q3") |>
    select(actor, to) |>
    as.data.frame()
  set2$actor <- as.factor(set2$actor)
  set2$to <- as.factor(set2$to)
  set3 <- flex.frame |>
    filter(actor == the.actor & question == "Q4") |>
    select(actor, to) |>
    as.data.frame()
  set3$actor <- as.factor(set3$actor)
  set3$to <- as.factor(set3$to)
  jaccard.1 <- calculate.jaccard(set1, set2)
  jaccard.2 <- calculate.jaccard(set2, set3)
  jaccard.3 <- calculate.jaccard(set1, set3)
  #the.flexibility <- 1 - ((1 / (3 * (3 - 1)) * (jaccard.1 + jaccard.2 + jaccard.3)) / 3)
  the.flexibility <- 1 - ((jaccard.1 + jaccard.2 + jaccard.3) / 3)
  response.frame <- data.frame(actor = the.actor, flexibility = the.flexibility)
  return(response.frame)
}

pates.frame <- import("https://osf.io/download/62qpa/", format = "csv") |>
  mutate_all(toupper) |>
  filter(id != "PA09") |>
  pivot_longer(
    cols = starts_with("Q"),
    names_to = "question",
    values_to = "to"
  ) |>
  drop_na() |>
  select("question", "from" = "id", "to") |>
  separate(col = question, into = c("question", "order"), sep = "_") |>
  filter(to != "")

ncfl.frame <- import("https://osf.io/download/ghz3c/", format = "csv") |>
  mutate_all(toupper) |>
  pivot_longer(
    cols = starts_with("Q"),
    names_to = "question",
    values_to = "to"
  ) |>
  drop_na() |>
  select("question", "from" = "ID", "to") |>
  separate(col = question, into = c("question", "order"), sep = "_") |>
  filter(to != "")

full.frame <- rbind(pates.frame, ncfl.frame)

full.salience <- calculate.salience(full.frame, "GROUPING", "full")
full.graph <- set.graph(full.frame, full.salience)
full.plot <- draw.graph(full.graph, "full")



pates.q1.frame <- pates.frame |>
  filter(question == "Q1")
pates.q1.salience <- calculate.salience(pates.q1.frame, "GROUPING", "Q1")
pates.q1.graph <- set.graph(pates.q1.frame, pates.q1.salience)
pates.q1.plot <- draw.graph(pates.q1.graph, "q1_pates")
pates.q1.cent <- calculate.centrality(pates.q1.graph, pates.q1.salience, "q1_pates")
pates.q1.key <- calculate.keyactors(pates.q1.cent, "q1_pates")
pates.q1.key.plot <- plot.keyactors(pates.q1.key, "q1_pates")

ncfl.q1.frame <- ncfl.frame |>
  filter(question == "Q1")
ncfl.q1.salience <- calculate.salience(ncfl.q1.frame, "GROUPING", "Q1")
ncfl.q1.graph <- set.graph(ncfl.q1.frame, ncfl.q1.salience)
ncfl.q1.plot <- draw.graph(ncfl.q1.graph, "q1_ncfl")
ncfl.q1.cent <- calculate.centrality(ncfl.q1.graph, ncfl.q1.salience, "q1_ncfl")
ncfl.q1.key <- calculate.keyactors(ncfl.q1.cent, "q1_ncfl")
ncfl.q1.key.plot <- plot.keyactors(ncfl.q1.key, "q1_ncfl")

pates.q3.frame <- pates.frame |>
  filter(question == "Q3")
pates.q3.salience <- calculate.salience(pates.q3.frame, "GROUPING", "Q3")
pates.q3.graph <- set.graph(pates.q3.frame, pates.q3.salience)
pates.q3.plot <- draw.graph(pates.q3.graph, "q3_pates")
pates.q3.cent <- calculate.centrality(pates.q3.graph, pates.q3.salience, "q3_pates")
pates.q3.key <- calculate.keyactors(pates.q3.cent, "q3_pates")
pates.q3.key.plot <- plot.keyactors(pates.q3.key, "q3_pates")

ncfl.q3.frame <- ncfl.frame |>
  filter(question == "Q3")
ncfl.q3.salience <- calculate.salience(ncfl.q3.frame, "GROUPING", "Q3")
ncfl.q3.graph <- set.graph(ncfl.q3.frame, ncfl.q3.salience)
ncfl.q3.plot <- draw.graph(ncfl.q3.graph, "q3_ncfl")
ncfl.q3.cent <- calculate.centrality(ncfl.q3.graph, ncfl.q3.salience, "q3_ncfl")
ncfl.q3.key <- calculate.keyactors(ncfl.q3.cent, "q3_ncfl")
ncfl.q3.key.plot <- plot.keyactors(ncfl.q3.key, "q3_ncfl")

pates.q4.frame <- pates.frame |>
  filter(question == "Q4")
pates.q4.salience <- calculate.salience(pates.q4.frame, "GROUPING", "Q4")
pates.q4.graph <- set.graph(pates.q4.frame, pates.q4.salience)
pates.q4.plot <- draw.graph(pates.q4.graph, "q4_pates")
pates.q4.cent <- calculate.centrality(pates.q4.graph, pates.q4.salience, "q4_pates")
pates.q4.key <- calculate.keyactors(pates.q4.cent, "q4_pates")
pates.q4.key.plot <- plot.keyactors(pates.q4.key, "q4_pates")

ncfl.q4.frame <- ncfl.frame |>
  filter(question == "Q4")
ncfl.q4.salience <- calculate.salience(ncfl.q4.frame, "GROUPING", "Q4")
ncfl.q4.graph <- set.graph(ncfl.q4.frame, ncfl.q4.salience)
ncfl.q4.plot <- draw.graph(ncfl.q4.graph, "q4_ncfl")
ncfl.q4.cent <- calculate.centrality(ncfl.q4.graph, ncfl.q4.salience, "q4_ncfl")
ncfl.q4.key <- calculate.keyactors(ncfl.q4.cent, "q4_ncfl")
ncfl.q4.key.plot <- plot.keyactors(ncfl.q4.key, "q4_ncfl")





# Calculating Actor Flexibility Score
flex.frame <<- full.frame |>
  select(question, actor = from, to)

flex.results <- lapply(unique(flex.frame$actor), calculate.node.flexibility)
flex.score <- do.call(rbind, flex.results)

q1.key <- create.q.key(pates.q1.key, ncfl.q1.key, "q1")
q3.key <- create.q.key(pates.q3.key, ncfl.q3.key, "q3")
q4.key <- create.q.key(pates.q4.key, ncfl.q4.key, "q4")
q.key <<- bind_rows(q1.key, q3.key, q4.key) |>
  select(actor, keystatus) |>
  mutate(keyscore = case_when(
    keystatus == "Sage" ~ 3,
    keystatus == "Steward" ~ 2,
    keystatus == "Weaver" ~ 1
  )) |>
  group_by(actor) |>
  summarize(keyscore = sum(keyscore)) |>
  mutate(keyscore = keyscore / 9)

q1.cent <- create.q.cent(pates.q1.cent, ncfl.q1.cent, "q1") |>
  calculate.ranks()
q3.cent <- create.q.cent(pates.q3.cent, ncfl.q3.cent, "q3") |>
  calculate.ranks()
q4.cent <- create.q.cent(pates.q4.cent, ncfl.q4.cent, "q4") |>
  calculate.ranks()
q.cent <<- bind_rows(q1.cent, q3.cent, q4.cent)

full.avg.cent <- q.cent |>
  select(
    actor, leverage_rank, laplacian_rank, outdegree_rank, indegree_rank,
    latora_rank, leaderrank_rank, smiths_rank
  ) |>
  group_by(actor) |>
  summarize(across(everything(), mean), .groups = "drop") |>
  left_join(flex.score) |>
  left_join(q.key) |>
  replace_na(list(flexibility = 0, keyscore = 0)) |>
  mutate(flexibility_rank = dense_rank(desc(flexibility))) |>
  mutate(keyscore_rank = dense_rank(desc(keyscore))) |>
  ungroup() |>
  as.data.frame()

corr.avg <- full.avg.cent |>
  select(-flexibility, -keyscore, -actor) |>
  plot.corr("rank-corr")
print(corr.avg)

keyactors.q.frame <- q.key |>
  left_join(q1.key) |>
  left_join(q3.key, by = "actor") |>
  left_join(q4.key, by = "actor") |>
  select(actor,
    q1_status = keystatus.x, q3_status = keystatus.y,
    q4_status = keystatus
  ) |>
  arrange(actor)

keyactors.frame <- full.avg.cent |>
  filter(keyscore > 0) |>
  mutate(positionality_score = ((1 / (laplacian_rank + leverage_rank) / 2)) * 10) |>
  mutate(reputation_score = ((1 / (smiths_rank + leaderrank_rank) / 2)) * 10) |>
  mutate(reachability_score = ((1 / (latora_rank + indegree_rank) / 2)) * 10) |>
  mutate(overall_score = (positionality_score + reputation_score + reachability_score) / 3) |>
  select(
    actor, overall_score, positionality_score, reachability_score, reputation_score,
    keyscore, keyscore_rank
  ) |>
  left_join(keyactors.q.frame) |>
  unique() |>
  arrange(desc(overall_score))

rmarkdown::paged_table(keyactors.frame)
write_csv(keyactors.frame, file = "output/csv/keyactors_analysis.csv")

keyactors.corr <- keyactors.frame |>
  select(positionality_score, reachability_score, reputation_score) |>
  plot.corr("keyactors-corr")
print(keyactors.corr)

key.reputation <- keyactors.frame |>
  select(actor, reputation_score)
participants.frame <- full.avg.cent |>
  left_join(key.reputation) |>
  filter(substr(actor, 1, 2) == "PA" | substr(actor, 1, 2) == "FL" | substr(actor, 1, 2) == "NC") |>
  mutate(reputation_rank = dense_rank(desc(reputation_score))) |>
  replace_na(list(reputation_rank = 0))
reputation_rank_na <- (max(participants.frame$reputation_rank)) + 1
participants.frame <- participants.frame |>
  mutate(reputation_rank = if_else(reputation_rank == 0, reputation_rank_na, reputation_rank)) |>
  mutate(potentiality_score = ((1 / (flexibility_rank + reputation_rank) / 2)) * 10) |>
  mutate(positionality_score = ((1 / (laplacian_rank + leverage_rank) / 2)) * 10) |>
  mutate(reachability_score = ((1 / (latora_rank + outdegree_rank) / 2)) * 10) |>
  mutate(overall_score = (potentiality_score + positionality_score + reachability_score) / 3) |>
  select(
    actor, overall_score, positionality_score, reachability_score, potentiality_score,
    laplacian_rank, leverage_rank, latora_rank, outdegree_rank,
    flexibility_rank, keyscore_rank, reputation_rank
  ) |>
  arrange(desc(overall_score)) |>
  as.data.frame()

rmarkdown::paged_table(participants.frame)
write_csv(participants.frame, file = "output/csv/participants_analysis.csv")

participants.corr <- participants.frame |>
  select(positionality_score, reachability_score, potentiality_score) |>
  plot.corr("participants-corr")
print(participants.corr)

overall.frame <- participants.frame |>
  select(actor, overall_score) |>
  mutate(color_code = substr(actor, 1, 2))
overall.plot <- ggstripchart(overall.frame,
  x = "color_code", y = "overall_score", label = "actor",
  repel = TRUE, color = "color_code", palette = "the.palette",
  fill = "color_code", label.rectangle = FALSE, show.legend = FALSE,
  label.color = "color_code", xlab = "Network", ylab = "Overall Score"
) +
  theme_few() +
  theme(legend.position = "none")
plot.save(overall.plot, "overall-corr")
print(overall.plot)

## timeline.frame <- read_csv("data/timeline-data.csv", col_names = TRUE, show_col_types = FALSE)
## timeline.plot <- gg_vistime(timeline.frame,
##   col.event = "event", col.group = "layer",
##   col.start = "start", col.end = "end", optimize_y = TRUE
## ) +
##   theme_few()
## plot.save(timeline.plot, "timeline")
