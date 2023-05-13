## #  __________________________________________________
## # |                                                  |
## # |     THE ANALYSIS IS NOT THE SOCIAL NETWORK       |
## # |  Illuminating the unruly nature of intentional   |
## # |  social networks in educational justice efforts  |
## # |__________________________________________________|
## # |                                                  |
## # |   Funded in part by a National Association for   |
## # |    Family, School, and Community Engagement      |
## # |                    Mini-Grant.                   |
## # |__________________________________________________|

## # == LOAD LIBRARIES ============================================================

library(igraph)
library(tidygraph)
library(ggraph)
library(centiserve)

library(AnthroTools)

library(readr)
library(rio)
library(glue)
library(tidyr)
library(dplyr)
library(gt)
library(ggthemes)
library(ggpubr)
library(ggcorrplot)
library(vistime)

## # == DEFINE CONSTANTS ==========================================================

## # -- Define the common color palette -------------------------------------------

the_palette <<- c(
  "FL" = "#6929c4", "NC" = "#1192e8", "PA" = "#005d5d",
  "CF" = "#9f1853", "FF" = "#fa4d56", "IL" = "#570408",
  "OR" = "#198038", "OT" = "#002d9c", "SA" = "#ee538b",
  "ST" = "#b28600", "UA" = "#009d9a", "UF" = "#012749",
  "US" = "#8a3800"
)

## # -- Define the participant abbreviation list ----------------------------------

the_abbrev <<- data.frame(
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

## # == DEFINE FUNCTIONS ==========================================================

## # -- Save plots as PDF and PNG -------------------------------------------------

plot_save <- function(the_plot, the_file) {
  # Set the filename for the PDF.
  pdf_name <- glue("output/plots/{the_file}.pdf")
  # Set the filname for the PNG.
  png_name <- glue("output/plots/{the_file}.png")
  # Save as PDF.
  ggsave(the_plot,
    filename = pdf_name,
    width = 11.5,
    height = 8,
    units = "in",
    dpi = 300
  )
  # Save as PNG.
  ggsave(the_plot,
    filename = png_name,
    width = 11.5,
    height = 8,
    units = "in",
    dpi = 300
  )
}

## # -- Create Correlation Plot ---------------------------------------------------

plot_corr <- function(the_frame, the_file) {
  # Calculate the correlation of the provided dataframe.
  corr <- round(cor(the_frame), 1)
  # Calculate a matrix of significance.
  p_mat <- cor_pmat(corr)
  # Initialize plot.
  corr_plot <- ggcorrplot(corr,
    hc.order = TRUE, # Order according to hierarchical clustering.
    type = "lower", # Only display the bottom half.
    p.mat = p_mat, # Account for statistical significance.
    colors = c("#750e13", "#ffffff", "#003a6d")
  ) # Set colors.
  # Save the plot...
  plot_save(corr_plot, the_file)
  # ...and return it.
  return(corr_plot)
}

## # -- Calculate Tukey's Fences --------------------------------------------------

calculate_tukey <- function(the_cent) {
  # Calculate Tukey's fences
  q <- quantile(the_cent, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  the_fence <- data.frame(
    lower = q[1] - 1.5 * iqr,
    upper = q[2] + 1.5 * iqr
  )
  return(the_fence)
}

## # -- Prepare the data and initialize the graph object --------------------------

set_graph <- function(the_frame, the_salience) {
  # Reduce the edge list to just i (from) and j (to).
  the_frame <- the_frame |>
    select("from", "to")
  # Calculate the number of times i names j.
  the_weight <- the_frame |>
    group_by(from, to) |>
    summarize(weight = n()) |>
    ungroup()
  # Combine the dataframes into one, matching the weight to the edge.
  the_frame <- merge(the_frame, the_weight, by = c("from", "to"))
  # Create the igraph object, and set it to be a directed graph (i.e., i -> j).
  the_graph <- the_frame |>
    graph_from_data_frame(directed = TRUE)
  the_salience <- the_salience |> rename("name" = "actor")
  node_data <- data.frame(name = V(the_graph)$name) |>
    mutate(id_no = substr(V(the_graph)$name, 3, 4)) |>
    mutate(color_code = substr(V(the_graph)$name, 1, 2)) |>
    left_join(the_salience) |>
    left_join(the_abbrev) |>
    mutate(label = glue("{full} {id_no}")) |>
    replace_na(list(SmithsS = 0.01))
  V(the_graph)$color_code <- node_data$color_code
  V(the_graph)$size_code <- (node_data$SmithsS) * 100
  V(the_graph)$label <- node_data$label
  # Send the graph object back for further processing.
  return(the_graph)
}

## # -- Plot the graph object -----------------------------------------------------

draw_graph <- function(the_graph, the_file) {
  # Set the filename for saving the graph.
  the_file <- glue("sna_{the_file}-plot")
  # Set a reproducible seed for randomization, used to ensure that the plot looks more or less
  # the same each time it is created.
  set.seed(123)
  # Create the plot.
  the_plot <- the_graph |>
    ggraph(layout = "fr") + # Display the graph using the Fruchterman and Reingold algorithm.
    geom_edge_fan(color = "#A7A9AB") + # Plot the edges between nodes.
    geom_node_point(
      aes(
        color = color_code, # Plot the nodes with the color determined by the
        linewidth = size_code
      ), # participant and the size of the node determined
      show.legend = FALSE
    ) + # by the Smith's S Salience Score.
    scale_size_continuous(range = c(2.5, 10)) + # Rescale the node size.
    scale_color_manual(values = the_palette) + # Bring in the color palette.
    geom_node_text(aes(label = label), repel = TRUE) + # Place the actor name on the graph.
    labs(
      edge_width = "Letters",
      title = "Social Network",
      caption = "Test caption"
    ) +
    theme_few() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.border = element_blank()
    )
  # Save the plot.
  plot_save(the_plot, the_file)
  # Return the plot for further use as necessary.
  return(the_plot)
}

calculate_centrality <- function(the_graph, the_salience, the_file) {
  analysis_network_data <- data.frame(
    indegree = igraph::degree(the_graph, mode = "in"),
    outdegree = igraph::degree(the_graph, mode = "out"),
    leaderrank = leaderrank(the_graph),
    laplace = laplacian(the_graph),
    leverage = leverage(the_graph),
    latora = closeness.latora(the_graph)
  )
  analysis_network_data$actor <- rownames(analysis_network_data)
  rownames(analysis_network_data) <- NULL
  analysis_network_data <- analysis_network_data |>
    select(actor, everything()) |>
    left_join(the_salience) |>
    replace_na(list(SmithsS = 0))
  rownames(analysis_network_data) <- analysis_network_data$actor
  return(analysis_network_data)
}

calculate_salience <- function(the_frame, the_grouping, the_file) {
  the_filename <- glue("output/csv/salience_{the_file}.csv")
  anthro_frame <- the_frame |>
    select("Subj" = "from", "Order" = "order", "CODE" = "to", "GROUPING" = "question") |>
    add_count(Subj, GROUPING) |>
    filter(n > 1)
  if (the_grouping == "none") {
    anthro_frame <- anthro_frame |>
      select("Subj", "Order", "CODE") |>
      distinct() |>
      as.data.frame()
    anthro_frame$Order <- as.numeric(anthro_frame$Order)
    the_salience <- CalculateSalience(anthro_frame)
  } else {
    anthro_frame <- anthro_frame |>
      select("Subj", "Order", "CODE", "GROUPING") |>
      distinct() |>
      as.data.frame()
    anthro_frame$Order <- as.numeric(anthro_frame$Order)
    the_salience <- CalculateSalience(anthro_frame, GROUPING = "GROUPING")
  }
  code_salience <- SalienceByCode(the_salience, dealWithDoubles = "MAX")
  write_csv(code_salience, the_filename, append = FALSE)
  code_salience <- code_salience |>
    select("actor" = "CODE", "SmithsS")
  return(code_salience)
}

calculate_keyactors <- function(the_frame, the_file) {
  max_leverage <- max(the_frame$leverage, na.rm = TRUE)
  min_leverage <- min(the_frame$leverage, na.rm = TRUE)
  key_frame <- the_frame %>%
    select(actor, leverage, leaderrank, SmithsS)
  key_res <- lm(leaderrank ~ leverage, data = key_frame)$residuals |>
    as.data.frame() |>
    rename(res = 1) |>
    mutate(res = abs(res))
  key_res$actor <- row.names(key_res)
  row.names(key_res) <- NULL
  key_frame <- key_frame |>
    left_join(key_res)
  leaderrank_fence <- calculate_tukey(key_frame$leaderrank)
  key_frame_leaderrank_trimmed <- key_frame |>
    filter(leaderrank >= leaderrank_fence$lower & leaderrank <= leaderrank_fence$upper)
  leverage_fence <- calculate_tukey(key_frame$leverage)
  key_frame_leverage_trimmed <- key_frame |>
    filter(leverage >= leverage_fence$lower & leverage_fence$upper)
  key_ymean <<- mean(key_frame_leaderrank_trimmed$leaderrank)
  key_xmean <<- mean(key_frame_leverage_trimmed$leverage)
  key_frame <- key_frame |>
    mutate(keystatus = case_when(
      (leaderrank > key_ymean & leverage > key_xmean) ~ "Sage",
      (leaderrank > key_ymean & leverage < key_xmean) ~ "Steward",
      (leaderrank < key_ymean & leverage > key_xmean) ~ "Weaver"
    )) |>
    na.omit() |>
    group_by(keystatus) |>
    arrange(desc(res), desc(SmithsS)) |>
    unique() |>
    ungroup()
  key_frame <- key_frame |>
    select(actor, leverage, leaderrank, res, SmithsS, keystatus) |>
    arrange(keystatus, desc(res), desc(SmithsS))
  return(key_frame)
}

plot_keyactors <- function(key_frame, the_file) {
  the_filename <- glue("keyactors_{the_file}-plot")
  key_xmin <- min(key_frame$leverage)
  key_xmax <- max(key_frame$leverage)
  key_ymin <- min(key_frame$leaderrank)
  key_ymax <- max(key_frame$leaderrank)
  steward_count <- count_keyactors(key_frame, "Steward")
  sage_count <- count_keyactors(key_frame, "Sage")
  weaver_count <- count_keyactors(key_frame, "Weaver")
  key_frame <- key_frame |>
    mutate(color_code = substr(key_frame$actor, 1, 2)) |>
    mutate(id_no = substr(key_frame$actor, 3, 4)) |>
    left_join(the_abbrev) |>
    mutate(label = glue("{full} {id_no}")) |>
    select(-full, -id_no)
  key_plot <- ggscatter(key_frame,
    x = "leverage", y = "leaderrank",
    label = "label", label.rectangle = FALSE, repel = TRUE,
    theme = theme_minimal(), ylab = "Leader Rank Centrality",
    xlab = "Leverage Centrality", point = TRUE, show.legend = FALSE,
    color = "color_code", palette = the_palette,
    conf.int = FALSE, cor.coef = FALSE, legend = "none"
  )
  if (steward_count != 0) {
    key_plot <- key_plot +
      geom_vline(xintercept = key_xmean, color = "#243142", alpha = 0.2) +
      geom_label(aes(x = key_xmin, y = key_ymax, label = "Stewards", hjust = 0),
        color = "#243142", fill = "#A7A9AB"
      )
  }
  if (weaver_count != 0) {
    key_plot <- key_plot +
      geom_hline(yintercept = key_ymean, color = "#243142", alpha = 0.2) +
      geom_label(aes(
        x = key_xmax, y = key_ymin,
        label = "Weavers", hjust = 1
      ), color = "#243142", fill = "#A7A9AB")
  }
  key_plot <- key_plot +
    geom_label(aes(
      x = key_xmax, y = key_ymax,
      label = "Sages", hjust = 1
    ), color = "#243142", fill = "#A7A9AB") +
    theme_few() +
    theme(legend.position = "none")
  plot_save(key_plot, the_filename)
  return(key_plot)
}

count_keyactors <- function(key_frame, the_actor) {
  the_count <- key_frame |>
    count(keystatus) |>
    filter(keystatus == the_actor) |>
    pull(n)
  the_count <- ifelse(is.numeric(the_count), the_count, 0)
  the_count <- the_count |> replace_na(0)
  return(the_count)
}

create_q_key <- function(the_1, the_2, the_question) {
  the_q_key <- bind_rows(the_1, the_2) |>
    mutate(question = the_question) |>
    select(question, actor, keystatus)
  return(the_q_key)
}

create_q_cent <- function(the_1, the_2, the.question) {
  the_q_cent <- bind_rows(the_1, the_2) |>
    mutate(question = the.question) |>
    select(
      question, actor, outdegree, indegree, leverage,
      laplace, leaderrank, latora, SmithsS
    )
}

calculate_ranks <- function(the_cent) {
  the_cent <- the_cent |>
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
  rownames(the_cent) <- NULL
  return(the_cent)
}

calculate_jaccard <- function(the_set_1, the_set_2) {
  the_jaccard <- (length(intersect(the_set_1$to, the_set_2$to)) / length(union(the_set_1$to, the_set_2$to)))
  return(the_jaccard)
}

calculate_situational_flexibility <- function(the_actor) {
  set1 <- flex_frame |>
    filter(actor == the_actor & question == "Q1") |>
    select(actor, to) |>
    as.data.frame()
  set1$actor <- as.factor(set1$actor)
  set1$to <- as.factor(set1$to)
  set2 <- flex_frame |>
    filter(actor == the_actor & question == "Q3") |>
    select(actor, to) |>
    as.data.frame()
  set2$actor <- as.factor(set2$actor)
  set2$to <- as.factor(set2$to)
  set3 <- flex_frame |>
    filter(actor == the_actor & question == "Q4") |>
    select(actor, to) |>
    as.data.frame()
  set3$actor <- as.factor(set3$actor)
  set3$to <- as.factor(set3$to)
  jaccard_1 <- calculate_jaccard(set1, set2)
  jaccard_2 <- calculate_jaccard(set2, set3)
  jaccard_3 <- calculate_jaccard(set1, set3)
  # the_flexibility <- 1 - ((1 / (3 * (3 - 1)) * (jaccard_1 + jaccard_2 + jaccard_3)) / 3)
  the_flexibility <- 1 - ((jaccard_1 + jaccard_2 + jaccard_3) / 3)
  response_frame <- data.frame(actor = the_actor, flexibility = the_flexibility)
  return(response_frame)
}

pates_frame <- import("https://osf.io/download/62qpa/", format = "csv") |>
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
write_csv(pates_frame, "output/csv/pates_frame.csv")

ncfl_frame <- import("https://osf.io/download/ghz3c/", format = "csv") |>
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
write_csv(ncfl_frame, "output/csv/ncfl_frame.csv")

full_frame <- rbind(pates_frame, ncfl_frame)

full_salience <- calculate_salience(full_frame, "GROUPING", "full")
full_graph <- set_graph(full_frame, full_salience)
full_plot <- draw_graph(full_graph, "full")



pates_q1_frame <- pates_frame |>
  filter(question == "Q1")
pates_q1_salience <- calculate_salience(pates_q1_frame, "GROUPING", "Q1")
pates_q1_graph <- set_graph(pates_q1_frame, pates_q1_salience)
pates_q1_plot <- draw_graph(pates_q1_graph, "q1_pates")
pates_q1_cent <- calculate_centrality(pates_q1_graph, pates_q1_salience, "q1_pates")
pates_q1_key <- calculate_keyactors(pates_q1_cent, "q1_pates")
pates_q1_key_plot <- plot_keyactors(pates_q1_key, "q1_pates")

ncfl_q1_frame <- ncfl_frame |>
  filter(question == "Q1")
ncfl_q1_salience <- calculate_salience(ncfl_q1_frame, "GROUPING", "Q1")
ncfl_q1_graph <- set_graph(ncfl_q1_frame, ncfl_q1_salience)
ncfl_q1_plot <- draw_graph(ncfl_q1_graph, "q1_ncfl")
ncfl_q1_cent <- calculate_centrality(ncfl_q1_graph, ncfl_q1_salience, "q1_ncfl")
ncfl_q1_key <- calculate_keyactors(ncfl_q1_cent, "q1_ncfl")
ncfl_q1_key_plot <- plot_keyactors(ncfl_q1_key, "q1_ncfl")

pates_q3_frame <- pates_frame |>
  filter(question == "Q3")
pates_q3_salience <- calculate_salience(pates_q3_frame, "GROUPING", "Q3")
pates_q3_graph <- set_graph(pates_q3_frame, pates_q3_salience)
pates_q3_plot <- draw_graph(pates_q3_graph, "q3_pates")
pates_q3_cent <- calculate_centrality(pates_q3_graph, pates_q3_salience, "q3_pates")
pates_q3_key <- calculate_keyactors(pates_q3_cent, "q3_pates")
pates_q3_key_plot <- plot_keyactors(pates_q3_key, "q3_pates")

ncfl_q3_frame <- ncfl_frame |>
  filter(question == "Q3")
ncfl_q3_salience <- calculate_salience(ncfl_q3_frame, "GROUPING", "Q3")
ncfl_q3_graph <- set_graph(ncfl_q3_frame, ncfl_q3_salience)
ncfl_q3_plot <- draw_graph(ncfl_q3_graph, "q3_ncfl")
ncfl_q3_cent <- calculate_centrality(ncfl_q3_graph, ncfl_q3_salience, "q3_ncfl")
ncfl_q3_key <- calculate_keyactors(ncfl_q3_cent, "q3_ncfl")
ncfl_q3_key_plot <- plot_keyactors(ncfl_q3_key, "q3_ncfl")

pates_q4_frame <- pates_frame |>
  filter(question == "Q4")
pates_q4_salience <- calculate_salience(pates_q4_frame, "GROUPING", "Q4")
pates_q4_graph <- set_graph(pates_q4_frame, pates_q4_salience)
pates_q4_plot <- draw_graph(pates_q4_graph, "q4_pates")
pates_q4_cent <- calculate_centrality(pates_q4_graph, pates_q4_salience, "q4_pates")
pates_q4_key <- calculate_keyactors(pates_q4_cent, "q4_pates")
pates_q4_key_plot <- plot_keyactors(pates_q4_key, "q4_pates")

ncfl_q4_frame <- ncfl_frame |>
  filter(question == "Q4")
ncfl_q4_salience <- calculate_salience(ncfl_q4_frame, "GROUPING", "Q4")
ncfl_q4_graph <- set_graph(ncfl_q4_frame, ncfl_q4_salience)
ncfl_q4_plot <- draw_graph(ncfl_q4_graph, "q4_ncfl")
ncfl_q4_cent <- calculate_centrality(ncfl_q4_graph, ncfl_q4_salience, "q4_ncfl")
ncfl_q4_key <- calculate_keyactors(ncfl_q4_cent, "q4_ncfl")
ncfl_q4_key_plot <- plot_keyactors(ncfl_q4_key, "q4_ncfl")





# Calculating Actor Flexibility Score
flex_frame <<- full_frame |>
  select(question, actor = from, to)

flex_results <- lapply(unique(flex_frame$actor), calculate_situational_flexibility)
flex_score <- do.call(rbind, flex_results)

q1_key <- create_q_key(pates_q1_key, ncfl_q1_key, "q1")
q3_key <- create_q_key(pates_q3_key, ncfl_q3_key, "q3")
q4_key <- create_q_key(pates_q4_key, ncfl_q4_key, "q4")
q_key <<- bind_rows(q1_key, q3_key, q4_key) |>
  select(actor, keystatus) |>
  mutate(keyscore = case_when(
    keystatus == "Sage" ~ 3,
    keystatus == "Steward" ~ 2,
    keystatus == "Weaver" ~ 1
  )) |>
  group_by(actor) |>
  summarize(keyscore = sum(keyscore)) |>
  mutate(keyscore = keyscore / 9)

q1_cent <- create_q_cent(pates_q1_cent, ncfl_q1_cent, "q1") |>
  calculate_ranks()
q3_cent <- create_q_cent(pates_q3_cent, ncfl_q3_cent, "q3") |>
  calculate_ranks()
q4_cent <- create_q_cent(pates_q4_cent, ncfl_q4_cent, "q4") |>
  calculate_ranks()
q_cent <<- bind_rows(q1_cent, q3_cent, q4_cent)

full_avg_cent <- q_cent |>
  select(
    actor, leverage_rank, laplacian_rank, outdegree_rank, indegree_rank,
    latora_rank, leaderrank_rank, smiths_rank
  ) |>
  group_by(actor) |>
  summarize(across(everything(), mean), .groups = "drop") |>
  left_join(flex_score) |>
  left_join(q_key) |>
  replace_na(list(flexibility = 0, keyscore = 0)) |>
  mutate(flexibility_rank = dense_rank(desc(flexibility))) |>
  mutate(keyscore_rank = dense_rank(desc(keyscore))) |>
  ungroup() |>
  as.data.frame()

corr_avg <- full_avg_cent |>
  select(-flexibility, -keyscore, -actor) |>
  plot_corr("rank-corr")
print(corr_avg)

keyactors_q_frame <- q_key |>
  left_join(q1_key) |>
  left_join(q3_key, by = "actor") |>
  left_join(q4_key, by = "actor") |>
  select(actor,
    q1_status = keystatus.x, q3_status = keystatus.y,
    q4_status = keystatus
  ) |>
  arrange(actor)

keyactors_frame <- full_avg_cent |>
  filter(keyscore > 0) |>
  mutate(positionality_score = ((1 / (laplacian_rank + leverage_rank) / 2)) * 10) |>
  mutate(reputation_score = ((1 / (smiths_rank + leaderrank_rank) / 2)) * 10) |>
  mutate(reachability_score = ((1 / (latora_rank + indegree_rank) / 2)) * 10) |>
  mutate(overall_score = (positionality_score + reputation_score + reachability_score) / 3) |>
  select(
    actor, overall_score, positionality_score, reachability_score, reputation_score,
    keyscore, keyscore_rank
  ) |>
  left_join(keyactors_q_frame) |>
  unique() |>
  arrange(desc(overall_score))
write_csv(keyactors_frame, file = "output/csv/keyactors_analysis.csv")



keyactors_corr <- keyactors_frame |>
  select(positionality_score, reachability_score, reputation_score) |>
  plot_corr("keyactors-corr")
print(keyactors_corr)

key_reputation <- keyactors_frame |>
  select(actor, reputation_score)
participants_frame <- full_avg_cent |>
  left_join(key_reputation) |>
  filter(substr(actor, 1, 2) == "PA" | substr(actor, 1, 2) == "FL" | substr(actor, 1, 2) == "NC") |>
  mutate(reputation_rank = dense_rank(desc(reputation_score))) |>
  replace_na(list(reputation_rank = 0))
reputation_rank_na <- (max(participants_frame$reputation_rank)) + 1
participants_frame <- participants_frame |>
  mutate(reputation_rank = if_else(reputation_rank == 0, reputation_rank_na, reputation_rank)) |>
  mutate(potentiality_score = ((1 / (flexibility_rank + reputation_rank) / 2)) * 10) |>
  mutate(positionality_score = ((1 / (laplacian_rank + leverage_rank) / 2)) * 10) |>
  mutate(reachability_score = ((1 / (latora_rank + outdegree_rank) / 2)) * 10) |>
  # mutate(potentiality_score = (((flexibility_rank + reputation_rank) / 2))) |>
  # mutate(positionality_score = (((laplacian_rank + leverage_rank) / 2))) |>
  # mutate(reachability_score = (((latora_rank + outdegree_rank) / 2))) |>
  mutate(
    overall_score =
      (potentiality_score +
        positionality_score +
        reachability_score)
      / 3
  ) |>
  select(
    actor,
    overall_score,
    positionality_score,
    reachability_score,
    potentiality_score,
    laplacian_rank,
    leverage_rank,
    latora_rank,
    outdegree_rank,
    flexibility_rank,
    keyscore_rank,
    reputation_rank
  ) |>
  arrange(desc(overall_score)) |>
  as.data.frame()
write_csv(participants_frame,
  file = "output/csv/participants_analysis.csv"
)



participants_corr <- participants_frame |>
  select(
    positionality_score,
    reachability_score,
    potentiality_score
  ) |>
  plot_corr("participants-corr")
print(participants_corr)

overall_frame <- participants_frame |>
  select(actor, overall_score) |>
  mutate(color_code = substr(actor, 1, 2))
overall_plot <- ggstripchart(
  overall_frame,
  x = "color_code",
  y = "overall_score",
  label = "actor",
  repel = TRUE,
  color = "color_code",
  palette = "the_palette",
  fill = "color_code",
  label.rectangle = FALSE,
  show.legend = FALSE,
  label.color = "color_code",
  xlab = "Network",
  ylab = "Overall Score"
) +
  ggtitle("Participants") +
  theme_few() +
  theme(legend.position = "none")
plot_save(overall_plot, "overall-corr")
print(overall_plot)

timeline_frame <- read_csv("data/timeline-data.csv",
  col_names = TRUE,
  show_col_types = FALSE
)
timeline_frame$layer <- factor(timeline_frame$layer,
  levels = c("act", "local", "sped", "policy", "epoch")
)
timeline_plot <- gg_vistime(timeline_frame,
  col.event = "event", col.group = "layer",
  col.start = "start", col.end = "end", optimize_y = FALSE,
  title = "Multilayered Timeline",
  background_lines = NULL
) +
  theme_few() +
  geom_segment(
    aes(
      x = as.POSIXct("1954-01-30"), y = 9,
      xend = as.POSIXct("1956-01-30"), yend = 31.5
    ),
    color = "#A7A9AB", linetype = 2, linewidth = 0.65
  ) +
  geom_segment(
    aes(
      x = as.POSIXct("1959-01-30"), y = 31.75,
      xend = as.POSIXct("1968-01-30"), yend = 31
    ),
    color = "#A7A9AB", linetype = 2, linewidth = 0.65
  ) +
  geom_segment(
    aes(
      x = as.POSIXct("1970-01-30"), y = 22,
      xend = as.POSIXct("1970-01-30"), yend = 30.5
    ),
    color = "#A7A9AB", linetype = 2, linewidth = 0.65
  )
plot_save(timeline_plot, "timeline")



author_contributions <- data.frame(
  Role = c(
    "Conceptualization", "Data Curation",
    "Formal Analysis", "Funding Acquisition",
    "Investigation", "Methodology",
    "Project Administration", "Software",
    "Supervision", "Visualization",
    "Writing - Original Draft",
    "Writing - Review & Editing"
  ),
  Authors = c(
    "Jeremy F Price, Cristinia Santamaría Graff", "Jeremy F Price, Cristinia Santamaría Graff, Akaash Arora, Amy Waechter-Versaw, Román Graff",
    "Jeremy F Price, Cristinia Santamaría Graff, Akaash Arora, Amy Waechter-Versaw, Román Graff", "Cristinia Santamaría Graff, Jeremy F Price",
    "Jeremy F Price, Cristinia Santamaría Graff", "Jeremy F Price", "Cristinia Santamaría Graff, Jeremy F Price", "Jeremy F Price",
    "Jeremy F Price", "Jeremy F Price", "Jeremy F Price, Cristinia Santamaría Graff, Akaash Arora, Amy Waechter-Versaw, Román Graff",
    "Jeremy F Price, Cristinia Santamaría Graff, Akaash Arora, Amy Waechter-Versaw, Román Graff"
  )
)

author_contributions |>
  gt() |>
  opt_table_font(
    font = list(google_font("Atkinson Hyperlegible"), default_fonts())
  ) |>
  tab_style(
    style = list(
      "font-weight: bold; vertical-align: top;"
    ),
    locations = cells_body(columns = Role)
  ) |>
  tab_options(
    table.font.size = 14,
    data_row.padding = px(3)
  ) |>
  tab_source_note(
    source_note = md("Available in [JATS](aux/credit.xml) format.")
  )

sessioninfo::session_info(pkgs = "attached")
