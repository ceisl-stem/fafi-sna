library(readr)
library(dplyr)
library(tidyr)
library(igraph)
#library(sna)
#library(GGally)
library(tidygraph)
library(ggraph)

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

pates.frame <- read_csv("data/pates-sna-01.csv", show_col_types = FALSE) |>
  mutate_all(toupper) |>
  pivot_longer(cols = starts_with("Q"),
               names_to = "question",
               values_to = "to") |>
  na.omit() |>
  select("question", "from" = "id", "to")
ncfl.frame <- read_csv("data/ncfl-sna-01.csv", show_col_types = FALSE) |>
  mutate_all(toupper) |>
  pivot_longer(cols = starts_with("Q"),
               names_to = "question",
               values_to = "to") |>
  na.omit() |>
  select("question", "from" = "ID", "to")

full.frame <- rbind(pates.frame, ncfl.frame)

df <- pates.frame[,2:3]
graph <- graph_from_data_frame(df, directed = TRUE)
pates.graph <- as_tbl_graph(graph)

ggraph(pates.graph, layout = "fr") + 
  geom_node_point() +
  geom_edge_link(alpha = 0.8) + 
  geom_node_text(aes(label = name), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

df <- full.frame[,2:3]
graph <- graph_from_data_frame(df, directed = TRUE)
full.graph <- as_tbl_graph(graph)

ggraph(full.graph, layout = "fr") + 
  geom_node_point(aes(color = as.factor(name))) +
  geom_edge_link(alpha = 0.8) + 
  geom_node_text(aes(label = name), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

full.graph |>
  ggraph(layout = "fr") +
  geom_edge_fan(arrow = arrow(length = unit(1.25, 'mm'), type = 'closed')) +
  geom_node_point(aes(color = name),
                  show.legend = F) +
  geom_edge_link(alpha = 0.8) + 
  geom_node_text(aes(label = name), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

plot(graph)
df$to <- gsub("\\d", "", df$to)
graph <- graph_from_data_frame(df, directed = TRUE)
plot(graph)

df <- ncfl.frame[,2:3]
graph <- graph_from_data_frame(df, directed = TRUE)
ncfl.graph <- as_tbl_graph(graph)

ggraph(ncfl.graph, layout = "dh") + 
  geom_node_point() +
  geom_edge_link(alpha = 0.8) + 
  geom_node_text(aes(label = name), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()


plot(graph)
df.cent <- calculate.centrality(df)
df.cent <- df.cent %>% filter(!grepl("^(PA|NC01|FL)", rownames(.)))
ggnet(graph)


