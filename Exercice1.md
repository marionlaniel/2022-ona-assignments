Exercice 1
================

## Load Dataset

``` r
Connections <- read.csv("~/Documents/McGill/S2022/ORGB672/RStudio/2022-ona-assignments/Connections.csv")
```

## Count Network per Employer

``` r
network_count = Connections %>% 
                count(Company)
network_count %>% arrange(desc(n)) %>% head(10)
```

    ##                                                Company  n
    ## 1                                                Apple 53
    ## 2  McGill University - Desautels Faculty of Management 13
    ## 3                                         Zone franche  6
    ## 4                                                       5
    ## 5                                          Air Transat  4
    ## 6                                             Deloitte  4
    ## 7                                              L'Oréal  4
    ## 8                                             Novartis  4
    ## 9                               Pratt & Whitney Canada  4
    ## 10                               Rogers Communications  4

``` r
nrow(Connections)
```

    ## [1] 302

## Create Nodes and Edges DataFrame

``` r
sources <- Connections %>%
  distinct(Last.Name) %>%
  rename(label = Last.Name)
destinations <- Connections %>%
  distinct(Company) %>%
  rename(label = Company)
nodes <- full_join(sources, destinations, by = "label")
nodes
nodes <- nodes %>% rowid_to_column("id")
nodes
per_route <- Connections %>%  
  group_by(Last.Name, Company) %>%
  summarise(weight = n()) %>% 
  ungroup()
```

    ## `summarise()` has grouped output by 'Last.Name'. You can override using the `.groups` argument.

``` r
per_route
edges <- per_route %>% 
  left_join(nodes, by = c("Last.Name" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("Company" = "label")) %>% 
  rename(to = id)
edges <- select(edges, from, to, weight)
edges
```

## Create Graph

``` r
routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
class(routes_tidy)
```

    ## [1] "tbl_graph" "igraph"

``` r
ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  labs(edge_width = "Letters") +
  theme_graph()
```

![](Untitled_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
