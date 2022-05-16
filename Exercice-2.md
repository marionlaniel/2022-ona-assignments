Exercice 2
================

## Load Dataset

``` r
seats <- read.csv("FacebookSeat.csv")
seats
```

    ##    Seat.1 Seat.2
    ## 1       6      5
    ## 2       6      d
    ## 3       6      b
    ## 4       1      2
    ## 5       2      a
    ## 6       a      b
    ## 7       a      c
    ## 8       b      c
    ## 9       b      d
    ## 10      b      3
    ## 11      c      d
    ## 12      c      3
    ## 13      c      4
    ## 14      d      3
    ## 15      d      5
    ## 16      3      5
    ## 17      3      4

## Calculating metrics

``` r
# Degree Centrality
Degree<-degree(seats_graph, v = V(seats_graph), 
      mode = c("total"), 
      loops = TRUE, normalized = FALSE)
Degree
```

    ## 6 5 d b 1 2 a c 3 4 
    ## 3 3 5 5 1 2 3 5 5 2

``` r
# Closeness Centrality
Closeness <- closeness(seats_graph)
Closeness
```

    ##          6          5          d          b          1          2          a 
    ## 0.05263158 0.04761905 0.06250000 0.07142857 0.03333333 0.04545455 0.06250000 
    ##          c          3          4 
    ## 0.07142857 0.06250000 0.05000000

``` r
# Betweeness Centrality
Betweenness <- betweenness(seats_graph)
Betweenness
```

    ##          6          5          d          b          1          2          a 
    ##  0.9333333  0.5333333  3.2666667  9.0333333  0.0000000  8.0000000 14.0000000 
    ##          c          3          4 
    ##  8.6000000  4.6333333  0.0000000

## Choice

Looking at the closeness centrality, we notice that seat B and C have
the highest scores, meaning that they have the shortest distance with
the other seats. However, looking at the betweeness centrality, we
notice that seat A has the highest score, meaning that it is the seat
with the highest level of influence for transmitting information in the
network. Finally, looking at the degree centrality, we notice that nodes
B, C and D have 5 links, while seat A has 3 links. Therefore, the best
seat would be B, since it is closest to more seats, has good metrics for
closeness and betweeness centrality.

## Plot Graph

``` r
ggraph(seats_graph, "stress", bbox = 15) +
  geom_edge_link2(aes(edge_colour = "orange"), edge_width = 0.5) +
  geom_node_point(aes(fill = Betweenness, size = Degree), shape = 21) +
  geom_node_text(aes(label = name, size=Closeness),
    family = "serif", repel = TRUE
  ) +
  scale_edge_colour_brewer(palette = "Set1") +
  scale_size(range = c(2, 5), guide = "none") +
  theme_graph() 
```

![](Exercice-2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
