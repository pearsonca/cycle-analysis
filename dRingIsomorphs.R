# directed "ring" sub-graph isomorphisms

rm(list=ls())

require(igraph)

.maxLength <- 6

# make isomorphisms up to desired size?
# makeIsomorphisms <- function(ringSize) {
#   base <- igraph::make_ring(n=ringSize, directed=TRUE) %>% 
#     set_edge_attr("color", value="black") %>%
#     set_vertex_attr("label", value=NA)
#   maxSwaps <- ringSize %/% 2 ## integer divide
#   one <- base + edge(2,1, color="red") - edge("1|2")
#   if (maxSwaps > 1) for (swaps in 2:maxSwaps) {
#     
#   }
# }

.makeBase <- function(n) igraph::make_ring(n, directed=TRUE) %>% 
  set_edge_attr("color", value="black") %>%
  set_vertex_attr("label", value=NA) %>%
  set_vertex_attr("size", value=1)

.refFlip <- function(g) g + edge(2,1, color="red") - edge("1|2")

.plotAllIsos <- function(...) plot(Reduce(`+`, list(...)))

.verif <- function(...) combn(list(...), 2, function(elems) print(elems[[1]], elems[[2]]) )

tri <- .makeBase(3)
triAlt <- .refFlip(tri)

.plotAllIsos(tri, triAlt)

quad <- .makeBase(4)
quadAlt <- .refFlip(quad)
quad21 <- quadAlt + edge(3,2,color="red") - edge("2|3")
quad22 <- quadAlt + edge(4,3,color="red") - edge("3|4")

.plotAllIsos(quad,quadAlt,quad21,quad22)

quint <- .makeBase(5)
quintAlt <- .refFlip(quint)
quint21 <- quintAlt + edge(3,2,color="red") - edge("2|3")
quint22 <- quintAlt + edge(4,3,color="red") - edge("3|4")

.plotAllIsos(quint, quintAlt, quint21, quint22)

sext <- .makeBase(6)
sextAlt <- .refFlip(sext)

sext21 <- sextAlt + edge(3,2,color="red") - edge("2|3")
sext22 <- sextAlt + edge(4,3,color="red") - edge("3|4")
sext23 <- sextAlt + edge(5,4,color="red") - edge("4|5")

sext31 <- sext21 + edge(4,3,color="red") - edge("3|4")
sext32 <- sext21 + edge(5,4,color="red") - edge("4|5")
sext33 <- sext22 + edge(5,4,color="red") - edge("4|5")
sext34 <- sext22 + edge(6,5,color="red") - edge("5|6")

.plotAllIsos(sext, sextAlt, sext21, sext22, sext23, sext31, sext32, sext33, sext34)



# store result