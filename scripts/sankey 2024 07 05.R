# https://blog.ouseful.info/2017/11/28/quick-round-up-visualising-flows-using-network-and-sankey-diagrams-in-python-and-r/

install.packages('networkD3')
library(networkD3)

nodes.sample <- data.frame("name" =
                            c("Sinaloa", # Node 0
                              "San Diego, CA",   # Node 1
                              "Tucson, AZ",   # Node 2
                              "Portland, OR", # Node 3
                              "Denver, CO", # Node 4
                              "CJNG")) # Node 5

links.sample <- as.data.frame(matrix(c(
  0, 1, 10,  # Each row represents a link. The first number
  0, 2, 20,  # represents the node being connected from.
  1, 3, 5,  # the second number represents the node connected to.
  2, 3, 10, # The third number is the value of the node
  5, 4, 5,
  1, 4, 3),
  byrow = TRUE, ncol = 3))

names(links.sample) <- c("source", "target", "value")

sankeyNetwork(Links = links.sample, Nodes = nodes.sample,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)