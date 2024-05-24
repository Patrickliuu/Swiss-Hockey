library(xml2)
library(DiagrammeRsvg)
library(magrittr)
library(rsvg)
library(DiagrammeR)
library(data.tree)
library(plyr)


graph <- grViz("digraph {

graph [layout = dot, rankdir = LR]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = Linen]

data1 [label = '49ing Daten', shape = folder, fillcolor = Beige]
process [label =  'Daten- \n verarbeitung']
statistical [label = 'Statistische \n Analyse']
results [label= 'Resultate']

# edge definitions with the node IDs
{data1}  -> process -> statistical -> results
}")



graph %>%
  export_svg() %>%
  read_xml() %>%
  write_xml("output/graph.svg")


# Create datastructure plot
library(fs)
install.packages("fs")
dir_tree(path = "./")
