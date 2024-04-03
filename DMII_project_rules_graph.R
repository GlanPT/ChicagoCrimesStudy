library(tidyverse)
library(arules)
library(arulesViz)
library(igraph)

## -- Reading and basic cleaning -------

# Read the dataset
chicago_crimes <- read_csv("data_clean.csv")

class(chicago_crimes)

summary(chicago_crimes)

df <- chicago_crimes %>% select(7,9,10,23,25)
#df <- df %>% filter('Location Descrition' != "STREET") 
df <- df %>% mutate_if(is.character,as.factor)



dfT <- as(df,"transactions")
item_dfT <- itemInfo(dfT)
head(item_dfT)

rules <- apriori(dfT,parameter=list(supp=0.01,conf=0.5))

plot(rules)

sorted_rules <- sort(rules, by="support", decreasing=TRUE)

inspect(sorted_rules[1:31])





# Set the support threshold
support_threshold <- 0.015

# Filter rules based on support
filtered_rules <- subset(sorted_rules, subset = support > support_threshold)

# Plot the filtered rules
plot(filtered_rules, method = "graph",shading = "lift", control = list(type = "items"))
plot(filtered_rules,method = "graph",
     control = list(
       edges = ggraph::geom_edge_link(
         end_cap = ggraph::circle(4, "mm"),
         start_cap = ggraph::circle(4, "mm"),
         color = "black",
         arrow = arrow(length = unit(2, "mm"), angle = 20, type = "closed"),
         alpha = .2
       ),
       nodes = ggraph::geom_node_point(aes_string(size = "support", color = "lift")),
       nodetext = ggraph::geom_node_label(aes_string(label = "label"), alpha = .8, repel = TRUE)
     ))
#Link analysis
graph <- associations2igraph(filtered_rules,associationsAsNodes = TRUE)
V(graph)
E(graph)
graph[]
betweenness(graph)
degree(graph)
closeness(graph)
tkplot(graph)

#inspect(subset(rules,support>0.015))

#plot(rules, method="graph")

#rul <- subset(rules,support>0.015)

#plot(rul, method="graph",control=list(type="itemsets"))
