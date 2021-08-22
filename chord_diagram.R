library(viridis)
library(colormap)
library(tidyverse)

points <- read.csv("Data/point_diff_2019_updated.csv", header = TRUE)

test <- as.matrix(points)
transTest<- t(test)
int <- transTest %*% test
finalInt <- int[3:(nrow(int) - 1), 3:(ncol(int) - 1)]

player2019 <- read.csv("Data/Players_2019.csv") %>% arrange(Player)

# Load the circlize library
library(circlize)

# Make the circular plot
mycolor <- viridis(13, alpha = 1, begin = 0, end = 1, option = "D")
mycolor <- mycolor[sample(1:13)]
diag(finalInt) <- 0
finalInt <- data.frame(finalInt)
copy <- player2019 %>% dplyr::select(Player, Tm) %>% bind_cols(finalInt)
rownames(copy) <- copy$Player
rownames(graph) <- graph$Player
names(copy) <- c("Player", "Tm", rownames(copy))
graph <- as.matrix(graph)
chordDiagram(graph, transparency = 0.2, grid.col = mycolor)
finalInt
diag(finalInt)
copy <- finalInt


# devtools::install_github("mattflor/chorddiag")
library(chorddiag)

groupColors <- mycolor
cols <- c(rep("#808080", 12), "#ff9900")
# Build the chord diagram:
p <- chorddiag(graph, groupColors = cols, groupnamePadding = 10, showTicks = FALSE, groupnameFontsize = 12)
p
?chorddiag()
head(copy)
ind_remove <- which(copy$Tm == "TOT")
copy2 <- copy %>% dplyr::select(-Player, -Tm)
copy2 <- copy2[-ind_remove, -ind_remove]
copy2$Player <- player2019$Player[-ind_remove]
copy2$Tm <- player2019$Tm[-ind_remove]
write.csv(copy2, "Data/possessions_player.csv", row.names = FALSE)
head(copy2)
