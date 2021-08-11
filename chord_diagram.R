library(viridis)
library(colormap)

points <- read.csv("Data/point_diff_2019_updated.csv", header = TRUE)

test <- as.matrix(points)
transTest<- t(test)
int <- transTest %*% test
finalInt <- int[3:(nrow(int) - 1), 3:(ncol(int) - 1)]

player2019 <- read.csv("Data/Players_2019.csv")

# Load the circlize library
library(circlize)

# Make the circular plot
mycolor <- viridis(13, alpha = 1, begin = 0, end = 1, option = "D")
mycolor <- mycolor[sample(1:13)]
diag(finalInt) <- 0
finalInt <- data.frame(finalInt)
copy <- player2019 %>% dplyr::select(Player, Tm) %>% bind_cols(finalInt)
test <- copy %>% filter(Tm == "LVA")
graph <- test[,names(test) %in% rownames(test)] %>% as.matrix()
chordDiagram(graph, transparency = 0.2, grid.col = mycolor)
finalInt
diag(finalInt)
copy <- finalInt


# devtools::install_github("mattflor/chorddiag")
library(chorddiag)

groupColors <- mycolor
cols <- c(rep("#808080", 12), "#ff9900")
# Build the chord diagram:
p <- chorddiag(graph, groupColors = cols, groupnamePadding = 10, showTicks = FALSE, groupnameFontsize = 14)
p

