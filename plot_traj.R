library(monocle)

load('Uso_filter10_seed100_iter10000.Rdata')




binary = (resim$x)
dim(binary)


binary.pca = prcomp(binary, center = FALSE, scale. = FALSE)

plot(binary.pca, type = "l")

scores = data.frame(binary.pca$x)

dim(scores)


cols=ncol(scores)
cells=rownames(scores)#gsub("_.*", "", rownames(scores))
scores=data.frame(scores,cells)
colnames(scores)[cols+1]="cell"


ct_cluster = resim$ac

re_cell <- as.factor(resim$ac)
colors = rainbow(length(unique(re_cell)))
names(colors) = unique(re_cell)

color_ct = c()
for (i in c(1:length(re_cell))){
color_ct[i] = colors[as.numeric(re_cell[i])]
}

cell_cluster <- read.table(file = "Usoskin_clu_t.txt")

re_cell_od <- as.factor(cell_cluster)
colors_od = rainbow(11)
names(colors_od) = unique(re_cell_od)

color_ct_od = c()
for (i in c(1:dim(cell_cluster)[1])){
color_ct_od[i] = colors_od[as.numeric(cell_cluster[i,1])]
}

id_color = uniq(cbind(cell_cluster, color_ct))

print(binary.pca)

par(mfrow=c(2,3))

### predicted label
plot(scores$PC1, scores$PC2, pch=16, col = color_ct)
plot(scores$PC2, scores$PC3, pch=16, col = color_ct)
plot(scores$PC3, scores$PC4, pch=16, col = color_ct)

### real label
plot(scores$PC1, scores$PC2, pch=16, col = color_ct_od)
plot(scores$PC2, scores$PC3, pch=16, col = color_ct_od)
plot(scores$PC3, scores$PC4, pch=16, col = color_ct_od)


### plot label vs color
par(mfrow=c(1,1))
plot(c(1:11) , rep(0,11), col=colors_od)
