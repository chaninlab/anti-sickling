data = read.csv("Gini_SubstructureFingerprintCount.csv", header = TRUE)     
data_melt1 <- melt(data[1:20,], id.vars = "feature")
data_melt1$feature <- factor(data_melt1$feature)
set.seed(10)
plot <- ggplot(data_melt1, aes(x = reorder(feature, value, FUN = mean), y = value)) +
  geom_boxplot(fill = "green", colour = "black", alpha = 0.5) +
  theme_bw() + xlab("") + ylab("Gini index") + coord_flip() + theme(
    axis.text.y = element_text(size = 20, colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black"),
    panel.border = element_rect(linetype = "solid", colour = "black", fill = NA, size = 1),
    axis.title.x = element_text(size = 25, face = "bold", colour = "black", margin = margin(t=15))
  )
print(plot)