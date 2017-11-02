library(ggplot2)
library(cowplot)
data = read.csv("lipinski_results.csv", header = TRUE)
p <- ggplot(data, aes(MW, ALogP))
p <- p + geom_point(aes(colour = factor(Activity)), size = 5, alpha = 0.3)+ 
  geom_point(shape = 1,size = 5,colour = "lavenderblush4")
p <- p + theme(aspect.ratio=1, legend.position = ("none"),
               panel.border = element_rect(linetype = "solid",
                                           colour = "black", fill = NA, size = 1),
               axis.text.x = element_text(colour = "black", size = 15),
               axis.text.y = element_text(colour = "black", size = 15),
               plot.margin = unit(c(1, 1, 1, 1), "cm"),
               axis.title.x = element_text(colour = "black", size = 18, face = "bold", margin = margin(t=15)),
               axis.title.y = element_text(colour = "black", size = 18, face = "bold", margin = margin(r=15))
)
print(p)