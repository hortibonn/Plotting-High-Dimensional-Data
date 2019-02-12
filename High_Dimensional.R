#Multidimensional plotting in ggplt2
#https://stackoverflow.com/questions/23199416/5-dimensional-plot-in-r

#all continuous####

df <- data.frame(replicate(4,sample(1:200,1000,rep=TRUE)))
addme <- data.frame(replicate(1,sample(0:1,1000,rep=TRUE)))
df <- cbind(df,addme)
colnames(df) <- c("var1","var2","var3","var4","var5")

library(ggplot2)

ggplot(df, aes(x=var1, y=var2, fill=var3, color=var4, size=var5)) +
  geom_point(shape=21) +
  #scale_color_gradient(low="grey", high="black") +
  #scale_color_gradientn(colours = rainbow(3))+
  scale_size_continuous(range=c(1,12))+ 
  scale_fill_gradient(low="grey", high="black")
  #scale_fill_gradientn(colours = rainbow(3))

#continuous and categorical####
#use facet_grid
#var3 and var4 as quintiles 

df$var4.cat <- cut(df$var4, quantile(df$var4, (0:5)/5), include.lowest=T)
df$var3.cat <- cut(df$var3, quantile(df$var3, (0:5)/5), include.lowest=T)

ggplot(df, aes(x=var1, y=var2, fill=var4, color=var3, size=var5^2)) +
  geom_point(shape=21) +
  scale_color_gradient(low="red", high="green") +
  scale_size_continuous(range=c(1,12)) +
  facet_grid(var3.cat ~ var4.cat)
