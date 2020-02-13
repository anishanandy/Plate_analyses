install.packages("virdis")
install.packages("ggplot2")
install.packages("hrbrthemes")
# library
    library(ggplot2)
    library(viridis)
    library(hrbrthemes)
    
    # create a dataset
    list = read.csv("actives_to_hits(nonSOS).csv", header = TRUE)
    list2 = read.csv("Dopaminehits.csv", header = TRUE)
    list3 = read.csv("Serotoninhits.csv", header = TRUE)
    # # Small multiple
    # ggplot(data, aes(fill=condition, y=value, x=specie)) + 
    #   geom_bar(position="stack", stat="identity") +
    #   scale_fill_viridis(discrete = T) +
    #   ggtitle("Studying 4 species..") +
    #   xlab("")
    # 
    # # Small multiple
    # ggplot(list, aes(fill=name, y=Total, x=Compound.name)) +
    #   geom_bar(position="stack", stat="identity") +
    #   scale_fill_viridis(discrete = FALSE) +
    #   xlab("")
    # 
    # ggplot() +
    #   geom_bar(data=list, aes(y = Total, x = Compound.name), width = 0.3, stat="identity",
    #            position='stack') +
    #   coord_flip() +
    #   facet_grid( name ~ .)
    # scale_fill_viridis()
    # 
    # ggplot() +
    #   geom_bar(data=list, aes(y = Total, x = Compound.name), width = 0.3, stat="identity",
    #            position='stack') +
    #   facet_grid( name ~ .)
    #   scale_fill_viridis()
    #   
    # list2 = list[list$name == 'one',]
    # theme_set(theme_gray(base_size = 7))
    # ggplot() +
    #   geom_bar(data=list2, aes(y = Total, x = Compound.name), width = 0.3, stat="identity",
    #            position='stack') +
    #   coord_flip() +
    #   scale_fill_viridis()
    # Dummy data
    # x <- LETTERS[1:20]
    # y <- paste0("var", seq(1,20))
    # data <- expand.grid(X=x, Y=y)
    # data$Z <- runif(400, 0, 5)
  ind <- seq(190) 
  ind2 <- seq(416,860)
  list1 <- list[ind, ]
  list2 <- list[ind2,]
    # Heatmap 
  ggplot(list, aes(compound.concentration, Compound.name, fill= phage.dilution)) + 
    geom_tile(
      colour = "white",position="identity")  +
    theme(text = element_text(size=5))+
      scale_fill_gradient2(low="#ef8a62", high="#67a9cf", limits=c(4,5)) +
    scale_x_discrete(labels=c("1.5","3.25","6.25","12.5","25")) + 
    coord_fixed(ratio = 0.3) 
  
  
  #edited heatmap for hits
 p <- ggplot(list, aes(compound.concentration, Compound.name, fill= phage.dilution)) + 
    geom_tile(
      colour = "white",position="identity")  +
    theme(text = element_text(size=5))+
    #scale_fill_manual(drop=FALSE, values=colorRampPalette(c("white","red"))(5), na.value="#EEEEEE", name="phage.dilution") +
   scale_fill_gradient( low="orange", high="orange3" , limits = c(3,5)) +
    scale_x_discrete(labels=c("1.5","3.25","6.25","12.5","25")) + 
   scale_y_discrete(labels=NULL) +
    #coord_fixed(ratio = 0.3) +
   
 theme_test() +
   theme(axis.ticks.x = element_blank(), axis.title.x = element_blank())
 
 p + coord_flip()  #flips co-ordinates
  # theme(axis.text.x = element_text(angle = 0, hjust = 1)) #changes the angle of the axis label
 
 
q <- ggplot(list2, aes(compound.concentration, Compound.name, fill= phage.dilution)) + 
   geom_tile(
     colour = "white",position="identity")  +
   theme(text = element_text(size=5))+
   #scale_fill_manual(drop=FALSE, values=colorRampPalette(c("white","red"))(5), na.value="#EEEEEE", name="phage.dilution") +
   scale_fill_gradient( low="skyblue", high="deepskyblue3" , limits = c(3,5)) +
   scale_x_discrete(labels=c("1.5","3.25","6.25","12.5","25")) + 
   scale_y_discrete(labels=NULL) +
   #coord_fixed(ratio = 0.3) +
   
   theme_test() +
   theme(axis.ticks.x = element_blank(), axis.title.x = element_blank())
 
 q + coord_flip()  #flips co-ordinates
   # theme(axis.text.x = element_text(angle = 0, hjust = 1)) #changes the angle of the axis label
   
   
 r <- ggplot(list3, aes(compound.concentration, Compound.name, fill= phage.dilution)) + 
   geom_tile(
     colour = "white",position="identity")  +
   theme(text = element_text(size=5))+
   #scale_fill_manual(drop=FALSE, values=colorRampPalette(c("white","red"))(5), na.value="#EEEEEE", name="phage.dilution") +
   scale_fill_gradient( low="mediumpurple3", high="mediumpurple4" , limits = c(3,5)) +
   scale_x_discrete(labels=c("1.5","3.25","6.25","12.5","25")) + 
   scale_y_discrete(labels=NULL) +
   #coord_fixed(ratio = 0.3) +
   
   theme_test() +
   theme(axis.ticks.x = element_blank(), axis.title.x = element_blank())
 
 r + coord_flip()  #flips co-ordinates
   # theme(axis.text.x = element_text(angle = 0, hjust = 1)) #changes the angle of the axis label
   
    #theme_ipsum()
   

 base_size <- 9
 p + theme_grey(base_size = base_size) + 
   labs(x = "",
     y = "") + scale_x_discrete(expand = c(0, 0)) +
       scale_y_discrete(expand = c(0, 0)) + opts(legend.position = "none",
           axis.ticks = theme_blank(), axis.text.x = theme_text(size = base_size *
          0.8, angle = 330, hjust = 0, colour = "grey50"))
  
  # Heatmap 
  ggplot(list2, aes(compound.concentration, Compound.name, fill= phage.dilution)) + 
    geom_tile(
      colour = "white",position="identity")  +
    theme(text = element_text(size=5))+
    scale_fill_gradient2(low="#ef8a62", mid="#f7f7f7", high="#67a9cf", midpoint=2, limits=c(1,5)) +
    scale_x_discrete(labels=c("1.5","3.25","6.25","12.5","25")) + 
    coord_fixed(ratio = 0.3) 
