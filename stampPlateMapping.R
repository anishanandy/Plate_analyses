install.packages("virdis")
install.packages("ggplot2")
install.packages("hrbrthemes")
# library
    library(ggplot2)
    library(viridis)
    library(hrbrthemes)
    
    # create a dataset
    list = read.csv("compoundConcentrationTallycopy.csv", header = TRUE)
    
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
  ggplot(list1, aes(compound.concentration, Compound.name, fill= phage.dilution)) + 
    geom_tile(
      colour = "white",position="identity")  +
    theme(text = element_text(size=5))+
      scale_fill_gradient2(low="#ef8a62", mid="#f7f7f7", high="#67a9cf", midpoint=2, limits=c(1,5)) +
    scale_x_discrete(labels=c("1.5","3.25","6.25","12.5","25")) + 
    coord_fixed(ratio = 0.3) 
  
  # Heatmap 
  ggplot(list2, aes(compound.concentration, Compound.name, fill= phage.dilution)) + 
    geom_tile(
      colour = "white",position="identity")  +
    theme(text = element_text(size=5))+
    scale_fill_gradient2(low="#ef8a62", mid="#f7f7f7", high="#67a9cf", midpoint=2, limits=c(1,5)) +
    scale_x_discrete(labels=c("1.5","3.25","6.25","12.5","25")) + 
    coord_fixed(ratio = 0.3) 
