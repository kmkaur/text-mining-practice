#make networks from processed data frames

#load packages
require(bipartite)
require(cowplot)
require(ggplot2)
require(network)
require(igraph)
require(sna)
require(ggnet)
require(ergm)
require(intergraph)
require(RColorBrewer)
require(ggbipart) #need to download the zip file online or source the functions below

#####functions to make network figures#####
#these are written by Pedro Jordano
bip_init_network <- function (mymat, mode1="P", mode2="A") {
  require(network)
  require(ggnet)
  if(!is.matrix(mymat)) mymat <- as.matrix(mymat)
  p<- dim(mymat)[1]    # Plants are rows
  a<- dim(mymat)[2]    # Animals are columns
  net<- network::network(mymat,
                         matrix.type = "bipartite",
                         ignore.eval = FALSE,
                         names.eval = "weights")
  net
  network::set.vertex.attribute(net,"mode",c(rep(mode1,p), rep(mode2,a)))
}

bip_edgewt<- function(M, x = 30) {
  if(!is.matrix(M)) as.matrix(M)
  # Transpose.
  M <- t(M)
  # Edge list and weights.
  M <- cbind(expand.grid(dimnames(M))[2:1], as.vector(M))
  # Discard null weights.
  M <- subset(M, M[, 3] != 0)
  # Scaled weights.
  M.scaled <- x*log(M[, 3] + 1) / max(log(M[, 3] + 1))
  # Vector of edge weights.
  return(M.scaled) # A numeric vector with scaled edge lengths.
}

bip_ggnet<- function(net, mat, mode= "fruchtermanreingold", size= 9,
                     palette= col, color= "mode",
                     label.size=3, label= F, shape= "mode",
                     edge.label = NULL,
                     layout.exp= 0)
{
  #    source("./R/bip_edgewt.R")
  if(!is.network(net)) stop("Must first initialize the network; use 'bip_init_network.R'.")
  #
  # Set colors for each mode to setup a palette.
  col= c("A"= "grey", "P"= "gold")
  
  pp<- ggnet2(net,
              shape= shape,                       label= label,
              color= color,                        palette= palette,
              size = size,                         legend.size = 9,
              mode = mode,                         label.size= 4,
              layout.par = NULL,                   layout.exp = layout.exp,
              size.legend = NA,                    label.trim = FALSE,
              edge.lty = "solid",                  edge.label = edge.label,
              edge.size= bip_edgewt(mat, 5),       edge.alpha= 0.25)
  return(pp)
}
####################

setwd("~/Desktop")

######import SD network#####
sd_net <- read.csv(file = "sd_network_for_bp.csv")

#convert to matrix to use in bipartite
sd_net <- as.matrix(sd_net)
dimnames(sd_net) <- list(1:86, 1:102) #req'd for bipartite functions

#for these functions, the "network" is just a matrix
#these are various network measures
gl_sd <- grouplevel(sd_net)
nest_sd <- nested(sd_net, method = "NODF") #use nested not nestedness fxn, different methods
cscore_sd <- C.score(sd_net, normalise = FALSE)
laws_sd <- degreedistr(sd_net)
fcscore_sd <- fc(sd_net)
nl_sd <- networklevel(sd_net, ISAmethod = "Bascompte")
pdiscore_sd <- PDI(sd_net)
plotweb(sd_net)
Barrat_hl <- bipartite::strength(t(sd_net), type = "Barrat")
Bascompte_hl <- bipartite::strength(t(sd_net), type = "Bascompte")
Barrat_ll <- bipartite::strength(sd_net, type="Barrat")
Bascompte_ll <- bipartite::strength(sd_net, type = "Bascompte")
cor.test(Bascompte_hl, Barrat_hl)
cor.test(Bascompte_ll, Barrat_ll)
visweb(sd_net)
sd_net_mods <- computeModules(sd_net)
mod_deg_and_par <- czvalues(sd_net_mods, weighted=FALSE, level="higher")
sl_sd <- specieslevel(sd_net)
dependences_sd <- linklevel(sd_net, index = "dependence")
path_lengths_sd <- nodespec(sd_net)

#plots
freq_p <- qplot(sl_sd$`higher level`$degree, geom="histogram", binwidth = 0.5, xlab = "k - Plants", ylab = "Frequency") 
freq_a <- qplot(sl_sd$`lower level`$degree, geom="histogram", binwidth = 0.5, xlab = "k - Ants", ylab = "Frequency") 
plot_grid(freq_p, freq_a)
laws <- degreedistr(sd_net)
visweb(sd_net, type="nested",  prednames=FALSE, preynames=FALSE,
       plotsize=20)
degree_strength_pbas <- as.data.frame(cbind(sl_sd$`higher level`$degree,strong_Barrat_hl))
degree_strength_abas <-as.data.frame(cbind(sl_sd$`lower level`$degree,strong_Barrat_ll))

degree_strength_pbas_plot <- ggplot(data = degree_strength_pbas, aes(x=V1, y=strong_Barrat_hl)) + geom_point() + labs(x="Plant Species Degree", y="Plant Species Strength")
degree_strength_pbas_plot <- ggplot(data = degree_strength_abas, aes(x=V1, y=strong_Barrat_ll)) + geom_point() + labs(x="Animal Species Degree", y="Animal Species Strength")
plot_grid(degree_strength_pbas_plot, degree_strength_abas_plot)
par(mfrow=c(2,2))
deps_plot1 <- plot(dependences$`LL dependence`,dependences$`HL dependence`, xlab = "Dependence of the animal", ylab="Dependence of the plant", pch=19)
deps_plot2 <- plot(dependences$`LL dependence`,dependences$`HL dependence`, xlab = "Dependence of the animal", ylab="Dependence of the plant", pch=19)
#plot(Bascompte_hl, Barrat_hl, log = "x")
#plot(Bascompte_ll, Barrat_ll, log = "x")

#net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 

##these are the nice plots of the whole network##
bip.net<- bip_init_network(as.matrix(sd_net)) 
pp2<- bip_ggnet(bip.net, as.matrix(bip.net),
                size= 0,
                shape= "mode", 
                palette= "Set1",
                color= "mode",
                layout.exp= 0.25) +
  geom_point(aes(color= color), size=8, color="white") +
  geom_point(aes(color= color), size= 8, alpha= 0.5) +
  geom_point(aes(color= color), size= 6) +
  geom_text(aes(label= network.vertex.names(bip.net)), 
            color= "black", size= 3.5) + # check_overlap= TRUE
  guides(color= FALSE) +
  theme(legend.position="none")          # Hide legend
pp2

nums<- as.vector(c(1:sum(dim(sd_net))))
bip.net<- bip_init_network(as.matrix(sd_net)) 
pp3<- bip_ggnet(bip.net, as.matrix(bip.net),
                size= 0,
                shape= "mode", 
                palette= "Set1",
                color= "mode",
                layout.exp = 0.25) +
  geom_point(aes(color= color), size=10, color="white") +
  geom_point(aes(color= color), size= 10, alpha= 0.5) +
  geom_point(aes(color= color), size= 8) +
  geom_text(aes(label= nums), 
            color= "white", size= 3.5, fontface="bold") + 
  guides(color= FALSE) +
  theme(legend.position="none")          # Hide legend
pp3

col = c("P" = "mediumseagreen", "A" = "burlywood4")

bip.net2<- bip_init_network(as.matrix(sd_net)) 
pp2<- bip_ggnet(bip.net2, as.matrix(bip.net2),
                size= 0,
                shape= "mode", 
                palette= col,
                color= "mode",
                layout.exp= 0.25) +
  geom_point(aes(color= color), size=6, color="white") +
  geom_point(aes(color= color), size= 6, alpha= 0.2) +
  geom_point(aes(color= color), size= 4) +
  #geom_text(aes(label= network.vertex.names(bip.net2)), 
  #          color= "black", size= 3.5) + # check_overlap= TRUE
  guides(color= FALSE) +
  theme(legend.position="none")          # Hide legend
pp2

nums<- as.vector(c(1:sum(dim(efn_net))))
bip.net2<- bip_init_network(as.matrix(efn_net)) 
pp3<- bip_ggnet(bip.net2, as.matrix(bip.net2),
                size= 0,
                shape= "mode", 
                palette= "Set2",
                color= "mode",
                layout.exp = 0.25) +
  geom_point(aes(color= color), size=10, color="white") +
  geom_point(aes(color= color), size= 10, alpha= 0.5) +
  geom_point(aes(color= color), size= 8) +
  geom_text(aes(label= nums), 
            color= "black", size= 3.5, fontface="bold") + 
  guides(color= FALSE) +
  theme(legend.position="none")          # Hide legend
pp3

#####import EFN network#####

#####import Dom network#####


