

library(tidygraph)
library(ggraph)
library(igraph)
library(scales)
library(dplyr)
library(lubridate)
library(circlize)
library(plotrix)
library(ggforce)
library(svglite)
library(knitr)
library(kableExtra)
library(gridExtra)
library(tidyr)
library(purrr)
library(RColorBrewer)


dat=readRDS( "Data/Cleaned_dataframe/coronavirus_host_data_Feb_12_2022.RDS")



dat$CoV=NA

dat[is.na(dat$species),]$species="Unknown"

dat=unique(dat[, c("CoV", "CoV_genus", "subgenus", "class", "order", "family", "species")])

dat[is.na(dat$CoV_genus),]$CoV_genus="Unknown"

dat.not.unknown=dat[dat$species!="Unknown",]



# BUILD DATA FOR PLOT  

# --------------- #
# ----- data ---- #
# ----------------#

# data receptor

dat.graph.2=vector(mode = "list", length = 2)

names(dat.graph.2) = c("edges", "vertices")


# number of host species by levels

l0=dat.not.unknown %>%dplyr::count(CoV_genus) 

l1=dat.not.unknown %>% dplyr::count(CoV_genus, subgenus) 

l2=dat.not.unknown %>% dplyr::count(CoV_genus, subgenus, class) 

l3=dat.not.unknown %>% dplyr::count(CoV_genus, subgenus, class, order)

l4=dat.not.unknown %>% dplyr::count(CoV_genus, subgenus, class, order, family) 


# all(c(sum(l0$n), sum(l1$n),sum(l2$n), sum(l3$n), sum(l4$n))==nrow(gb.not.unknown))


# ---------------- #
# ----- edges ---- #
# -----------------#

l0.e=cbind("CoV", l0)

l1.e=l1%>%unite(subgenus, c(CoV_genus,subgenus), sep = ".", remove = FALSE)

l2.e=l2%>%unite(class, c(CoV_genus,subgenus, class), sep = ".", remove = FALSE)

l3.e=l3%>%unite(order, c(CoV_genus,subgenus, class, order), sep = ".", remove = FALSE)

l4.e=l4%>%unite(to, c(CoV_genus,subgenus, class, order, family), sep = ".", remove = FALSE)

l4.e=l4.e%>%unite(from, c(CoV_genus,subgenus, class, order), sep = ".", remove = FALSE)


#fill the data receptor with the edges data

dat.graph.2$edges=data.frame(from=
                              unlist(
                              c(l0.e[,1],
                                l1.e$CoV_genus,
                                paste(l2.e$CoV_genus, l2.e$subgenus, sep = "."),
                                paste(l3.e$CoV_genus, l3.e$subgenus, l3.e$class, sep = "."),
                                l4.e$from)),
                            
                            to=c(l0.e$CoV_genus,
                                 l1.e$subgenus,
                                 l2.e$class,
                                 l3.e$order,
                                 l4.e$to))

dat.graph.2$edges$size=c(l0$n, l1.e$n, l2.e$n, l3.e$n, l4.e$n)


# ------------------- #
# ----- vertices ---- #
# --------------------#

dat.graph.2$vertices=data.frame( 
                                 name=c("CoV", dat.graph.2$edges$to),
                                 size=0)


dat.graph.2$vertices$group = dat.graph.2$edges$from[match( dat.graph.2$vertices$name, dat.graph.2$edges$to ) ]

dat.graph.2$vertices$shortname=sapply(strsplit(as.character(dat.graph.2$vertices$name), "[.]"), function(x) tail(x, 1))

dat.graph.2$vertices$shortname[1]="Coronaviridae"

families <- which(is.na( match(dat.graph.2$vertices$name, dat.graph.2$edges$from) )) #identify families

nfamilies <- length(families)

dat.graph.2$vertices$label <- NA

dat.graph.2$vertices$label[ families ] <- paste0(l4.e$family, " (n=", l4.e$n, ")") # adding the family label and the number of species per family


# -----------------#
# -- DENDROGRAM -- #
# -----------------#

# create the graph

graph <- tbl_graph(dat.graph.2$vertices, dat.graph.2$edges)


# set parameters

# names.circles.cofge.per.cov.family<-map(c("alpha", "beta", "delta", "gamma"), paste0, c(1:4))
# 
# diameters=c(0.616, 0.9945, 1.4444, 1.9) # these are try and error
# 
# starts=c(-2.12, -2.491, -3.465, -3.702) # these are try and error
# 
# end=c(-2.491, -3.465, -3.702, -4.12)  # these are try and error
# 
# 
# circles.alpha= seq(0.3, 0.2, length.out = 4)# these are the values for the alpha colors of the CoV subgenus, and host's class, order, and family.
# 
colors.branches=c("#ACA483", "skyblue2", "darkgreen", "#D6B0C8", "#C7D1D1")

# the color pallets to label the host families per each CoV genus

colors.for.family.labels<-list("Reds"= colorRampPalette(c("indianred1", "red4")),
                               "Blues"= colorRampPalette(c("royalblue1", "navyblue")),
                               "Greens" = colorRampPalette(c("seagreen3", "darkgreen")),
                               "Purples" = colorRampPalette(c("orchid1", "purple4")),
                               "Greys" = colorRampPalette(c("grey60", "black")))



# preparing the circles

# function to create the circles

# circleFun <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2, filled=TRUE){
#   tt <- seq(start*pi, end*pi, length.out=npoints)
#   df <- data.frame(
#     x = center[1] + diameter / 2 * cos(tt),
#     y = center[2] + diameter / 2 * sin(tt)
#   )
#   if(filled==TRUE) { #add a point at the center so the whole 'pie slice' is filled
#     df <- rbind(df, center)
#   }
#   return(df)
# }
# 
# 
# 
# test<-map(c(1:4), function(x)
#   map(1:length(names.circles.cofge.per.cov.family), function(y)
#     circleFun(c(0,0), diameter = diameters[x], start=starts[y], end=end[y], filled=TRUE)))
# 
# test<-map(1:length(test), function(x) setNames(test[[x]], unlist(map(names.circles.cofge.per.cov.family, x))))
# 
# test<-map(1:length(test), function(x) mapply(cbind, test[[x]], "alpha"=circles.alpha[x], SIMPLIFY=F)) # adding the correct alpha values to each level for each CoV genus
# 
# test<-map(1:length(test), function(x) sapply(test, function(y) y[x]))
# 
# test<-map(1:length(test), function(x) mapply(cbind, test[[x]], "fill"=colors.branches[x], SIMPLIFY=F)) # adding the correct alpha values to each level for each CoV genus
# 
# test<-unlist(test, recursive = F)[order(names(unlist(test, recursive = F)))]





# -----------------#
# -- DENDROGRAM -- #
# -----------------#


dendrogram=
  
ggraph(graph, layout = 'dendrogram', circular = TRUE )  +  
  
  # coord_fixed() +
  
  scale_edge_colour_distiller(palette = "RdPu") +
  
  # branches
  
  geom_edge_elbow2(aes(edge_width= size),
                   
                   colour=c(#21700 colors
                    
                      #CoV Genus

                     unlist(mapply(function(x,y) rep(x,y),  
                                   colors.branches,
                                   l0%>%ungroup() %>%distinct(CoV_genus)%>%dplyr::count(CoV_genus)%>%pull(n)*100), use.names = F),
                     
                     
                     # CoV subgenus
                     
                     unlist(mapply(function(x,y) rep(x,y),  
                                   colors.branches,
                                   l1%>%ungroup() %>%distinct(CoV_genus, subgenus)%>%dplyr::count(CoV_genus)%>%pull(n)*100), use.names = F),
                   
                     
                     # Class
                     
                     unlist(mapply(function(x,y) rep(x,y),  
                                   colors.branches,
                                   l2%>%ungroup() %>%distinct(CoV_genus, subgenus, class)%>%dplyr::count(CoV_genus)%>%pull(n)*100), use.names = F),
                   
                     
                     # Order
                     
                     unlist(mapply(function(x,y) rep(x,y),  
                                   colors.branches,
                                   l3%>%ungroup() %>%distinct(CoV_genus, subgenus, class, order)%>%dplyr::count(CoV_genus)%>%pull(n)*100), use.names = F),
                   
                     
                     
                     #Family
                     
                     unlist(mapply(function(x,y) rep(x,y),  
                                   colors.branches,
                                   l4%>%ungroup() %>%distinct(CoV_genus, subgenus, class, order, family)%>%dplyr::count(CoV_genus)%>%pull(n)*100), use.names = F)),
                     
                   
                   alpha=1, 
                   linejoin = "bevel", 
                   lineend = "butt", 
                   edge_linetype=1) 
  
  
  # Add circles per Cov genera and subgenera, and host class, order, and family
  
  # #alpha
  # 
  # for(i in which(grepl(pattern = "alpha", names(test)))){
  # 
  # dendrogram<-dendrogram+geom_polygon(data=test[[i]], aes(x,y), alpha=test[[i]]$alpha, fill=test[[i]]$fill)}
  # 
  # 
  # 
  # # # # beta
  # # 
  # for(i in which(grepl(pattern = "beta", names(test)))){
  # 
  # dendrogram<-dendrogram+geom_polygon(data=test[[i]], aes(x,y), alpha=test[[i]]$alpha, fill=test[[i]]$fill)}
  # 
  # # 
  # # # delta=
  # # 
  # for(i in which(grepl(pattern = "delta", names(test)))){
  # 
  # dendrogram<-dendrogram+geom_polygon(data=test[[i]], aes(x,y), alpha=test[[i]]$alpha, fill=test[[i]]$fill)}
  # 
  # # # # gamma=
  # # 
  # for(i in which(grepl(pattern = "gamma", names(test)))){
  # 
  # dendrogram<-dendrogram+geom_polygon(data=test[[i]], aes(x,y), alpha=test[[i]]$alpha, fill=test[[i]]$fill)}
  # 
  # 
  # 
  # 
  
  # adding the white circle at the center
  dendrogram <- dendrogram + geom_circle(aes(x0 = 0, y0 = 0, r = 0.185*0.65), fill="white", colour="white")

  
  
  # family labels position
  
  #find nearest point to the 0,-1 point (rect angle from 0, 0)
  low.pos <- c(0, -1) # New position
  
  # Compute distance to points and select nearest index
  nearest.idx <- which.min(colSums((t(dendrogram$data[,c("x","y")]) - low.pos)^2)) +1 # add one because starts to count after the first beyond -90
  
  # the sorted indexes of the families
  index.families.based.on.plot.position=c(nearest.idx:nrow(dendrogram$data), families[1]:c(nearest.idx-1)) # the first of the list is the one closer to the (0,-1) position
  
  # adding an id variable that starts with the closest family  to (0,-1)
  dendrogram$data$id <- NA

  dendrogram$data$id[index.families.based.on.plot.position]<-seq(1:nfamilies)
    
  # the angle values
  dendrogram$data$angle <- 90 - 360 * dendrogram$data$id / nfamilies
  
  # calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90
  dendrogram$data$hjust <- ifelse(dendrogram$data$angle < -90, 0, 1)
  
  # flip angle BY to make them readable
  dendrogram$data$angle <- ifelse(dendrogram$data$angle < -90, dendrogram$data$angle+180, dendrogram$data$angle)
  
  
  
  
  # family labels colors
  
  # the host orders per CoV genus
  host.orders.per.cov.genus=dat.not.unknown %>% distinct(CoV_genus, subgenus, class, order) %>% group_by(CoV_genus) %>%dplyr::count() %>% pull(n)                                            
  
  # the host families per order per CoV genus
  host.families.per.order.per.cov.genus=map(sort(unique(dat.not.unknown$CoV_genus)), function(x)
    dat.not.unknown %>% 
      distinct(CoV_genus, subgenus, class, order, family) %>% 
      dplyr::count(CoV_genus, subgenus, class, order) %>%
      filter(CoV_genus==x)%>%pull(n))
  
   
  #the set of colors for all the family labels
  colors.for.family.labels.per.cov.genus=vector(mode = "list", length(host.orders.per.cov.genus))
  
  #sample the colors per order per CoV genus
  for(i in 1:length(host.orders.per.cov.genus)){
    colors.for.family.labels.per.cov.genus[[i]]<-unlist(mapply(FUN = function(x,y) rep(x,y),
                                                               sample(colors.for.family.labels[[i]](host.orders.per.cov.genus[i])),
                                                               host.families.per.order.per.cov.genus[[i]]), use.names = F)}
  
  
  
  # Add the labels per family colored by order within each  CoV genus
  dendrogram <- dendrogram + 
    
    geom_node_text(aes(x = x*1.05, y=y*1.05, filter = leaf, label=label, angle = angle, hjust=hjust),	
                   color=unlist(colors.for.family.labels.per.cov.genus), 
                   size=2.5,
                   alpha=1)
  
  
  # Add alpha, beta, delta, gamma labels and the 1:4 to show the coV subgenues, host class, host order, host family levels
  
  # dendrogram <- dendrogram +
  
  # geom_text(x=0.4, y= -0.37, label="\u03b1", colour="red", size=6.5) +
  # 
  # geom_text(x=-0.52, y=-0.03, label="\u03b2", colour="navy", size=6.5) +
  # 
  # geom_text(x=0.08, y=0.54, label="\u03b4", colour="seagreen", size=6.5) +
  # 
  # geom_text(x=0.53, y=0.07, label="\u03b3", colour="purple", size=6.5)  +
  # 
  #   # gb.not.unknown%>%
  #   # filter(CoV_genus=="Betacoronavirus")%>%
  #   # distinct(subgenus, class, order, family, species)%>%
  #   # dplyr::count(subgenus, class, order, family)
  # 
  #   
  # geom_label(size = 2, label="E", x=-0.165, y=-0.39, label.size = 0.18, color = "black",fill="white") +
  # 
  # geom_label(size = 2, label="H", x=-0.31, y=-0.275, label.size = 0.18, color = "black",fill="white") +
  # 
  # geom_label(size = 2, label="M", x=-0.35, y=-0.205, label.size = 0.18, color = "black",fill="white") +
  # 
  # geom_label(size = 2, label="N", x=-0.37, y=-0.125, label.size = 0.18, color = "black",fill="white") +
  #  
  # geom_label(size = 2, label="S", x=-0.35, y=0.04, label.size = 0.18, color = "black",fill="white") +
  # 
  # geom_label(size = 2, label="U", x=-0.238, y=0.345, label.size = 0.2, color = "black",fill="white") +

  
  # Add attribution

  dendrogram <- dendrogram +
  
  geom_text(x=1, y= -1.4, label="Credits: Diego Montecino-Latorre", colour="black", size=2.5) +  
  
  #scale the lines  
    
  scale_edge_width_continuous(range = c(0.2, 15)) +
  
  # format of the dendogram
    
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position="none",
    plot.margin=margin(0,0,0,0,"cm"),
    plot.background = element_rect(fill="transparent", color=NA))   +
    
    xlim(-1.35, 1.35)+
    ylim(-1.35, 1.35)
  
  
  # dendrogram
  
  
  #ggsave(file="dendrogram_hosts_cov.tiff", plot=dendrogram, width=20, height=20, dpi = 1000, units = "cm")
  ggsave(file="dendrogram_hosts_cov.tiff", plot=dendrogram, width=10, height=10, dpi = 300, units = "cm")
  
  
