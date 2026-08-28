'library(readr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(fields)
library(MASS)
library(maps) # maps::county() maps::usa() maps:map()
library(ggplot2)
library(cowplot)

df <- readRDS("county_long.rds")
df <- df |> 
      group_by(geoid_2010) |>
      mutate(CountyID = as.integer(cur_group_id())) |>
      ungroup()

df_coords <- df |> dplyr::select(geoid_2010, CountyID, lat, lng) |> distinct()

county <- map_data("county")
county <- county |> mutate(polyname = paste(region, subregion, sep = ","))
data("county.fips")
county <- county |> left_join(county.fips, by = "polyname")'
#####
##### Plotting population in 2015 across US counties
df_pop <- df |> filter(TAX_YEAR == 2015) |> dplyr::select(geoid_2010, CountyID, lat, lng, total_population) |> distinct()

df_plot <- left_join(county, df_pop |> 
                             dplyr::select(geoid_2010, total_population) |> 
                             mutate(geoid_2010 = as.integer(geoid_2010)) |>
                             rename(fips = geoid_2010), 
                     by = "fips")

ggplot(data=county, aes(x=long, y=lat, fill=region, group=group)) + 
      geom_polygon(color = "black") + 
      guides(fill="none") + 
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
      ggtitle('U.S. Map with Couties') + 
      coord_fixed(1.3)

ggplot(data=df_plot, aes(x=long, y=lat, fill=log(total_population), group=group)) + 
      geom_polygon(color = "black", linewidth = 0.2) + 
      guides(fill="none") + 
      viridis::scale_fill_viridis(name="LOG(Population)") +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
      ggtitle('U.S. Map with Couties, log(population)') + 
      coord_fixed(1.3)


#####
# Plotting the eigenvectors for different smoothness
nu_values <- list("point5"= 0.5, "2point5"= 2.5, "5"=5, "10"= 10)
for (smooth_val in names(nu_values)){
      print(smooth_val)
      nu <- nu_values[[smooth_val]]
      x <- as.matrix(df_coords |> dplyr::select(lat, lng)) # inputs: latitude and longitude
      d <- rdist(x)
      K <- Matern(d, smoothness=nu) # for smoothness paramter, should we do some sort of optimization?
      E <- eigen(K)
      
      df_eigen <- df_coords |> dplyr::select(geoid_2010) |>
            mutate(geoid_2010 = as.integer(geoid_2010)) |>
            rename(fips = geoid_2010)
      
      df_eigen$x1 <- E$vectors[,1]
      df_eigen$x2 <- E$vectors[,2]
      df_eigen$x20 <- E$vectors[,20]
      df_eigen$x50 <- E$vectors[,50]
      df_eigen$x100 <- E$vectors[,100]
      df_eigen$x102 <- E$vectors[,102]
      
      df_eigen <- left_join(county, df_eigen, by = "fips")
      
      p1 <- ggplot(data=df_eigen, aes(x=long, y=lat, fill=x1, group=group)) + 
            geom_polygon(color = "black", linewidth = 0.2) + 
            guides(fill="none") + 
            viridis::scale_fill_viridis(option = "inferno") +
            theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
            ggtitle('E$vectors[,1]') + 
            coord_fixed(1.3)
      
      p2 <- ggplot(data=df_eigen, aes(x=long, y=lat, fill=x2, group=group)) + 
            geom_polygon(color = "black", linewidth = 0.2) + 
            guides(fill="none") + 
            viridis::scale_fill_viridis(option = "inferno") +
            theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
            ggtitle('E$vectors[,2]') + 
            coord_fixed(1.3)
      
      p3 <- ggplot(data=df_eigen, aes(x=long, y=lat, fill=x20, group=group)) + 
            geom_polygon(color = "black", linewidth = 0.2) + 
            guides(fill="none") + 
            viridis::scale_fill_viridis(option = "inferno") +
            theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
            ggtitle('E$vectors[,20]') + 
            coord_fixed(1.3)
      
      p4 <- ggplot(data=df_eigen, aes(x=long, y=lat, fill=x50, group=group)) + 
            geom_polygon(color = "black", linewidth = 0.2) + 
            guides(fill="none") + 
            viridis::scale_fill_viridis(option = "inferno") +
            theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
            ggtitle('E$vectors[,50]') + 
            coord_fixed(1.3)
      
      p5 <- ggplot(data=df_eigen, aes(x=long, y=lat, fill=x100, group=group)) + 
            geom_polygon(color = "black", linewidth = 0.2) + 
            guides(fill="none") + 
            viridis::scale_fill_viridis(option = "inferno") +
            theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
            ggtitle('E$vectors[,100]') + 
            coord_fixed(1.3)
      
      p6 <- ggplot(data=df_eigen, aes(x=long, y=lat, fill=x102, group=group)) + 
            geom_polygon(color = "black", linewidth = 0.2) + 
            guides(fill="none") + 
            viridis::scale_fill_viridis(option = "inferno") +
            theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
            ggtitle('E$vectors[,102]') + 
            coord_fixed(1.3)
      
      plot_row <- plot_grid(p1, p2, p3, p4, p5, p6, nrow = 3)
      
      # now add the title
      title <- ggdraw() + 
            draw_label(
                  paste("Examples with smoothness =", nu),
                  fontface = 'bold',
                  x = 0,
                  hjust = 0
            ) +
            theme(
                  # add margin on the left of the drawing canvas,
                  # so title is aligned with left edge of first plot
                  plot.margin = margin(0, 0, 0, 7)
            )
      plot_grid(
            title, plot_row,
            ncol = 1,
            # rel_heights values control vertical title margins
            rel_heights = c(0.1, 1)
      )
      
      ggsave(filename = paste0("eigen_grid_",smooth_val,".pdf"),
             device = "pdf",
             height = 10, width = 6, units = "in")
}

#####
x <- as.matrix(df_coords |> dplyr::select(lat, lng)) # inputs: latitude and longitude
d <- rdist(x)
K <- Matern(d, smoothness=0.5) # for smoothness paramter, should we do some sort of optimization?
E <- eigen(K)

df_eigen <- df_coords |> dplyr::select(geoid_2010) |>
      mutate(geoid_2010 = as.integer(geoid_2010)) |>
      rename(fips = geoid_2010)

df_eigen$x1 <- E$vectors[,1]
df_eigen$x2 <- E$vectors[,2]
df_eigen$x20 <- E$vectors[,20]
df_eigen$x25 <- E$vectors[,25]
df_eigen$x50 <- E$vectors[,50]
df_eigen$x75 <- E$vectors[,75]
df_eigen$x90 <- E$vectors[,90]
df_eigen$x100 <- E$vectors[,100]
df_eigen$x102 <- E$vectors[,102]

df_eigen <- left_join(county, df_eigen, by = "fips")

ggplot(data=df_eigen, aes(x=long, y=lat, fill=x100, group=group)) + 
      geom_polygon(color = "black", linewidth = 0.2) + 
      viridis::scale_fill_viridis(option = "inferno") +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
      ggtitle('SVC 100') + 
      coord_fixed(1.3)

x_s <- cbind(x[,1] * E$vectors[,1:100], x[,2] * E$vectors[,1:100])

df_coords <- cbind(df_coords, x_s)
df_svc <- left_join(county, 
                    df_coords |> dplyr::select(-lat, -lng, -CountyID) |> mutate(fips = as.integer(geoid_2010)), 
                    by = "fips")

ggplot(data=df_svc, aes(x=long, y=lat, fill=`20`, group=group)) + 
      geom_polygon(color = "black", linewidth = 0.2) + 
      viridis::scale_fill_viridis(option = "inferno") +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
      ggtitle('SVC 20') + 
      coord_fixed(1.3)