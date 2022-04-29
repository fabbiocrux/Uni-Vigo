# Selecting the Phase II----
Fase.2 <- files[17:27]
# Deleting test repetido Recycled 70-RE
Fase.2 <- Fase.2[-4]


# Files missing:``
# PLA reciclado/40-RE.txt

# Reading the Data Fase II ----
## Identifying missing data
Fase.II <- Fase.2 %>%  map( ~ read.delim2(.,  skip=7))
Fase.II <- Fase.II  %>%   set_names(Fase.2) %>%   enframe("Type", "Data")

# Organising datafram ----
Fase.II <- 
   Fase.II %>%  
   separate(Type, 
            sep = "/",
            into = c(c(LETTERS[1:9]), c("Phase", "Material", "Sample", "Sample name"))
   ) %>% 
   select(-A, -B, -C, -D, -E, -F, -G, -H, -I)

# Arranging factors ----
Fase.II$Material <- 
   factor(Fase.II$Material,
          levels = c("PLA virgen", "PLA reciclado"),
          labels = c("Virgin" , "Recycled"))

Fase.II <- Fase.II %>% arrange(Material)

# Correcting the Tensile Strength Nov 19/2021 -----
Fase.II$area <- rep(c(100, 40, 55, 70, 85), 2)  
Fase.II$area <- Fase.II$area/100

# Adding the factor correction for each data
Fase.II <-
   Fase.II %>%
   mutate(Data = 
             map2(Data, Fase.II$area , ~ .x %>%
                     mutate ( section = 32 +  ( .y * 33))
             )
   )

Fase.II <-
   Fase.II %>%
   mutate(Data = 
             map(Data, ~ .x %>%
                    mutate ( Tensile = (1000 * kN ) / section ,
                             Strain = mm / 28 )
             ))

# Calculating max Values ----
Fase.II <- 
   Fase.II %>% 
   mutate(
      Load.max = map_dbl(Data,  function(df) max(df$kN)),
      Tensile.max = map_dbl(Data,  function(df) max(df$Tensile))
   )

# Creating final dataframe of Phase II
Fase.II <- 
   Fase.II %>% 
   separate(Sample, 
            sep = "-",
            into = c("ID", "Material2")) %>% 
   select(Material, ID, Load.max, Tensile.max , area)

# As factor the the variables ID
Fase.II$ID <- as.numeric(Fase.II$ID)




# Graphic -----

A <- 
   ggplot(Fase.II, aes(x=ID, y=Load.max, col=Material)) + 
   #aes(col=Material),
   geom_point( size=2 ) + 
   
   # Nov 15/2021
   geom_line(stat="smooth",
             method = "lm", formula = y ~ poly(x, 4), alpha = 0.8, size= 1) +
   # stat_regline_equation(label.y = c(1,1.3), label.x = c(40,40) , data = Fase.II, aes(x=ID, y=Load.max, label = paste(..eq.label..)),  formula = y ~ poly(x, 4), inherit.aes = TRUE ) +
   
   # geom_line(data = Fase.II %>% filter(ID < 90), stat="smooth",
   #           method = "lm", formula = y ~ x, linetype ="dashed", alpha = 0.8, size= 1) +
   # geom_line(data = Fase.II %>% filter(ID > 75), stat="smooth",
   #           method = "lm", formula = y ~ poly(x, 1), linetype ="dotted", alpha = 0.8, size= 1) +
   #  stat_smooth(data = Fase.II %>% filter(ID < 90) , method = "lm", formula = y ~ x, se=FALSE, aes(alpha=0.5)) +
   #  stat_smooth(data = Fase.II %>% filter(ID > 65) , method = "lm", formula = y ~ x, se=FALSE) +
   #stat_smooth(method = "lm", formula = y ~ poly(x, 2), se=FALSE) +
   # stat_regline_equation(label.y = c(1,1.3), label.x = c(40,40) , data = Fase.II, aes(x=ID, y=Load.max, label = paste(..eq.label.. , ..adj.rr.label.., sep = '~\n~')),  formula = y ~ poly(x, 4), inherit.aes = TRUE ) +
   
annotate("segment", x = 85, xend = 85, y = 1.5, yend = 3,  colour = "blue", linetype = "dashed") +
   coord_cartesian(xlim=c(40, 100), ylim=c(1, 4)) + 
   scale_x_continuous(breaks = c(40, 55, 70, 85, 100)) +
   annotate("segment", x = 40, xend = 85, y = 3.4, yend = 3.4, colour = "blue", size=0.5, alpha=0.8, arrow= arrow(ends = "both", angle = 30, length = unit(.2,"cm"))) +
   annotate("segment", x = 85, xend = 100, y = 3.5, yend = 3.5, colour = "blue", size=0.5, alpha=0.8, arrow= arrow(ends = "both", angle = 30, length = unit(.2,"cm"))) +
   # geom_vline(xintercept = 80) +
   annotate("text", x = c(50, 93), y = 3.6, label = c("Region A", "Region B"))+
   labs(title="", 
        subtitle="", 
        caption="",
        y="Maximum load (kN)",
        x="Infill density (%)" ) +
   theme(legend.position="top")
#ggsave("Correciones/Phase-2-corrected.jpg", width=5, height=5, dpi = "print")



B  <- 
   ggplot(Fase.II, aes(x=ID, y=Tensile.max, col=Material)) + 
   #aes(col=Material),
   geom_point( size=2 ) + 
   
   # Nov 15/2021
   geom_line(stat="smooth",
             method = "lm", formula = y ~ poly(x, 3), alpha = 0.8, size= 1) +
   # stat_regline_equation(label.y = c(1,1.3), label.x = c(40,40) , data = Fase.II, aes(x=ID, y=Load.max, label = paste(..eq.label..)),  formula = y ~ poly(x, 4), inherit.aes = TRUE ) +
   
   # geom_line(data = Fase.II %>% filter(ID < 90), stat="smooth",
   #           method = "lm", formula = y ~ x, linetype ="dashed", alpha = 0.8, size= 1) +
   # geom_line(data = Fase.II %>% filter(ID > 75), stat="smooth",
   #           method = "lm", formula = y ~ poly(x, 1), linetype ="dotted", alpha = 0.8, size= 1) +
   #  stat_smooth(data = Fase.II %>% filter(ID < 90) , method = "lm", formula = y ~ x, se=FALSE, aes(alpha=0.5)) +
   #  stat_smooth(data = Fase.II %>% filter(ID > 65) , method = "lm", formula = y ~ x, se=FALSE) +
   #stat_smooth(method = "lm", formula = y ~ poly(x, 2), se=FALSE) +
   # stat_regline_equation(label.y = c(1,1.3), label.x = c(40,40) , data = Fase.II, aes(x=ID, y=Load.max, label = paste(..eq.label.. , ..adj.rr.label.., sep = '~\n~')),  formula = y ~ poly(x, 4), inherit.aes = TRUE ) +
   
annotate("segment", x = 85, xend = 85, y = 30, yend = 50,  colour = "blue", linetype = "dashed") +
   coord_cartesian(xlim=c(40, 100), ylim=c(20, 60)) + 
   scale_x_continuous(breaks = c(40, 55, 70, 85, 100)) +
   annotate("segment", x = 40, xend = 85, y = 55, yend = 55, colour = "blue", size=0.5, alpha=0.8, arrow= arrow(ends = "both", angle = 30, length = unit(.2,"cm"))) +
   annotate("segment", x = 85, xend = 100, y = 55, yend = 55, colour = "blue", size=0.5, alpha=0.8, arrow= arrow(ends = "both", angle = 30, length = unit(.2,"cm"))) +
   # geom_vline(xintercept = 80) +
   annotate("text", x = c(50, 93), y = 58, label = c("Region A", "Region B"))+
   labs(title="", 
        subtitle="", 
        caption="",
        y="Tensile strength (MPa)",
        x="Infill density (%)" ) +
   theme(legend.position="top")
#ggsave("Correciones/Phase-2-corrected.jpg", width=5, height=5, dpi = "print")


ggarrange(#LH, IP, ID , PS,
   A,
   B,
   ncol = 2, nrow = 1 ,common.legend = FALSE)
#ggsave("Figures/Figure-4b.jpg", width=10, height=5, dpi = "print")


