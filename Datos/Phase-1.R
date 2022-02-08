# Libraries
library(tidyverse)
library(here)
library(kableExtra)
library(ggpubr)

## Ggplot Theme setting
theme_set(theme_bw(base_size = 16, base_family = "Palatino")) 
options(digits = 4)


# Data Loading ----
## Loading the paths
files <-
   here("Datos", "Datos ensayos") %>% 
   dir( recursive=TRUE, full.names=TRUE, pattern="\\.txt$")


# Selecting the Phase I
Fase.1 <- files[1:16]


## Creating the Nested dataframe ----
Fase.I <- Fase.1 %>%  map( ~ read.delim2(.,  skip=7))
Fase.I <- Fase.I %>%   set_names(Fase.1) %>%   enframe("Type", "Data")


## Organising the dataframe
Fase.I <- 
   Fase.I %>%  
   separate(Type, 
            sep = "/",
            into = c(c(LETTERS[1:9]), c("Phase", "Material", "Sample", "Sample name"))
   ) %>% 
   select(-A, -B, -C, -D, -E, -F, -G, -H, -I)

## Arranging factors 
Fase.I$Material <- 
   factor(Fase.I$Material,
          levels = c("PLA virgen", "PLA reciclado"),
          labels = c("Virgin" , "Recycled"))
Fase.I <- Fase.I %>% arrange(Material)


# Correcting the Tensile Strength Nov 17/2021
Fase.I$area <- c(60, 60, 60, 100, 100, 100, 100, 60, 60, 60, 60, 100, 100, 60, 100, 100) 
Fase.I$area <- Fase.I$area/100


## Adding the factor correction for each data
Fase.I <-
   Fase.I %>%
   mutate(Data = map2(Data, Fase.I$area , ~ .x %>%
                         mutate ( section = 32 +  ( .y * 33))
                      )
          )


## Calculating the Tensile Strength

Fase.I <-
   Fase.I %>%
   mutate(Data = 
             map(Data, ~ .x %>%
                    mutate ( Tensile = (1000 * kN ) / section ,
                             Strain = mm / 28 )
             ))


## Adding variables to the Original data
Fase.I <-
   Fase.I %>%
   mutate(Data = map(Data, ~ .x %>%
                        mutate (Tensile_old = (kN / (5 * 13))*(1000),
                                Strain = mm / 28 )
   ))

### Function to identify Young Modulus
Young_mod <- function(df) {
   # Filtering the Dataframe
   df <- df %>% filter(Strain >= 0.0005 & Strain <= 0.035 )
   # Doing the linear model
   model= lm(Tensile ~ Strain, data = df)
   E= coefficients(model)[[2]]
   return(E)
}


# Calculating the Max mechanical properties ----

## Identifying max Tensile 
Fase.I <- 
   Fase.I %>% 
   mutate(
      Young = map_dbl(Data,  Young_mod ),
      Load.max = map_dbl(Data,  function(df) max(df$kN)),
      Tensile.max = map_dbl(Data,  function(df) max(df$Tensile)),
      Tensile.max_old = map_dbl(Data,  function(df) max(df$Tensile_old))
   )

## Creating the experimental dataframe with the other factors 
### Layer height 
Fase.I$LH <- c(0.15, 0.3, 0.15, 0.3, 0.3, 0.15, 0.15, 0.3, 0.15, 0.3, 0.3, 0.15, 0.3, 0.15, 0.15, 0.3) 
Fase.I$LH <- as.factor(Fase.I$LH)

### Infill Pattern 
Fase.I$IP <- c("Tri-hex", "Tri-hex", "Grid", "Grid", 
               "Tri-hex", "Tri-hex", "Grid", "Grid", 
               "Tri-hex", "Tri-hex", "Grid", "Tri-hex", 
               "Tri-hex", "Grid", "Grid", "Grid") 
Fase.I$IP <- as.factor(Fase.I$IP)


### Infill density 
Fase.I$ID <- c(60, 60, 60, 100, 100, 100, 100, 60, 60, 60, 60, 100, 100, 60, 100, 100) 
Fase.I$ID <- as.factor(Fase.I$ID)


### Printing speed 
Fase.I$PS <- c(40, 80, 80, 80, 40, 80, 40, 40, 40, 80, 40, 80, 40, 80, 40, 80 )
Fase.I$PS <- as.factor(Fase.I$PS)

# Final Dataframe
Fase.I <- Fase.I %>% select(Material, LH, IP, ID, PS, Load.max, Young, Tensile.max, Sample, `Sample name`, Data )


# Exporting the Table ----

table.fase1 <- 
   Fase.I %>% select(Material:Load.max,Tensile.max, Young ) %>% 
   set_names("Material", "Layer Height (mm)", "Infill Pattern", "Infill Density (%)", "Printing Speed (mm/s)", "Max Load (kN)", "Tensile Strength (MPa)" ,"Young Modulus (MPa)" )

table.fase1$`Max Load (kN)` <- table.fase1$`Max Load (kN)` %>% round( digits=2)
table.fase1$`Tensile Strength (MPa)` <- table.fase1$`Tensile Strength (MPa)` %>% round( digits=2)
table.fase1$`Young Modulus (MPa)` <- table.fase1$`Young Modulus (MPa)` %>% round( digits=2)

#write_csv2(table.fase1, "tables/Phase-1.csv")



# Exporting the Graphics ----
Fase.I.Graphs <- list()
Fase.I.Graphs$MT <- 
   Fase.I %>% 
   ggplot(aes(x=Material, y = Load.max )) +
   geom_boxplot(aes(fill=Material)) +
   #facet_grid( .  ~  ) +
   labs(y="Max Load (kN)", x="Material type") +
   coord_cartesian(ylim = c(1, 4)) +
   theme(legend.position = "none") 

Fase.I.Graphs$MT_Y <- 
   Fase.I %>% 
   ggplot(aes(x=Material, y = Young )) +
   geom_boxplot(aes(fill=Material)) +
   #facet_grid( .  ~  ) +
   labs(y="Young modulus (MPa)", x="Material type") +
   coord_cartesian(ylim = c(900, 1250))+
   theme(legend.position = "none") 

Fase.I.Graphs$MT_Tensile <- 
   Fase.I %>% 
   ggplot(aes(x=Material, y = Tensile.max )) +
   geom_boxplot(aes(fill=Material)) +
   #facet_grid( .  ~  ) +
   labs(y="Tensile Strength (MPa)", x="Material type") +
   coord_cartesian(ylim = c(40, 60))+
   theme(legend.position = "none") 


## Comparing the Virgin et Recycling
Fase.I.Graphs$LH <- 
   Fase.I %>% 
   ggplot(aes(x = LH, y = Load.max )) +
   geom_boxplot(aes(fill=Material)) +
   #facet_grid( .  ~  ) +
   labs(y="Max Load (kN)", x="Layer heigth\n(mm)") +
   coord_cartesian(ylim = c(1, 4)) +
   theme(legend.position = "none") 

Fase.I.Graphs$LH_Y <- 
   Fase.I %>% 
   ggplot(aes(x = LH, y = Young )) +
   geom_boxplot(aes(fill=Material)) +
   #facet_grid( .  ~  ) +
   labs(y="Young modulus (MPa)", x="Layer heigth\n(mm)") +
   coord_cartesian(ylim = c(900, 1250))+
   theme(legend.position = "none") 

Fase.I.Graphs$LH_Tensile <- 
   Fase.I %>% 
   ggplot(aes(x = LH, y = Tensile.max )) +
   geom_boxplot(aes(fill=Material)) +
   #facet_grid( .  ~  ) +
   labs(y="Tensile Strength (MPa)", x="Layer heigth\n(mm)") +
   coord_cartesian(ylim = c(40, 60))+
   theme(legend.position = "none") 

## Infill pattern
Fase.I.Graphs$IP <- 
   Fase.I %>% 
   ggplot(aes(x = IP, y = Load.max )) +
   geom_boxplot(aes(fill=Material)) +
   #facet_grid( .  ~  ) +
   labs(y="Max Load (kN)", x="Infill pattern") +
   coord_cartesian(ylim = c(1, 4))

Fase.I.Graphs$IP_Y <- 
   Fase.I %>% 
   ggplot(aes(x = IP, y = Young )) +
   geom_boxplot(aes(fill=Material)) +
   #facet_grid( .  ~  ) +
   labs(y="Young modulus (MPa)", x="Infill pattern") +
   coord_cartesian(ylim = c(900, 1250))+
   theme(legend.position = "none") 

Fase.I.Graphs$IP_Tensile <- 
   Fase.I %>% 
   ggplot(aes(x = IP, y = Tensile.max )) +
   geom_boxplot(aes(fill=Material)) +
   #facet_grid( .  ~  ) +
   labs(y="Tensile Strength (MPa)", x="Infill pattern") +
   coord_cartesian(ylim = c(40, 60))+
   theme(legend.position = "none") 


## Infill density
Fase.I.Graphs$ID <- 
   Fase.I %>% 
   ggplot(aes(x = ID, y = Load.max )) +
   geom_boxplot(aes(fill=Material)) +
   #facet_grid( .  ~  ) +
   labs(y="Max Load (kN)", x="Infill density\n(%)") +
   coord_cartesian(ylim = c(1, 4))

Fase.I.Graphs$ID_Y <- 
   Fase.I %>% 
   ggplot(aes(x = ID, y = Young )) +
   geom_boxplot(aes(fill=Material)) +
   #facet_grid( .  ~  ) +
   labs(y="Young modulus (MPa)" , x="Infill density\n(%)") +
   coord_cartesian(ylim = c(900, 1250))

Fase.I.Graphs$ID_Tensile <- 
   Fase.I %>% 
   ggplot(aes(x = ID, y = Tensile.max )) +
   geom_boxplot(aes(fill=Material)) +
   #facet_grid( .  ~  ) +
   labs(y="Tensile Strength (MPa)", x="Infill density\n(%)") +
   coord_cartesian(ylim = c(40, 60))

## Infill density
Fase.I.Graphs$PS <- 
   Fase.I %>% 
   ggplot(aes(x = PS, y = Load.max )) +
   geom_boxplot(aes(fill=Material)) +
   #facet_grid( Load.max  ~ LH + IP ) +
   labs(y="Max Load (kN)", x="Printing speed\n(mm/s)") +
   coord_cartesian(ylim = c(1, 4))

Fase.I.Graphs$PS_Y <- 
   Fase.I %>% 
   ggplot(aes(x = PS, y = Young )) +
   geom_boxplot(aes(fill=Material)) +
   #facet_grid( Load.max  ~ LH + IP ) +
   labs(y="Young modulus (MPa)", x="Printing speed\n(mm/s)") +
   coord_cartesian(ylim = c(900, 1250))
#theme(legend.position = "none")

Fase.I.Graphs$PS_Tensile <- 
   Fase.I %>% 
   ggplot(aes(x = PS, y = Tensile.max )) +
   geom_boxplot(aes(fill=Material)) +
   #facet_grid( Load.max  ~ LH + IP ) +
   labs(y="Tensile Strength (MPa)", x="Printing speed\n(mm/s)") +
   coord_cartesian(ylim = c(40, 60))
#theme(legend.position = "none")


# Total Graphic
ggarrange(#LH, IP, ID , PS,
   Fase.I.Graphs$LH_Tensile, Fase.I.Graphs$IP_Tensile, Fase.I.Graphs$ID_Tensile , Fase.I.Graphs$PS_Tensile,
   Fase.I.Graphs$LH_Y, Fase.I.Graphs$IP_Y, Fase.I.Graphs$ID_Y , Fase.I.Graphs$PS_Y,
   ncol = 4, nrow = 2 ,common.legend = TRUE)

# Exporting the data
ggsave(here("Figures","Figure-3b.jpg"), 
       width=1500, height=800, dpi = 130, 
       units = "px")



# Anova Phase I ----

## Creating the Linear Model without interactions
Fase.I_anova <- Fase.I %>% select(Material:PS, Load.max)
Total_model <- lm(Load.max ~ LH+IP+ID+PS+Material , data = Fase.I_anova)
#summary(Total_model)
anv.fase.1 <- anova(Total_model)


# Plotting Anovoa table
anv.fase.1 %>%  set_names("Df", "Sum Sq", "Mean Sq", "F value", "Pr(F)") %>% 
   kbl(booktabs = T, digits = c(0, 1, 3, 3, 3, 5),
       caption = "ANOVA results at 95\\% significance level",  
       linesep = "")  %>%
   kable_styling(latex_options = c("striped"))


### Anova Phase I - Virgin and Recycled

## Virgin
Virgen <- Fase.I %>% filter(Material == "Virgin") %>% select(LH:PS, Load.max)
Virgin_model <- lm(Load.max ~ . , data =Virgen )
Virgin_anova <- anova(Virgin_model) 

## Recycled
Recycled <- Fase.I %>% filter(Material == "Recycled") %>% select(LH:PS, Load.max)
Recycled_model <- lm(Load.max ~ . , data =Recycled)
Recycled_anova <- anova(Recycled_model) 



## Anova for Virgin material
Virgin_anova %>%  set_names("Df", "Sum Sq", "Mean Sq", "F value", "Pr(F)") %>% 
   kbl(booktabs = T, digits = c(0, 1, 3, 3, 3, 5),
       caption = "ANOVA Virgin",  
       linesep = "")  %>%
   kable_styling(latex_options = c("striped"))

Recycled_anova %>% set_names("Df", "Sum Sq", "Mean Sq", "F value", "Pr(F)") %>% 
   kbl(booktabs = T, digits = c(0, 1, 3, 3, 3, 5),
       caption = "ANOVA Recycled",  
       linesep = "")  %>%
   kable_styling(latex_options = c("striped"))






















