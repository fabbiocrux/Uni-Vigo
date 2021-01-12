---
title: "Data Analysis of the Mechanical data"
description: |
  A new article created using the Distill format.
author:
  - name: Fabio Cruz
    url: https://example.com/norajones
    affiliation: Spacely Sprockets
    affiliation_url: https://example.com/spacelysprokets
date: "`r Sys.Date()`"
output: distill::distill_article
editor_options: 
  chunk_output_type: console
---

```{r setup.data, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, include = FALSE)
require(tidyverse)

## Ggplot Theme setting
theme_set(theme_bw(base_size = 15, base_family = "Palatino")) # Defining the global theme
#theme_minimal

```

```{r Load.data}
# Reading data ----
## Loading the data from Excel

#camino <- paste0 (getwd(),"/Datos/Datos ensayos")  
#files <- list.files(path = camino, recursive=TRUE, full.names=TRUE, pattern="\\.txt$")

files <- dir("Datos/Datos ensayos", recursive=TRUE, full.names=TRUE, pattern="\\.txt$")
#files <- list.files("Datos/Datos ensayos", recursive=TRUE, full.names=TRUE, pattern="\\.txt$")



## Identifying Endommage files
files <- files[-(16:17)]

# Reading the Data Fase I
Fase.I <- files[1:15] %>%  map( ~ read.delim2(.,  skip=7))
Fase.I <- Fase.I %>%   set_names(files[1:15]) %>%   enframe("Type", "Data")



## Creating the columns for the global data
Fase.I <- Fase.I %>%  
  separate(Type, 
           sep = "/", 
           into = c("X", "Y", "Phase", "Type material", "Sample", "Sample name")) %>% 
  select(-X, -Y)
  

## Adding the other factors

### Layer height 
Fase.I$LH <- c(0.15, 0.3, 0.15, 0.3, 0.3, 0.15, 0.15, 0.3, 0.15, 0.3, 0.3, 0.15, 0.3, 0.15, 0.15 )
Fase.I$LH <- as.factor(Fase.I$LH)

### Infill Pattern 
Fase.I$IP <- c("Tri-hex","Tri-hex", "Grid","Grid","Tri-hex","Tri-hex", "Grid","Grid", "Tri-hex","Tri-hex" ,"Grid",  "Tri-hex","Tri-hex" , "Grid","Grid")
Fase.I$IP <- as.factor(Fase.I$IP)

### Infill density 
Fase.I$ID <- c(60, 60, 60, 100, 100, 100, 100, 60, 60, 60, 60,  100, 100 , 60, 100)
Fase.I$ID <- as.factor(Fase.I$ID)

### Printing speed 
Fase.I$PS <- c(40, 80, 80, 80, 40, 80, 40, 40, 40, 80, 40,  80, 40 , 80, 40)
Fase.I$PS <- as.factor(Fase.I$PS)


Fase.I <- Fase.I %>% select(Phase, `Type material`, LH, IP, ID,PS, Data, Sample, `Sample name`)

## Adding variables to the Original data
Fase.I <- 
  Fase.I %>% 
  mutate(Data = map(Data, ~ .x %>% mutate (Tensile = (kN / (5 * 13))*(1000), 
                                           Strain = mm / 28 )
                    ))


### Function to identify Young Modulus
Young <- function(df) {
  # Filtering the Dataframe
  df <- df %>% filter(Strain >= 0.0005 & Strain <= 0.0025 ) 
  # Doing the linear model
   model= lm(Tensile ~ Strain, data = df)
   E= coefficients(model)[[2]]
  return(E)
}

## Identifying max Tensile
Fase.I <- 
  Fase.I %>% 
  mutate(Young = map_dbl(Data,  Young ),
         Load.max = map_dbl(Data,  function(df) max(df$kN)),
         Tensile.max = map_dbl(Data,  function(df) max(df$Tensile))
         )


#write_csv2(Fase.I %>% select(Phase:PS, Sample: Tensile.max ), file = "Phase.I.csv")
```

## Data

```{r Table.Data, include=TRUE, layout="l-body-outset"}

Fase.I %>% select(Phase:PS, Sample: Tensile.max ) %>% 
  kbl() %>%
  kable_styling()

```

## Phase: 1
### Influence of the parameters

```{r}

## Comparing the Virgin et Recycling
MT <- 
  Fase.I %>% 
  ggplot(aes(x=`Type material`, y = Load.max )) +
  geom_boxplot(aes(fill=`Type material`)) +
  #facet_grid( .  ~  ) +
  labs(y="Max Load [kN]", x="Material type") +
  coord_cartesian(ylim = c(0, 5))

## Comparing the Virgin et Recycling
LH <- 
  Fase.I %>% 
  ggplot(aes(x = LH, y = Load.max )) +
  geom_boxplot(aes(fill=`Type material`)) +
  #facet_grid( .  ~  ) +
  labs(y="Max Load [kN]", x="Layer heigth [mm]") +
  coord_cartesian(ylim = c(0, 5))

## Infill pattern
IP <- 
  Fase.I %>% 
  ggplot(aes(x = IP, y = Load.max )) +
  geom_boxplot(aes(fill=`Type material`)) +
  #facet_grid( .  ~  ) +
  labs(y="Max Load [kN]", x="Infill pattern") +
  coord_cartesian(ylim = c(0, 5))

## Infill density
ID <- 
  Fase.I %>% 
  ggplot(aes(x = ID, y = Load.max )) +
  geom_boxplot(aes(fill=`Type material`)) +
  #facet_grid( .  ~  ) +
  labs(y="Max Load [kN]", x="Infill pattern") +
  coord_cartesian(ylim = c(0, 5))

## Infill density
PS <- 
  Fase.I %>% 
  ggplot(aes(x = PS, y = Load.max )) +
  geom_boxplot(aes(fill=`Type material`)) +
  #facet_grid( .  ~  ) +
  labs(y="Max Load [kN]", x="Printing speed [mm/s]") +
  coord_cartesian(ylim = c(0, 5))

```


```{r, include=TRUE, layout="l-body-outset", fig.width=10, fig.height=5}
library(ggpubr)
Phase.1 <- ggarrange(MT, LH, IP, ID , PS,  nrow = 1 ,common.legend = TRUE)
ggsave("Phase.1.jpg",width = 40, height = 10, units = "cm", dpi = 100)

```

### Anova
```{r}

```



```{r}
Test <- Fase.I  %>% select( -Data, -`Sample name`, -Young, -Load.max)  
Test %>% ?pivot_longer(!Phase, names_to = "key", values_to = "value")

Fase.I %>% 
  ggplot(aes(x = IP , y = Tensile.max)) +
    geom_boxplot() +
  coord_cartesian(ylim = c(0, 5))



Fase.I %>% 
  ggplot(aes(x = LH , y = Tensile.max)) +
    geom_boxplot() +
```


```{r PhaseII}


## Phase II ----
  
Fase.II <- files[1:8] %>% 
  map(~read.delim2( . , skip=7)) %>%  
  set_names(files[1:8]) %>%   enframe("Type", "Data")

```


```{r}
red.cell.folate <- ISwR::red.cell.folate
ggplot(red.cell.folate, aes(x = ventilation, y = folate)) + 
  geom_boxplot()

# Linear model
rcf.mod <- lm(folate~ventilation, data = red.cell.folate)
anova(rcf.mod)


plot(rcf.mod)


```









































