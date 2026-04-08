###### Ecology Lab: Natural selection simulation

#### Clear everything
rm(list=ls())

#### load functions
source("function.R")


############# Habitat and Species trait decision #############

### Habitat with certain patch mean color
Habitat <- Major_color(seed = 420, len_x = 100, len_y = 100, homo = 5)
### Manual habitat main color
Habitat <- Manual_color(seed = 420, len_x = 100, len_y = 100, homo = 5,
                        #col = c("#AADD22", "#88EDCC", "#DDEE23", "#CCF0AA", "#A04FCC"),
                        #col_ratio = c(30, 30, 20, 15, 5),
                        col = c("#FF0000"),
                        col_ratio = c(1),
                        patch = 1000)

### Species colors: draw
Species_Traits <- Species_trait(N = 5, seed = 420)
### Manually decide species colors
Species_Traits <- Manual_species_trait(c("#E04E04", "#EEE123", "#BB7711", "#114514", "#116767", "#420420", "#676767"))
Species_Traits <- Manual_species_trait(c("#FF00FF", "#FFFF00", "#00FF00", "#0000FF"))

############# Parameter settings #############
Parameters <- list(
  ### Habitat
  habitat <- Habitat,
  ### Species traits
  species_traits <- Species_Traits,
  ### Number of individuals per species (must be an integer)
  ini_pop <- 40,
  ### Generations (must be integer)
  gen <- 100,
  pred_intensity <- 1e-9, 
  fitness_varience <- 2,
  prob = F
)
#############

############# Execute simulation: natural selection #############
Results <- do.call(Natural_selection, Parameters)

############# PLotting #############

#### Plot selection process
Selection_process(Results$Pop_Process)

#### Plot habitat
Draw_habitat(habitat = Results$Habitat)

#### Plot mean color
display_main_color(main_color = Results$Main_color)

#### Plot species traits
display_species_color(species_trait = Results$Species_Trait)

