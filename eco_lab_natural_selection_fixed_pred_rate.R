###### Ecology Lab: Natural selection simulation

### Clear everything
rm(list=ls())
#############

############# Function building #############

### Visualize main color
display_main_color <- function(main_color){
  plot(NULL, xlim = c(0,1), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "", asp = 1)
  rect(0, 0, 1, 1, col = rgb(main_color[1], main_color[2], main_color[3]), border = NA)
  title(main = "Habitat mean color")
}

#### Visualize patches, origin (1,1) at left lower corner
Draw_habitat <- function(habitat){
  
  len_x <- habitat$x[nrow(habitat)]
  len_y <- habitat$y[nrow(habitat)]
  
  plot(
    NULL, 
    xlim = c(0.5, len_x + 0.5), 
    ylim = c(0.5, len_y + 0.5),
    xlab = "", ylab = "", 
    axes = FALSE, asp = 1
  )
  for (i in 1:nrow(habitat)) {
    rect(
      habitat$x[i] - 0.5, habitat$y[i] - 0.5,
      habitat$x[i] + 0.5, habitat$y[i] + 0.5,
      col = habitat$col_code[i],
      border = NA 
    )
  }
  title(main = "Habitat")
}

#### Patch building: fully random
Random_patch <- function(seed, len_x, len_y){
  ## Fix draw results (number changeable)
  set.seed(seed)
  ### Patch building
  habitat <- within(
    data.frame(
      x = rep(1:len_x, each = len_y),
      y = rep(1:len_y, times = len_x),
      R = runif(len_x * len_y, 0, 1),
      G = runif(len_x * len_y, 0, 1),
      B = runif(len_x * len_y, 0, 1)
    ), {
      col_code <- rgb(R, G, B)
    }
  )
  Main_color <- colMeans(habitat[, c("R", "G", "B")])
  set.seed(NULL)
  display_main_color(main_color = Main_color)
  Habitat <- list(habitat = habitat, main_color = Main_color)
  Draw_habitat(habitat = habitat)
  return(Habitat)
}

#### Patch building: major color
Major_color <- function(seed, len_x, len_y, homo = 2){
  ## Fix draw results (number changeable)
  set.seed(seed)
  ### Draw a major color
  major_R <- runif(1, 0.1, 0.9)
  major_G <- runif(1, 0.1, 0.9)
  major_B <- runif(1, 0.1, 0.9)
  ## Fix draw results (optional, number changeable)
  set.seed(seed)
  ### Patch building
  habitat <- within(
    data.frame(
      x = rep(1:len_x, each = len_y),
      y = rep(1:len_y, times = len_x),
      R = rbeta(len_x * len_y, major_R * homo, (1-major_R) * homo),
      G = rbeta(len_x * len_y, major_G * homo, (1-major_G) * homo),
      B = rbeta(len_x * len_y, major_B * homo, (1-major_B) * homo)
    ), {
      col_code <- rgb(R, G, B)
    }
  )
  Main_color <- colMeans(habitat[, c("R", "G", "B")])
  set.seed(NULL)
  display_main_color(main_color = Main_color)
  Habitat <- list(habitat = habitat, main_color = Main_color)
  Draw_habitat(habitat = habitat)
  return(Habitat)
}

### Function: display species traits function
display_species_color <- function(species_trait){
  plot(NULL, xlim = c(0, nrow(species_trait)), ylim = c(0, 1),
       axes = FALSE, xlab = "", ylab = "", asp = 1)
  for (i in 1:nrow(species_trait)) {
    rect(i - 1, 0, i, 1, col = species_trait$col_code[i], border = NA)
  }
  title(main = "Species colors")
}

### Function: species trait decision
Species_trait <- function(N, seed){
  ## Fix draw results (optional, number changeable)
  set.seed(seed)
  ### Draw species trait
  species_trait <- within(
    data.frame(species_number = c(1:N),
               R = runif(N, 0, 1),
               G = runif(N, 0, 1),
               B = runif(N, 0, 1)), {
                 col_code <- rgb(R, G, B)
               })
  display_species_color(species_trait)
  return(species_trait)
}

### Function: manually decide species trait
Manual_species_trait <- function(hexcode_vector){
  # Convert the hex code to RGB values
  species_trait <- as.data.frame(t(col2rgb(hexcode_vector)/255))
  species_trait <- setNames(species_trait, c("R", "G", "B"))
  species_number <- c(1:nrow(species_trait))
  species_trait <- cbind(species_number, species_trait)
  species_trait$col_code <- hexcode_vector
  display_species_color(species_trait)
  return(species_trait)
}

### Function: dispersal
dispersal <- function(sp_pop, len_x, len_y){
  sp <- rep(sp_pop$species_number, sp_pop$individuals)
  position <- sample((len_x - 2) * (len_y - 2), length(sp))
  pos_x <- position %% (len_x - 2) + 1
  pos_y <- position %/% (len_x - 2) + 1
  disp_df <- data.frame(sp, pos_x, pos_y)
  return(disp_df)
}

### Function: predation
predation <- function(sp_disp, habitat, species_trait, pred_rate){
  
  ## Decide predation number
  total_abundance <- nrow(sp_disp)
  pred_num <- round(total_abundance * pred_rate)
  
  ## Add species traits
  color_df <- merge(sp_disp, species_trait,
                    by.x = "sp", by.y = "species_number", all.x = TRUE)
  color_df$consp <- -100
  
  ## Calculate conspicuousness
  for (i in 1:nrow(color_df)) {
    target_x <- color_df$pos_x[i]
    target_y <- color_df$pos_y[i]
    ## Surrounding patch color 
    neighbors <- subset(habitat,
                        x >= target_x - 1 & x <= target_x + 1 & y >= target_y - 1 & y <= target_y + 1)
    neighbors_9 <- as.matrix(neighbors[, c("R", "G", "B")])
    ### Target trait
    target_rgb <- color_df[i, c("R", "G", "B")]
    ### Target color difference with surrounding patches
    trait_diff <- sum((neighbors_9 - matrix(as.numeric(target_rgb), nrow(neighbors), 3, byrow = TRUE))^2)
    ### Color variance of surrounding patches
    mean_color <- colMeans(neighbors[, c("R", "G", "B")])
    patch_mess <- sum((neighbors_9 - matrix(mean_color, nrow(neighbors), 3, byrow = TRUE))^2)
    
    ### Conspicuousness calculation
    consp_level <- trait_diff / patch_mess
    color_df$consp[i] <- consp_level
  }
  
  ### Predation
  color_df <- color_df[-order(color_df$consp, decreasing = T)[1:pred_num], ]
  sp_abund <- as.data.frame(table(color_df$sp))
  colnames(sp_abund) <- c("species_number", "individuals")
  
  return(sp_abund)
  
}

### Function: reproduction
reproduction <- function(sp_pop, ini_pop){
  r <- ini_pop * nrow(sp_pop) / sum(sp_pop$individuals)
  sp_pop$individuals <- round(sp_pop$individuals * r)
  return(sp_pop)
}

#### Function: plot selection process
Selection_process <- function(pop_process){
  species <- unique(pop_process$sp)
  plot(NA, xlim = range(pop_process$generation),
       ylim = range(pop_process$individuals),
       xlab = "Generation",
       ylab = "Individuals")
  for (s in species) {
    dat <- subset(pop_process, species_number == s)
    lines(dat$generation, dat$individuals, col = dat$col_code[1], lwd = 2)
  }
  title(main = "Selection process")
}

### Simulation function
Natural_selection <- function(habitat, species_traits, ini_pop, pred_rate, gen){
  
  Habitat <- habitat$habitat
  len_x <- Habitat$x[nrow(Habitat)]
  len_y <- Habitat$y[nrow(Habitat)]
  
  if(nrow(species_traits) * ini_pop > (len_x - 2) * (len_y - 2)){
    cat("Number of individuals larger than number of available patches.\nPlease change habitat size or initial individual number per species.")
    return("Error in parameters settings.")
  }
  
  Sp_Pop <- data.frame(species_number = c(1:nrow(species_traits)),
                       individuals = ini_pop)
  pop_process <- cbind(generation = 0, Sp_Pop)
  
  
  ### True random simulation results
  set.seed(NULL)
  
  for (g in 1:gen) {
    Sp_Disp <- dispersal(sp_pop = Sp_Pop, len_x = len_x, len_y = len_y)
    Sp_Pop <- predation(sp_disp = Sp_Disp, habitat = Habitat, species_trait = species_traits, pred_rate = pred_rate)
    Sp_Pop <- reproduction(sp_pop = Sp_Pop, ini_pop = ini_pop)
    
    ### save results
    pop_process <- rbind(pop_process, cbind(generation = g, Sp_Pop))
  }
  
  pop_process <- merge(pop_process, species_traits[, c("species_number", "col_code")],
                       by.x = "species_number", by.y = "species_number", all.x = TRUE)
  pop_process <- pop_process[order(pop_process$generation, pop_process$sp), ]
  
  selection <- list(Habitat = Habitat, Main_color = habitat$main_color, Pop_Process = pop_process, Species_Trait = species_traits)
  
  Selection_process(pop_process = pop_process)
  
  return(selection)
}
#############

############# Habitat and Species trait decision #############

### Uniform patch color
Habitat <- Random_patch(seed = 420, len_x = 50, len_y = 50)
### Habitat with certain patch mean color
Habitat <- Major_color(seed = 420, len_x = 100, len_y = 100)
### Species colors: draw
Species_Traits <- Species_trait(N = 5, seed = 420)
### Manually decide species colors
Species_Traits <- Manual_species_trait(c("#E04000", "#FFF123", "#114514", "#567789", "#420420"))

############# Parameter settings #############
Parameters <- list(
  ### Habitat
  habitat <- Habitat,
  ### Species traits
  species_traits <- Species_Traits,
  ### Number of individuals per species (must be an integer)
  ini_pop <- 40,
  ### Predation rate
  pred_rate <- 0.05,
  ### Generations (must be integer)
  gen <- 200
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