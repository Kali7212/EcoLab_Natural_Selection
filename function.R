###### Ecology Lab: Natural selection simulation


############# Function building #############

### Visualize main color
display_main_color <- function(main_color){
  plot(NULL, xlim = c(0, nrow(main_color)), ylim = c(0, 1.1),
       axes = FALSE, xlab = "", ylab = "", asp = 1)
  for (i in 1:nrow(main_color)) {
    hex <- main_color$hex_code[i]
    perc <- main_color$percentage[i]
    rect(i - 1, 0.1, i, 1.1, col = hex, border = NA)
    text(i-0.5, 0, hex)
    text(i-0.5, 0.6, paste(round(perc*100, 2), "%"))
  }
  title(main = "Habitat main colors")
}

#### Visualize habitat, origin (1,1) at left lower corner
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
Random_patch <- function(seed = 466057982386929236631, len_x = 20, len_y = 20){
  
  ## idiots gate keeper
  len_x <- ceiling(len_x)
  len_y <- ceiling(len_y)
  if(len_x < 5 || len_y < 5){
    print("Habitat too small.")
    return("Habitat too small.")
  }
  
  ## Fix draw results (number changeable)
  if(seed != 466057982386929236631){set.seed(seed)}
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
  #Main_color <- colMeans(habitat[, c("R", "G", "B")])
  Main_color <- data.frame(color_no = 1,
                           hex_code = rgb(mean(habitat$R),
                                          mean(habitat$G),
                                          mean(habitat$B)),
                           percentage = 1)
  set.seed(NULL)
  display_main_color(main_color = Main_color)
  Habitat <- list(habitat = habitat, main_color = Main_color)
  Draw_habitat(habitat = habitat)
  return(Habitat)
}

#### Patch building: random, single major color
Major_color <- function(seed = 466057982386929236631, len_x = 20, len_y = 20, homo = 2){
  
  ## idiots gate keeper
  len_x <- ceiling(len_x)
  len_y <- ceiling(len_y)
  if(len_x < 5 || len_y < 5){
    print("Habitat too small.")
    return("Habitat too small.")
  }
  homo <- abs(homo)
  
  ## Fix draw results (number changeable)
  if(seed != 466057982386929236631){set.seed(seed)}
  ### Draw a major color
  major_R <- runif(1, 0.1, 0.9)
  major_G <- runif(1, 0.1, 0.9)
  major_B <- runif(1, 0.1, 0.9)
  ## Fix draw results (optional, number changeable)
  if(seed != 466057982386929236631){set.seed(seed)}
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
  
  Main_color <- data.frame(color_no = 1,
                           hex_code = rgb(mean(habitat$R),
                                          mean(habitat$G),
                                          mean(habitat$B)),
                           percentage = 1)
  set.seed(NULL)
  display_main_color(main_color = Main_color)
  Habitat <- list(habitat = habitat, main_color = Main_color)
  Draw_habitat(habitat = habitat)
  return(Habitat)
}

#### Patch building: manual major colors and patch pattern
Manual_color <- function(seed = 466057982386929236631, len_x = 20, len_y = 20, homo = 2,
                         col = c("#8888FF", "#88FF88", "#FF8888"),
                         col_ratio = c(33, 33, 34),
                         patch = 200){
  
  ## idiots gate keeper
  len_x <- ceiling(len_x)
  len_y <- ceiling(len_y)
  if(len_x < 5 || len_y < 5){
    print("Habitat too small.")
    return("Habitat too small.")
  }
  col_count <- round(abs(col_ratio)*patch/sum(col_ratio))
  
  if(seed != 466057982386929236631){set.seed(seed)}
  
  grid <- matrix(0, len_x+2, len_y+2)
  
  main_col_vec <- rep(1:length(col), col_count)
  idx <- sample((len_x+2)*(len_y+2), patch, replace=TRUE)
  grid[idx] <- sample(main_col_vec, patch, replace=TRUE)
  
  nbr_majority <- function(g){
    g2 <- g
    for (i in 1:len_x+1){
      for (j in 1:len_y+1){
        if (g[i,j]==0L){
          nb <- c(g[i-1,j-1],g[i-1,j],g[i-1,j+1],
                  g[i,j-1],           g[i,j+1],
                  g[i+1,j-1],g[i+1,j],g[i+1,j+1])
          g2[i,j] <- nb[sample(8, 1)]
        }
      } 
    } 
    return(g2)
  }
  
  while (min(grid[1:len_x+1, 1:len_y+1]) == 0){
    grid <- nbr_majority(grid)
    # image(t(apply(grid, 2, rev)),
    #       col = c("#FFFFFF", col),
    #       axes = FALSE,
    #       asp = 1)
  }
  
  grid <- grid[2:(len_x+1), 2:(len_y+1)]
  # image(t(apply(grid, 2, rev)),
  #       col = c(col),
  #       axes = FALSE,
  #       asp = 1)
  
  Habitat <- data.frame(x = rep(1:len_x, each = len_y),
                        y = rep(1:len_y, times = len_x),
                        R = -10,
                        G = -10,
                        B = -10,
                        col_code = c("no"))
  
  for (i in 1:length(col)) {
    rgb <- as.data.frame(t(col2rgb(col[i])/255))
    major_R <- rgb$red
    major_G <- rgb$green
    major_B <- rgb$blue
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
      })
    Habitat[grid == i,] <- habitat[as.vector(grid == i),]
  }
  Main_color <- data.frame(color_no = c(1:length(col)),
                           hex_code = col,
                           percentage = col_count/sum(col_count))
  set.seed(NULL)
  display_main_color(main_color = Main_color)
  Habitat_list <- list(habitat = Habitat, main_color = Main_color)
  Draw_habitat(habitat = Habitat)
  return(Habitat_list)
}

### Function: display species traits function
display_species_color <- function(species_trait){
  plot(NULL, xlim = c(0, nrow(species_trait)), ylim = c(0, 1.1),
       axes = FALSE, xlab = "", ylab = "", asp = 1)
  for (i in 1:nrow(species_trait)) {
    hex_code <- species_trait$col_code[i]
    rect(i - 1, 0.1, i, 1.1, col = hex_code, border = NA)
    text(i-0.5, 0, hex_code)
  }
  title(main = "Species colors")
}

### Function: species trait decision
Species_trait <- function(N, seed = 466057982386929236631){
  ## Fix draw results (optional, number changeable)
  if(seed != 466057982386929236631){set.seed(seed)}
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
predation <- function(sp_disp, habitat, species_trait, pred_intensity, fitness_varience){
  
  if(pred_intensity == 0){
    sp_abund <- as.data.frame(table(sp_disp$sp))
    colnames(sp_abund) <- c("species_number", "individuals")
    return(sp_abund)
  }
  
  ## Add species traits
  color_df <- merge(sp_disp, species_trait,
                    by.x = "sp", by.y = "species_number", all.x = TRUE)
  color_df$consp <- -100
  
  Rmat <- matrix(habitat$R, max(habitat$x))
  Gmat <- matrix(habitat$G, max(habitat$x))
  Bmat <- matrix(habitat$B, max(habitat$x))
  
  ## Calculate conspicuousness
  for (i in seq_len(nrow(color_df))) {
    
    x <- color_df$pos_x[i]
    y <- color_df$pos_y[i]
    
    nR <- Rmat[(x-1):(x+1),(y-1):(y+1)]
    nG <- Gmat[(x-1):(x+1),(y-1):(y+1)]
    nB <- Bmat[(x-1):(x+1),(y-1):(y+1)]
    
    target <- as.numeric(color_df[i,c("R","G","B")])
    
    trait_diff <- sum((nR-target[1])^2 + (nG-target[2])^2 + (nB-target[3])^2)
    
    mR <- mean(nR)
    mG <- mean(nG)
    mB <- mean(nB)
    
    patch_mess <- sum((nR-mR)^2 + (nG-mG)^2 + (nB-mB)^2)
    if(patch_mess <= 1e-10){patch_mess <- 1e-10}
    
    color_df$consp[i] <- trait_diff / patch_mess
  }
  
  ### Predation
  
  pred_prob <- function(consp, pred_int, fit_var){
    (1 / (1 + exp(fit_var * (100 / (pred_int) - consp)))) * (consp >= 0)
  }
  color_df$pred_prob <- pred_prob(consp = color_df$consp, pred_int = pred_intensity, fit_var = fitness_varience)
  color_df$pred_decision <- runif(nrow(color_df))
  color_df$survive <- color_df$pred_decision - color_df$pred_prob
  color_df <- color_df[color_df$survive > 0, ]
  
  sp_abund <- data.frame(species_number = c(1:nrow(species_trait)),
                         individuals = 0)
  if(nrow(color_df) == 0){
    return(sp_abund)
  }
  for (i in 1:nrow(sp_abund)) {
    sp_abund$individuals[i] <- sum(color_df$sp == i)
  }
  return(sp_abund)
  
}


### Function: predation, fixed predation rate
predation_fixed_rate <- function(sp_disp, habitat, species_trait, pred_intensity, fitness_varience){
  
  pred_rate <- abs(pred_intensity)/100
  if(pred_rate >= 0.99){pred_rate <- 0.99}
  
  ## Decide predation number
  total_abundance <- nrow(sp_disp)
  pred_num <- round(total_abundance * pred_rate)
  
  if(nrow(species_trait) == 1){
    sp_abund <- data.frame(species_number = c(1),
                           individuals = nrow(sp_disp))
    return(sp_abund)
  }
  if(pred_rate == 0){
    sp_abund <- as.data.frame(table(sp_disp$sp))
    colnames(sp_abund) <- c("species_number", "individuals")
    return(sp_abund)
    }

  
  ## Add species traits
  color_df <- merge(sp_disp, species_trait,
                    by.x = "sp", by.y = "species_number", all.x = TRUE)
  color_df$consp <- -100
  
  Rmat <- matrix(habitat$R, max(habitat$x))
  Gmat <- matrix(habitat$G, max(habitat$x))
  Bmat <- matrix(habitat$B, max(habitat$x))
  
  ## Calculate conspicuousness
  for (i in seq_len(nrow(color_df))) {
    
    x <- color_df$pos_x[i]
    y <- color_df$pos_y[i]
    
    nR <- Rmat[(x-1):(x+1),(y-1):(y+1)]
    nG <- Gmat[(x-1):(x+1),(y-1):(y+1)]
    nB <- Bmat[(x-1):(x+1),(y-1):(y+1)]
    
    target <- as.numeric(color_df[i,c("R","G","B")])
    
    trait_diff <- sum((nR-target[1])^2 + (nG-target[2])^2 + (nB-target[3])^2)
    
    mR <- mean(nR)
    mG <- mean(nG)
    mB <- mean(nB)
    
    patch_mess <- sum((nR-mR)^2 + (nG-mG)^2 + (nB-mB)^2)
    if(patch_mess <= 1e-10){patch_mess <- 1e-10}
    color_df$consp[i] <- trait_diff / patch_mess
  }
  
  ### Predation
  color_df <- color_df[-order(color_df$consp, decreasing = T)[1:pred_num], ]
  sp_abund <- as.data.frame(table(color_df$sp))
  colnames(sp_abund) <- c("species_number", "individuals")
  
  return(sp_abund)
  
}


### Function: reproduction
reproduction <- function(sp_pop, ini_pop){
  if (sum(sp_pop$individuals) == 0){
    r <- 0
    }else{
    r <- ini_pop * nrow(sp_pop) / sum(sp_pop$individuals)
  }
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
Natural_selection <- function(habitat, species_traits, ini_pop, gen, pred_intensity, fitness_varience, prob = T){
  
  ## idiots gate keeper
  if(gen > 100000){
    print("Please consider a shorter generation time (if you insist to torture your computer, go to function.R and turn off the limit.)")
    return("Too many generations for most computers to complete simulations in time.")
    }
  
  Habitat <- habitat$habitat
  len_x <- Habitat$x[nrow(Habitat)]
  len_y <- Habitat$y[nrow(Habitat)]
  
  pred_intensity <- abs(pred_intensity)
  
  if(nrow(species_traits) * ini_pop > (len_x - 2) * (len_y - 2)){
    cat("Number of individuals larger than number of available patches.\nPlease increase habitat size or decrease initial individual number per species.")
    return("Error in parameters settings.")
  }
  
  fitness_varience <- abs(fitness_varience)
  
  Sp_Pop <- data.frame(species_number = c(1:nrow(species_traits)),
                       individuals = ini_pop)
  pop_process <- cbind(generation = 0, Sp_Pop)
  
  if(prob != T){predation <- predation_fixed_rate}
  
  ### True random simulation results
  set.seed(NULL)
  
  for (g in 1:gen) {
    if(sum(Sp_Pop$individuals) > 0){
      Sp_Disp <- dispersal(sp_pop = Sp_Pop, len_x = len_x, len_y = len_y)
      Sp_Pop <- predation(sp_disp = Sp_Disp, habitat = Habitat, species_trait = species_traits, pred_intensity = pred_intensity, fitness_varience = fitness_varience)
      Sp_Pop <- reproduction(sp_pop = Sp_Pop, ini_pop = ini_pop)
      ext <- F
    }else{
      Sp_Pop <- reproduction(sp_pop = Sp_Pop, ini_pop = ini_pop)
      ext <- T
    }
    ### save results
    pop_process <- rbind(pop_process, cbind(generation = g, Sp_Pop))
  }
  
  if(ext == T){
    print("All the individuals are eaten by the predaters. What have you done?")
    }
  
  pop_process <- merge(pop_process, species_traits[, c("species_number", "col_code")],
                       by.x = "species_number", by.y = "species_number", all.x = TRUE)
  pop_process <- pop_process[order(pop_process$generation, pop_process$sp), ]
  
  selection <- list(Habitat = Habitat, Main_color = habitat$main_color, Pop_Process = pop_process, Species_Trait = species_traits)
  
  Selection_process(pop_process = pop_process)
  
  return(selection)
}
