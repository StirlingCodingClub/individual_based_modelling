

# Initialise new individuals on landscape
# N_ini = individual number, L_size = landscape x and y dim
# shp = shape of the gamma distr, rat = rate of gamma distr
ini_Ps <- function(L_size = 20, N_ini = 30, shp = 5, rat = 1){
    p_ID   <- 1:N_ini;
    p_x    <- sample(x = 1:L_size, size = N_ini, replace = TRUE);
    p_y    <- sample(x = 1:L_size, size = N_ini, replace = TRUE);
    p_sz   <- rgamma(n = N_ini, shape = shp, rate = rat);
    P      <- cbind(p_ID, p_x, p_y, p_sz);
    return(P);
}

# Causes competition between plants on the same landscape cell
# The plant with the bigger size is removed
# P = the array of individuals, L_size is the x & y dimension
competition <- function(P, L_size){
    for(x in 1:L_size){
        for(y in 1:L_size){
            on_cell <- sum(P[,2] == x & P[,3] == y);
            if(on_cell > 1){
                P_row <- which(P[,2] == x & P[,3] == y);
                P_com <- P[P_row,];
                winnr <- which.max(P_com[,4]);
                dead  <- which(P[,1] %in% P_com[-winnr, 1]);
                P[dead, 4] <- -1;
            }
        }
    }
    P <- P[P[,4] > 0, ];
    return(P);
}
    
# Produces new seeds and disperse them on the landscape
# P is the array of parent plants
# L_size is the x and y dimension of the landscape
# N_seeds is the number of seeds each parent above thresh_size produces
# rng is the range within the parent location of seed dispersal
# thresh_size is the threshold size of producing seeds for a parent
reproduction <- function(P, L_size, N_seeds = 10, rng = 2, thresh_size = 10){
    P_sp    <- P[P[,4] > thresh_size, ];
    N_sp    <- sum(P[,4] > thresh_size);
    all_of  <- NULL;
    if(N_sp > 0){
        for(i in 1:N_sp){
             if(is.vector(P_sp) == TRUE){
                 P_x    <- P_sp[2];
                 P_y    <- P_sp[3];   
             }else{
                 P_x    <- P_sp[i, 2];
                 P_y    <- P_sp[i, 3];
             }
             Pxs    <- seq(from = P_x - rng, to = P_x + rng, by = 1);
             Pys    <- seq(from = P_y - rng, to = P_y + rng, by = 1);
             p_ID   <- rep(x = 0, times = N_seeds);
             p_x    <- sample(x = Pxs, size = N_seeds, replace = TRUE);
             p_y    <- sample(x = Pys, size = N_seeds, replace = TRUE);
             p_sz   <- rep(x = 1, times = N_seeds);
             P_off  <- cbind(p_ID, p_x, p_y, p_sz);
             all_of <- rbind(all_of, P_off);
        }
        all_of[all_of[, 2] < 0 | all_of[, 2] > L_size, 4] <- -1; 
        all_of[all_of[, 3] < 0 | all_of[, 3] > L_size, 4] <- -1; 
        all_of <- all_of[all_of[,4] > 0, ];
    }
    return(all_of);
}

# Add the new seedlings to the plant array
# P is the existing plant array
# O is the offspring (seedlings) of the parent plants
# Will return an updated P
inc_Os <- function(P, O){
    st_IDs <- max(P[,1]) + 1;
    new_ID <- seq(from = st_IDs, to = st_IDs + dim(O)[1] - 1);
    O[,1]  <- new_ID;
    P      <- rbind(P, O);
    return(P);
}


ind_death <- function(P, pr_d = 0.1){
   is_live <- rbinom(n = dim(P)[1], size = 1, pr = 1 - pr_d);
   P       <- P[is_live == 1,];  
   return(P);
}







P <- ini_Ps(L_size = 20, N_ini = 50, shp = 5, rat = 1)
P <- competition(P, L_size = 20)
O <- reproduction(P = P, L_size = 20, N_seeds = 10, rng = 2, thresh_size = 10)
if(!is.null(O)){
    P <- inc_Os(P = P, O = O);
}






plot(x = P_sp[,2], y = P_sp[,3], pch = 16, cex = 2, col = "red", 
     xlim = c(-1, 11), ylim = c(-1, 11));
points(x = all_of[,2], y = all_of[,3])



