

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
    





P <- ini_Ps(L_size = 10, N_ini = 200);
P <- competition(P = P, L_size = 10);


