

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

