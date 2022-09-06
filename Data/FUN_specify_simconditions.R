selection_error = list(
  list(matrix(c(.95, .05, .05, .95), ncol=2, byrow=T),                      # 5% SE + weak
       matrix(c(.95, .05, .05, .95), ncol=2, byrow=T),
       matrix(c(.95, .05, .05, .95), ncol=2, byrow=T),
       matrix(c(.95, .05, .05, .95), ncol=2, byrow=T),
       matrix(c(.5,  .5,  .5,  .5),  ncol=2, byrow=T)
  ), 
  list(matrix(c(.95, .05, .05, .95), ncol=2, byrow=T),                      # 5% SE + strong
       matrix(c(.95, .05, .05, .95), ncol=2, byrow=T),
       matrix(c(.95, .05, .05, .95), ncol=2, byrow=T),
       matrix(c(.95, .05, .05, .95), ncol=2, byrow=T),
       matrix(c(.7,  .3,  .3,  .7),  ncol=2, byrow=T)
  ),
  list(matrix(c(.8,  .2,  .2,  .8),  ncol=2, byrow=T),                      # 20% SE + weak
       matrix(c(.8,  .2,  .2,  .8),  ncol=2, byrow=T),
       matrix(c(.8,  .2,  .2,  .8),  ncol=2, byrow=T),
       matrix(c(.8,  .2,  .2,  .8),  ncol=2, byrow=T),
       matrix(c(.5,  .5,  .5,  .5),  ncol=2, byrow=T)
  ), 
  list(matrix(c(.8,  .2,  .2,  .8),  ncol=2, byrow=T),                      # 20% SE + strong
       matrix(c(.8,  .2,  .2,  .8),  ncol=2, byrow=T),
       matrix(c(.8,  .2,  .2,  .8),  ncol=2, byrow=T),
       matrix(c(.8,  .2,  .2,  .8),  ncol=2, byrow=T),  
       matrix(c(.7,  .3,  .3,  .7),  ncol=2, byrow=T))
)

measurement_error = list(
  list(matrix(c(.95, .025, .025, .025, .95, .025, .025, .025, .95), ncol=3, byrow=TRUE), # 5% ME + weak
       matrix(c(.95, .025, .025, .025, .95, .025, .025, .025, .95), ncol=3, byrow=TRUE), 
       matrix(c(.95, .025, .025, .025, .95, .025, .025, .025, .95), ncol=3, byrow=TRUE), 
       matrix(c(.95, .025, .025, .025, .95, .025, .025, .025, .95), ncol=3, byrow=TRUE), 
       matrix(c(1/3,  1/3,  1/3,  1/3, 1/3,  1/3,  1/3,  1/3, 1/3), ncol=3, byrow=TRUE)
  ),
  list(matrix(c(.95, .025, .025, .025, .95, .025, .025, .025, .95), ncol=3, byrow=TRUE), # 5% ME + strong
       matrix(c(.95, .025, .025, .025, .95, .025, .025, .025, .95), ncol=3, byrow=TRUE), 
       matrix(c(.95, .025, .025, .025, .95, .025, .025, .025, .95), ncol=3, byrow=TRUE), 
       matrix(c(.95, .025, .025, .025, .95, .025, .025, .025, .95), ncol=3, byrow=TRUE), 
       matrix(c(.7, .15, .15, .15, .7, .15, .15, .15, .7), ncol=3, byrow=TRUE)
  ),
  list(matrix(c(.8,   .1,  .1,  .1,  .8,  .1,  .1,  .1,  .8), ncol=3, byrow=TRUE), # 20% ME + weak
       matrix(c(.8,   .1,  .1,  .1,  .8,  .1,  .1,  .1,  .8), ncol=3, byrow=TRUE), 
       matrix(c(.8,   .1,  .1,  .1,  .8,  .1,  .1,  .1,  .8), ncol=3, byrow=TRUE), 
       matrix(c(.8,   .1,  .1,  .1,  .8,  .1,  .1,  .1,  .8), ncol=3, byrow=TRUE), 
       matrix(c(1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3), ncol=3, byrow=TRUE)
  ), 
  list(matrix(c(.8,  .1,  .1,  .1, .8,  .1,  .1, .1,  .8), ncol=3, byrow=TRUE), # 20% ME + strong
       matrix(c(.8,  .1,  .1,  .1, .8,  .1,  .1, .1,  .8), ncol=3, byrow=TRUE), 
       matrix(c(.8,  .1,  .1,  .1, .8,  .1,  .1, .1,  .8), ncol=3, byrow=TRUE), 
       matrix(c(.8,  .1,  .1,  .1, .8,  .1,  .1, .1,  .8), ncol=3, byrow=TRUE), 
       matrix(c(.7, .15, .15, .15, .7, .15, .15, .15, .7), ncol=3, byrow=TRUE)
  ) 
)

simconds = function(){
  #de eerste is steeds covariaat sterk met sel, en de tweede covariaat sterk met meas
  A_sel5meas5_covsel    = list(selection_error[[2]], measurement_error[[1]]) 
  A_sel5meas5_covmeas       = list(selection_error[[1]], measurement_error[[2]])
  B_sel5meas20_covsel   = list(selection_error[[2]], measurement_error[[3]])
  B_sel5meas20_covmeas       = list(selection_error[[1]], measurement_error[[4]])
  C_sel20meas5_covsel   = list(selection_error[[4]], measurement_error[[1]])
  C_sel20meas5_covmeas     = list(selection_error[[3]], measurement_error[[2]])
  D_sel20meas20_covsel  = list(selection_error[[4]], measurement_error[[3]])
  D_sel20meas20_covmeas     = list(selection_error[[3]], measurement_error[[4]])
  
  return(list(A_sel5meas5_covsel, 
              A_sel5meas5_covmeas,
              B_sel5meas20_covsel,
              B_sel5meas20_covmeas,
              C_sel20meas5_covsel,
              C_sel20meas5_covmeas,
              D_sel20meas20_covsel,
              D_sel20meas20_covmeas
  ))}