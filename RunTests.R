library(sudoku)
library(neuralnet)

# The list of algorithms to use
algorithms = c('backprop', 'rprop+', 'rprop-', 'sag', 'slr')
# Error functions
err_fncs = c('sse', 'ce')
# Activation functions
act_fncs = c('logistic', 'tanh')
# Learning rates
learningrates = c(NULL, 0.00001, 0.0002, 0.005, 0.05)
# Network shapes
net_shapes = list(c(3, 6, 3),
                  #NS
                  c(10, 15, 10),
                  #NM
                  c(25, 30, 30,
                    25),
                  #NL
                  c(1),
                  #TS
                  c(100),
                  #TL
                  c(1, 1, 1, 1, 1),
                  #WS
                  c(25, 25, 25, 25, 25)) #WL

# Sample size
total_board_count = 15

newSudokuBoard <- function() {
  sb = generateSudoku(print.it = F)
  sol = solveSudoku(sb, print.it = F)
  dim(sb) <- c(1, 81) # Single row of values.
  dim(sol) <- c(1, 81)
  res = cbind(sb, sol)
}

genboards = function(count) {
  if (count > 0) {
    boards = newSudokuBoard()
    if (count > 1)
      for (i in 2:count)
        boards = rbind(boards, newSudokuBoard())
      as.data.frame(boards)
  } else {
    data.frame()
  }
}

evalU8 = function(net, boardcount, xnam, ynam) {
  newBoards = genboards(boardcount)
  colnames(newBoards[1:81]) <- xnam
  colnames(newBoards[82:162]) <- ynam
  res = round(predict(net, newBoards))
  
  
  # plot(net)
  # print(res)
  for (ri in 1:nrow(res)) {
    for (ci in 1:ncol(res)) {
      res[ri, ci] = net$err.fct(newBoards[ri, 81 + ci], res[ri, ci])
    }
  }
  paste("Error on new dataset:", sum(res))
}

iteration_count = 0

performTests = function() {
  for (algo in algorithms) {
    for (error_function in err_fncs) {
      for (activation_function in act_fncs) {
        for (network_shape in net_shapes) {
          if (algo == 'backprop') {
            for (lr in learningrates) {
              iteration_count = iteration_count + 1
              
              try({
                print(
                  paste(
                    "algo=",
                    algo,
                    ", errf=",
                    error_function,
                    ", actf=",
                    activation_function,
                    ", ntwk=[",
                    paste(network_shape, collapse = ','),
                    "], bplr=",
                    lr
                    ,
                    sep = ""
                  )
                )
                xnam <- paste0("V", 1:81)
                ynam <- paste0("V", 82:162)
                
                dat = genboards(total_board_count)
                colnames(dat[1:81]) <- xnam
                colnames(dat[82:162]) <- ynam
                
                fmla <-
                  as.formula(paste(paste(
                    paste(ynam, collapse = "+"), " ~ "
                  ), paste(xnam, collapse = "+")))
                
                net = neuralnet(
                  fmla,
                  dat,
                  network_shape,
                  err.fct = error_function,
                  algorithm = algo,
                  act.fct = activation_function,
                  learningrate = lr, rep=0
                )
                
                
                message(evalU8(net, 10, xnam, ynam))
              }, silent = T)
              
            }
          } else {
            iteration_count = iteration_count + 1
            
            
            try({
              print(
                paste(
                  "algo=",
                  algo,
                  ", errf=",
                  error_function,
                  ", actf=",
                  activation_function,
                  ", ntwk=[",
                  paste(network_shape, collapse = ','),
                  "]"
                  ,
                  sep = ""
                )
              )
              xnam <- paste0("V", 1:81)
              ynam <- paste0("V", 82:162)
              
              dat = genboards(total_board_count)
              colnames(dat[1:81]) <- xnam
              colnames(dat[82:162]) <- ynam
              
              fmla <-
                as.formula(paste(paste(
                  paste(ynam, collapse = "+"), " ~ "
                ), paste(xnam, collapse = "+")))
              
              net = neuralnet(
                fmla,
                dat,
                network_shape,
                err.fct = error_function,
                algorithm = algo,
                act.fct = activation_function, rep=0
              )
              message(evalU8(net, 10, xnam, ynam))
            }, silent = T)
          }
        }
      }
    }
  }
}

performTests()

# algo = algorithms[1]
# error_function=err_fncs[1]
# activation_function=act_fncs[2]
# network_shape=c(25, 25, 25, 25, 25)
# 
# print(
#   paste(
#     "algo=",
#     algo,
#     ", errf=",
#     error_function,
#     ", actf=",
#     activation_function,
#     ", ntwk=[",
#     paste(network_shape, collapse = ','),
#     "]"
#     ,
#     sep = ""
#   )
# )
# xnam <- paste0("V", 1:81)
# ynam <- paste0("V", 82:162)
# 
# dat = genboards(total_board_count)
# colnames(dat[1:81]) <- xnam
# colnames(dat[82:162]) <- ynam
# 
# fmla <-
#   as.formula(paste(paste(
#     paste(ynam, collapse = "+"), " ~ "
#   ), paste(xnam, collapse = "+")))
# 
# net = neuralnet(
#   fmla,
#   dat,
#   network_shape,
#   err.fct = error_function,
#   algorithm = algo,
#   act.fct = activation_function, rep=0,
#   learningrate = learningrates[4]
# )
# 
# 
# message(evalU8(net, 10, xnam, ynam))
# 
# ### Print results of a net on a brand new board.
# newBoards = genboards(1)
# colnames(newBoards[1:81]) <- xnam
# 
# colnames(newBoards[82:162]) <- ynam
# res = round(predict(net, newBoards))
# print(res)
