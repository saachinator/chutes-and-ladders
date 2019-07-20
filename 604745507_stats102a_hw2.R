
# UID: 604745507

# representation of a board:
# first part of list is number of rows, second is number of columns, third is list of all chutes, fourth is list of all ladders
default_board <- list(10, 10, c(6, 16, 11, 49, 19, 62, 24, 87, 26, 47, 53, 56, 60, 64, 73, 93, 75, 95, 78, 98), c(1, 38, 4, 14, 9, 31, 21, 42, 28, 84, 36, 44, 51, 67, 71, 91, 80, 100))

show_board <- function(board){
  plot(board[[1]], board[[2]], xlim=c(0,board[[2]]), ylim=c(0,board[[1]]), ann=FALSE)
  grid(board[[1]], board[[2]])
  numIterations <- length(board[[3]])/2
  count <- 1
  while(numIterations != 0){ # adding in the chutes
    x <- 0
    y <- 0
    if (board[[3]][count] < board[[3]][count+1]){
      x <- board[[3]][count+1]
      y <- board[[3]][count]
    } else {
      y <- board[[3]][count+1]
      x <- board[[3]][count]
    }
    x1 <- x%%board[[2]]
    y1 <- x/board[[2]]
    x2 <- y%%board[[2]]
    y2 <- y/board[[2]]
    
    arrows(x1, y1, x2, y2, col = "red")
    count <- count + 2
    numIterations <- numIterations - 1
  }
  numIterations <- length(board[[4]])/2
  count <- 1
  while(numIterations != 0){ # adding in the ladders
    x <- 0
    y <- 0
    if (board[[4]][count] > board[[4]][count+1]){
      x <- board[[4]][count+1]
      y <- board[[4]][count]
    } else {
      y <- board[[4]][count+1]
      x <- board[[4]][count]
    }
    x1 <- x%%board[[2]]
    y1 <- x/board[[2]]
    x2 <- y%%board[[2]]
    y2 <- y/board[[2]]
    
    arrows(x1, y1, x2, y2, col = "green")
    count <- count + 2
    numIterations <- numIterations - 1
  }
} 

play_c1 <- function(board, n_players = 1, spinner){
  if (n_players < 1){
    return("Please input a valid number of players (at least 1)")
  }
  spinner_vector <- c(mode= "numeric")
  if (length(spinner) == 1){
    spinner_vector <- c(1:spinner)
  } else {
    spinner_vector <- spinner
  } # ---------- works
  
  board_vector <- make_list_from_board(board)
  
  winner <- 0
  sixth_turn <- replicate(n_players, 0) # tile person is on at end of 6th turn
  is_close <- FALSE # tells us if the game was close
  winner_found <- FALSE
  everyones_place <- replicate(n_players, 0) # tile person is on
  everyones_status <- replicate(n_players, FALSE) # checks to see who is finished
  everyones_finished <- FALSE # true if everyone is finished
  turns <- replicate(n_players, 0) # number of turns per player
  chutes <- replicate(n_players, 0) #number of chutes per player
  ladders <- replicate(n_players, 0) # number of ladders per player
  
  while(everyones_finished == FALSE) {
    # want to create a loop that does one round of plays
    for (i in 1:n_players){
      current_addition <- sample(spinner_vector, 1)
      if ((everyones_place[i] + current_addition) <= length(board_vector)){
        everyones_place[i] <- everyones_place[i] + current_addition
        turns[i] <- turns[i] + 1
        if(turns[i] == 6){
          sixth_turn[i] <- everyones_place[i]
        }
      }
      if (board_vector[everyones_place[i]] != 0 && everyones_place[i] != 0 && everyones_place[i] != 100){
        value_to_match <- board_vector[everyones_place[i]]
        if(value_to_match %%2 == 0){ # means this is a ladder
          for (x in ((everyones_place[i] + 1):length(board_vector))){
            if (board_vector[x] == value_to_match){
              everyones_place[i] <- x
              ladders[i] <- ladders[i] + 1
            }
          }
        } else { # means this is a chute
          for (x in (1 : (everyones_place[i]-1))){
            if (board_vector[x] == value_to_match){
              everyones_place[i] <- x
              chutes[i] <- chutes[i] + 1
            }
          }
        }
      }
      
      if (everyones_place[i] == length(board_vector)){
        everyones_status[i] <- TRUE
        if (winner_found == FALSE){
          winner <- i
          winner_found <- TRUE
          # check to see if it was close
          count <- 0
          for (y in 1:n_players){
            if((everyones_place[y] + max(board_vector)) >= length(board_vector)){
              count <- count + 1
            }
          }
          if (count >= 2){
            is_close <- TRUE
          }
        }
      }
    }
    if(all(everyones_status)){
      everyones_finished <- TRUE
    }
  }
  turns_done <- 0
  total_turns <- 0
  for (i in 1:n_players){
    if (turns[i] > turns_done){
      turns_done <- turns[i]
    }
    total_turns <- total_turns + turns[i]
  }
  toReturn <- list(winner, turns, chutes, ladders, turns_done, is_close, sixth_turn, total_turns)
  return(toReturn)
}

make_random_board <- function(n_rows = 10, n_cols = 10, n_chutes = 10, n_ladders = 9){
  num_tiles <- n_rows*n_cols
  if ((n_chutes + n_ladders) > ((num_tiles-2)/2)){
    return("Please input a valid number of chutes and ladders")
  }
  board_vector <- replicate(num_tiles, 0) # lets us know if there's something there already
  # set your chutes
  chutes_vector <- numeric()
  count <- 0
  while(count != n_chutes*2){
    current <- sample((2:(num_tiles-1)), 1)
    if(board_vector[current] == 0 && board_vector[current + 1] == 0 && board_vector[current-1] == 0){
      board_vector[current] <- 1
      chutes_vector <- c(chutes_vector, current)
      count <- count + 1
    } 
  }

  # set your ladders
  ladders_vector <- numeric()
  count <- 0
  while(count != n_ladders*2){
    current <- sample((1:num_tiles), 1)
    if(board_vector[current] == 0){
      board_vector[current] <- 1
      ladders_vector <- c(ladders_vector, current)
      count <- count + 1
    } 
  }
  toReturn <- list(n_rows, n_cols, chutes_vector, ladders_vector)
  return(toReturn)
}

make_list_from_board <- function(board){ 
  board_vector <- replicate((board[[1]]*board[[2]]), 0)
  # adding in the chutes, all represented by odd numbers
  start_number <- 1
  num_iterations_chutes <- length(board[[3]])/2
  start_chutes <- 1
  for (i in (1:num_iterations_chutes)){
    board_vector[board[[3]][start_chutes]] <- start_number
    board_vector[board[[3]][start_chutes+1]] <- start_number
    start_number <- start_number + 2
    start_chutes <- start_chutes + 2
  }
  # adding in the ladders, all represented by even numbers
  start_number <- 2
  num_iterations_ladders <- length(board[[4]])/2
  start_ladders <- 1
  for (i in (1:num_iterations_ladders)){
    board_vector[board[[4]][start_ladders]] <- start_number
    board_vector[board[[4]][start_ladders+1]] <- start_number
    start_number <- start_number + 2
    start_ladders <- start_ladders + 2
  }
  return(board_vector)
} 

simulate_game_10000_times <- function(seed, board, nplayers, spinner){
  set.seed(seed)
  count <- 10000
  ans1 <- 10000 # got it
  ans2 <- replicate(nplayers, 0) # got it, outputs NUMBER of games won
  helper_3 <- numeric()
  ans3 <- 0 # got it, outputs NUMBER of games
  ans4 <- 0 # got it, outputs NUMBER of games close
  ans5 <- 0 # got it
  helper_6 <- numeric()
  ans6 <- 0 # got it
  total_turns_needed <- numeric()
  while (count != 0){
    results <- play_c1(board, nplayers, spinner)
    # ans 1
    total_turns_needed <- c(total_turns_needed, results[[8]])
    if (results[[5]] < ans1){
      ans1 <- results[[5]]
    }
    if (results[[5]] > ans5){
      ans5 <- results[[5]]
    }
    if (results[[6]]){
      ans4 <- ans4 + 1
    }
    helper_3 <- c(helper_3, results[[5]])
    for (i in 1:nplayers){
      helper_6 <- c(helper_6, (results[[7]][i]))
    }
    ans2[results[[1]]] <- ans2[results[[1]]] + 1
    count <- count - 1
  }
  for (i in 1: length(helper_3)){
    if (helper_3[i] == ans1){
      ans3 <- ans3 + 1
    }
  }
  ans6 <- getMode(helper_6)
  # parse through helper_3 and find how many were equivalent to ans1
  toReturn <- list(ans1, ans2, ans3, ans4, ans5, ans6)
  plot(total_turns_needed)
  return(toReturn)
}

question5 <- function(total_turns, value){
  count <- 0
  for (i in 1:length(total_turns)){
    if (total_turns[i] < value){
      count <- count + 1
    }
  }
  return(count)
}

getMode <- function(x) {
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

