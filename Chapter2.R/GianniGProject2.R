hand1 <- c("ace", "king", "queen", "jack", "ten", "spades", "spades", 
           "spades", "spades", "spades")

matrix(hand1, nrow = 5)
matrix(hand1, ncol = 2)
dim(hand1) <- c(5, 2)

now <- Sys.time()

card <- list("ace", "hearts", 1)
card

dframe1 <- data.frame(face = c("ace", "two", "six"),  
                 suit = c("clubs", "clubs", "clubs"), value = c(1, 2, 3))
dframe1

dframe1 <- data.frame(face = c("ace", "two", "six"),  
                 suit = c("clubs", "clubs", "clubs"), value = c(1, 2, 3),
                 stringsAsFactors = FALSE)
deck <- data.frame(
  face = c("king", "queen", "jack", "ten", "nine", "eight", "seven", "six",
           "five", "four", "three", "two", "ace", "king", "queen", "jack", "ten", 
           "nine", "eight", "seven", "six", "five", "four", "three", "two", "ace", 
           "king", "queen", "jack", "ten", "nine", "eight", "seven", "six", "five", 
           "four", "three", "two", "ace", "king", "queen", "jack", "ten", "nine", 
           "eight", "seven", "six", "five", "four", "three", "two", "ace"),  
  suit = c("spades", "spades", "spades", "spades", "spades", "spades", 
           "spades", "spades", "spades", "spades", "spades", "spades", "spades", 
           "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", 
           "clubs", "clubs", "clubs", "clubs", "clubs", "diamonds", "diamonds", 
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", 
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "hearts", 
           "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", 
           "hearts", "hearts", "hearts", "hearts", "hearts"), 
  value = c(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 13, 12, 11, 10, 9, 8, 
            7, 6, 5, 4, 3, 2, 1, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 13, 12, 11, 
            10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
)

head(deck)

write.csv(deck, file = "cards.csv", row.names = FALSE)

deck[1, 1]

deck[1, c(1, 2, 3)]

new <- deck[1, c(1, 2, 3)]

new

deal <- function(cards) {
  cards[1, ]
}

deal(deck)

random <- sample(1:52, size = 52)

deck.r <- deck[random, ]
head(deck.r)

shuffle <- function(cards) { 
  random <- sample(1:52, size = 52)
  cards[random, ]
}

deck2 <- shuffle(deck)

deal(deck2)

deck$value

deck2 <- deck

vec <- c(0, 0, 0, 0, 0, 0)
vec[1]
vec[1] <- 1000
vec[c(1, 3, 5)] <- c(1, 1, 1)
vec[4:6] <- vec[4:6] + 1
vec[7] <- 0
vec

deck2$new <- 1:52
deck2$new <- NULL
head(deck2)
deck2$value[c(13, 26, 39, 52)] <- 14
head(deck2, 13)

deck3 <- shuffle(deck)
head(deck3)

show_env <- function(){
  list(ran.in = environment(), 
       parent = parent.env(environment()), 
       objects = ls.str(environment()))
}
show_env()

deal <- function() {
  deck[1, ]
}

environment(deal)

deal <- function() {
  card <- deck[1, ]
  assign("deck", deck[-1, ], envir = globalenv())
  card
}

deal()

shuffle <- function(){
  random <- sample(1:52, size = 52)
  assign("deck", DECK[random, ], envir = globalenv())
}

  
shuffle()

setup <- function(deck) {
  DECK <- deck
  
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = globalenv())
    card
  }
  
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = globalenv())
  }
}

setup <- function(deck) {
  DECK <- deck
  
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = globalenv())
    card
  }
  
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = globalenv())
  }
  
  list(deal = DEAL, shuffle = SHUFFLE)
}

cards <- setup(deck)



  
