get_symbols <- function() {
  wheel <- c("DD","7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}
 get_symbols()
 
 play <- function() {
  
   symbols <- get_symbols()
   print(symbols)
   score(symbols)
 }
 
# outline for slot machine
 
 score <- function (symbols) {
   # identify
   same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
   bars <- symbols %in% c("B", "BB", "BBB")
   
   # get prize
   if (same) {
     payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
                  "B" = 10, "C" = 10, "0" = 0)
     prize <- unname(payouts[symbols[1]])
   } else if (all(bars)) {
     prize <- 5
   } else {
     cherries <- sum(symbols == "C")
     prize <- c(0, 2, 5)[cherries + 1]
   }
   
   # adjust for diamonds
   diamonds <- sum(symbols == "DD")
   prize * 2 ^ diamonds
 }

 # setting up play function
 play <- function() {
   symbols <- get_symbols()
   print(symbols)
   score(symbols)
 }
 
play()
symbols <- c("7", "7", "7")
symbols <- c("B", "BB", "BBB")
symbols <- c("C", "DD", "0")

#Testing
symbols

symbols[1] == symbols[2] & symbols[2] == symbols[3]


symbols[1] == symbols[2] & symbols[1] == symbols[3]


all(symbols == symbols[1])

symbols <- c("B", "BBB", "BB")

all(symbols %in% c("B", "BB", "BBB"))

# Assigning values to symbols 
payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
             "B" = 10, "C" = 10, "0" = 0)

# more testing
symbols <- c("7", "7", "7")
symbols[1]

payouts[symbols[1]]

symbols <- c("C", "C", "C")
payouts[symbols[1]]

# polishing slot machine
play <- function() {
  symbols <- get_symbols()
  prize <- score(symbols)
  attr(prize, "symbols") <- symbols
  prize
}
play()

slot_display <- function(prize){
  
symbols <- attr(prize, "symbols")
symbols <- paste(symbols, collapse = " ")
string <- paste(symbols, prize, sep = "\n$")
  cat(string)
}

three_play <- play()
slot_display(three_play)

slot_display(three_play)

print.slots <- function(x, ...) {
  slot_display(x)
}

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols, class = "slots")
}

class(play())

play()
# messing with probabilities and making loops
wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)
combos

prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, 
          "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)
combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]

head(combos, 3)

combos$prob <- combos$prob1 * combos$prob2 * combos$prob3

sum(combos$prob)
combos$prize <- NA

for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}

sum(combos$prize * combos$prob)

plays_till_broke <- function(start_with) {
  cash <- start_with
  n <- 0
  while (cash > 0) {
    cash <- cash - 1 + play()
    n <- n + 1
  }
  n
}

plays_till_broke(100)


