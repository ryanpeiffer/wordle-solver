
#######################setup
library(tidyverse)

all_wordles <- read_csv("wordle_answers.csv")[[1]]
all_guesses <- read_csv("wordle_guesses.csv")[[1]]

#function to calculate relative frequency of letters for remaining words
#input word_list is a vector of remaining possible final words
find_letter_frequencies <- function(word_list) {
    wordle_freqs <- tibble(word = word_list) %>%
                        separate(word, into = c("l1", "l2", "l3", "l4", "l5"), sep = 1:4) %>%
                        pivot_longer(everything()) %>%
                        select(value) %>%
                        group_by(value) %>%
                        tally() %>%
                        arrange(desc(n)) %>%
                        mutate(rank = row_number()) %>%
                        select(value, rank)
    
    tibble(value = tolower(LETTERS)) %>%
        left_join(wordle_freqs, by = "value") %>%
        replace_na(list(rank = 999))
}

#function to calculate a frequency score for a word
#inputs are the word, and "letter_rankings" which is a dataframe of letters and their rank with colnames (value, rank)
#note that lower scores are better, since it's a ranking of frequencies
score_word <- function(word, letter_rankings) {
    letters <- unlist(strsplit(word, split = ""))
    scores <- letter_rankings$rank[match(letters, letter_rankings$value)]
    sum(scores)
}

#word_list is a vector of words that you want to use as valid guesses
#letter_rankings is a dataframe of letters and their frequency rank with colnames (value, rank)
find_best_word <- function(word_list, letter_rankings) {
    scores <- unlist(lapply(word_list, score_word, letter_rankings = letter_rankings))
    word_list[match(min(scores), scores)]
}

#function to score a guess
rate_guess <- function(word, correct_word) {
    word_letters <- unlist(strsplit(word, split = ""))
    correct_letters <- unlist(strsplit(correct_word, split = ""))
    perfects <- 2*(word_letters == correct_letters)
    mismatches <- 1*(word_letters %in% correct_letters)
    pmax(perfects, mismatches) #return vector of 0=not in answer, 1=wrong place, 2=correct
}

#function to filter the list of valid guesses & possible answers based on guess feedback
revise_list <- function(word_list, word, word_score) {
    word_letters <- unlist(strsplit(word, split = ""))
    wrong_letters <- word_letters[word_score == 0]
    wrong_letters_regex <- ifelse(length(wrong_letters) > 0,  
                                  paste0("[^",paste(wrong_letters, collapse = ''),"]"),
                                  "[a-z]")
    check_regex <- paste(ifelse(word_score == 2, word_letters, wrong_letters_regex), collapse = '')
    imperfect_letters <- word_letters[word_score == 1]
    
    #filter for wrong and right letters with one handy regex 
    filtered_list <- grep(check_regex, word_list, value = TRUE)

    #ugly way of handling the imperfect letters, as I can't find a regex way to solve at once
    for(letter in imperfect_letters) {
        filtered_list <- grep(letter, filtered_list, value = TRUE)
    }
    
    #remove the actual guessed word if it's still in the list (shouldn't be, but has happened in testing)
    final_list <- filtered_list[filtered_list != word] 
    final_list
}

#####################play the game!
#for now, our ranking function is pretty basic, and favors the word "areae" as the first guess.
#obviously that's kind of dumb, right now as a shortcut I'm only going to guess words with 5 unique letters.
#eventually I should make this smarter.
play_wordle <- function(correct_word) {

    guess_list <- sapply(all_guesses, strsplit, split = "")
    guess_list <- lapply(guess_list, function(x) length(unique(x)))
    guess_list <- names(guess_list[guess_list == 5])
    wordle_list <- all_wordles
    letter_rankings <- find_letter_frequencies(wordle_list)
    
    guesses <- c()
    scores <- c()
    
    for(guess in 1:6) {
        if(guess > 1) {
            guess_list <- revise_list(guess_list, guessed_word, guess_score)
            wordle_list <- revise_list(wordle_list, guessed_word, guess_score)
            letter_rankings <- find_letter_frequencies(wordle_list)
        }
        
        #not sure if this is awesome logic, but will guess from only the wordle list once at round 3
        if(guess > 2) {
            guessed_word <- find_best_word(wordle_list, letter_rankings)
        } else {
            guessed_word <- find_best_word(guess_list, letter_rankings)
            
        }
        guess_score <- rate_guess(guessed_word, correct_word)
        guesses <- c(guesses, guessed_word)
        scores <- c(scores, paste(guess_score, collapse = ''))
        
        if(sum(guess_score) == 10) {break}
    }
    
    scoreboard <- data.frame("target" = rep(correct_word, length(guesses)),
                             "guess" = guesses,
                             "score" = scores)
    scoreboard
}




### loop to play wordle across a bunch of words
wordle_play_list <- all_wordles
games <- data.frame(target = c(), guess = c(), score = c())

t1 <- Sys.time()

for(word in wordle_play_list) {
    game <- play_wordle(word)
    games <- rbind(games, game)
}

t2 <- Sys.time()

### analyze the results
results <- games %>%
    group_by(target) %>%
    mutate(win = max(target == guess),
           rounds = n()) %>%
    select(target, win, rounds) %>%
    distinct() %>%
    ungroup()

win_tally <- results %>% 
    group_by(rounds) %>% 
    arrange(rounds, desc = T) %>%
    mutate(rounds = ifelse(win == 1, as.character(rounds), "loss")) %>%
    tally() %>%
    mutate(percent = sprintf("%.1f", n/sum(n)*100)) %>%  
    select(rounds, percent)

avg_guesses <- win_tally %>%
    filter(rounds != 'loss') %>%
    mutate(rounds = as.integer(rounds),
           percent = as.double(percent)) %>%
    mutate(sumprod = rounds * percent)

avg_guess <- sum(avg_guesses$sumprod) / sum(avg_guesses$percent)

### display results
print(paste0(length(wordle_play_list), " wordles completed"))
print(t2 - t1)
win_tally
round(avg_guess,2)
