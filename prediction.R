nextTerm <- function(phrase) {

	# Read in the n-gram tables
	load("quingramData.RData")
	load("quadgramData.RData")
	load("trigramData.RData")
	load("bigramData.RData")
	load("unigramData.RData")
	
	# Clean up the phrase
	phrase <- as.character(phrase)
	phrase <- tolower(phrase)
	phrase <- removeNumbers(phrase)
	phrase <- removePunctuation(phrase)
	phrase <- stripWhitespace(phrase)
	
	# Split the string with a separation criteria of blank spaces between the words
	phrase <- strsplit(phrase, split = " ")

	# After the split, the class of the string is currently a "list"
	# We will simplify the list structure to produce a "vector" structure
	phrase <- unlist(phrase)

	# We will assign the "word" variable as NULL as a place holder. 
	# Once we find the next term in the string, the "word" variable will be assigned to term that was found.
	# As a result, we will stop looking for the next term given that we already found the next term. 
	word <- "null"

	# The nested if statements are model for Katz's Back-Off Model, which estimates the predictive probability of a word given its frequency in a n-gram
	# For this function, there are 5 ngrams: the Quingram (5-gram), the Quadgram (4-gram), the Trigram (3-gram), the Bigram (2-gram), and the Unigram (1-gram)
	# Concerning the "Back-Off", for a phrase WITH 4 OR MORE WORDS, we will begin with the Quingram
	# If the 1st 4 words of any of the quintets in the Quingram table match the last 4 words of the phrase
	# Then we will return the 5th word of the quintet that appears most frequently in the Quingram table
	# If not, then we will move to the Quadgram
	# If the 1st 3 words of any of the quartets in the Quadgram table match the last 3 words of the phrase
	# Then we will return the 4th word of the quartet that appears most frequently in the Quadgram table
	# If not, then we will move to the Trigram
	# If the 1st 2 words of any of the triplets in the Trigram table match the last 2 words of the phrase
	# Then we will return the 3rd word of the triplet that appears most frequently in the Trigram table
	# If not, then we will move to the Bigram
	# If the 1st word of any of the pairs in the Bigram table matches the last word of the phrase
	# Then we will return the 2nd word of the pair that appears most frequently in the Bigram table
	# If not, then we will return the most frequent word in the Unigram Table, which is the word "the"
	# The frequency of all of the n-grams have already been ordered from highest to lowest in the "buildingNgrams.R" file
	
	if (length(phrase) >= 4) {
		# We extract the LAST FOUR WORDS of the phrase
		lastFourWords <- paste(phrase[c(length(phrase) - 3, length(phrase) - 2, length(phrase) - 1, length(phrase))], sep = " ", collapse = " ")
		
		# Extract all strings from the quingramTable that contain the phrase as the FIRST FOUR WORDS
		# PLEASE NOTE: As described in the Week 1 video "Regular Expressions: Part1", the "^" metacharacter represents the start of a line and as a result,
		# will return all strings for which the last four words of the provided phrase ARE the first four words of the string in the quingram table 
		firstFourWords <- paste("^", lastFourWords, sep = "", collapse = NULL)
		quingramStrings <- quingramTable[grep(firstFourWords, quingramTable$words), ]

		# Check to see if any word quintets in the quingram table for which the 1st 4 words of the quintet matches the last 4 words of the provided phrase
		# If so, then return return the 5th word of the quintet with the highest frequency.
		# If not, then move to the Quadgram
		if (length(quingramStrings$words) > 0) {

			word <- quingramStrings$words[1]
			word <- as.character(word)
			word <- strsplit(word, split = " ")
			word <- unlist(word)
			word <- word[length(word)]

			print(word)
					
		} else if (word == "null") {
		
			# We extract the LAST THREE WORDS of the phrase
			lastThreeWords <- paste(phrase[c(length(phrase) - 2, length(phrase) - 1, length(phrase))], sep = " ", collapse = " ")
		
			# Extract all strings from the quadgramTable that contain the phrase as the FIRST THREE WORDS
			firstThreeWords <- paste("^", lastThreeWords, sep = "", collapse = NULL)
			quadgramStrings <- quadgramTable[grep(firstThreeWords, quadgramTable$words), ]

			# Check to see if there are word quartets in the quadgram table for which the 1st 3 words of the quartet matches the last 3 words of the provided phrase
			# If so, then return return the 4th word of the quartet with the highest frequency
			# If not, then move to the Trigram
			if (length(quadgramStrings$words) > 0) {

				word <- quadgramStrings$words[1]
				word <- as.character(word)
				word <- strsplit(word, split = " ")
				word <- unlist(word)
				word <- word[length(word)]

				print(word)
							
			} else if (word == "null") {
		
				# We extract the LAST TWO WORDS of the phrase
				lastTwoWords <- paste(phrase[c(length(phrase) - 1, length(phrase))], sep = " ", collapse = " ")
		
				# Extract all strings from the bigramTable that contain the phrase as the FIRST WORD
				firstTwoWords <- paste("^", lastTwoWords, sep = "", collapse = NULL)
				trigramStrings <- trigramTable[grep(firstTwoWords, trigramTable$words), ]

				# Check to see if there are word triplets in the trigram table for which the 1st 2 words of the triplet matches the last 2 words of the provided phrase
				# If so, then return return the 3rd word of the triplet with the highest frequency
				# If not, then print the message "No word found. Please try again."
				if (length(trigramStrings$words) > 0) {

				word <- trigramStrings$words[1]
				word <- as.character(word)
				word <- strsplit(word, split = " ")
				word <- unlist(word)
				word <- word[length(word)]

				print(word)
				
				} else if (word == "null") {
		
					# We extract the LAST WORD of the phrase
					lastWord <- phrase[length(phrase)]
		
					# Extract all strings from the bigramTable that contain the phrase as the FIRST WORD
					firstWord <- paste("^", lastWord, sep = "", collapse = NULL)
					bigramStrings <- bigramTable[grep(firstWord, bigramTable$words), ]

					# Check to see if there are word pairs in the bigram table for which the 1st word of the pair matches the last word of the provided phrase
					# If so, then return return the 2nd word of the pair with the highest frequency
					# If not, then print the message the most frequent word in the unigram, which is the word "the"
					if (length(bigramStrings$words) > 0) {

					word <- bigramStrings$words[1]
					word <- as.character(word)
					word <- strsplit(word, split = " ")
					word <- unlist(word)
					word <- word[length(word)]

					print(word)
						
					} else {print(as.character(unigramTable$word[1]))}
							
				} 
			}
		}

	# For a phrase with ONLY 3 WORDS, when applying the Back-Off Model, we will start with the Quadgram
	
	} else if (length(phrase) == 3) {
		# We extract the LAST THREE WORDS of the phrase
		lastThreeWords <- paste(phrase[c(length(phrase) - 2, length(phrase) - 1, length(phrase))], sep = " ", collapse = " ")
		
		# Extract all strings from the quadgramTable that contain the phrase as the FIRST THREE WORDS
		firstThreeWords <- paste("^", lastThreeWords, sep = "", collapse = NULL)
		quadgramStrings <- quadgramTable[grep(firstThreeWords, quadgramTable$words), ]

		# Check to see if any word quartets in the quadgram table for which the 1st 3 words of the quartet matches the last 3 words of the provided phrase
		# If so, then return return the 4th word of the quartet with the highest frequency.
		# If not, then move to the Trigram
		if (length(quadgramStrings$words) > 0) {

			word <- quadgramStrings$words[1]
			word <- as.character(word)
			word <- strsplit(word, split = " ")
			word <- unlist(word)
			word <- word[length(word)]

			print(word)
					
		} else if (word == "null") {
		
			# We extract the LAST TWO WORDS of the phrase
			lastTwoWords <- paste(phrase[c(length(phrase) - 1, length(phrase))], sep = " ", collapse = " ")
		
			# Extract all strings from the trigramTable that contain the phrase as the FIRST TWO WORDS
			firstTwoWords <- paste("^", lastTwoWords, sep = "", collapse = NULL)
			trigramStrings <- trigramTable[grep(firstTwoWords, trigramTable$words), ]

			# Check to see if there are word triplets in the trigram table for which the 1st 2 words of the triplet matches the last 2 words of the provided phrase
			# If so, then return return the 3rd word of the triplet with the highest frequency
			# If not, then move to the Bigram
			if (length(trigramStrings$words) > 0) {

				word <- trigramStrings$words[1]
				word <- as.character(word)
				word <- strsplit(word, split = " ")
				word <- unlist(word)
				word <- word[length(word)]

				print(word)
							
			} else if (word == "null") {
		
				# We extract the LAST WORD of the phrase
				lastWord <- phrase[length(phrase)]
		
				# Extract all strings from the bigramTable that contain the phrase as the FIRST WORD
				firstWord <- paste("^", lastWord, sep = "", collapse = NULL)
				bigramStrings <- bigramTable[grep(firstWord, bigramTable$words), ]

				# Check to see if there are word pairs in the bigram table for which the 1st word of the pair matches the last word of the provided phrase
				# If so, then return return the 2nd word of the pair with the highest frequency
				# If not, then print the message the most frequent word in the unigram, which is the word "the"
				if (length(bigramStrings$words) > 0) {

				word <- bigramStrings$words[1]
				word <- as.character(word)
				word <- strsplit(word, split = " ")
				word <- unlist(word)
				word <- word[length(word)]

				print(word)
						
				} else {print(as.character(unigramTable$word[1]))}
			}
		}

	# For a phrase with ONLY 2 WORDS, when applying the Back-Off Model, we will start with the Trigram
	} else if (length(phrase) == 2) {
		
		# Extract all strings from the trigramTable that contain the phrase as the FIRST TWO WORDS
		onlyTwoWords <- paste(phrase, sep = " ", collapse = " ")
		firstTwoWords <- paste("^", onlyTwoWords, sep = "", collapse = NULL)
		trigramStrings <- trigramTable[grep(firstTwoWords, trigramTable$words), ]

		# Check to see if there are word triplets in the trigram table for which the 1st 2 words of the triplet matches the last 2 words of the provided phrase
		# If so, then return return the 3rd word of the triplet with the highest frequency
		# If not, then move to the Bigram
		if (length(trigramStrings$words) > 0) {

			word <- trigramStrings$words[1]
			word <- as.character(word)
			word <- strsplit(word, split = " ")
			word <- unlist(word)
			word <- word[length(word)]

			print(word)
		
		} else if (word == "null") {
		
			# We extract the LAST WORD of the phrase
			lastWord <- phrase[length(phrase)]
		
			# Extract all strings from the bigramTable that contain the phrase as the FIRST WORD
			firstWord <- paste("^", lastWord, sep = "", collapse = NULL)
			bigramStrings <- bigramTable[grep(firstWord, bigramTable$words), ]

			# Check to see if there are word pairs in the bigram table for which the 1st word of the pair matches the last word of the provided phrase
			# If so, then return return the 2nd word of the pair with the highest frequency
			# If not, then print the message the most frequent word in the unigram, which is the word "the"
			if (length(bigramStrings$words) > 0) {

				word <- bigramStrings$words[1]
				word <- as.character(word)
				word <- strsplit(word, split = " ")
				word <- unlist(word)
				word <- word[length(word)]

				print(word)
						
			} else {print(as.character(unigramTable$word[1]))}
		}
	
	# For a phrase with ONLY 1 WORD, when applying the Back-Off Model, we will start with the Bigram
	# Instead, we will only look in the Bigram	
	} else if (length(phrase) == 1) {

		# Extract all strings from the bigramTable that contain the phrase as the FIRST WORD
		firstWord <- paste("^", phrase, sep = "", collapse = NULL)
		bigramStrings <- bigramTable[grep(firstWord, bigramTable$words), ]

		# Check to see if there are word pairs in the bigram table for which the 1st word of the pair matches the last word of the provided phrase
		# If so, then return return the 2nd word of the pair with the highest frequency
		# If not, then print the message the most frequent word in the unigram, which is the word "the"
		if (length(bigramStrings$words) > 0) {

			word <- bigramStrings$words[1]
			word <- as.character(word)
			word <- strsplit(word, split = " ")
			word <- unlist(word)
			word <- word[length(word)]

			print(word)
			
		} else {print(as.character(unigramTable$word[1]))}		
	}							
}