# Libraries for this file                 # Functions in this file
#_______________________                  #_______________________
library(openNLP)                          # TagPOS()
library(stringr)                          # PluralOfException()
library(NLP)                              # IsPlural()
#_______________________                  # PluralGenerator()
#                                         # WordPlural()
#                                         #_______________________

# Clears the R memory as TagPOS tends to take up a lot of memory quickly. Use when required.
# rm(list = ls())

#######-----------------------------------  TagPOS()  ----------------------------------#######

# Purpose: Tags the Parts Of Speech of a character (i.e as nouns verbs etc)

# Arguments: (Character, ...)

# Returns: (Listof (Character, Character Vector))

# NOTE: It is best not to mess with this function unless you know what you're doing
#       Refer to 'Read Me.txt' Section 4 for more info.

TagPOS <-  function(x, specific.Word = NULL, untag.Token = FALSE) 
{
      s <- as.String(x)
      
      # Need sentence and word token annotations
      word_token_annotator <- Maxent_Word_Token_Annotator()
      a2 <- Annotation(1L, "sentence", 1L, nchar(s))
      a2 <- annotate(s, word_token_annotator, a2)
      a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
      
      # Determine the distribution of POS tags for word tokens
      a3w <- a3[a3$type == "word"]
      pos.Tags <- unlist(lapply(a3w$features, `[[`, "POS"))
      
      if(untag.Token == TRUE)
      {
            # Extract token (all of them)
            pos.Tagged <- paste(sprintf("%s", s[a3w]))
      }
      
      else
      {
            # Extract token/POS pairs (all of them)
            pos.Tagged <- paste(sprintf("%s/%s", s[a3w], pos.Tags), collapse = " ")
      }
      
      # If a specific word to look for is provided
      if(!is.null(specific.Word))
      {
            split.Sentence <- strsplit(pos.Tagged[1], "[[:space:]]+")
            pos.Tagged <- grep(specific.Word, split.Sentence[[1]], value="TRUE")
            
            pos.Tags <- substring(sub("^[^/]*", "", pos.Tagged) , 2)
      }
      
      # Creates a list with POStagged and POStags as elements
      tag.List <- list(POStagged = pos.Tagged, POStags = pos.Tags)
      tag.List
}
#######---------------------------------------------------------------------------------#######

#######-----------------------------  PluralOfException()  -----------------------------#######

# Purpose: To check if the given word is an exception word for plurals.

# Arguments: (Character)

# Constants: kdefault.Exceptions.File (Character)

# Returns: (Character Vector OR NULL)

kdefault.Exceptions.File <- "PluralExceptionsList.txt"

exceptions.Scan <- scan(file = kdefault.Exceptions.File, what = "", sep = "\n",
                        comment.char = "#")

PluralOfException <- function(singular.Noun, file.Name = kdefault.Exceptions.File)
{     
      if(file.Name != kdefault.Exceptions.File)
      {
            # Scans in the file with plural exception words
            exceptions.Scan <- scan(file = file.Name, what="", sep = "\n", 
                                    comment.char = "#")     
      }
      
      # Separate elements by one or more whitepace
      exceptions.List <- strsplit(exceptions.Scan, "[[:space:]]+")
      
      # Extract the first vector element and set it as the list element name
      names(exceptions.List) <- sapply(exceptions.List, function(x) x[[1]]) 
      
      # Remove the first vector element from each list element
      exceptions.List <- lapply(exceptions.List, function(x) x[-1])
      
      return(exceptions.List[[singular.Noun]])
}
#######---------------------------------------------------------------------------------#######

#######----------------------------------  IsPlural()  ---------------------------------#######

# Purpose: Returns TRUE if the words are in plural form, else FALSE

# Arguments: (Character Vector or Character)

# Returns: (Boolean)

IsPlural <- function(noun.Vector)
{
      noun.Vector.Len <- length(noun.Vector)
      
      is.Noun.Vector <- character(noun.Vector.Len)
      
      for(i in 1 : noun.Vector.Len)
      {
            tag.POS <- TagPOS(noun.Vector[i])
            
            if((tag.POS$POStags == "NNS") || (tag.POS$POStags == "NNPS"))
            {
                  is.Noun.Vector[i] <- TRUE
            }
            else
            { 
                  is.Noun.Vector[i] <- FALSE 
            }
      }
      as.logical(is.Noun.Vector)
}
#######---------------------------------------------------------------------------------#######

#######------------------------------  PluralGenerator()  ------------------------------#######

# Purpose: Consumes a noun and returns it's generated plural.

# Arguments: (Character)

# Constants: kend.Len (Integer), vowels (Character Vector)

# Returns: (Character Vector OR NULL)

# Vowels
vowels <- c("a","e","i","o","u")

# To check if call is from WordPlural() or not.
from.Word.Plural <- FALSE

PluralGenerator <- function(input.Noun, check.Plur = FALSE )
{     
      run <- TRUE   
      
      #________________ Refer to Section 6 i in "Read Me.txt ______________________#
      # Rest of code only runs if the word is not a plural
      if(check.Plur == TRUE)
      {
            run <- !IsPlural(input.Noun)
      }
      #____________________________________________________________________________#
      
      if(run)
      {
            # Converts to lowercase for comparison reasons
            noun <- tolower(input.Noun)
            
            # Last character in the noun
            noun.Last.Char <- substring(noun, nchar(noun))
            
            # Second last character in the noun
            noun.Slast.Char <- substring(noun, nchar(noun) -1, nchar(noun) -1)
            
            # Third last character in the noun
            noun.Tlast.Char <- substring(noun, nchar(noun) -2, nchar(noun) -2)
            
            # Is second last character is a consonant
            is.Consonant <- which(noun.Slast.Char == vowels)
            
            # Returns the plural if the word is an exception, else NULL
            plur.Exception <- PluralOfException(input.Noun)
            
            # If the word is an exception word
            if(!is.null(plur.Exception))
            {
                  plural.Noun <- plur.Exception
            }
            
            else
            {
                  # If the word ends with an "o" and second last character is a consonant
                  if(noun.Last.Char == "o" && !any(is.Consonant))
                  {
                        plural.Noun <- paste(input.Noun, "es", sep = "")
                  }
                  
                  # If the word ends with an "y" and second last character is a consonant
                  else if(noun.Last.Char == "y" && !any(is.Consonant))
                  {     
                        # Removes the last character
                        noun.Replace <- substring(input.Noun, 0, nchar(input.Noun) - 1)
                        plural.Noun <- paste(noun.Replace, "ies", sep = "")
                  }
                  
                  # Noun ending is "ex" or "ix"
                  else if((noun.Last.Char == "x") && ((noun.Slast.Char == "i") || (noun.Slast.Char == "e")))
                  {
                        noun.Replace <- substring(input.Noun, 0, nchar(input.Noun) - 2)
                        plural.Noun <- paste(noun.Replace, "ices", sep = "")
                  }
                  
                  # Noun ending is "ies"
                  else if ((noun.Tlast.Char == "i") && (noun.Slast.Char == "e") && (noun.Last.Char == "s"))
                  {
                        plural.Noun <- input.Noun
                  }  
                  
                  # Noun ending is "ss", "sh", "ch", "s" or "x"
                  else if (((noun.Slast.Char == "s") && (noun.Last.Char == "s")) ||
                                 ((noun.Slast.Char == "s") && (noun.Last.Char == "h")) ||
                                 ((noun.Slast.Char == "c") && (noun.Last.Char == "h")) ||
                                 (noun.Last.Char == "s") || (noun.Last.Char == "x"))
                  {
                        plural.Noun <- paste(input.Noun, "es", sep = "")
                  } 
                  
                  # Default condition adds an "s".
                  else
                  {
                        plural.Noun <- paste(input.Noun, "s", sep = "")
                  }
            }
            plural.Noun
      }
      
      # If the check.Plur function is TRUE and the noun is already plural
      else
      {
            input.Noun
      }
}

#######---------------------------------------------------------------------------------#######

#######---------------------------------  WordPlural()  --------------------------------#######

# Purpose: Returns the plural of a word if it exists

# Arguments: (Character, ...)

# Constants: kpos.Tags (Character Vector)

# Returns: (Character OR NULL)

# Note: Refer to 'Read Me.txt' Section 5 for more info

# Tags for nouns
kpos.Tags <- c("NN","NNS","NNP","NNPS")

WordPlural <- function(word, sentence = NULL)
{     
      # Variable to store the final Plural or NULL
      word.Plural <- character()
      
      # Variable to store the POS tag of the word
      word.Tag <- character()
      
      print("Generating plural...")
      
      #If sentence is not provided
      if(is.null(sentence))
      {
            # List with the word and its POS tag
            tag.List <- TagPOS(word)
      }
      
      #If sentence is provided
      else
      { 
            tag.List <- TagPOS(sentence, word)
      } 
      
      word.Tag <- tag.List$POStags[1]
      
      # Looks whether the POS tag of the word is a noun
      is.Noun.Word <- which(word.Tag == kpos.Tags)
      
      # If the word is not a noun
      if(!any(is.Noun.Word))
      {
            # Refer to 'Read Me.txt' Section 5 i 
            print(sprintf("The word '%s' appears to be a %s in this context. Try providing a sentence use of the word and the word itself to the function as: WordPlural(sentence, %s).", word, word.Tag, word))
            word.Plural <- NULL
      }
      
      # If the word is some form of a noun
      else 
      {     
            # If the word is a noun(singular or mass)
            if (word.Tag == "NN")
            {
                  from.Word.Plural <- TRUE
                  word.Plural <- PluralGenerator(word)
            }
            
            # If the noun is already a plural
            else
            {
                  print(sprintf("The word '%s' is already a plural word", word))
                  word.Plural <- word
            }     
      } 
      
      # If the word is a form a noun
      if(!is.null(word.Plural))
      {
            print(sprintf("The word %s without context is a: %s", word, word.Tag))
            print("The plural is: ")
      }
      
      word.Plural
}

