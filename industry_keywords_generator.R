# Libraries For This File           # Functions in this file
#_______________________            #_______________________
library(twitteR)                    # PDFToText()
library(tm)                         # ScanFilterWords()
library(wordnet)                    # CharFilter()
library(stringr)                    # KeywordGenerator()                                  
#_______________________            #_______________________

##--------------------------------------START - Setting Path-------------------------------------##
# Set your path here 
# Path should be the path upto to folder which contains the required files
# The path should not contain the trailing slash ( "/" )

path <- # Your path goes here!
##---------------------------------------END - Setting Path--------------------------------------##

path.plural <- paste(path,'/plural_word_functions.R', sep='')
source(path.plural)

# Install WordNet 2.1 for Windows from the internet and set the dictionary path as:

setDict("C:/Program Files (x86)/WordNet/2.1/dict")

# Install XPDF for Windows from the internet and set the dictionary path as:
#     C:/Program Files (x86)

#######--------------------------------  PDFToText()  --------------------------------#######

# Purpose: Extracts all the PDFs present in the default directory (kwd) or from
#          a specific file path, or a file name and coverts it into a Character Vector 
#          with all the words.

# Arguments: (Void)

# Constants: kwd (Character)

# Returns: Character Vector

kwd <- path

PDFToText <- function(pdf.Dir = kwd, del.Txt.Files = TRUE)
{     
  dir.Len <- nchar(pdf.Dir)
{ 
    # If file path is provided
    if((dir.Len > 3) && any(grep("/", pdf.Dir)) && (tolower(str_sub(pdf.Dir, dir.Len - 3, dir.Len)) == ".pdf"))
    {
      pdf.Files.Path <- pdf.Dir
      
      path.Split <- unlist(str_split(pdf.Dir,"/"))
      
      # Extract only the file name
      pdf.Files <- path.Split[length(path.Split)]
      
      # Extract file path
      pdf.Dir <- path.Split[-length(path.Split)]
      pdf.Dir <- CharFilter(pdf.Dir, "CharCombine", combining.Char = "/")
    }
    
    # If just the file name is given, it searches in the set directory (kwd)
    else if((dir.Len > 3) && (tolower(str_sub(pdf.Dir, dir.Len - 3, dir.Len)) == ".pdf"))
    {
      pdf.Files.Path <- paste(kwd,"/",pdf.Dir, sep = "")
      pdf.Files <- pdf.Dir
      pdf.Dir <- kwd
    }
    
    # else if just a directory is given
    else
    {
      pdf.Files.Path <- list.files(path = pdf.Dir, pattern = "pdf", full.names = TRUE)
      pdf.Files <- list.files(path = pdf.Dir, pattern = "pdf", full.names = FALSE)
    }
  }     
# convert each PDF file that is named in the vector into a text file 
# text file is created in the same directory as the PDFs
# pdftotext.exe is Save in different files later...
lapply(pdf.Files.Path, 
       function(x) system(paste('"C:/Program Files (x86)/xpdfbin-win-3.04/bin64/pdftotext.exe"', 
                                paste0('"', x, '"')), wait = TRUE))

# Vector of files ending with .txt
pdf.Txt.Files <- list.files(path = pdf.Dir, pattern = "txt", full.names = FALSE)

# Only PDF files converted above will be taken
pdf.Txt.Files <- intersect(gsub("\\..*","",pdf.Files), gsub("\\..*","",pdf.Txt.Files))

# Reforms the path of the text file
pdf.Txt.Files <- unlist(lapply(pdf.Txt.Files, function(x) paste(pdf.Dir,"/", x,".txt", sep = "")))

# Variable to store all the PDF content
content.Len <- length(pdf.Txt.Files)

pdf.Content <- NULL
for(m in 1:content.Len)
{
  pdf.Content <- c(pdf.Content, readLines(pdf.Txt.Files[m]))
  
  if(del.Txt.Files == TRUE)
  {
    try(file.remove(pdf.Txt.Files[m]))
  }
}
pdf.Content
}
#######-------------------------------------------------------------------------------#######

#######-----------------------------  ScanFilterWords()  -----------------------------#######

# Purpose: Scans the filter words present in "FilterWords.txt"

# Arguments: (Void)

# Return: Character Vector

ScanFilterWords <- function(file.Name = "FilterWords.txt")
{
  filter.Words <- scan(file = file.Name, what = character(),
                       sep = "\n", comment.char = "#")
}
#######------------------------------------------------------------------------------#######

#######-------------------------------  CharFilter()  -------------------------------#######

# Purpose: Custom functions for character vectors.

# Arguments: (Character Vector, Character Vector, ...)

# Return: Character Vector

# Note: Refer to "Read Me.txt" Section 3

CharFilter <- function(word.Vector, func, combining.Char = NULL, syn.Depth = 1, 
                       syn.POS.Exclude = NULL, syn.Quick = FALSE, file.Name = "SpecialChars.txt",
                       sep = "")
{
  # Determine the total number of elements
  if(is.list(word.Vector))
  {
    word.Vector <- unlist(word.Vector)
    word.Vector.Len <- length(word.Vector)
  }
  
  else
  {
    word.Vector.Len <- length(word.Vector)
  }
  
  
  temp <- word.Vector
  for(k in 1:word.Vector.Len)
  {
    # If the word vectors are of the following extensions
    word.Char.Len <- nchar(word.Vector[k])
    if(word.Char.Len >= 3)
    {
      word.Ext <- tolower(str_sub(word.Vector[k], word.Char.Len - 3, word.Char.Len))
      if(word.Ext == ".txt")
      {
        temp[k] <- list(scan(file = word.Vector[k], what = character(),
                             sep = "\n", comment.char = "#"))
      }
      
      else if(word.Ext == ".pdf")
      {
        temp[k] <- list(PDFToText(word.Vector[k]))
      }
      
      else
      {
        temp[k] <- word.Vector[k]
      }
    }
  }
  word.Vector <- temp
  
  # If .txt or .pdf is found a list will be created again.
  if(is.list(word.Vector))
  {
    word.Vector <- unlist(word.Vector)
    word.Vector.Len <- length(word.Vector)
  }
  
  func.Len <- length(func)
  for(i in 1:func.Len)
  {
    #____________________________ (Refer to "Read Me.txt" Section 3 i) ____________________________#
    # Removes special characters
    if(func[i] == "RemSpChar")
    {     
      for(k in 1:word.Vector.Len)
      {
        # Removes things things that look like a URL
        if(any(grep("htt", word.Vector[k])))
        {
          replace.Word <- grep("htt+", unlist(str_split(word.Vector[k], " ")), value = TRUE)
          word.Vector[k] <- str_replace_all(word.Vector[k], replace.Word[1], "")
        }
        
      }
      
      # Converts the character vector into alpha numeric form.
      word.Vector <- lapply(word.Vector ,str_replace_all,"[^[:alnum:]]", " ")
      
      
      # Vector of special characters and the number of characters
      sp.Chars <- scan(file = file.Name, what = character(),
                       sep = " ", comment.char = "#", encoding = "UTF-8")
      sp.Chars.Len <- length(sp.Chars)
      
      if(0 == sp.Chars.Len)
      {
        print("No special characters to check. Update special character file.")
        break
      }
      
      else
      {
        print(sprintf("Filtering the special characters  %s", paste(sp.Chars, collapse = ' ')))
        for(j in 1:sp.Chars.Len)
        {
          word.Vector <- lapply(word.Vector, str_replace_all, sp.Chars[j], "")
        }
      }
      
      # Makes the list into a vector
      word.Vector <- c(do.call("cbind",word.Vector))
      
      # Removes empty vector elements
      word.Vector <- word.Vector[word.Vector != ""]
      
    }
    #______________________________________________________________________________________________#
    
    # Returns the plurals of the words
    if(func[i] == "MakePlur")
    {
      # Finds the plural of the word assuming it is a noun
      word.Vector <- lapply(word.Vector, function(x) PluralGenerator(x))
      
      # Makes the list into a vector
      word.Vector <- unlist(word.Vector)
    }
    
    # Returns the synonyms of words
    if(func[i] == "GetSyn")
    {
      
      temp.Word.Vector <- unique(tolower(word.Vector))
      syn <- NULL
      word.Tag <- FALSE
      
      #
      if(word.Vector.Len == 1)
      {
        word.Tag <- TagPOS(word.Vector)$POStags
      }
      
      for(j in 1:syn.Depth)
      {
        word.Vector.Len <- length(temp.Word.Vector)
        
        # For faster in-depth searches
        if((j > 1) && (syn.Quick == TRUE) && (word.Vector.Len >= 7))
        {
          if(word.Vector.Len > 15)
          {
            word.Vector.Len <- 10
          }
          else
          {
            word.Vector.Len <- word.Vector.Len * 0.7
          }
        }
        
        # POS of word must be "ADJECTIVE", "ADVERB", "NOUN", or "VERB" to get synonym
        for(k in 1:word.Vector.Len)
        {
          syn.Noun <- NULL
          syn.Adverb <- NULL
          syn.Adjective <- NULL
          syn.Verb <- NULL
          
          # Only check the word's specific tag if syn.Quick == TRUE
          if((word.Tag %in% c("JJ", "JJR", "JJS")) && syn.Quick == TRUE)
          {
            syn.Adjective <- synonyms(temp.Word.Vector[k], "ADJECTIVE")
          }
          
          else if((word.Tag %in% c("RBR", "RBS")) && syn.Quick == TRUE)
          {
            syn.Adverb <- synonyms(temp.Word.Vector[k], "ADVERB")
          }
          
          else if((word.Tag %in% c("NN","NNS","NNP","NNPS")) && syn.Quick == TRUE)
          {
            syn.Noun <- synonyms(temp.Word.Vector[k], "NOUN") 
          }
          else if((word.Tag %in% c("VB","VBD","VBG","VBN","VBP","VBZ")) && syn.Quick == TRUE)
          {
            syn.Verb <- synonyms(temp.Word.Vector[k], "VERB")
          }
          else
          {
            syn.Noun <- synonyms(temp.Word.Vector[k], "NOUN")
            syn.Verb <- synonyms(temp.Word.Vector[k], "VERB")
            syn.Adjective <- synonyms(temp.Word.Vector[k], "ADJECTIVE")
            syn.Adverb <- synonyms(temp.Word.Vector[k], "ADVERB")
          }
          
          syn <- unique(c(syn.Noun, syn.Adverb, syn.Adjective, syn.Verb, syn))
        }
        temp.Word.Vector <- tolower(syn[!(syn %in% word.Vector)])
        word.Vector <- unique(c(word.Vector, syn))
      }
    }
    
    # Combines the words with a space or with a provided character
    if(func[i] == "CharCombine")
    {
      print("Combining the characters...")
      # If a combining character is not given
      if(is.null(combining.Char))
      {
        word.Vector <- paste( paste( word.Vector[1 : (word.Vector.Len - 1)], 
                                     collapse = sep, sep = sep), 
                              word.Vector[word.Vector.Len], sep = sep)
      }
      
      # If the combining character is given
      else
      {
        word.Vector <- paste( paste( word.Vector[1 : (word.Vector.Len - 1)], 
                                     combining.Char, collapse = sep, sep = sep), 
                              word.Vector[word.Vector.Len], sep = sep)
      }
    }
    #KeepNoun
    #RemDup
  }
  word.Vector
}
#######------------------------------------------------------------------------------#######

#######---------------------------- KeywordGenerator() ------------------------------#######

# Purpose: Using the provided tweets, function finds keywords related to the initial search.

# Arguments: (Listof Status Objects i.e tweets, ...)

# Return: Character Vector

# Note: Refer to Section 7 in "Read Me.txt".

KeywordGenerator <- function(text.Vector, return.Top.Terms = TRUE, occur.Freq = 9, most.Freq = 10)
{     
  # Getting the singular and plural words
  singular.Filter.Words <- ScanFilterWords()
  plural.Filter.Words <- CharFilter(singular.Filter.Words, "MakePlur")
  
  filter.Words <- c(singular.Filter.Words, plural.Filter.Words)
  
  # Build a corpus
  data.Corpus <- Corpus(VectorSource(as.vector(text.Vector)))
  
  tm.Funcs <- list(tolower, removePunctuation, removeNumbers, stripWhitespace)
  
  # Converts to lowercase and removes punctuations, numbers, and whitespaces.
  data.Corpus <- tm_map(data.Corpus, FUN = tm_reduce, tmFuns = tm.Funcs)
  
  # Removes filter words from data corpus.
  data.Corpus <- tm_map(data.Corpus, removeWords, filter.Words)
  
  # Build a term-document matrix
  data.DTM <- TermDocumentMatrix(data.Corpus)
  
  # Removes plural and the display top words and its count
  data.Matrix <- as.matrix(data.DTM)
  condensed.Matrix <- as.matrix(sort(rowSums(data.Matrix), decreasing=TRUE))
  matrix.Rows <- rownames(condensed.Matrix)
  valid.Rows <- removeWords(matrix.Rows, CharFilter(matrix.Rows, "MakePlur"))
  valid.Rows <- intersect(valid.Rows[valid.Rows != ""], matrix.Rows)
  condensed.Matrix <- condensed.Matrix[valid.Rows, ]
  top.Terms <- rownames(as.matrix(head(condensed.Matrix, most.Freq)))
  top.Terms <- CharFilter(top.Terms,"RemSpChar")
  
  # Inspect most popular words
  freq.Terms <- findFreqTerms(data.DTM, lowfreq = occur.Freq)
  
  # Removes special characters
  freq.Terms <- CharFilter(freq.Terms, "RemSpChar")
  
  # Remove generic and custom stopwords
  print("Filtering words ...")
  freq.Terms <- removeWords(freq.Terms, filter.Words)
  
  # Removes plural words
  freq.Terms <- removeWords(freq.Terms, CharFilter(freq.Terms, "MakePlur"))
  
  # Used to remove the empty strings
  freq.Terms <- CharFilter(freq.Terms, "RemSpChar")
  
  if(return.Top.Terms)
  {
    return(top.Terms)
  }
  
  else
  {
    return(freq.Terms)
  }
}
#######------------------------------------------------------------------------------#######
