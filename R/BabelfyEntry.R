#Private variables:
#startIdx: (numeric) The start index of an entry
#endIdx: (numeric) The last index of the entrys
#score: (numeric) The score
#synsetId: (character) The synset ID
#text: (character) The text of an entry

#' @title Class to represent a babelfy Semantic annotation
#' @description This class is to represent a babelfy Semantic annotation with all relevant
#' attributes to made intensive searches and discard the irrelevant information
#' achieved by Babelfy.
#' @docType class
#' @usage BabelfyEntry$new(startIdx, endIdx, score, synsetId, text)
#' @param startIdx  (numeric) The start index of an entry.
#' @param endIdx  (numeric) The last index of the entrys.
#' @param score (numeric) The score.
#' @param synsetId (character) The synset ID.
#' @param text (character) The text of an entry.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{getStartIdx}}{
#' Getter of startIdx variable.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getStartIdx()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of startIdx variable.
#' }
#' }
#' }
#'
#' \item{\bold{setStartIdx}}{
#' Setter of startIdx variable.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{setStartIdx(startIdx)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{startIdx}}{
#' (numeric) The new value of startIdx variable.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getEndIdx}}{
#' Getter of endIdx variable.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getEndIdx()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of endIdx variable.
#' }
#' }
#' }
#'
#' \item{\bold{setEndIdx}}{
#' Setter of endIdx variable.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{setEndIdx(endIdx)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{endIdx}}{
#' (numeric) The new value of endIdx variable.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getScore}}{
#' Getter of score variable.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getScore()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of score variable.
#' }
#' }
#' }
#'
#' \item{\bold{setScore}}{
#' Setter of score variable.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{setScore(score)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{score}}{
#' (numeric) The new value of score variable.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getSynsetId}}{
#' Getter of synsetId variable.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getSynsetId()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of synsetId variable.
#' }
#' }
#' }
#'
#' \item{\bold{setSynsetId}}{
#' Setter of synsetId variable.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{setSynsetId(synsetId)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{synsetId}}{
#' (character) The new value of synsetId variable.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getText}}{
#' Getter of text variable.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getText()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of text variable.
#' }
#' }
#' }
#'
#' \item{\bold{setText}}{
#' Setter of text variable.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{setText(text)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{text}}{
#' (character) The new value of text variable.
#' }
#' }
#' }
#' }
#' }
#'
#' }
#'
#' @import R6
# @export BabelfyEntry

BabelfyEntry <- R6Class(

  "BabelfyEntry",

  public = list(

    initialize = function(startIdx, endIdx, score, synsetId, text) {
      #
      #Class constructor
      #
      #This constructor initialize all attributes of a BabelfyEntry
      #
      #Args:
      #   startIdx: (numeric) The start index of an entry
      #   endIdx: (numeric) The last index of the entrys
      #   score: (numeric) The score
      #   synsetId: (character) The synset ID
      #   text: (character) The text of an entry
      #
      #Returns:
      #   null
      #
      if (!"numeric" %in% class(startIdx)) {
        stop("[BabelfyEntry][initialize][Error]
                Checking the type of the variable: startIdx ",
                  class(startIdx))
      }

      if (!"numeric" %in% class(endIdx)) {
        stop("[BabelfyEntry][initialize][Error]
                Checking the type of the variable: endIdx ",
                  class(endIdx))
      }

      if (!"numeric" %in% class(score)) {
        stop("[BabelfyEntry][initialize][Error]
                Checking the type of the variable: score ",
                  class(score))
      }

      if (!"character" %in% class(synsetId)) {
        stop("[BabelfyEntry][initialize][Error]
                Checking the type of the variable: synsetId ",
                  class(synsetId))
      }

      if (!"character" %in% class(text)) {
        stop("[BabelfyEntry][initialize][Error]
                Checking the type of the variable: text ",
                  class(text))
      }

      private$startIdx <- startIdx
      private$endIdx <- endIdx
      private$score <- score
      private$synsetId <- synsetId
      private$text <- text
    },

    getStartIdx = function() {
      #
      #Getter of startIdx variable
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of startIdx variable
      #
      return(private$startIdx)
    },

    setStartIdx = function(startIdx) {
      #
      #Setter of startIdx variable
      #
      #Args:
      #   startIdx: (numeric) the new value of startIdx variable
      #
      #Returns:
      #   null
      #s
      if (!"numeric" %in% class(startIdx)) {
        stop("[BabelfyEntry][setStartIdx][Error]
                Checking the type of the variable: startIdx ",
                  class(startIdx))
      }

      private$startIdx <- startIdx

      return()
    },

    getEndIdx = function() {
      #
      #Getter of endIdx variable
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of endIdx variable
      #
      return(private$endIdx)
    },

    setEndIdx = function(endIdx) {
      #
      #Setter of endIdx variable
      #
      #Args:
      #   endIdx: (character) the new value of endIdx variable
      #
      #Returns:
      #   null
      #s
      if (!"numeric" %in% class(endIdx)) {
        stop("[BabelfyEntry][setEndIdx][Error]
                Checking the type of the variable: endIdx ",
                  class(endIdx))
      }

      private$endIdx <- endIdx

      return()
    },

    getScore = function() {
      #
      #Getter of score variable
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of score variable
      #
      return(private$score)
    },

    setScore = function(score) {
      #
      #Setter of score variable
      #
      #Args:
      #   score: (numeric) the new value of score variable
      #
      #Returns:
      #   null
      #
      if (!"numeric" %in% class(score)) {
        stop("[BabelfyEntry][setScore][Error]
                Checking the type of the variable: score ",
                  class(score))
      }

      private$score <- score

      return()
    },

    getSynsetId = function() {
      #
      #Getter of synsetId variable
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of synsetId variable
      #
      return(private$synsetId)
    },

    setSynsetId = function(synsetId) {
      #
      #Setter of synsetId variable
      #
      #Args:
      #   synsetId: (character) the new value of synsetId variable
      #
      #Returns:
      #   null
      #
      if (!"character" %in% class(synsetId)) {
        stop("[BabelfyEntry][setSynsetId][Error]
                Checking the type of the variable: synsetId ",
                  class(synsetId))
      }

      private$synsetId <- synsetId

      return()
    },

    getText = function() {
      #
      #Getter of text variable
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of text variable
      #
      return(private$text)
    },

    setText = function(text) {
      #
      #Setter of text variable
      #
      #Args:
      #   text: (character) the new value of text variable
      #
      #Returns:
      #   null
      #
      if (!"character" %in% class(text)) {
        stop("[BabelfyEntry][setText][Error]
                Checking the type of the variable: text ",
                  class(text))
      }

      private$text <- text

      return()
    }
  ),
  private = list(
    startIdx = 0,
    endIdx = 0,
    score = 0,
    synsetId = "",
    text = ""
  )
)
