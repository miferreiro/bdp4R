#Private variables:
#pathResourcesSlangs: (character) the path where are the resources
#

#' @title Class to get the match with the specific word from an Urban dictionary
#' @description Class to get the match with the specific word from an
#' Urban dictionary.
#' @docType class
#' @usage UrbanDictionaryHandler$new(pathResourcesSlangs = "resources/slangs-json")
#' @param propertyName  (character) Name of the property.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{handle}}{
#' Get the matches with the originalText and set it to replacementText.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' UrbanDictionaryHandler$new()$handle(originalText, replacementText, lang)
#' }
#' \item{\emph{Value}}{
#'
#' The word that matches with the originalText.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{originalText}}{
#' (character)  The original text to replace.
#' }
#' \item{\strong{replacementText}}{
#' (character) The word that matches with the originalText.
#' }
#' \item{\strong{lang}}{
#' (character) The language of the original string.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getReplacement4SlangTerm}}{
#' Get the matches with the originalText and set it to replacementText.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' UrbanDictionaryHandler$new()$getReplacement4SlangTerm(slangTerm, lang)
#' }
#' \item{\emph{Value}}{
#'
#' The word that matches with the originalText.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{slangTerm}}{
#' (character) The original text to replace.
#' }
#' \item{\strong{lang}}{
#' (character) The language of the original string.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getPathResourcesSlangs}}{
#' Getter of path of slangs resources.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' UrbanDictionaryHandler$new()$getPathResourcesSlangs()
#' }
#' \item{\emph{Value}}{
#'
#' Value of pathResourcesSlangs variable
#' }
#' }
#' }
#'
#' }
#'
#' @seealso \code{\link{UnmatchedTextHandler}}
#'
#' @import R6
#' @export UrbanDictionaryHandler

UrbanDictionaryHandler <- R6Class(

  "UrbanDictionaryHandler",

  inherit = UnmatchedTextHandler,

  public = list(

    initialize = function(pathResourcesSlangs = "resources/slangs-json") {
      #
      #Class constructor
      #
      #This constructor initialize the variable which contains the place where
      #the resources of the abbreviations are stored.
      #
      #Args:
      #   pathResourcesSlangs: (character) Path where are stored the
      #                                           slangs resources
      #Returns:
      #   null
      #

      if (!"character" %in% class(pathResourcesSlangs)) {
        stop("[UrbanDictionaryHandler][initialize][Error]
                Checking the type of the variable: pathResourcesSlangs ",
                  class(pathResourcesSlangs))
      }

      private$pathResourcesSlangs <- pathResourcesSlangs
    },

    handle = function(originalText, replacementText, lang) {
      #
      #Get the matches with the originalText and set it to replacementText.
      #
      #Args:
      #   originalText: (character)  The original text to replace
      #   replacementText: (character) The word that matches with the originalText
      #   lang: (character) The language of the original string
      #Returns:
      #   The word that matches with the originalText
      #
      if (!"character" %in% class(originalText)) {
        stop("[UrbanDictionaryHandler][handle][Error]
                Checking the type of the variable: originalText ",
                  class(originalText))
      }

      if (!"character" %in% class(lang)) {
        stop("[UrbanDictionaryHandler][handle][Error]
                Checking the type of the variable: lang ",
                  class(lang))
      }

      matchedString <- replacementText

      if (is.null(replacementText)) {
        matchedString <- self$getReplacement4SlangTerm(originalText, lang)
        if (!is.null(matchedString)) {
          cat("[UrbanDictionaryHandler][handle][Info]"," Sucessfull match for string ", matchedString, "\n")
        }
      }

      return(matchedString)
    },

    getReplacement4SlangTerm = function(slangTerm, lang) {
      #
      #Get the matches with the originalText and set it to replacementText.
      #
      #Args:
      #   slangTerm: (character) The original text to replace
      #   lang: (character) The language of the original string
      #Returns:
      #   The word that matches with the originalText
      #
      if (!"character" %in% class(slangTerm)) {
        stop("[UrbanDictionaryHandler][getReplacement4SlangTerm][Error]
                Checking the type of the variable: slangTerm ",
                  class(slangTerm))
      }

      if (!"character" %in% class(lang)) {
        stop("[UrbanDictionaryHandler][getReplacement4SlangTerm][Error]
                Checking the type of the variable: lang ",
                  class(lang))
      }

      JsonFile <- paste(self$getPathResourcesSlangs(),
                        "/slang.",
                        base::tolower(lang),
                        ".json",
                        sep = "")

      jsonData <- Bdp4R[["private_fields"]][["resourceHandle"]]$isLoadResource(JsonFile)

      if (is.null(jsonData)) {
        message <- c( "Has not an SlangsJsonFile to apply to the language -> ", base::tolower(lang))

        cat("[UrbanDictionaryHandler][getReplacement4SlangTerm][Warning] ", message, " \n")
        return(NULL)
      }

      if (!slangTerm %in% names(jsonData)) {
        return(NULL)
      }

      return(jsonData)[[slangTerm]]
    },

    getPathResourcesSlangs = function() {
      #
      #Getter of path of slangs resources
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of pathResourcesSlangs variable
      #
      return(private$pathResourcesSlangs)
    }
  ),

  private = list(
    pathResourcesSlangs = ""
  )
)
