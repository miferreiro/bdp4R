#' @title Class to get the match with the specific word from an Urban dictionary
#' @description Class to get the match with the specific word from an
#' Urban dictionary.
#' @docType class
#' @usage UrbanDictionaryHandler$new()
#' @param propertyName  (character) Name of the property.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details This class needs files in json format that will contain the slangs
#' to be located and the string that will replace them. For this it is necessary
#' that the instance contains a property that indicates the language of the data
#' to be able to correctly choose the list of slangs that apply to the data.
#' The format of the file names of the resources has to be: slang.xxx.json
#' (Being xxx the value of the language property of the instance).
#'
#' To indicate the path where the associated resources are located, the
#' configuration file is used. It is necessary to indicate in the section called
#' resourcesPath, the path of resourcesUrbanDictionaryPath:
#'
#' [resourcesPath]
#'
#' resourcesUrbanDictionaryPath = YourResourcesrbanDictionaryPath
#'
#' The pipe will invalidate the instance in the moment that the resulting data is
#' empty.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{handle}}{
#' Get the matches with the originalText and set it to replacementText.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{handle(originalText, replacementText, lang)}
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
#' \code{getReplacement4SlangTerm(slangTerm, lang)}
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
#' \item{\bold{getResourcesUrbanDictionaryPath}}{
#' Getter of path of slangs resources.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getResourcesUrbanDictionaryPath()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of path of slangs eesources.
#' }
#' }
#' }
#'
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{resourcesUrbanDictionaryPath}}{
#'  (character) The path where are the resources.
#' }
#' }
#'
#' @seealso \code{UnmatchedTextHandler}
#'
#' @import R6
#' @export UrbanDictionaryHandler

UrbanDictionaryHandler <- R6Class(

  "UrbanDictionaryHandler",

  inherit = UnmatchedTextHandler,

  public = list(

    initialize = function() {

      private$resourcesUrbanDictionaryPath <- read.ini(Bdp4R[["private_fields"]][["configurationFilePath"]])$resourcesPath$resourcesUrbanDictionaryPath

    },

    handle = function(originalText, replacementText, lang) {

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

      JsonFile <- paste(self$getResourcesUrbanDictionaryPath(),
                        "/slang.",
                        base::tolower(lang),
                        ".json",
                        sep = "")

      jsonData <- Bdp4R[["private_fields"]][["resourceHandler"]]$isLoadResource(JsonFile)

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

    getResourcesUrbanDictionaryPath = function() {

      return(private$resourcesUrbanDictionaryPath)
    }
  ),

  private = list(
    resourcesUrbanDictionaryPath = ""
  )
)
