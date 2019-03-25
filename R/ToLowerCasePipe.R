#' @title Class to convert the data of an instance to lower case
#' @description Class to convert the data to lowercase.
#' @docType class
#' @usage ToLowerCasePipe$new(propertyName = "",
#'                     alwaysBeforeDeps = list(),
#'                     notAfterDeps = list("AbbreviationPipe", "SlangPipe"))
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe}}{
#' Function that preprocesses the instance to convert the data to lower case.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipe(instance)}
#' }
#' \item{\emph{Value}}{
#'
#' The instance with the modifications that have occurred in the pipe.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{instance}}{
#' (Instance) Instance to preproccess.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{toLowerCase}}{
#' Function that convert the data to lowercase.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{toLowerCase(data)}
#' }
#' \item{\emph{Value}}{
#'
#' Data in lowercase.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{data}}{
#' (character) Text to preproccess.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{PipeGeneric}}, \code{\link{Instance}}
#'
#' @import R6  pipeR
#' @export ToLowerCasePipe

ToLowerCasePipe <- R6Class(

  "ToLowerCasePipe",

  inherit = PipeGeneric,

  public = list(

    initialize = function(propertyName = "",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list("AbbreviationPipe",
                                              "SlangPipe")) {
      #
      #Class constructor
      #
      #This constructor initialize the variable of propertyName.This variable
      #contains the name of the property that will be obtained in the pipe
      #In addition, the name of the property of the language is indicated,
      #and the place where the resources of the interjections are stored.
      #
      #
      #Args:
      #   propertyName: (character) Name of the property
      #   alwaysBeforeDeps: (list) The dependences alwaysBefore (pipes that must
      #                            be executed before this one)
      #   notAfterDeps: (list) The dependences notAfter (pipes that cannot be
      #                       executed after this one)
      #Returns:
      #   null
      #
      if (!"character" %in% class(propertyName)) {
        stop("[ToLowerCasePipe][initialize][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[TargetAssigningFromPathPipe][initialize][Error]
                Checking the type of the variable: alwaysBeforeDeps ",
                  class(alwaysBeforeDeps))
      }

      if (!"list" %in% class(notAfterDeps)) {
        stop("[TargetAssigningFromPathPipe][initialize][Error]
                Checking the type of the variable: notAfterDeps ",
                  class(notAfterDeps))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },

    pipe = function(instance) {
      #
      #Function that preprocesses the instance to convert the data to lowercase
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #
      if (!"Instance" %in% class(instance)) {
          stop("[ToLowerCasePipe][pipe][Error]
                  Checking the type of the variable: instance ",
                    class(instance))
      }

      instance$addFlowPipes("ToLowerCasePipe")

      if (!instance$checkCompatibility("ToLowerCasePipe", self$getAlwaysBeforeDeps())) {
        stop("[ToLowerCasePipe][pipe][Error] Bad compatibility between Pipes.")
      }

      instance$addBanPipes(unlist(super$getNotAfterDeps()))

      instance$getData() %>>%
        self$toLowerCase() %>>%
          instance$setData()

      return(instance)
    },

    toLowerCase = function(data) {
      #
      #Function that convert the data to lowercase
      #
      #Args:
      #   data: (character) text to preproccess
      #Returns:
      #   Data in lowercase
      #
      if (!"character" %in% class(data)) {
          stop("[ToLowerCasePipe][toLowerCase][Error]
                  Checking the type of the variable: data ",
                    class(data))
      }

      return(data %>>% tolower())
    }
  )
)
