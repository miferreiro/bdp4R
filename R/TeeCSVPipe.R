#' @title Class to complete the data.frame with the preprocessed instance
#' @description Complete the data.frame with the preprocessed instance.
#' @docType class
#' @usage TeeCSVPipe$new(propertyName = "",
#'                alwaysBeforeDeps = list(),
#'                notAfterDeps = list())
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details Building...
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe}}{
#' Function that complete the data.frame with the preprocessed instance.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipe(instance, withData = TRUE, withSource = TRUE)}
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
#' \item{\strong{withData}}{
#' (logical) Indicate if the data is added to data.frame.
#' }
#' \item{\strong{withSource}}{
#' (logical) Indicate if the source is added to data.frame.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{PipeGeneric}}, \code{\link{Instance}}
#'
#' @import R6
#' @export TeeCSVPipe

TeeCSVPipe <- R6Class(

  "TeeCSVPipe",

  inherit = PipeGeneric,

  public = list(

    initialize = function(propertyName = "",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list()) {

      if (!"character" %in% class(propertyName)) {
        stop("[TeeCSVPipe][initialize][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[TeeCSVPipe][initialize][Error]
                Checking the type of the variable: alwaysBeforeDeps ",
                  class(alwaysBeforeDeps))
      }

      if (!"list" %in% class(notAfterDeps)) {
        stop("[TeeCSVPipe][initialize][Error]
                Checking the type of the variable: notAfterDeps ",
                  class(notAfterDeps))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },

    pipe = function(instance, withData = TRUE, withSource = TRUE) {

      if (!"Instance" %in% class(instance)) {
        stop("[TeeCSVPipe][pipe][Error]
                Checking the type of the variable: instance ",
                  class(instance))
      }

      if (!"logical" %in% class(withSource)) {
        stop("[TeeCSVPipe][pipe][Error]
                Checking the type of the variable: withSource ",
                  class(withSource))
      }

      if (!"logical" %in% class(withData)) {
        stop("[TeeCSVPipe][pipe][Error]
                Checking the type of the variable: withData ",
                  class(withData))
      }

      instance$addFlowPipes("TeeCSVPipe")

      if (!instance$checkCompatibility("TeeCSVPipe", self$getAlwaysBeforeDeps())) {
        stop("[TeeCSVPipe][pipe][Error] Bad compatibility between Pipes.")
      }

      instance$addBanPipes(unlist(super$getNotAfterDeps()))

      if (!instance$isInstanceValid()) {
        return(instance)
      }

      pos <- dim(Bdp4R[["private_fields"]][["dataFrameAll"]])[1] + 1

      Bdp4R[["private_fields"]][["dataFrameAll"]][pos, "path"] <- instance$getPath()

      if (withData) {
        Bdp4R[["private_fields"]][["dataFrameAll"]][pos, "data"] <- instance$getData()
      }

      if (withSource) {
        Bdp4R[["private_fields"]][["dataFrameAll"]][pos, "source"] <-
          as.character(paste0(unlist(instance$getSource())))
      }

      Bdp4R[["private_fields"]][["dataFrameAll"]][pos, "date"] <- instance$getDate()

      namesPropertiesList <- as.list(instance$getNamesOfProperties())
      names(namesPropertiesList) <- instance$getNamesOfProperties()

      for (name in namesPropertiesList) {
        Bdp4R[["private_fields"]][["dataFrameAll"]][pos, name] <-
          paste0(unlist(instance$getSpecificProperty(name)), collapse = "|")
      }

      return(instance)
    }
  )
)
