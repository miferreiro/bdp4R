#' @title Class to manage the preprocess of the files through the pipes' flow
#' @description It contains the "static" variables that will be used throughout
#' the classes and the function that prepares and launches the execution of the
#' pipes from the TypePipe object that is passed to it as an argument.
#' @docType class
#' @usage Bdp4R$new(configurationFilePath)
#' @param pathKeys (character) Path where the file with configurations are located.
#' @details The configurationFilePath file has to have the following structure:
#'
#'
#' [twitter]
#' ConsumerKey= YourConsumerKey
#' ConsumerSecret= YourConsumerSecret
#' AccessToken= YourAccessToken
#' AccessTokenSecret= YourAccessTokenSecret
#'
#' [youtube]
#' app_id= YourAppId
#' app_password= YourAppPassword
#'
#' [eml]
#' PartSelectedOnMPAlternative = yourPartSelectedOnMPAlternative(text/html or text/plain)
#'
#' [babelfy]
#' keyBabelfy = YourKeyBabelfy
#'
#' [resourcesPath]
#' resourcesAbbreviationsPath = YourResourcesAbbreviationsPath
#' resourcesContractionsPath = YourResourcesContractionsPath
#' resourcesInterjectionsPath = YourResourcesInterjectionsPath
#' resourcesSlangsPath = YourResourcesSlangsPath
#' resourcesStopWordsPath = YourResourcesStopWordsPath
#' resourcesUrbanDictionaryPath = YourResourcesUrbanDictionaryPath
#'
#' [CSVPath]
#' outPutTeeCSVPipePath = YourOutPutTeeCSVPipePath
#' outPutTeeCSVFromSynsetFeatureVectorPipePath = YourOutPutTeeCSVFromSynsetFeatureVectorPipePath
#'
#' [cache]
#' cachePathTwtid = YourCachePathTwtid
#' cachePathYtbid = YourCachePathYtbid
#'
#'
#' @section Static variables:
#' \itemize{
#' \item{\bold{connections}}{
#' (Connections) Initialize the object that handles the
#' different types of connections with youtube and twitter.
#' }
#' \item{\bold{BabelUtils}}{
#' (BabelUtils) Initialize the object that handles the different
#' types of connections with babelfy and babelnet.
#' }
#' \item{\bold{ResourceHandler}}{
#' (ResourceHandler) Initialize the object that manages the
#' loading of the resource files, such as abbreviation, slang, stopword, etc.
#' }
#' \item{\bold{configurationFilePath}}{
#' (character) Path where the file with keys are located.
#' }
#' }
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{proccess_files}}{
#' Preprocess files through a pipes' flow
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{proccess_files(pipe = SerialPipes$new(), instanceFactory = InstanceFactory$new())}
#'
#' }
#' \item{\emph{Value}}{
#'
#' List of instances that have been preprocessed.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{filesPath}}{
#' (character) Path where the files to be processed are located.
#' }
#' \item{\strong{pipe}}{
#' (TypePipe) Subclass of TypePipe, which implements the pipe method.
#' }
#' \item{\strong{instanceFactory}}{
#' (InstanceFactory) Class which implements the method "createInstance"
#' to choose which type of Instance is created.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @import R6  tools
#' @importFrom utils write.table
#' @export Bdp4R

Bdp4R <- R6Class(

  "Bdp4R",

  public = list(

    initialize = function(configurationFilePath) {

      if (!"character" %in% class(configurationFilePath)) {
        stop("[Bdp4R][initialize][Error]
                Checking the type of the variable: configurationFilePath ",
                  class(pathFileConfiguration))
      }

      if (!"ini" %in% file_ext(configurationFilePath)) {
        stop("[Bdp4R][initialize][Error]
                Checking the extension of the file: configurationFilePath ",
                  file_ext(configurationFilePath))
      }

      Bdp4R[["private_fields"]][["configurationFilePath"]] <- configurationFilePath
      Bdp4R[["private_fields"]][["resourceHandler"]] <- ResourceHandler$new()
      Bdp4R[["private_fields"]][["connections"]] <- Connections$new(configurationFilePath)
      Bdp4R[["private_fields"]][["babelUtils"]] <- BabelUtils$new(configurationFilePath)

    },

    proccess_files = function(filesPath, pipe = SerialPipes$new(), instanceFactory = InstanceFactory$new()) {

      if (!"character" %in% class(filesPath)) {
        stop("[Bdp4R][proccess_files][Error]
                Checking the type of the variable: filesPath ",
                  class(filesPath))
      }

      if (!"TypePipe" %in% class(pipe)) {
        stop("[Bdp4R][proccess_files][Error]
                Checking the type of the variable: pipe ",
                  class(pipe))
      }

      if (!"InstanceFactory" %in% class(instanceFactory)) {
        stop("[Bdp4R][proccess_files][Error]
                Checking the type of the variable: instanceFactory ",
                  class(instanceFactory))
      }

      #Array of files to preprocess
      Files <- list.files(path = filesPath, recursive = TRUE, full.names = TRUE, all.files = TRUE)
      #Create the list of instances, which will contain the date, source, path, data
      #and a list of properties of the file that is in the indicated path
      InstancesList <- sapply(Files, instanceFactory$createInstance)
      cat("[Bdp4R][proccess_files][Info] ", "Has been created: ", length(InstancesList)," instances.\n")
      listInstances <- sapply(InstancesList, pipe$pipeAll)

      return(listInstances)
    }
  ),

  private = list(
    #Initialize the object that handles the different types of connections with youtube and twitter
    connections = NULL,
    #Initialize the object that handles the different types of connections with babelfy and babelnet
    babelUtils = NULL,
    #Initialize the object that manages the loading of the resource files, such as
    #abbreviation, slang, stopword, etc.
    resourceHandler = NULL,
    #Path where the file with keys are located.
    configurationFilePath = NULL
  )
)
