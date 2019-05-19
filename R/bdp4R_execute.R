#' @title Preproccess files through pipes
#' @description Method that allows to preprocess files in a comfortable way.
#' @docType methods
#' @param configurationFilePath (character) Path where the file with keys are located.
#' @param filesPath (character) Path where the files to be preprocessed are located.
#' @param pipe (TypePipe) Subclass of TypePipe, which implements the pipe method.
#' @param instanceFactory (InstanceFactory) Object which implements the method "createInstance"
#' to choose which type of Instance is created.
#' @usage bdp4R_execute(configurationFilePath, filesPath, pipe = SerialPipes$new(),
#' instanceFactory = InstanceFactory$new())
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
#' @return List of instances that have been preprocessed.
#' @import streamR urltools backports
#' @importFrom purrr is_function
#'
#' @export bdp4R_execute
#'
bdp4R_execute = function(configurationFilePath,
                         filesPath,
                         pipe = SerialPipes$new(),
                         instanceFactory = InstanceFactory$new()) {

  if (!"character" %in% class(configurationFilePath)) {
    stop("[bdp4R_execute][Error]
            Checking the type of the variable: configurationFilePath ",
              class(configurationFilePath))
  }

  if (!"ini" %in% file_ext(configurationFilePath)) {
    stop("[bdp4R_execute][Error]
            Checking the extension of the file: configurationFilePath ",
              file_ext(configurationFilePath))
  }

  if (!"character" %in% class(filesPath)) {
    stop("[bdp4R_execute][Error]
            Checking the type of the variable: filesPath ",
              class(filesPath))
  }

  if (!"TypePipe" %in% class(pipe)) {
    stop("[bdp4R_execute][Error]
            Checking the type of the variable: pipe ",
              class(pipe))
  }

  if (!"InstanceFactory" %in% class(instanceFactory)) {
    stop("[Bdp4R][proccess_files][Error]
                Checking the type of the variable: instanceFactory ",
         class(instanceFactory))
  }

  bdp4R_object <- Bdp4R$new(configurationFilePath)
  bdp4R_object$proccess_files(filesPath, pipe, instanceFactory)

}
