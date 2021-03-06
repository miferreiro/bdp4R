# @title Read a WARC file (compressed or uncompressed)
# @description Method that allows to process files in a comfortable way.
# @docType methods
# @details Function of the jwart package that has been modified to read correctly
#          the date of the warc records. This override has been made since the
#          date obtained was without the time because the transformation of the
#          format of the warc dates to the standard format was incompatible.
# @param path Path to WARF file.
# @param warc_types If not NULL and one or more of warcinfo, request, response,
#                   resource, metadata, revisit, conversion then returned WARC
#                   records will be filtered to only include the specified
#                   record types.
# @param include_payload If TRUE then the payload for each WARC record will be
#                        included.
# @usage read_warc(path, warc_types = NULL, include_payload = FALSE)
#
# @seealso \code{\link{ExtractorWarc}}
#
# @return listInstances list of instances that have been preprocessed
#' @import rJava tibble
# @export read_warc
#
# @examples read_warc(system.file("extdata/bbc.warc", package="jwatr"))
# read_warc(system.file("extdata/sample.warc.gz", package="jwatr"),
#           warc_types = "response", include_payload = FALSE)

read_warc = function(path, warc_types = NULL, include_payload = FALSE)
{
  if (!is.null(warc_types)) {
    warc_types <- match.arg(warc_types, several.ok = TRUE,
                            choices = c("warcinfo", "request", "response", "resource",
                                        "metadata", "revisit", "conversion"))
  }
  path <- path.expand(path)
  if (!file.exists(path))
    stop(sprintf("\"%s\" not found.", path, call. = FALSE))
  warc_obj <- new(J("is.rud.wrc.App"))
  warc_obj$process(path)
  if (is.null(warc_obj$warcDateStr[[1]])) {
    warcDate <- warc_obj$warcDateStr
  } else {
    warcDate <- warc_obj$warcDateStr[[1]]
  }

  xdf <- suppressWarnings(data_frame(target_uri = warc_obj$warcTargetUriStr,
                                     ip_address = warc_obj$warcIpAddress, warc_content_type = warc_obj$contentTypeStr,
                                     warc_type = warc_obj$warcTypeStr, content_length = as.numeric(warc_obj$contentLengthStr),
                                     payload_type = warc_obj$warcIdentifiedPayloadTypeStr,
                                     profile = warc_obj$warcProfileStr, date = as.POSIXct(paste(substr(warcDate,1,10), substr(warcDate,12,19), sep = " " )),
                                     http_status_code = as.numeric(warc_obj$httpStatusCode),
                                     http_protocol_content_type = warc_obj$httpProtocolContentType,
                                     http_version = warc_obj$httpVersion, http_raw_headers = lapply(warc_obj$httpRawHeaders,
                                                                                                    .jevalArray), warc_record_id = warc_obj$warcRecordIdStr))

  if (include_payload)
    xdf$payload <- lapply(warc_obj$warc_payload, .jevalArray)
  if (!is.null(warc_types))
    xdf <- dplyr::filter(xdf, warc_type %in% warc_types)
  xdf
}
