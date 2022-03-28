#' Update task annotation
#'
#' Update the 'completed_successfully' task annotation on a Synapse folder.
#' Expected that the annotation is a text type.
#'
#' @export
#' @param annots Synapse annotation object for the task folder
#' @param success TRUE if the task was completed successfully, else FALSE
#' @param task_view synID for the task file view. If provided, will update the
#' view after annotating the task folder.
update_task_annotation <- function(task_id, annots, success, task_view = NA) {
  annots['completed_successfully'][[1]] <- success
  synapser::synSetAnnotations(task_id, annots)
  ## Force file view update, if given task_view
  if (!is.na(task_view)) {
    synTableQuery(glue::glue("SELECT * FROM {task_view}"))
  }
}

#' Upload the log file to Synapse
#'
#' Upload the log file to Synapse
#'
#' @export
#' @param folder synID of parent folder for logs
#' @param path log file path
upload_log_file <- function(folder, path) {
  syn_file <- File(path = path, parent = folder)
  synStore(syn_file)
}

#' Get config
#'
#' @export
#' @param value the config value to return
#' @param config the config settings needed
get_config <- function(value, config) {
  config::get(
    value = value,
    config = config,
    file = system.file("config.yml", package = "cleanAD")
    #file = "inst/config.yml"
  )
}
