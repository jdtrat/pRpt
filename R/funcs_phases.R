#Create a phase environment
phase <- base::new.env(parent = base::emptyenv())

#' Get Task Phases
#'
#' This function takes the data column from the output of
#' \code{\link{read_prpt}} and separates the three phases of the task, saving
#' them to the environment \code{phrase}, which is initialized behind the
#' scenes. It also separates the initial icon rating phase and the final icon
#' rating phase. This function \strong{must} be run in order to use the other
#' functions that parse the choice data. If it is not, the phases will need to
#' be isolated by hand and assigned to the phase environment (or local/global
#' variables). See the \emph{Details} section for more on how to access the
#' three phases.
#'
#' @return A list of data for each phase in the PRPT task.
#'
#' @importFrom rlang .data
#' @param data The data output from \code{\link{read_prpt}}
#'
#' @export
#'
#' @examples
#'
#' phase_info <- get_phases(sample_data)
#'
#' # Print different phase information by indexing into the object.
#' # Phase three can be accessed as follows, for example:
#' phase_info$phaseThree
#'
get_phases <- function(data) {

  data <- data %>%
    dplyr::select(.data$data) %>%
    dplyr::mutate(row = dplyr::row_number())

  startForced <- data %>%
    dplyr::filter(stringr::str_detect(.data$data, "starting game")) %>%
    dplyr::select(.data$row) %>%
    dplyr::pull(.data$row)

  endForced <- data %>%
    dplyr::filter(stringr::str_detect(.data$data, "SHOW: ICON END TRANSITION SCREEN")) %>%
    dplyr::select(.data$row) %>%
    dplyr::pull(.data$row)

  start1 <- data %>%
    dplyr::filter(stringr::str_detect(.data$data, "Phase 1 Group")) %>%
    dplyr::select(.data$row) %>%
    dplyr::pull(.data$row)

  start2 <- data %>%
    dplyr::filter(stringr::str_detect(.data$data, "Phase 2 Group")) %>%
    dplyr::select(.data$row) %>%
    dplyr::pull(.data$row)

  start3 <- data %>%
    dplyr::filter(stringr::str_detect(.data$data, "Phase 3 Group")) %>%
    dplyr::select(.data$row) %>%
    dplyr::pull(.data$row)

  end3 <- data %>%
    dplyr::filter(stringr::str_detect(.data$data, "CLEAR TRANSITION SCREEN")) %>%
    utils::tail(1) %>%
    dplyr::select(.data$row) %>%
    dplyr::pull(.data$row)

  startFinalIcon <- end3

  endFinalIcon <- data %>%
    dplyr::filter(stringr::str_detect(.data$data, "Experiment Complete:")) %>%
    dplyr::select(.data$row) %>%
    dplyr::pull(.data$row)

  phase$forced <- data %>%
    dplyr::filter(dplyr::between(.data$row, startForced, endForced - 1))

  phase$one <- data %>%
    dplyr::filter(dplyr::between(.data$row, start1, start2 - 1)) %>%
    dplyr::mutate(phase = rep(1))

  phase$two <- data %>%
    dplyr::filter(dplyr::between(.data$row, start2, start3 - 1)) %>%
    dplyr::mutate(phase = rep(2))

  phase$three <- data %>%
    dplyr::filter(dplyr::between(.data$row, start3, end3 - 1)) %>%
    dplyr::mutate(phase = rep(3))

  phase$finalIcon <- data %>%
    dplyr::filter(dplyr::between(.data$row, startFinalIcon, endFinalIcon - 1))

  return(list("forced" = phase$forced,
              "phaseOne" = phase$one,
              "phaseTwo" = phase$two,
              "phaseThree" = phase$three,
              "finalIcon" = phase$finalIcon))

}
