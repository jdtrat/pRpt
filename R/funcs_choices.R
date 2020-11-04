#' Get Choices for Each Round
#'
#' This function gets the choices for each round for each phase.
#'
#' @param phase_data One of the three phase objects defined by calling \code{\link{get_phases}}.
#'
#' @return Returns a dataframe with five columns:
#' \itemize{
#' \item \emph{option:} The option the subject chose.
#' This should correspond to the icon order as presented in the task.
#' \item \emph{choice:} The icon the subject chose.
#' \item \emph{group:} The (payment weighting) group the icon corresponds to.
#' \item \emph{phase:} The phase the choices were made in (constant for each \code{phase} object).
#' \item \emph{round:} The round number these choices were made in, specific to the phase.
#' }
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{get_choices(phase$two)}

get_choices <- function(phase_data) {

  phase_data %>%
    dplyr::filter(stringr::str_detect(.data$data, "HIGHLIGHT CHOICE")) %>%
    dplyr::mutate(data = stringr::str_remove_all(.data$data, "HIGHLIGHT CHOICE: ")) %>%
    tidyr::separate(col = .data$data,
                    into = c("wordOption", "option", "wordChoice", "choice", "wordGroup", "group", "roundWord", "round"),
                    extra = "warn",
                    remove = FALSE,
                    sep = " ") %>%
    dplyr::select(.data$option, .data$choice, .data$group, .data$round, .data$phase) %>%
    dplyr::mutate(dplyr::across(.cols = c(.data$option, .data$choice, .data$group), ~forcats::as_factor(stringr::str_remove_all(.x, ","))),
                  round = dplyr::row_number())

}


