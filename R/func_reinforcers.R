#' Get Reinforcer Information
#'
#' This function gets the monetary reinforcement (reward) shown at each round.
#'
#' @param phase_data One of the three phase objects defined by calling
#'   \code{\link{get_phases}}.
#'
#' @return A data frame with four columns:
#' * \emph{phase:} The phase the choices were made in.
#' * \emph{round number:} The round number, specific to the phase.
#' * \emph{reward:} The reward (in dollars) participants received from their choice.
#' * \emph{cumulative_reward:} The cumulative reward since the beginning of the game (phase 1, round 1).
#'
#' @export
#'
get_reinforcers <- function(phase_data) {

  phase_data %>%
    dplyr::filter(stringr::str_detect(.data$data, "SHOW: Results - reward: ")) %>%
    dplyr::mutate(data = stringr::str_remove_all(.data$data, "SHOW: Results - reward: ")) %>%
    tidyr::separate(col = .data$data,
                    into = c("reward", "game", "total",
                             "cumulative_reward", "ROUND", "round"),
                    extra = "warn",
                    remove = T,
                    sep = " ") %>%
    dplyr::mutate(reward = stringr::str_remove(.data$reward, "\\,"),
                  cumulative_reward = stringr::str_remove(.data$cumulative_reward, "\\,"),
                  round = dplyr::row_number()) %>%
    dplyr::select(.data$phase, .data$round, .data$reward, .data$cumulative_reward)

}
