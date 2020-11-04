#' Process Choice Data
#'
#' This function takes the data column from the output of
#' \code{\link{read_prpt}} and runs four functions, binding the results in a
#' dataframe for easy manipulation: \itemize{ \item \code{\link{get_phases}}
#' which separates the three phases of the task, saving them to the environment
#' \code{phase}, which is initialized behind the scenes. \item
#' \code{\link{get_icons}} which gets the icon information shown to the subject
#' per round. \item \code{\link{get_choices}} which gets the choice the subject
#' makes. \item \code{\link{get_reinforcers}} which gets the monetary
#' reinforcement the subject is shown after choosing an icon.}
#'
#'
#' @param data The data output from \code{\link{read_prpt}}
#'
#' @return A dataframe 150 rows and 16 columns: \itemize{ \item \emph{phase:}
#'   The phase these icons were displayed in (constant for each \code{phase}
#'   object). \item \emph{round:} The round number these icons were displayed
#'   in, specific to the phase. \item \emph{icon1:} The icon presented in the
#'   first order position. \item \emph{icon1Group:} The group mapping for the
#'   icon presented in the first order position. \item \emph{icon1Prob:} The
#'   probability of getting a monetary reinforcement by choosing icon 1. \item
#'   \emph{icon1Sign:} The relative sign of the reinforcer (positive or
#'   negative) by choosing icon 1. \item \emph{icon2:} The icon presented in the
#'   second order position. \item \emph{icon2Group:} The group mapping for the
#'   icon presented in the second order position. \item \emph{icon2Prob:} The
#'   probability of getting a monetary reinforcement by choosing icon 2. \item
#'   \emph{icon2Sign:} The relative sign of the reinforcer (positive or
#'   negative) by choosing icon 2. \item \item \emph{option:} The option the
#'   subject chose. \item \emph{chosen_icon:} The icon the subject chose. \item
#'   \emph{chosen_icon_group:} The (payment weighting) group the chosen icon
#'   corresponds to. \item \emph{chosen_icon_prob:} The probability that the
#'   chosen icon leads to a monetary reinforcement. \item \emph{reinforcer:} The
#'   amount (dollars) the subject was reinforced with after making a choice.
#'   \item \emph{cumulative_reward:} The cumulative reward since the beginning
#'   of the game (phase 1, round 1).}
#'
#' @importFrom rlang .data
#'
#' @export

process_choice_data <- function(data){

  get_phases(data)
  phases <- list(phase$one, phase$two, phase$three)

  icons <- get_icons(data)
  choices <- purrr::map_df(phases, ~get_choices(.x))
  reinforcers <- purrr::map_df(phases, ~get_reinforcers(.x))

  combined <- dplyr::left_join(icons, choices, by = c("phase", "round")) %>%
    dplyr::left_join(reinforcers, by = c("phase", "round")) %>%
    dplyr::rename(chosen_icon_group = .data$group,
                  chosen_icon = .data$choice,
                  reinforcer = .data$reward) %>%
    dplyr::mutate(chosen_icon_prob = dplyr::case_when(.data$chosen_icon == .data$icon1 ~ .data$icon1Prob,
                                                      .data$chosen_icon == .data$icon2 ~ .data$icon2Prob),
                  .before = .data$reinforcer)

  return(combined)

}



