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
#' variables). See the \emph{Details} section for more on how to access the three
#' phases.
#'
#' The task and icon rating phases may be accessed by calling the variables:
#' \itemize{ \item \code{phase$one} \item \code{phase$two} \item
#' \code{phase$three} \item \code{phase$forced} \item \code{phase$finalIcon}}
#'
#' @importFrom rlang .data
#' @param data The data output from \code{\link{read_prpt}}
#'
#' @export
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

#' Get Icons for Each Round per Phase
#'
#' This function gets the icons for each round in each phase.
#'
#' @param phase_data One of the three phase objects defined by calling
#'   \code{\link{getPhases}}.
#'
#' @return This returns a dataframe with four columns: \itemize{\item
#'   \emph{phase:} The phase these icons were displayed in (constant for each
#'   \code{phase} object). \item \emph{round:} The round number these icons were
#'   displayed in, specific to the phase. \item \emph{icon1:} The icon presented
#'   in the first order position. \item \emph{icon2:} The icon presented in the
#'   second order position. }

get_phase_icons <- function(phase_data) {

  phaseIcons <- phase_data %>%
    dplyr::filter(stringr::str_detect(.data$data, "SHOW: OPTION SELECTION")) %>%
    tidyr::separate(col = .data$data,
                    into = c("show", "option", "selection", "icon", "order", "icon_group"),
                    extra = "drop",
                    sep = " ",
                    remove = TRUE) %>%
    dplyr::select(.data$icon_group, .data$phase) %>%
    dplyr::mutate(icon_group = stringr::str_remove_all(.data$icon_group, "\"|\\{|\\}")) %>%
    tidyr::separate(col = .data$icon_group, into = c("order1", "icon1", "icon2"),
                    extra = "warn",
                    remove = FALSE,
                    sep = ":") %>%
    tidyr::separate(col = .data$icon1, into = c("icon1", "order2"),
                    extra = "warn",
                    remove = FALSE,
                    sep = ",") %>%
    dplyr::mutate(icon2 = stringr::str_remove_all(.data$icon2, ",")) %>%
    dplyr::select(.data$icon1, .data$icon2, .data$phase) %>%
    dplyr::mutate(round = dplyr::row_number()) %>%
    dplyr::relocate(3,4)

  return(phaseIcons)

}

#' Get One Phase's Group Reward and Probability Info
#'
#' @param phase_data One of the three phase objects defined by calling
#'   \code{\link{getPhases}}.
#'
#' @importFrom rlang .data
#'
#' @return A dataframe with four columns. It has the reward and probability of
#'   receiving it as a function of each image's group per phase. This can be
#'   compared to the output of \code{\link{getChoices}} or
#'   \code{\link{processChoiceData}}. This function is called by
#'   \code{\link{getGroupInfo}} which combines the information for all three of
#'   the phases.

get_group_reward_probs <- function(phase_data) {

  phase_data %>%
    dplyr::slice(1) %>%
    dplyr::mutate(data = stringr::str_remove_all(.data$data, "Phase [1|2|3] Group \\{Reward, Probability\\}: ")) %>%
    tidyr::separate(col = .data$data,
                    into = c("1", "2", "3", "4", "5", "6"),
                    extra = "warn",
                    remove = TRUE,
                    sep = "\\},") %>%
    dplyr::select(-row) %>%
    dplyr::mutate(dplyr::across(.cols = -.data$phase, ~ stringr::str_remove(.x, "Group [1|2|3|4|5|6] \\{\\$"))) %>%
    tidyr::pivot_longer(cols = -.data$phase,
                        names_to = "group",
                        values_to = "info") %>%
    dplyr::mutate(info = stringr::str_trim(.data$info),
                  info = stringr::str_remove_all(.data$info, ",|\\}|%")) %>%
    tidyr::separate(col = .data$info,
                    into = c("reward", "probability"),
                    extra = "warn",
                    remove = TRUE,
                    sep = " ") %>%
    dplyr::mutate(group = forcats::as_factor(.data$group),
                  probability = base::as.numeric(.data$probability),
                  probability = .data$probability/100) %>%
    tidyr::drop_na()

}

#' Get Group Reward and Probability Info for All Phases
#'
#' @param data The data output from \code{\link{readChoices}}
#'
#' @importFrom rlang .data
#'
#' @return A dataframe with 15 rows and seven columns. It has the reward amount
#'   and probability of receiving a reward as a function of each image's group
#'   per phase. This also has the icon to group mapping for the subject. Most
#'   importantly, it has the EU and rank columns. These are used to determine
#'   optimal choice when measuring behavioral performance. This can be used in
#'   conjunction with the output of \code{\link{getChoices}} or
#'   \code{\link{processChoiceData}}.
#' @export

get_group_info <- function(data) {

  get_phases(data)
  phases <- list(phase$one, phase$two, phase$three)

  iconGroupMaps <- dplyr::filter(stringr::str_detect(.data$data, "ICON TO GROUP MAPPING: ")) %>%
    dplyr::select(-.data$time) %>%
    dplyr::mutate(data = stringr::str_remove(.data$data, "ICON TO GROUP MAPPING:"),
                  data = stringr::str_remove(.data$data, "\\{|\\}"),
                  data = stringr::str_remove(.data$data, "\\}")) %>%
    tidyr::separate(col = .data$data,
                    into = c("icon1", "icon2", "icon3", "icon4", "icon5", "icon6"),
                    sep = "\\,") %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "iconNum") %>%
    dplyr::mutate(value = stringr::str_remove_all(.data$value, "\\\"")) %>%
    tidyr::separate(.data$value,
                    into = c("icon", "group"),
                    sep = "\\:") %>%
    dplyr::arrange(.data$group) %>%
    dplyr::transmute(icon = stringr::str_trim(.data$icon),
                     group = base::as.factor(.data$group))

  #warnings suppressed due to there only being three groups in phase 1. It throws a warning but it doesn't matter.
  output <- base::suppressWarnings(dplyr::left_join(iconGroupMaps, purrr::map_df(phases, ~get_group_reward_probs(.x)), by = "group")) %>%
    dplyr::arrange(phase) %>%
    dplyr::select(phase, .data$icon, .data$group, .data$reward, .data$probability) %>%
    dplyr::mutate(EU = base::as.numeric(.data$reward) * .data$probability) %>%
    dplyr::group_by(phase) %>%
    dplyr::mutate(rank = dplyr::min_rank(.data$EU),
                  sign = ifelse(.data$EU > 0, "pos", "neg"))

  return(output)

}

#' Get Reinforcer Information
#'
#' This function gets the monetary reinforcement (reward) shown at each round.
#'
#' @param data The data output from \link{read_prpt}.
#'
#' @return A data frame with three rows: round number, reward (monetary value),
#'   cumulative  reward.
#' @export
#'
get_reinforcers <- function(data) {

  data %>%
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
    dplyr::select(.data$round, .data$reward, .data$cumulative_reward)

}

