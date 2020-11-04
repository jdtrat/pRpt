#' Get Icons for Each Round per Phase
#'
#' This function gets the icons for each round in each phase.
#'
#' @param phase_data One of the three phase objects defined by calling
#'   \code{\link{get_phases}}.
#'
#' @return This returns a dataframe with four columns: \itemize{\item
#'   \emph{phase:} The phase these icons were displayed in (constant for each
#'   \code{phase} object). \item \emph{round:} The round number these icons were
#'   displayed in, specific to the phase. \item \emph{icon1:} The icon presented
#'   in the first order position. \item \emph{icon2:} The icon presented in the
#'   second order position. }

get_icons_internal <- function(phase_data) {

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

#' Get Icons for Each Round
#'
#' This function parses the information for the icons presented in each round.
#'
#' @param data The data output from \code{\link{read_prpt}}
#' @param specific_phase If this argument is specified, the icons and groups for
#'   a specific phase will be returned. If this is not specified, the icons and
#'   groups for all phases will be returned.
#'
#' @return Returns a dataframe with ten columns: \itemize{ \item \emph{phase:}
#'   The phase these icons were displayed in (constant for each \code{phase}
#'   object). \item \emph{round:} The round number these icons were displayed
#'   in, specific to the phase. \item \emph{icon1:} The icon presented in the
#'   first order position. \item \emph{icon1Group:} The group mapping for the
#'   icon presented in the first order position. \item \emph{icon1Prob:} The
#'   probability of getting a reinforcing image by choosing icon 1. \item
#'   \emph{icon1Sign:} The relative sign of the reinforcing image (positive or
#'   negative) by choosing icon 1. \item \emph{icon2:} The icon presented in the
#'   second order position. \item \emph{icon2Group:} The group mapping for the
#'   icon presented in the second order position. \item \emph{icon2Prob:} The
#'   probability of getting a reinforcing image by choosing icon 2. \item
#'   \emph{icon2Sign:} The relative sign of the reinforcing image (positive or
#'   negative) by choosing icon 2. }
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{get_icons(sampleChoiceData, specific_phase = 1)}

get_icons <- function(data, specific_phase = NULL) {

  get_phases(data)
  #create group key, keeping only the relevant columns
  groupKey <- get_group_info(data) %>%
    dplyr::select(-c(.data$reward, .data$EU, .data$rank))
  phases <- list(phase$one, phase$two, phase$three)

  #For each phase of the data, get the icon information and then join it with
  #the groupKey information. Specifically, add the icon groups, signs, and
  #probabilities.
  icons <- purrr::map_df(phases, ~get_icons_internal(.x))

  icon1 <- icons %>%
    dplyr::select(.data$phase, .data$round, icon = .data$icon1) %>%
    dplyr::left_join(groupKey, by = c("phase", "icon")) %>%
    dplyr::rename(icon1 = .data$icon,
                  icon1Group = .data$group,
                  icon1Prob = .data$probability,
                  icon1Sign = .data$sign)

  icon2 <- icons %>%
    dplyr::select(phase, .data$round, icon = .data$icon2) %>%
    dplyr::left_join(groupKey, by = c("phase", "icon")) %>%
    dplyr::rename(icon2 = .data$icon,
                  icon2Group = .data$group,
                  icon2Prob = .data$probability,
                  icon2Sign = .data$sign)

  together <- dplyr::left_join(icon1, icon2, by = c("phase", "round"))

  # if specific_phase is specified, filter the data for that phase.
  if (!base::is.null(specific_phase)) {
    together <- together %>% dplyr::filter(phase == {{ specific_phase }})
  }

  return(together)

}
