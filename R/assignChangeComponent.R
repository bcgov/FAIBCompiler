#' Assign component change for repeatedly-measured trees
#'
#'
#' @description This function is to assign component change for repeatedly-measured trees based on
#'              trees' live and dead, and visit number
#'
#' @param treelist data.table, A tree list data table contains trees' unique tree id, i.e., site_identifier, plot, and tree_number,
#'                             trees' diameters (\code{DIAMETER}), live and dead status (\code{LV_D}),
#'                             and \code{MEASUREMENT_ANOMALY_CODE}
#' @param samples data.table, Table contains if a sample is measured (\code{SAMP_TYP}) in fixed area plot (\code{F}) or
#'                            variable area plot (\code{V}).
#'
#' @return one table that contains tree id and component change (column \code{COMPONENT_CHANGE}).
#'
#' \itemize{
#'  \item{\code{NA: }} {at the site first measurement, A tree was measured}
#'  \item{\code{I: }} {ingrowth, a tree passed the size shreshold in the sites' remeasurements}
#'  \item{\code{S: }} {suivival, a tree is live in both previous measurement and current measurement}
#'  \item{\code{M: }} {mortality, a tree is dead in current measurement but alive in the previous measurement}
#'  \item{\code{D: }} {dead, a tree is dead in current measurement and dead in previous measurement}
#'  \item{\code{H: }} {harvested, tree is harvested}
#'  \item{\code{DROP: }} {droped, a tree is dropped for some reason when it is live}
#'  }
#'
#'
#' @importFrom data.table ':=' setkey data.table
#' @importFrom FAIBBase merge_dupUpdate
#'
#'
#' @export
#' @docType methods
#' @rdname assignChangeComponent
#' @author Yong Luo
assignChangeComponent <- function(treelist, samples){
  treelist <- treelist[OUT_OF_PLOT_IND == "N" &
                         !(MEASUREMENT_ANOMALY_CODE %in% c("Z")),
                       .(SITE_IDENTIFIER, VISIT_NUMBER,
                         PLOT,
                         DIAMETER,
                         TREE_NO = TREE_NUMBER,
                         LV_D = TREE_EXTANT_CODE,
                         LVD_EDIT,
                         MSMT_MISSING_EDIT,
                         DIAMETER_EDIT,
                         MEASUREMENT_ANOMALY_CODE)]

  treelist <- rbind(treelist[MEASUREMENT_ANOMALY_CODE == "H"], # harvest trees regardless of diameter
                    treelist[(MEASUREMENT_ANOMALY_CODE != "H" |
                                is.na(MEASUREMENT_ANOMALY_CODE)) &
                               !is.na(DIAMETER)]) # non harvest trees, diameter must be present
  samples_change <- samples[SAMP_TYP == "F",]
  samples_change <- unique(samples_change[order(SITE_IDENTIFIER, VISIT_NUMBER),
                                          .(SITE_IDENTIFIER, VISIT_NUMBER)])
  samples_change[, ':='(VISIT_NUMBER_PREVIOUS = shift(VISIT_NUMBER, type = "lag"),
                        VISIT_NUMBER_NEXT = shift(VISIT_NUMBER, type = "lead"),
                        VISIT_NUMBER_SITE_FIRST = min(VISIT_NUMBER),
                        VISIT_NUMBER_SITE_LAST = max(VISIT_NUMBER),
                        NO_SITE_VISIT = length(unique(VISIT_NUMBER))),
                 by = "SITE_IDENTIFIER"]

  samples_change <- samples_change[NO_SITE_VISIT > 1, ]

  treelist <- merge(treelist,
                    samples_change[,.(SITE_IDENTIFIER, VISIT_NUMBER,
                                      VISIT_NUMBER_PREVIOUS,
                                      VISIT_NUMBER_NEXT,
                                      VISIT_NUMBER_SITE_FIRST,
                                      VISIT_NUMBER_SITE_LAST,
                                      NO_SITE_VISIT)],
                    by = c("SITE_IDENTIFIER", "VISIT_NUMBER"))


  treelist[, ':='(firstVisit_tree = min(VISIT_NUMBER),
                  lastVisit_tree = max(VISIT_NUMBER)),
           by = c("SITE_IDENTIFIER", "PLOT", "TREE_NO")]
  treelist <- treelist[order(SITE_IDENTIFIER, PLOT, TREE_NO, VISIT_NUMBER),]

  treelist[LV_D == "D",
           first_dead := min(VISIT_NUMBER),
           by = c("SITE_IDENTIFIER", "PLOT", "TREE_NO")]

  treelist[VISIT_NUMBER != VISIT_NUMBER_SITE_FIRST &
             VISIT_NUMBER_SITE_FIRST != firstVisit_tree &
             LV_D == "L" &
             firstVisit_tree == VISIT_NUMBER,
           ':='(COMPONENT_CHANGE = "I",
                VISIT_NUMBER_PREVIOUS = NA)]
  treelist[VISIT_NUMBER != VISIT_NUMBER_SITE_FIRST &
             first_dead == VISIT_NUMBER & first_dead != firstVisit_tree,
           COMPONENT_CHANGE := "M"]
  treelist[first_dead == VISIT_NUMBER &
             VISIT_NUMBER != VISIT_NUMBER_SITE_FIRST &
             first_dead == firstVisit_tree,
           COMPONENT_CHANGE := "D"] # tree at first msmt is dead
  treelist[LV_D == "D" & is.na(COMPONENT_CHANGE) &
             VISIT_NUMBER != VISIT_NUMBER_SITE_FIRST,
           COMPONENT_CHANGE := "D"] # dead trees after it turns to M and standing
  treelist[LV_D == "L" & is.na(COMPONENT_CHANGE) &
             VISIT_NUMBER != VISIT_NUMBER_SITE_FIRST,
           COMPONENT_CHANGE := "S"] # all the live trees except for ingrowth trees

  treelist[MEASUREMENT_ANOMALY_CODE == "H",
           COMPONENT_CHANGE := "H"]
  treelist[MEASUREMENT_ANOMALY_CODE == "D" & LV_D == "L",
           COMPONENT_CHANGE := "DROP"]

  treelist[,':='(VISIT_NUMBER_SITE_FIRST = NULL,
                 VISIT_NUMBER_SITE_LAST = NULL,
                 NO_SITE_VISIT = NULL,
                 firstVisit_tree = NULL,
                 lastVisit_tree = NULL,
                 first_dead = NULL)]
  treelist <- treelist[order(SITE_IDENTIFIER, PLOT, TREE_NO, VISIT_NUMBER),]
  return(treelist)
}
