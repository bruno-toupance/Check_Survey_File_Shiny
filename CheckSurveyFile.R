#==============================================================================
#    CheckSurveyFile.R : Check Survey File
#    Copyright (C) 2023  Bruno Toupance <bruno.toupance@mnhn.fr>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#==============================================================================



#==============================================================================
# Values to be modified every year
#==============================================================================

# Current year
year <- 2023

# Extra variable names
extra_var <- c("sleep", "pets")

# Extra variable minimum values
extra_min <- c(0, 0)

# Extra variable maximum values
extra_max <- c(24, 100)

# Extra variable precision values: "value * mul" must be an integer
extra_mul <- c(2.0, 1.0)


# Debug flag
show_all <- FALSE



#==============================================================================
# Separator constants
#==============================================================================
SEP_60 <- "#----------------------------------------------------------"
TAB_1 <- "  "
TAB_2 <- "    "
TAB_3 <- "      "


#==============================================================================
# qualitative_check
#==============================================================================
qualitative_check <- function(survey_df, var_name, expected_levels) {

    txt <- sprintf("# CHECK Column [%s]", var_name)
    log_msg <- c("", SEP_60, txt, SEP_60)

    expected_levels <- unique(sort(expected_levels))
    names(survey_df) <- tolower(names(survey_df))

    if (var_name %in% names(survey_df)) {
        X <- survey_df[, var_name]

        if (class(X) == "character") {
            # observed_levels <- sort(levels(X))
            observed_levels <- unique(sort(X))

            result <- TRUE
            for (i in observed_levels) {
                result <- result & (i %in% expected_levels)    
            }

            if (result) {

                # txt <- sprintf("%sPASS", TAB_1)
                # log_msg <- c(log_msg, txt)
                log_msg <- NULL

            } else {

                txt <- sprintf("%sFAIL: Unexpected Levels...", TAB_1)
                log_msg <- c(log_msg, txt)

                txt <- paste(expected_levels, collapse = "] [")
                txt <- sprintf("%sExpected: [%s]", TAB_2, txt)
                log_msg <- c(log_msg, txt)

                txt <- paste(observed_levels, collapse = "] [")
                txt <- sprintf("%sObserved: [%s]", TAB_2, txt)
                log_msg <- c(log_msg, txt)

                unexpected_levels <- setdiff(observed_levels, expected_levels)
                
                for (level in unexpected_levels) {
                    ind_vec <- which(X == level)
                    txt <- paste(ind_vec, collapse = "] [")
                    txt <- sprintf("%sCheck individual [%s]: [%s]", TAB_3, txt, level)
                    log_msg <- c(log_msg, txt)
                }
            }

        } else {

            txt <- class(X)
            txt <- sprintf("%sFAIL: Column should be coded as [character] not as [%s]...", TAB_1, txt)
            log_msg <- c(log_msg, txt)

        }

    } else {

        # txt <- sprintf("%sFAIL: Column not found...", TAB_1)
        # log_msg <- c(log_msg, txt)
        log_msg <- NULL

    }

    return(log_msg)
}
#==============================================================================



#==============================================================================
# id_column_check
#==============================================================================
id_column_check <- function(survey_df) {

    txt <- sprintf("# CHECK Column [id]")
    log_msg <- c("", SEP_60, txt, SEP_60)

    var_name = "id"
    names(survey_df) <- tolower(names(survey_df))
    
    obs_nb_row <- nrow(survey_df)

    if (var_name %in% names(survey_df)) {
        X <- survey_df[, var_name]

        if (class(X) == "character") {

            txt <- sprintf("%sFAIL: Unexpected values in column [id]...", TAB_1)
            log_msg <- c(log_msg, txt)

            txt <- paste(1:obs_nb_row, collapse = "] [")
            txt <- sprintf("%sExpected: [%s]", TAB_2, txt)
            log_msg <- c(log_msg, txt)

            txt <- paste(X, collapse = "] [")
            txt <- sprintf("%sObserved: [%s]", TAB_2, txt)
            log_msg <- c(log_msg, txt)

        } else {

            if (identical(1:obs_nb_row, X)) {

                # txt <- sprintf("%sPASS", TAB_1)
                # log_msg <- c(log_msg, txt)
                log_msg <- NULL

            } else {

                txt <- sprintf("%sFAIL: Unexpected values in column [id]...", TAB_1)
                log_msg <- c(log_msg, txt)

                txt <- paste(1:obs_nb_row, collapse = "] [")
                txt <- sprintf("%sExpected:[%s]", TAB_2, txt)
                log_msg <- c(log_msg, txt)

                txt <- paste(X, collapse = "] [")
                txt <- sprintf("%sObserved:[%s]", TAB_2, txt)
                log_msg <- c(log_msg, txt)

            }
        }

    } else {

        # txt <- sprintf("%sFAIL: Column not found...", TAB_1)
        # log_msg <- c(log_msg, txt)
        log_msg <- NULL

    }

    return(log_msg)
}
#==============================================================================






#==============================================================================
# binome_column_check
#==============================================================================
binome_column_check <- function(survey_df) {

    txt <- sprintf("# CHECK Column [binome]")
    log_msg <- c("", SEP_60, txt, SEP_60)

    var_name = "binome"
    names(survey_df) <- tolower(names(survey_df))

    if (var_name %in% names(survey_df)) {

        X <- unique(survey_df[, var_name])

        if ( (length(X) == 1) & (class(X) == "character") ) {

            XX <- as.character(X)
            binome_check <- grep("^BIN_[0-2][0-9]$", XX, perl = TRUE)

            if (length(binome_check) == 1) {

                if (XX == "BIN_00") {

                    txt <- sprintf("%sFAIL: Unexpected values in column [binome]...", TAB_1)
                    log_msg <- c(log_msg, txt)

                    txt <- sprintf("%sUse your binome id, not 00", TAB_2)
                    log_msg <- c(log_msg, txt)

                } else {
                    if (show_all) {
                        txt <- sprintf("%sPASS", TAB_1)
                        log_msg <- c(log_msg, txt)
                    } else {
                        log_msg <- NULL    
                    }
                }

            } else {

                txt <- sprintf("%sFAIL: Unexpected values in column [binome]...", TAB_1)
                log_msg <- c(log_msg, txt)

                txt <- sprintf("%sExpected pattern: [%s]", TAB_2, "BIN_##")
                log_msg <- c(log_msg, txt)

                txt <- paste(X, collapse = "] [")
                txt <- sprintf("%sObserved values:  [%s]", TAB_2, txt)
                log_msg <- c(log_msg, txt)

            }

        } else {

            txt <- sprintf("%sFAIL: Unexpected values in column [binome]...", TAB_1)
            log_msg <- c(log_msg, txt)

            txt <- sprintf("%sExpected pattern: [%s]", TAB_2, "BIN_##")
            log_msg <- c(log_msg, txt)

            txt <- paste(X, collapse = "] [")
            txt <- sprintf("%sObserved values:  [%s]", TAB_2, txt)
            log_msg <- c(log_msg, txt)

        }

    } else {

        # txt <- sprintf("%sFAIL: Column not found...", TAB_1)
        # log_msg <- c(log_msg, txt)
        log_msg <- NULL

    }

    return(log_msg)
}
#==============================================================================



#==============================================================================
# quantitative_check
#==============================================================================
quantitative_check <- function(survey_df, var_name, min_val, max_val, mul = 1.0) {

    txt <- sprintf("# CHECK Column [%s]", var_name)
    log_msg <- c("", SEP_60, txt, SEP_60)
    
    error_flag <- FALSE

    names(survey_df) <- tolower(names(survey_df))
    if (var_name %in% names(survey_df)) {
        X <- survey_df[, var_name]

        if (class(X) == "character") {
            error_flag <- TRUE
            
            XX <- as.character(X)

            txt <- sprintf("%sFAIL: Column should be numeric...", TAB_1)
            log_msg <- c(log_msg, txt)

            non_numeric <- ""
            for (k in 1:length(XX)) {
                if (is.na(as.numeric(XX[k]))) {
                    non_numeric <- sprintf("%s [%s]", non_numeric, XX[k])
                }
            }

            txt <- sprintf("%sRemember to use '.' as decimal separator", TAB_2)
            log_msg <- c(log_msg, txt)

            txt <- sprintf("%sCheck values: %s", TAB_2, non_numeric)
            log_msg <- c(log_msg, txt)

        } else {

            min_x <- min(X, na.rm = TRUE)
            max_x <- max(X, na.rm = TRUE)

            if ( (max_x > max_val) || (min_x < min_val) ) {
                error_flag <- TRUE

                txt <- sprintf("[%s, %s]", as.character(min_val), as.character(max_val))
                txt <- sprintf("%sExpected Range: %s", TAB_1, txt)
                log_msg <- c(log_msg, txt)

                txt <- sprintf("[%s, %s]", as.character(min_x), as.character(max_x))
                txt <- sprintf("%sObserved Range: %s", TAB_1, txt)
                log_msg <- c(log_msg, txt)

                txt <- sprintf("%sWARNING: Some values are out of bounds...", TAB_1)
                log_msg <- c(log_msg, txt)

                for (k in 1:length(X)) {
                    if (! is.na(X[k])) {
                        if (X[k] > max_val) {
                            txt <- sprintf("%sCheck individual [%s]: Unexpected high value [%s]", TAB_2, k, X[k])
                            log_msg <- c(log_msg, txt)
                        }
                        if (X[k] < min_val) {
                            txt <- sprintf("%sCheck individual [%s]: Unexpected low value [%s]", TAB_2, k, X[k])
                            log_msg <- c(log_msg, txt)
                        }
                    }
                }
            }

            XX <- mul * X
            dec_pos <- which(XX != round(XX))
            if (length(dec_pos) > 0) {
                error_flag <- TRUE
                
                txt <- sprintf("%sWARNING: Unexpected precision...", TAB_1)
                log_msg <- c(log_msg, txt)

                for (k in dec_pos) {
                    txt <- sprintf("%sCheck individual [%s]: Unexpected numerical value [%s]", TAB_2, k, X[k])
                    log_msg <- c(log_msg, txt)
                }
            }
            
            if (! error_flag) {
                if (show_all) {
                    txt <- sprintf("%sPASS", TAB_1)
                    log_msg <- c(log_msg, txt)
                } else {
                    log_msg <- NULL    
                }
            }
            
        }

    } else {
        if (show_all) {
            txt <- sprintf("%sPASS", TAB_1)
            log_msg <- c(log_msg, txt)
        } else {
            log_msg <- NULL    
        }
    }

    return(log_msg)
}
#==============================================================================



#==============================================================================
# dimension_check
#==============================================================================
dimension_check <- function(survey_df) {
    log_msg <- NULL

    exp_nb_col <- 11 + length(extra_var)
    obs_nb_col <- ncol(survey_df)

    if (obs_nb_col != exp_nb_col) {

        txt <- sprintf("%sFAIL: Unexpected number of columns...", TAB_1)
        log_msg <- c(log_msg, txt)

        txt <- sprintf("%sExpected [%d]", TAB_2, exp_nb_col)
        log_msg <- c(log_msg, txt)

        txt <- sprintf("%sObserved [%d]", TAB_2, obs_nb_col)
        log_msg <- c(log_msg, txt)

    }

    exp_nb_row <- 40
    obs_nb_row <- nrow(survey_df)

    if (obs_nb_row != exp_nb_row) {

        txt<- sprintf("%sFAIL: Unexpected number of lines...", TAB_1)
        log_msg <- c(log_msg, txt)

        txt <- sprintf("%sExpected [%d]", TAB_2, exp_nb_row)
        log_msg <- c(log_msg, txt)

        txt <- sprintf("%sObserved [%d]", TAB_2, obs_nb_row)
        log_msg <- c(log_msg, txt)

        if (obs_nb_row == exp_nb_row / 2) {
            txt <- sprintf("%sBut that's fine if your are 'solo'!", TAB_2)
            log_msg <- c(log_msg, txt)
            }

    }

    if (length(log_msg) > 0) {

        txt <- "# CHECK Dimension"
        log_msg <- c("", SEP_60, txt, SEP_60, log_msg)

    }

    return(log_msg)
}
#==============================================================================



#==============================================================================
# column_names_check
#==============================================================================
column_names_check <- function(survey_df) {

    txt <- "# CHECK Column Names"
    log_msg <- c("", SEP_60, txt, SEP_60)

    exp_var_names <- c("binome", "id", "annee", "mois", "sexe", "poids", 
                       "taille", "yeux", "cheveux", "pointure", "fratrie")
    exp_var_names <- c(exp_var_names, extra_var)
    
    obs_var_names <- names(survey_df)


    col_msg <- NULL

    for (i in exp_var_names) {

        if (! i %in% obs_var_names) {

            txt <- sprintf("%sFAIL: Column [%s] not found...", TAB_1, i)
            col_msg <- c(col_msg, txt)

            if (i %in% tolower(obs_var_names)) {

                pos <- which(tolower(obs_var_names) == i)

                txt <- sprintf("%sReplace [%s] by [%s]", TAB_2, obs_var_names[pos], i)
                col_msg <- c(col_msg, txt)

            }
        }
    }

    if (length(col_msg) > 0) {

        log_msg <- c(log_msg, col_msg)

        txt <- paste(exp_var_names, collapse = "] [")
        txt <- sprintf("%sExpected: [%s]", TAB_2, txt)
        log_msg <- c(log_msg, txt)

        txt <- paste(obs_var_names, collapse = "] [")
        txt <- sprintf("%sObserved: [%s]", TAB_2, txt)
        log_msg <- c(log_msg, txt)

    } else {

        if (all(exp_var_names == obs_var_names)) {

            # txt <- sprintf("%sPASS", TAB_1)
            # log_msg <- c(log_msg, txt)
            log_msg <- NULL

        } else {

            txt <- sprintf("%sFAIL: Unexpected column order...", TAB_1)
            log_msg <- c(log_msg, txt)

            txt <- paste(exp_var_names, collapse = "] [")
            txt <- sprintf("%sExpected: [%s]", TAB_2, txt)
            log_msg <- c(log_msg, txt)

            txt <- paste(obs_var_names, collapse = "] [")
            txt <- sprintf("%sObserved: [%s]", TAB_2, txt)
            log_msg <- c(log_msg, txt)

        }
    }

    return(log_msg)
}
#==============================================================================



#==============================================================================
# duplicated_individuals_check
#==============================================================================
duplicated_individuals_check <- function(survey_df) {

    txt <- "# CHECK Duplicated Individuals"
    log_msg <- c("", SEP_60, txt, SEP_60)

    ind_id <- apply(survey_df, 1, function(x) {paste0(x[-2], collapse = "")})
    dup_vec <- which(duplicated(ind_id))

    if (length(dup_vec) > 0 ) {
        txt <- sprintf("%sWARNING:", TAB_1)
        log_msg <- c(log_msg, txt)
        for (dup_ind in dup_vec) {
            ini_ind <- which(ind_id == ind_id[dup_ind])[1]
            txt <- sprintf("%sCheck duplicated individual [%d] == individual [%d]", TAB_2, dup_ind, ini_ind)
            log_msg <- c(log_msg, txt)
        }

    } else {

        # txt <- sprintf("%sPASS", TAB_1)
        # log_msg <- c(log_msg, txt)
        log_msg <- NULL

    }

    return(log_msg)
}
#==============================================================================



#==============================================================================
# do_check
#==============================================================================
do_check <- function(survey_filepath = "", survey_filename = "") {
    log_msg <- NULL

    if (file.exists(survey_filepath)) {
#------------------------------------------------------------------------------
        filename_check <- grep("^DATA_BIN_[0-2][0-9].txt$", 
                               survey_filename, 
                               perl = TRUE)

        if (length(filename_check) == 0) {

            txt <- "# CHECK File Name"
            log_msg <- c(log_msg, "", SEP_60, txt, SEP_60)

            txt <- sprintf("%sFAIL: Unexpected file name...", TAB_1)
            log_msg <- c(log_msg, txt)

            txt <- sprintf("%sExpected pattern: [DATA_BIN_##.txt]", TAB_2)
            log_msg <- c(log_msg, txt)

            txt <- survey_filename
            txt <- sprintf("%sObserved value:   [%s]", TAB_2, txt)
            log_msg <- c(log_msg, txt)

        }
#------------------------------------------------------------------------------
        survey_df <- read.table(survey_filepath, 
                                header = TRUE, 
                                sep = "\t", 
                                dec = ".", 
                                stringsAsFactors = FALSE)

        if (class(survey_df) == "data.frame") {

#------------------------------------------------------------------------------

            log_msg <- c(log_msg, dimension_check(survey_df))

#------------------------------------------------------------------------------

            log_msg <- c(log_msg, column_names_check(survey_df))
#------------------------------------------------------------------------------

            log_msg <- c(log_msg, duplicated_individuals_check(survey_df))
#------------------------------------------------------------------------------

            log_msg <- c(log_msg, id_column_check(survey_df))
            log_msg <- c(log_msg, binome_column_check(survey_df))
#------------------------------------------------------------------------------

            txt <- qualitative_check(survey_df, "sexe", c("H", "F"))
            log_msg <- c(log_msg, txt)

            txt <- qualitative_check(survey_df, "yeux", c("B", "V", "M"))
            log_msg <- c(log_msg, txt)

            txt <- qualitative_check(survey_df, "cheveux", c("BR", "BL", "CH", "RX"))
            log_msg <- c(log_msg, txt)

#------------------------------------------------------------------------------

            txt <- quantitative_check(survey_df, "mois", 1, 12)
            log_msg <- c(log_msg, txt)

            txt <- quantitative_check(survey_df, "annee", year - 122, year - 18)
            log_msg <- c(log_msg, txt)

            txt <- quantitative_check(survey_df, "poids", 39, 180)
            log_msg <- c(log_msg, txt)

            txt <- quantitative_check(survey_df, "taille", 130, 210)
            log_msg <- c(log_msg, txt)

            txt <- quantitative_check(survey_df, "pointure", 30, 50)
            log_msg <- c(log_msg, txt)

            txt <- quantitative_check(survey_df, "fratrie", 0, 40)
            log_msg <- c(log_msg, txt)

            txt <- quantitative_check(survey_df, extra_var[1], extra_min[1], extra_max[1], extra_mul[1])
            log_msg <- c(log_msg, txt)

            txt <- quantitative_check(survey_df, extra_var[2], extra_min[2], extra_max[2], extra_mul[2])
            log_msg <- c(log_msg, txt)

        } else {

            txt <- c("", SEP_60, "# CHECK Data Importation", SEP_60)
            log_msg <- c(log_msg, txt)

            txt <- sprintf("%sFAIL: Unable to open file as a [data.frame]...", TAB_1)
            log_msg <- c(log_msg, txt)

        }

    } else {

        txt <- "# CHECK File"
        log_msg <- c("", SEP_60, txt, SEP_60)

        txt <- sprintf("%sFAIL: Unable to open file...", TAB_1)
        log_msg <- c(log_msg, txt)

    }
#------------------------------------------------------------------------------
    if (length(log_msg) == 0) {

        log_msg <- "PASS"

    } else {

        txt <- "> MODIFY YOUR FILE AND RERUN THIS APP"
        log_msg <- c(log_msg, "", "", SEP_60, txt)

    }
#------------------------------------------------------------------------------
    log_msg <- paste(log_msg, collapse = "\n")
#------------------------------------------------------------------------------
    return(log_msg)
#------------------------------------------------------------------------------
}
#==============================================================================


