#==============================================================================
#    CheckSurveyFile.R : Check Survey File
#    Copyright (C) 2021  Bruno Toupance <bruno.toupance@mnhn.fr>
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


SEP_60 <- "#----------------------------------------------------------"


#==============================================================================
# qualitative_check
#==============================================================================
qualitative_check <- function(survey_df, var_name, expected_levels) {
	log_message <- c("", SEP_60, 
		sprintf("# CHECK Column [%s]", var_name), SEP_60)
	expected_levels <- sort(expected_levels)
	names(survey_df) <- tolower(names(survey_df))
	if (var_name %in% names(survey_df)) { 
		X <- survey_df[, var_name]
		if (class(X) == "factor") {
			observed_levels <- sort(levels(X))
			result <- TRUE
			for (i in observed_levels) {
				result <- result & (i %in% expected_levels)
			}
			if (result) {
				# log_message <- c(log_message, "  PASS")
				log_message <- NULL
			} else {
				log_message <- c(log_message, "  FAIL: Unexpected Levels...")
				log_message <- c(log_message, 
					sprintf("        -> Expected: [%s]", 
						paste(expected_levels, collapse = "] [")))
				log_message <- c(log_message, 
					sprintf("        -> Observed: [%s]", 
						paste(observed_levels, collapse = "] [")))
			}
		} else {
			log_message <- c(log_message, 
				sprintf("  FAIL: Column should be coded as [factor] not as [%s]...", 
					class(X)))
		}
	} else {
		# log_message <- c(log_message, "  FAIL: Column not found...")
		log_message <- NULL
	}
	return(log_message)
}
#==============================================================================



#==============================================================================
# id_column_check
#==============================================================================
id_column_check <- function(survey_df) {
	log_message <- c("", SEP_60, sprintf("# CHECK Column [id]"), SEP_60)
	var_name = "id"
	names(survey_df) <- tolower(names(survey_df))
	if (var_name %in% names(survey_df)) { 
		X <- survey_df[, var_name]
		if (class(X) == "factor") {
			log_message <- c(log_message, 
				sprintf("  FAIL: Unexpected values in column [id]..."))
			log_message <- c(log_message, 
				sprintf("        -> Expected: [%s]", 
					paste(1:40, collapse = "] [")))
			log_message <- c(log_message, 
				sprintf("        -> Observed: [%s]", 
					paste(X,    collapse = "] [")))
		} else {
			if (all(1:40 == X)) {
				# log_message <- c(log_message, "  PASS")
				log_message <- NULL
			} else {
				log_message <- c(log_message, 
					sprintf("  FAIL: Unexpected values in column [id]..."))
				log_message <- c(log_message, 
					sprintf("        -> Expected:[%s]", 
						paste(1:40, collapse = "] [")))
				log_message <- c(log_message, 
					sprintf("        -> Observed:[%s]", 
						paste(X, collapse = "] [")))
			}
		}
	} else {
		# log_message <- c(log_message, "  FAIL: Column not found...")
		log_message <- NULL
	}
	return(log_message)
}
#==============================================================================






#==============================================================================
# binome_column_check
#==============================================================================
binome_column_check <- function(survey_df) {
	log_message <- c("", SEP_60, sprintf("# CHECK Column [binome]"), SEP_60)
	var_name = "binome"
	names(survey_df) <- tolower(names(survey_df))
	if (var_name %in% names(survey_df)) { 
		X <- unique(survey_df[, var_name])
		if ( (length(X) == 1) & (class(X) == "factor") ) {
			XX <- as.character(X)
			binome_check <- grep("^BIN_[0-2][0-9]$", XX, perl = TRUE)
			if (length(binome_check) == 1) {
				if (XX == "BIN_00") {
					log_message <- c(log_message, 
						sprintf("  FAIL: Unexpected values in column [binome]..."))
					log_message <- c(log_message, 
						sprintf("        -> Use your binome id, not 00"))
				} else {
					# log_message <- c(log_message, "  PASS")
					log_message <- NULL
				}
			} else {
				log_message <- c(log_message, 
					sprintf("  FAIL: Unexpected values in column [binome]..."))
				log_message <- c(log_message, 
					sprintf("        -> Expected pattern: [%s]", "BIN_##"))
				log_message <- c(log_message, 
					sprintf("        -> Observed values:  [%s]", 
						paste(X, collapse = "] [")))
			}
		} else {
			log_message <- c(log_message, 
				sprintf("  FAIL: Unexpected values in column [binome]..."))
			log_message <- c(log_message, 
				sprintf("        -> Expected pattern: [%s]", "BIN_##"))
			log_message <- c(log_message, 
				sprintf("        -> Observed values:  [%s]", 
					paste(X, collapse = "] [")))
		}
	} else {
		# log_message <- c(log_message, "  FAIL: Column not found...")
		log_message <- NULL
	}
	return(log_message)
}
#==============================================================================



#==============================================================================
# quantitative_check
#==============================================================================
quantitative_check <- function(survey_df, var_name, min_value, max_value) {
	log_message <- c("", SEP_60, sprintf("# CHECK Column [%s]", var_name), SEP_60)
	names(survey_df) <- tolower(names(survey_df))
	if (var_name %in% names(survey_df)) { 
		X <- survey_df[, var_name]
		if (class(X) == "factor") {
			XX <- as.character(X)
			log_message <- c(log_message, "  FAIL: Column should be numeric...")
			non_numeric <- ""
			for (k in 1:length(XX)) {
				if (is.na(as.numeric(XX[k]))) {
					non_numeric <- sprintf("%s [%s]", non_numeric, XX[k])
				}
			}
			log_message <- c(log_message, 
				"        -> Remember to use '.' as decimal separator")
			log_message <- c(log_message, 
				sprintf("        -> Check values: %s", non_numeric))
		} else {
			min_x <- min(X, na.rm = TRUE)
			max_x <- max(X, na.rm = TRUE)
			if ( (max_x > max_value) || (min_x < min_value) ) {
				log_message <- c(log_message, 
					sprintf("  Expected Range: [%s, %s]", 
						as.character(min_value), as.character(max_value)))
				log_message <- c(log_message, 
					sprintf("  Observed Range: [%s, %s]", 
						as.character(min_x), as.character(max_x)))
				log_message <- c(log_message, "  WARNING: Unexpected Values...")
				high_values <- ""
				low_values <- ""
				for (k in 1:length(X)) {
				  if (! is.na(X[k])) {
				    if (X[k] > max_value) {
				      high_values <- sprintf("%s [%s]", high_values, X[k])
				    }
				    if (X[k] < min_value) {
				      low_values <- sprintf("%s [%s]", low_values, X[k])
				    }
				  }
				}
				if (max_x > max_value) {
					log_message <- c(log_message, 
						sprintf("        -> Check high values: %s", high_values))
				}
				if (min_x < min_value) {
					log_message <- c(log_message, 
						sprintf("        -> Check low values: %s", low_values))
				}
			} else {
				# log_message <- c(log_message, "  PASS")
				log_message <- NULL
			}
		}
	} else {
		# log_message <- c(log_message, "  FAIL: Column not found...")
		log_message <- NULL
	}
	return(log_message)
}
#==============================================================================



#==============================================================================
# dimension_check
#==============================================================================
dimension_check <- function(survey_df) {
	log_message <- NULL
	if (ncol(survey_df) != 13) {
		log_message <- c(log_message, 
			sprintf("  FAIL: Unexpected number of columns..."))
		log_message <- c(log_message, 
			sprintf("        -> Expected [%d] - Observed [%d]", 13, ncol(survey_df)))
	}
	if (nrow(survey_df) != 40) {
		log_message <- c(log_message, 
			sprintf("  FAIL: Unexpected number of lines..."))
		log_message <- c(log_message, 
			sprintf("        -> Expected [%d] - Observed [%d]", 40, nrow(survey_df)))
	}
	if (length(log_message) > 0) {
		log_message <- c("", SEP_60, "# CHECK Dimension", SEP_60, log_message)
	}
	return(log_message)
}
#==============================================================================



#==============================================================================
# column_names_check
#==============================================================================
column_names_check <- function(survey_df) {
	log_message <- c("", SEP_60, "# CHECK Column Names", SEP_60)
	expected_var_names <- c("binome", "id", "annee", "mois", "sexe", "poids", 
		"taille", "yeux", "cheveux", "pointure", "fratrie", "transport", "etude")
	observed_var_names <- names(survey_df)


	column_message <- NULL
	for (i in expected_var_names) {
		if (! i %in% observed_var_names) {
			column_message <- c(column_message, 
				sprintf("  FAIL: Column [%s] not found...", i))
			if (i %in% tolower(observed_var_names)) {
				pos <- which(tolower(observed_var_names) == i)
				column_message <- c(column_message, 
					sprintf("        -> Replace [%s] by [%s]", 
						observed_var_names[pos], i))
			}
		}
	}
	
	if (length(column_message) > 0) {
		log_message <- c(log_message, column_message)
		log_message <- c(log_message, 
			sprintf("        -> Expected: [%s]", 
				paste(expected_var_names, collapse = "] [")))
		log_message <- c(log_message, 
			sprintf("        -> Observed: [%s]", 
				paste(observed_var_names, collapse = "] [")))
	} else {
		if (all(expected_var_names == observed_var_names)) {
			# log_message <- c(log_message, "  PASS")
			log_message <- NULL
		} else {
			log_message <- c(log_message, 
				sprintf("  FAIL: Unexpected column order..."))
			log_message <- c(log_message, 
				sprintf("        -> Expected: [%s]", 
					paste(expected_var_names, collapse = "] [")))
			log_message <- c(log_message, 
				sprintf("        -> Observed: [%s]", 
					paste(observed_var_names, collapse = "] [")))
		}
	}
	
	return(log_message)
}
#==============================================================================



#==============================================================================
# duplicated_individuals_check
#==============================================================================
duplicated_individuals_check <- function(survey_df) {
	log_message <- c("", SEP_60, "# CHECK Duplicated Individuals", SEP_60)
	ID <- apply(survey_df, 1, function(x) {paste0(x[-2], collapse = "")})
	DUP <- which(duplicated(ID))
	if (length(DUP) > 0 ) {
		log_message <- c(log_message, 
			sprintf("  FAIL: [%s]", paste(DUP, collapse = "] [")))
	} else {
		# log_message <- c(log_message, "  PASS")
		log_message <- NULL
	}
	return(log_message)
}
#==============================================================================



#==============================================================================
# do_check
#==============================================================================
do_check <- function(survey_filepath = "", survey_filename = "") {
	log_message <- NULL
	if (file.exists(survey_filepath)) {
#------------------------------------------------------------------------------
		filename_check <- grep("^DATA_BIN_[0-2][0-9].txt$", survey_filename, perl = TRUE)
		if (length(filename_check) == 0) {
			log_message <- c(log_message, "", SEP_60, "# CHECK File Name", SEP_60)
			log_message <- c(log_message, 
				sprintf("  FAIL: Unexpected file name..."))
			log_message <- c(log_message, 
				sprintf("        -> Expected pattern: [DATA_BIN_##.txt]"))
			log_message <- c(log_message, 
				sprintf("        -> Observed value:   [%s]", survey_filename))
		}
#------------------------------------------------------------------------------
		survey_df <- read.table(survey_filepath, header = TRUE, sep = "\t", 
			dec = ".", stringsAsFactors = TRUE)
		if (class(survey_df) == "data.frame") {
#------------------------------------------------------------------------------
			log_message <- c(log_message, dimension_check(survey_df))
#------------------------------------------------------------------------------
			log_message <- c(log_message, column_names_check(survey_df))
#------------------------------------------------------------------------------
			log_message <- c(log_message, duplicated_individuals_check(survey_df))
#------------------------------------------------------------------------------
			log_message <- c(log_message, id_column_check(survey_df))
			log_message <- c(log_message, binome_column_check(survey_df))
#------------------------------------------------------------------------------
			log_message <- c(log_message, 
				qualitative_check(survey_df, "sexe", c("H", "F")))
			log_message <- c(log_message, 
				qualitative_check(survey_df, "yeux", c("B", "V", "M")))
			log_message <- c(log_message, 
				qualitative_check(survey_df, "cheveux", c("BR", "BL", "CH", "RX")))
			log_message <- c(log_message, 
				quantitative_check(survey_df, "mois", 1, 12))
			log_message <- c(log_message, 
				quantitative_check(survey_df, "annee", 1910, 2003))
			log_message <- c(log_message, 
				quantitative_check(survey_df, "poids", 39, 180))
			log_message <- c(log_message, 
				quantitative_check(survey_df, "taille", 130, 210))
			log_message <- c(log_message, 
				quantitative_check(survey_df, "pointure", 30, 50))
			log_message <- c(log_message, 
				quantitative_check(survey_df, "fratrie", 0, 40))
			log_message <- c(log_message, 
				quantitative_check(survey_df, "transport", 0, 6*60))
			log_message <- c(log_message, 
				quantitative_check(survey_df, "etude", 0, 30))
		} else {
			log_message <- c(log_message, "", SEP_60, "# CHECK Data Importation", SEP_60)
			log_message <- c(log_message, 
				sprintf("  FAIL: Unable to open file as a [data.frame]..."))
		}
	} else {
		log_message <- c(log_message, "", SEP_60, "# CHECK File", SEP_60)
		log_message <- c(log_message, sprintf("  FAIL: Unable to open file..."))
	}
#------------------------------------------------------------------------------
	if (length(log_message) == 0) {
		log_message <- "PASS"
	} else {
		log_message <- c(log_message, "", "", SEP_60, "> MODIFY YOUR FILE AND RERUN THIS APP")
	}
#------------------------------------------------------------------------------
	log_message <- paste(log_message, collapse = "\n")
#------------------------------------------------------------------------------
	return(log_message)
#------------------------------------------------------------------------------
}
#==============================================================================




#==============================================================================
# filepath <- file.choose()
# survey_df <- read.table(filepath, header=TRUE)
# do_check(filepath)
#==============================================================================
