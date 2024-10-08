#==============================================================================
#    ui.R : Survey File Check - Shiny Server
#    Copyright (C) 2024  Bruno Toupance <bruno.toupance@mnhn.fr>
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

library("shiny")

source("CheckSurveyFile.R")

#==============================================================================
# shinyServer
#==============================================================================
shinyServer(
    function(input, output) {
        output$contents <- renderText(
            {
                
                survey_infile <- input$survey_infile
                
                if (is.null(survey_infile)) {
                    return(NULL)
                }
                
                log_message <- do_check(survey_infile$datapath, survey_infile$name)
                
                return(log_message)
            }
        )
    }
)
