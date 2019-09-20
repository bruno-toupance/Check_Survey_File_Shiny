#==============================================================================
#    ui.R : Survey File Check - Shiny User Interface
#    Copyright (C) 2019  Bruno Toupance <bruno.toupance@mnhn.fr>
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

require(shiny)


#==============================================================================
# shinyUI
#==============================================================================
shinyUI(
	pageWithSidebar(
		headerPanel("L3-MEG Survey File Checker"),
		sidebarPanel(
			fileInput('survey_infile', 'Choose Survey File',
			accept=c('text/plain', '.txt'))
		),
		mainPanel(
			verbatimTextOutput('contents')
		)
	)
)
#==============================================================================
