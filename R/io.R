# Copyright (C) 2016-2018 Ren-Huai Huang <huangrenhuai@gmail.com>
#
# This file is part of rstarating.
#
# rstarating is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# rstarating is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with rstarating.  If not, see <http://www.gnu.org/licenses/>.


#' S3 method of print
#' @export
print.mstbl <- function(object,...) {
    cat("\n$class:\n");         print.default(class(object))
    cat("\n$structure:\n");     print.default(str(object,1))
    cat("\n$avalable fields:\n",names(object),"\n")
  }


#' Output Directory
#'
#' Check the input string as a directory. Create a directory if it doesn't exists.
#'
#' @param x A full path directory string.
#' @return A directory string.
#' @export
out_dir <- function(x) {
  if (!dir.exists(x)) dir.create(x,recursive=TRUE)
  x
}

#' Pipe
#'
#' Pipe function from magrittr package.
#'
`%>%` <- magrittr:::`%>%`


#' A modified ggplot
#'
#' A modified ggplot
#'
ggplot <- function() {
  require(ggplot2)
  g <- ggplot2::ggplot() + theme_classic()
  g <- g + theme(line = element_line(size=0.5,colour='black',linetype="solid"))
  g <- g + theme(plot.title= element_text(size=15, colour="blue", hjust=0.5))
  g <- g + theme(axis.text = element_text(size=10, colour="black"))
  g <- g + theme(axis.text.x=element_text(angle=30,hjust=1))
  g <- g + theme(axis.line  =element_line(size=0.5,colour='black',linetype="solid"))
  g <- g + theme(axis.line.x=element_line(size=0.5,colour='black',linetype="solid"))
  g <- g + theme(axis.line.y=element_line(size=0.5,colour="black",linetype="solid"))
  # g <- g + theme(panel.background=element_rect(fill="grey"))
}
