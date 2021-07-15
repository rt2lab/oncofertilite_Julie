# #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
#
#                        Copyright (c) 2020
#            Marcel Ribeiro-Dantas <marcel.ribeiro-dantas@curie.fr>
#
# This script is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This script is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

library(readr)

dataset <- readr::read_delim(file = 'BC_2010-16_SEER18.tsv',
                             delim = '\t',
                             col_types = paste(c(rep('c', 2), 'f',
                                                 rep('c', 43), rep('f', 3),
                                                 rep('c', 18), 'f',
                                                 rep('c', 8), 'f',
                                                 rep('c', 11), 'f',
                                                 rep('c', 2), 'f',
                                                 rep('c', 9), rep('f', 3),
                                                 'c'),
                                               collapse = '')
)
