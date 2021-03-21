## Copyright (C) 2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
##
## This file is part of Guile-Deck.
##
## Guile-Deck is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## Guile-Deck is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Guile-Deck.  If not, see
## <http://www.gnu.org/licenses/>.

bin_SCRIPTS = hello-bot.scm send-message.scm
hello_bot_scm_SOURCES = hello-bot.scm.in
send_message_scm_SOURCES = send-message.scm.in

EXTRA_DIST = \
	hello-bot.scm.in	\
	send-message.scm.in

SOURCES = \
	hello-bot.scm.in	\
	send-message.scm.in

examplesdir = $(pkgdatadir)/examples
dist_examples_DATA = \
	hello-bot.scm		\
	send-message.scm

substitute = sed -e 's,[@]GUILE[@],$(GUILE),g'

SUFFIXES = .in

.in:
	$(MKDIR_P) "`dirname "$@"`"
	$(substitute) "$^" > "$@.tmp" && 	\
	chmod +x "$@.tmp" &&			\
	mv "$@.tmp" "$@"

CLEANFILES = \
	$(bin_SCRIPTS)

