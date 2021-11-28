## Makefile.am -- Config file for GNU Automake.
##
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
## along with Guile-Deck.  If not, see <http://www.gnu.org/licenses/>.

include $(top_srcdir)/build-aux/am/guile.am

SOURCES = \
	matrix-id.scm	\
	matrix-content-uri.scm	\
	matrix-event.scm 	\
	third-party-identifier.scm \
	presence.scm		\
	device.scm		\
	turn-server.scm		\
	state.scm		\
	filter.scm

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/deck/core/types
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/deck/core/types

SUFFIXES = .scm .go

guilec_opts = --load-path=$(abs_top_srcdir)/modules/
.scm.go:
	$(AM_V_GUILEC)$(guilec_env) $(GUILEC) $(guilec_opts) \
	--output=$@ $<

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

# Makefile.am ends here
