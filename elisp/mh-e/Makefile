# Makefile for MH-E.

# Copyright (C) 2003, 2004, 2005, 2006 Bill Wohler

# This file is part of MH-E.

# MH-E is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# MH-E is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with MH-E; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

#
# Note: most changes to this file should also be mirrored in the Makefiles in
# $(TOP)/doc and $(TOP)/contrib.
#
TOP		= ..

CVSROOT		= cvs.savannah.gnu.org:/cvsroot/emacs

CVSLOGIN	= anoncvs

# Developers can use "ext".
CVSMETHOD	= pserver

# Program to install files
INSTALL		= install -m 644

# MH-E source.
MH_E_SRC	=			\
        emacs/lisp/mh-e/mh-alias.el	\
        emacs/lisp/mh-e/mh-buffers.el	\
        emacs/lisp/mh-e/mh-comp.el	\
        emacs/lisp/mh-e/mh-e.el		\
        emacs/lisp/mh-e/mh-folder.el	\
        emacs/lisp/mh-e/mh-funcs.el	\
        emacs/lisp/mh-e/mh-identity.el	\
        emacs/lisp/mh-e/mh-inc.el	\
        emacs/lisp/mh-e/mh-junk.el	\
        emacs/lisp/mh-e/mh-letter.el	\
        emacs/lisp/mh-e/mh-limit.el	\
        emacs/lisp/mh-e/mh-mime.el	\
        emacs/lisp/mh-e/mh-print.el	\
        emacs/lisp/mh-e/mh-scan.el	\
        emacs/lisp/mh-e/mh-search.el	\
        emacs/lisp/mh-e/mh-seq.el	\
        emacs/lisp/mh-e/mh-show.el	\
        emacs/lisp/mh-e/mh-speed.el	\
        emacs/lisp/mh-e/mh-thread.el	\
        emacs/lisp/mh-e/mh-tool-bar.el	\
        emacs/lisp/mh-e/mh-utils.el	\
        emacs/lisp/mh-e/mh-xface.el

MH_E_OBJ	=  $(MH_E_SRC:.el=.elc)

MH_E_SRC_NONCOMPILED =			\
	emacs/lisp/mh-e/mh-acros.el	\
	emacs/lisp/mh-e/mh-compat.el	\
	emacs/lisp/mh-e/mh-gnus.el

MH_E_XEMACS_SRC	= mh-xemacs.el

MH_E_XEMACS_OBJ	= $(MH_E_XEMACS_SRC:.el=.elc)

# Automatically generated files.
MH_E_LOADDEFS	= emacs/lisp/mh-e/mh-loaddefs.el

MH_E_LOADDEFS_SRC = $(MH_E_SRC)

MH_E_AUTOLOADS	= emacs/lisp/mh-e/mh-autoloads.el

MH_E_CUSTOM_LOAD = emacs/lisp/mh-e/mh-cus-load.el

MH_E_AUTOGEN	=			\
	$(MH_E_AUTOLOADS)		\
	$(MH_E_CUSTOM_LOAD)		\
	$(MH_E_LOADDEFS)

# Emacs images needed by MH-E.
# These names are duplicated in mh-xemacs.el and mh-tool-bar.el.
MH_E_IMG	=					\
	emacs/etc/images/attach.pbm			\
	emacs/etc/images/attach.xpm			\
	emacs/etc/images/contact.pbm			\
	emacs/etc/images/contact.xpm			\
	emacs/etc/images/copy.pbm			\
	emacs/etc/images/copy.xpm			\
	emacs/etc/images/cut.pbm			\
	emacs/etc/images/cut.xpm			\
	emacs/etc/images/data-save.pbm			\
	emacs/etc/images/data-save.xpm			\
	emacs/etc/images/delete.pbm			\
	emacs/etc/images/delete.xpm			\
	emacs/etc/images/help.pbm			\
	emacs/etc/images/help.xpm			\
	emacs/etc/images/left-arrow.pbm			\
	emacs/etc/images/left-arrow.xpm			\
	emacs/etc/images/mail/compose.pbm		\
	emacs/etc/images/mail/compose.xpm		\
	emacs/etc/images/mail/flag-for-followup.pbm	\
	emacs/etc/images/mail/flag-for-followup.xpm	\
	emacs/etc/images/mail/inbox.pbm			\
	emacs/etc/images/mail/inbox.xpm			\
	emacs/etc/images/mail/move.pbm			\
	emacs/etc/images/mail/move.xpm			\
	emacs/etc/images/mail/repack.pbm		\
	emacs/etc/images/mail/repack.xpm		\
	emacs/etc/images/mail/reply-all.pbm		\
	emacs/etc/images/mail/reply-all.xpm		\
	emacs/etc/images/mail/reply-from.pbm		\
	emacs/etc/images/mail/reply-from.xpm		\
	emacs/etc/images/mail/reply-to.pbm		\
	emacs/etc/images/mail/reply-to.xpm		\
	emacs/etc/images/mail/reply.pbm			\
	emacs/etc/images/mail/reply.xpm			\
	emacs/etc/images/mail/send.pbm			\
	emacs/etc/images/mail/send.xpm			\
	emacs/etc/images/mh-logo.xpm			\
	emacs/etc/images/next-page.pbm			\
	emacs/etc/images/next-page.xpm			\
	emacs/etc/images/open.pbm			\
	emacs/etc/images/open.xpm			\
	emacs/etc/images/paste.pbm			\
	emacs/etc/images/paste.xpm			\
	emacs/etc/images/preferences.pbm		\
	emacs/etc/images/preferences.xpm		\
	emacs/etc/images/refresh.pbm			\
	emacs/etc/images/refresh.xpm			\
	emacs/etc/images/right-arrow.pbm		\
	emacs/etc/images/right-arrow.xpm		\
	emacs/etc/images/search.pbm			\
	emacs/etc/images/search.xpm			\
	emacs/etc/images/save.pbm			\
	emacs/etc/images/save.xpm			\
	emacs/etc/images/show.pbm			\
	emacs/etc/images/show.xpm			\
	emacs/etc/images/spell.pbm			\
	emacs/etc/images/spell.xpm			\
	emacs/etc/images/undo.pbm			\
	emacs/etc/images/undo.xpm			\
	emacs/etc/images/zoom-out.pbm			\
	emacs/etc/images/zoom-out.xpm	

# Other MH-E files.
MH_E_ETC	=			\
	COPYING				\
	ChangeLog			\
	ChangeLog.1			\
	Makefile			\
	README				\
	emacs/etc/MH-E-NEWS		\
	emacs/lisp/mh-e/ChangeLog	\
	emacs/lisp/mh-e/ChangeLog.1

# MH-E maintainer files.
MH_E_MAINT	=			\
	.cvsignore			\
	emacs/lisp/mh-e/.cvsignore	\
	mh-unit.el			\
	release-utils

# MH-E files for distribution.
MH_E_DIST_NOAUTOGEN =			\
	$(MH_E_SRC)			\
	$(MH_E_SRC_NONCOMPILED)		\
	$(MH_E_XEMACS_SRC)		\
	$(MH_E_IMG)			\
	$(MH_E_ETC)

# MH-E files for distribution, including automatically generated files.
MH_E_DIST	=			\
	$(MH_E_DIST_NOAUTOGEN)		\
	$(MH_E_AUTOGEN)

# All MH-E files, excluding automatically generated files.
MH_E_ALL	=			\
	$(MH_E_DIST_NOAUTOGEN)		\
	$(MH_E_MAINT)

# Compiling variables.
EMACS		= emacs

# emacs/lisp/mh-e only needed by custom-deps on Emacs 21.
EMACS_OPTIONS   = --batch --no-site-file --multibyte \
	--eval "(setq load-path (cons \".\" (cons \"emacs/lisp/mh-e\" load-path)))"

EMACS_LOADDEFS_COOKIE = \
	--eval "(setq generate-autoload-cookie \";;;\#\#\#mh-autoload\")"

# autoload-package-name needed by XEmacs; harmless on GNU Emacs.
EMACS_EXPORT_MH_LOADDEFS = \
	--eval "(setq generated-autoload-file (expand-file-name \"$(MH_E_LOADDEFS)\"))" \
	--eval "(setq make-backup-files nil)" \
	--eval "(setq autoload-package-name \"mh-loaddefs\")"

EMACS_EXPORT_MH_AUTOLOADS = \
	--eval "(setq generated-autoload-file (expand-file-name \"$(MH_E_AUTOLOADS)\"))" \
	--eval "(setq make-backup-files nil)" \
	--eval "(setq autoload-package-name \"mh\")"

COMPILE_COMMAND	= $(EMACS) $(EMACS_OPTIONS)

# XEmacs variables.
XEMACS	        = false

XEMACS_OPTIONS  = --batch --vanilla \
	--eval "(setq load-path (cons (expand-file-name \"emacs/lisp/mh-e\") load-path))" \
	--eval "(setq load-path (cons \".\" load-path))"

# Since sed did not support the -i (--inplace) option at the time of
# the initial Emacs 21 release, use a temporary file instead of sed -i
# as long as we support Emacs 21. Solaris 9 sed also had problems
# using an expression to insert multiple lines starting at line 2 of
# the file, so we may end up keeping an intermediate temporary file as
# a workaround for longer than just support of the Emacs 21 release.
MH_E_TMP 	= /tmp/mh-e$$$$

# Default target. Works for both Emacs and XEmacs.
all: emacs
	case $(EMACS) in \
	*xemacs*) \
	    $(MAKE) EMACS='$(EMACS)' \
		EMACS_OPTIONS='$(XEMACS_OPTIONS)' \
		XEMACS=: \
		xemacs ;;\
	*) \
	     $(MAKE) EMACS='$(EMACS)' \
		Emacs ;;\
	esac

# Check out emacs directory if necessary.
emacs:
	cvs -d :$(CVSMETHOD):$(CVSLOGIN)@$(CVSROOT) co emacs/lisp/mh-e
	cvs -d :$(CVSMETHOD):$(CVSLOGIN)@$(CVSROOT) co -l emacs/etc/MH-E-NEWS \
	  emacs/etc/NEWS emacs/etc/ChangeLog \
	  emacs/etc/images emacs/etc/images/mail

emacs-devel:
	cvs -d $(USER)@$(CVSROOT) co emacs

# The .PHONY targets are always considered to be out-of-date.
# This rule needs to be after the ``all'' rule for compatibility 
# with old versions of make.
.PHONY: all clean compile Emacs emacs-devel

# Ensure clean compile which is advised since old .elc files can
# interfere with the compilation process.
Emacs: clean loaddefs compile autoloads custom-deps

# Remove everything we make.
clean::
	rm -rf $(MH_E_OBJ) $(MH_E_AUTOGEN)

# Compile MH-E. See $EMACS_HOME/lisp/Makefile for commentary.
compile: $(MH_E_SRC)
	$(COMPILE_COMMAND) -f batch-byte-compile $?

# Automatically extract internal autoloads.
MH_E_LOADDEFS_HEADER =\
";;; mh-loaddefs.el --- automatically extracted internal autoloads for MH-E\
\n;;\
\n;; Copyright (C) 2003, 2004, 2005, 2006 Free Software Foundation, Inc.\
\n;; Author: Bill Wohler <wohler@newt.com>\
\n;; Keywords: mail\
\n;;; Commentary:\
\n;;; Change Log:\
\n;;; Code:\n\n"

MH_E_LOADDEFS_FOOTER =\
"\f\
\n(provide 'mh-loaddefs)\
\n;; Local Variables:\
\n;; version-control: never\
\n;; no-byte-compile: t\
\n;; no-update-autoloads: t\
\n;; End:\
\n;;; mh-loaddefs.el ends here\n"

.PHONY: loaddefs
loaddefs: $(MH_E_LOADDEFS)
$(MH_E_LOADDEFS): $(MH_E_LOADDEFS_SRC)
        # For XEmacs, the autoload file must not be present.
	if $(XEMACS); then :; else \
		printf $(MH_E_LOADDEFS_HEADER) > $@; \
	fi
	$(COMPILE_COMMAND) \
		-l autoload \
		$(EMACS_LOADDEFS_COOKIE) \
		$(EMACS_EXPORT_MH_LOADDEFS) \
		-f batch-update-autoloads emacs/lisp/mh-e
        # Prepend the header in case of XEmacs.
	if $(XEMACS); then \
		(printf $(MH_E_LOADDEFS_HEADER); cat $@) > $(MH_E_TMP); \
		mv $(MH_E_TMP) $@; \
	fi; \
	printf $(MH_E_LOADDEFS_FOOTER) >> $@

# Automatically extract external autoloads.
MH_E_AUTOLOADS_HEADER =\
";;; mh-autoloads.el --- automatically extracted external autoloads for MH-E\
\n\
\n;; Copyright (C) 2003, 2004, 2005, 2006 Free Software Foundation, Inc.\
\n;; Author: Bill Wohler <wohler@newt.com>\
\n;; Keywords: mail\
\n;;; Commentary:\
\n\
\n;; If you installed MH-E separately from Emacs, you can set up its entry\
\n;; points using the following in your .emacs file:\
\n\
\n;;   (require 'mh-autoloads)\
\n\
\n;;; Change Log:\
\n;;; Code:\n\n"

MH_E_AUTOLOADS_FOOTER =\
"\f\
\n(provide 'mh-autoloads)\
\n;; Local Variables:\
\n;; version-control: never\
\n;; no-byte-compile: t\
\n;; no-update-autoloads: t\
\n;; End:\
\n;;; mh-autoloads.el ends here\n"

.PHONY: autoloads
autoloads: $(MH_E_AUTOLOADS)
$(MH_E_AUTOLOADS): $(MH_E_SRC)
        # In GNU Emacs, the autoloads must be present; in XEmacs, it must not.
	if $(XEMACS); then :; else \
		printf $(MH_E_AUTOLOADS_HEADER) > $@; \
	fi
	$(COMPILE_COMMAND) \
		-l autoload \
		$(EMACS_EXPORT_MH_AUTOLOADS) \
		-f batch-update-autoloads emacs/lisp/mh-e
        # Prepend the header in case of XEmacs.
	if $(XEMACS); then \
		(printf $(MH_E_AUTOLOADS_HEADER); cat $@) > $(MH_E_TMP); \
		mv $(MH_E_TMP) $@; \
	fi; \
	printf $(MH_E_AUTOLOADS_FOOTER) >> $@

# Automatically extract custom autoloads.
# Emacs 22 has generated-custom-dependencies-file, but Emacs 21 hard-codes
# the output file to cus-load.el. XEmacs has cusload-base-file which is set to
# custom-load.el.
MH_E_CUSTOM_LOAD_HEADER =\
"\
\n;; Copyright (C) 2003, 2004, 2005, 2006 Free Software Foundation, Inc.\
\n;; Author: Bill Wohler <wohler@newt.com>\
\n;; Keywords: mail\
\n;;; Commentary:\
\n\
\n;; If you installed MH-E separately from Emacs, you can set up its custom\
\n;; dependencies using the following in your .emacs file:\
\n\
\n;;   (require 'mh-cus-load)\
\n\
\n;;; Change Log:\n\n"

MH_E_CUSTOM_LOAD_FOOTER =\
"\f\
\n(provide 'mh-cus-load)\
\n;; Local Variables:\
\n;; version-control: never\
\n;; no-byte-compile: t\
\n;; no-update-autoloads: t\
\n;; End:\n"

.PHONY: custom-deps
custom-deps: $(MH_E_CUSTOM_LOAD)
$(MH_E_CUSTOM_LOAD): $(MH_E_SRC)
	if $(XEMACS); then \
		$(COMPILE_COMMAND) \
			-l cus-dep -l emacs/lisp/mh-e/mh-e \
			-f Custom-make-dependencies emacs/lisp/mh-e; \
		(sed -e "s/custom-load/mh-cus-load/" \
			emacs/lisp/mh-e/custom-load.el; \
		 printf $(MH_E_CUSTOM_LOAD_FOOTER)) > $(MH_E_TMP); \
		rm -f emacs/lisp/mh-e/custom-load.el; \
	else \
		$(COMPILE_COMMAND) \
			-l cus-dep -l emacs/lisp/mh-e/mh-e \
			-f custom-make-dependencies emacs/lisp/mh-e; \
		sed -e "s/cus-load/mh-cus-load/" cus-load.el > $(MH_E_TMP); \
		rm -f cus-load.el; \
	fi; \
	(sed -e 2,\$$d $(MH_E_TMP); \
	 printf $(MH_E_CUSTOM_LOAD_HEADER); \
	 sed -e 1d $(MH_E_TMP)) > $@; \
	rm -f $(MH_E_TMP)

# XEmacs targets.
.PHONY: xemacs compile-xemacs
xemacs: clean loaddefs compile-xemacs autoloads custom-deps

clean::
	rm -rf $(MH_E_XEMACS_OBJ)

compile-xemacs: $(MH_E_SRC) $(MH_E_XEMACS_SRC)
	$(COMPILE_COMMAND) -l bytecomp -f batch-byte-compile $?



#
# The following targets are only of interest to maintainers.
#
.PHONY: install-maint tag dist

# Installs MH-E outside of the source directory to provide a stable
# environment while developing. Two variables are used:
#
#   INSTALL_LISP_DIR - installation directory for compiled .elc and
# 	               non-compiled .el files
#   INSTALL_IMG_DIR -  installation directory for images
#
install-maint:
	@if [ -z "$(INSTALL_LISP_DIR)" -o ! -d "$(INSTALL_LISP_DIR)" ]; then \
		echo "Set INSTALL_LISP_DIR to an existing directory where" \
		     "lisp files should be installed."; \
	else \
		$(INSTALL) -c $(MH_E_SRC_NONCOMPILED) $(MH_E_OBJ) \
			$(MH_E_AUTOGEN) $(INSTALL_LISP_DIR); \
		for file in $(MH_E_XEMACS_OBJ); do \
			if [ -f $$file ]; then \
				$(INSTALL) -c $$file $(INSTALL_LISP_DIR); \
			fi \
		done; \
	fi
	@if [ -z "$(INSTALL_IMG_DIR)" -o ! -d "$(INSTALL_IMG_DIR)" ]; then \
		echo "Set INSTALL_IMG_DIR to an existing directory where" \
		     "images should be installed."; \
	else \
		$(INSTALL) -c $(MH_E_IMG) $(INSTALL_IMG_DIR); \
	fi

# Tag the MH-E files that are checked out.
tag:
	@TAG=$(TAG) ./release-utils --tag $(MH_E_ALL)

# Create a distribution suitable for installing at SourceForge.
dist: all
	@./release-utils --dist $(MH_E_DIST)

# The unit test that finds unused code runs much faster when compiled.
clean::
	rm -f mh-unit.elc

mh-unit.elc: mh-unit.el
	$(COMPILE_COMMAND) -f batch-byte-compile $?

# Back up MH-E CVS files at Savannah.
MH_BACKUP_DIR = ../backup
MH_DIRS = sources/emacs/emacs/lisp/mh-e
MH_FILES = emacs/etc/MH-E-NEWS $(MH_E_IMG)

backup:
	@[ -d $(MH_BACKUP_DIR) ] || mkdir $(MH_BACKUP_DIR)
	@(host=`echo $(CVSROOT) | awk -F: '{print $$1}'` \
	set $(MH_FILES); \
	files=`for i; do \
		    echo -n $$i|sed -e 's/^/sources\/emacs\//' -e 's/$$/,v /';\
		done`; \
	set $(MH_DIRS) $$files; \
	for file; do \
	    rsync -aziR $$host::$$file $(MH_BACKUP_DIR); \
	done;)
