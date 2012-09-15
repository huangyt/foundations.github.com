PERL=perl

WINPERL=//d/programr/lang/perl/interpreter/perl-5.6.0/bin/perl
#PERL=$(WINPERL)
#PERL5LIB=d:/artefaktur/genweb/bin

SHELL=bash
SRC2HTML=src2html
WEBHOME=/artefaktur/wweb/acdk/modules
GENWEBHOME=../genweb
WINGENWEBHOME=..\\genweb
GENWEB_FWDID=-fwdid 'http://acdk.sourceforge.net/phpincl/gw/fwdid.php?id='


MAKETARGET=linux
# packages to include in distro
PKG_PACKAGES=acdk_core acdk_text acdk_make acdk_net acdk_boot acdk_xml acdk_vfile \
         acdk_lisp acdkx_com acdkx_orb  \
         acdk_perl acdk_python acdk_tcl acdk_java \
         acdk_sql acdk_sql_odbc

#package should build on linux in package process
PKG_BUILD=acdk_core acdk_text acdk_make acdk_net acdk_boot acdk_xml acdk_vfile \
         acdk_lisp acdkx_orb  



pkg_compile:
	@cd ..; \
	$(MAKE) pkg_compile; \

binpkg_filelist:
	cd ..; \
	$(PERL) ./distrib/backup.pl \
	--exclcvs --distignore \
	-ptsrc\.html -pids\.db -p\.\#.* \
	-etds -eilf -eils -eilc -eild -elimg -estackdump -eref -epft -esuo -epbi -epbt -edout -edb \
	-elog -e_ll -n_profile.txt -n_profile1.txt -nKopie \
	-dCVS -dtobj \
	--filelist --name acdk-pkg-filelist

docs-clean:
	cd ..; \
	find . -name '*.html' -exec rm {} ';'; \
	find . -name '*.txt' -exec rm {} ';'

HTML2TEXT="html2text -nobs -style pretty -rcfile $(GENWEBHOME)/html2textrc"
WHTML2TEXT="$(GENWEBHOME)/windows/html2text.exe -nobs -style pretty -rcfile $(GENWEBHOME)/html2textrc"
tools-docs::
	cd ..; \
	export GENWEBHOME=$(GENWEBHOME); \
	export SRC2HTML=$(SRC2HTML); \
	export PERL5LIB=$(GENWEBHOME):$(GENWEBHOME)/bin:$(PERL5LIB); export PATH=$(GENWEBHOME)/bin:$(PATH); \
	export GENWEBHOME=$(GENWEBHOME)/bin; \
	$(PERL) $(GENWEBHOME)/bin/genweb.pl -genweb-home $(GENWEBHOME)/bin -gw -imagesource docs/images -ignoreorg \
	                                    -src2html $(SRC2HTML) \
					    -root acdk_bin_tools -trace 1 \
	                                    $(GENWEB_FWDID) \
	                                    -template $(GENWEBHOME)/images/acdk_readme_html_template.htmt; \
        $(PERL) $(GENWEBHOME)/bin/genweb.pl -genweb-home $(GENWEBHOME)/bin -gw -noimages \
	                                    -src2html $(SRC2HTML) -html2text $(HTML2TEXT) \
					    -root acdk_bin_tools \
	                                    -ignoreorg -fileext '.txt' -format txt \
	                                    -template $(GENWEBHOME)/images/acdk_readme_txt_template.htmt; \



src-docs:
	cd ..; \
	$(MAKE) -f acdk_all.linux alldoc

tools-pkg:
	$(MAKE) docs-clean tools-docs; \
	cd ..; \
	$(SHELL) distrib/createtoolspkg.sh

src-pkg:
	$(MAKE) docs-clean src-docs; \
	cd ..; \
	$(SHELL) distrib/createpkg.sh
