#!perl

# We are miniperl, building extensions
# Reset @INC completely, adding the directories we need, and removing the
# installed directories (which we don't need to read, and may confuse us)
@INC = (q E:\MSVCLib\build\Perl_5_16_0\perl-5.16.0\cpan\AutoLoader\lib ,
        q E:\MSVCLib\build\Perl_5_16_0\perl-5.16.0\dist\Carp\lib ,
        q E:\MSVCLib\build\Perl_5_16_0\perl-5.16.0\dist\Cwd ,
        q E:\MSVCLib\build\Perl_5_16_0\perl-5.16.0\dist\Cwd\lib ,
        q E:\MSVCLib\build\Perl_5_16_0\perl-5.16.0\dist\ExtUtils-Command\lib ,
        q E:\MSVCLib\build\Perl_5_16_0\perl-5.16.0\dist\ExtUtils-Install\lib ,
        q E:\MSVCLib\build\Perl_5_16_0\perl-5.16.0\cpan\ExtUtils-MakeMaker\lib ,
        q E:\MSVCLib\build\Perl_5_16_0\perl-5.16.0\dist\ExtUtils-Manifest\lib ,
        q E:\MSVCLib\build\Perl_5_16_0\perl-5.16.0\cpan\File-Path\lib ,
        q E:\MSVCLib\build\Perl_5_16_0\perl-5.16.0\ext\re ,
        q E:\MSVCLib\build\Perl_5_16_0\perl-5.16.0\dist\Term-ReadLine\lib ,
        q E:\MSVCLib\build\Perl_5_16_0\perl-5.16.0\cpan\Text-ParseWords\lib ,
        q E:\MSVCLib\build\Perl_5_16_0\perl-5.16.0\lib ,
        q . );
