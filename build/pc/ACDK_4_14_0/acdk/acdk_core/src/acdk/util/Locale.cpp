// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
/*
   This file contains a C++ port of GNU Classpath project (http://www.gnu.org/software/classpath/classpath.html)
   with following copyright statement:

 * java.util.Locale: part of the Java Class Libraries project.
 * Copyright (C) 1998 Jochen Hoenicke
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA  02111-1307, USA.
*/


// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License (LGPL).
// 
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the 
// License ACDK-FreeLicense document enclosed in the distribution
// for more for more details.

// This file is part of the Artefaktur Component Development Kit:
//                         ACDK
// 
// Please refer to
// - http://www.acdk.de
// - http://www.artefaktur.com
// - http://acdk.sourceforge.net
// for more information.
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Locale.cpp,v 1.25 2005/04/18 14:28:03 kommer Exp $


#include <acdk.h>
#include "Locale.h"
#include "StringTokenizer.h"
#include "ResourceBundle.h"

#include <acdk/lang/System.h>
#include <acdk/lang/StringBuffer.h>
#include <acdk/lang/ObjectArrayImpl.h>
#include <acdk/io/File.h>
#include <acdk/io/GlobFilenameFilter.h>
#include "MissingResourceException.h"
#include <acdk/lang/UnsupportedOperationException.h>

#include <locale.h>

namespace acdk {
namespace util {


RLocale Locale::_defaultLocale;
RLocale Locale::ENGLISH = Nil;
RLocale Locale::FRENCH;
RLocale Locale::GERMAN;
RLocale Locale::ITALIAN;
RLocale Locale::JAPANESE;
RLocale Locale::KOREAN;
RLocale Locale::CHINESE;
RLocale Locale::SIMPLIFIED_CHINESE;
RLocale Locale::TRADITIONAL_CHINESE;
RLocale Locale::FRANCE;
RLocale Locale::GERMANY;
RLocale Locale::ITALY;
RLocale Locale::JAPAN;
RLocale Locale::KOREA;
RLocale Locale::CHINA;
RLocale Locale::PRC;
RLocale Locale::TAIWAN;
RLocale Locale::UK;
RLocale Locale::US;
RLocale Locale::CANADA;
RLocale Locale::CANADA_FRENCH;

enum 
{
  territories_table_size = 240,
  languages_table_size = 188
};

char* territories_table[territories_table_size][2]  = {
{ "AD", "Andorra" },
{ "AE", "United Arab Emirates" },
{ "AF", "Afghanistan" },
{ "AG", "Antigua and Barbuda" },
{ "AI", "Anguilla" },
{ "AL", "Albania" },
{ "AM", "Armenia" },
{ "AN", "Netherlands Antilles" },
{ "AO", "Angola" },
{ "AQ", "Antarctica" },
{ "AR", "Argentina" },
{ "AS", "American Samoa" },
{ "AT", "Austria" },
{ "AU", "Australia" },
{ "AW", "Aruba" },
{ "AX", "\\u00C5land Islands" },
{ "AZ", "Azerbaijan" },
{ "BA", "Bosnia and Herzegovina" },
{ "BB", "Barbados" },
{ "BD", "Bangladesh" },
{ "BE", "Belgium" },
{ "BF", "Burkina Faso" },
{ "BG", "Bulgaria" },
{ "BH", "Bahrain" },
{ "BI", "Burundi" },
{ "BJ", "Benin" },
{ "BM", "Bermuda" },
{ "BN", "Brunei" },
{ "BO", "Bolivia" },
{ "BR", "Brazil" },
{ "BS", "Bahamas" },
{ "BT", "Bhutan" },
{ "BV", "Bouvet Island" },
{ "BW", "Botswana" },
{ "BY", "Belarus" },
{ "BZ", "Belize" },
{ "CA", "Canada" },
{ "CC", "Cocos Islands" },
{ "CD", "The Democratic Republic Of Congo" },
{ "CF", "Central African Republic" },
{ "CG", "Congo" },
{ "CH", "Switzerland" },
{ "CI", "C\\u00F4te d'Ivoire" },
{ "CK", "Cook Islands" },
{ "CL", "Chile" },
{ "CM", "Cameroon" },
{ "CN", "China" },
{ "CO", "Colombia" },
{ "CR", "Costa Rica" },
{ "CS", "Serbia and Montenegro" },
{ "CU", "Cuba" },
{ "CV", "Cape Verde" },
{ "CX", "Christmas Island" },
{ "CY", "Cyprus" },
{ "CZ", "Czech Republic" },
{ "DE", "Germany" },
{ "DJ", "Djibouti" },
{ "DK", "Denmark" },
{ "DM", "Dominica" },
{ "DO", "Dominican Republic" },
{ "DZ", "Algeria" },
{ "EC", "Ecuador" },
{ "EE", "Estonia" },
{ "EG", "Egypt" },
{ "EH", "Western Sahara" },
{ "ER", "Eritrea" },
{ "ES", "Spain" },
{ "ET", "Ethiopia" },
{ "FI", "Finland" },
{ "FJ", "Fiji" },
{ "FK", "Falkland Islands" },
{ "FM", "Micronesia" },
{ "FO", "Faroe Islands" },
{ "FR", "France" },
{ "GA", "Gabon" },
{ "GB", "United Kingdom" },
{ "GD", "Grenada" },
{ "GE", "Georgia" },
{ "GF", "French Guiana" },
{ "GH", "Ghana" },
{ "GI", "Gibraltar" },
{ "GL", "Greenland" },
{ "GM", "Gambia" },
{ "GN", "Guinea" },
{ "GP", "Guadeloupe" },
{ "GQ", "Equatorial Guinea" },
{ "GR", "Greece" },
{ "GS", "South Georgia And The South Sandwich Islands" },
{ "GT", "Guatemala" },
{ "GU", "Guam" },
{ "GW", "Guinea-Bissau" },
{ "GY", "Guyana" },
{ "HK", "Hong Kong" },
{ "HM", "Heard Island And McDonald Islands" },
{ "HN", "Honduras" },
{ "HR", "Croatia" },
{ "HT", "Haiti" },
{ "HU", "Hungary" },
{ "ID", "Indonesia" },
{ "IE", "Ireland" },
{ "IL", "Israel" },
{ "IN", "India" },
{ "IO", "British Indian Ocean Territory" },
{ "IQ", "Iraq" },
{ "IR", "Iran" },
{ "IS", "Iceland" },
{ "IT", "Italy" },
{ "JM", "Jamaica" },
{ "JO", "Jordan" },
{ "JP", "Japan" },
{ "KE", "Kenya" },
{ "KG", "Kyrgyzstan" },
{ "KH", "Cambodia" },
{ "KI", "Kiribati" },
{ "KM", "Comoros" },
{ "KN", "Saint Kitts And Nevis" },
{ "KP", "North Korea" },
{ "KR", "South Korea" },
{ "KW", "Kuwait" },
{ "KY", "Cayman Islands" },
{ "KZ", "Kazakhstan" },
{ "LA", "Laos" },
{ "LB", "Lebanon" },
{ "LC", "Saint Lucia" },
{ "LI", "Liechtenstein" },
{ "LK", "Sri Lanka" },
{ "LR", "Liberia" },
{ "LS", "Lesotho" },
{ "LT", "Lithuania" },
{ "LU", "Luxembourg" },
{ "LV", "Latvia" },
{ "LY", "Libya" },
{ "MA", "Morocco" },
{ "MC", "Monaco" },
{ "MD", "Moldova" },
{ "MG", "Madagascar" },
{ "MH", "Marshall Islands" },
{ "MK", "Macedonia" },
{ "ML", "Mali" },
{ "MM", "Myanmar" },
{ "MN", "Mongolia" },
{ "MO", "Macao" },
{ "MP", "Northern Mariana Islands" },
{ "MQ", "Martinique" },
{ "MR", "Mauritania" },
{ "MS", "Montserrat" },
{ "MT", "Malta" },
{ "MU", "Mauritius" },
{ "MV", "Maldives" },
{ "MW", "Malawi" },
{ "MX", "Mexico" },
{ "MY", "Malaysia" },
{ "MZ", "Mozambique" },
{ "NA", "Namibia" },
{ "NC", "New Caledonia" },
{ "NE", "Niger" },
{ "NF", "Norfolk Island" },
{ "NG", "Nigeria" },
{ "NI", "Nicaragua" },
{ "NL", "Netherlands" },
{ "NO", "Norway" },
{ "NP", "Nepal" },
{ "NR", "Nauru" },
{ "NU", "Niue" },
{ "NZ", "New Zealand" },
{ "OM", "Oman" },
{ "PA", "Panama" },
{ "PE", "Peru" },
{ "PF", "French Polynesia" },
{ "PG", "Papua New Guinea" },
{ "PH", "Philippines" },
{ "PK", "Pakistan" },
{ "PL", "Poland" },
{ "PM", "Saint Pierre And Miquelon" },
{ "PN", "Pitcairn" },
{ "PR", "Puerto Rico" },
{ "PS", "Palestine" },
{ "PT", "Portugal" },
{ "PW", "Palau" },
{ "PY", "Paraguay" },
{ "QA", "Qatar" },
{ "RE", "Reunion" },
{ "RO", "Romania" },
{ "RU", "Russia" },
{ "RW", "Rwanda" },
{ "SA", "Saudi Arabia" },
{ "SB", "Solomon Islands" },
{ "SC", "Seychelles" },
{ "SD", "Sudan" },
{ "SE", "Sweden" },
{ "SG", "Singapore" },
{ "SH", "Saint Helena" },
{ "SI", "Slovenia" },
{ "SJ", "Svalbard And Jan Mayen" },
{ "SK", "Slovakia" },
{ "SL", "Sierra Leone" },
{ "SM", "San Marino" },
{ "SN", "Senegal" },
{ "SO", "Somalia" },
{ "SR", "Suriname" },
{ "ST", "Sao Tome And Principe" },
{ "SV", "El Salvador" },
{ "SY", "Syria" },
{ "SZ", "Swaziland" },
{ "TC", "Turks And Caicos Islands" },
{ "TD", "Chad" },
{ "TF", "French Southern Territories" },
{ "TG", "Togo" },
{ "TH", "Thailand" },
{ "TJ", "Tajikistan" },
{ "TK", "Tokelau" },
{ "TL", "Timor-Leste" },
{ "TM", "Turkmenistan" },
{ "TN", "Tunisia" },
{ "TO", "Tonga" },
{ "TR", "Turkey" },
{ "TT", "Trinidad and Tobago" },
{ "TV", "Tuvalu" },
{ "TW", "Taiwan" },
{ "TZ", "Tanzania" },
{ "UA", "Ukraine" },
{ "UG", "Uganda" },
{ "UM", "United States Minor Outlying Islands" },
{ "US", "United States" },
{ "UY", "Uruguay" },
{ "UZ", "Uzbekistan" },
{ "VA", "Vatican" },
{ "VC", "Saint Vincent And The Grenadines" },
{ "VE", "Venezuela" },
{ "VG", "British Virgin Islands" },
{ "VI", "U.S. Virgin Islands" },
{ "VN", "Vietnam" },
{ "VU", "Vanuatu" },
{ "WF", "Wallis And Futuna" },
{ "WS", "Samoa" },
{ "YE", "Yemen" },
{ "YT", "Mayotte" },
{ "ZA", "South Africa" },
{ "ZM", "Zambia" },
{ "ZW", "Zimbabwe" },
};
char* languages_table[languages_table_size][2] = {
{ "aa", "Afar" },
{ "ab", "Abkhazian" },
{ "ae", "Avestan" },
{ "af", "Afrikaans" },
{ "ak", "Akan" },
{ "am", "Amharic" },
{ "an", "Aragonese" },
{ "ar", "Arabic" },
{ "as", "Assamese" },
{ "av", "Avaric" },
{ "ay", "Aymara" },
{ "az", "Azerbaijani" },
{ "ba", "Bashkir" },
{ "be", "Belarusian" },
{ "bg", "Bulgarian" },
{ "bh", "Bihari" },
{ "bi", "Bislama" },
{ "bm", "Bambara" },
{ "bn", "Bengali" },
{ "bo", "Tibetan" },
{ "br", "Breton" },
{ "bs", "Bosnian" },
{ "ca", "Catalan" },
{ "ce", "Chechen" },
{ "ch", "Chamorro" },
{ "co", "Corsican" },
{ "cr", "Cree" },
{ "cs", "Czech" },
{ "cu", "Church Slavic" },
{ "cv", "Chuvash" },
{ "cy", "Welsh" },
{ "da", "Danish" },
{ "de", "German" },
{ "dv", "Divehi" },
{ "dz", "Dzongkha" },
{ "ee", "Ewe" },
{ "el", "Greek" },
{ "en", "English" },
{ "eo", "Esperanto" },
{ "es", "Spanish" },
{ "et", "Estonian" },
{ "eu", "Basque" },
{ "fa", "Persian" },
{ "ff", "Fulah" },
{ "fi", "Finnish" },
{ "fj", "Fijian" },
{ "fo", "Faroese" },
{ "fr", "French" },
{ "fy", "Frisian" },
{ "ga", "Irish" },
{ "gd", "Scottish Gaelic" },
{ "gl", "Gallegan" },
{ "gn", "Guarani" },
{ "gu", "Gujarati" },
{ "gv", "Manx" },
{ "ha", "Hausa" },
{ "he", "Hebrew" },
{ "hi", "Hindi" },
{ "ho", "Hiri Motu" },
{ "hr", "Croatian" },
{ "ht", "Haitian" },
{ "hu", "Hungarian" },
{ "hy", "Armenian" },
{ "hz", "Herero" },
{ "ia", "Interlingua" },
{ "id", "Indonesian" },
{ "ie", "Interlingue" },
{ "ig", "Igbo" },
{ "ii", "Sichuan Yi" },
{ "ik", "Inupiaq" },
{ "in", "Indonesian" },
{ "io", "Ido" },
{ "is", "Icelandic" },
{ "it", "Italian" },
{ "iu", "Inuktitut" },
{ "iw", "Hebrew" },
{ "ja", "Japanese" },
{ "ji", "Yiddish" },
{ "jv", "Javanese" },
{ "ka", "Georgian" },
{ "kg", "Kongo" },
{ "ki", "Kikuyu" },
{ "kj", "Kwanyama" },
{ "kk", "Kazakh" },
{ "kl", "Greenlandic" },
{ "km", "Khmer" },
{ "kn", "Kannada" },
{ "ko", "Korean" },
{ "kr", "Kanuri" },
{ "ks", "Kashmiri" },
{ "ku", "Kurdish" },
{ "kv", "Komi" },
{ "kw", "Cornish" },
{ "ky", "Kirghiz" },
{ "la", "Latin" },
{ "lb", "Luxembourgish" },
{ "lg", "Ganda" },
{ "li", "Limburgish" },
{ "ln", "Lingala" },
{ "lo", "Lao" },
{ "lt", "Lithuanian" },
{ "lu", "Luba-Katanga" },
{ "lv", "Latvian" },
{ "mg", "Malagasy" },
{ "mh", "Marshallese" },
{ "mi", "Maori" },
{ "mk", "Macedonian" },
{ "ml", "Malayalam" },
{ "mn", "Mongolian" },
{ "mo", "Moldavian" },
{ "mr", "Marathi" },
{ "ms", "Malay" },
{ "mt", "Maltese" },
{ "my", "Burmese" },
{ "na", "Nauru" },
{ "nb", "Norwegian Bokm\\u00E5l" },
{ "nd", "North Ndebele" },
{ "ne", "Nepali" },
{ "ng", "Ndonga" },
{ "nl", "Dutch" },
{ "nn", "Norwegian Nynorsk" },
{ "no", "Norwegian" },
{ "nr", "South Ndebele" },
{ "nv", "Navajo" },
{ "ny", "Nyanja" },
{ "oc", "Occitan" },
{ "oj", "Ojibwa" },
{ "om", "Oromo" },
{ "or", "Oriya" },
{ "os", "Ossetian" },
{ "pa", "Panjabi" },
{ "pi", "Pali" },
{ "pl", "Polish" },
{ "ps", "Pushto" },
{ "pt", "Portuguese" },
{ "qu", "Quechua" },
{ "rm", "Raeto-Romance" },
{ "rn", "Rundi" },
{ "ro", "Romanian" },
{ "ru", "Russian" },
{ "rw", "Kinyarwanda" },
{ "sa", "Sanskrit" },
{ "sc", "Sardinian" },
{ "sd", "Sindhi" },
{ "se", "Northern Sami" },
{ "sg", "Sango" },
{ "si", "Sinhalese" },
{ "sk", "Slovak" },
{ "sl", "Slovenian" },
{ "sm", "Samoan" },
{ "sn", "Shona" },
{ "so", "Somali" },
{ "sq", "Albanian" },
{ "sr", "Serbian" },
{ "ss", "Swati" },
{ "st", "Southern Sotho" },
{ "su", "Sundanese" },
{ "sv", "Swedish" },
{ "sw", "Swahili" },
{ "ta", "Tamil" },
{ "te", "Telugu" },
{ "tg", "Tajik" },
{ "th", "Thai" },
{ "ti", "Tigrinya" },
{ "tk", "Turkmen" },
{ "tl", "Tagalog" },
{ "tn", "Tswana" },
{ "to", "Tonga" },
{ "tr", "Turkish" },
{ "ts", "Tsonga" },
{ "tt", "Tatar" },
{ "tw", "Twi" },
{ "ty", "Tahitian" },
{ "ug", "Uighur" },
{ "uk", "Ukrainian" },
{ "ur", "Urdu" },
{ "uz", "Uzbek" },
{ "ve", "Venda" },
{ "vi", "Vietnamese" },
{ "vo", "Volap\\u00FCk" },
{ "wa", "Walloon" },
{ "wo", "Wolof" },
{ "xh", "Xhosa" },
{ "yi", "Yiddish" },
{ "yo", "Yoruba" },
{ "za", "Zhuang" },
{ "zh", "Chinese" },
{ "zu", "Zulu" },
};

 

//////////////////////////////////////////////////////////////////////////////
//   Contructor
///////////////////////////////////////////////////////////////////////////



Locale::Locale() 
: _language("")
, _country("")
, _variant("")
, _hashCode(0)
{
}

void
Locale::_calcHashCode()
{
  _hashCode = (String::defaultString(_language)->hashCode() * 31 + 
    String::defaultString(_country)->hashCode()) * 31 + String::defaultString(_variant)->hashCode();
}

Locale::Locale(IN(RString) language, IN(RString) country) 
: _language(language)
, _country(country)
, _variant("")
, _hashCode(0)   
{
}

Locale::Locale(IN(RString) language, IN(RString) country, IN(RString) variant) 
: _language(language)
, _country(country)
, _variant(variant)
, _hashCode(0)
{
  _calcHashCode();
}



//////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////


//static
OUT(RLocale)
Locale::getDefault()
{
  static RLocale _defaultLocale;

  if (_defaultLocale == Nil)
    _defaultLocale =  new Locale(System::getProperty("user.language", "en"),
                            System::getProperty("user.region", "US"),
                            System::getProperty("user.variant", ""));
  
  return _defaultLocale;
}

//static
RLocale 
Locale::getENGLISH()
{
  if (ENGLISH == Nil) {
    ENGLISH =  new Locale("en", "US");
    System::registerStaticReference(ENGLISH);
  }
  return ENGLISH;
}

//static
RLocale 
Locale::getFRENCH()
{
  RLocale loc =  new Locale("fr", "FR");
  return loc;
}

//static
RLocale 
Locale::getGERMAN()
{
  RLocale loc =  new Locale("de", "DE");
  return loc;
}

//static
RLocale 
Locale::getITALIAN()
{
  RLocale loc =  new Locale("it", "IT");
  return loc;
}

//static
RLocale 
Locale::getJAPANESE()
{
  RLocale loc =  new Locale("ja", "");
  return loc;
}

//static
RLocale 
Locale::getKOREAN()
{
  RLocale loc =  new Locale("ko", "");
  return loc;
}

//static
RLocale 
Locale::getCHINESE()
{
  RLocale loc =  new Locale("zh", "");
  return loc;
}

//static
RLocale 
Locale::getSIMPLIFIED_CHINESE()
{
  RLocale loc =  new Locale("zh", "CN");
  return loc;
}

//static
RLocale 
Locale::getTRADITIONAL_CHINESE()
{
  RLocale loc =  new Locale("zh", "TW");
  return loc;
}

//static
RLocale 
Locale::getFRANCE()
{
  RLocale loc =  new Locale("fr", "FR");
  return loc;
}

//static
RLocale 
Locale::getGERMANY()
{
  RLocale loc =  new Locale("de", "DE");
  return loc;
}

//static
RLocale 
Locale::getITALY()
{
  RLocale loc =  new Locale("it", "IT");
  return loc;
}

//static
RLocale 
Locale::getJAPAN()
{
  RLocale loc =  new Locale("ja", "JP");
  return loc;
}

//static
RLocale 
Locale::getKOREA()
{
  RLocale loc =  new Locale("ko", "KR");
  return loc;
}

//static
RLocale 
Locale::getCHINA()
{
  return SIMPLIFIED_CHINESE;
}

//static
RLocale 
Locale::getPRC()
{
  return CHINA;
}

//static
RLocale 
Locale::getTAIWAN()
{
  return TRADITIONAL_CHINESE;
}

//static
RLocale 
Locale::getUK()
{
  RLocale loc =  new Locale("en", "GB");
  return loc;
}

//static
RLocale 
Locale::getUS()
{
  RLocale loc =  new Locale("en", "US");
  return loc;
}

//static
RLocale 
Locale::getCANADA()
{
  RLocale loc =  new Locale("en", "CA");
  return loc;
}

//static
RLocale 
Locale::getCANADA_FRENCH()
{
  RLocale loc =  new Locale("fr", "CA");
  return loc;
}

//static
RLocaleArray
Locale::getAvailableLocals()
{
  using namespace acdk::io;
  RString path = System::getAcdkToolsHome() + File::separator() + "cfg" + File::separator() + "acdk" + File::separator() + "locale";
  File dir(path);
  RStringArray sa = dir.list(new GlobFilenameFilter("LocaleInfo_*.properties"));
  RLocaleArray availableLocales = new LocaleArray(0);
  for (int i = 0; i < sa->length(); ++i)
  {
    RString t = sa[i]->substr(strlen("LocaleInfo_"));
    t = t->substr(0, t->length() - strlen(".properties"));

    RStringArray ltk = StringTokenizer(t, "_").allToken();
    if (ltk->length() == 0)
      continue;
    RString lang = ltk[0];
    RString country = "";
    RString variant = "";
    if (ltk->length() > 1)
    {
      country = ltk[1];
      if (ltk->length() > 2)
        variant = ltk[2];
    }
    availableLocales->append(new Locale(lang, country, variant));
  }
  return availableLocales;
}


RString
Locale::toString()
{
  StringBuffer sb(_language);
  RString underscore = String::emptyString();
  if (_language->length() != 0)
    underscore = "_";
  if (_country->length() != 0) 
  {
    sb << underscore << _country;
    underscore = "_";
  }

  if (_variant->length() != 0) 
    sb << underscore << _variant;
  return sb.toString();
}

bool
Locale::_equals(IN(RLocale) l)
{
  if (l == Nil)
    return false;
  if (_language->equals(l->getLanguage()) == false)
    return false;
  
  if (_country->equals(l->getCountry()) == false)
    return false;

  if (_variant->equals(l->getVariant()) == false)
    return false;
  return true;
}


//static
RStringArray
Locale::getISOCountries()
{
  RStringArray reVal = new StringArray(2);
  reVal[0] = RString("DE");
  reVal[1] = RString("UD");
  return reVal;
}

//static
RStringArray
Locale::getISOLanguages()
{
  RStringArray reVal = new StringArray(2);
  reVal[0] = RString("de");
  reVal[1] = RString("en");
  return reVal;  
}

RString
Locale::getISO3Languages()
{
  RString s = "aa,ab,af,am,ar,as,ay,az,ba,be,bg,bh,bi,bn,bo,br,ca,co,cs,cy," 
    "da,de,dz,el,en,eo,es,et,eu,fa,fi,fj,fo,fr,fy,ga,gd,gl,gn,gu,"
    "gv,ha,hi,hr,hu,hy,ia,ie,ik,id,is,it,iu,he,ja,yi,jw,ka,kk,kl,"
    "km,kn,ko,ks,ku,kw,ky,la,lb,ln,lo,lt,lv,mg,mi,mk,ml,mn,mo,mr,"
    "ms,mt,my,na,ne,nl,no,oc,om,or,pa,pl,ps,pt,qu,rm,rn,ro,ru,rw,"
    "sa,sd,se,sg,sh,si,sk,sl,sm,sn,so,sq,sr,ss,st,su,sv,sw,ta,te,"
    "tg,th,ti,tk,tl,tn,to,tr,ts,tt,tw,ug,uk,ur,uz,vi,vo,wo,xh,yo,"
    "za,zh,zu,";

  int index = s->indexOf(_language+",");
  if (index == -1 || _language->length() !=  2)
    THROW1(MissingResourceException,"Can't find ISO3 language for " 
           + _language + "acdk::util::Locale");
  RString result = 
     "aarabkaframharaasmaymazebakbelbulbihbisbenbodbrecatcoscescym"
     "dandeudzoellengepospaesteusfasfinfijfaofrafrygaigdhglggrnguj"
     "maxhauhinhrvhunhyeinaileipkindislitaikuhebjpnyidjawkatkazkal"
     "khmkankorkaskurcorkirlatltzlinlaolitlavmlgmrimkdmalmonmolmar"
     "msamltmyanaunepnldnorociormoripanpolpusporquerohrunronruskin"
     "sansndsmisagsrpsinslkslvsmosnasomsqisrpsswsotsunsweswatamtel"
     "tgkthatirtuktgltsntonturtsotattwiuigukrurduzbvievolwolxhoyor"
     "zhazhozul";
  return result->substring(index,index+3);
}

RString
Locale::getISO3Country()
{
  RString s = 
      "AF,AL,DZ,AS,AD,AO,AI,AQ,AG,AR,AM,AW,AU,AT,AZ,BS,BH,BD,BB,BY,BE,"
      "BZ,BJ,BM,BT,BO,BA,BW,BV,BR,IO,BN,BG,BF,BI,KH,CM,CA,CV,KY,CF,TD,"
      "CL,CN,CX,CC,CO,KM,CG,CD,CK,CR,CI,HR,CU,CY,CZ,DK,DJ,DM,DO,TP,EC,"
      "EG,SV,GQ,ER,EE,ET,FK,FO,FJ,FI,FR,FX,GF,PF,TF,GA,GM,GE,DE,GH,GI,"
      "GR,GL,GD,GP,GU,GT,GN,GW,GY,HT,HM,VA,HN,HK,HU,IS,IN,ID,IR,IQ,IE,"
      "IL,IT,JM,JP,JO,KZ,KE,KI,KP,KR,KW,KG,LA,LV,LB,LS,LR,LY,LI,LT,LU,"
      "MO,MK,MG,MW,MY,MV,ML,MT,MH,MQ,MR,MU,YT,MX,FM,MD,MC,MN,MS,MA,MZ,"
      "MM,NA,NR,NP,NL,AN,NC,NZ,NI,NE,NG,NU,NF,MP,NO,OM,PK,PW,PA,PG,PY,"
      "PE,PH,PN,PL,PT,PR,QA,RE,RO,RU,RW,KN,LC,VC,WS,SM,ST,SA,SN,SC,SL,"
      "SG,SK,SI,SB,SO,ZA,GS,ES,LK,SH,PM,SD,SR,SJ,SZ,SE,CH,SY,TW,TJ,TZ,"
      "TH,TG,TK,TO,TT,TN,TR,TM,TC,TV,UG,UA,AE,GB,US,UM,UY,UZ,VU,VE,VN,"
      "VG,VI,WF,EH,YE,YU,ZM,ZW,";
  
  int index = s->indexOf(_country+",");
  if (index == -1 || _language->length() !=  2)
    THROW1(MissingResourceException,"Can't find ISO3 country for " 
           + _country + "acdk::util::Locale");
  RString result = 
      "AFGALBDZAASMANDAGOAIAATAATGARGARMABWAUSAUTAZEBHSBHRBGDBRBBLRBEL"
      "BLZBENBMUBTNBOLBIHBWABVTBRAIOTBRNBGRBFABDIKHMCMRCANCPVCYMCAFTCD"
      "CHLCHNCXRCCKCOLCOMCOGCODCOKCRICIVHRVCUBCYPCZEDNKDJIDMADOMTMPECU"
      "EGYSLVGNQERIESTETHFLKFROFJIFINFRAFXXGUFPYFATFGABGMBGEODEUGHAGIB"
      "GRCGRLGRDGLPGUMGTMGINGNBGUYHTIHMDVATHNDHKGHUNISLINDIDNIRNIRQIRL"
      "ISRITAJAMJPNJORKAZKENKIRPRKKORKWTKGZLAOLVALBNLSOLBRLBYLIELTULUX"
      "MACMKDMDGMWIMYSMDVMLIMLTMHLMTQMRTMUSMYTMEXFSMMDAMCOMNGMSRMARMOZ"
      "MMRNAMNRUNPLNLDANTNCLNZLNICNERNGANIUNFKMNPNOROMNPAKPLWPANPNGPRY"
      "PERPHLPCNPOLPRTPRIQATREUROMRUSRWAKNALCAVCTWSMSMRSTPSAUSENSYCSLE"
      "SGPSVKSVNSLBSOMZAFSGSESPLKASHNSPMSDNSURSJMSWZSWECHESYRTWNTJKTZA"
      "THATGOTKLTONTTOTUNTURTKMTCATUVUGAUKRAREGBRUSAUMIURYUZBVUTVENVNM"
      "VGBVIRWLFESHYEMYUGZMBZWE";
  return result->substring(index,index+3);
}

RString
Locale::getDisplayLanguage(IN(RLocale) locale)
{
  RResourceBundle resb = ResourceBundle::getBundle("acdk/locale/LocaleInfo", locale);
  return resb->getString("languages." + getLanguage());
}

RString
Locale::getDisplayCountry(IN(RLocale) locale)
{
  RResourceBundle resb = ResourceBundle::getBundle("acdk/locale/LocaleInfo", locale);
  return resb->getString("territories." + getCountry());
}

RString
Locale::getDisplayVariant(IN(RLocale) locale)
{
  //THROW0(UnsupportedOperationException);
  return String::emptyString();
}

RString
Locale::getDisplayName(IN(RLocale) locale)
{
  RStringBuffer result = new StringBuffer();
  int count = 0;
  RString delimiters = new String(",(,");
  if (_language->length() != 0) 
  {
    result->append(delimiters->charAt(count++));
    result->append(getDisplayLanguage(locale));

  }
  if (_country->length() != 0) {
    result->append(delimiters->charAt(count++));
    result->append(getDisplayCountry(locale));
  }

  if (_variant->length() != 0) {
    result->append(delimiters->charAt(count++));
    result->append(getDisplayVariant(locale));
  }

  if (count > 1)
    result->append(")");
  return result->toString();
}

//virtual
RObject
Locale::clone(sys::Allocator* alc)
{
  
  return new Locale(_language, _country, _variant);
}

//static 
RString 
Locale::longLang2ISO(IN(RString) language)
{
  for (int i = 0; i < languages_table_size; ++i)
  {
      if (language->equals(String::decodeAscUnicode(languages_table[i][1])) == true)
      return languages_table[i][0];
  }
  return Nil;

}
//static 
RString 
Locale::longCountry2ISO(IN(RString) country)
{
  for (int i = 0; i < territories_table_size; ++i)
  {
    RString tc = String::decodeAscUnicode(territories_table[i][1]);
    if (country->equals(tc) == true)
      return territories_table[i][0];
  }
  return Nil;
}

//static 
RString 
Locale::isoLang2longLang(IN(RString) language)
{
  for (int i = 0; i < languages_table_size; ++i)
  {
      if (language->equals(String::decodeAscUnicode(languages_table[i][0])) == true)
      return languages_table[i][1];
  }
  return language;
}

//static 
RString 
Locale::isoCountry2longCountry(IN(RString) country)
{
  for (int i = 0; i < territories_table_size; ++i)
  {
    RString tc = String::decodeAscUnicode(territories_table[i][0]);
    if (country->equals(tc) == true)
      return territories_table[i][1];
  }
  return country;
}


RString 
Locale::convertLanguage(IN(RString) language)
{
  if (language->length() == 0)
    return language;
  int index = RString("iw,in,ji")->indexOf(language);
  if (index != -1)
    return RString("he,id,yi")->substring(index, index+2);
  return language;
}

//static 
RLocale 
Locale::getSystemLocale()
{
  RString language;
  RString country;
  RString encoding;
  getSystemLocaleValues(language, country, encoding);
  if (language == Nil)
    return getUS();
  return new Locale(language, country);
}


//static 
RString 
Locale::getSystemLocaleString()
{
  return SCS(setlocale(LC_ALL, ""));
}
//static 
void 
Locale::getSystemLocaleValues(OUT(RString) language, OUT(RString) country, OUT(RString) encoding)
{
  RString oslocale = getSystemLocaleString();
  int fidx = oslocale->indexOf('_');
  int sidx = oslocale->indexOf('.');
  if (fidx == -1)
    return;
  RString l = oslocale->substr(0, fidx);
  RString c;
  RString e;
  if (sidx != -1)
  {
    c = oslocale->substr(fidx + 1, sidx);
    e = oslocale->substr(sidx + 1);
  }
  else
  {
    c = oslocale->substr(fidx + 1);
  }
#if defined(_MSC_VER)
  l = acdk::util::Locale::longLang2ISO(l);
  c = acdk::util::Locale::longCountry2ISO(c);
#endif
  if (l != Nil)
    language = l;
  if (c != Nil)
    country = c;
  if (e != Nil)
    encoding = e->toLowerCase();
}



RString 
Locale::getSystemLanguage()
{
  RString language;
  RString country;
  RString encoding;
  getSystemLocaleValues(language, country, encoding);
  return language;
}

//static 
RString 
Locale::getSystemCountry()
{
  RString language;
  RString country;
  RString encoding;
  getSystemLocaleValues(language, country, encoding);
  return country;
}

//static 
RString 
Locale::getSystemEncoding()
{
  RString language;
  RString country;
  RString encoding;
  getSystemLocaleValues(language, country, encoding);
  return encoding;
}

//static 
void 
Locale::setSystemLocaleValues(IN(RString) language, IN(RString) country, IN(RString) encoding)
{
  // ### not implemented
}

//static 
void 
Locale::setSystemLocale(IN(RLocale) loc, SystemLocaleCategory cat)
{
  RString language = loc->getLanguage();
  RString country = loc->getCountry();
#if defined(_MSC_VER)
  language = isoLang2longLang(language);
  country = isoCountry2longCountry(country);
#endif
  RString identifier;
  if (language->length() > 0 && country->length() > 0)
    identifier = language + "_" + country;
  else if (language->length() > 0)
    identifier = language;
  else if (country->length() > 0)
    identifier = country;
  setSystemLocale(identifier, cat);
}

//static 
void 
Locale::setSystemLocale(IN(RString) localeIdent, SystemLocaleCategory cat)
{
  setlocale(cat, localeIdent->convert(CCAscii)->c_str());
}

} // util
} // acdk
