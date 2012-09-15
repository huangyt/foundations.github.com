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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Locale.h,v 1.19 2005/04/08 10:53:20 kommer Exp $

#ifndef acdk_util_Locale_h
#define acdk_util_Locale_h

#include <acdk.h>
#include <acdk/lang/String.h>
#include <acdk/lang/Cloneable.h>
#include <acdk/io/Serializable.h>

#include <locale.h>

namespace acdk {
namespace util {

/**
  equivalent to std c lib locale categories
*/
enum SystemLocaleCategory
{
  SysLocaleAll = LC_ALL,
  SysLocaleCollate = LC_COLLATE,
  SysLocaleCtype = LC_CTYPE,
  SysLocaleMonetary = LC_MONETARY,
  SysLocaleNumberic = LC_NUMERIC,
  SysLocaleTime = LC_TIME 

};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, SystemLocaleCategory);

ACDK_DECL_CLASS(Locale);
/**
 * This Class represent a specific country an culture.
 * Classes which can be passed a Locale object tailor their information for a
 * given locale. For instance, currency number formating is handled differently
 * for the USA and France.
 * Locales are made up of a language code, a country code, and an optional set
 * of variant strings.

   This class and source comments contains a C++ port of GNU Classpath project 
   (http://www.gnu.org/software/classpath/classpath.html)
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
 *
 */
class ACDK_CORE_PUBLIC Locale
: extends acdk::lang::Object,
  implements acdk::lang::Cloneable,
  implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(Locale)
private:
  RString _language;
  RString _country;
  RString _variant;
  int _hashCode;
public:

  Locale();
  Locale(IN(RString) language, IN(RString) country);
  Locale(IN(RString) language, IN(RString) country, IN(RString) variant);
  /**
   * This method returns the languege code of this locale
   * 
   *
   * @return languege code portion of this locale, or an emty String if none
   * exists.
   *
   */
  RString getLanguage() { return _language; }

  /**
   * This method returns the country code of this locale.
   * 
   *
   * @return country code portion of this locale, or an empty String if
   * none exists.
   *
   */
  RString getCountry() { return _country; }

  /**
   * this method returns the variant code of this locale.
   * 
   */
  RString getVariant() { return _variant; }

  foreign virtual int hashCode() { return _hashCode; }

  /**
   * This method gets the string representation of the current locale. This
   * consists of the language, the country, and variant, separated by an 
   * underscore. If one of this three component is missing the underscore
   * will also diappear.
   * 
   * @return the string representation of this Locale.
   */
  foreign virtual RString toString() ;

  /**
   * Compares two locales.
   * @param obj the other locale.
   * @return true, if obj is a Locale with the same language, country, and
   * variant code as this locale, otherwise false.
   */
  virtual bool equals(IN(RObject) obj)
  {
    if (instanceof(obj, Locale) == false)
      return false;
    return equals(RLocale(obj));
  }
  virtual bool equals(IN(RLocale) other)
  {
    if (hashCode() != other->hashCode())
      return false;
    return _equals(other);
  }
  
 
  static OUT(RLocale) getDefault();
  static RLocale getENGLISH();
  static RLocale getFRENCH();
  static RLocale getGERMAN();
  static RLocale getITALIAN();
  static RLocale getJAPANESE();
  static RLocale getKOREAN();
  static RLocale getCHINESE();
  static RLocale getSIMPLIFIED_CHINESE();
  static RLocale getTRADITIONAL_CHINESE();
  static RLocale getFRANCE();
  static RLocale getGERMANY();
  static RLocale getITALY();
  static RLocale getJAPAN();
  static RLocale getKOREA();
  static RLocale getCHINA();
  static RLocale getPRC();
  static RLocale getTAIWAN();
  static RLocale getUK();
  static RLocale getUS();
  static RLocale getCANADA();
  static RLocale getCANADA_FRENCH();

  void setDefault(IN(RLocale) newLocale) { _defaultLocale = newLocale; }

  static RLocaleArray getAvailableLocals();
  static RStringArray getISOCountries();
  static RStringArray getISOLanguages();
  /**
    retrive the locale from the underlying opertion system
    If no system locale is available, it returns the en_US locale
  */
  static RLocale getSystemLocale();
  /**
    retrive the system locale as string
    Note: this string may not compatible with acdk::util::Locale identifier
    use getSystemLocale() to retrive an Locale compatible locale.
  */
  static RString getSystemLocaleString();
  /**
    sets the locale of the underlying operation system
  */
  static void setSystemLocale(IN(RLocale) loc, SystemLocaleCategory cat = SysLocaleAll);
  /**
    set the locale string for the system locale
    note this string is may be not compatible with acdk::util::Locale identifier
  */
  static void setSystemLocale(IN(RString) localeIdent, SystemLocaleCategory cat = SysLocaleAll);
  /** 
    ask underlying opertion or C lib system locales.
    This function try to translate the operation system locale indentifier to ACDK Locale identifier
    @return Nil in params if not known
  */
  static void getSystemLocaleValues(OUT(RString) language, OUT(RString) country, OUT(RString) encoding);
  /**
    set the underlying opertion or C lib system locales.
    This function try to translate ACDK Locale identifier to the operation system locale indentifier.
    @bug not implemented
  */
  static void setSystemLocaleValues(IN(RString) language, IN(RString) country, IN(RString) encoding);
  /** 
    ask underlying opertion system locales 
    @return Nil if not known
  */
  static RString getSystemLanguage();
  /** 
    ask underlying opertion system locales 
    @return Nil if not known
  */
  static RString getSystemCountry();
  /** 
    ask underlying opertion system locales 
    @return Nil if not known
  */
  static RString getSystemEncoding();
  /**
   * This method returns three-letter ISO language abbrevation of this locale. 
   * 
   * @exception MissingResourceException if three-letter code is not known. 
   */
  
  RString getISO3Languages();
  
  /**
   * This method return the three-letter ISO country abbrevation of locale.
   *
   * @exception MissingResourceException if three-letter code is not known. 
   */
  RString getISO3Country();

  /**
   * This method gets the language name suitable for display to the user, 
   * formated for default locale. This has the same effect as 
   * getDisplayLanguage(Locale->getDefault())
   *
   * @return the language name of this locale localized to the default locale.
   * if the localized is not foubd, the ISO code is returned 
   */
  RString getDisplayLanguage() 
  {
    return getDisplayLanguage(getDefault());
  }

  /**
   * Gets the language name suitable for display to the user, formatted for
   * a specified locale.
   * 
   * @param locale locale to use for formatting.
   *
   * @return the language name of this locale localized to the given lovale. If
   * the localized is not found, the ISO code is returned. 
   */
  RString getDisplayLanguage(IN(RLocale) locale);

  /**
   * This method gets the country name suitable for display to the user, 
   * formated for default locale. This has the same effect as 
   * getDisplayLanguage(Locale->getDefault())
   *
   * @return the country name of this locale localized to the default locale.
   * if the localized is not foubd, the ISO code is returned 
   */
  RString getDisplayCountry() 
  {
    return getDisplayCountry(getDefault());
  }

  /**
   * Gets the country name suitable for display to the user, formatted for
   * a specified locale.
   * 
   * @param locale locale to use for formatting.
   *
   * @return the country name of this locale localized to the given lovale. If
   * the localized is not found, the ISO code is returned. 
   */
  RString getDisplayCountry(IN(RLocale) locale);
  
  /**
   * Return the variant name of this locale localized tothe given locale. If 
   * the localized is not found, the variant code itself is returned. This 
   * has the same effect as 
   * getDisplayVariant(getDefault())
   */
  RString getDisplayVariant() 
  {
    return getDisplayVariant(getDefault());
  } 

  /**
   * Gets the country name suitable for display to the user, formatted for
   * a specified locale.
   * 
   * @param locale locale to use for formatting.
   *
   * @return the country name of this locale localized to the given lovale. If
   * the localized is not found, the ISO code is returned. 
   */
  RString getDisplayVariant(IN(RLocale) locale);

  /**
   * Gets all local compenent suitable for display to the user, formatted for
   * the default locale. For the language component, getDisplayLanguage is
   * called. For the country component, getDisplayCountry is called. For the 
   * variant set component, getDisplayVariant is called.
   *
   * The returned String will be one of the following forms:
   * language (country, variant)
   * language (country)
   * language (varian)
   * country (varian)
   * language
   * country
   * variant
   *
   * @return  RString version of this locale, suitable for display to the user.
   */
  RString getDisplayName() {
    return getDisplayName(getDefault());
  }

  /**
   * Gets all local components suitable for display to the user, formatted
   * for a specified locale.  For the language component, 
   * getDisplayLanguage(Locale) is called.  For the country component, 
   * getDisplayCountry(Locale) is called.  For the variant set component, 
   * getDisplayVariant(Locale) is called.
   *
   * The returned String will be one of the following forms:
   *
   * language (country, variant)
   * language (country)
   * language (variant)
   * country (variant)
   * language
   * country
   * variant
   *
   * @param locale locale to use for formatting
   *
   * @return String version of this locale, suitable for display to the
   * user.
   */
  RString getDisplayName(IN(RLocale) locale);
  
  /**
   * Does the same as Object::clone
   * 
   */
  foreign virtual RObject clone() { return clone(allocator()); }
  foreign virtual RObject clone(sys::Allocator* alc);
  /** 
    translate long language identifier to 2 letter iso language identifier 
    returns Nil if not found
  */
  static RString longLang2ISO(IN(RString) language);
  /** 
    translate long country identifier to 2 letter iso country identifier 
    returns Nil if not found
  */
  static RString longCountry2ISO(IN(RString) country);
  /**
    translate iso language identifier to long language identifier 
    return orginal value if not found
  */
  static RString isoLang2longLang(IN(RString) language);
  /**
    translate iso country identifier to long country identifier 
    return orginal value if not found
  */
  static RString isoCountry2longCountry(IN(RString) country);
protected:

private:
  RString convertLanguage(IN(RString) language);
  
  bool _equals(IN(RLocale) other);
  void _calcHashCode();
private:
  static RLocale _defaultLocale;
  /**
   * Locale which represents the English language.
   */
  static  RLocale ENGLISH;
  /**
   * Locale which represents the French language.
   */
  static RLocale FRENCH;
  /**
   * Locale which represents the German language.
   */
  static RLocale GERMAN;
  /**
   * Locale which represents the Italian language.
   */
  static RLocale ITALIAN;
  /**
   * Locale which represents the Japanese language.
   */
  static RLocale JAPANESE;
   
  /**
   * Locale which represents the Korean language.
   */
  static RLocale KOREAN;
  /**
   * Locale which represents the Chinese language.
   */
  static RLocale CHINESE;
  /**
   * Locale which represents the Chinese language as used in China.
   */
  static RLocale SIMPLIFIED_CHINESE;

  /**
   * Locale which represents the Chinese language as used in Taiwan.
   * Same as TAIWAN Locale.
   */
  static RLocale TRADITIONAL_CHINESE;  
  /**
   * Locale which represents France.
   */
  static RLocale FRANCE;
  /**
   * Locale which represents Germany.
   */
  static RLocale GERMANY;
  
  /**
   * Locale which represents Italy.
   */
  static RLocale ITALY;
  
  /**
   * Locale which represents Japan.
   */
  static RLocale JAPAN;

  /**
   * Locale which represents Korea.
   */
  static RLocale KOREA;
  /**
   * Locale which represents China.
   * Same as SIMPLIFIED_CHINESE Locale.
   */
  static RLocale CHINA; 
  /**
   * Locale which represents the People's Republic of China.
   * Same as CHINA Locale.
   */
  static RLocale PRC;
  /**
   * Locale which represents Taiwan.
   * Same as TRADITIONAL_CHINESE Locale.
   */
  static RLocale TAIWAN;
  /**
   * Locale which represents the United Kingdom.
   */
  static RLocale UK;
  /**
   * Locale which represents the United States.
   */
  static RLocale US;
  /**
   * Locale which represents the English speaking portion of Canada.
   */
  static RLocale CANADA;
  /**
   * Locale which represents the French speaking portion of Canada.
   */
  static RLocale CANADA_FRENCH;


};


} // Util
} //MM

#endif //acdk_util_Locale_h

