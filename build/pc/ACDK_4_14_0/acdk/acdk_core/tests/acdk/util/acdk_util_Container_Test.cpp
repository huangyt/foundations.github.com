// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/util/acdk_util_Container_Test.cpp,v 1.16 2005/02/05 10:45:09 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>

#include <acdk/io/MemReader.h>
#include <acdk/io/MemWriter.h>
#include <acdk/util/StringTokenizer.h>

#include <acdk/util/Arrays.h>
#include <acdk/util/ArrayList.h>
#include <acdk/util/LinkedList.h>

#include <acdk/util/TreeMap.h>
#include <acdk/util/HashSet.h>
#include <acdk/util/TreeSet.h>
#include <acdk/util/Collections.h>

#include <acdk/lang/sys/core_tick.h>

#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/locale/Encoding.h>
#include <acdk/io/BytePtrReader.h>

namespace tests {
namespace acdk {
namespace util {

  using namespace ::acdk::lang;
  using namespace ::acdk::io;
  using namespace ::acdk::util;

BEGIN_DECLARE_TEST( Container_Test )
  DECLARE_TEST( standardMap )
  DECLARE_TEST( arrayList )
END_DECLARE_TEST( Container_Test  )

BEGIN_DEFINE_TEST( Container_Test )
  ADD_TEST( Container_Test, standardMap ) 
  ADD_TEST( Container_Test, arrayList ) 

END_DEFINE_TEST( Container_Test )

namespace {
struct NamedPair
{
  const char* first;
  const char* second;
};

NamedPair philophers_namess[] =
{
  { "Diogenes", "" }, // begin of subset
  { "Husserl", "Edmund" }, // end of subset
  { "Kant", "Imanuel" },
  { "Lucacs", "George" },
  { "Hegel", "GWF" },
  { "Schopenhauer", "Arthur" },
  { "Sartre", "Paul" },
  { "Nietzsche", "Friedrich" },
  { "Kierkegaard", "Soeren" },
  { "Russell", "Bertrand" },
  { "Adorno", "TW" },
  { "Horkheimer", "Max" },
  { "Heidegger", "Martin" },
  { "Leibniz", "GWv" },
  { "Wittgenstein", "Ludwig" },
  { "Popper", "Karl" },
  { "Hume", "David" },
  { "Aristotle", "" },
  { "Epicurus", "" },
  { "Plato", "" },
  { "Socrates", "" },
  { "Descartes", "Rene" },
  { "Foucault", "Michel" },
  { "Pascal", "Blaise" },
  { "Rousseau", "Jean Jacques" },
  
  { 0, 0 }
};


NamedPair componist_namess[] = {
{ "Debussy, Achille-Claude", "Frankreich"}, // begin of subset
{ "Sciarrino, Salvatore", "Italien"},// end of subset
  { "440 hertz", "unknown"},{ "Abel, Carl Friedrich", "Deutschland"},{ "Adams, John Coolidge", "USA"},{ "Adorno, Theodor W.", "Deutschland"},{ "Aho, Kalevi", "Finnland"},{ "Akses, Necil K\\u00e2zim", "T\\u00fcrkei"},
{ "Alb\\u00e9niz, Isaac Manuel Francisco", "Spanien"},{ "Albert, Eugen d'", "England;Italien"},{ "Albinoni, Tomaso", "Italien"},{ "Alfv\\u00e9n, Hugo", "Schweden"},{ "Anonymus", "unknown"},{ "Antheil, George", "USA"},
{ "Apostel, Hans Erich", "Deutschland"},{ "Arnold, Sir Malcolm", "England"},{ "Atterberg, Kurt", "Schweden"},{ "Babadjanian, Arno", "Armenien"},{ "Bacewicz, Grazyna", "Polen"},{ "Bach, Carl Philip Emanuell", "Deutschland"},
{ "Bach, Johann Christian", "Deutschland"},{ "Bach, Johann Christoph Friedrich", "Deutschland"},{ "Bach, Johann Ernst", "Deutschland"},{ "Bach, Johann-Sebastian", "Deutschland"},{ "Balada, Leonardo", "USA;Spanien"},{ "Balakauskas, Osvaldas", "Litauen"},
{ "Balakirew, Milij Aleksejewitsch", "Russland"},{ "Barber, Samuel", "USA"},{ "Barbingant", "unknown"},{ "Bart\\u00f3k, B\\u00e9la Viktor Janos", "Ungarn"},{ "Bax, Sir Arnold Edward Trevor", "England"},{ "Beethoven, Ludwig van", "Deutschland"},
{ "Bellini, Vincenzo", "Italien"},{ "Bentzon, Niels Viggo", "D\\u00e4nemark"},{ "Berg, Alban", "Deutschland"},{ "Berlioz, Louis Hector", "Frankreich"},{ "Bernstein, Leonard", "USA"},{ "Bialas, G\\u00fcnter", "Deutschland"},
{ "Birtwistle, Harrison", "England"},{ "Bizet, Georges", "Frankreich"},{ "Blacher, Boris", "Deutschland"},{ "Bliss, Sir Arthur Edward Drummond", "England"},{ "Bloch Ernest", "Schweiz"},{ "Boccherini, Ridolfo Luigi", "Italien"},
{ "Bolcom, William", "USA"},{ "Borodin, Aleksandr Porfirjewitsch", "Russland"},{ "Borresen, Hakon", "Schweden"},{ "Boughton, Rutland", "England"},{ "Boulez, Pierre", "Frankreich"},{ "Bowles, Paul", "USA"},
{ "Boyce, William", "England"},{ "Brahms, Johannes", "Deutschland"},{ "Brian, Havergal", "England"},{ "Bridge, Frank", "England"},{ "Britten, Edward Benjamin", "England"},{ "Brouwer, Leo", "Kuba"},
{ "Browne, William Denis", "England"},{ "Bruch, Max", "Deutschland"},{ "Bruckner, Anton", "\\u00d6sterreich"},{ "Bryars, Gavin", "England"},{ "Burt, Francis", "England"},{ "Busoni, Ferruccio Benvenuto", "Italien"},
{ "Butterworth, George", "England"},{ "Buxtehude, Dietrich", "Schweden"},{ "Cage, John", "USA"},{ "Caldara, Antonio", "Italien"},{ "Camilleri, Charles", "Malta"},{ "Cannabich, Christian", "Deutschland"},
{ "Carpenter, John Alden", "USA"},{ "Casella, Alfredo", "Italien"},{ "Castelnuovo-Tedesco, Mario", "Italien"},{ "Cavalli, Pier Francesco", "Italien"},{ "Charpentier, Gustave", "Frankreich"},{ "Charpentier, Marc-Antoine", "Frankreich"},
{ "Chatschaturjan, Aram Iljitsch", "Armenien;Russland"},{ "Cherubini, Luigi Carlo Zanobi Salvadore Maria", "Italien"},{ "Chesky, David", "unknown"},{ "Chopin, Fr\\u00e9d\\u00e9ric Fran\\u00e7ois", "Frankreich"},{ "Cl\\u00e9ment, Jacques", "Frankreich"},{ "Coates, Gloria", "USA"},
{ "Combert, Nicholas", "unknown"},{ "Copland, Aaron", "USA"},{ "Corelli, Arcangelo", "Italien"},{ "Cornysh, William Junior", "England"},{ "Coste, Napol\\u00e9on", "Frankreich"},{ "Creston, Paul", "USA"},
{ "Delibes, Cl\\u00e9ment Philibert L\\u00e9o", "Frankreich"},{ "Delius, Frederick", "England"},{ "Dessau, Paul", "Deutschland"},{ "Devreese, Fr\\u00e9d\\u00e9ric", "Belgien"},{ "Devreese, Godfried", "Belgien"},
{ "Ditters von Dittersdorf, Karl", "\\u00d6sterreich"},{ "Dohn\\u00e1nyi, Ern\\u00f6 (Ernst) von", "Ungarn"},{ "Donizetti, Domenico Gaetano Maria", "Italien"},{ "Dor\\u00e1ti, Antal", "Ungarn"},{ "Dukas, Paul Abraham", "Frankreich"},{ "Dunhill, Thomas", "England"},
{ "Durufl\\u00e9, Maurice", "Frankreich"},{ "Dutilleux, Henri", "Frankreich"},{ "Dvor\\u00e1k, Anton\\u00edn", "Tschechien"},{ "Eberl, Anton", "\\u00d6sterreich"},{ "Eespere, Ren\\u00e9", "Estland"},{ "Egk, Werner", "Deutschland"},
{ "Eisler, Hanns", "Deutschland"},{ "Elgar, Sir Edward William", "England"},{ "Eller, Heino", "Estland"},{ "Enescu, George", "Rum\\u00e4nien"},{ "Englund, Einar", "Finnland"},{ "Eybler, Joseph Leopold", "\\u00d6sterreich"},
{ "Falla, Manuel de", "Spanien"},{ "Faur\\u00e9, Gabriel Urbain", "Frankreich"},{ "Ferrero, Lorenzo", "Italien"},{ "Finzi, Gerald", "Italien"},{ "Franck, C\\u00e9sar Auguste Jean Guillaume Hubert", "Frankreich"},{ "Frankel, Benjamin", "England"},
{ "Fry, William Henry", "USA"},{ "Furtw\\u00e4ngler, Gustav Ernst Heinrich Wilhelm", "Deutschland"},{ "Gade, Niels Wilhelm", "D\\u00e4nemark"},{ "Gaos Guilloch\\u00f3n, Andr\\u00e9s", "Spanien"},{ "Gardener, John", "England"},{ "Geminiani, Francesco Saverio", "Italien"},
{ "Genzmer, Harald", "Deutschland"},{ "Gerber, Steven R.", "USA"},{ "German, Edward", "England"},{ "Gesualdo, Don Carlo", "Italien"},{ "Glass, Louis", "D\\u00e4nemark"},{ "Glass, Philip", "USA"},
{ "Glasunow, Aleksandr Konstantinowitsch", "Russland"},{ "Glinka, Michail Iwanowitsch", "Russland"},{ "Gluck, Christoph Willibald Ritter von", "Deutschland"},{ "Goldmark, Karl", "Ungarn"},{ "Goldschmidt, Berthold", "Deutschland"},{ "G\\u00f3recki, Henryk Mikolaj", "Polen"},
{ "Gotovac, Jakov", "Kroatien"},{ "Gould, Morton", "USA"},{ "Gounod, Charles Fran\\u00e7ois", "Frankreich"},{ "Gouvy, Th\\u00e9odore", "Frankreich"},{ "Grainger, Percy Adridge", "England"},{ "Granados y Campi\\u00f1a, Enrique", "Spanien"},
{ "Greef, Arthur de", "Niederlande"},{ "Gretschaninow, Alexander", "Russland"},{ "Grieg, Edvard Hagerup", "Norwegen"},{ "Grof\\u00e9, Ferde", "USA"},{ "Gubaidulina, Sofia", "Russland"},{ "Gurney, Ivor", "England"},
{ "Halffter, Ernesto", "Spanien"},{ "H\\u00e4ndel, Georg Friedrich", "Deutschland"},{ "Hanson, Howard", "USA"},{ "Hartmann, Karl Amedeus", "Deutschland"},{ "Harty, Sir Hamilton", "Irland"},{ "Haydn, Franz Joseph", "\\u00d6sterreich"},
{ "Haydn, Michael", "\\u00d6sterreich"},{ "Headington, Christopher", "England"},{ "Heini\\u00f6, Mikko", "Finnland"},{ "Henze, Hans Werner", "Deutschland"},{ "Herbert, Victor", "USA"},{ "Herzogenberg, Heinrich von", "\\u00d6sterreich"},
{ "Hill, Alfred", "Australien"},{ "Hindemith, Paul", "Deutschland"},{ "Holmboe, Vagn", "D\\u00e4nemark"},{ "Holst, Gustav Theodore", "England"},{ "Honegger, Arthur", "Schweiz"},{ "Hovhaness, Alan", "USA"},
{ "Howells, Herbert", "England"},{ "Huber, Hans", "Schweiz"},{ "Huber, Paul", "Schweiz"},{ "Hummel, Franz", "Deutschland"},{ "Ibert, Jacques Fran\\u00e7ois Antoine", "Frankreich"},{ "Ireland, John Nicholson", "England"},
{ "Ives, Charles Edward", "USA"},{ "Jan\\u00e1cek, Leos", "Tschechien"},{ "Jarrett, Keith", "USA"},{ "Kabalewskij, Dmitrij Borissowitsch", "Russland"},{ "Kaipainen, Jouni", "Finnland"},{ "Kajanus, Robert", "Finnland"},
{ "Kancheli, Giya", "Georgien"},{ "Kangro, Raimo", "Estland"},{ "Kapustin, Nikolai", "Russland"},{ "Kelemen, Milko", "Yugoslavien"},{ "Kernis, Aaron Jay", "USA"},{ "Kilpinen, Yrj\\u00f6", "Finnland"},
{ "Klami, Uuno", "Finnland"},{ "Klengel, Julius", "Deutschland"},{ "Kod\\u00e1ly, Zolt\\u00e1n", "Ungarn"},{ "Kokkonen, Joonas", "Finnland"},{ "Korngold, Erich Wolfgang", "\\u00d6sterreich;USA"},{ "Kraus, Joseph Martin", "Deutschland"},
{ "Krenek, Ernst", "Deutschland"},{ "Krohn, Felix", "Finnland"},{ "Kukal, Ondrej", "Tschechien"},{ "K\\u00fcnneke, Eduard", "Deutschland"},{ "Lajtha, L\\u00e1szl\\u00f3", "Ungarn"},{ "Lalo, Victor Antoine Edouard", "Frankreich"},
{ "Larsson, Lars-Erik", "Schweden"},{ "Lassus, Orlande de", "Niederlande"},{ "Lauro, Antonio", "Venezuela"},{ "Lees, Benjamin", "USA"},{ "Leifs, J\\u00f3n", "Island"},{ "Leighton, Kenneth", "England"},
{ "Liszt, Franz", "Ungarn;Deutschland"},{ "Llobet, Miguel", "Spanien"},{ "Lloyd, George", "England"},{ "Louri\\u00e9, Arthur", "Frankreich"},{ "Lukas, Zdenek", "Tschechien"},{ "Lully, Jean-Baptiste", "Frankreich"},
{ "Lutoslawski, Witold", "Polen"},{ "MacDowell, Edward Alexander", "USA"},{ "Macfarren, George", "England"},{ "MacMillan, James", "Scottland"},{ "Magnor\\u00e9, Agustin Barrios", "unknown"},{ "Mahler, Gustav", "\\u00d6sterreich"},
{ "Malipiero, Gian Francesco", "Italien"},{ "Maltas, Joaquin", "unknown"},{ "Manzoni, Giacomo", "Italien"},{ "Marco, Tom\\u00e1s", "Spanien"},{ "Margola, Franco", "Italien"},{ "Markevitsch, Igor", "Russland"},
{ "Martin, Frank Th\\u00e9odore", "Schweiz"},{ "Martin, Philip", "Irland"},{ "Martinu, Bohuslav", "Tschechien"},{ "Martynov, Vladimir", "unknown"},{ "Maslanka, David", "USA"},{ "Massenet, Jules \\u00c9mile Fr\\u00e9d\\u00e9ric", "Frankreich"},
{ "Meale, Richard", "Australien"},{ "Medtner, Nikolay Karlovich", "Russland"},{ "Mendelssohn Bartholdy, Jakob Ludwig Felix", "Deutschland"},{ "Mennin, Peter", "USA"},{ "Meril\\u00e4inen, Usko", "Finnland"},{ "Messiaen, Olivier Eug\\u00e9ne Prosper Charles", "Frankreich"},
{ "Meyer, Edgar", "USA"},{ "Meyer, Krzysztof", "Polen"},{ "Meyerbeer, Giacomo", "Deutschland"},{ "Miaskovsky, Nikolay", "Russland"},{ "Milhaud, Darius", "Frankreich"},{ "Moeran, Ernest John", "England"},
{ "Morel, Jorge", "Argentinien"},{ "Moszkowski, Moritz", "Polen"},{ "Mouton, Jean", "Frankreich"},{ "Moyzes, Alexander", "Slovakai"},{ "Mozart, Wolfgang Amadeus", "\\u00d6sterreich"},{ "Mudarra, Alonso de", "Spanien"},
{ "Mul, Jan", "unknown"},{ "Mussorgskij, Modest Petrowitsch", "Russland"},{ "Nepomuceno, Alberto", "Brasilien"},{ "Nielsen, Carl August", "D\\u00e4nemark"},{ "Nordgren, Pehr Henrik", "Finnland"},{ "Nordheim, Arne", "Norwegen"},
{ "Nosyrev, Mikhail", "Russland"},{ "Nummi, Seppo", "Finnland"},{ "Nyman, Michael", "England"},{ "Ockeghem, Johannes", "Niederlande"},{ "Ogermann, Claus", "Deutschland"},{ "Onslow, Andr\\u00e9 Georges Louis", "Frankreich"},
{ "Orff, Carl", "Deutschland"},{ "Osiek, Hans", "Holland"},{ "Pachelbel, Johann", "Deutschland"},{ "Pacius, Friedrich (Fredrik)", "Finnland"},{ "Paderewski, Ignacy Jan", "Polen"},{ "Paganini, Niccol\\u00f2", "Italien"},
{ "Palestrina, Giovanni Pierluigi da", "Italien"},{ "Parry, C. Hubert H.", "England"},{ "P\\u00e4rt, Arvo", "Estland;Russland"},{ "Pashe, William", "unknown"},{ "Penderecki, Krzysztof", "Polen"},{ "Pergolesi, Giovanni Battista", "Italien"},
{ "Peterson-Berger, Wilhelm", "Schweden"},{ "Pettersson, Allan", "Schweden"},{ "Pfitzner, Hans Erich", "Deutschland"},{ "Piston, Walter", "USA"},{ "Pizzetti, Ildebrando", "Italien"},{ "Pleyel, Ignace", "\\u00d6sterreich"},
{ "Ponce, Manuel Maria", "Mexico"},{ "Ponchielli, Amilcare", "Italien"},{ "Poulenc, Francis", "Frankreich"},{ "Preisner, Zbigniew", "Polen"},{ "Prez, Josquin des", "Frankreich"},{ "Prokofjew, Sergej Sergejewitsch", "Russland"},
{ "Purcell, Henry", "England"},{ "Quilter, Roger", "England"},{ "Rachmaninow, Sergej Wassiljewitsch", "Russland"},{ "Raitio, V\\u00e4in\\u00f6", "Finnland"},{ "Rangstr\\u00f6m, Ture", "Schweden"},{ "Rautavaara, Einojuhani", "Finnland"},
{ "Ravel, Joseph Maurice", "Frankreich"},{ "Rawsthorne, Alan", "England"},{ "Reinecke, Carl", "Deutschland"},{ "Respighi, Ottorino", "Italien"},{ "Rihm, Wolfgang Michael", "Deutschland"},{ "Rimskij-Korsakow, Nikolaj Andrejewitsch", "Russland"},
{ "Rodrigo, Joaquin", "Spanien"},{ "Rorem, Ned", "USA"},{ "Rosenberg, Hilding", "Schweden"},{ "Rosenstengel, Albrecht", "unknown"},{ "Rossini, Gioachino", "Italien"},{ "Rota, Nino", "Italien"},
{ "Rouse, Christopher", "USA"},{ "Roussel, Albert Charles Paul Marie", "Frankreich"},{ "Rubbra, Edmund", "England"},{ "Rubenstein, Anton", "Russland"},{ "Ruders, Poul", "D\\u00e4nemark"},{ "Ruiz-Pipo, Antonio", "Spanien"},
{ "Rutter, John", "England"},{ "Ruzicka, Peter", "Deutschland"},{ "Saint-Sa\\u00ebns, Camille", "Frankreich"},{ "Sainz de la Maza, Eduardo", "Spanien"},{ "Sallinen, Aulis", "Finnland"},{ "Salmanov, Vadim", "Russland"},
{ "Salonen, Esa-Pekka", "Finnland"},{ "Sanz, Gaspar", "Spanien"},{ "Satie, Alfred Erik Leslie", "Frankreich"},{ "S\\u00e1vio, Isa\\u00edas", "Uruguay;Brazilien"},{ "Saygun, Ahmet Adnan", "T\\u00fcrkei"},{ "Scarlatti, (Giuseppe) Domenico", "Italien"},
{ "Scarlatti, Alessandro", "Italien"},{ "Schmidt, Franz", "\\u00d6sterreich"},{ "Schnittke, Alfred Garrijewitsch", "Deutschland"},{ "Schoeck, Othmar", "Schweiz"},{ "Sch\\u00f6nberg, Arnold", "\\u00d6sterreich"},{ "Schostakowitsch, Dmitrij Dmitrijewitsch", "Russland"},
{ "Schreker, Franz", "Deutschland"},{ "Schtschedrin, Rodion Konstantinowitsch", "Russland"},{ "Schubert, Franz Peter", "\\u00d6sterreich"},{ "Schuman, William Howard", "USA"},{ "Schumann, Robert Alexander", "Deutschland"},{ "Sch\\u00fctz, Heinrich", "Deutschland"},
{ "Searle, Humphrey", "England"},{ "Segerstam, Leif", "Finnland"},{ "Serly, Tibor", "Ungarn"},{ "Sheppard, John", "England"},{ "Sheriff, Noam", "Israel"},
{ "Sibelius, Johan (Jean) Julius Christian", "Finnland"},{ "Silvestrov, Valentin", "Russland"},{ "Sinding, Christian", "Norwegen"},{ "Sisask, Urmas", "Estnien"},{ "Skrjabin, Aleksandr Nikolajewitsch", "Russland"},{ "Smetana, Bedrich", "Tschechien"},
{ "Smit, Leo", "Niederlande"},{ "Sojo, Vincente Emilio", "Venezuela"},{ "Soler, Antonio", "Spanien"},{ "Somervell, Arthur", "England"},{ "Sor, Fernando", "Spanien"},{ "Spohr, Louis", "Deutschland"},
{ "Spontini, Gaspare Luigi Pacifico", "Italien"},{ "Stamitz, Carl Philipp", "Deutschland"},{ "Stanford, Sir Charles Villiers", "England"},{ "Steinberg, Maximilian", "Litauen"},{ "Stenhammar, Wilhelm", "Schweden"},{ "Stevens, Bernard", "England"},
{ "St\\u00f6lzel, Gottfried Heinrich", "Deutschland"},{ "Strauss, Richard", "Deutschland"},{ "Strawinsky, Igor Fjodorowitsch", "Russland;USA"},{ "Strong, George Templeton", "USA;Schweiz"},{ "Suk, Josef", "Tschechien"},{ "Sullivan, Sir Arthur Seymour", "England"},
{ "Sutermeister, Heinrich", "Schweden"},{ "Svendsen, Johan", "Norwegen"},{ "Szymanowski, Karol Maciej", "Polen"},{ "Tajcevic, Marko", "Serbien"},{ "Takemitsu, Toru", "Japan"},{ "Tallis, Thomas", "England"},
{ "Tamberg, Eino", "Estnien"},{ "Taneyev, Alexander Sergeievich", "Russland"},{ "Tansman, Alexandre", "Polen"},{ "T\\u00e1rrega, Francisco", "Spanien"},{ "Tartini, Giuseppe", "Italien"},{ "Tavener, John", "England"},
{ "Tcherepnin, Alexander Nikolaiewitch", "Russland"},{ "Telemann, Georg Philipp", "Deutschland"},{ "Terzakis, Dimitri", "Griechenland"},{ "Thalberg, Sgismond", "Schweiz"},{ "Theodorakis, Mikis", "Griechenland"},{ "Thomas, Charles Louis Ambroise", "Frankreich"},
{ "Thomson, Virgil", "USA"},{ "Tippett, Sir Michael", "England"},{ "Toch, Ernst", "\\u00d6sterreich"},{ "Torroba, Federico Moreno", "Spanien"},{ "Traditional", "unknown"},{ "Tschaikowsky, Peter Ilych", "Russland"},
{ "Turina, Joaquin", "Spanien"},{ "Tveitt, Geirr", "Norwegen"},{ "Ullmann, Viktor Josef", "Deutschland"},{ "unbekannt", "unknown"},{ "V\\u00e4hi, Peeter", "Estnien"},{ "Vanhal, Johann Baptist", "\\u00d6sterreich"},
{ "Vaughan Williams, Ralph", "England"},{ "Veale, John", "England"},{ "Verdi, Giuseppe Fortunino Francesco", "Italien"},{ "Veress, S\\u00e1ndor", "Ungarn"},{ "Victory, Gerard", "Irland"},{ "Vieuxtemps, Henri Joseph Fran\\u00e7ois", "Belgien"},
{ "Villa-Lobos, Heitor", "Brasilien"},{ "Viotti, Giovanni Battista", "Italien"},{ "Vivaldi, Antonio Lucio", "Italien"},{ "Volkmann, Robert", "Deutschland"},{ "Vorisek, Jan Hugo", "Tschechien"},{ "Wagner, (Wilhelm) Richard", "Deutschland"},
{ "Wallace, Stewart", "USA"},{ "Walton, Sir William Turner", "England"},{ "Warlock, Peter", "England"},{ "Webern, Anton Friedrich Wilhelm", "\\u00d6sterreich"},{ "Weil, Kurt", "Deutschland"},{ "Wetz, Richard", "Deutschland"},
{ "Widor, Charles Marie", "England"},{ "Williams, John", "USA"},{ "Willson, Meredith", "USA"},{ "Wilson, James", "England"},{ "Wir\\u00e9n, Dag", "Schweden"},{ "Wolf, Hugo Philipp Jakob", "\\u00d6sterreich"},
{ 0, 0 }
};


RStringArrayArray philophers_names;
RStringArrayArray componist_names;

void prepare(NamedPair* np, RStringArrayArray& sa)
{
  
  int size; 
  for (size = 0; np[size].first != 0; ++size)
  {
  }
  sa = new StringArrayArray(size);
  int i;
  for (i = 0; np[i].first != 0; ++i)
  {
    sa[i] = new StringArray(2);
    sa[i][0] = _US(np[i].first);
    sa[i][1] = _US(np[i].second);
  }
}
 


void fillMap(IN(RMap) map, IN(RStringArrayArray)  np)
{
  testAssert(map->isEmpty() == true);

  int size = np->length();
  for (int i = 0; i < size; ++i)
  {
    map->put(&np[i][0], &np[i][1]);
  }
  testAssert(map->isEmpty() == false);
  testAssert(map->size() == size);
}

void fetchOwnElements(IN(RMap) map)
{
   {
    RSet keyset = map->keySet();
    testAssert(keyset->size() == map->size());
    RIterator it = keyset->iterator();
    while (it->hasNext() == true)
    {
      RString obj = (RString)it->next();
      if(map->containsKey(&obj) == false)
        testAssert(map->containsKey(&obj) == true);
    }
  }
  {
    RCollection valcol = map->values();
    testAssert(valcol->size() == map->size());
    RIterator it = valcol->iterator();
    int ic = 0;
    while (it->hasNext() == true)
    {
      RString obj = (RString)it->next();
      if (map->containsValue(&obj) == false)
        testAssert(map->containsValue(&obj) == true);
      ++ic;
    }
    testAssert(ic == map->size());
  }
}

void fetchElements(IN(RMap) map, IN(RStringArrayArray)  np)
{
  int size = np->length();
  int i;
  for (i = 0; i < size; ++i)
  {
    if (map->get(&np[i][0])->equals(&np[i][1]) != true)
      testAssert(map->get(&np[i][0])->equals(&np[i][1]) == true);
  }
  fetchOwnElements(map);
  for (i = 0; i < size; ++i)
  {
    if (map->containsKey(&np[i][0]) == false)
      testAssert(map->containsKey(&np[i][0]) == true);
    if (map->containsValue(&np[i][1]) == false)
      testAssert(map->containsValue(&np[i][1]) == true);
  }
}


void removeElements(IN(RMap) map, IN(RStringArrayArray)  np)
{
  int size = np->length();
  int oldsize = map->size();
  int i;
  for (i = 0; i < size; ++i)
  {
    if ((i % 3) == 0) 
    {
      map->remove(&np[i][0]);
      if (map->containsKey(&np[i][0]) == true)
        testAssert(map->containsKey(&np[i][0]) == false);
    }
  }
  testAssert(map->size() < oldsize);
  
}

void clearMap(IN(RMap) cont)
{
  cont->clear();
  testAssert(cont->isEmpty() == true);
  testAssert(cont->size() == 0);

}

void testContainer(IN(RMap) map, IN(RStringArrayArray) np)
{
  fillMap(map, np);
  fetchElements(map, np);
  removeElements(map, np);
  clearMap(map);
}

void testSortedMap(IN(RSortedMap) smap, IN(RStringArrayArray) np)
{
  fillMap(&smap, np);
  RSortedMap submap = smap->tailMap(&np[0][0]);
  fetchOwnElements(&submap);
  submap = submap->headMap(&np[1][0]);
  fetchOwnElements(&submap);
}


void testContainer(IN(RList) list, IN(RStringArrayArray) np)
{
  int size = np->length();
  int i;
  for (i = 0; i < size; ++i)
  {
    list->add(&np[i][0]);
  }
  testAssert(list->isEmpty() == false);
  testAssert(list->size() == size);
  for (i = 0; i < size; ++i)
  {
    if (list->indexOf(&np[i][0]) == -1)
      testAssert(list->indexOf(&np[i][0]) != -1);
    if (list->lastIndexOf(&np[i][0]) == -1)
      testAssert(list->lastIndexOf(&np[i][0]) != -1);
  }
  int csize = list->size();
  for (i = 1; i < size; ++i)
  {
    if ((i % 7) == 0 && i < csize)
    {
      RObject obj = list->get(i);
      csize = list->size();
      list->remove(i);
      if (list->size() + 1 != csize)
        testAssert(list->size() + 1 == csize);
      csize = list->size();
      if (list->contains(obj) == true)
        testAssert(list->contains(obj) == false);
    } else if ((i % 5) == 0) {
      int idx = list->indexOf(&np[i][0]);
      if (idx == -1)
        continue;
      RObject obj = list->get(idx);
      csize = list->size();
      list->remove(obj);
      if (list->size() + 1 != csize)
        testAssert(list->size() + 1 == csize);
      if (list->indexOf(obj) != -1)
        testAssert(list->indexOf(obj) == -1);
      csize = list->size();
    }
  }
  list->clear();
  testAssert(list->size() == 0);
  testAssert(list->isEmpty() == true);
}


void testList()
{
  RArrayList alist = new ArrayList();
  testContainer((RList)&alist, philophers_names);
  testContainer(Collections::synchronizedList(new ArrayList()), philophers_names);
  
  RLinkedList llist = new LinkedList();
  testContainer((RList)&llist, philophers_names);
  testContainer(Collections::synchronizedList(new LinkedList()), philophers_names);
}

void testMap()
{
   RHashMap hm = new HashMap();

  //testContainer(&hm, philophers_names);
  testContainer((RMap)&hm, componist_names);
  testContainer(Collections::synchronizedMap(new HashMap()), componist_names);

  RTreeMap tm = new TreeMap();
  //testContainer(&tm, philophers_names);
  testContainer((RMap)&tm, componist_names);
  testContainer(Collections::synchronizedMap(new TreeMap()), componist_names);

  //testSortedMap(&tm, philophers_names);
  testSortedMap(&tm, componist_names);
  testSortedMap(Collections::synchronizedSortedMap(new TreeMap()), componist_names);

}


void testContainer(IN(RSet) set, IN(RStringArrayArray) np)
{
  int size = np->length();
  int i;
  for (i = 0; i < size; ++i)
  {
    set->add(&np[i][0]);
  }
  testAssert(set->isEmpty() == false);
  testAssert(set->size() == size);
  
  for (i = 1; i < size; ++i)
  {
    if (set->contains(&np[i][0]) != true)
      testAssert(set->contains(&np[i][0]) == true);
    if ((i % 7) == 0)
    {
      set->remove(&np[i][0]);
      if (set->contains(&np[i][0]) != false)
        testAssert(set->contains(&np[i][0]) == false);
    }
  }
  

  set->clear();
  testAssert(set->size() == 0);
  testAssert(set->isEmpty() == true);
}

void testSet()
{
  RHashSet hashset = new HashSet();
  testContainer((RSet)&hashset, componist_names);
  testContainer(Collections::synchronizedSet(new HashSet()), componist_names);

  RTreeSet treeset = new TreeSet();
  testContainer((RSet)&treeset, componist_names);
  testContainer(Collections::synchronizedSet(new TreeSet()), componist_names);
}

} // anon namespace

void Container_Test::standardMap()
{
  prepare(philophers_namess, philophers_names);
  prepare(componist_namess, componist_names);
  ::acdk::lang::sys::tick_t tstart = ::acdk::lang::sys::core_tick::now();
  testMap();
  int millis = ::acdk::lang::sys::core_tick::millisecsSince(tstart);
  System::out->println(RString("Container::testMap takes millis: ") + millis);
  testList();
  testSet();
}

void
Container_Test::arrayList()
{
  RArrayList arr = new ArrayList(0);
  RObject o = new String("asdf");
  arr->add(o);
}


} // namespace util
} // namespace acdk
} // namespace tests

