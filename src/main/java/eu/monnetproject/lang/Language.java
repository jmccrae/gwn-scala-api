/**
 * ********************************************************************************
 * Copyright (c) 2011, Monnet Project All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met: *
 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer. * Redistributions in binary
 * form must reproduce the above copyright notice, this list of conditions and
 * the following disclaimer in the documentation and/or other materials provided
 * with the distribution. * Neither the name of the Monnet Project nor the names
 * of its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE MONNET PROJECT BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ********************************************************************************
 */
package eu.monnetproject.lang;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import java.util.LinkedList;
import java.util.HashMap;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Describes a language. This is primarily based around the ISO 639 standard,
 * all languages specified in ISO 639-1 are represented here, and others can be
 * created as desired. Languages can be obtained as follows <ul> <li>By static
 * declaration
 * <code>Language.ENGLISH</code></li> <li>By defining a regional or script
 * variation
 * <code>Language.ENGLISH.getRegionalVariant(Region.UNITED_KINGDOM)</code></li>
 * <li>By looking up an ISO code
 * <code>Language.getByIso639_1("en")</code></li> <li>By looking up an IETF code
 * <code>Language.get("en-GB")</code></li> <li>By creating a new instance
 * <code>Language.getInstance("German","Deutsch","de","deu","deu")</code></li>
 * </ul> Language tags follow the IETF language tag model, as described in RFC
 * 4646. The format of the tags is as follows: <p><code>language (-script)? (-region)? (-variant)* (-extension)* (-privateuse)?</code></p>
 * The string form of these may be obtained as follows <ul> <li>The full tag can
 * be obtained by calling
 * <code>toString()</code></li> <li>The language by calling
 * <code>getIso639_1</code> (two letter code),
 * <code>getIso639_2</code> or
 * <code>getIso639_3</code> (three letter code)</li> <li>The script by calling
 * <code>getScript()</code></li> <li>The region by calling
 * <code>getRegion()</code></li> <li>The variant(s) by calling
 * <code>getVariants()</code></li> <li>The extension(s) by calling
 * <code>getExtensions()</code></li> <li>The private use by calling
 * <code>getPrivateUse()</code></li> </ul>
 *
 * Note we use ISO 639-2 T codes. ISO 639-2 B codes are not supported.
 *
 * @author John McCrae
 */
public final class Language implements Serializable {

    private static final long serialVersionUID = 3056669048941419450L;
    private String name;
    private String nativeName;
    private String iso639_1;
    private String iso639_2;
    private String iso639_3;
    private Region region;
    private Script script;
    private List<String> variant;
    private List<String> extension;
    private String privateUse;

    private Language(String name, String nativeName, String iso639_1, String iso639_2, String iso639_3) {
        this.name = name;
        this.nativeName = nativeName;
        this.iso639_1 = iso639_1;
        this.iso639_2 = iso639_2;
        this.iso639_3 = iso639_3;
        region = null;
        script = null;
        this.variant = new LinkedList<String>();
        this.extension = new LinkedList<String>();
        this.privateUse = null;
    }

    private Language(String name, String nativeName, String iso639_1, String iso639_2, String iso639_3, Region region, Script script, List<String> variant, List<String> extension, String privateUse) {
        this.name = name;
        this.nativeName = nativeName;
        this.iso639_1 = iso639_1;
        this.iso639_2 = iso639_2;
        this.iso639_3 = iso639_3;
        this.region = region;
        this.script = script;
        this.variant = variant;
        this.extension = extension;
        this.privateUse = privateUse;
    }
    private static final HashMap<String, Language> byIso639_1 = new HashMap<String, Language>();
    private static final HashMap<String, Language> byIso639_2 = new HashMap<String, Language>();
    private static final HashMap<String, Language> byIso639_3 = new HashMap<String, Language>();
    public static final Language ABKHAZIAN = new Language("Abkhazian", "\u0410\u04a7\u0441\u0443\u0430", "ab", "abk", "abk");

    static {
        byIso639_1.put("ab", ABKHAZIAN);
        byIso639_2.put("abk", ABKHAZIAN);
        byIso639_3.put("abk", ABKHAZIAN);
    }
    public static final Language AFAR = new Language("Afar", "Afaraf", "aa", "aar", "aar");

    static {
        byIso639_1.put("aa", AFAR);
        byIso639_2.put("aar", AFAR);
        byIso639_3.put("aar", AFAR);
    }
    public static final Language AFRIKAANS = new Language("Afrikaans", "Afrikaans", "af", "afr", "afr");

    static {
        byIso639_1.put("af", AFRIKAANS);
        byIso639_2.put("afr", AFRIKAANS);
        byIso639_3.put("afr", AFRIKAANS);
    }
    public static final Language AKAN = new Language("Akan", "Akan", "ak", "aka", "aka");

    static {
        byIso639_1.put("ak", AKAN);
        byIso639_2.put("aka", AKAN);
        byIso639_3.put("aka", AKAN);
    }
    public static final Language ALBANIAN = new Language("Albanian", "Shqip", "sq", "sqi", "sqi");

    static {
        byIso639_1.put("sq", ALBANIAN);
        byIso639_2.put("sqi", ALBANIAN);
        byIso639_3.put("sqi", ALBANIAN);
    }
    public static final Language AMHARIC = new Language("Amharic", "\u12a0\u121b\u122d\u129b", "am", "amh", "amh");

    static {
        byIso639_1.put("am", AMHARIC);
        byIso639_2.put("amh", AMHARIC);
        byIso639_3.put("amh", AMHARIC);
    }
    public static final Language ARABIC = new Language("Arabic", "\u0627\u0644\u0639\u0631\u0628\u064a\u0629", "ar", "ara", "ara");

    static {
        byIso639_1.put("ar", ARABIC);
        byIso639_2.put("ara", ARABIC);
        byIso639_3.put("ara", ARABIC);
    }
    public static final Language ARAGONESE = new Language("Aragonese", "Aragon\u00e9s", "an", "arg", "arg");

    static {
        byIso639_1.put("an", ARAGONESE);
        byIso639_2.put("arg", ARAGONESE);
        byIso639_3.put("arg", ARAGONESE);
    }
    public static final Language ASSAMESE = new Language("Assamese", "\u0985\u09b8\u09ae\u09c0\u09af\u09bc\u09be", "as", "asm", "asm");

    static {
        byIso639_1.put("as", ASSAMESE);
        byIso639_2.put("asm", ASSAMESE);
        byIso639_3.put("asm", ASSAMESE);
    }
    public static final Language ARMENIAN = new Language("Armenian", "\u0540\u0561\u0575\u0565\u0580\u0565\u0576", "hy", "hye", "hye");

    static {
        byIso639_1.put("hy", ARMENIAN);
        byIso639_2.put("hye", ARMENIAN);
        byIso639_3.put("hye", ARMENIAN);
    }
    public static final Language AVARIC = new Language("Avaric", "\u0430\u0432\u0430\u0440 \u043c\u0430\u0446\u04c0, \u043c\u0430\u0433\u04c0\u0430\u0440\u0443\u043b \u043c\u0430\u0446\u04c0", "av", "ava", "ava");

    static {
        byIso639_1.put("av", AVARIC);
        byIso639_2.put("ava", AVARIC);
        byIso639_3.put("ava", AVARIC);
    }
    public static final Language AVESTAN = new Language("Avestan", "Avesta", "ae", "ave", "ave");

    static {
        byIso639_1.put("ae", AVESTAN);
        byIso639_2.put("ave", AVESTAN);
        byIso639_3.put("ave", AVESTAN);
    }
    public static final Language AYMARA = new Language("Aymara", "Aymar aru", "ay", "aym", "aym");

    static {
        byIso639_1.put("ay", AYMARA);
        byIso639_2.put("aym", AYMARA);
        byIso639_3.put("aym", AYMARA);
    }
    public static final Language AZERBAIJANI = new Language("Azerbaijani", "az\u0259rbaycan dili", "az", "aze", "aze");

    static {
        byIso639_1.put("az", AZERBAIJANI);
        byIso639_2.put("aze", AZERBAIJANI);
        byIso639_3.put("aze", AZERBAIJANI);
    }
    public static final Language BASHKIR = new Language("Bashkir", "\u0431\u0430\u0448\u04a1\u043e\u0440\u0442 \u0442\u0435\u043b\u0435", "ba", "bak", "bak");

    static {
        byIso639_1.put("ba", BASHKIR);
        byIso639_2.put("bak", BASHKIR);
        byIso639_3.put("bak", BASHKIR);
    }
    public static final Language BAMBARA = new Language("Bambara", "Bamanankan", "bm", "bam", "bam");

    static {
        byIso639_1.put("bm", BAMBARA);
        byIso639_2.put("bam", BAMBARA);
        byIso639_3.put("bam", BAMBARA);
    }
    public static final Language BASQUE = new Language("Basque", "Euskara", "eu", "eus", "eus");

    static {
        byIso639_1.put("eu", BASQUE);
        byIso639_2.put("eus", BASQUE);
        byIso639_3.put("eus", BASQUE);
    }
    public static final Language BELARUSIAN = new Language("Belarusian", "\u0411\u0435\u043b\u0430\u0440\u0443\u0441\u043a\u0430\u044f", "be", "bel", "bel");

    static {
        byIso639_1.put("be", BELARUSIAN);
        byIso639_2.put("bel", BELARUSIAN);
        byIso639_3.put("bel", BELARUSIAN);
    }
    public static final Language BENGALI = new Language("Bengali", "\u09ac\u09be\u0982\u09b2\u09be", "bn", "ben", "ben");

    static {
        byIso639_1.put("bn", BENGALI);
        byIso639_2.put("ben", BENGALI);
        byIso639_3.put("ben", BENGALI);
    }
    public static final Language BIHARI = new Language("Bihari", "\u092d\u094b\u091c\u092a\u0941\u0930\u0940", "bh", "bih", null);

    static {
        byIso639_1.put("bh", BIHARI);
        byIso639_2.put("bih", BIHARI);
    }
    public static final Language BISLAMA = new Language("Bislama", "Bislama", "bi", "bis", "bis");

    static {
        byIso639_1.put("bi", BISLAMA);
        byIso639_2.put("bis", BISLAMA);
        byIso639_3.put("bis", BISLAMA);
    }
    public static final Language BOSNIAN = new Language("Bosnian", "Bosanski jezik", "bs", "bos", "bos");

    static {
        byIso639_1.put("bs", BOSNIAN);
        byIso639_2.put("bos", BOSNIAN);
        byIso639_3.put("bos", BOSNIAN);
    }
    public static final Language BRETON = new Language("Breton", "Brezhoneg", "br", "bre", "bre");

    static {
        byIso639_1.put("br", BRETON);
        byIso639_2.put("bre", BRETON);
        byIso639_3.put("bre", BRETON);
    }
    public static final Language BULGARIAN = new Language("Bulgarian", "\u0431\u044a\u043b\u0433\u0430\u0440\u0441\u043a\u0438 \u0435\u0437\u0438\u043a", "bg", "bul", "bul");

    static {
        byIso639_1.put("bg", BULGARIAN);
        byIso639_2.put("bul", BULGARIAN);
        byIso639_3.put("bul", BULGARIAN);
    }
    public static final Language BURMESE = new Language("Burmese", "\u1017\u1019\u102c\u1005\u102c", "my", "mya", "mya");

    static {
        byIso639_1.put("my", BURMESE);
        byIso639_2.put("mya", BURMESE);
        byIso639_3.put("mya", BURMESE);
    }
    public static final Language CATALAN = new Language("Catalan", "Catal\u00e0", "ca", "cat", "cat");

    static {
        byIso639_1.put("ca", CATALAN);
        byIso639_2.put("cat", CATALAN);
        byIso639_3.put("cat", CATALAN);
    }
    public static final Language CHAMORRO = new Language("Chamorro", "Chamoru", "ch", "cha", "cha");

    static {
        byIso639_1.put("ch", CHAMORRO);
        byIso639_2.put("cha", CHAMORRO);
        byIso639_3.put("cha", CHAMORRO);
    }
    public static final Language CHECHEN = new Language("Chechen", "\u043d\u043e\u0445\u0447\u0438\u0439\u043d \u043c\u043e\u0442\u0442", "ce", "che", "che");

    static {
        byIso639_1.put("ce", CHECHEN);
        byIso639_2.put("che", CHECHEN);
        byIso639_3.put("che", CHECHEN);
    }
    public static final Language CHICHEWA = new Language("Chichewa", "chiChe\u0175a", "ny", "nya", "nya");

    static {
        byIso639_1.put("ny", CHICHEWA);
        byIso639_2.put("nya", CHICHEWA);
        byIso639_3.put("nya", CHICHEWA);
    }
    public static final Language CHINESE = new Language("Chinese", "\u6f22\u8a9e", "zh", "zho", "zho");

    static {
        byIso639_1.put("zh", CHINESE);
        byIso639_2.put("zho", CHINESE);
        byIso639_3.put("zho", CHINESE);
    }
    public static final Language CHUVASH = new Language("Chuvash", "\u0447\u04d1\u0432\u0430\u0448 \u0447\u04d7\u043b\u0445\u0438", "cv", "chv", "chv");

    static {
        byIso639_1.put("cv", CHUVASH);
        byIso639_2.put("chv", CHUVASH);
        byIso639_3.put("chv", CHUVASH);
    }
    public static final Language CORNISH = new Language("Cornish", "Kernewek", "kw", "cor", "cor");

    static {
        byIso639_1.put("kw", CORNISH);
        byIso639_2.put("cor", CORNISH);
        byIso639_3.put("cor", CORNISH);
    }
    public static final Language CORSICAN = new Language("Corsican", "Corsu", "co", "cos", "cos");

    static {
        byIso639_1.put("co", CORSICAN);
        byIso639_2.put("cos", CORSICAN);
        byIso639_3.put("cos", CORSICAN);
    }
    public static final Language CREE = new Language("Cree", "\u14c0\u1426\u1403\u152d\u140d\u140f\u1423", "cr", "cre", "cre");

    static {
        byIso639_1.put("cr", CREE);
        byIso639_2.put("cre", CREE);
        byIso639_3.put("cre", CREE);
    }
    public static final Language CROATIAN = new Language("Croatian", "Hrvatski", "hr", "hrv", "hrv");

    static {
        byIso639_1.put("hr", CROATIAN);
        byIso639_2.put("hrv", CROATIAN);
        byIso639_3.put("hrv", CROATIAN);
    }
    public static final Language CZECH = new Language("Czech", "\u010desky, \u010de\u0161tina", "cs", "ces", "ces");

    static {
        byIso639_1.put("cs", CZECH);
        byIso639_2.put("ces", CZECH);
        byIso639_3.put("ces", CZECH);
    }
    public static final Language DANISH = new Language("Danish", "Dansk", "da", "dan", "dan");

    static {
        byIso639_1.put("da", DANISH);
        byIso639_2.put("dan", DANISH);
        byIso639_3.put("dan", DANISH);
    }
    public static final Language DIVEHI = new Language("Divehi", "\u078b\u07a8\u0788\u07ac\u0780\u07a8", "dv", "div", "div");

    static {
        byIso639_1.put("dv", DIVEHI);
        byIso639_2.put("div", DIVEHI);
        byIso639_3.put("div", DIVEHI);
    }
    public static final Language DZONGKHA = new Language("Dzongkha", "\u0f62\u0fab\u0f7c\u0f44\u0f0b\u0f41", "dz", "dzo", "dzo");

    static {
        byIso639_1.put("dz", DZONGKHA);
        byIso639_2.put("dzo", DZONGKHA);
        byIso639_3.put("dzo", DZONGKHA);
    }
    public static final Language ENGLISH = new Language("English", "English", "en", "eng", "eng");

    static {
        byIso639_1.put("en", ENGLISH);
        byIso639_2.put("eng", ENGLISH);
        byIso639_3.put("eng", ENGLISH);
    }
    public static final Language ESPERANTO = new Language("Esperanto", "Esperanto", "eo", "epo", "epo");

    static {
        byIso639_1.put("eo", ESPERANTO);
        byIso639_2.put("epo", ESPERANTO);
        byIso639_3.put("epo", ESPERANTO);
    }
    public static final Language ESTONIAN = new Language("Estonian", "Eesti", "et", "est", "est");

    static {
        byIso639_1.put("et", ESTONIAN);
        byIso639_2.put("est", ESTONIAN);
        byIso639_3.put("est", ESTONIAN);
    }
    public static final Language EWE = new Language("Ewe", "E\u028begbe", "ee", "ewe", "ewe");

    static {
        byIso639_1.put("ee", EWE);
        byIso639_2.put("ewe", EWE);
        byIso639_3.put("ewe", EWE);
    }
    public static final Language FAROESE = new Language("Faroese", "F\u00f8royskt", "fo", "fao", "fao");

    static {
        byIso639_1.put("fo", FAROESE);
        byIso639_2.put("fao", FAROESE);
        byIso639_3.put("fao", FAROESE);
    }
    public static final Language FIJIAN = new Language("Fijian", "vosa Vakaviti", "fj", "fij", "fij");

    static {
        byIso639_1.put("fj", FIJIAN);
        byIso639_2.put("fij", FIJIAN);
        byIso639_3.put("fij", FIJIAN);
    }
    public static final Language FINNISH = new Language("Finnish", "Suomi", "fi", "fin", "fin");

    static {
        byIso639_1.put("fi", FINNISH);
        byIso639_2.put("fin", FINNISH);
        byIso639_3.put("fin", FINNISH);
    }
    public static final Language FRENCH = new Language("French", "Fran\u00e7ais", "fr", "fra", "fra");

    static {
        byIso639_1.put("fr", FRENCH);
        byIso639_2.put("fra", FRENCH);
        byIso639_3.put("fra", FRENCH);
    }
    public static final Language FULA = new Language("Fula", "Fulfulde", "ff", "ful", "ful");

    static {
        byIso639_1.put("ff", FULA);
        byIso639_2.put("ful", FULA);
        byIso639_3.put("ful", FULA);
    }
    public static final Language GALICIAN = new Language("Galician", "Galego", "gl", "glg", "glg");

    static {
        byIso639_1.put("gl", GALICIAN);
        byIso639_2.put("glg", GALICIAN);
        byIso639_3.put("glg", GALICIAN);
    }
    public static final Language GERMAN = new Language("German", "Deutsch", "de", "deu", "deu");

    static {
        byIso639_1.put("de", GERMAN);
        byIso639_2.put("deu", GERMAN);
        byIso639_3.put("deu", GERMAN);
    }
    public static final Language GREEK = new Language("Greek", "\u0395\u03bb\u03bb\u03b7\u03bd\u03b9\u03ba\u03ac", "el", "ell", "ell");

    static {
        byIso639_1.put("el", GREEK);
        byIso639_2.put("ell", GREEK);
        byIso639_3.put("ell", GREEK);
    }
    public static final Language GUARANI = new Language("Guaran\u00ed", "Ava\u00f1e'\u1ebd", "gn", "grn", "grn");

    static {
        byIso639_1.put("gn", GUARANI);
        byIso639_2.put("grn", GUARANI);
        byIso639_3.put("grn", GUARANI);
    }
    public static final Language GUJARATI = new Language("Gujarati", "\u0a97\u0ac1\u0a9c\u0ab0\u0abe\u0aa4\u0ac0", "gu", "guj", "guj");

    static {
        byIso639_1.put("gu", GUJARATI);
        byIso639_2.put("guj", GUJARATI);
        byIso639_3.put("guj", GUJARATI);
    }
    public static final Language HAITIAN = new Language("Haitian", "Krey\u00f2l ayisyen", "ht", "hat", "hat");

    static {
        byIso639_1.put("ht", HAITIAN);
        byIso639_2.put("hat", HAITIAN);
        byIso639_3.put("hat", HAITIAN);
    }
    public static final Language HAUSA = new Language("Hausa", "Hausa", "ha", "hau", "hau");

    static {
        byIso639_1.put("ha", HAUSA);
        byIso639_2.put("hau", HAUSA);
        byIso639_3.put("hau", HAUSA);
    }
    public static final Language HEBREW = new Language("Hebrew", "\u05e2\u05d1\u05e8\u05d9\u05ea", "he", "heb", "heb");

    static {
        byIso639_1.put("he", HEBREW);
        byIso639_2.put("heb", HEBREW);
        byIso639_3.put("heb", HEBREW);
    }
    public static final Language HERERO = new Language("Herero", "Otjiherero", "hz", "her", "her");

    static {
        byIso639_1.put("hz", HERERO);
        byIso639_2.put("her", HERERO);
        byIso639_3.put("her", HERERO);
    }
    public static final Language HINDI = new Language("Hindi", "\u0939\u093f\u0928\u094d\u0926\u0940, \u0939\u093f\u0902\u0926\u0940", "hi", "hin", "hin");

    static {
        byIso639_1.put("hi", HINDI);
        byIso639_2.put("hin", HINDI);
        byIso639_3.put("hin", HINDI);
    }
    public static final Language HIRI_MOTU = new Language("Hiri Motu", "Hiri Motu", "ho", "hmo", "hmo");

    static {
        byIso639_1.put("ho", HIRI_MOTU);
        byIso639_2.put("hmo", HIRI_MOTU);
        byIso639_3.put("hmo", HIRI_MOTU);
    }
    public static final Language HUNGARIAN = new Language("Hungarian", "Magyar", "hu", "hun", "hun");

    static {
        byIso639_1.put("hu", HUNGARIAN);
        byIso639_2.put("hun", HUNGARIAN);
        byIso639_3.put("hun", HUNGARIAN);
    }
    public static final Language INTERLINGUA = new Language("Interlingua (IALA)", "Interlingua", "ia", "ina", "ina");

    static {
        byIso639_1.put("ia", INTERLINGUA);
        byIso639_2.put("ina", INTERLINGUA);
        byIso639_3.put("ina", INTERLINGUA);
    }
    public static final Language INDONESIAN = new Language("Indonesian", "Bahasa Indonesia", "id", "ind", "ind");

    static {
        byIso639_1.put("id", INDONESIAN);
        byIso639_2.put("ind", INDONESIAN);
        byIso639_3.put("ind", INDONESIAN);
    }
    public static final Language INTERLINGUE = new Language("Interlingue", "Interlingue", "ie", "ile", "ile");

    static {
        byIso639_1.put("ie", INTERLINGUE);
        byIso639_2.put("ile", INTERLINGUE);
        byIso639_3.put("ile", INTERLINGUE);
    }
    public static final Language IRISH = new Language("Irish", "Gaeilge", "ga", "gle", "gle");

    static {
        byIso639_1.put("ga", IRISH);
        byIso639_2.put("gle", IRISH);
        byIso639_3.put("gle", IRISH);
    }
    public static final Language IGBO = new Language("Igbo", "Igbo", "ig", "ibo", "ibo");

    static {
        byIso639_1.put("ig", IGBO);
        byIso639_2.put("ibo", IGBO);
        byIso639_3.put("ibo", IGBO);
    }
    public static final Language SICHUAN_YI = new Language("Sichuan Yi", "\ua187\ua259", "ii", "iii", "iii");

    static {
        byIso639_1.put("ii", SICHUAN_YI);
        byIso639_2.put("iii", SICHUAN_YI);
        byIso639_3.put("iii", SICHUAN_YI);
    }
    public static final Language INUPIAQ = new Language("Inupiaq", "I\u00f1upiaq", "ik", "ipk", "ipk");

    static {
        byIso639_1.put("ik", INUPIAQ);
        byIso639_2.put("ipk", INUPIAQ);
        byIso639_3.put("ipk", INUPIAQ);
    }
    public static final Language IDO = new Language("Ido", "Ido", "io", "ido", "ido");

    static {
        byIso639_1.put("io", IDO);
        byIso639_2.put("ido", IDO);
        byIso639_3.put("ido", IDO);
    }
    public static final Language ICELANDIC = new Language("Icelandic", "\u00cdslenska", "is", "isl", "isl");

    static {
        byIso639_1.put("is", ICELANDIC);
        byIso639_2.put("isl", ICELANDIC);
        byIso639_3.put("isl", ICELANDIC);
    }
    public static final Language ITALIAN = new Language("Italian", "Italiano", "it", "ita", "ita");

    static {
        byIso639_1.put("it", ITALIAN);
        byIso639_2.put("ita", ITALIAN);
        byIso639_3.put("ita", ITALIAN);
    }
    public static final Language INUKTITUT = new Language("Inuktitut", "\u1403\u14c4\u1483\u144e\u1450\u1466", "iu", "iku", "iku");

    static {
        byIso639_1.put("iu", INUKTITUT);
        byIso639_2.put("iku", INUKTITUT);
        byIso639_3.put("iku", INUKTITUT);
    }
    public static final Language JAPANESE = new Language("Japanese", "\u65e5\u672c\u8a9e", "ja", "jpn", "jpn");

    static {
        byIso639_1.put("ja", JAPANESE);
        byIso639_2.put("jpn", JAPANESE);
        byIso639_3.put("jpn", JAPANESE);
    }
    public static final Language JAVANESE = new Language("Javanese", "basa Jawa", "jv", "jav", "jav");

    static {
        byIso639_1.put("jv", JAVANESE);
        byIso639_2.put("jav", JAVANESE);
        byIso639_3.put("jav", JAVANESE);
    }
    public static final Language GEORGIAN = new Language("Georgian", "\u10e5\u10d0\u10e0\u10d7\u10e3\u10da\u10d8", "ka", "kat", "kat");

    static {
        byIso639_1.put("ka", GEORGIAN);
        byIso639_2.put("kat", GEORGIAN);
        byIso639_3.put("kat", GEORGIAN);
    }
    public static final Language KONGO = new Language("Kongo", "KiKongo", "kg", "kon", "kon");

    static {
        byIso639_1.put("kg", KONGO);
        byIso639_2.put("kon", KONGO);
        byIso639_3.put("kon", KONGO);
    }
    public static final Language KIKUYU = new Language("Kikuyu", "G\u0129k\u0169y\u0169", "ki", "kik", "kik");

    static {
        byIso639_1.put("ki", KIKUYU);
        byIso639_2.put("kik", KIKUYU);
        byIso639_3.put("kik", KIKUYU);
    }
    public static final Language KWANYAMA = new Language("Kwanyama", "Kuanyama", "kj", "kua", "kua");

    static {
        byIso639_1.put("kj", KWANYAMA);
        byIso639_2.put("kua", KWANYAMA);
        byIso639_3.put("kua", KWANYAMA);
    }
    public static final Language KAZAKH = new Language("Kazakh", "\u049a\u0430\u0437\u0430\u049b \u0442\u0456\u043b\u0456", "kk", "kaz", "kaz");

    static {
        byIso639_1.put("kk", KAZAKH);
        byIso639_2.put("kaz", KAZAKH);
        byIso639_3.put("kaz", KAZAKH);
    }
    public static final Language KALAALLISUT = new Language("Kalaallisut", "Kalaallisut", "kl", "kal", "kal");

    static {
        byIso639_1.put("kl", KALAALLISUT);
        byIso639_2.put("kal", KALAALLISUT);
        byIso639_3.put("kal", KALAALLISUT);
    }
    public static final Language KHMER = new Language("Khmer", "\u1797\u17b6\u179f\u17b6\u1781\u17d2\u1798\u17c2\u179a", "km", "khm", "khm");

    static {
        byIso639_1.put("km", KHMER);
        byIso639_2.put("khm", KHMER);
        byIso639_3.put("khm", KHMER);
    }
    public static final Language KANNADA = new Language("Kannada", "\u0c95\u0ca8\u0ccd\u0ca8\u0ca1", "kn", "kan", "kan");

    static {
        byIso639_1.put("kn", KANNADA);
        byIso639_2.put("kan", KANNADA);
        byIso639_3.put("kan", KANNADA);
    }
    public static final Language KOREAN = new Language("Korean", "\ud55c\uad6d\uc5b4", "ko", "kor", "kor");

    static {
        byIso639_1.put("ko", KOREAN);
        byIso639_2.put("kor", KOREAN);
        byIso639_3.put("kor", KOREAN);
    }
    public static final Language KANURI = new Language("Kanuri", "Kanuri", "kr", "kau", "kau");

    static {
        byIso639_1.put("kr", KANURI);
        byIso639_2.put("kau", KANURI);
        byIso639_3.put("kau", KANURI);
    }
    public static final Language KASHMIRI = new Language("Kashmiri", "\u0915\u0936\u094d\u092e\u0940\u0930\u0940\u200e", "ks", "kas", "kas");

    static {
        byIso639_1.put("ks", KASHMIRI);
        byIso639_2.put("kas", KASHMIRI);
        byIso639_3.put("kas", KASHMIRI);
    }
    public static final Language KURDISH = new Language("Kurdish", "\u200e", "ku", "kur", "kur");

    static {
        byIso639_1.put("ku", KURDISH);
        byIso639_2.put("kur", KURDISH);
        byIso639_3.put("kur", KURDISH);
    }
    public static final Language KOMI = new Language("Komi", "\u043a\u043e\u043c\u0438 \u043a\u044b\u0432", "kv", "kom", "kom");

    static {
        byIso639_1.put("kv", KOMI);
        byIso639_2.put("kom", KOMI);
        byIso639_3.put("kom", KOMI);
    }
    public static final Language KIRGHIZ = new Language("Kirghiz", "\u043a\u044b\u0440\u0433\u044b\u0437 \u0442\u0438\u043b\u0438", "ky", "kir", "kir");

    static {
        byIso639_1.put("ky", KIRGHIZ);
        byIso639_2.put("kir", KIRGHIZ);
        byIso639_3.put("kir", KIRGHIZ);
    }
    public static final Language LATIN = new Language("Latin", "Latine", "la", "lat", "lat");

    static {
        byIso639_1.put("la", LATIN);
        byIso639_2.put("lat", LATIN);
        byIso639_3.put("lat", LATIN);
    }
    public static final Language LUXEMBOURGISH = new Language("Luxembourgish", "L\u00ebtzebuergesch", "lb", "ltz", "ltz");

    static {
        byIso639_1.put("lb", LUXEMBOURGISH);
        byIso639_2.put("ltz", LUXEMBOURGISH);
        byIso639_3.put("ltz", LUXEMBOURGISH);
    }
    public static final Language LUGANDA = new Language("Luganda", "Luganda", "lg", "lug", "lug");

    static {
        byIso639_1.put("lg", LUGANDA);
        byIso639_2.put("lug", LUGANDA);
        byIso639_3.put("lug", LUGANDA);
    }
    public static final Language LIMBURGISH = new Language("Limburgish", "Limburgs", "li", "lim", "lim");

    static {
        byIso639_1.put("li", LIMBURGISH);
        byIso639_2.put("lim", LIMBURGISH);
        byIso639_3.put("lim", LIMBURGISH);
    }
    public static final Language LINGALA = new Language("Lingala", "Ling\u00e1la", "ln", "lin", "lin");

    static {
        byIso639_1.put("ln", LINGALA);
        byIso639_2.put("lin", LINGALA);
        byIso639_3.put("lin", LINGALA);
    }
    public static final Language LAO = new Language("Lao", "\u0e9e\u0eb2\u0eaa\u0eb2\u0ea5\u0eb2\u0ea7", "lo", "lao", "lao");

    static {
        byIso639_1.put("lo", LAO);
        byIso639_2.put("lao", LAO);
        byIso639_3.put("lao", LAO);
    }
    public static final Language LITHUANIAN = new Language("Lithuanian", "Lietuvi\u0173 kalba", "lt", "lit", "lit");

    static {
        byIso639_1.put("lt", LITHUANIAN);
        byIso639_2.put("lit", LITHUANIAN);
        byIso639_3.put("lit", LITHUANIAN);
    }
    public static final Language LUBA_KATANGA = new Language("Luba-Katanga", null, "lu", "lub", "lub");

    static {
        byIso639_1.put("lu", LUBA_KATANGA);
        byIso639_2.put("lub", LUBA_KATANGA);
        byIso639_3.put("lub", LUBA_KATANGA);
    }
    public static final Language LATVIAN = new Language("Latvian", "Latvie\u0161u valoda", "lv", "lav", "lav");

    static {
        byIso639_1.put("lv", LATVIAN);
        byIso639_2.put("lav", LATVIAN);
        byIso639_3.put("lav", LATVIAN);
    }
    public static final Language MALAGASY = new Language("Malagasy", "Malagasy fiteny", "mg", "mlg", "mlg");

    static {
        byIso639_1.put("mg", MALAGASY);
        byIso639_2.put("mlg", MALAGASY);
        byIso639_3.put("mlg", MALAGASY);
    }
    public static final Language MARSHALLESE = new Language("Marshallese", "Kajin M\u0327aje\u013c", "mh", "mah", "mah");

    static {
        byIso639_1.put("mh", MARSHALLESE);
        byIso639_2.put("mah", MARSHALLESE);
        byIso639_3.put("mah", MARSHALLESE);
    }
    public static final Language MANX = new Language("Manx", "Gaelg", "gv", "glv", "glv");

    static {
        byIso639_1.put("gv", MANX);
        byIso639_2.put("glv", MANX);
        byIso639_3.put("glv", MANX);
    }
    public static final Language MAORI = new Language("M\u0101ori", "te reo M\u0101ori", "mi", "mri", "mri");

    static {
        byIso639_1.put("mi", MAORI);
        byIso639_2.put("mri", MAORI);
        byIso639_3.put("mri", MAORI);
    }
    public static final Language MACEDONIAN = new Language("Macedonian", "\u043c\u0430\u043a\u0435\u0434\u043e\u043d\u0441\u043a\u0438 \u0458\u0430\u0437\u0438\u043a", "mk", "mkd", "mkd");

    static {
        byIso639_1.put("mk", MACEDONIAN);
        byIso639_2.put("mkd", MACEDONIAN);
        byIso639_3.put("mkd", MACEDONIAN);
    }
    public static final Language MALAYALAM = new Language("Malayalam", "\u0d2e\u0d32\u0d2f\u0d3e\u0d33\u0d02", "ml", "mal", "mal");

    static {
        byIso639_1.put("ml", MALAYALAM);
        byIso639_2.put("mal", MALAYALAM);
        byIso639_3.put("mal", MALAYALAM);
    }
    public static final Language MONGOLIAN = new Language("Mongolian", "\u041c\u043e\u043d\u0433\u043e\u043b", "mn", "mon", "mon");

    static {
        byIso639_1.put("mn", MONGOLIAN);
        byIso639_2.put("mon", MONGOLIAN);
        byIso639_3.put("mon", MONGOLIAN);
    }
    public static final Language MARATHI = new Language("Marathi", "\u092e\u0930\u093e\u0920\u0940", "mr", "mar", "mar");

    static {
        byIso639_1.put("mr", MARATHI);
        byIso639_2.put("mar", MARATHI);
        byIso639_3.put("mar", MARATHI);
    }
    public static final Language MALAY = new Language("Malay", "\u200e", "ms", "msa", "msa");

    static {
        byIso639_1.put("ms", MALAY);
        byIso639_2.put("msa", MALAY);
        byIso639_3.put("msa", MALAY);
    }
    public static final Language MALTESE = new Language("Maltese", "Malti", "mt", "mlt", "mlt");

    static {
        byIso639_1.put("mt", MALTESE);
        byIso639_2.put("mlt", MALTESE);
        byIso639_3.put("mlt", MALTESE);
    }
    public static final Language NAURU = new Language("Nauru", "Ekakair\u0169 Naoero", "na", "nau", "nau");

    static {
        byIso639_1.put("na", NAURU);
        byIso639_2.put("nau", NAURU);
        byIso639_3.put("nau", NAURU);
    }
    public static final Language NORWEGIAN_BOKMAL = new Language("Norwegian Bokm\u00e5l", "Norsk bokm\u00e5l", "nb", "nob", "nob");

    static {
        byIso639_1.put("nb", NORWEGIAN_BOKMAL);
        byIso639_2.put("nob", NORWEGIAN_BOKMAL);
        byIso639_3.put("nob", NORWEGIAN_BOKMAL);
    }
    public static final Language NORTH_NDEBELE = new Language("North Ndebele", "isiNdebele", "nd", "nde", "nde");

    static {
        byIso639_1.put("nd", NORTH_NDEBELE);
        byIso639_2.put("nde", NORTH_NDEBELE);
        byIso639_3.put("nde", NORTH_NDEBELE);
    }
    public static final Language NEPALI = new Language("Nepali", "\u0928\u0947\u092a\u093e\u0932\u0940", "ne", "nep", "nep");

    static {
        byIso639_1.put("ne", NEPALI);
        byIso639_2.put("nep", NEPALI);
        byIso639_3.put("nep", NEPALI);
    }
    public static final Language NDONGA = new Language("Ndonga", "Owambo", "ng", "ndo", "ndo");

    static {
        byIso639_1.put("ng", NDONGA);
        byIso639_2.put("ndo", NDONGA);
        byIso639_3.put("ndo", NDONGA);
    }
    public static final Language DUTCH = new Language("Dutch", "Nederlands", "nl", "nld", "nld");

    static {
        byIso639_1.put("nl", DUTCH);
        byIso639_2.put("nld", DUTCH);
        byIso639_3.put("nld", DUTCH);
    }
    public static final Language NORWEGIAN_NYNORSK = new Language("Norwegian Nynorsk", "Norsk nynorsk", "nn", "nno", "nno");

    static {
        byIso639_1.put("nn", NORWEGIAN_NYNORSK);
        byIso639_2.put("nno", NORWEGIAN_NYNORSK);
        byIso639_3.put("nno", NORWEGIAN_NYNORSK);
    }
    public static final Language NORWEGIAN = new Language("Norwegian", "Norsk", "no", "nor", "nor");

    static {
        byIso639_1.put("no", NORWEGIAN);
        byIso639_2.put("nor", NORWEGIAN);
        byIso639_3.put("nor", NORWEGIAN);
    }
    public static final Language SOUTH_NDEBELE = new Language("South Ndebele", "isiNdebele", "nr", "nbl", "nbl");

    static {
        byIso639_1.put("nr", SOUTH_NDEBELE);
        byIso639_2.put("nbl", SOUTH_NDEBELE);
        byIso639_3.put("nbl", SOUTH_NDEBELE);
    }
    public static final Language NAVAJO = new Language("Navajo", "Din\u00e9 bizaad, Din\u00e9k\u02bceh\u01f0\u00ed", "nv", "nav", "nav");

    static {
        byIso639_1.put("nv", NAVAJO);
        byIso639_2.put("nav", NAVAJO);
        byIso639_3.put("nav", NAVAJO);
    }
    public static final Language OCCITAN = new Language("Occitan", "Occitan", "oc", "oci", "oci");

    static {
        byIso639_1.put("oc", OCCITAN);
        byIso639_2.put("oci", OCCITAN);
        byIso639_3.put("oci", OCCITAN);
    }
    public static final Language OJIBWA = new Language("Ojibwa", "\u140a\u14c2\u1511\u14c8\u142f\u14a7\u140e\u14d0", "oj", "oji", "oji");

    static {
        byIso639_1.put("oj", OJIBWA);
        byIso639_2.put("oji", OJIBWA);
        byIso639_3.put("oji", OJIBWA);
    }
    public static final Language OLD_CHURCH_SLAVONIC = new Language("Old Church Slavonic", "\u0469\u0437\u044b\u043a\u044a \u0441\u043b\u043e\u0432\u0463\u043d\u044c\u0441\u043a\u044a", "cu", "chu", "chu");

    static {
        byIso639_1.put("cu", OLD_CHURCH_SLAVONIC);
        byIso639_2.put("chu", OLD_CHURCH_SLAVONIC);
        byIso639_3.put("chu", OLD_CHURCH_SLAVONIC);
    }
    public static final Language OROMO = new Language("Oromo", "Afaan Oromoo", "om", "orm", "orm");

    static {
        byIso639_1.put("om", OROMO);
        byIso639_2.put("orm", OROMO);
        byIso639_3.put("orm", OROMO);
    }
    public static final Language ORIYA = new Language("Oriya", "\u0b13\u0b21\u0b3c\u0b3f\u0b06", "or", "ori", "ori");

    static {
        byIso639_1.put("or", ORIYA);
        byIso639_2.put("ori", ORIYA);
        byIso639_3.put("ori", ORIYA);
    }
    public static final Language OSSETIAN = new Language("Ossetian", "\u0418\u0440\u043e\u043d \u00e6\u0432\u0437\u0430\u0433", "os", "oss", "oss");

    static {
        byIso639_1.put("os", OSSETIAN);
        byIso639_2.put("oss", OSSETIAN);
        byIso639_3.put("oss", OSSETIAN);
    }
    public static final Language PANJABI = new Language("Panjabi", "\u200e\u0a2a\u0a70\u0a1c\u0a3e\u0a2c\u0a40", "pa", "pan", "pan");

    static {
        byIso639_1.put("pa", PANJABI);
        byIso639_2.put("pan", PANJABI);
        byIso639_3.put("pan", PANJABI);
    }
    public static final Language PALI = new Language("P\u0101li", "\u092a\u093e\u0934\u093f", "pi", "pli", "pli");

    static {
        byIso639_1.put("pi", PALI);
        byIso639_2.put("pli", PALI);
        byIso639_3.put("pli", PALI);
    }
    public static final Language PERSIAN = new Language("Persian", "\u0641\u0627\u0631\u0633\u06cc", "fa", "fas", "fas");

    static {
        byIso639_1.put("fa", PERSIAN);
        byIso639_2.put("fas", PERSIAN);
        byIso639_3.put("fas", PERSIAN);
    }
    public static final Language POLISH = new Language("Polish", "Polski", "pl", "pol", "pol");

    static {
        byIso639_1.put("pl", POLISH);
        byIso639_2.put("pol", POLISH);
        byIso639_3.put("pol", POLISH);
    }
    public static final Language PASHTO = new Language("Pashto", "\u067e\u069a\u062a\u0648", "ps", "pus", "pus");

    static {
        byIso639_1.put("ps", PASHTO);
        byIso639_2.put("pus", PASHTO);
        byIso639_3.put("pus", PASHTO);
    }
    public static final Language PORTUGUESE = new Language("Portuguese", "Portugu\u00eas", "pt", "por", "por");

    static {
        byIso639_1.put("pt", PORTUGUESE);
        byIso639_2.put("por", PORTUGUESE);
        byIso639_3.put("por", PORTUGUESE);
    }
    public static final Language QUECHUA = new Language("Quechua", "Runa Simi", "qu", "que", "que");

    static {
        byIso639_1.put("qu", QUECHUA);
        byIso639_2.put("que", QUECHUA);
        byIso639_3.put("que", QUECHUA);
    }
    public static final Language ROMANSH = new Language("Romansh", "Rumantsch grischun", "rm", "roh", "roh");

    static {
        byIso639_1.put("rm", ROMANSH);
        byIso639_2.put("roh", ROMANSH);
        byIso639_3.put("roh", ROMANSH);
    }
    public static final Language KIRUNDI = new Language("Kirundi", "kiRundi", "rn", "run", "run");

    static {
        byIso639_1.put("rn", KIRUNDI);
        byIso639_2.put("run", KIRUNDI);
        byIso639_3.put("run", KIRUNDI);
    }
    public static final Language ROMANIAN = new Language("Romanian", "Rom\u00e2n\u0103", "ro", "ron", "ron");

    static {
        byIso639_1.put("ro", ROMANIAN);
        byIso639_2.put("ron", ROMANIAN);
        byIso639_3.put("ron", ROMANIAN);
    }
    public static final Language RUSSIAN = new Language("Russian", "\u0420\u0443\u0441\u0441\u043a\u0438\u0439 \u044f\u0437\u044b\u043a", "ru", "rus", "rus");

    static {
        byIso639_1.put("ru", RUSSIAN);
        byIso639_2.put("rus", RUSSIAN);
        byIso639_3.put("rus", RUSSIAN);
    }
    public static final Language KINYARWANDA = new Language("Kinyarwanda", "Ikinyarwanda", "rw", "kin", "kin");

    static {
        byIso639_1.put("rw", KINYARWANDA);
        byIso639_2.put("kin", KINYARWANDA);
        byIso639_3.put("kin", KINYARWANDA);
    }
    public static final Language SANSKRIT = new Language("Sanskrit", "\u0938\u0902\u0938\u094d\u0915\u0943\u0924\u092e\u094d", "sa", "san", "san");

    static {
        byIso639_1.put("sa", SANSKRIT);
        byIso639_2.put("san", SANSKRIT);
        byIso639_3.put("san", SANSKRIT);
    }
    public static final Language SARDINIAN = new Language("Sardinian", "Sardu", "sc", "srd", "srd");

    static {
        byIso639_1.put("sc", SARDINIAN);
        byIso639_2.put("srd", SARDINIAN);
        byIso639_3.put("srd", SARDINIAN);
    }
    public static final Language SINDHI = new Language("Sindhi", "\u200e\u0938\u093f\u0928\u094d\u0927\u0940", "sd", "snd", "snd");

    static {
        byIso639_1.put("sd", SINDHI);
        byIso639_2.put("snd", SINDHI);
        byIso639_3.put("snd", SINDHI);
    }
    public static final Language NORTHERN_SAMI = new Language("Northern Sami", "Davvis\u00e1megiella", "se", "sme", "sme");

    static {
        byIso639_1.put("se", NORTHERN_SAMI);
        byIso639_2.put("sme", NORTHERN_SAMI);
        byIso639_3.put("sme", NORTHERN_SAMI);
    }
    public static final Language SAMOAN = new Language("Samoan", "gagana fa'a Samoa", "sm", "smo", "smo");

    static {
        byIso639_1.put("sm", SAMOAN);
        byIso639_2.put("smo", SAMOAN);
        byIso639_3.put("smo", SAMOAN);
    }
    public static final Language SANGO = new Language("Sango", "Y\u00e2ng\u00e2 t\u00ee s\u00e4ng\u00f6", "sg", "sag", "sag");

    static {
        byIso639_1.put("sg", SANGO);
        byIso639_2.put("sag", SANGO);
        byIso639_3.put("sag", SANGO);
    }
    public static final Language SERBIAN = new Language("Serbian", "\u0441\u0440\u043f\u0441\u043a\u0438 \u0458\u0435\u0437\u0438\u043a", "sr", "srp", "srp");

    static {
        byIso639_1.put("sr", SERBIAN);
        byIso639_2.put("srp", SERBIAN);
        byIso639_3.put("srp", SERBIAN);
    }
    public static final Language SCOTTISH_GAELIC = new Language("Scottish Gaelic", "G\u00e0idhlig", "gd", "gla", "gla");

    static {
        byIso639_1.put("gd", SCOTTISH_GAELIC);
        byIso639_2.put("gla", SCOTTISH_GAELIC);
        byIso639_3.put("gla", SCOTTISH_GAELIC);
    }
    public static final Language SHONA = new Language("Shona", "chiShona", "sn", "sna", "sna");

    static {
        byIso639_1.put("sn", SHONA);
        byIso639_2.put("sna", SHONA);
        byIso639_3.put("sna", SHONA);
    }
    public static final Language SINHALA = new Language("Sinhala", "\u0dc3\u0dd2\u0d82\u0dc4\u0dbd", "si", "sin", "sin");

    static {
        byIso639_1.put("si", SINHALA);
        byIso639_2.put("sin", SINHALA);
        byIso639_3.put("sin", SINHALA);
    }
    public static final Language SLOVAK = new Language("Slovak", "Sloven\u010dina", "sk", "slk", "slk");

    static {
        byIso639_1.put("sk", SLOVAK);
        byIso639_2.put("slk", SLOVAK);
        byIso639_3.put("slk", SLOVAK);
    }
    public static final Language SLOVENE = new Language("Slovene", "Sloven\u0161\u010dina", "sl", "slv", "slv");

    static {
        byIso639_1.put("sl", SLOVENE);
        byIso639_2.put("slv", SLOVENE);
        byIso639_3.put("slv", SLOVENE);
    }
    public static final Language SOMALI = new Language("Somali", "Soomaaliga, af Soomaali", "so", "som", "som");

    static {
        byIso639_1.put("so", SOMALI);
        byIso639_2.put("som", SOMALI);
        byIso639_3.put("som", SOMALI);
    }
    public static final Language SOUTHERN_SOTHO = new Language("Southern Sotho", "Sesotho", "st", "sot", "sot");

    static {
        byIso639_1.put("st", SOUTHERN_SOTHO);
        byIso639_2.put("sot", SOUTHERN_SOTHO);
        byIso639_3.put("sot", SOUTHERN_SOTHO);
    }
    public static final Language SPANISH = new Language("Spanish", "Espa\u00f1ol", "es", "spa", "spa");

    static {
        byIso639_1.put("es", SPANISH);
        byIso639_2.put("spa", SPANISH);
        byIso639_3.put("spa", SPANISH);
    }
    public static final Language SUNDANESE = new Language("Sundanese", "Basa Sunda", "su", "sun", "sun");

    static {
        byIso639_1.put("su", SUNDANESE);
        byIso639_2.put("sun", SUNDANESE);
        byIso639_3.put("sun", SUNDANESE);
    }
    public static final Language SWAHILI = new Language("Swahili", "Kiswahili", "sw", "swa", "swa");

    static {
        byIso639_1.put("sw", SWAHILI);
        byIso639_2.put("swa", SWAHILI);
        byIso639_3.put("swa", SWAHILI);
    }
    public static final Language SWATI = new Language("Swati", "SiSwati", "ss", "ssw", "ssw");

    static {
        byIso639_1.put("ss", SWATI);
        byIso639_2.put("ssw", SWATI);
        byIso639_3.put("ssw", SWATI);
    }
    public static final Language SWEDISH = new Language("Swedish", "Svenska", "sv", "swe", "swe");

    static {
        byIso639_1.put("sv", SWEDISH);
        byIso639_2.put("swe", SWEDISH);
        byIso639_3.put("swe", SWEDISH);
    }
    public static final Language TAMIL = new Language("Tamil", "\u0ba4\u0bae\u0bbf\u0bb4\u0bcd", "ta", "tam", "tam");

    static {
        byIso639_1.put("ta", TAMIL);
        byIso639_2.put("tam", TAMIL);
        byIso639_3.put("tam", TAMIL);
    }
    public static final Language TELUGU = new Language("Telugu", "\u0c24\u0c46\u0c32\u0c41\u0c17\u0c41", "te", "tel", "tel");

    static {
        byIso639_1.put("te", TELUGU);
        byIso639_2.put("tel", TELUGU);
        byIso639_3.put("tel", TELUGU);
    }
    public static final Language TAJIK = new Language("Tajik", "\u200e\u0442\u043e\u04b7\u0438\u043a\u04e3", "tg", "tgk", "tgk");

    static {
        byIso639_1.put("tg", TAJIK);
        byIso639_2.put("tgk", TAJIK);
        byIso639_3.put("tgk", TAJIK);
    }
    public static final Language THAI = new Language("Thai", "\u0e44\u0e17\u0e22", "th", "tha", "tha");

    static {
        byIso639_1.put("th", THAI);
        byIso639_2.put("tha", THAI);
        byIso639_3.put("tha", THAI);
    }
    public static final Language TIGRINYA = new Language("Tigrinya", "\u1275\u130d\u122d\u129b", "ti", "tir", "tir");

    static {
        byIso639_1.put("ti", TIGRINYA);
        byIso639_2.put("tir", TIGRINYA);
        byIso639_3.put("tir", TIGRINYA);
    }
    public static final Language TIBETAN_STANDARD = new Language("Tibetan Standard", "\u0f56\u0f7c\u0f51\u0f0b\u0f61\u0f72\u0f42", "bo", "bod", "bod");

    static {
        byIso639_1.put("bo", TIBETAN_STANDARD);
        byIso639_2.put("bod", TIBETAN_STANDARD);
        byIso639_3.put("bod", TIBETAN_STANDARD);
    }
    public static final Language TURKMEN = new Language("Turkmen", "T\u00fcrkmen", "tk", "tuk", "tuk");

    static {
        byIso639_1.put("tk", TURKMEN);
        byIso639_2.put("tuk", TURKMEN);
        byIso639_3.put("tuk", TURKMEN);
    }
    public static final Language TAGALOG = new Language("Tagalog", "Wikang Tagalog", "tl", "tgl", "tgl");

    static {
        byIso639_1.put("tl", TAGALOG);
        byIso639_2.put("tgl", TAGALOG);
        byIso639_3.put("tgl", TAGALOG);
    }
    public static final Language TSWANA = new Language("Tswana", "Setswana", "tn", "tsn", "tsn");

    static {
        byIso639_1.put("tn", TSWANA);
        byIso639_2.put("tsn", TSWANA);
        byIso639_3.put("tsn", TSWANA);
    }
    public static final Language TONGA = new Language("Tonga", "faka Tonga", "to", "ton", "ton");

    static {
        byIso639_1.put("to", TONGA);
        byIso639_2.put("ton", TONGA);
        byIso639_3.put("ton", TONGA);
    }
    public static final Language TURKISH = new Language("Turkish", "T\u00fcrk\u00e7e", "tr", "tur", "tur");

    static {
        byIso639_1.put("tr", TURKISH);
        byIso639_2.put("tur", TURKISH);
        byIso639_3.put("tur", TURKISH);
    }
    public static final Language TSONGA = new Language("Tsonga", "Xitsonga", "ts", "tso", "tso");

    static {
        byIso639_1.put("ts", TSONGA);
        byIso639_2.put("tso", TSONGA);
        byIso639_3.put("tso", TSONGA);
    }
    public static final Language TATAR = new Language("Tatar", "\u200e\u0442\u0430\u0442\u0430\u0440\u0447\u0430", "tt", "tat", "tat");

    static {
        byIso639_1.put("tt", TATAR);
        byIso639_2.put("tat", TATAR);
        byIso639_3.put("tat", TATAR);
    }
    public static final Language TWI = new Language("Twi", "Twi", "tw", "twi", "twi");

    static {
        byIso639_1.put("tw", TWI);
        byIso639_2.put("twi", TWI);
        byIso639_3.put("twi", TWI);
    }
    public static final Language TAHITIAN = new Language("Tahitian", "Reo M\u0101`ohi", "ty", "tah", "tah");

    static {
        byIso639_1.put("ty", TAHITIAN);
        byIso639_2.put("tah", TAHITIAN);
        byIso639_3.put("tah", TAHITIAN);
    }
    public static final Language UIGHUR = new Language("Uighur", "Uy\u01a3urq\u0259\u200e", "ug", "uig", "uig");

    static {
        byIso639_1.put("ug", UIGHUR);
        byIso639_2.put("uig", UIGHUR);
        byIso639_3.put("uig", UIGHUR);
    }
    public static final Language UKRAINIAN = new Language("Ukrainian", "\u0423\u043a\u0440\u0430\u0457\u043d\u0441\u044c\u043a\u0430", "uk", "ukr", "ukr");

    static {
        byIso639_1.put("uk", UKRAINIAN);
        byIso639_2.put("ukr", UKRAINIAN);
        byIso639_3.put("ukr", UKRAINIAN);
    }
    public static final Language URDU = new Language("Urdu", "\u0627\u0631\u062f\u0648", "ur", "urd", "urd");

    static {
        byIso639_1.put("ur", URDU);
        byIso639_2.put("urd", URDU);
        byIso639_3.put("urd", URDU);
    }
    public static final Language UZBEK = new Language("Uzbek", "\u200eO'zbek", "uz", "uzb", "uzb");

    static {
        byIso639_1.put("uz", UZBEK);
        byIso639_2.put("uzb", UZBEK);
        byIso639_3.put("uzb", UZBEK);
    }
    public static final Language VENDA = new Language("Venda", "Tshiven\u1e13a", "ve", "ven", "ven");

    static {
        byIso639_1.put("ve", VENDA);
        byIso639_2.put("ven", VENDA);
        byIso639_3.put("ven", VENDA);
    }
    public static final Language VIETNAMESE = new Language("Vietnamese", "Ti\u1ebfng Vi\u1ec7t", "vi", "vie", "vie");

    static {
        byIso639_1.put("vi", VIETNAMESE);
        byIso639_2.put("vie", VIETNAMESE);
        byIso639_3.put("vie", VIETNAMESE);
    }
    public static final Language VOLAPUK = new Language("Volap\u00fck", "Volap\u00fck", "vo", "vol", "vol");

    static {
        byIso639_1.put("vo", VOLAPUK);
        byIso639_2.put("vol", VOLAPUK);
        byIso639_3.put("vol", VOLAPUK);
    }
    public static final Language WALLOON = new Language("Walloon", "Walon", "wa", "wln", "wln");

    static {
        byIso639_1.put("wa", WALLOON);
        byIso639_2.put("wln", WALLOON);
        byIso639_3.put("wln", WALLOON);
    }
    public static final Language WELSH = new Language("Welsh", "Cymraeg", "cy", "cym", "cym");

    static {
        byIso639_1.put("cy", WELSH);
        byIso639_2.put("cym", WELSH);
        byIso639_3.put("cym", WELSH);
    }
    public static final Language WOLOF = new Language("Wolof", "Wollof", "wo", "wol", "wol");

    static {
        byIso639_1.put("wo", WOLOF);
        byIso639_2.put("wol", WOLOF);
        byIso639_3.put("wol", WOLOF);
    }
    public static final Language WESTERN_FRISIAN = new Language("Western Frisian", "Frysk", "fy", "fry", "fry");

    static {
        byIso639_1.put("fy", WESTERN_FRISIAN);
        byIso639_2.put("fry", WESTERN_FRISIAN);
        byIso639_3.put("fry", WESTERN_FRISIAN);
    }
    public static final Language XHOSA = new Language("Xhosa", "isiXhosa", "xh", "xho", "xho");

    static {
        byIso639_1.put("xh", XHOSA);
        byIso639_2.put("xho", XHOSA);
        byIso639_3.put("xho", XHOSA);
    }
    public static final Language YIDDISH = new Language("Yiddish", "\u05d9\u05d9\u05b4\u05d3\u05d9\u05e9", "yi", "yid", "yid");

    static {
        byIso639_1.put("yi", YIDDISH);
        byIso639_2.put("yid", YIDDISH);
        byIso639_3.put("yid", YIDDISH);
    }
    public static final Language YORUBA = new Language("Yoruba", "Yor\u00f9b\u00e1", "yo", "yor", "yor");

    static {
        byIso639_1.put("yo", YORUBA);
        byIso639_2.put("yor", YORUBA);
        byIso639_3.put("yor", YORUBA);
    }
    public static final Language ZHUANG = new Language("Zhuang", "Sa\u026f cue\u014b\u0185", "za", "zha", "zha");

    static {
        byIso639_1.put("za", ZHUANG);
        byIso639_2.put("zha", ZHUANG);
        byIso639_3.put("zha", ZHUANG);
    }
    public static final Language ZULU = new Language("Zulu", "isiZulu", "zu", "zul", "zul");

    static {
        byIso639_1.put("zu", ZULU);
        byIso639_2.put("zul", ZULU);
        byIso639_3.put("zul", ZULU);
    }

    /**
     * Get the English name of the language
     */
    public String getName() {
        return name;
    }

    /**
     * Get the name of the language in that language
     */
    public String getNativeName() {
        return nativeName;
    }

    /**
     * Get the ISO 639-1 code. For example, English = "en", German = "de",
     * Japanese = "ja"
     *
     * @return The code or null if there is no code for this language
     */
    public String getIso639_1() {
        return iso639_1;
    }

    /**
     * Get the ISO 639-2 code. For example, English = "eng", German = "deu",
     * Japanese = "jpn"
     *
     * @return The code or null if there is no code for this language
     */
    public String getIso639_2() {
        return iso639_2;
    }

    /**
     * Get the ISO 639-3 code. This is generally the same as the ISO 639-2 code
     * if that exists
     *
     * @return The code
     */
    public String getIso639_3() {
        return iso639_3;
    }

    /**
     * Get the region, if specified
     *
     * @return The region or null if no regional variant is specified
     */
    public Region getRegion() {
        return region;
    }

    /**
     * Get the script var
     *
     * @return The script or null if no script is specified
     */
    public Script getScript() {
        return script;
    }

    /**
     * Get the registered variants
     *
     * @return A non-null list of variants
     */
    public List<String> getVariants() {
        return variant;
    }

    /**
     * Get the registered extensions
     *
     * @return A non-null list of extensions
     */
    public List<String> getExtensions() {
        return extension;
    }

    /**
     * Get the private use description
     *
     * @return The private use description or null if no private use description
     * is specified
     */
    public String getPrivateUse() {
        return privateUse;
    }

    /**
     * Get an instance of a language by code
     *
     * @param code The code (as either a 2-letter ISO 639-1 code or a 3-letter
     * ISO 639-2 code)
     * @return The language
     * @throws LanguageCodeFormatException If the language code was not
     * correctly formatted
     */
    public static Language get(String code) {
        if (code == null) {
            throw new IllegalArgumentException("Language code was null");
        }
        Matcher m = Pattern.compile(("(...?)(-[A-Za-z]{4})?(-[A-Za-z]{2}|-[0-9]{3})?((-[A-Za-z0-9]{5,8}|-[0-9][A-Za-z0-9]{3})*)((-[A-WY-Za-wy-z0-9]-\\w{2,8})*)(-[Xx]-\\w{1,8})?")).matcher(code);
        if (!m.matches()) {
            throw new LanguageCodeFormatException(code);
        }
        String lang = m.group(1);
        Language rval;
        if (lang.length() == 2) {
            rval = getByIso639_1(m.group(1));
        } else if (lang.length() == 3) {
            rval = getByIso639_2(m.group(1));
        } else {
            throw new LanguageCodeFormatException(code);
        }
        Region region = null;
        Script script = null;
        List<String> variants = new LinkedList<String>();
        List<String> extensions = new LinkedList<String>();
        String privateUse = null;
        for (int i = 2; i < 4; i++) {
            if (m.group(i) != null) {
                if (m.group(i).length() == 3) {

                    if (region != null) {
                        throw new LanguageCodeFormatException(code);
                    }
                    region = Region.getByAlpha2Code(m.group(i).substring(1));
                }
                if (m.group(i).length() == 5) {

                    if (script != null) {
                        throw new LanguageCodeFormatException(code);
                    }
                    script = Script.getByAlpha4Code(m.group(i).substring(1));
                }
                if (m.group(i).length() == 4) {
                    if (m.group(i).matches("-[0-9]+")) {
                        Integer numericCode = Integer.parseInt(m.group(i).substring(1));
                        region = Region.getByNumericCode(numericCode);
                    } else {
                        if (region != null) {
                            throw new LanguageCodeFormatException(code);
                        }
                        region = Region.getByAlpha3Code(m.group(i).substring(1));
                    }
                }
            }
        }
        if (m.group(4) != null) {
            variants = new LinkedList<String>();
            String[] vars = m.group(4).split("-");
            for (int i = 1; i < vars.length; i++) {
                variants.add(vars[i]);
            }
        }
        if (m.group(6) != null) {
            extensions = new LinkedList<String>();
            String[] vars = m.group(6).split("-");
            for (int i = 1; i < vars.length; i += 2) {
                extensions.add(vars[i] + "-" + vars[i + 1]);
            }
        }
        if (m.group(8) != null) {
            privateUse = m.group(8).substring(1);
        }

        if (script == null && region == null && variants == Collections.EMPTY_LIST && extensions == Collections.EMPTY_LIST && privateUse == null) {
            return rval;
        }
        return new Language(rval.name, rval.nativeName, rval.iso639_1, rval.iso639_2, rval.iso639_3, region, script, variants, extensions, privateUse);
    }

    /**
     * Get a language by its ISO 639-1 code
     *
     * @param code The code
     * @return The language or null if the language is not known
     */
    public static Language getByIso639_1(String code) {
        if (!byIso639_1.containsKey(code.toLowerCase())) {
            throw new LanguageCodeFormatException("Unknown ISO 639-1 language code:" + code);
        }
        return byIso639_1.get(code.toLowerCase());
    }

    /**
     * Get a language by its ISO 639-2 code
     *
     * @param code The code
     * @return The language or null if the language is not known
     */
    public static Language getByIso639_2(String code) {
        if (byIso639_2.containsKey(code.toLowerCase())) {
            return byIso639_2.get(code.toLowerCase());
        } else {
            return new Language(null, null, null, code.toLowerCase(), code.toLowerCase());
        }
    }

    /**
     * Get a language by its ISO 639-3 code
     *
     * @param code The code
     * @return The language or null if the language is not known
     */
    public static Language getByIso639_3(String code) {
        if (byIso639_3.containsKey(code.toLowerCase())) {
            return byIso639_3.get(code.toLowerCase());
        } else {
            return new Language(null, null, null, null, code.toLowerCase());
        }
    }

    
    /**
     * Get an instance. This will return the instance if the code is already
     * known
     *
     * @param name The English name
     * @param nativeName The name in that language
     * @param iso639_1
     * @param iso639_2
     * @param iso639_3
     * @throws IllegalArgumentException If the ISO 639-1 is not exactly two
     * characters long or the ISO 639-2 and 3 codes are not 3 characters long or
     * the ISO 639-1 code is known but does not match the static code known.
     * @return An instance of this language
     */
    public static Language getInstance(String name, String nativeName, String iso639_1, String iso639_2, String iso639_3) {
        if ((iso639_1 != null && iso639_1.length() != 2)
                || (iso639_2 != null && iso639_2.length() != 3)
                || iso639_3.length() != 3) {
            throw new IllegalArgumentException("Bad ISO-639 codes : " + iso639_1 + " " + iso639_2 + " " + iso639_3);
        }
        if (iso639_1 != null && byIso639_1.containsKey(iso639_1)) {
            final Language lang = byIso639_1.get(iso639_1);
            if (!lang.getIso639_2().equals(iso639_2) || !lang.getIso639_3().equals(iso639_3)) {
                throw new IllegalArgumentException("ISO-639 codes do not match known: "
                        + lang.getIso639_2() + "/" + iso639_2
                        + lang.getIso639_3() + "/" + iso639_3);
            }
            return lang;
        }
        if (iso639_2 != null && byIso639_2.containsKey(iso639_2)) {
            return byIso639_2.get(iso639_2);
        }
        if (iso639_3 != null && byIso639_3.containsKey(iso639_3)) {
            return byIso639_3.get(iso639_3);
        }
        return new Language(name, nativeName, iso639_1, iso639_2, iso639_3);
    }

    /**
     * Get a new instance that differs from this language by some regional
     * variant
     *
     * @param region The region
     * @return A new language instance
     */
    public Language getRegionalVariant(Region region) {
        return new Language(this.name, this.nativeName, this.iso639_1, this.iso639_2, this.iso639_3, region, this.script, variant, extension, privateUse);
    }

    /**
     * Get a new instance that differs from this language by using a different
     * script
     *
     * @param script The script
     * @return A new language instance
     */
    public Language getScriptVariant(Script script) {
        return new Language(name, nativeName, iso639_1, iso639_2, iso639_3, region, script, variant, extension, privateUse);
    }

    /**
     * Get a new instance that differs from this language by a registered
     * variant. See IANA registry for list of registered variants
     *
     * @param variant The variant
     * @return A new language instance
     */
    public Language getVariant(String variant) {
        if (!variant.matches("[A-Za-z0-9]{5,8}|[0-9][A-Za-z0-9]{3}")) {
            throw new IllegalArgumentException("Not a valid variant: " + variant);
        }
        List<String> variants = new LinkedList<String>();
        variants.addAll(this.variant);
        variants.add(variant);
        return new Language(this.name, this.nativeName, this.iso639_1, this.iso639_2, this.iso639_3, region, this.script, variants, extension, privateUse);
    }

    /**
     * Get a new instance that differs from this language by an extension.
     *
     * @param extension The extension
     * @return A new language instance
     */
    public Language getExtensionVariant(String extension) {
        if (!extension.matches("[A-WY-Za-wy-z0-9]-\\w{2,8}")) {
            throw new IllegalArgumentException("Not a valid extension: " + extension);
        }
        List<String> extensions = new LinkedList<String>();
        extensions.addAll(this.extension);
        extensions.add(extension);
        return new Language(this.name, this.nativeName, this.iso639_1, this.iso639_2, this.iso639_3, region, this.script, variant, extensions, privateUse);
    }

    /**
     * Get a new instance that differs from this language by a private use
     * description
     *
     * @param privateUse The private use description (staring -x-)
     * @return A new language instance
     */
    public Language getPrivateUseVariant(String privateUse) {
        if (!privateUse.matches("-[Xx]-\\w{1,8}")) {
            throw new IllegalArgumentException("Not a private use description: " + privateUse);
        }
        return new Language(this.name, this.nativeName, this.iso639_1, this.iso639_2, this.iso639_3, region, this.script, this.variant, this.extension, privateUse);
    }

    /**
     * Return this language without script,region, variant, extension or private
     * use information.
     */
    public Language getLanguageOnly() {
        if (iso639_1 != null) {
            return getByIso639_1(iso639_1);
        } else if (iso639_2 != null) {
            return getByIso639_2(iso639_2);
        } else {
            return getByIso639_3(iso639_3);
        }
    }

    /**
     * Convert this to a Java Locale.
     */
    public Locale toLocale() {
        if (region != null && script != null) {
            return new Locale(iso639_1, region.getAlpha2code(), script.getAlpha4code());
        } else if (region != null) {
            return new Locale(iso639_1, region.getAlpha2code());
        } else {
            return new Locale(iso639_1);
        }
    }

    /**
     * Convert this to a string representation following RFC 4646
     */
    @Override
    public String toString() {
        StringBuilder str = new StringBuilder();
        if (iso639_1 != null) {
            str.append(iso639_1);
        } else if (iso639_2 != null) {
            str.append(iso639_2);
        } else {
            str.append(iso639_3);
        }
        if (region != null) {
            str.append("-");
            str.append(region.toString());
        }
        if (script != null) {
            str.append("-");
            str.append(script.toString());
        }
        for (String v : variant) {
            str.append("-");
            str.append(v);
        }
        for (String x : extension) {
            str.append("-");
            str.append(x);
        }
        if (privateUse != null) {
            str.append("-");
            str.append(privateUse);
        }
        return str.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final Language other = (Language) obj;
        if ((this.iso639_1 == null) ? (other.iso639_1 != null) : !this.iso639_1.equals(other.iso639_1)) {
            return false;
        }
        if ((this.iso639_2 == null) ? (other.iso639_2 != null) : !this.iso639_2.equals(other.iso639_2)) {
            return false;
        }
        if ((this.iso639_3 == null) ? (other.iso639_3 != null) : !this.iso639_3.equals(other.iso639_3)) {
            return false;
        }
        if (this.region != other.region && (this.region == null || !this.region.equals(other.region))) {
            return false;
        }
        if (this.script != other.script && (this.script == null || !this.script.equals(other.script))) {
            return false;
        }
        if ((this.variant == null) ? (other.variant != null)
                : (!this.variant.equals(other.variant))) {
            return false;
        }
        if ((this.extension == null) ? (other.extension != null) : !this.extension.equals(other.extension)) {
            return false;
        }
        if (this.privateUse != other.privateUse && (this.privateUse == null || !this.privateUse.equals(other.privateUse))) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        int hash = 3;
        hash = 53 * hash + (this.iso639_1 != null ? this.iso639_1.hashCode() : 0);
        hash = 53 * hash + (this.iso639_2 != null ? this.iso639_2.hashCode() : 0);
        hash = 53 * hash + (this.iso639_3 != null ? this.iso639_3.hashCode() : 0);
        hash = 53 * hash + (this.region != null ? this.region.hashCode() : 0);
        hash = 53 * hash + (this.script != null ? this.script.hashCode() : 0);
        hash = 53 * hash + (this.variant != null ? this.variant.hashCode() : 0);
        hash = 53 * hash + (this.extension != null ? this.extension.hashCode() : 0);
        hash = 53 * hash + (this.privateUse != null ? this.privateUse.hashCode() : 0);
        return hash;
    }
}
