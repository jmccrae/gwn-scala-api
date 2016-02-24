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
import java.util.HashMap;

/**
 * Script codings. Based on ISO 15924 codes
 *
 * @author John McCrae
 */
public final class Script implements Serializable {

    private static final long serialVersionUID = 2121020421354178367L;
    private String alpha4code;
    private int numericCode;
    private final static HashMap<String, Script> byAlpha4 = new HashMap<String, Script>();
    private final static HashMap<Integer, Script> byNumeric = new HashMap<Integer, Script>();
    private final static HashMap<Language, Script[]> scriptsByLang = new HashMap<Language, Script[]>();

    /**
     * Create a new instance
     *
     * @param alpha4code The 4-letter alphabetic code
     * @param numericCode The numeric code
     */
    public Script(String alpha4code, int numericCode) {
        assert (alpha4code.length() == 4);
        assert (numericCode < 1000 && numericCode >= 0);
        this.alpha4code = alpha4code.substring(0, 1).toUpperCase() + alpha4code.substring(1).toLowerCase();
        this.numericCode = numericCode;
        if (!byAlpha4.containsKey(this.alpha4code)) {
            byAlpha4.put(this.alpha4code, this);
        }
        if (!byNumeric.containsKey(numericCode)) {
            byNumeric.put(this.numericCode, this);
        }
    }

    /**
     * Get 4-Letter alphabetic code
     */
    public String getAlpha4code() {
        return alpha4code;
    }

    /**
     * Get numeric code
     */
    public int getNumericCode() {
        return numericCode;
    }

    /**
     * Get script by 4-letter alphabetic code
     *
     * @param alpha4code The code
     */
    public static Script getByAlpha4Code(String alpha4code) {
        return byAlpha4.get(Character.toUpperCase(alpha4code.charAt(0)) + alpha4code.substring(1).toLowerCase());
    }

    /**
     * Get script by numeric code
     *
     * @param numericCode The code
     */
    public static Script getByNumericCode(int numericCode) {
        return byNumeric.get(numericCode);
    }

    @Override
    public String toString() {
        return alpha4code;
    }
    public static final Script ARABIC = new Script("Arab", 160);
    public static final Script IMPERIAL_ARAMAIC = new Script("Armi", 124);
    public static final Script ARMENIAN = new Script("Armn", 230);
    public static final Script AVESTAN = new Script("Avst", 134);
    public static final Script BALINESE = new Script("Bali", 360);
    public static final Script BAMUM = new Script("Bamu", 435);
    public static final Script BATAK = new Script("Batk", 365);
    public static final Script BENGALI = new Script("Beng", 325);
    public static final Script BLISSYMBOLS = new Script("Blis", 550);
    public static final Script BOPOMOFO = new Script("Bopo", 285);
    public static final Script BRAHMI = new Script("Brah", 300);
    public static final Script BRAILLE = new Script("Brai", 570);
    public static final Script BUGINESE = new Script("Bugi", 367);
    public static final Script BUHID = new Script("Buhd", 372);
    public static final Script CHAKMA = new Script("Cakm", 349);
    public static final Script UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS = new Script("Cans", 440);
    public static final Script CARIAN = new Script("Cari", 201);
    public static final Script CHAM = new Script("Cham", 358);
    public static final Script CHEROKEE = new Script("Cher", 445);
    public static final Script CIRTH = new Script("Cirt", 291);
    public static final Script COPTIC = new Script("Copt", 204);
    public static final Script CYPRIOT = new Script("Cprt", 403);
    public static final Script CYRILLIC = new Script("Cyrl", 220);
    public static final Script OLD_CHURCH_SLAVONIC_Cyrl = new Script("Cyrs", 221);
    public static final Script DEVANAGARI = new Script("Deva", 315);
    public static final Script DESERET = new Script("Dsrt", 250);
    public static final Script EGYPTIAN_DEMOTIC = new Script("Egyd", 70);
    public static final Script EGYPTIAN_HIERATIC = new Script("Egyh", 60);
    public static final Script EGYPTIAN_HIEROGLYPHS = new Script("Egyp", 50);
    public static final Script ETHIOPIC = new Script("Ethi", 430);
    public static final Script GEORGIAN = new Script("Geor", 240);
    public static final Script KHUTSURI = new Script("Geok", 241);
    public static final Script GLAGOLITIC = new Script("Glag", 225);
    public static final Script GOTHIC = new Script("Goth", 206);
    public static final Script GREEK = new Script("Grek", 200);
    public static final Script GUJARATI = new Script("Gujr", 320);
    public static final Script GURMUKHI = new Script("Guru", 310);
    public static final Script HANGUL = new Script("Hang", 286);
    public static final Script HAN = new Script("Hani", 500);
    public static final Script HANUNOO = new Script("Hano", 371);
    public static final Script SIMPLIFIED_HAN = new Script("Hans", 501);
    public static final Script TRADITIONAL_HAN = new Script("Hant", 502);
    public static final Script HEBREW = new Script("Hebr", 125);
    public static final Script HIRAGANA = new Script("Hira", 410);
    public static final Script PAHAWH_HMONG = new Script("Hmng", 450);
    public static final Script JAPANESE_KANA = new Script("Hrkt", 412);
    public static final Script OLD_HUNGARIAN = new Script("Hung", 176);
    public static final Script INDUS = new Script("Inds", 610);
    public static final Script OLD_ITALIC = new Script("Ital", 210);
    public static final Script JAVANESE = new Script("Java", 361);
    public static final Script JAPANESE = new Script("Jpan", 413);
    public static final Script KAYAH_LI = new Script("Kali", 357);
    public static final Script KATAKANA = new Script("Kana", 411);
    public static final Script KHAROSHTHI = new Script("Khar", 305);
    public static final Script KHMER = new Script("Khmr", 355);
    public static final Script KANNADA = new Script("Knda", 345);
    public static final Script KOREAN = new Script("Kore", 287);
    public static final Script KAITHI = new Script("Kthi", 317);
    public static final Script TAI_THAM = new Script("Lana", 351);
    public static final Script LAO = new Script("Laoo", 356);
    public static final Script FRAKTUR_Latn = new Script("Latf", 217);
    public static final Script GAELIC_Latn = new Script("Latg", 216);
    public static final Script LATIN = new Script("Latn", 215);
    public static final Script LEPCHA = new Script("Lepc", 335);
    public static final Script LIMBU = new Script("Limb", 336);
    public static final Script LINEAR_A = new Script("Lina", 400);
    public static final Script LINEAR_B = new Script("Linb", 401);
    public static final Script LISU = new Script("Lisu", 399);
    public static final Script LYCIAN = new Script("Lyci", 202);
    public static final Script LYDIAN = new Script("Lydi", 116);
    public static final Script MANDAIC = new Script("Mand", 140);
    public static final Script MANICHAEAN = new Script("Mani", 139);
    public static final Script MAYAN_HIEROGLYPHS = new Script("Maya", 90);
    public static final Script MEROITIC = new Script("Mero", 100);
    public static final Script MALAYALAM = new Script("Mlym", 347);
    public static final Script MONGOLIAN = new Script("Mong", 145);
    public static final Script MOON = new Script("Moon", 218);
    public static final Script MEITEI_MAYEK = new Script("Mtei", 337);
    public static final Script MYANMAR = new Script("Mymr", 350);
    public static final Script NAKHI_ = new Script("Nkgb", 420);
    public static final Script N_KO = new Script("Nkoo", 165);
    public static final Script OGHAM = new Script("Ogam", 212);
    public static final Script OL_CHIKI = new Script("Olck", 261);
    public static final Script OLD_TURKIC = new Script("Orkh", 175);
    public static final Script ORIYA = new Script("Orya", 327);
    public static final Script OSMANYA = new Script("Osma", 260);
    public static final Script OLD_PERMIC = new Script("Perm", 227);
    public static final Script PHAGS_PA = new Script("Phag", 331);
    public static final Script INSCRIPTIONAL_PAHLAVI = new Script("Phli", 131);
    public static final Script PSALTER_PAHLAVI = new Script("Phlp", 132);
    public static final Script BOOK_PAHLAVI = new Script("Phlv", 133);
    public static final Script PHOENICIAN = new Script("Phnx", 115);
    public static final Script MIAO = new Script("Plrd", 282);
    public static final Script INSCRIPTIONAL_PARTHIAN = new Script("Prti", 130);
    public static final Script REJANG = new Script("Rjng", 363);
    public static final Script RONGORONGO = new Script("Roro", 620);
    public static final Script RUNIC = new Script("Runr", 211);
    public static final Script SAMARITAN = new Script("Samr", 123);
    public static final Script SARATI = new Script("Sara", 292);
    public static final Script OLD_SOUTH_ARABIAN = new Script("Sarb", 105);
    public static final Script SAURASHTRA = new Script("Saur", 344);
    public static final Script SIGNWRITING = new Script("Sgnw", 95);
    public static final Script SHAVIAN = new Script("Shaw", 281);
    public static final Script SHARADA = new Script("Shrd", 319);
    public static final Script SINHALA = new Script("Sinh", 348);
    public static final Script SUNDANESE = new Script("Sund", 362);
    public static final Script SYLOTI_NAGRI = new Script("Sylo", 316);
    public static final Script SYRIAC = new Script("Syrc", 135);
    public static final Script ESTRANGELO_SYRIAC = new Script("Syre", 138);
    public static final Script WESTERN_SYRIAC = new Script("Syrj", 137);
    public static final Script EASTERN_SYRIAC = new Script("Syrn", 136);
    public static final Script TAGBANWA = new Script("Tagb", 373);
    public static final Script TAI_LE = new Script("Tale", 353);
    public static final Script NEW_TAI_LUE = new Script("Talu", 354);
    public static final Script TAMIL = new Script("Taml", 346);
    public static final Script TAI_VIET = new Script("Tavt", 359);
    public static final Script TELUGU = new Script("Telu", 340);
    public static final Script TENGWAR = new Script("Teng", 290);
    public static final Script TIFINAGH = new Script("Tfng", 120);
    public static final Script TAGALOG = new Script("Tglg", 370);
    public static final Script THAANA = new Script("Thaa", 170);
    public static final Script THAI = new Script("Thai", 352);
    public static final Script TIBETAN = new Script("Tibt", 330);
    public static final Script UGARITIC = new Script("Ugar", 40);
    public static final Script VAI = new Script("Vaii", 470);
    public static final Script VISIBLE_SPEECH = new Script("Visp", 280);
    public static final Script OLD_PERSIAN = new Script("Xpeo", 30);
    public static final Script CUNEIFORM = new Script("Xsux", 20);
    public static final Script YI = new Script("Yiii", 460);

    static {
        scriptsByLang.put(Language.get("aat"), new Script[]{GREEK, LATIN});
        scriptsByLang.put(Language.get("ab"), new Script[]{CYRILLIC});
        scriptsByLang.put(Language.get("abk"), new Script[]{CYRILLIC});
        scriptsByLang.put(Language.get("ae"), new Script[]{AVESTAN, GUJARATI});
        scriptsByLang.put(Language.get("ain"), new Script[]{KATAKANA, LATIN});
        scriptsByLang.put(Language.get("akk"), new Script[]{CUNEIFORM});
        scriptsByLang.put(Language.get("am"), new Script[]{ETHIOPIC});
        scriptsByLang.put(Language.get("amh"), new Script[]{ETHIOPIC});
        scriptsByLang.put(Language.get("ang"), new Script[]{RUNIC, LATIN});
        scriptsByLang.put(Language.get("ara"), new Script[]{ARABIC, SYRIAC, LATIN});
        scriptsByLang.put(Language.get("ar"), new Script[]{ARABIC, SYRIAC, LATIN});
        scriptsByLang.put(Language.get("as"), new Script[]{DEVANAGARI});
        scriptsByLang.put(Language.get("asm"), new Script[]{DEVANAGARI});
        scriptsByLang.put(Language.get("ast"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ath"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("aus"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ave"), new Script[]{AVESTAN, GUJARATI});
        scriptsByLang.put(Language.get("aze"), new Script[]{LATIN, ARABIC});
        scriptsByLang.put(Language.get("az"), new Script[]{LATIN, ARABIC});
        scriptsByLang.put(Language.get("ban"), new Script[]{LATIN, BALINESE});
        scriptsByLang.put(Language.get("bar"), new Script[]{LATIN, GOTHIC});
        scriptsByLang.put(Language.get("be"), new Script[]{CYRILLIC, LATIN});
        scriptsByLang.put(Language.get("bel"), new Script[]{CYRILLIC, LATIN});
        scriptsByLang.put(Language.get("bg"), new Script[]{CYRILLIC});
        scriptsByLang.put(Language.get("bos"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("brh"), new Script[]{ARABIC, LATIN});
        scriptsByLang.put(Language.get("bs"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("bug"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("bul"), new Script[]{CYRILLIC});
        scriptsByLang.put(Language.get("ceb"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ce"), new Script[]{CYRILLIC, LATIN});
        scriptsByLang.put(Language.get("cel"), new Script[]{OLD_ITALIC, GREEK, LATIN});
        scriptsByLang.put(Language.get("ces"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("che"), new Script[]{CYRILLIC, LATIN});
        scriptsByLang.put(Language.get("chn"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("chr"), new Script[]{CHEROKEE, LATIN});
        scriptsByLang.put(Language.get("chu"), new Script[]{GLAGOLITIC, CYRILLIC});
        scriptsByLang.put(Language.get("co"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("cop"), new Script[]{COPTIC});
        scriptsByLang.put(Language.get("cor"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("cos"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("cre"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("cr"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("cs"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("cu"), new Script[]{GLAGOLITIC, CYRILLIC});
        scriptsByLang.put(Language.get("cy"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("cym"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("da"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("dan"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("de"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("dng"), new Script[]{CYRILLIC});
        scriptsByLang.put(Language.get("dsb"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("dum"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("dzo"), new Script[]{TIBETAN});
        scriptsByLang.put(Language.get("dz"), new Script[]{TIBETAN});
        scriptsByLang.put(Language.get("egy"), new Script[]{EGYPTIAN_HIEROGLYPHS, EGYPTIAN_HIERATIC, EGYPTIAN_DEMOTIC, COPTIC});
        scriptsByLang.put(Language.get("el"), new Script[]{GREEK});
        scriptsByLang.put(Language.get("eng"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("en"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("es"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("est"), new Script[]{LATIN, RUNIC});
        scriptsByLang.put(Language.get("et"), new Script[]{LATIN, RUNIC});
        scriptsByLang.put(Language.get("ext"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("eya"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("fa"), new Script[]{ARABIC, CYRILLIC});
        scriptsByLang.put(Language.get("fao"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("fil"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("fo"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("fra"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("fri"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("fr"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("fur"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("gaa"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("gd"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("gem"), new Script[]{LATIN, GOTHIC, RUNIC});
        scriptsByLang.put(Language.get("gez"), new Script[]{ETHIOPIC});
        scriptsByLang.put(Language.get("gla"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("gn"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("grc"), new Script[]{GREEK});
        scriptsByLang.put(Language.get("grn"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("gug"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("gu"), new Script[]{GUJARATI});
        scriptsByLang.put(Language.get("guj"), new Script[]{GUJARATI});
        scriptsByLang.put(Language.get("hak"), new Script[]{SIMPLIFIED_HAN, TRADITIONAL_HAN, LATIN});
        scriptsByLang.put(Language.get("ha"), new Script[]{LATIN, ARABIC});
        scriptsByLang.put(Language.get("hau"), new Script[]{LATIN, ARABIC});
        scriptsByLang.put(Language.get("haw"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("heb"), new Script[]{HEBREW});
        scriptsByLang.put(Language.get("he"), new Script[]{HEBREW});
        scriptsByLang.put(Language.get("hi"), new Script[]{DEVANAGARI, ARABIC});
        scriptsByLang.put(Language.get("hin"), new Script[]{DEVANAGARI, ARABIC});
        scriptsByLang.put(Language.get("hnd"), new Script[]{ARABIC, DEVANAGARI});
        scriptsByLang.put(Language.get("hsb"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("hu"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("hun"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("hy"), new Script[]{ARMENIAN});
        scriptsByLang.put(Language.get("hy"), new Script[]{ARMENIAN});
        scriptsByLang.put(Language.get("ia"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ibo"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("id"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ig"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ilo"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ina"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ind"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ine"), new Script[]{OLD_ITALIC});
        scriptsByLang.put(Language.get("ita"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("it"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ja"), new Script[]{JAPANESE, SIMPLIFIED_HAN, LATIN});
        scriptsByLang.put(Language.get("jav"), new Script[]{JAVANESE, ARABIC, LATIN});
        scriptsByLang.put(Language.get("jpn"), new Script[]{JAPANESE, SIMPLIFIED_HAN, LATIN});
        scriptsByLang.put(Language.get("jv"), new Script[]{JAVANESE, ARABIC, LATIN});
        scriptsByLang.put(Language.get("ka"), new Script[]{GEORGIAN});
        scriptsByLang.put(Language.get("kas"), new Script[]{ARABIC, DEVANAGARI, SHARADA});
        scriptsByLang.put(Language.get("kat"), new Script[]{GEORGIAN});
        scriptsByLang.put(Language.get("kaz"), new Script[]{CYRILLIC, LATIN, ARABIC});
        scriptsByLang.put(Language.get("kfa"), new Script[]{KANNADA});
        scriptsByLang.put(Language.get("khm"), new Script[]{KHMER});
        scriptsByLang.put(Language.get("kir"), new Script[]{CYRILLIC, ARABIC});
        scriptsByLang.put(Language.get("kk"), new Script[]{CYRILLIC, LATIN, ARABIC});
        scriptsByLang.put(Language.get("km"), new Script[]{KHMER});
        scriptsByLang.put(Language.get("ko"), new Script[]{HANGUL, SIMPLIFIED_HAN, CYRILLIC});
        scriptsByLang.put(Language.get("kor"), new Script[]{HANGUL, SIMPLIFIED_HAN, CYRILLIC});
        scriptsByLang.put(Language.get("krl"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ks"), new Script[]{ARABIC, DEVANAGARI, SHARADA});
        scriptsByLang.put(Language.get("ku"), new Script[]{ARABIC});
        scriptsByLang.put(Language.get("kur"), new Script[]{ARABIC});
        scriptsByLang.put(Language.get("kw"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ky"), new Script[]{CYRILLIC, ARABIC});
        scriptsByLang.put(Language.get("la"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("lao"), new Script[]{LAO});
        scriptsByLang.put(Language.get("lat"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("lav"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("lin"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("lit"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ln"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("lo"), new Script[]{LAO});
        scriptsByLang.put(Language.get("lt"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("lv"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("mak"), new Script[]{BRAHMI, LATIN});
        scriptsByLang.put(Language.get("mar"), new Script[]{DEVANAGARI});
        scriptsByLang.put(Language.get("may"), new Script[]{LATIN, ARABIC});
        scriptsByLang.put(Language.get("mk"), new Script[]{CYRILLIC});
        scriptsByLang.put(Language.get("mkd"), new Script[]{CYRILLIC});
        scriptsByLang.put(Language.get("mlt"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("mn"), new Script[]{MONGOLIAN, CYRILLIC});
        scriptsByLang.put(Language.get("mnw"), new Script[]{MYANMAR});
        scriptsByLang.put(Language.get("mon"), new Script[]{MONGOLIAN, CYRILLIC});
        scriptsByLang.put(Language.get("mr"), new Script[]{DEVANAGARI});
        scriptsByLang.put(Language.get("ms"), new Script[]{LATIN, ARABIC});
        scriptsByLang.put(Language.get("mt"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("mwl"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("mya"), new Script[]{MYANMAR});
        scriptsByLang.put(Language.get("my"), new Script[]{MYANMAR});
        scriptsByLang.put(Language.get("nan"), new Script[]{LATIN, SIMPLIFIED_HAN, TRADITIONAL_HAN});
        scriptsByLang.put(Language.get("nb"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ne"), new Script[]{DEVANAGARI});
        scriptsByLang.put(Language.get("nep"), new Script[]{DEVANAGARI});
        scriptsByLang.put(Language.get("nl"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("nn"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("nno"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("nob"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("nod"), new Script[]{TAI_THAM, THAI});
        scriptsByLang.put(Language.get("no"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("non"), new Script[]{RUNIC, LATIN});
        scriptsByLang.put(Language.get("nor"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("nys"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ofs"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ofs"), new Script[]{RUNIC});
        scriptsByLang.put(Language.get("om"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ori"), new Script[]{ORIYA});
        scriptsByLang.put(Language.get("orm"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("or"), new Script[]{ORIYA});
        scriptsByLang.put(Language.get("osc"), new Script[]{OLD_ITALIC});
        scriptsByLang.put(Language.get("pa"), new Script[]{DEVANAGARI});
        scriptsByLang.put(Language.get("pan"), new Script[]{DEVANAGARI});
        scriptsByLang.put(Language.get("peo"), new Script[]{CUNEIFORM});
        scriptsByLang.put(Language.get("phn"), new Script[]{PHOENICIAN});
        scriptsByLang.put(Language.get("pi"), new Script[]{BRAHMI});
        scriptsByLang.put(Language.get("pli"), new Script[]{BRAHMI});
        scriptsByLang.put(Language.get("pl"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("pol"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("por"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ps"), new Script[]{ARABIC});
        scriptsByLang.put(Language.get("pt"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("pus"), new Script[]{ARABIC});
        scriptsByLang.put(Language.get("que"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("qu"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ru"), new Script[]{CYRILLIC});
        scriptsByLang.put(Language.get("rus"), new Script[]{CYRILLIC});
        scriptsByLang.put(Language.get("sa"), new Script[]{DEVANAGARI, LATIN});
        scriptsByLang.put(Language.get("sai"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("san"), new Script[]{DEVANAGARI, LATIN});
        scriptsByLang.put(Language.get("sbv"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("scc"), new Script[]{LATIN, CYRILLIC});
        scriptsByLang.put(Language.get("sc"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("scr"), new Script[]{LATIN, CYRILLIC});
        scriptsByLang.put(Language.get("sd"), new Script[]{ARABIC, DEVANAGARI});
        scriptsByLang.put(Language.get("sdc"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("sin"), new Script[]{SINHALA});
        scriptsByLang.put(Language.get("si"), new Script[]{SINHALA});
        scriptsByLang.put(Language.get("sit"), new Script[]{CYRILLIC});
        scriptsByLang.put(Language.get("sk"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("slk"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("sl"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("slv"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("snd"), new Script[]{ARABIC, DEVANAGARI});
        scriptsByLang.put(Language.get("spa"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("srd"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("sth"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("sv"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("swa"), new Script[]{LATIN, ARABIC});
        scriptsByLang.put(Language.get("swe"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("swg"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("sw"), new Script[]{LATIN, ARABIC});
        scriptsByLang.put(Language.get("syc"), new Script[]{SYRIAC});
        scriptsByLang.put(Language.get("tai"), new Script[]{TAI_THAM, THAI});
        scriptsByLang.put(Language.get("tam"), new Script[]{TAMIL});
        scriptsByLang.put(Language.get("ta"), new Script[]{TAMIL});
        scriptsByLang.put(Language.get("tat"), new Script[]{CYRILLIC, LATIN, ARABIC});
        scriptsByLang.put(Language.get("tcb"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("tcy"), new Script[]{KANNADA});
        scriptsByLang.put(Language.get("tel"), new Script[]{TELUGU});
        scriptsByLang.put(Language.get("te"), new Script[]{TELUGU});
        scriptsByLang.put(Language.get("tfn"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("tg"), new Script[]{CYRILLIC, LATIN, ARABIC});
        scriptsByLang.put(Language.get("tgk"), new Script[]{CYRILLIC, LATIN, ARABIC});
        scriptsByLang.put(Language.get("tg"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("tgl"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("tha"), new Script[]{THAI});
        scriptsByLang.put(Language.get("th"), new Script[]{THAI});
        scriptsByLang.put(Language.get("tlh"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("tl"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("tpw"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("tr"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("tt"), new Script[]{CYRILLIC, LATIN, ARABIC});
        scriptsByLang.put(Language.get("tur"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("ug"), new Script[]{ARABIC});
        scriptsByLang.put(Language.get("uig"), new Script[]{ARABIC});
        scriptsByLang.put(Language.get("uk"), new Script[]{CYRILLIC});
        scriptsByLang.put(Language.get("ukr"), new Script[]{CYRILLIC});
        scriptsByLang.put(Language.get("ur"), new Script[]{ARABIC, DEVANAGARI});
        scriptsByLang.put(Language.get("urd"), new Script[]{ARABIC, DEVANAGARI});
        scriptsByLang.put(Language.get("vie"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("vi"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("xlc"), new Script[]{LYCIAN});
        scriptsByLang.put(Language.get("xum"), new Script[]{OLD_ITALIC});
        scriptsByLang.put(Language.get("yid"), new Script[]{HEBREW});
        scriptsByLang.put(Language.get("yi"), new Script[]{HEBREW});
        scriptsByLang.put(Language.get("yo"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("yor"), new Script[]{LATIN});
        scriptsByLang.put(Language.get("yue"), new Script[]{TRADITIONAL_HAN});
        scriptsByLang.put(Language.get("zh"), new Script[]{SIMPLIFIED_HAN, TRADITIONAL_HAN, BOPOMOFO, LATIN});
        scriptsByLang.put(Language.get("zlm"), new Script[]{LATIN, ARABIC});
    }

    /**
     * Get the default scripts used to write the language. This data was derived
     * from DBPedia 3.6 and is not complete
     *
     * @return The list of scripts used to represent the language, in order of
     * importance or null if no script is known
     */
    public static Script[] getKnownScriptsForLanguage(Language lang) {
        return scriptsByLang.get(lang);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final Script other = (Script) obj;
        if ((this.alpha4code == null) ? (other.alpha4code != null) : !this.alpha4code.equals(other.alpha4code)) {
            return false;
        }
        if (this.numericCode != other.numericCode) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        int hash = 3;
        hash = 47 * hash + (this.alpha4code != null ? this.alpha4code.hashCode() : 0);
        hash = 47 * hash + this.numericCode;
        return hash;
    }
}
