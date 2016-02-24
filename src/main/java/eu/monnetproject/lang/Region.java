/**********************************************************************************
 * Copyright (c) 2011, Monnet Project
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the Monnet Project nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE MONNET PROJECT BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *********************************************************************************/
package eu.monnetproject.lang;

import java.io.Serializable;
import java.util.HashMap;

/**
 * Region codings. This is based on the ISO 3166-1 standard
 *
 * @author John McCrae
 */
public final class Region implements Serializable {

    private static final long serialVersionUID = 8760263036055576045L;
    private String alpha2code;
    private String alpha3code;
    private int numericCode;
    private static final HashMap<String, Region> byAlpha2Code = new HashMap<String, Region>();
    private static final HashMap<String, Region> byAlpha3Code = new HashMap<String, Region>();
    private static final HashMap<Integer, Region> byNumericCode = new HashMap<Integer, Region>();

    /**
     * Get a new instance
     *
     * @param alpha2code The 2 letter alphabetic code
     * @param alpha3code The 3 letter alphabetic code
     * @param numericCode The numeric code
     */
    public Region(String alpha2code, String alpha3code, int numericCode) {
        assert (alpha2code.length() == 2);
        assert (alpha3code.length() == 3);
        assert (numericCode >= 0 && numericCode < 1000);
        this.alpha2code = alpha2code.toUpperCase();
        this.alpha3code = alpha3code.toUpperCase();
        this.numericCode = numericCode;
        if (!byAlpha2Code.containsKey(this.alpha2code)) {
            byAlpha2Code.put(this.alpha2code, this);
        }
        if (!byAlpha3Code.containsKey(this.alpha3code)) {
            byAlpha3Code.put(this.alpha3code, this);
        }
        if (!byNumericCode.containsKey(this.numericCode)) {
            byNumericCode.put(this.numericCode, this);
        }
    }

    /**
     * Get the 2-letter alphabetic code
     *
     * @return
     */
    public String getAlpha2code() {
        return alpha2code;
    }

    /**
     * Get the 3-letter alphabetic code
     *
     * @return
     */
    public String getAlpha3code() {
        return alpha3code;
    }

    /**
     * Get the numeric code
     *
     * @return
     */
    public int getNumericCode() {
        return numericCode;
    }

    /**
     * Get a region instance by 2-letter alphabetic code
     *
     * @param alpha2code The code
     * @return The instance or null if the code is not recognised
     */
    public static Region getByAlpha2Code(String alpha2code) {
        return byAlpha2Code.get(alpha2code.toUpperCase());
    }

    /**
     * Get a region instance by 3-letter alphabetic code
     *
     * @param alpha3code The code
     * @return The instance or null if the code is not recognised
     */
    public static Region getByAlpha3Code(String alpha3code) {
        return byAlpha3Code.get(alpha3code.toUpperCase());
    }

    /**
     * Get a region instance by numeric code
     *
     * @param numericCode The code
     * @return The instance or null if the code is not recognised
     */
    public static Region getByNumericCode(int numericCode) {
        return byNumericCode.get(numericCode);
    }

    @Override
    public String toString() {
        if (alpha2code != null) {
            return alpha2code;
        }
        return alpha3code;
    }
    public static final Region AFGHANISTAN = new Region("AF", "AFG", 4);
    public static final Region ALAND_ISLANDS = new Region("AX", "ALA", 248);
    public static final Region ALBANIA = new Region("AL", "ALB", 8);
    public static final Region ALGERIA = new Region("DZ", "DZA", 12);
    public static final Region AMERICAN_SAMOA = new Region("AS", "ASM", 16);
    public static final Region ANDORRA = new Region("AD", "AND", 20);
    public static final Region ANGOLA = new Region("AO", "AGO", 24);
    public static final Region ANGUILLA = new Region("AI", "AIA", 660);
    public static final Region ANTARCTICA = new Region("AQ", "ATA", 10);
    public static final Region ANTIGUA_AND_BARBUDA = new Region("AG", "ATG", 28);
    public static final Region ARGENTINA = new Region("AR", "ARG", 32);
    public static final Region ARMENIA = new Region("AM", "ARM", 51);
    public static final Region ARUBA = new Region("AW", "ABW", 533);
    public static final Region AUSTRALIA = new Region("AU", "AUS", 36);
    public static final Region AUSTRIA = new Region("AT", "AUT", 40);
    public static final Region AZERBAIJAN = new Region("AZ", "AZE", 31);
    public static final Region BAHAMAS = new Region("BS", "BHS", 44);
    public static final Region BAHRAIN = new Region("BH", "BHR", 48);
    public static final Region BANGLADESH = new Region("BD", "BGD", 50);
    public static final Region BARBADOS = new Region("BB", "BRB", 52);
    public static final Region BELARUS = new Region("BY", "BLR", 112);
    public static final Region BELGIUM = new Region("BE", "BEL", 56);
    public static final Region BELIZE = new Region("BZ", "BLZ", 84);
    public static final Region BENIN = new Region("BJ", "BEN", 204);
    public static final Region BERMUDA = new Region("BM", "BMU", 60);
    public static final Region BHUTAN = new Region("BT", "BTN", 64);
    public static final Region BOLIVIA = new Region("BO", "BOL", 68);
    public static final Region BOSNIA_AND_HERZEGOVINA = new Region("BA", "BIH", 70);
    public static final Region BOTSWANA = new Region("BW", "BWA", 72);
    public static final Region BOUVET_ISLAND = new Region("BV", "BVT", 74);
    public static final Region BRAZIL = new Region("BR", "BRA", 076);
    public static final Region BRITISH_INDIAN_OCEAN_TERRITORY = new Region("IO", "IOT", 86);
    public static final Region BRUNEI_DARUSSALAM = new Region("BN", "BRN", 96);
    public static final Region BULGARIA = new Region("BG", "BGR", 100);
    public static final Region BURKINA_FASO = new Region("BF", "BFA", 854);
    public static final Region BURUNDI = new Region("BI", "BDI", 108);
    public static final Region CAMBODIA = new Region("KH", "KHM", 116);
    public static final Region CAMEROON = new Region("CM", "CMR", 120);
    public static final Region CANADA = new Region("CA", "CAN", 124);
    public static final Region CAPE_VERDE = new Region("CV", "CPV", 132);
    public static final Region CAYMAN_ISLANDS = new Region("KY", "CYM", 136);
    public static final Region CENTRAL_AFRICAN_REPUBLIC = new Region("CF", "CAF", 140);
    public static final Region CHAD = new Region("TD", "TCD", 148);
    public static final Region CHILE = new Region("CL", "CHL", 152);
    public static final Region CHINA = new Region("CN", "CHN", 156);
    public static final Region CHRISTMAS_ISLAND = new Region("CX", "CXR", 162);
    public static final Region COCOS_ISLANDS = new Region("CC", "CCK", 166);
    public static final Region COLOMBIA = new Region("CO", "COL", 170);
    public static final Region COMOROS = new Region("KM", "COM", 174);
    public static final Region CONGO = new Region("CG", "COG", 178);
    public static final Region DEMOCRATIC_REPUBLIC_OF_CONGO = new Region("CD", "COD", 180);
    public static final Region COOK_ISLANDS = new Region("CK", "COK", 184);
    public static final Region COSTA_RICA = new Region("CR", "CRI", 188);
    public static final Region COTE_DIVOIRE = new Region("CI", "CIV", 384);
    public static final Region CROATIA = new Region("HR", "HRV", 191);
    public static final Region CUBA = new Region("CU", "CUB", 192);
    public static final Region CYPRUS = new Region("CY", "CYP", 196);
    public static final Region CZECH_REPUBLIC = new Region("CZ", "CZE", 203);
    public static final Region DENMARK = new Region("DK", "DNK", 208);
    public static final Region DJIBOUTI = new Region("DJ", "DJI", 262);
    public static final Region DOMINICA = new Region("DM", "DMA", 212);
    public static final Region DOMINICAN_REPUBLIC = new Region("DO", "DOM", 214);
    public static final Region ECUADOR = new Region("EC", "ECU", 218);
    public static final Region EGYPT = new Region("EG", "EGY", 818);
    public static final Region EL_SALVADOR = new Region("SV", "SLV", 222);
    public static final Region EQUATORIAL_GUINEA = new Region("GQ", "GNQ", 226);
    public static final Region ERITREA = new Region("ER", "ERI", 232);
    public static final Region ESTONIA = new Region("EE", "EST", 233);
    public static final Region ETHIOPIA = new Region("ET", "ETH", 231);
    public static final Region FALKLAND_ISLANDS = new Region("FK", "FLK", 238);
    public static final Region FAROE_ISLANDS = new Region("FO", "FRO", 234);
    public static final Region FIJI = new Region("FJ", "FJI", 242);
    public static final Region FINLAND = new Region("FI", "FIN", 246);
    public static final Region FRANCE = new Region("FR", "FRA", 250);
    public static final Region FRENCH_GUIANA = new Region("GF", "GUF", 254);
    public static final Region FRENCH_POLYNESIA = new Region("PF", "PYF", 258);
    public static final Region FRENCH_SOUTHERN_TERRITORIES = new Region("TF", "ATF", 260);
    public static final Region GABON = new Region("GA", "GAB", 266);
    public static final Region GAMBIA = new Region("GM", "GMB", 270);
    public static final Region GEORGIA = new Region("GE", "GEO", 268);
    public static final Region GERMANY = new Region("DE", "DEU", 276);
    public static final Region GHANA = new Region("GH", "GHA", 288);
    public static final Region GIBRALTAR = new Region("GI", "GIB", 292);
    public static final Region GREECE = new Region("GR", "GRC", 300);
    public static final Region GREENLAND = new Region("GL", "GRL", 304);
    public static final Region GRENADA = new Region("GD", "GRD", 308);
    public static final Region GUADELOUPE = new Region("GP", "GLP", 312);
    public static final Region GUAM = new Region("GU", "GUM", 316);
    public static final Region GUATEMALA = new Region("GT", "GTM", 320);
    public static final Region GUERNSEY = new Region("GG", "GGY", 831);
    public static final Region GUINEA = new Region("GN", "GIN", 324);
    public static final Region GUINEA_BISSAU = new Region("GW", "GNB", 624);
    public static final Region GUYANA = new Region("GY", "GUY", 328);
    public static final Region HAITI = new Region("HT", "HTI", 332);
    public static final Region HEARD_ISLAND_AND_MCDONALD_ISLANDS = new Region("HM", "HMD", 334);
    public static final Region HOLY_SEE = new Region("VA", "VAT", 336);
    public static final Region HONDURAS = new Region("HN", "HND", 340);
    public static final Region HONG_KONG = new Region("HK", "HKG", 344);
    public static final Region HUNGARY = new Region("HU", "HUN", 348);
    public static final Region ICELAND = new Region("IS", "ISL", 352);
    public static final Region INDIA = new Region("IN", "IND", 356);
    public static final Region INDONESIA = new Region("ID", "IDN", 360);
    public static final Region IRAN = new Region("IR", "IRN", 364);
    public static final Region IRAQ = new Region("IQ", "IRQ", 368);
    public static final Region IRELAND = new Region("IE", "IRL", 372);
    public static final Region ISLE_OF_MAN = new Region("IM", "IMN", 833);
    public static final Region ISRAEL = new Region("IL", "ISR", 376);
    public static final Region ITALY = new Region("IT", "ITA", 380);
    public static final Region JAMAICA = new Region("JM", "JAM", 388);
    public static final Region JAPAN = new Region("JP", "JPN", 392);
    public static final Region JERSEY = new Region("JE", "JEY", 832);
    public static final Region JORDAN = new Region("JO", "JOR", 400);
    public static final Region KAZAKHSTAN = new Region("KZ", "KAZ", 398);
    public static final Region KENYA = new Region("KE", "KEN", 404);
    public static final Region KIRIBATI = new Region("KI", "KIR", 296);
    public static final Region DPR_KOREA = new Region("KP", "PRK", 408);
    public static final Region REPUBLIC_OF_KOREA = new Region("KR", "KOR", 410);
    public static final Region KUWAIT = new Region("KW", "KWT", 414);
    public static final Region KYRGYZSTAN = new Region("KG", "KGZ", 417);
    public static final Region LAOS = new Region("LA", "LAO", 418);
    public static final Region LATVIA = new Region("LV", "LVA", 428);
    public static final Region LEBANON = new Region("LB", "LBN", 422);
    public static final Region LESOTHO = new Region("LS", "LSO", 426);
    public static final Region LIBERIA = new Region("LR", "LBR", 430);
    public static final Region LIBYAN_ARAB_JAMAHIRIYA = new Region("LY", "LBY", 434);
    public static final Region LIECHTENSTEIN = new Region("LI", "LIE", 438);
    public static final Region LITHUANIA = new Region("LT", "LTU", 440);
    public static final Region LUXEMBOURG = new Region("LU", "LUX", 442);
    public static final Region MACAO = new Region("MO", "MAC", 446);
    public static final Region MACEDONIA = new Region("MK", "MKD", 807);
    public static final Region MADAGASCAR = new Region("MG", "MDG", 450);
    public static final Region MALAWI = new Region("MW", "MWI", 454);
    public static final Region MALAYSIA = new Region("MY", "MYS", 458);
    public static final Region MALDIVES = new Region("MV", "MDV", 462);
    public static final Region MALI = new Region("ML", "MLI", 466);
    public static final Region MALTA = new Region("MT", "MLT", 470);
    public static final Region MARSHALL_ISLANDS = new Region("MH", "MHL", 584);
    public static final Region MARTINIQUE = new Region("MQ", "MTQ", 474);
    public static final Region MAURITANIA = new Region("MR", "MRT", 478);
    public static final Region MAURITIUS = new Region("MU", "MUS", 480);
    public static final Region MAYOTTE = new Region("YT", "MYT", 175);
    public static final Region MEXICO = new Region("MX", "MEX", 484);
    public static final Region MICRONESIA = new Region("FM", "FSM", 583);
    public static final Region MOLDOVA = new Region("MD", "MDA", 498);
    public static final Region MONACO = new Region("MC", "MCO", 492);
    public static final Region MONGOLIA = new Region("MN", "MNG", 496);
    public static final Region MONTENEGRO = new Region("ME", "MNE", 499);
    public static final Region MONTSERRAT = new Region("MS", "MSR", 500);
    public static final Region MOROCCO = new Region("MA", "MAR", 504);
    public static final Region MOZAMBIQUE = new Region("MZ", "MOZ", 508);
    public static final Region MYANMAR = new Region("MM", "MMR", 104);
    public static final Region NAMIBIA = new Region("NA", "NAM", 516);
    public static final Region NAURU = new Region("NR", "NRU", 520);
    public static final Region NEPAL = new Region("NP", "NPL", 524);
    public static final Region NETHERLANDS = new Region("NL", "NLD", 528);
    public static final Region NETHERLANDS_ANTILLES = new Region("AN", "ANT", 530);
    public static final Region NEW_CALEDONIA = new Region("NC", "NCL", 540);
    public static final Region NEW_ZEALAND = new Region("NZ", "NZL", 554);
    public static final Region NICARAGUA = new Region("NI", "NIC", 558);
    public static final Region NIGER = new Region("NE", "NER", 562);
    public static final Region NIGERIA = new Region("NG", "NGA", 566);
    public static final Region NIUE = new Region("NU", "NIU", 570);
    public static final Region NORFOLK_ISLAND = new Region("NF", "NFK", 574);
    public static final Region NORTHERN_MARIANA_ISLANDS = new Region("MP", "MNP", 580);
    public static final Region NORWAY = new Region("NO", "NOR", 578);
    public static final Region OMAN = new Region("OM", "OMN", 512);
    public static final Region PAKISTAN = new Region("PK", "PAK", 586);
    public static final Region PALAU = new Region("PW", "PLW", 585);
    public static final Region PALESTINIAN_TERRITORY = new Region("PS", "PSE", 275);
    public static final Region PANAMA = new Region("PA", "PAN", 591);
    public static final Region PAPUA_NEW_GUINEA = new Region("PG", "PNG", 598);
    public static final Region PARAGUAY = new Region("PY", "PRY", 600);
    public static final Region PERU = new Region("PE", "PER", 604);
    public static final Region PHILIPPINES = new Region("PH", "PHL", 608);
    public static final Region PITCAIRN = new Region("PN", "PCN", 612);
    public static final Region POLAND = new Region("PL", "POL", 616);
    public static final Region PORTUGAL = new Region("PT", "PRT", 620);
    public static final Region PUERTO_RICO = new Region("PR", "PRI", 630);
    public static final Region QATAR = new Region("QA", "QAT", 634);
    public static final Region ReUNION = new Region("RE", "REU", 638);
    public static final Region ROMANIA = new Region("RO", "ROU", 642);
    public static final Region RUSSIAN_FEDERATION = new Region("RU", "RUS", 643);
    public static final Region RWANDA = new Region("RW", "RWA", 646);
    public static final Region SAINT_BARTaLEMY = new Region("BL", "BLM", 652);
    public static final Region SAINT_HELENA = new Region("SH", "SHN", 654);
    public static final Region SAINT_KITTS_AND_NEVIS = new Region("KN", "KNA", 659);
    public static final Region SAINT_LUCIA = new Region("LC", "LCA", 662);
    public static final Region SAINT_MARTIN = new Region("MF", "MAF", 663);
    public static final Region SAINT_PIERRE_AND_MIQUELON = new Region("PM", "SPM", 666);
    public static final Region SAINT_VINCENT_AND_THE_GRENADINES = new Region("VC", "VCT", 670);
    public static final Region SAMOA = new Region("WS", "WSM", 882);
    public static final Region SAN_MARINO = new Region("SM", "SMR", 674);
    public static final Region SAO_TOME_AND_PRINCIPE = new Region("ST", "STP", 678);
    public static final Region SAUDI_ARABIA = new Region("SA", "SAU", 682);
    public static final Region SENEGAL = new Region("SN", "SEN", 686);
    public static final Region SERBIA = new Region("RS", "SRB", 688);
    public static final Region SEYCHELLES = new Region("SC", "SYC", 690);
    public static final Region SIERRA_LEONE = new Region("SL", "SLE", 694);
    public static final Region SINGAPORE = new Region("SG", "SGP", 702);
    public static final Region SLOVAKIA = new Region("SK", "SVK", 703);
    public static final Region SLOVENIA = new Region("SI", "SVN", 705);
    public static final Region SOLOMON_ISLANDS = new Region("SB", "SLB", 90);
    public static final Region SOMALIA = new Region("SO", "SOM", 706);
    public static final Region SOUTH_AFRICA = new Region("ZA", "ZAF", 710);
    public static final Region SOUTH_GEORGIA_AND_THE_SOUTH_SANDWICH_ISLANDS = new Region("GS", "SGS", 239);
    public static final Region SPAIN = new Region("ES", "ESP", 724);
    public static final Region SRI_LANKA = new Region("LK", "LKA", 144);
    public static final Region SUDAN = new Region("SD", "SDN", 736);
    public static final Region SURINAME = new Region("SR", "SUR", 740);
    public static final Region SVALBARD_AND_JAN_MAYEN = new Region("SJ", "SJM", 744);
    public static final Region SWAZILAND = new Region("SZ", "SWZ", 748);
    public static final Region SWEDEN = new Region("SE", "SWE", 752);
    public static final Region SWITZERLAND = new Region("CH", "CHE", 756);
    public static final Region SYRIAN_ARAB_REPUBLIC = new Region("SY", "SYR", 760);
    public static final Region TAIWAN = new Region("TW", "TWN", 158);
    public static final Region TAJIKISTAN = new Region("TJ", "TJK", 762);
    public static final Region TANZANIA = new Region("TZ", "TZA", 834);
    public static final Region THAILAND = new Region("TH", "THA", 764);
    public static final Region TIMOR_LESTE = new Region("TL", "TLS", 626);
    public static final Region TOGO = new Region("TG", "TGO", 768);
    public static final Region TOKELAU = new Region("TK", "TKL", 772);
    public static final Region TONGA = new Region("TO", "TON", 776);
    public static final Region TRINIDAD_AND_TOBAGO = new Region("TT", "TTO", 780);
    public static final Region TUNISIA = new Region("TN", "TUN", 788);
    public static final Region TURKEY = new Region("TR", "TUR", 792);
    public static final Region TURKMENISTAN = new Region("TM", "TKM", 795);
    public static final Region TURKS_AND_CAICOS_ISLANDS = new Region("TC", "TCA", 796);
    public static final Region TUVALU = new Region("TV", "TUV", 798);
    public static final Region UGANDA = new Region("UG", "UGA", 800);
    public static final Region UKRAINE = new Region("UA", "UKR", 804);
    public static final Region UNITED_ARAB_EMIRATES = new Region("AE", "ARE", 784);
    public static final Region UNITED_KINGDOM = new Region("GB", "GBR", 826);
    public static final Region UNITED_STATES = new Region("US", "USA", 840);
    public static final Region UNITED_STATES_MINOR_OUTLYING_ISLANDS = new Region("UM", "UMI", 581);
    public static final Region URUGUAY = new Region("UY", "URY", 858);
    public static final Region UZBEKISTAN = new Region("UZ", "UZB", 860);
    public static final Region VANUATU = new Region("VU", "VUT", 548);
    public static final Region VENEZUELA = new Region("VE", "VEN", 862);
    public static final Region VIET_NAM = new Region("VN", "VNM", 704);
    public static final Region BRITISH_VIRGIN_ISLANDS = new Region("VG", "VGB", 92);
    public static final Region AMERICAN_VIRGIN_ISLANDS = new Region("VI", "VIR", 850);
    public static final Region WALLIS_AND_FUTUNA = new Region("WF", "WLF", 876);
    public static final Region WESTERN_SAHARA = new Region("EH", "ESH", 732);
    public static final Region YEMEN = new Region("YE", "YEM", 887);
    public static final Region ZAMBIA = new Region("ZM", "ZMB", 894);
    public static final Region ZIMBABWE = new Region("ZW", "ZWE", 716);
    // Since 15/12/2011
    public static final Region CURACAO = new Region("CW", "CUW", 351);
    public static final Region SOUTH_SUDAN = new Region("SS", "SSD", 728);
    public static final Region SINT_MAARTIN = new Region("SX", "SXM", 534);

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final Region other = (Region) obj;
        if ((this.alpha2code == null) ? (other.alpha2code != null) : !this.alpha2code.equals(other.alpha2code)) {
            return false;
        }
        if ((this.alpha3code == null) ? (other.alpha3code != null) : !this.alpha3code.equals(other.alpha3code)) {
            return false;
        }
        if (this.numericCode != other.numericCode) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 17 * hash + (this.alpha2code != null ? this.alpha2code.hashCode() : 0);
        hash = 17 * hash + (this.alpha3code != null ? this.alpha3code.hashCode() : 0);
        hash = 17 * hash + this.numericCode;
        return hash;
    }
}
