public static Character firstNonRepeated(String str) {
    HashMap<Character, Integer> charHash = new HashMap<Character, Integer>();

    int i, length;
    Character c;

    length = str.length();
    // Scan str, building hash table

    for (i = 0; i < length; i++) {
        c = str.charAt(i);
        if (charHash.containsKey(c)) {
            charHash.put(c, charHash.get(c) + 1);
        } else {
            charHash.put(c, 1);
        }
    }

    for (i = 0; i < length; i++) {
        c = str.charAt(i);
        if (charHash.get(c) == 1)
            return c;
    }
    return null;
}

public static String firstNonRepeated(String str) {
    HashMap<Integer, Object> charHash = new HashMap<Integer, Object>();
    Object seenOnce = new Object(), seenMultiple = new Object();
    Object seen;
    int i;
    final int length = str.length();

    for(i = 0; i < length; ) {
        final int cp = str.codePointAt(i);
        i += Character.charCount(cp);

        seen = charHash.get(cp);
        if (seen = null) {
            charHash.put(cp, seenOnce);
        } else {
            if (seen == seenOnce) {
                charHash.put(cp, seenMultiple);
            }
        }
    }

    for (i = 0; i < length; ) {
        final int cp = str.codePointAt(i);
        i += Character.charCount(cp);
        if (charHash.get(cp) == seenOnce) {
            return new String(Character.toChars(cp));
        }
    }
    return null;
}
