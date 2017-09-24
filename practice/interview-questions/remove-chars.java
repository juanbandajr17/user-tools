public class RemoveChars {

    public static String removeChars(String str, String remove) {
        char[] s = str.toCharArray();
        char[] r = remove.toCharArray();
        int src, dst = 0;

        boolean[] flags = new boolean[128];

        for(src = 0; src < r.length; ++src) {
            flags[r[src]] = true;
        }

        for(src = 0; src < s.length; ++src) {
            if (!flags[s[src]])
                s[dst++] = s[src];

        }
        return new String(s, 0, dst);
    }
}
