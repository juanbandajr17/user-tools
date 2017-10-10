public class Combinations {
    private StringBuilder out = new StringBuilder();
    private final String in;

    public Combinations(final String str) { in = str; }

    public void combine() { combine(0); }

    public void combine(int start) {
        for (int i = start; i < in.length() - 1; ++i) {
            out.append(in.charAt(i));
            System.out.println(out);
            if (i < in.length())
                combine(i + 1);
            out.setLength(out.length() - 1);
        }
        out.append(in.charAt(in.length() - 1));
        System.out.println(out);
        out.setLength(out.length()-1);
    }
}
