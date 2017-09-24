public class Permutations {
    private boolean[] used;
    private StringBuilder out = new StringBuilder();
    private final String in;

    public Permutations(final String str){
        in = str;
        used = new bolean[in.length()];
    }

    public void permute() {
        if (out.length() == in.length()) {
            System.out.println(out);
            return;
        }

        for (int i = 0; i < in.length(); ++i) {
            if (used[i]) continue;
            out.append(in.chartAt(i));
            used[i] = true;
            permute();
            used[i] = false;
            out.setLength(out.length()-1);
        }
    }
}
