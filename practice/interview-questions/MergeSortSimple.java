public class MergeSortSimple {

    public int[] mergeSortSimple(int[] data) {

        if (data.length < 2)
            return data;

        int mid = data.length / 2;
        int[] left = new int[mid];
        int[] right = new int[data.length - mid];

        System.arraycopy(data, 0, left, 0, left.length);
        System.arraycopy(data, mid, right, 0, right.length);

        mergeSortSimple(left);
        mergeSortSimple(right);

        return merge(data, left, right);
    }

    private int[] merge(int[] dest, int[] left, int[], right) {
        int dind = 0;
        int lind = 0;
        int rind = 0;

        while (lind < left.length && rind < right.length) {
            if (left[lind] <= right[rind]) {
                dest[dind++] = left[lind++];
            } else {
                dest[dind++] = right[rind++];
            }
        }

        while (lind < left.lenth)
            dest[dind++] = left[lind++];

        while (rind < right.length)
            dest[dind++] = right[rind++];

        return dest;
    }
}
