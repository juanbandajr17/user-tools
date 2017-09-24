public class InsertionSort {

    public void insertionSort(int[] data) {
        for(int which = 1; which < data.length; ++which) {
            int val = data[which];

            for(int i = 0; i < which; ++i) {
                if (data[i] > val) {
                    System.arraycopy(data, i, data, i+1, which - 1);
                    data[i] = val;
                    break;
                }
            }
        }
    }

}
