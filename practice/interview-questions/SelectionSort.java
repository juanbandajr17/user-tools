public class SelectionSort {
    public void selectionSort(int[] data) {
        for (int start = 0; start < data.length - 1; ++start) {
            swap(data, start, findMinimumIndex(data, start));
        }
    }

    public void selectionSortStable(int[] data) {
        for(int start = 0; start < data.length - 1; ++start) {
            insert(data, start, findMinimumIndex(data, start));
        }
    }

    private void insert(int[] data, int start, int minIndex) {
        if (minIndex > start) {
            int tmp = data[minIndex];
            System.arraycopy(data, start, data, start+1, minIndex - start);
            data[start] = tmp;
        }
    }

}
