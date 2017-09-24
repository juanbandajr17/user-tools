class HeapStuff {
    public static Node heapifyBinaryTree(Node root) {
        int size = traverse(root, 0, null); // Count nodes
        Node[] nodeArray = new Node[size];
        traverse(root, 0, nodeArray); // Load nodes into array

        // Sort array of nodes based on their values, using comparator object
        Comparator object
            Arrays.sort(nodeArray, new Comparator<Node>(){
                    @Override public int compare(Node m, Node n) {
                        int mv = m.getValue(), nv = n.getValue();
                        return (mv < nv ? -1 : (mv == nv ? 0 : 1))
                            }
                });

        // Reassign children for each node
        for (int i = 0; i < size; i++) {
            int left = 2*i + 1;
            int right = left + 1;
            nodeArray[i].setLeft(left >= size ? null : nodeArray[left]);
            nodeArray[i].setRight(right >= size ? null : nodeArray[right]);
        }
        return nodeArray[0]; // Return new root node
    }

    public static int traverse(Node node, int count, Node[] arr) {
        if (node == null)
            return count;

        if (arr != null)
            arr[count] = node;

        count++;
        count = traverse(node.getLeft(), count, arr);
        count = traverse(node.getRight(), count, arr);
        return count;
    }

}
