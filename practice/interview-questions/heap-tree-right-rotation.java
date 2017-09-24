class HeapStuff {

    public static Node rotateRight(Node oldRoot) {
        Node newRoot = oldRoot.getLeft();
        oldRoot.setLeft(newRoot.getRight());
        newRoot.setRight(oldRoot);
        return newRoot;
    }

    public Node rotateRight() {
        Node newRoot = left;
        left = newRoot.right;
        newRoot.right = this;
        return newRoot;
    }

}
