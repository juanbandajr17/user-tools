Node findLowestCommonAncestor(Node root, int value1, int value2) {
  int value;

  while (root != null) {
    value = root.getValue();

    if (value > value1 && value > value2) {
      root = root.getLeft();
    } else if (value < value1 && value < value2) {
      root = root.getRight();
    } else {
      return root;
    }
  }

  return null;
}
