void preorderTraversal(Node root) {
  NodeStack stack = new NodeStack();
  stack.push(root);

  Node current;
  Node n;

  while(stack.size() > 0) {
    current = stack.pop();
    current.printValue();
    n = current.getRight();
    if (n != null) stack.push(n);
    n = current.getLeft();
    if (n != null) stack.push(n);
  }
}
