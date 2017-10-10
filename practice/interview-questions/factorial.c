int factorial(int n) {
  if (n > 1) {
    return factorial(n - 1) * n;
  } else {
    return 1;
  }
}

int factorial(int n) {
  int i, val = 1;
  for(i = n; i > 1; i--)
    val *= i;
  return val;
}

int[] allFactorials(int n) {
  int[] results = new int[n == 0 ? 1 : n];
  doAllFactorials(n, results, 0);
  return results;
}

int doAllFactorials(int n, int[] results, int level) {
  if (n > 1) {
    results[level] = n * doAllFactorials(n-1, results, level + 1);
    return results[level];
  } else {
    results[level] = 1;
    return 1;
  }
}

int factorial(int n) {
  int i, val = 1;
  for(i = n; i > 1; i--)
    val *= i;
  return val;
}
