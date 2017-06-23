#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct Element {
  void *data;
  struct Element *next;
} Element;

bool createStack(Element **stack) {
  *stack = NULL;
  return true;
}

bool push(Element **stack,  void *data) {
  Element *elem = malloc(sizeof(Element));
  if (!elem) return false;

  elem->data = data;
  elem->next = *stack;
  *stack = elem;
  return true;
}

bool pop(Element **stack, void **data) {
  Element *elem;
  if (!(elem = *stack)) return false;

  *data = elem->data;
  *stack = elem->next;
  free(elem);
  return true;
}

bool deleteStack(Element **stack) {
  Element *next;
  while (*stack) {
    next = (*stack)->next;
    free(*stack);
    *stack = next;
  }
  return true;
}


int main() {
  /* gcc stack.c && ./a.out */
  printf("Hello World\n");

  Element *stack;
  createStack(&stack);

  int data;

  int data1 = 1;
  printf("p2\n");
  push(&stack, &data1);
  printf("p1\n");

  int data2 = 2;
  push(&stack, &data2);

  int data3 = 3;
  push(&stack, &data3);

  int *ret;
  printf("Halfway\n");

  pop(&stack, &ret);
  printf("Popped: %d\n", *ret);

  pop(&stack, &ret);
  printf("Popped: %d\n", *ret);

  pop(&stack, &ret);
  printf("Popped: %d\n", *ret);

  pop(&stack, &ret);
  printf("Popped: %d\n", *ret);

  pop(&stack, &ret);
  printf("Popped: %d\n", *ret);

  free(stack);
};
