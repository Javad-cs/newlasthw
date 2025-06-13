#include "homework.h"

int main() {
  int x = source();
  int y;
  if (x > 0) {
    y = 1;
  } else {
    y = sanitizer(x);
  }
  sink(x); // bug
  return 0;
}
