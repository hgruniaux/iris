fn f(n) {
  if n == 0 {
    return 0;
  } else {
    return f(f(n - 1));
  }
}

fn main() {
  print f(5);
  return 0;
}
