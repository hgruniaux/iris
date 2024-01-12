fn fact(n, acc) {
  if (n) {
    return fact(n - 1, acc * n);
  } else {
    return acc;
  }
}

fn main() {
  let x = fact(5, 1);
  print x;
  return 0;
}
