fn f(a, b) {
  return a + b * 2;
}

fn main() {
  let x = f(3, 4); // 3 + 4 * 2 = 11
  print x;
  let y = 3;
  print y;
  let z = x / y;
  let w = x % y;
  print z;
  print w;
  return 0;
}
