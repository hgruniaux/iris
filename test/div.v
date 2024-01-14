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

  // Power of twos
  print(x / 8);
  print(x % 8);

  // Algebraic identities
  print(x / 1);
  print(x / x);
  print(x % 1);
  print(x % x);

  return 0;
}
