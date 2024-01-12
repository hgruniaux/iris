fn fib(n) {
  print n;
  if n < 2 {
    return n;
  } else {
    let x = n - 1;
    let y = n - 2;
    return fib(n - 1) + fib(n - 2);
  }
}

fn main() {
  // print fib(0);
  // print fib(1);
  // print fib(2);
  // print fib(3);
  // print fib(4);
  // print fib(5);
  print fib(6);
  return 0;
}
