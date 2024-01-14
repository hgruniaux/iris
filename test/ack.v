fn ack(m, n) {
  if (m == 0) {
    return n + 1;
  } else if (n == 0) {
    return ack(m - 1, 1);
  } else {
    return ack(m - 1, ack(m, n - 1));
  }
}

fn main() {
  print ack(2, 3);
  print ack(3, 2);
  return 0;
}
