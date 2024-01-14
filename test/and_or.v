fn print_bool(b) {
  if b {
    print "true";
  } else {
    print "false";
  }

  return;
}

fn main() {
  // Test with constants.
  print_bool(false && false);
  print_bool(true && false);
  print_bool(false && true);
  print_bool(true && true);

  // Test with constants.
  print_bool(false || false);
  print_bool(true || false);
  print_bool(false || true);
  print_bool(true || true);

  return 0;
}
