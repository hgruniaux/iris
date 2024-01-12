fn a() { return 3; }
fn b() { return 5; }
fn c() { return -9; }

fn print_bool(b) {
  if b {
    print "true";
  } else {
    print "false";
  }

  return;
}

fn main() {
  let x = a(); // 3
  let y = b(); // 5
  let z = c(); // -9

  print("x and y");
  print_bool(x == y);
  print_bool(x != y);
  print_bool(x < y);
  print_bool(x <= y);
  print_bool(x > y);
  print_bool(x >= y);

  print("x and z");
  print_bool(x == z);
  print_bool(x != z);
  print_bool(x < z);
  print_bool(x <= z);
  print_bool(x > z);
  print_bool(x >= z);

  return 0;
}
