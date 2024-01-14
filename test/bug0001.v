// The function print_bool() was compiled as
//
//   if b { print "true"; }
//   print "false";
//
// The else branch was executed unconditionally due to a
// bad simplification in SimplifyCFG.

fn print_bool(b) {
  if b {
    print "true";
  } else {
    print "false";
  }

  return;
}

fn main() {
  print_bool(true);
  return 0;
}
