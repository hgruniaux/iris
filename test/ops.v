fn a() { return 2; }
fn b() { return 3; }

fn main() {
  // With two constants
  print "two constants";
  print 2 + 3;
  print 2 - 3;
  print 2 * 3;
  print 8 / 2;
  print 13 % 5;
  print 3 << 4;
  print 16 >> 2;

  // With a single constant
  print "one constant";
  let x = a();
  print x + 3;
  print x - 3;
  print x * 3;
  print (4 * x) / 2;
  print (5 * x + 3) % 5;
  print (x + 1) << 4;
  print (x * 8) >> 2;

  // With no constants
  print "no constant";
  let y = b();
  print x + y;
  print x - y;
  print x * y;
  print (4 * x) / (y - 1);
  print (5 * x + 3) % (y + 2);
  print (x + 1) << (2 * x);
  print (x * 8) >> x;

  return 0;
}
