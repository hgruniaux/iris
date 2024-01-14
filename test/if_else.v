fn non_constant_true() {
  return true;
}

fn non_constant_false() {
  return false;
}

fn main() {
  // Check for constant conditions.
  if true {
    print "Ok";
  } else {
    print "Oops, false branch taken";
  }

  if false {
    print "Oops, true branch taken";
  } else {
    print "Ok";
  }

  // Check for non-constant conditions.
  if non_constant_true() {
    print "Ok";
  } else {
    print "Oops, false branch taken";
  }

  if non_constant_false() {
    print "Oops, true branch taken";
  } else {
    print "Ok";
  }

  return 0;
}
