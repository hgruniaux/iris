fn main(n) {
  let a = n;
  let b = a + n;
  let c = a + b + n;
  let d = a + b + c + n;
  let e = a + b + c + d + n;
  let f = a + b + c + d + e + n;
  let g = a + b + c + d + e + f + n;
  let h = a + b + c + d + e + f + g + n;
  let i = a + b + c + d + e + f + g + h + 1;
  let j = a + b + c + d + e + f + g + h + i + 1;
  let k = a + b + c + d + e + f + g + h + i + j + 1;
  let l = a + b + c + d + e + f + g + h + i + j + k + 1;
  let m = a + b + c + d + e + f + g + h + i + j + k + l + 1;
  print m;
  return m;
}
