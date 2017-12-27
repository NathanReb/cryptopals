let fold f acc stream =
  let acc = ref acc in
  Stream.iter (fun x -> acc := f !acc x) stream;
  !acc
