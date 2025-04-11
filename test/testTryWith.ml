let x = 0 in
try
  if x = 0 then raise (E 10) else 1
with
  |E i -> i