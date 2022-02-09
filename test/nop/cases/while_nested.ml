let pf = Format.printf

let () =
  let i = ref 1 in
  let j = ref 34 in
  while%nop pf "while\n"; !i <= 3 do
    while%nop pf "while %d\n" !i; !j > 31 do
      pf "while %d %d\n" !i !j;
      decr j
    done;
    incr i
  done
