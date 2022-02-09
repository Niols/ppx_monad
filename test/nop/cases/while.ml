let pf = Format.printf

let () =
  let i = ref 1 in
  while%nop pf "while\n"; !i <= 3 do
    pf "while %d\n" !i;
    incr i
  done

let () =
  let i = ref 3 in
  while%nop pf "while\n"; !i <= 1 do
    pf "while %d\n" !i;
    incr i
  done
