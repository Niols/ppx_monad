let pf = Format.printf

let () =
  for%nop i = (pf "1\n"; 1) to (pf "3\n"; 3) do
    for%nop j = (pf "75\n"; 75) downto (pf "71\n"; 71); do
      pf "for %d %d\n" i j
    done
  done
