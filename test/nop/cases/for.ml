let pf = Format.printf

let () =
  for%nop i = (pf "1\n"; 1) to (pf "3\n"; 3) do
    pf "for %d\n" i
  done

let () =
  for%nop i = (pf "1\n"; 1) downto (pf "3\n"; 3) do
    pf "for %d\n" i
  done

let () =
  for%nop i = (pf "5\n"; 5) to (pf "2\n"; 2) do
    pf "for %d\n" i
  done

let () =
  for%nop i = (pf "5\n"; 5) downto (pf "2\n"; 2) do
    pf "for %d\n" i
  done
