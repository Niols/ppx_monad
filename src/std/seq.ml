let return = Stdlib.Seq.return

let bind e f = Stdlib.Seq.flat_map f e
