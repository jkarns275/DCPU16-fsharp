open DCPU16

[<EntryPoint>]
let main argv = 
    let source = "7C01 0030 7FC1 0020 1000 7803 1000 C413 7F81 0019 ACC1 7C01 2000 22C1 2000 88C3 84D3 BB81 9461 7C20 0017 7F81 0019 946F 6381 EB81"
    let words = source.Split [|' '|]
    let parse (x: string) = System.UInt16.Parse (x, System.Globalization.NumberStyles.HexNumber)
    let program = words |> Seq.map parse
    let prog = Seq.toArray program 
    CPU.loadProgram prog
    while true do ()
    0