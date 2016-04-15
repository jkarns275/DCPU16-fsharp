open DCPU16
open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open OpenTK.Input

[<EntryPoint>]
let main argv =

    let program = "
    set a, [data]
    set pc, crash
    :data dat \" aaaaaaaaaaaaaa aaaaaaa\"
    :crash 
    set pc, crash
    "
    program |> DCPU16.Assembler.assemble |> DCPU16.CPU.loadProgram
    //DCPU16.CPU.dump ()
    use g = new DCPU16.lem1802.Screen ()
    g.Run ()
    0