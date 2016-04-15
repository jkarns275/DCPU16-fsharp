namespace DCPU16
open System.Text.RegularExpressions
open System.Collections.Generic

module Assembler =
    (* 
     * Tagged union for the first pass of the assembler, contains 4 types that cover every possible valid statement
     *)
    type P1Ins3 = string*string*string
    type P1Ins2 = string*string
    type Label = string
    type P1Dat = string

    type P1Ins = Label of Label | P1Ins3 of P1Ins3 | P1Dat of P1Dat | P1Ins2 of P1Ins2
    type Arg = ThisWord of uint16 | NextWord of uint16*uint16

    let COMMA_SPACE = [|',';' '|]
    let COMMA = [|','|]
    let NEWL = [|'\n'|]
    let SEMIC = [|';'|]
    let SPACE = [|' '|]
    let registers = ["a";"b";"c";"x";"y";"z";"i";"j";"ia";"ex";"pc";"sp"]
    let registers_8 = ["a";"b";"c";"x";"y";"z";"i";"j"]
    let altRegisters = ["ia";"ex";"pc";"sp"]
    let instructions = dict[
                        "set", 0x01us; "add", 0x02us; "sub", 0x03us; "mul", 0x04us; "mli", 0x05us;
                        "div", 0x06us; "dvi", 0x07us; "mod", 0x08us; "mdi", 0x09us; "and", 0x0Aus;
                        "bor", 0x0Bus; "xor", 0x0Cus; "shr", 0x0Dus; "asr", 0x0Eus; "shl", 0x0Fus;
                        "ifb", 0x10us; "ifc", 0x11us; "ife", 0x12us; "ifn", 0x13us; "ifg", 0x14us;
                        "ifa", 0x15us; "ifl", 0x16us; "ifu", 0x17us; "adx", 0x1Aus; "sbx", 0x1Bus;
                        "sti", 0x1Eus; "std", 0x1Fus; "jsr", 0x01us]
    let labels = new Dictionary<string, uint16>()
    let mutable program: uint16 list = []

    let lines (s: string) = 
        let r = Regex.Replace(s, ";.+[\n\r]*", "\n")    // Remove comments
        let t = Regex.Replace(r, ":[a-zA-Z0-9]+", (fun m -> (m.ToString ()) + "\n"))    // Add a new line when a label is encountered
        printfn "%s" t
        t.Split NEWL |> Array.toList    // Split by \n

    let getRegIndex (s: string): uint16 =
        match s.ToLower () with
        | "a" -> 0us
        | "b" -> 1us
        | "c" -> 2us
        | "x" -> 3us
        | "y" -> 4us
        | "z" -> 5us
        | "i" -> 6us
        | "j" -> 7us
        | "sp" -> 0xBus
        | _ as x   -> raise (System.Exception ("Error: '" + x + "' canot be used in a pointer"))
    let isReg (s: string) =
        let a = s.ToLower ()
        "abcxyzij".Contains a
    let isRegAll (s: string) = 
        let a = s.ToLower ()
        List.exists (fun elem -> elem = a) registers

    let isInt (s: string) =
        if isReg s then None else
            if s.Contains "0x" then
                try
                    Some (DCPU16.CPU.asUnsigned <| System.Convert.ToInt16 (s, 16))
                with
                |   _ -> None
            else
                try
                    Some (DCPU16.CPU.asUnsigned <| System.Convert.ToInt16 s)
                with
                | _ -> None

    let finalizeArg (arg_dirty: string): Arg =
        let mutable arg = arg_dirty.Replace ("+", " + ")
        arg <- arg.Trim ()
        match arg.Chars 0 with
        | '[' ->
            arg <- (arg.Replace ("[", "")).Replace ("]", "")
            let splitDirty = arg.Split ([|'+'|], System.StringSplitOptions.RemoveEmptyEntries)
            let split = Array.map (fun (elem: string) -> elem.Trim ()) splitDirty
            match split.Length with
            | 1 ->
                match isInt split.[0] with
                | Some(x)   ->
                    NextWord (0x1Eus, x)
                | None      -> 
                    if isRegAll arg then
                        ThisWord (0x08us + getRegIndex arg) 
                    else
                        NextWord(0x1Eus, labels.[arg])

            | 2 ->
                let mutable r2 = 0
                let p1 = match isRegAll split.[0] with
                            | true  -> split.[1]
                            | false -> split.[0]
                let p2 = if p1 = split.[0] then split.[1] else split.[0]    // p2 is gaurenteed to be a register (if it is a valid program)
                let n = match isInt p1 with
                            | Some(x)   -> x
                            | None      -> if labels.ContainsKey p1 then labels.[p1] else raise (System.Exception ("Error: unknown constant / label '" + p1 + "'"))
                match p2.ToLower () with
                | "sp" -> NextWord (0x1Aus, n)
                | _ as x ->
                    if isReg p2 then
                        NextWord(getRegIndex p2 + 0x10us, n)
                    else
                        raise (System.Exception ("Error: the register '" + p2 + "' is either not a valid register or cannot be used as a pointer"))
                    
                    
            | _ -> raise (System.Exception ("Error: Invalid number of arguments in pointer, '" + arg + "'. (Compile time arithmatic is not supported)"))
        | _ -> 
            match isInt arg with
            | Some(x)   ->
                if x = 0xffffus || x <= 30us then ThisWord (x + 0x21us) else NextWord (0x1Fus, x)
            | None      -> 
                match arg.ToLower () with
                | "pop" | "push"    -> ThisWord (0x18us)
                | "sp"              -> ThisWord (0x1Bus)
                | "pc"              -> ThisWord (0x1Cus)
                | "ex"              -> ThisWord (0x1Dus)
                | _                 ->                  
                    if isReg arg then
                        ThisWord (getRegIndex arg)                
                    else
                        if labels.ContainsKey arg then
                            printfn "aaaa : %s" arg
                            NextWord (0x1Fus, labels.[arg])
                        else
                            raise (System.Exception ("Invalid instruction operand '" + arg + "'"))

    let finalizeIns (insDirty: string) =
        let ins = insDirty.ToLower ()
        if instructions.ContainsKey (ins.ToLower ()) then instructions.[ins] else raise (System.Exception  ("Error: unknown opcode '" + ins + "'"))
    let combine (a: uint16, b: uint16, o: uint16) =
        o ||| (b <<< 5) ||| (a <<< 10)
    (*
     * Tokenizes every line of the program into a P1Ins list
     * Valid instructions are laid out like:
     * :label
     * (INS) (b), (a)
     * or
     * (INS) (b)
     * (DAT) (arg0) (arg1) ... (argN)
     *)
    let rec firstPass (acc: P1Ins list) (lines: string list): P1Ins list =
        match lines with
        | [] -> acc
        | dirtyIns :: tail ->
            let ins = Array.toList <| dirtyIns.Split (COMMA_SPACE, System.StringSplitOptions.RemoveEmptyEntries)
            match ins.Length with
            | 0 -> firstPass acc tail                                           // empty line
            | 1 -> firstPass (acc @ [Label ins.[0]]) tail                       // Label
            | 2 -> match ins.[0] with
                |   "dat"   -> firstPass (acc @ [P1Dat dirtyIns]) tail
                |   _       -> firstPass (acc @ [P1Ins2  (ins.[0], ins.[1])]) tail            // Instruction with 1 argument (2 chunks)
            | 3 -> match ins.[0] with
                |   "dat"   -> firstPass (acc @ [P1Dat dirtyIns]) tail
                |   _       -> firstPass (acc @ [P1Ins3  (ins.[0], ins.[1], ins.[2])]) tail            // Instruction with 2 argument (2 chunks)
            | _ -> firstPass (acc @ [P1Dat dirtyIns]) tail                        // Dat instruiction with n arguments (will be handled later)

    let rec thirdPass (program: uint16 list) (ins: P1Ins list): uint16 list =
        match ins with
        | [] -> program
        | dirtyIns :: tail ->
            match dirtyIns with
            | P1Ins2 (ins, b)       ->
                printfn "%s %s" ins b 

                let arg = finalizeArg b
                let instruction = finalizeIns ins
                match arg with
                | ThisWord (x) -> thirdPass (program @ [combine (x, instruction, 0us)]) tail
                | NextWord (x, nextWord) -> thirdPass (program @ [combine (x, instruction, 0us); nextWord]) tail
            | P1Ins3 (ins, b, a)    ->
                let argb = finalizeArg b
                let arga = finalizeArg a
                let o = finalizeIns ins
                match arga with
                | ThisWord (x1) -> 
                    match argb with
                    | ThisWord (x2) ->
                        thirdPass (program @ [combine (x1, x2, o)]) tail
                    | NextWord (x2, nextWord2) ->
                        thirdPass (program @ [combine (x1, x2, o); nextWord2]) tail
                | NextWord (x1, nextWord1) ->
                    match argb with
                    | ThisWord (x2) ->
                        thirdPass (program @ [combine (x1, x2, o); nextWord1]) tail
                    | NextWord (x2, nextWord2) ->
                        thirdPass (program @ [combine (x1, x2, o); nextWord1; nextWord2]) tail
            | P1Dat dat             ->
                let arg = (dat.Trim ()).Remove(0,4).Trim ()
                match arg.Chars 0 with
                | '"' ->
                    let isolated = arg.Replace ("\"", "")
                    let c = Array.map (fun (elem: char) -> uint16 elem) (isolated.ToCharArray ())
                    thirdPass  (program @ Array.toList c) tail

                | _ ->
                    let sanitized = dat.Remove(0, 4).Split(COMMA, System.StringSplitOptions.RemoveEmptyEntries)
                    let converted: uint16 [] = Array.map (fun (dirty: string) ->
                                                let elem = dirty.Trim ()
                                                match isInt elem with
                                                | Some(x) -> x
                                                | None ->   if labels.ContainsKey (elem) then
                                                                labels.[elem]
                                                            else
                                                                raise (System.Exception ("Error: '" + elem + "' is not a valid constant or label"))
                                                            ) sanitized
                    thirdPass (program @ Array.toList converted) tail

            | Label l               -> thirdPass program tail

    let sizeOfArg (dirty: string) =
        let mutable arg = dirty.Replace ("+", " + ")
        let n = 
                match arg.Chars 0 with
                | '[' ->
                    arg <- (arg.Replace ("[", "")).Replace ("]", "")
                    let split = arg.Split ([|'+'|], System.StringSplitOptions.RemoveEmptyEntries)
                    match split.Length with
                    | 1 ->
                        match isInt split.[0] with
                        | Some(_)   -> 1us
                        | None      -> 
                            if isReg arg then 0us else 1us
                    | 2 ->
                        1us
                    | _ -> 0us
                | _ -> 
                    match isInt arg with
                    | Some(x)   ->
                        if x = 0xffffus || x <= 30us then 0us else 1us
                    | None      -> 
                        if isRegAll arg then 
                            0us 
                        else 
                            match arg.ToLower () with
                            | "pop" | "push" -> 0us
                            | _ -> 1us
        n


    let rec secondPass (acc: P1Ins list) (pcLast: uint16) (prog: P1Ins list): P1Ins list =
        match prog with
        | [] -> acc
        | head :: tail -> 
            let mutable pc = pcLast
            match head with
            | Label n ->
                labels.[n.Replace (":","")] <- pc
                secondPass (acc) pc tail
            | P1Ins2 (ins, b) ->
                secondPass (acc @ [head]) (pc + sizeOfArg b + 1us) tail
            | P1Ins3 (ins, b, a) ->
                secondPass (acc @ [head]) (pc + sizeOfArg b + sizeOfArg a + 1us) tail
            | P1Dat dat ->
                if dat.Contains "\"" then   secondPass (acc @ [head]) (uint16 (dat.Split [|','|]).Length) tail
                else                        
                    let mutable s = (dat.Split ([|" \""|], System.StringSplitOptions.RemoveEmptyEntries)).[1]
                    printfn  "nig %s" s
                    if s.Chars (s.Length - 1) <> '"' then
                        s <- s + " \""

                    secondPass (acc @ [head]) (uint16 s.Length) tail


    let assemble (program: string) =
        let r = program.ToLower () |> lines |> firstPass [] |> secondPass [] 0us |> thirdPass []
        for word in r do printf "0x%04x " word
        r
    let print (l: list<P1Ins>) = 0

    let test (s: string) = 
        let prog = assemble s
        for i in prog do printf "%04x " i
        ()