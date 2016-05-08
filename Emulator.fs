namespace DCPU16

module CPU =
    type Word = uint16
    let inline Word x = (uint16) x

    type VALUE = A = 0us | B = 1us | C = 2us | X = 3us | Y = 4us | Z = 5us | I = 6us | J = 7us | PC = 8us | SP = 9us | EX = 10us | IA = 11us | temp = 12us
    (*
      Ram.[0] ->
             * 0 -> 7 = A, B, C, X, Y, Z, I, J
             * 8 -> PC
             * 9 -> SP
             * 10 -> EX
             * 11 -> IA 
      Ram.[1] -> actual ram 
    *)
    let mutable Ram: uint16 array array = [| Array.zeroCreate 13 ; Array.zeroCreate 0x10000 ; Array.zeroCreate 0x100 |]
    let mutable indices = 0, 0

    let inline shr x by: Word = x >>> int by
    let inline ashr x by: Word = Word (int x >>> int by)
    let inline shl x by: Word = x <<< int by
    let inline band x y: Word = x &&& y
    let inline xor x y: Word = x ^^^ y
    let inline bor x y: Word = x ||| y

    let inline mem addr                 = Ram.[1].[int addr]
    let inline setmem addr value        = Ram.[1].[addr] <- value
    let inline regl (addr: VALUE)       = Ram.[0].[int addr]
    let inline reg (addr: Word)         = Ram.[0].[int addr]
    let inline setreg (addr: Word) n    = Ram.[0].[int addr] <- n
    let inline setregl (addr: VALUE) n  = Ram.[0].[int addr] <- n
    let inline setPC (value: Word)      = setregl VALUE.PC value
    let inline getPC ()                 = regl VALUE.PC
    let inline incPC ()                 = getPC () + 1us |> setPC
    let inline setSP (value: Word)      = setregl VALUE.SP value
    let inline getSP ()                 = regl VALUE.SP
    let inline setEX (value: Word)      = setregl VALUE.EX value 

    let dump () =
        printfn "\n\n___________________________________\nPC: %04x SP: %04x IA: %04x EX: %04x|" (regl VALUE.PC) (regl VALUE.SP) (regl VALUE.IA) (regl VALUE.EX) 
        printfn "___________________________________|\nA : %04x B : %04x C : %04x         |\n___________________________________|\nX : %04x Y : %04x Z : %04x         |\n___________________________________|\nI : %04x J : %04x                  |\n___________________________________|"
                    (reg 0us) (reg 1us) (reg 2us) (reg 3us) (reg 4us) (reg 5us) (reg 6us) (reg 7us)


    (* returns a, b, and o, respectively *)
    let isolate (word: Word) =
        (word >>> int 10) &&& 0x3fus, (word >>> int 5) &&& 0x1fus, word &&& 0x1fus

    let create (w: Word*Word*Word) =
        let (a,b,o) = w
        a |> band b |> band o

    let insArr = [| "";"SET";"ADD";"SUB";"MUL";"MLI";"DIV";
                    "DVI";"MOD";"MDI";"AND";"BOR";"XOR";
                    "SHR";"ASR";"SHL";"IFB";"IFC";"IFE";
                    "IFN";"IFG";"IFA";"IFL";"IFU";"";"";
                    "ADX";"SBX";"";"";"STI";"STD" |]

    let printIns (a: Word) (b: Word) (o: Word) =
        () //printfn "%s: %d , %d" insArr.[int o] b a  
                  
    let inline stack () = Ram.[1].[int VALUE.SP]

    (* Loads a program into memory *)
    let loadProgram (prog: Word list) = 
        for i in 0 .. prog.Length - 1 do
            Array.set Ram.[1] i prog.[i]
        ()

    let addOverflow (a: Word) (b: Word) (c: Word) =
        let sum = int a + int b + int c
        match sum with
        | sum when sum >= 0x10000    -> 1us
        | _                          -> 0us
    let subUnderflow (b: Word) (a: Word) =
        let sum = int b - int a
        match sum with
        | sum when sum < 0  -> 0xffffus
        | _                 -> 0us

    (* JUST *)
    let asSigned (x: Word) =
        let (sign, value) = int16 (x >>> 15),  int16 (x &&& 0x7fffus)
        int16 (value ||| (sign <<< 15))
    let asUnsigned (x: int16): Word =
        let (sign, value) = uint16 (x >>> 15),  uint16 (x &&& 0x7fffs)
        uint16 (value ||| (sign <<< 15))

    let nextIns () =
        let r = mem <| getPC ()
        incPC ()
        r  

    (* Decode an argument on the right side of an instruction *)
    let getA (a: Word) = 
        match int a with
        | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7             ->  reg <| a                    (* registers A,B,C,X,Y,Z,I,J in order*)
        | 8 | 9 | 0xA | 0xB | 0xC | 0xD | 0xE | 0xF ->  a - 8us |> reg |> mem       (* memory the address A,B,C,X,Y,Z,I,J in order*)
        | v when v <= 0x17  ->  let r = mem (reg (a - 16us) + mem (regl VALUE.PC))  (* memory at the address PC + [register] *)
                                incPC ()
                                r
        | 0x18 ->   let r = stack ()                                                (* the value on top of the stack and decreases stack pointer, POP *)
                    getSP () + 1us |> setSP
                    r
        | 0x19 ->   stack ()                                                        (* PEEK, gets value on top of stack *)
        | 0x1A ->   let r = getSP () + mem (getPC ()) |> mem
                    getPC () + 1us |> setPC
                    r
        | 0x1B ->   getSP ()                                                        (* stack pointer, SP *)
        | 0x1C ->   getPC ()                                                        (* program counter, PC *)
        | 0x1D ->   regl (VALUE.EX)                                                 (* extra register, EX *)
        | 0x1E ->   let r = getPC () |> mem |> mem                                  (* memory with the next word as the memory address *)
                    incPC()
                    r
        | 0x1f ->   let r = getPC () |> mem                                         (* the next word *)
                    incPC()
                    r
        | _    ->   a - 0x21us                                                      (* Literal value *)

    (* The same as the last function except every instruction sets something called indices, which are indexes on the 2d memory array that contain the proper operands *)
    let getB (b: Word) =
        let v = int b
        match v with
        | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 ->  indices <- 0, v
                                            reg <| Word v
        | 8 | 9 | 0xA | 0xB | 0xC | 0xD | 0xE | 0xF ->  let r = v - 8 |> Word |> reg 
                                                        indices <- 1, int r
                                                        mem r
        | v when v <= 0x17     ->   let r = reg (Word (v - 16)) + mem (regl VALUE.PC)
                                    indices <- 1, int r
                                    incPC ()
                                    mem r
        | 0x18 ->   getSP () - 1us |> setSP
                    indices <- 1, int <| getSP ()
                    stack ()
        | 0x19 ->   indices <- 1, int <| getSP ()
                    stack ()
        | 0x1A ->   let r = getSP () + mem (getPC ())
                    indices <- 1, int r
                    incPC()
                    mem r
        | 0x1B ->   indices <- 0, int VALUE.SP
                    getSP ()
        | 0x1C ->   indices <- 0, int VALUE.PC
                    getPC ()
        | 0x1D ->   indices <- 0, int VALUE.EX
                    regl (VALUE.EX)         
        | 0x1E ->   let r = getPC () |> mem
                    indices <- 1, int r
                    incPC()
                    mem r
        | 0x1f ->   let r = getPC ()
                    indices <- 1, int r
                    incPC()
                    mem r
        | _    ->   indices <- 0, 12
                    b - 0x21us
    (*
     * Run an instruction
     *)
    let tick () =
        let (ba, bb, bo) = nextIns () |> isolate
        let a = getA ba
        let bv = getB bb
        let (arrIndex, ind) = indices
        let mutable b = Ram.[arrIndex]
        let sb = asSigned bv
        let sa = asSigned a

        match bo with
            |   0x00us ->   match bb with
                            (* JSR *)
                            | 0x01us    ->  setSP (getSP () - 1us)
                                            Ram.[1].[int <| getSP ()] <- getPC () + 1us
                                            setPC a
                            (* INT *)
                            | 0x08us    -> () (* Unsupported *)
                            (* IAG *)
                            | 0x09us    -> ()
                            (* IAS *)
                            | 0x0Aus    -> ()
                            (* RFI *)
                            | 0x0Bus    -> ()
                            (* IAQ *)
                            | 0x0Cus    -> ()
                            (* HWN *)
                            | 0x10us    -> ()
                            (* HWQ *)
                            | 0x11us    -> ()
                            (* HWI *)
                            | 0x12us    -> ()
            (* SET *)
            |   0x01us ->   b.[ind] <- a
            (* ADD *)
            |   0x02us ->   b.[ind] <- bv + a
                            addOverflow bv a 0us |> setEX
            (* SUB *)
            |   0x03us ->   b.[ind] <- bv - a 
                            subUnderflow bv a |> setEX
            (* MUL *)
            |   0x04us ->   b.[ind] <- bv * a
                            (16 |> shr b.[ind] |> band 0xffffus) |> setEX
            (* MLI *)
            |   0x05us ->   b.[ind] <- asUnsigned <| sa * sb
                            (sa * sb) >>> 16 |> asUnsigned |> band 0xffffus |> setEX
            (* DIV *)
            |   0x06us ->   let _b, _EX = 
                                match a with
                                | 0us   -> 0us, 0us
                                | _     -> bv / a , ((bv <<< 16) / a) &&& 0xffffus
                            b.[ind] <- _b
                            uint16 ((int b.[ind]) * 0x10000) &&& 0xffffus |> setEX
            (* DVI *)
            |   0x07us ->   let _b, _EX = 
                                match a with
                                | 0us   -> 0us, 0us
                                | _     -> asUnsigned <| (sb / sa) , ((bv <<< 16) / a) &&& 0xffffus
                            b.[ind] <- _b
                            asUnsigned (int16 ((int (asSigned b.[ind])) * 0x10000)) &&& 0xffffus |> setEX

            (* MOD *)
            |   0x08us ->   let n =
                                match a with
                                | 0us   -> 0us
                                | _     -> bv % a
                            b.[ind] <- n
            (* MDI *)
            |   0x09us ->   let n =
                                match a with
                                | 0us   -> 0us
                                | _     -> asUnsigned <| sb % sa
                            b.[ind] <- n
            (* aND *)
            |   0x0aus ->   b.[ind] <- band bv a
            (* bOR *)
            |   0x0bus ->   b.[ind] <- bor bv a
            (* XOR *)
            |   0x0Cus ->   b.[ind] <- xor bv a
            (* SHR *)
            |   0x0Dus ->   b.[ind] <- shr bv a
                            (shr (shl bv 16) a) &&& 0xffffus |> setEX // (shr ((b.contents <<< 16), a)) &&& 0xffffus
            (* aSR *)
            |   0x0Eus ->   b.[ind] <- ashr bv a
                            (shr (shl bv 16) a) &&& 0xffffus |> setEX
            (* SHL *)
            |   0x0Fus ->   b.[ind] <- shl bv a
                            (shl (shr bv a) 16us) &&& 0xffffus |> setEX
            (* IFb *)
            |   0x10us ->   match band bv a with
                            | 0us -> ()
                            | _   -> incPC ()
            (* IFC *)
            |   0x11us ->   match band bv a with
                            | 0us -> incPC ()
                            | _   -> ()
            (* IFE *)
            |   0x12us ->   match a = bv with
                            | false -> incPC ()
                            | _    -> ()
            (* IFN *)
            |   0x13us ->   match a = bv with
                            | true -> incPC ()
                            | _    -> ()
            (* IFG *)
            |   0x14us ->   match bv > a with
                            | false -> incPC ()
                            | _    -> ()
            (* IFa *)
            |   0x15us ->   match sb > sa with
                            | false -> incPC ()
                            | _    -> ()
            (* IFL *)
            |   0x16us ->   match bv < a with
                            | false -> incPC ()
                            | _    -> ()
            (* IFU *)
            |   0x17us ->   match sb > sa with
                            | false -> incPC ()
                            | _    -> ()
            (* aDX *)
            |   0x1aus ->   b.[ind] <- bv + a + regl VALUE.EX
                            addOverflow bv a <| regl VALUE.EX |> setEX
            (* SbX *)
            |   0x1bus ->   b.[ind] <- bv - a + regl VALUE.EX
                            subUnderflow (bv + regl VALUE.EX) a |> setEX
            (* STI *)
            |   0x1Eus ->   b.[ind] <- a
                            Ram.[1].[6] <- Ram.[1].[6] + 1us
                            Ram.[1].[7] <- Ram.[1].[7] + 1us
            |   _      ->  ()
