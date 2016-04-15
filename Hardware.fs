namespace DCPU16

module Hardware =

    type Hardware (ID: uint32, version: uint16, manufacturer: uint32) as hw =
        do ()
        (* Returns new values for A, B, C, X, Y in that order, which all give information about the hardware.
         * A + (B << 16) is a 32 bit word identifying the hardware
         * C is the hardware version
         * X + (Y << 16) is a 32 bit word identifying the manufacturer
         *)
        let HWQ () = (uint16 (ID &&& 0xffffu), uint16 ((ID &&& 0xffff0000u) >>> 16), version, uint16 (manufacturer &&& 0xffffu), uint16 ((manufacturer &&& 0xffff0000u) >>> 16))
        new () = Hardware (0u, 0us, 0u)

    type lem1802 () as hw =
        inherit Hardware ()


