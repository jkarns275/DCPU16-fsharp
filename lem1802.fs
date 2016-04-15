namespace DCPU16

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Content
open System.Collections

module font =
    let font = [|0x0000us;0x0000us;0x3E65us;0x653Eus;0x3E5Bus;0x5B3Eus;0x1E7Cus;0x1E00us;0x1C7Fus;0x1C00us;0x4C73us;0x4C00us;0x5C7Fus;0x5C00us;0x183Cus;0x1800us;
              0xE7C3us;0xE7FFus;0x1824us;0x1800us;0xE7DBus;0xE7FFus;0xE7DBus;0xE7FFus;0x2C72us;0x2C00us;0x607Fus;0x0507us;0x607Fus;0x617Fus;0x2A1Fus;0x7C2Aus;
              0x7F3Eus;0x1C08us;0x081Cus;0x3E7Fus;0x227Fus;0x7F22us;0x5F00us;0x5F00us;0x0609us;0x7F7Fus;0x9AA5us;0xA559us;0x6060us;0x6060us;0xA2FFus;0xFFA2us;
              0x027Fus;0x7F02us;0x207Fus;0x7F20us;0x1818us;0x3C18us;0x183Cus;0x1818us;0x3020us;0x2020us;0x081Cus;0x1C08us;0x707Eus;0x7E70us;0x0E7Eus;0x7E0Eus;
              0x0000us;0x0000us;0x005Fus;0x0000us;0x0700us;0x0700us;0x3E14us;0x3E00us;0x266Bus;0x3200us;0x611Cus;0x4300us;0x6659us;0xE690us;0x0005us;0x0300us;
              0x1C22us;0x4100us;0x4122us;0x1C00us;0x2A1Cus;0x2A00us;0x083Eus;0x0800us;0x00A0us;0x6000us;0x0808us;0x0800us;0x0060us;0x0000us;0x601Cus;0x0300us;
              0x3E4Dus;0x3E00us;0x427Fus;0x4000us;0x6259us;0x4600us;0x2249us;0x3600us;0x0E08us;0x7F00us;0x2745us;0x3900us;0x3E49us;0x3200us;0x6119us;0x0700us;
              0x3649us;0x3600us;0x2649us;0x3E00us;0x0066us;0x0000us;0x8066us;0x0000us;0x0814us;0x2241us;0x1414us;0x1400us;0x4122us;0x1408us;0x0259us;0x0600us;
              0x3E59us;0x5E00us;0x7E09us;0x7E00us;0x7F49us;0x3600us;0x3E41us;0x2200us;0x7F41us;0x3E00us;0x7F49us;0x4100us;0x7F09us;0x0100us;0x3E49us;0x3A00us;
              0x7F08us;0x7F00us;0x417Fus;0x4100us;0x2040us;0x3F00us;0x7F0Cus;0x7300us;0x7F40us;0x4000us;0x7F0Eus;0x7F00us;0x7E1Cus;0x7F00us;0x7F41us;0x7F00us;
              0x7F09us;0x0600us;0x3E41us;0xBE00us;0x7F09us;0x7600us;0x2649us;0x3200us;0x017Fus;0x0100us;0x7F40us;0x7F00us;0x1F60us;0x1F00us;0x7F30us;0x7F00us;
              0x771Cus;0x7700us;0x0778us;0x0700us;0x615Dus;0x4300us;0x007Fus;0x4100us;0x0618us;0x6000us;0x0041us;0x7F00us;0x0C06us;0x0C00us;0x8080us;0x8080us;
              0x0003us;0x0500us;0x2454us;0x7800us;0x7F44us;0x3800us;0x3844us;0x2800us;0x3844us;0x7F00us;0x3854us;0x5800us;0x087Eus;0x0900us;0x98A4us;0x7C00us;
              0x7F04us;0x7800us;0x047Dus;0x0000us;0x4080us;0x7D00us;0x7F10us;0x6C00us;0x417Fus;0x4000us;0x7C18us;0x7C00us;0x7C04us;0x7800us;0x3844us;0x3800us;
              0xFC24us;0x1800us;0x1824us;0xFC80us;0x7C04us;0x0800us;0x4854us;0x2400us;0x043Eus;0x4400us;0x3C40us;0x7C00us;0x1C60us;0x1C00us;0x7C30us;0x7C00us;
              0x6C10us;0x6C00us;0x9CA0us;0x7C00us;0x6454us;0x4C00us;0x0836us;0x4100us;0x0077us;0x0000us;0x4136us;0x0800us;0x0201us;0x0201us;0x704Cus;0x7000us|]

module lem1802 =
    let mutable counter = 0
    let mutable VRAM_PTR = 0x8000
    let mutable FONT_PTR = 0x8180
    let mutable PALETTE_PTR = 0x8280
    



    type Screen () as x =
        inherit Game ()
        do x.Content.RootDirectory <- "Content"

        let mutable counter = 0
        let mutable VRAM_PTR = 0x8000
        let mutable FONT_PTR = 0x8180
        let mutable PALETTE_PTR = 0x8280

        let mutable graphics = new GraphicsDeviceManager(x)
        let mutable spriteBatch = null
        
        let mutable texture: Texture2D = null

        let data: uint16 array = Array.zeroCreate (96 * 128)

        let getFBBC (w: uint16) =
            (w >>> 12) , ((w >>> 7) &&& 0x1us) , ((w >>> 8) &&& 0xFus) , w &&& 0x7fus
        let getColor (index: int) =
            let p = DCPU16.CPU.Ram.[1].[index + PALETTE_PTR]
            let (r,g,b) = p >>> 8 , (p >>> 4) &&& 0xFus, p &&& 0xFus
            uint16 ((b <<< 12) &&& (g <<< 8) &&& (r <<< 4) &&& 0xffus)
        let setFontPTR (i) = FONT_PTR <- i
        let setFont (font: uint16 [], ()) =
            for i in [0..255] do
                DCPU16.CPU.Ram.[1].[FONT_PTR + i] <- font.[i]

        let rec pow (x: uint32) (n: uint32) =
            match n with
            |   0u  -> 1u
            |   _   -> x * pow x (n - 1u)

        override x.Initialize () =
            do base.Initialize ()
            graphics.PreferredBackBufferHeight <- 96 * 3
            graphics.PreferredBackBufferWidth <- 128 * 3
            texture <- new Texture2D(graphics.GraphicsDevice, 128, 96, false, SurfaceFormat.Bgra4444)
            spriteBatch <- new SpriteBatch(graphics.GraphicsDevice)
            setFont (font.font, ())
            graphics.GraphicsDevice.SamplerStates.[0] <- SamplerState.PointClamp

        override x.Update gameTime =
            counter <- counter + 1
            DCPU16.CPU.tick ()
            DCPU16.CPU.dump ()
             
        override x.Draw gameTime =
            graphics.GraphicsDevice.Clear(Color.Black)
            ignore <| List.map (fun elem -> 
                let ins = DCPU16.CPU.Ram.[1].[elem]
                let (fg, bg, blink, char) = 0xffus, 0xffus, 0us, elem - VRAM_PTR
                if blink = 1us && counter % 120 > 60 then
                    ()
                else
                    let x = ((elem - VRAM_PTR) % 32) * 4
                    let y = ((elem - VRAM_PTR) / 32) * 8
                    let fc = getColor (int fg)
                    let bc = getColor (int bg)

                    //GL.DrawArrays (BeginMode.Triangles, 0, 6)

                    let charWords = (uint32 (DCPU16.CPU.asSigned DCPU16.CPU.Ram.[1].[FONT_PTR + 2 * int char]) <<< 16) ||| (uint32 (DCPU16.CPU.asSigned DCPU16.CPU.Ram.[1].[FONT_PTR + 2 * (int char) + 1]))

                    //GL.Color3 (r,g,b)
                    let mutable placeCount = 0
                    let mutable yc = 0
                    let mutable xc = 0
                    for pwr in [0u..31u] do
                        data.[xc + x + (y + (yc % 8)) * 128] <- match (pow 2u pwr) &&& charWords >= 1u with
                                                                |   true    -> fc
                                                                |   _       -> bc
                            
                        yc <- yc + 1
                        xc <- 3 - yc / 8
                    ()
            ) [VRAM_PTR..(VRAM_PTR + 384)-1]
            texture.SetData data
            spriteBatch.Begin ()
            spriteBatch.Draw(texture, Rectangle(0, 0, 128 * 3, 96 * 3), Color.White)
            spriteBatch.End ()

