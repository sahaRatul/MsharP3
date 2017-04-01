namespace MsharP3

module Utils = 
    //Get a Array of bits from a Array of bytes
    let getBitsArrayfromByteArray x = 
        let getBits x = 
            let y = [|0uy;0uy;0uy;0uy;0uy;0uy;0uy;0uy|]
            y.[7] <- (x >>> 0 &&& 0x01uy)
            y.[6] <- (x >>> 1 &&& 0x01uy)
            y.[5] <- (x >>> 2 &&& 0x01uy)
            y.[4] <- (x >>> 3 &&& 0x01uy)
            y.[3] <- (x >>> 4 &&& 0x01uy)
            y.[2] <- (x >>> 5 &&& 0x01uy)
            y.[1] <- (x >>> 6 &&& 0x01uy)
            y.[0] <- (x >>> 7 &&& 0x01uy)
            y
        x |> Array.map getBits |> Array.concat

    //Generate Number from array of bits (x<32 bits
    let bitsArraytoNumber (x:array<byte>) = 
        let mutable out = 0
        let len = x.Length
        for i = 0 to (len-1) do
            out <- (out <<< 1) ||| (x.[i] |> int)
        out

    //Extract bits from a bits array and convert to number
    let getBits count (x:array<byte>) = 
        let temp = (x.[0..(count-1)])
        let ret = (temp |> bitsArraytoNumber,x.[count..])
        ret

    //For 32 bit
    let bitsArraytoNumber32 x = 
        let mutable out = 0ul
        for bit in x do
            out <- (out <<< 1) ||| bit
        out

    let getBits32 start count (x:array<byte>) = 
        let temp = (x.[start..(start + count-1)])
        let ret = (temp |> Array.map uint32 |> bitsArraytoNumber32)
        (ret,start + count)

    let getBits2 start count (x:array<byte>) = 
        let result = 
            x.[start..(start + count - 1)] |> bitsArraytoNumber
        (result,start+count)
