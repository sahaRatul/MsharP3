namespace FSharpMP3

module Utils = 
    //Get a Array of bits from a Array of bytes
    let getBitsArrayfromByteArray x = 
        let getBits x = 
            let y = [|0;0;0;0;0;0;0;0|]
            y.[7] <- (x >>> 0 &&& 0x01)
            y.[6] <- (x >>> 1 &&& 0x01)
            y.[5] <- (x >>> 2 &&& 0x01)
            y.[4] <- (x >>> 3 &&& 0x01)
            y.[3] <- (x >>> 4 &&& 0x01)
            y.[2] <- (x >>> 5 &&& 0x01)
            y.[1] <- (x >>> 6 &&& 0x01)
            y.[0] <- (x >>> 7 &&& 0x01)
            y
        x |> Array.map getBits |> Array.concat

    //Generate Number from array of bits
    let bitsArraytoNumber x = 
        x 
        |> Array.zip[|(x.Length-1)..(-1)..0|] 
        |> Array.sumBy (fun x -> snd x * (pown 2 (fst x)))

    //Extract bits from a bits array and convert to number
    let getBits count (x:array<byte>) = 
        (x.[0..(count-1)] |> Array.map int |> bitsArraytoNumber,x.[count..])