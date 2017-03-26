namespace FSharpMP3

open Utils
open Header
open Frame
open Sideinfo

module Maindata = 
    type ScaleFactorsLong = {
        granule:int
        channel:int
        data:int []
    }

    type ScaleFactorsShort = {
        granule:int
        channel:int
        data:int [,]
    }

    type ScaleFactors = 
    |Short of ScaleFactorsShort
    |Long of ScaleFactorsLong
    |Mixed of (ScaleFactorsShort * ScaleFactorsLong)

    //Parse Main data from frame
    let parseMainData (data:array<byte>) (header:HeaderConfig) (sideinfo:SideInfoConfig) = 
        let arrayBits = data |> Array.map int |> getBitsArrayfromByteArray
        let mutable bitcount = 0
        let parseScaleFactors (x:array<byte>) (y:sideInfoGranule) = 
            let slen = 
                [|
                    [|0;0|];[|0;1|];[|0;2|];[|0;3|];[|3;0|];[|1;1|];[|1;2|];[|1;3|];
                    [|2;1|];[|2;2|];[|2;3|];[|3;1|];[|3;2|];[|3;3|];[|4;2|];[|4;3|];
                |]
            let scaleFactorLengths = [|slen.[y.scaleFactorCompress].[0];slen.[y.scaleFactorCompress].[1]|]
            let (result:ScaleFactors) = 
                match (y.blockType,y.windowSwitchFlag) with
                |(2,true) -> //Mixed/Short scalefactors
                    match y.mixedBlockFlag with
                    |true -> //Mixed
                        let (longScale:ScaleFactorsLong) = {
                            granule = y.granule
                            channel = y.channel
                            data = 
                                let temp = Array.zeroCreate 8
                                let mutable arr = x
                                for i = 0 to 7 do
                                    let (num,tmp) = arr |> getBits scaleFactorLengths.[0]
                                    arr <- tmp
                                    temp.[i] <- num
                                bitcount <- bitcount + (8 * scaleFactorLengths.[0])
                                temp
                        }
                        let (shortScale:ScaleFactorsShort) = {
                            granule = y.granule
                            channel = y.channel
                            data = 
                                let temp = Array2D.zeroCreate 3 13
                                let mutable arr = x
                                for sfb = 3 to 5 do
                                    for win = 0 to 2 do
                                        let (num,tmp) = arr |> getBits scaleFactorLengths.[0]
                                        arr <- tmp
                                        temp.[win,sfb] <- num
                                for sfb = 6 to 11 do
                                    for win = 0 to 2 do
                                        let (num,tmp) = arr |> getBits scaleFactorLengths.[1]
                                        arr <- tmp
                                        temp.[win,sfb] <- num
                                bitcount <- bitcount + (9 * scaleFactorLengths.[0])
                                bitcount <- bitcount + (18 * scaleFactorLengths.[1])
                                temp
                        }
                        Mixed (shortScale,longScale)
                    |false -> //Short
                        let (shortScale:ScaleFactorsShort) = {
                            granule = y.granule
                            channel = y.channel
                            data = 
                                let temp = Array2D.zeroCreate 3 13
                                let mutable arr = x
                                for sfb = 0 to 5 do
                                    for win = 0 to 2 do
                                        let (num,tmp) = arr |> getBits scaleFactorLengths.[0]
                                        arr <- tmp
                                        temp.[win,sfb] <- num
                                for sfb = 6 to 11 do
                                    for win = 0 to 2 do
                                        let (num,tmp) = arr |> getBits scaleFactorLengths.[1]
                                        arr <- tmp
                                        temp.[win,sfb] <- num
                                temp
                        }
                        bitcount <- bitcount + (18 * scaleFactorLengths.[0])
                        bitcount <- bitcount + (18 * scaleFactorLengths.[1])
                        Short (shortScale)
                |(_,_) -> //Long scalefactors
                    match y.granule = 0 with
                    |true -> //Granule 0
                        let (longScale:ScaleFactorsLong) = {
                            granule = y.granule
                            channel = y.channel
                            data = 
                                let temp = Array.zeroCreate 20
                                let mutable arr = x
                                for sfb = 0 to 9 do
                                    let (num,tmp) = arr |> getBits scaleFactorLengths.[0]
                                    arr <- tmp
                                    temp.[sfb] <- num
                                for sfb = 10 to 19 do
                                    let (num,tmp) = arr |> getBits scaleFactorLengths.[0]
                                    arr <- tmp
                                    temp.[sfb] <- num
                                temp
                        }
                        bitcount <- bitcount + 20
                        Long (longScale)
                    |false -> //Granule 1 (maybe with scalefactor reuse)
                        let (longScale:ScaleFactorsLong) = {
                            granule = y.granule
                            channel = y.channel
                            data = Array.zeroCreate 20 //Handle this later
                        }
                        bitcount <- bitcount + 20
                        Long (longScale)
            result
        
        let (sclfactors:array<ScaleFactors>) = Array.zeroCreate 4
        for i = 0 to 3 do
            let maxbit = bitcount + sideinfo.sideInfoGr.[i].par23Length
            sclfactors.[i] <- parseScaleFactors (arrayBits.[bitcount..] |> Array.map byte) sideinfo.sideInfoGr.[i]
            bitcount <- maxbit
        sclfactors

module Huffman = 
    let parseHuffmanData (frame:FrameInfo) (granule:sideInfoGranule) = 
        ()