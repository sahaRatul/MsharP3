namespace FSharpMP3

open Utils
open Header

module Sideinfo = 
    //Side Info granule
    type sideInfoGranule = {
        granule:int
        channel:int
        par23Length:int
        bigValues:int
        globalGain:int
        scaleFactorCompress:int
        windowSwitchFlag:bool
        blockType:int
        mixedBlockFlag:bool
        tableSelect:array<int>
        subBlockGain:array<int>
        region0Count:int
        region1Count:int
        preflag:int
        scaleFactorScale:int
        count1TableSelect:int
    }

    //SideInfo
    type SideInfoConfig = {
        mainDataBegin:int
        privateBits:int
        scfsi:array<array<int>>
        sideInfoGr:array<sideInfoGranule>
    }

    let parseSideConfig (x:array<byte>) (y:HeaderConfig) = 
        //Outer function
        let getSideGranule channels granule data = 
            //Inner Function
            let extractGranule (x:array<int>) gr ch = 
                //Actual data
                let (granule:sideInfoGranule) = {
                    granule = gr
                    channel = ch
                    par23Length = x.[0..11] |> bitsArraytoNumber
                    bigValues = x.[12..20] |> bitsArraytoNumber
                    globalGain = x.[21..28] |> bitsArraytoNumber
                    scaleFactorCompress = x.[29..32] |> bitsArraytoNumber
                    windowSwitchFlag = 
                        if x.[33] = 1 then true else false
                    blockType = 
                        if x.[33] = 1 then x.[34..35] |> bitsArraytoNumber
                        else 100000
                    mixedBlockFlag = 
                        if x.[33] = 1 
                            then x.[36] |> (fun x -> if x = 1 then true else false)
                        else 
                            false
                    tableSelect = 
                        if x.[33] = 1
                            then [|x.[37..41] |> bitsArraytoNumber ;x.[42..46] |> bitsArraytoNumber|]
                        else
                            [|
                            x.[34..38] |> bitsArraytoNumber
                            x.[39..43] |> bitsArraytoNumber
                            x.[44..48] |> bitsArraytoNumber
                            |]
                    subBlockGain = 
                        if x.[33] = 1 
                            then [|
                                    x.[47..49] |> bitsArraytoNumber
                                    x.[50..52] |> bitsArraytoNumber
                                    x.[53..55] |> bitsArraytoNumber
                                |]
                        else
                            null
                    region0Count = 
                        if x.[33] = 1
                            then if (x.[34..35] |> bitsArraytoNumber) = 2 
                                    then 8 
                                    else 7
                        else
                            x.[49..52] |> bitsArraytoNumber
                    region1Count = 
                        if x.[33] = 1
                            then 20 - (if (x.[34..35] |> bitsArraytoNumber) = 2 
                                            then 8 
                                            else 7)
                        else
                            x.[53..55] |> bitsArraytoNumber
                    preflag = x.[56]
                    scaleFactorScale = x.[57]
                    count1TableSelect = x.[58]
                }
                granule
            
            match channels with
            |3uy -> //For mono
                let (granuleArray:array<sideInfoGranule>) = Array.zeroCreate 1
                granuleArray.[0] <- extractGranule data granule 0  //Channel 0
                granuleArray
            |0uy|1uy|2uy -> //For stereo
                let (granuleArray:array<sideInfoGranule>) = Array.zeroCreate 2
                granuleArray.[0] <- extractGranule data granule 0 //Channel 0
                granuleArray.[1] <- extractGranule data.[59..] granule 1 //Channel 1
                granuleArray
            |_ -> failwith "unknown channel mode"

        match (x.Length,y.channelMode) with
        |(17,3uy) -> //For mono
            let bitArray = x |> Array.map int |> getBitsArrayfromByteArray
            let (configs:SideInfoConfig) = {
                mainDataBegin = bitArray.[0..8] |> bitsArraytoNumber
                privateBits = bitArray.[9..13] |> bitsArraytoNumber
                scfsi = [|bitArray.[14..17]|] 
                sideInfoGr = 
                    [|
                        (bitArray.[18..] |> getSideGranule y.channelMode 0).[0]
                        (bitArray.[77..] |> getSideGranule y.channelMode 1).[1]
                    |]
            }
            configs
        |(32,0uy)|(32,1uy)|(32,2uy) -> //For dual channel modes
            let bitArray = x |> Array.map int |> getBitsArrayfromByteArray
            let (configs:SideInfoConfig) = {
                mainDataBegin = bitArray.[0..8] |> bitsArraytoNumber
                privateBits = bitArray.[9..11] |> bitsArraytoNumber
                scfsi = [|bitArray.[12..15];bitArray.[16..19]|]
                sideInfoGr = 
                    [|
                        (bitArray.[20..] |> getSideGranule y.channelMode 0).[0]
                        (bitArray.[20..] |> getSideGranule y.channelMode 0).[1]
                        (bitArray.[138..] |> getSideGranule y.channelMode 1).[0]
                        (bitArray.[138..] |> getSideGranule y.channelMode 1).[1]
                    |]
            }
            configs
        |(_,_) -> //Any other cases
            failwith "Error while decoding side config"
