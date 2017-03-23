open System

//Configs stored here after parsing header
type HeaderConfig = {
    audioVersion:byte
    layerDesc:byte
    protection:bool
    bitRate:int
    sampleRate:int
    padding:bool
    privateBit:int
    channelMode:byte
    modeExtension:byte
    copyright:bool
    original:bool
    emphasis:byte
}

//Side Info granule
type sideInfoGranule = {
    channel:int
    par23Length:array<int>
    bigValues:array<int>
    globalGain:array<int>
    scaleFactorCompress:array<int>
    windowSwitchFlag:bool
    blockType:array<int>
    mixedBlockFlag:bool
    tableSelect:array<array<int>>
    subBlockGain:array<array<int>>
    region0Count:array<int>
    region1Count:array<int>
    preflag:int
    scaleFactorScale:int
    count1TableSelect:int
}

//SideInfo
type SideInfoConfig = {
    mainDataBegin:array<int>
    privateBits:array<int>
    scfsi:array<array<int>>
    sideInfoGr0:array<sideInfoGranule>
    sideInfoGr1:array<sideInfoGranule>
}

//Main Data
type MainData = {
    channel:int
}

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

//Parse Header
let parseHeader (x:array<Byte>) = 
    
    let getBitrate x index = 
        let arr12 = [|0;32;48;56;64;80;96;112;128;160;192;224;256;320;384|]
        let arr13 = [|0;32;40;48;56;64;80;96;112;128;160;192;224;256;320|]
        match x with
        |(1,1) -> arr13.[index] * 1000;
        |(1,2) -> arr12.[index] * 1000;
        |(1,3) -> index * 32000;
        |(0,_) -> failwith "MPEG 2 unimplemented"
        |(_,_) -> failwith "Bitrate error"

    let getSamplerate x index = 
        match x,index with
        |(1,0) -> 44100
        |(1,1) -> 48000
        |(1,3) -> 32000
        |(_,_) -> failwith "SampleRate error"

    match x.Length = 4 with
    |false -> failwith "Not 4 bytes of header"
    |true -> 
        let bits = x |> Array.map int |> getBitsArrayfromByteArray
        let (config:HeaderConfig) = {
            audioVersion = bits.[12] |> byte;
            layerDesc = bits.[13..14] |> bitsArraytoNumber |> byte;
            protection = if bits.[15] = 0 then false else true;
            bitRate = bits.[16..19] |> bitsArraytoNumber |> getBitrate (bits.[12],bits.[13..14] |> bitsArraytoNumber);
            sampleRate = bits.[20..21] |> bitsArraytoNumber |> getSamplerate bits.[12]
            padding = if bits.[22] = 0 then false else true;
            privateBit = bits.[23];
            channelMode = bits.[24..25] |> bitsArraytoNumber |> byte;
            modeExtension = bits.[26..27] |> bitsArraytoNumber |> byte;
            copyright = if bits.[28] = 0 then false else true;
            original = if bits.[29] = 0 then false else true;
            emphasis = bits.[30..31] |> bitsArraytoNumber |> byte
        }
        config
        
//Parse side information
let parseSideConfig (x:array<Byte>) (y:HeaderConfig) = 
    //Outer function
    let getSideGranule channels data = 
        //Inner Function
        let extractGranule (x:array<int>) = 
            //Actual data
            let (granule:sideInfoGranule) = {
                channel = channels |> int
                par23Length = x.[0..11]
                bigValues = x.[12..20]
                globalGain = x.[21..28]
                scaleFactorCompress = x.[29..32]
                windowSwitchFlag = 
                    if x.[33] = 1 then true else false
                blockType = 
                    if x.[33] = 1 then x.[34..35]
                    else null
                mixedBlockFlag = 
                    if x.[33] = 1 
                        then x.[36] |> (fun x -> if x = 1 then true else false)
                    else 
                        false
                tableSelect = 
                    if x.[33] = 1
                        then [|x.[37..41];x.[42..46]|]
                    else
                        [|x.[34..38];x.[39..43];x.[44..48]|]
                subBlockGain = 
                    if x.[33] = 1 
                        then [|x.[47..49];x.[50..52];x.[53..55]|]
                    else
                        null
                region0Count = 
                    if x.[33] = 1
                        then null
                    else
                        x.[49..52]
                region1Count = 
                    if x.[33] = 1
                        then null
                    else
                        x.[53..55]
                preflag = x.[56]
                scaleFactorScale = x.[57]
                count1TableSelect = x.[58]
            }
            granule
        
        match channels with
        |3uy -> //For mono
            let (granuleArray:array<sideInfoGranule>) = Array.zeroCreate 1
            granuleArray.[0] <- extractGranule data //Channel 0
            granuleArray
        |0uy|1uy|2uy -> //For stereo
            let (granuleArray:array<sideInfoGranule>) = Array.zeroCreate 2
            granuleArray.[0] <- extractGranule data //Channel 0
            granuleArray.[1] <- extractGranule data.[59..] //Channel 1
            granuleArray
        |_ -> failwith "unknown channel mode"

    match (x.Length,y.channelMode) with
    |(17,3uy) -> //For mono
        let bitArray = x |> Array.map int |> getBitsArrayfromByteArray
        let (configs:SideInfoConfig) = {
            mainDataBegin = bitArray.[0..8]
            privateBits = bitArray.[9..13]
            scfsi = [|bitArray.[14..17]|]
            sideInfoGr0 = bitArray.[18..] |> getSideGranule y.channelMode
            sideInfoGr1 = bitArray.[77..] |> getSideGranule y.channelMode
        }
        configs
    |(32,0uy)|(32,1uy)|(32,2uy) -> //For dual channel modes
        let bitArray = x |> Array.map int |> getBitsArrayfromByteArray
        let (configs:SideInfoConfig) = {
            mainDataBegin = bitArray.[0..8]
            privateBits = bitArray.[9..11]
            scfsi = [|bitArray.[12..15];bitArray.[16..19]|]
            sideInfoGr0 = bitArray.[20..] |> getSideGranule y.channelMode
            sideInfoGr1 = bitArray.[138..] |> getSideGranule y.channelMode
        }
        configs
    |(_,_) -> //Any other cases
        failwith "Error while decoding "

//Parse Main Data
let parseMainData x = 
    x

//Test Data
let main = 
    let data = IO.File.ReadAllBytes "aframe.mp3"

    let header = data.[0..3] |> parseHeader
    printfn "Header\n\n%A\n" header
    header |> parseSideConfig data.[4..35] |> printfn "Side config\n\n%A\n"
