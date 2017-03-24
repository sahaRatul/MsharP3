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
    scfsi:array<int>
    sideInfoGr:array<sideInfoGranule>
}

type ScaleFactorsLong = {
    granule:int
    channel:int
    data:array<int>
}

type ScaleFactorsShort = {
    granule:int
    channel:int
    window:int
    data:array<int>
}

type Samples = {
    group:int
    channel:int
    sampledata:array<int>
}

type FrameInfo = {
    frameSize:int
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
        
//Get Frame frame Size
let getFrameSize (x:HeaderConfig) = 
    let samplesCount = 
        match (x.audioVersion,x.layerDesc) with
        |(3uy,1uy) -> 1152
        |(3uy,_) -> 576
        |(_,2uy) -> 1152
        |(_,3uy) -> 384
        |(_,_) -> failwith "Unknown error while getting framesize"
    let (frameinfo:FrameInfo) = {
        frameSize = (samplesCount / 8) * x.bitRate * x.sampleRate
    }
    frameinfo

//Parse side information
let parseSideConfig (x:array<Byte>) (y:HeaderConfig) = 
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
            scfsi = [|bitArray.[14..17]|] |> Array.map bitsArraytoNumber
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
            scfsi = [|bitArray.[12..15];bitArray.[16..19]|] |> Array.map bitsArraytoNumber
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

let parseMainData (data:array<Byte>) (header:HeaderConfig) (sideinfo:SideInfoConfig) = 
    let parseScaleFactors (x:array<byte>) (y:sideInfoGranule) = 
        let slen = 
            [|
                [|0;0|];[|0;1|];[|0;2|];[|0;3|];[|3;0|];[|1;1|];[|1;2|];[|1;3|];
                [|2;1|];[|2;2|];[|2;3|];[|3;1|];[|3;2|];[|3;3|];[|4;2|];[|4;3|];
            |]
        let scalefactorLength = [|slen.[y.scaleFactorCompress].[0];slen.[y.scaleFactorCompress].[1]|]
        let lScaleFactor = Array.zeroCreate 22
        let sScaleFactors = Array2D.zeroCreate 3 13
        let result = 
            match (y.blockType,y.windowSwitchFlag) with
            |(2,true) -> //short blocks
                match (y.mixedBlockFlag) with
                |true -> //Mixed blocks
                    for i = 0 to 7 do
                        lScaleFactor.[0] <- 
                            x.[(scalefactorLength.[0] * i)..(scalefactorLength.[0] * (i+1)) - 1] 
                            |> Array.map int 
                            |> bitsArraytoNumber
                    for i = 3 to 5 do
                        for j = 0 to 2 do
                            sScaleFactors.[j,i] <- 
                                x.[(scalefactorLength.[0] * i)..(scalefactorLength.[0] * (i+1))] 
                                |> Array.map int 
                                |> bitsArraytoNumber
                |false -> 
                     for i = 0 to 5 do
                        for j = 0 to 2 do
                            sScaleFactors.[j,i] <- 
                                x.[(scalefactorLength.[0] * i)..(scalefactorLength.[0] * (i+1))] 
                                |> Array.map int 
                                |> bitsArraytoNumber
                 for i = 6 to 12 do
                    for j = 0 to 3 do
                        sScaleFactors.[j,i] <- 
                            x.[(scalefactorLength.[0] * i)..(scalefactorLength.[0] * (i+1))] 
                            |> Array.map int 
                            |> bitsArraytoNumber
                for j = 0 to 3 do
                    sScaleFactors.[j,12] <- 
                        x.[(scalefactorLength.[0] * i)..(scalefactorLength.[0] * (i+1))] 
                        |> Array.map int 
                        |> bitsArraytoNumber
            |(_,_) -> //Long blocks
                match y.granule = 0 with
                |true ->
                    for i = 0 to 10
                        lScaleFactor.[i] <- 
                            x.[(scalefactorLength.[0] * i)..(scalefactorLength.[0] * (i+1)) - 1] 
                            |> Array.map int 
                            |> bitsArraytoNumber
                    for i = 11 to 20
                        x.[(scalefactorLength.[1] * i)..(scalefactorLength.[1] * (i+1)) - 1] 
                        |> Array.map int 
                        |> bitsArraytoNumber
                |false ->
                    let sb = [|6;11;16;21|];
                    for i = 0 to 1
                        for j = 0 to (sb.[i] - 1)
                            if (sideinfo.scfsi.[i]) 
                                then
        slen
    data
//Test Data
let main = 
    let data = IO.File.ReadAllBytes "aframe.mp3"
    let header = data.[0..3] |> parseHeader
    printfn "Header\n\n%A\n" header
    header |> parseSideConfig data.[4..35] |> printfn "Side config\n\n%A\n"
