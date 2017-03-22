open System;

//Header bytes stored here
type HeaderBytes = {
    firstByte:byte;
    secondByte:byte;
    thirdByte:byte;
    fourthByte:byte
}

//Configs stored here after parsing header
type HeaderConfigs = {
    audioVersion:byte;
    layerDesc:byte;
    protection:bool;
    bitRate:int;
    sampleRate:int;
    padding:bool;
    privateBit:int;
    channelMode:byte;
    modeExtension:byte;
    copyright:bool;
    original:bool;
    emphasis:byte
}

//Side info stored here
type SideInfo = {
    data:List<byte>
}

type SideInfoConfig = {
    mainDataBegin:array<int>;
    privateBits:array<int>;
    scfsi:array<array<int>>;
    par23Length:array<array<int>>;
    bigValues:array<array<int>>;
    globalGain:array<array<int>>;
    scaleFactorCompress:array<array<int>>;
    windowSwitchFlag:array<int>;
    blockType:array<array<int>>;
    mixedBlockFlag:array<int>;
    (*tableSelect:array<array<int>>;
    subBlockGain:array<array<int>>;
    region0Count:array<array<int>>;
    preflag:array<array<int>>;
    scaleFactorScale:array<array<int>>;
    count1TableSelect:array<array<int>>;*)
}

//Get a Array of bits from a list of bytes
let getBitsArrayfromList x = 
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
    x |> List.map getBits |> Array.concat

let x = [|1;1;1;1;0;1|]
let bitsArrayTonumber x = 
    x 
    |> Array.zip[|(x.Length-1)..(-1)..0|] 
    |> Array.sumBy (fun x -> snd x * (pown 2 (fst x)))

let parseHeader (x:List<Byte>) = 
    
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
        let bits = x |> List.map int |> getBitsArrayfromList
        let (config:HeaderConfigs) = {
            audioVersion = bits.[12] |> byte;
            layerDesc = bits.[13..14] |> bitsArrayTonumber |> byte;
            protection = if bits.[15] = 0 then false else true;
            bitRate = bits.[16..19] |> bitsArrayTonumber |> getBitrate (bits.[12],bits.[13..14] |> bitsArrayTonumber);
            sampleRate = bits.[20..21] |> bitsArrayTonumber |> getSamplerate bits.[12]
            padding = if bits.[22] = 0 then false else true;
            privateBit = bits.[23];
            channelMode = bits.[24..25] |> bitsArrayTonumber |> byte;
            modeExtension = bits.[26..27] |> bitsArrayTonumber |> byte;
            copyright = if bits.[28] = 0 then false else true;
            original = if bits.[29] = 0 then false else true;
            emphasis = bits.[30..31] |> bitsArrayTonumber |> byte
        }
        config
        
let getSideConfig (x:List<Byte>) (y:HeaderConfigs) = 
    match (x.Length,y.channelMode) with
    |(17,3uy) -> //For mono
        let bitArray = x |> List.map int |> getBitsArrayfromList
        let (configs:SideInfoConfig) = {
            mainDataBegin = bitArray.[0..8];
            privateBits = bitArray.[9..11];
            scfsi = [|bitArray.[12..15]|];
            par23Length = [|bitArray.[16..28]|];
            bigValues = [|bitArray.[29..37]|];
            globalGain = [|bitArray.[38..45]|];
            scaleFactorCompress = [|bitArray.[46..49]|];
            windowSwitchFlag = [|bitArray.[50]|];
            blockType = [|bitArray.[51..52]|];
            mixedBlockFlag = [|bitArray.[53]|];
        }
        configs
    |(32,0uy)|(32,1uy)|(32,2uy) -> //For dual channel modes
        let bitArray = x |> List.map int |> getBitsArrayfromList
        let (configs:SideInfoConfig) = {
            mainDataBegin = bitArray.[0..8];
            privateBits = bitArray.[9..11];
            scfsi = [|bitArray.[12..15];bitArray.[16..19]|];
            par23Length = [|bitArray.[20..31];bitArray.[32..43]|];
            bigValues = [|bitArray.[44..52];bitArray.[53..61]|];
            globalGain = [|bitArray.[62..69];bitArray.[70..77]|];
            scaleFactorCompress = [|bitArray.[78..81];bitArray.[82..85]|];
            windowSwitchFlag = [|bitArray.[86];bitArray.[87]|];
            blockType = [|bitArray.[88..89];bitArray.[90..91]|];
            mixedBlockFlag = [|bitArray.[92];bitArray.[93]|];
        }
        configs
    |(_,_) -> //Any other cases
        failwith "Error while decoding "

//Test Data
let main = 
    let data = 
        [0x7A;0x86;0x05;0xD2;0x76;0x5A;0x51;0xE9;0x35;0xB6;
        0x30;0x82;0x4B;0x4E;0x3D;0x82;0x18;0x16;0xEE;0x11;
        0x63;0xA7;0xB0;0xD6;0x98;0xC5;0x89;0xEC;0xF8;0xC0;0x99;0x98]

    //data |> getBitsArrayfromList |> printfn "%A"
    let header = [0xFFuy;0xFBuy;0xE2uy;0x64uy] |> parseHeader
    printfn "Header = %A\n" header
    getSideConfig (data |> List.map byte) header |> printfn "%A"
