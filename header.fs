open System;

//Header bytes stored here
type headerBytes = {
    firstByte:byte;
    secondByte:byte;
    thirdByte:byte;
    fourthByte:byte
}

//Configs stored here after parsing header
type headerConfigs = {
    audioVersion:byte;
    layerDesc:byte;
    protection:bool;
    bitRate:int;
    sampleRate:int;
    padding:bool;
    privateBit:bool;
    channelMode:byte;
    modeExtension:byte * string;
    copyright:bool;
    original:bool;
    emphasis:(byte * string)
}

//Side info stored here
type sideInfo = {
    data:List<byte>
}

type sideInfoConfig = {
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

//Tuple used for the parsing process
type headerFlags = headerBytes * headerConfigs

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
    |> Array.map (fun x -> snd x * (pown 2 (fst x)))
    |> Array.fold (fun acc elem -> acc + elem) 0

x |> bitsArrayTonumber |> printfn "%A"

let parseHeader2 (x:List<Byte>) = 
    match x.Length = 4 with
    |false -> failwith "Not 4 bytes of header"
    |true -> 
        let bits = x |> List.map int |> getBitsArrayfromList
        let (config:headerConfigs) = {
            audioVersion = bits.[13] |> byte;
            layerDesc = bits.[14..15] |> bitsArrayTonumber |> byte;
            protection = if bits.[16] = 0 then false else true;
        }
        config
        //bits

let parseHeader = 
    //Determine MPEG version
    let getAudioVersion (x:headerFlags) = 
        let sndByte = (fst x).secondByte
        let version = (sndByte >>> 3) &&& 0b00000011uy
        let newConfig = {snd x with audioVersion = version}
        let (newFlags:headerFlags) = (fst x,newConfig)
        newFlags

    //Determine MPEG Layer
    let getLayerDesc (x:headerFlags) = 
        let sndByte = (fst x).secondByte
        let desc = (sndByte >>> 1) &&& 0b00000011uy
        let newConfig = {snd x with layerDesc = desc}
        let (newFlags:headerFlags) = (fst x,newConfig)
        newFlags

    //Determine whether protected
    let getProtectionBit (x:headerFlags) = 
        let protection = (fst x).secondByte &&& 0b00000001uy
        let newConfig = 
            match protection with
            |1uy -> {snd x with protection = false}
            |_ -> {snd x with protection = true}
        let (newFlags:headerFlags) = (fst x,newConfig)
        newFlags

    //Determine bitrate from bitrate index
    let getBitrate (x:headerFlags) = 
        
        //Define bitrate arrays
        let arr12 = [|0;32;48;56;64;80;96;112;128;160;192;224;256;320;384|]
        let arr13 = [|0;32;40;48;56;64;80;96;112;128;160;192;224;256;320|]
        
        //Get bitrate index
        let bitIndex = (((fst x).thirdByte >>> 4) &&& 0b00001111uy) |> int
        let version = (snd x).audioVersion |> int
        let layer = (snd x).layerDesc |> int
        
        //Determine bitrate
        let bitrate = 
            match (version,layer) with
            |(3,1) -> arr13.[bitIndex] * 1000
            |(3,2) -> arr12.[bitIndex] * 1000
            |(3,3) -> 32000 * bitIndex
            |(3,_) -> failwith "Layer Error"
            |(2,_) -> failwith "Version 2 unimplemented"
            |(_,_) -> failwith "Unimplemented"
        let newConfig = {snd x with bitRate = bitrate}  
        let (newFlags:headerFlags) = (fst x,newConfig)
        newFlags

    //Get Sampling Rate
    let getSampleRate (x:headerFlags) = 
        let sampleIndex = ((fst x).thirdByte >>> 2) &&& 0b00000011uy |> int
        let version = (snd x).audioVersion |> int
        let samplerate = 
            match (version,sampleIndex) with
            |(3,0) -> 44100
            |(3,1) -> 48000
            |(3,2) -> 32000
            |(3,3) -> 0
            |(_,_) -> failwith "Version 2 unimplemented"
        let newConfig = {snd x with sampleRate = samplerate}  
        let (newFlags:headerFlags) = (fst x,newConfig)
        newFlags

    //Check padding
    let getPadding (x:headerFlags) =
        let ispadded = ((fst x).thirdByte >>> 1) &&& 0b00000001uy |> int
        let newConfig = 
            match ispadded with
            |0 -> {snd x with padding = false}
            |_ -> {snd x with padding = true}
        let (newFlags:headerFlags) = (fst x,newConfig)
        newFlags

    //Check private Bit
    let getPrivateBit (x:headerFlags) =
        let privateBit = (fst x).thirdByte &&& 0b00000001uy |> int
        let newConfig = 
            match privateBit with
            |0 -> {snd x with privateBit = false}
            |_ -> {snd x with privateBit = true}
        let (newFlags:headerFlags) = (fst x,newConfig)
        newFlags

    //Get channel mode
    let getChannelMode (x:headerFlags) = 
        let mode = ((fst x).fourthByte >>> 6) &&& 0b00000011uy
        let newConfig = {snd x with channelMode = mode}  
        let (newFlags:headerFlags) = (fst x,newConfig)
        newFlags

    //Get mode Extension
    let getModeExtension (x:headerFlags) = 
        let extension = 
            match (snd x).channelMode with
            |01uy -> ((((fst x).fourthByte >>> 4) &&& 0b00000011uy),"Applicable")
            |_ -> (00uy,"Not Applicable")
        let newFlags = fst x,{snd x with modeExtension = extension}
        newFlags

    //check copyright
    let getCopyright (x:headerFlags) =
        let copyBit = ((fst x).fourthByte >>> 3) &&& 0b00000001uy
        let newConfig = 
            match copyBit with
            |0uy -> {snd x with copyright = false}
            |_ -> {snd x with copyright = true}
        let (newFlags:headerFlags) = (fst x,newConfig)
        newFlags

    //Check original
    let getOriginal (x:headerFlags) =
        let origBit = ((fst x).fourthByte >>> 2) &&& 0b00000001uy
        let newConfig = 
            match origBit with
            |0uy -> {snd x with original = false}
            |_ -> {snd x with original = true}
        let (newFlags:headerFlags) = (fst x,newConfig)
        newFlags

    //Get Emphasis
    let getEmphasis (x:headerFlags) = 
        let emphasisBits = (fst x).fourthByte &&& 0b00000011uy
        let emphasisString = 
            match emphasisBits with
            |0uy -> "none"
            |1uy -> "50/15 ms"
            |2uy -> "reserved"
            |3uy -> "CCIT J.17"
            |_ -> failwith "Unknown Emphasis"
        let newConfig = {snd x with emphasis = (emphasisBits,emphasisString)}
        let (newFlags:headerFlags) = (fst x,newConfig)
        newFlags
    
    //Stitch all of the above functions
    getAudioVersion >> 
    getLayerDesc >> 
    getProtectionBit >> 
    getBitrate >>
    getSampleRate >>
    getPadding >>
    getChannelMode >>
    getModeExtension >>
    getCopyright >>
    getOriginal >>
    getEmphasis

let getSideConfig (x:List<Byte>) (y:headerConfigs) = 
    match (x.Length,y.channelMode) with
    |(17,3uy) -> //For mono
        let bitArray = x |> List.map int |> getBitsArrayfromList
        let (configs:sideInfoConfig) = {
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
        let (configs:sideInfoConfig) = {
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
    let testHeader = {
        firstByte = 0xFFuy; 
        secondByte = 0xFBuy; 
        thirdByte = 0xE2uy; 
        fourthByte = 0x64uy
    }

    let data = 
        [0x7A;0x86;0x05;0xD2;0x76;0x5A;0x51;0xE9;0x35;0xB6;
        0x30;0x82;0x4B;0x4E;0x3D;0x82;0x18;0x16;0xEE;0x11;
        0x63;0xA7;0xB0;0xD6;0x98;0xC5;0x89;0xEC;0xF8;0xC0;0x99;0x98]

    let testConfig = {
        audioVersion = 0uy;
        layerDesc = 0uy;
        protection = false;
        bitRate = 0;
        sampleRate = 0;
        padding = false;
        privateBit = false;
        channelMode = 0uy;
        modeExtension = 0uy,"";
        copyright = true;
        original = true;
        emphasis = (0uy,"")
    }

    data |> getBitsArrayfromList |> printfn "%A"
    let header = (testHeader,testConfig) |> parseHeader
    getSideConfig (data |> List.map byte) (snd header) |> printfn "%A"
