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

//Tuple used for the parsing process
type headerFlags = headerBytes * headerConfigs

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
    let arr13 = [|0;32;40;48;56;64;80;96;112;128;144;160;176;192;224;256|]
    
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
let parseHeader = 
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

//Test code
let main = 
    let testHeader = {
        firstByte = 0xFFuy; 
        secondByte = 0xFBuy; 
        thirdByte = 0x78uy; 
        fourthByte = 0x64uy
    }

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

    (testHeader,testConfig) |> parseHeader |> snd |> printfn "%A"
