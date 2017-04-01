namespace MsharP3

open Utils

module Header = 
    //Data
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

    //Parse Header
    let parseHeader (x:array<byte>) = 
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
            let bits = x |> getBitsArrayfromByteArray
            let (config:HeaderConfig) = {
                audioVersion = bits.[12];
                layerDesc = bits.[13..14] |> bitsArraytoNumber |> byte;
                protection = if bits.[15] = 0uy then false else true;
                bitRate = 
                    bits.[16..19]  
                    |> bitsArraytoNumber 
                    |> getBitrate (bits.[12] |> int,bits.[13..14] |> bitsArraytoNumber);
                sampleRate = bits.[20..21] |> bitsArraytoNumber |> getSamplerate (bits.[12] |> int)
                padding = if bits.[22] = 0uy then false else true;
                privateBit = bits.[23] |> int;
                channelMode = bits.[24..25] |> bitsArraytoNumber |> byte;
                modeExtension = bits.[26..27] |> bitsArraytoNumber |> byte;
                copyright = if bits.[28] = 0uy then false else true;
                original = if bits.[29] = 0uy then false else true;
                emphasis = bits.[30..31] |> bitsArraytoNumber |> byte
            }
            config