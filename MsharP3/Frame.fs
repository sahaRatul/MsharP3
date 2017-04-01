namespace MsharP3

open Header
module Frame = 

    type Samples = {
        group:int
        channel:int
        sampledata:array<int>
    }

    type FrameInfo = {
        frameSize:int
        bandWidth:(int array * int array)
        bandIndex:(int array * int array)
    }

    let getBandWidth sampleRate = 
        let long32 = [|4;4;4;4;4;4;6;6;8;10;12;16;20;24;30;38;46;56;68;84;102|]
        let short32 = [|4;4;4;4;6;8;12;16;20;26;34;42|]

        let long44 = [|4;4;4;4;4;4;6;6;8;8;10;12;16;20;24;28;34;42;50;54;76|]
        let short44 = [|4;4;4;4;6;8;10;12;14;18;22;30|]

        let long48 = [|4;4;4;4;4;4;6;6;6;8;10;12;16;18;22;28;34;40;46;54;54|]
        let short48 = [|4;4;4;4;6;6;10;12;14;16;20;26|]

        match sampleRate with
        |32000 -> (long32,short32)
        |44100 -> (long44,short44)
        |48000 -> (long48,short48)
        |_ -> failwith "Unknown Samplerate"
    
    let getBandIndex sampleRate = 
        let long32 = 
            [|
                0;4;8;12;16;20;24;30;36;44;54;66;82;
                102;126;156;194;240;296;364;448;550;576
            |]
        let short32 = [|0;4;8;12;16;22;30;42;58;78;104;138;180;192|]

        let long44 = 
            [|
                0;4;8;12;16;20;24;30;36;44;52;62;74;
                90;110;134;162;196;238;288;342;418;576
            |]
        let short44 = [|0;4;8;12;16;22;30;40;52;66;84;106;136;192|]

        let long48 = 
            [|
                0;4;8;12;16;20;24;30;36;42;50;60;72;
                88;106;128;156;190;230;276;330;384;576
            |]
        let short48 = [|0;4;8;12;16;22;28;38;50;64;80;100;126;192|]
        
        match sampleRate with
        |32000 -> (long32,short32)
        |44100 -> (long44,short44)
        |48000 -> (long48,short48)
        |_ -> failwith "Unknown Samplerate"

    //Get Frame frame Size
    let getFrameInfo (x:HeaderConfig) = 
        let samplesCount = 
            match (x.audioVersion,x.layerDesc) with
            |(1uy,1uy) -> 1152
            |(1uy,_) -> 576
            |(_,2uy) -> 1152
            |(_,3uy) -> 384
            |(_,_) -> failwith "Error while getting framesize"
        
        let (frameinfo:FrameInfo) = {
            frameSize = (144 * x.bitRate) / x.sampleRate + (if x.padding then 1 else 0)
            bandWidth = getBandWidth x.sampleRate
            bandIndex = getBandIndex x.sampleRate
        }
        frameinfo