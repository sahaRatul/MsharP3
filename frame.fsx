namespace FSharpMP3

open Header
module Frame = 

    type Samples = {
        group:int
        channel:int
        sampledata:array<int>
    }

    type FrameInfo = {
        frameSize:int
    }

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