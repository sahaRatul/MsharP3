namespace MsharP3

open System
open Header
open Sideinfo
open Maindata
open Frame
open MathUtils

module Test = 
    //Test Data
    let main = 
        let data = IO.File.ReadAllBytes "Again.mp3"
        let mutable offset = 0
        let mutable count = 0
        while offset < data.Length do
            //Get Frame header
            let header = data.[offset..(3 + offset)] |> parseHeader
            offset <- offset + 4

            //Get Frame Sideinfo
            let sideinfosize = if header.channelMode = 3uy then 16 else 31
            let sideconfig = header |> parseSideConfig data.[offset..(sideinfosize + offset)]
            offset <- offset + sideinfosize + 1

            //Get Frameinfo
            let frameinfo = getFrameInfo header

            //Get Maindata
            let (scalefactors,samples) = parseMainData data.[offset..(offset + frameinfo.frameSize - 1 - sideinfosize)] header frameinfo sideconfig
            offset <- offset + frameinfo.frameSize - 4 - sideinfosize - 1

            //Requantize
            let requantizedSamples = Array.map3 (requantizeSamples frameinfo) sideconfig.sideInfoGr scalefactors samples
                
            //Mid side 
            let result = 
                if header.channelMode = 1uy
                    then
                        let samplesgranule0 = decodeMidSide requantizedSamples.[0] requantizedSamples.[1]
                        let samplesgranule1 = decodeMidSide requantizedSamples.[2] requantizedSamples.[3]
                        Array.concat [|samplesgranule0;samplesgranule1|]
                    else
                        requantizedSamples
                
            //Reorder/Alias reduction
            let result2 = Array.zeroCreate 4
            for i = 0 to (if header.channelMode = 3uy then 1 else 3) do
                result2.[i] <-
                    if (sideconfig.sideInfoGr.[i].mixedBlockFlag || sideconfig.sideInfoGr.[i].blockType = 2) 
                        then reorderSamples frameinfo result.[i]
                        else
                            reduceAlias sideconfig.sideInfoGr.[i] result.[i]
            
            //IMDCT
            let result3 = Array.map2 (fun x y -> IMDCT x y) sideconfig.sideInfoGr result2
            printfn "Count = %d" count
            count <- count + 1
            result2 |> ignore
