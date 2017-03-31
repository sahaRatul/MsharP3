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
        let data = IO.File.ReadAllBytes "aframe.mp3"
        
        let result = 
            for i = 0 to 1000 do
                //Get Frame header
                let header = data.[0..3] |> parseHeader
        
                //Get Frame Sideinfo
                let sideconfig = header |> parseSideConfig data.[4..35]
                
                //Get Frameinfo
                let frameinfo = getFrameInfo header

                //Get Maindata
                let (scalefactors,samples) = parseMainData data.[36..] header frameinfo sideconfig

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
                printfn "%d" i
                result2 |> ignore
        System.Console.ReadLine();
