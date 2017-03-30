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
            for i = 0 to 1 do
                //Get Frame header
                let header = data.[0..3] |> parseHeader
        
                //Get Frame Sideinfo
                let sideconfig = header |> parseSideConfig data.[4..35]
                
                //Get Frameinfo
                let frameinfo = getFrameInfo header

                //Get Maindata
                let (scalefactors,samples) = parseMainData data.[36..] header frameinfo sideconfig
                //printfn "%d\r" i

                //Requantize
                let result = requantizeSamples sideconfig.sideInfoGr.[0] frameinfo scalefactors.[0] samples.[0]
                let result2 = requantizeSamples sideconfig.sideInfoGr.[1] frameinfo scalefactors.[1] samples.[1]
                let result3 = requantizeSamples sideconfig.sideInfoGr.[2] frameinfo scalefactors.[2] samples.[2]
                let result4 = requantizeSamples sideconfig.sideInfoGr.[3] frameinfo scalefactors.[3] samples.[3]
                
                result |> ignore
        System.Console.ReadLine();
